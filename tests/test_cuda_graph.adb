--------------------------------------------------------------------------------
--  Mandelbrot Explorer
--  Copyright (C) 2026 Marcin Kaim
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

with AUnit.Assertions; use AUnit.Assertions;
with CUDA_Driver_API;  use CUDA_Driver_API;
with Interfaces.C;     use Interfaces.C;
with System;
with Ada.Text_IO;

package body Test_CUDA_Graph is

   type Param_Array is array (0 .. 0) of System.Address;
   Global_Params   : aliased Param_Array;
   Global_Dev_Ptr  : aliased CUdeviceptr;
   Global_Input    : aliased int := 100;
   Global_Output   : aliased int := 0;

   -------------------------------------------------------------------------
   -- Test Simple Graph
   -------------------------------------------------------------------------
   procedure Test_Simple_Graph_Execution (T : in out Test_Case) is
      Res           : CUresult;
      Dev           : aliased CUdevice;
      Ctx           : aliased CUcontext;
      Module_Handle : aliased CUmodule;
      Func          : aliased CUfunction;
      
      Graph         : aliased CUgraph;
      Graph_Exec    : aliased CUgraphExec;
      Node          : aliased CUgraphNode;
      Node_Params   : aliased CUDA_KERNEL_NODE_PARAMS;
   begin
      -- 1. Setup
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit fail");
      
      Res := cuDeviceGet(Dev'Access, 0);
      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);

      declare
         Ptx_Path  : char_array := To_C ("kernels/test_kernel.ptx");
         Func_Name : char_array := To_C ("test_inc");
      begin
         Res := cuModuleLoad(Module_Handle'Access, Ptx_Path);
         Res := cuModuleGetFunction(Func'Access, Module_Handle, Func_Name);
         Assert (Res = CUDA_SUCCESS, "Load kernel fail");
      end;

      -- Alloc & Copy
      Res := cuMemAlloc(Global_Dev_Ptr'Access, size_t(Global_Input'Size / 8));
      Assert (Res = CUDA_SUCCESS, "Alloc fail");
      
      Res := cuMemcpyHtoD(Global_Dev_Ptr, Global_Input'Address, size_t(Global_Input'Size / 8));
      Assert (Res = CUDA_SUCCESS, "HtoD fail");

      -- 2. GRAPH CONSTRUCTION
      Res := cuGraphCreate(Graph'Access, 0);

      -- Prepare Params
      Global_Params(0) := Global_Dev_Ptr'Address;

      -- FIX: Fully explicit initialization to avoid -gnatwv warnings
      Node_Params := 
        (func           => Func,
         gridDimX       => 1,
         gridDimY       => 1,
         gridDimZ       => 1,
         blockDimX      => 1,
         blockDimY      => 1,
         blockDimZ      => 1,
         sharedMemBytes => 0,
         kernelParams   => Global_Params'Address,
         extra          => System.Null_Address);

      Res := cuGraphAddKernelNode
        (phGraphNode     => Node'Access,
         hGraph          => Graph,
         dependencies    => System.Null_Address,
         numDependencies => 0,
         nodeParams      => Node_Params'Access);
      Assert (Res = CUDA_SUCCESS, "AddNode fail: " & CUresult'Image(Res));

      Res := cuGraphInstantiate
        (phGraphExec => Graph_Exec'Access,
         hGraph      => Graph,
         pLogBuffer  => System.Null_Address,
         bufferSize  => 0,
         flags       => 0);
      Assert (Res = CUDA_SUCCESS, "Instantiate fail: " & CUresult'Image(Res));

      -- 3. EXECUTION
      Res := cuGraphLaunch(Graph_Exec, CUstream(System.Null_Address));
      Assert (Res = CUDA_SUCCESS, "Launch fail");

      Res := cuCtxSynchronize;
      Assert (Res = CUDA_SUCCESS, "Graph Exec Failed");

      Res := cuMemcpyDtoH(Global_Output'Address, Global_Dev_Ptr, size_t(Global_Output'Size / 8));
      
      Assert (Global_Output = 101, "Result Mismatch: " & int'Image(Global_Output));

      -- 4. Cleanup
      Res := cuGraphExecDestroy(Graph_Exec);
      Res := cuGraphDestroy(Graph);
      Res := cuMemFree(Global_Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Simple_Graph_Execution;

   -------------------------------------------------------------------------
   -- Test Dependent Graph
   -------------------------------------------------------------------------
   procedure Test_Dependent_Graph_Execution (T : in out Test_Case) is
      Res           : CUresult;
      Dev           : aliased CUdevice;
      Ctx           : aliased CUcontext;
      Module_Handle : aliased CUmodule;
      Func          : aliased CUfunction;
      
      Graph         : aliased CUgraph;
      Graph_Exec    : aliased CUgraphExec;
      Node_A        : aliased CUgraphNode;
      Node_B        : aliased CUgraphNode;
      Node_Params   : aliased CUDA_KERNEL_NODE_PARAMS;
      
      type Node_Array is array (0 .. 0) of aliased CUgraphNode;
      Node_Dependencies : aliased Node_Array;
   begin
      -- 1. Setup
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit fail");
      Res := cuDeviceGet(Dev'Access, 0);
      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);

      declare
         Ptx_Path  : char_array := To_C ("kernels/test_kernel.ptx");
         Func_Name : char_array := To_C ("test_inc");
      begin
         Res := cuModuleLoad(Module_Handle'Access, Ptx_Path);
         Res := cuModuleGetFunction(Func'Access, Module_Handle, Func_Name);
         Assert (Res = CUDA_SUCCESS, "Load kernel fail");
      end;

      Global_Input  := 100;
      Global_Output := 0;

      Res := cuMemAlloc(Global_Dev_Ptr'Access, size_t(Global_Input'Size / 8));
      Res := cuMemcpyHtoD(Global_Dev_Ptr, Global_Input'Address, size_t(Global_Input'Size / 8));
      
      Global_Params(0) := Global_Dev_Ptr'Address;

      -- 2. GRAPH CONSTRUCTION
      Res := cuGraphCreate(Graph'Access, 0);

      -- FIX: Fully explicit initialization
      Node_Params := 
        (func           => Func,
         gridDimX       => 1,
         gridDimY       => 1,
         gridDimZ       => 1,
         blockDimX      => 1,
         blockDimY      => 1,
         blockDimZ      => 1,
         sharedMemBytes => 0,
         kernelParams   => Global_Params'Address,
         extra          => System.Null_Address);

      -- A. Add Node A
      Res := cuGraphAddKernelNode
        (phGraphNode     => Node_A'Access,
         hGraph          => Graph,
         dependencies    => System.Null_Address,
         numDependencies => 0,
         nodeParams      => Node_Params'Access);
      Assert (Res = CUDA_SUCCESS, "Add Node A fail");

      -- B. Add Node B (Dependent on A)
      Node_Dependencies(0) := Node_A;

      Res := cuGraphAddKernelNode
        (phGraphNode     => Node_B'Access,
         hGraph          => Graph,
         dependencies    => Node_Dependencies(0)'Address, 
         numDependencies => 1,                      
         nodeParams      => Node_Params'Access);
      Assert (Res = CUDA_SUCCESS, "Add Node B fail");

      -- C. Instantiate
      Res := cuGraphInstantiate
        (phGraphExec => Graph_Exec'Access,
         hGraph      => Graph,
         pLogBuffer  => System.Null_Address,
         bufferSize  => 0,
         flags       => 0);
      Assert (Res = CUDA_SUCCESS, "Instantiate fail");

      -- 3. EXECUTION
      Res := cuGraphLaunch(Graph_Exec, CUstream(System.Null_Address));
      Assert (Res = CUDA_SUCCESS, "Launch fail");

      Res := cuCtxSynchronize;
      Assert (Res = CUDA_SUCCESS, "Graph Exec Failed");

      -- 4. READBACK
      Res := cuMemcpyDtoH(Global_Output'Address, Global_Dev_Ptr, size_t(Global_Output'Size / 8));
      
      Assert (Global_Output = 102, 
              "Dependency Chain Failed. Expected 102, got: " & int'Image(Global_Output));

      -- 5. Cleanup
      Res := cuGraphExecDestroy(Graph_Exec);
      Res := cuGraphDestroy(Graph);
      Res := cuMemFree(Global_Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Dependent_Graph_Execution;

end Test_CUDA_Graph;