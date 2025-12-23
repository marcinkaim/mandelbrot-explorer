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

   procedure Test_Simple_Graph_Execution (T : in out Test_Case) is
      Res           : CUresult;
      Dev           : aliased CUdevice;
      Ctx           : aliased CUcontext;
      Module_Handle : aliased CUmodule;
      Func          : aliased CUfunction;
      
      -- Graph Handles
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

      -- Alloc & Copy using GLOBAL variables
      Res := cuMemAlloc(Global_Dev_Ptr'Access, size_t(Global_Input'Size / 8));
      Assert (Res = CUDA_SUCCESS, "Alloc fail");
      
      Res := cuMemcpyHtoD(Global_Dev_Ptr, Global_Input'Address, size_t(Global_Input'Size / 8));
      Assert (Res = CUDA_SUCCESS, "HtoD fail");

      -- 2. GRAPH CONSTRUCTION
      Res := cuGraphCreate(Graph'Access, 0);

      -- [FIX] Zero-init structure manually or via aggregate
      Node_Params := (others => <>); 

      -- Prepare Params
      Global_Params(0) := Global_Dev_Ptr'Address;
      
      -- Debug Log
      Ada.Text_IO.Put_Line ("DEBUG: Dev_Ptr Address (GPU): " & System.Address'Image(Global_Dev_Ptr'Address));
      Ada.Text_IO.Put_Line ("DEBUG: Params Array Address: " & System.Address'Image(Global_Params'Address));

      -- Fill Node Params
      Node_Params.func           := Func;
      Node_Params.gridDimX       := 1;
      Node_Params.gridDimY       := 1;
      Node_Params.gridDimZ       := 1;
      Node_Params.blockDimX      := 1;
      Node_Params.blockDimY      := 1;
      Node_Params.blockDimZ      := 1;
      Node_Params.sharedMemBytes := 0;
      Node_Params.kernelParams   := Global_Params'Address;
      Node_Params.extra          := System.Null_Address; -- Explicit NULL

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
      Assert (Res = CUDA_SUCCESS, "Graph Exec Failed (Code " & CUresult'Image(Res) & ")");

      Res := cuMemcpyDtoH(Global_Output'Address, Global_Dev_Ptr, size_t(Global_Output'Size / 8));
      
      Assert (Global_Output = 101, "Result Mismatch: " & int'Image(Global_Output));

      -- 4. Cleanup
      Res := cuGraphExecDestroy(Graph_Exec);
      Res := cuGraphDestroy(Graph);
      Res := cuMemFree(Global_Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Simple_Graph_Execution;

   -------------------------------------------------------------------------
   -- Test_Dependent_Graph_Execution
   -- Scenariusz: Input(100) -> Node A (+1) -> Node B (+1) -> Output(102)
   -------------------------------------------------------------------------
   procedure Test_Dependent_Graph_Execution (T : in out Test_Case) is
      Res           : CUresult;
      Dev           : aliased CUdevice;
      Ctx           : aliased CUcontext;
      Module_Handle : aliased CUmodule;
      Func          : aliased CUfunction;
      
      -- Graph Handles
      Graph         : aliased CUgraph;
      Graph_Exec    : aliased CUgraphExec;
      Node_A        : aliased CUgraphNode; -- Root
      Node_B        : aliased CUgraphNode; -- Dependent
      
      -- Parametry
      Node_Params   : aliased CUDA_KERNEL_NODE_PARAMS;
      
      type Node_Array is array (0 .. 0) of aliased CUgraphNode;
      Node_Dependencies : aliased Node_Array;

   begin
      -- 1. Setup (Standard boilerplate)
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit fail");
      Res := cuDeviceGet(Dev'Access, 0);
      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);
      
      -- Load Kernel
      declare
         Ptx_Path  : char_array := To_C ("kernels/test_kernel.ptx");
         Func_Name : char_array := To_C ("test_inc");
      begin
         Res := cuModuleLoad(Module_Handle'Access, Ptx_Path);
         Res := cuModuleGetFunction(Func'Access, Module_Handle, Func_Name);
         Assert (Res = CUDA_SUCCESS, "Load kernel fail");
      end;

      -- Reset Global Data
      Global_Input  := 100;
      Global_Output := 0;

      -- Alloc & Copy HtoD
      Res := cuMemAlloc(Global_Dev_Ptr'Access, size_t(Global_Input'Size / 8));
      Res := cuMemcpyHtoD(Global_Dev_Ptr, Global_Input'Address, size_t(Global_Input'Size / 8));
      
      -- Prepare Params (Both nodes operate on the same pointer)
      Global_Params(0) := Global_Dev_Ptr'Address;

      -- 2. GRAPH CONSTRUCTION
      Res := cuGraphCreate(Graph'Access, 0);
      
      -- Common Node Params setup
      Node_Params := (others => <>); -- Zero init
      Node_Params.func           := Func;
      Node_Params.gridDimX       := 1; Node_Params.gridDimY := 1; Node_Params.gridDimZ := 1;
      Node_Params.blockDimX      := 1; Node_Params.blockDimY := 1; Node_Params.blockDimZ := 1;
      Node_Params.sharedMemBytes := 0;
      Node_Params.kernelParams   := Global_Params'Address;
      Node_Params.extra          := System.Null_Address;

      -- A. Add Node A (Root - No dependencies)
      Res := cuGraphAddKernelNode
        (phGraphNode     => Node_A'Access,
         hGraph          => Graph,
         dependencies    => System.Null_Address,
         numDependencies => 0,
         nodeParams      => Node_Params'Access);
      Assert (Res = CUDA_SUCCESS, "Add Node A fail: " & CUresult'Image(Res));

      -- B. Add Node B (Dependent on Node A)
      Node_Dependencies(0) := Node_A; -- Define the edge A -> B

      Res := cuGraphAddKernelNode
        (phGraphNode     => Node_B'Access,
         hGraph          => Graph,
         dependencies    => Node_Dependencies(0)'Address, -- Pointer to handle array
         numDependencies => 1,                            -- Count
         nodeParams      => Node_Params'Access);          -- Reuse same params (same kernel, same data)
      Assert (Res = CUDA_SUCCESS, "Add Node B fail: " & CUresult'Image(Res));

      -- C. Instantiate
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

      -- Sync & Verify
      Res := cuCtxSynchronize;
      Assert (Res = CUDA_SUCCESS, "Graph Exec Failed (Code " & CUresult'Image(Res) & ")");

      -- 4. READBACK
      Res := cuMemcpyDtoH(Global_Output'Address, Global_Dev_Ptr, size_t(Global_Output'Size / 8));
      
      -- Expected: 100 + 1 (Node A) + 1 (Node B) = 102
      Assert (Global_Output = 102, 
              "Dependency Chain Failed. Expected 102, got: " & int'Image(Global_Output));

      -- 5. Cleanup
      Res := cuGraphExecDestroy(Graph_Exec);
      Res := cuGraphDestroy(Graph);
      Res := cuMemFree(Global_Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Dependent_Graph_Execution;

end Test_CUDA_Graph;