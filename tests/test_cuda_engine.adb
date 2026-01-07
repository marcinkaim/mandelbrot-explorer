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
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;
with Ada.Text_IO;

package body Test_CUDA_Engine is

   procedure Init_Direct_Kernel 
     (Ctx    : aliased out CUcontext;
      Module : aliased out CUmodule;
      Kernel : aliased out CUfunction) 
   is
      Res : CUresult;
      Dev : aliased CUdevice;
      Path : char_array := To_C ("kernels/mandelbrot_fp64.ptx");
      Name : char_array := To_C ("mandelbrot_fp64_kernel");
   begin
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit fail: " & CUresult'Image(Res));

      Res := cuDeviceGet(Dev'Access, 0);
      Assert (Res = CUDA_SUCCESS, "cuDeviceGet fail: " & CUresult'Image(Res));

      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);
      Assert (Res = CUDA_SUCCESS, "cuCtxCreate fail: " & CUresult'Image(Res));

      Res := cuModuleLoad(Module'Access, Path);
      if Res /= CUDA_SUCCESS then
         Ada.Text_IO.Put_Line ("[ERROR] Failed to load PTX. Code: " & CUresult'Image(Res));
      end if;
      Assert (Res = CUDA_SUCCESS, "cuModuleLoad fail");

      Res := cuModuleGetFunction(Kernel'Access, Module, Name);
      Assert (Res = CUDA_SUCCESS, "cuModuleGetFunction fail: " & CUresult'Image(Res));
   end Init_Direct_Kernel;

   procedure Test_FP64_Render (T : in out Test_Case) is
      Ctx     : aliased CUcontext;
      Module  : aliased CUmodule;
      Kernel  : aliased CUfunction;
      Dev_Ptr : aliased CUdeviceptr;
      Res     : CUresult;

      W : aliased int := 4;
      H : aliased int := 4;
      
      -- Parameters
      Step      : aliased Long_Float := 1.0;
      Min_X     : aliased Long_Float := -2.0;
      Min_Y     : aliased Long_Float := -2.0;
      Max_Iter  : aliased int := 100;

      Num_Pixels  : constant int := 16;
      Buffer_Size : constant size_t := size_t(Num_Pixels * 4); 
      Host_Buffer : array (0 .. Num_Pixels - 1) of aliased float;

      type Param_Array is array (0 .. 5) of System.Address;
      Params : aliased Param_Array;
   begin
      Ada.Text_IO.Put_Line ("[Test] Initializing Direct Kernel Test...");
      Init_Direct_Kernel (Ctx, Module, Kernel);

      -- 1. Alloc
      Res := cuMemAlloc (Dev_Ptr'Access, Buffer_Size);
      Assert (Res = CUDA_SUCCESS, "Alloc fail: " & CUresult'Image(Res));
      
      -- 2. Args
      Params(0) := Min_X'Address;
      Params(1) := Min_Y'Address;
      Params(2) := Step'Address;
      Params(3) := W'Address;
      Params(4) := Max_Iter'Address;
      Params(5) := Dev_Ptr'Address;

      -- 3. Launch
      Ada.Text_IO.Put_Line ("[Test] Launching Kernel (4x4 threads)...");
      Res := cuLaunchKernel
        (f              => Kernel,
         gridDimX       => 1, gridDimY => 1, gridDimZ => 1,
         blockDimX      => 4, blockDimY => 4, blockDimZ => 1,
         sharedMemBytes => 0,
         hStream        => CUstream(System.Null_Address),
         kernelParams   => Params'Address,
         extra          => System.Null_Address);
      
      Assert (Res = CUDA_SUCCESS, "Kernel Launch Submission fail: " & CUresult'Image(Res));
      
      -- 4. Sync (Here catches execution errors)
      Res := cuCtxSynchronize;
      if Res /= CUDA_SUCCESS then
          Ada.Text_IO.Put_Line ("[ERROR] Kernel Execution Failed!");
          Ada.Text_IO.Put_Line ("    Code: " & CUresult'Image(Res));
          Ada.Text_IO.Put_Line ("    Hint: 700=IllegalAddress, 719=LaunchOutOfResources");
      end if;
      Assert (Res = CUDA_SUCCESS, "cuCtxSynchronize fail: " & CUresult'Image(Res));

      -- 5. Readback
      Res := cuMemcpyDtoH (Host_Buffer(0)'Address, Dev_Ptr, Buffer_Size);
      Assert (Res = CUDA_SUCCESS, "Readback fail: " & CUresult'Image(Res));

      -- 6. Verify (DEBUG MODE: Just print values first)
      Ada.Text_IO.Put_Line ("[Test] Verification:");
      Ada.Text_IO.Put_Line ("    Idx(0) [Corner]: " & float'Image(Host_Buffer(0)));
      Ada.Text_IO.Put_Line ("    Idx(10) [Center]: " & float'Image(Host_Buffer(10)));
      
      -- Assert logic
      Assert (Host_Buffer(10) = 0.0, "Center point check failed");
      Assert (Host_Buffer(0) > 0.0, "Corner point check failed");

      -- Cleanup
      Res := cuMemFree(Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_FP64_Render;

end Test_CUDA_Engine;