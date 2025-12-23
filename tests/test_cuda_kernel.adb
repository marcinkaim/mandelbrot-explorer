with AUnit.Assertions; use AUnit.Assertions;
with CUDA_Driver_API;  use CUDA_Driver_API;
with Interfaces.C;     use Interfaces.C;
with System;

package body Test_CUDA_Kernel is

   procedure Test_Kernel_Launch (T : in out Test_Case) is
      Res           : CUresult;
      Dev           : aliased CUdevice;
      Ctx           : aliased CUcontext;
      Module_Handle : aliased CUmodule;
      Func          : aliased CUfunction;
      Dev_Ptr       : aliased CUdeviceptr;
      
      Host_Input  : aliased int := 10;
      Host_Output : aliased int := 0;
      
      type Param_Array is array (0 .. 0) of System.Address;
      Params : aliased Param_Array;
   begin
      -- 1. Setup
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit fail: " & CUresult'Image(Res));
      
      Res := cuDeviceGet(Dev'Access, 0);
      Assert (Res = CUDA_SUCCESS, "cuDeviceGet fail: " & CUresult'Image(Res));
      
      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);
      Assert (Res = CUDA_SUCCESS, "cuCtxCreate fail: " & CUresult'Image(Res));

      -- 2. Load Module
      declare
         Ptx_Path  : char_array := To_C ("kernels/test_kernel.ptx");
         Func_Name : char_array := To_C ("test_inc");
      begin
         Res := cuModuleLoad(Module_Handle'Access, Ptx_Path);
         Assert (Res = CUDA_SUCCESS, "cuModuleLoad fail: " & CUresult'Image(Res));
         
         Res := cuModuleGetFunction(Func'Access, Module_Handle, Func_Name);
         Assert (Res = CUDA_SUCCESS, "cuModuleGetFunction fail: " & CUresult'Image(Res));
      end;

      -- 3. Alloc & Copy
      Res := cuMemAlloc(Dev_Ptr'Access, size_t(Host_Input'Size / 8));
      Assert (Res = CUDA_SUCCESS, "cuMemAlloc fail: " & CUresult'Image(Res));

      Res := cuMemcpyHtoD(Dev_Ptr, Host_Input'Address, size_t(Host_Input'Size / 8));
      Assert (Res = CUDA_SUCCESS, "cuMemcpyHtoD fail: " & CUresult'Image(Res));

      -- 4. Kernel Launch
      Params(0) := Dev_Ptr'Address;

      Res := cuLaunchKernel
        (f              => Func,
         gridDimX       => 1, gridDimY => 1, gridDimZ => 1,
         blockDimX      => 1, blockDimY => 1, blockDimZ => 1,
         sharedMemBytes => 0,
         hStream        => CUstream (System.Null_Address),
         kernelParams   => Params'Address,
         extra          => System.Null_Address);
      
      Assert (Res = CUDA_SUCCESS, "cuLaunchKernel fail: " & CUresult'Image(Res));

      -- Explicit Synchronization to catch Launch Failures
      Res := cuCtxSynchronize;
      Assert (Res = CUDA_SUCCESS, "Kernel Execution Failed (cuCtxSynchronize): " & CUresult'Image(Res));

      -- 5. Copy & Verify
      Res := cuMemcpyDtoH(Host_Output'Address, Dev_Ptr, size_t(Host_Output'Size / 8));
      Assert (Res = CUDA_SUCCESS, "cuMemcpyDtoH fail: " & CUresult'Image(Res));

      Assert (Host_Output = 11, "Logic Error. Expected 11, got: " & int'Image(Host_Output));

      -- 6. Cleanup
      Res := cuMemFree(Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Kernel_Launch;

end Test_CUDA_Kernel;