with AUnit.Assertions; use AUnit.Assertions;
with CUDA_Driver_API; use CUDA_Driver_API;
with Interfaces.C;      use Interfaces.C;

package body Test_CUDA_Init is

   procedure Test_Driver_Initialization (T : in out Test_Case) is
      Res     : CUresult;
      Dev     : aliased CUdevice;
      Ctx     : aliased CUcontext;
      Dev_Ptr : aliased CUdeviceptr;
   begin
      -- 1. Initialize Driver
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit failed! Error code: " & CUresult'Image(Res));

      -- 2. Get Device 0
      Res := cuDeviceGet(Dev'Access, 0);
      Assert (Res = CUDA_SUCCESS, "cuDeviceGet failed!");

      -- 3. Create Context
      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);
      Assert (Res = CUDA_SUCCESS, "cuCtxCreate failed!");

      -- 4. Alloc 64 bytes (Sanity check for memory controller)
      Res := cuMemAlloc(Dev_Ptr'Access, 64);
      Assert (Res = CUDA_SUCCESS, "cuMemAlloc failed!");

      -- 5. Cleanup
      Res := cuMemFree(Dev_Ptr);
      Assert (Res = CUDA_SUCCESS, "cuMemFree failed!");

      Res := cuCtxDestroy(Ctx);
      Assert (Res = CUDA_SUCCESS, "cuCtxDestroy failed!");
   end Test_Driver_Initialization;

end Test_CUDA_Init;