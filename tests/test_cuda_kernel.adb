with AUnit.Assertions; use AUnit.Assertions;
with CUDA_Driver_API;  use CUDA_Driver_API;
with Interfaces.C;     use Interfaces.C;
with System;
with Ada.Text_IO;

package body Test_CUDA_Kernel is

   -------------------------------------------------------------------------
   -- Helper: Init CUDA & Load Module
   -- Boilerplate reduction for tests
   -------------------------------------------------------------------------
   procedure Setup_Context_And_Module 
     (Ctx     : aliased out CUcontext;
      Module  : aliased out CUmodule;
      Dev_Ptr : aliased out CUdeviceptr) 
   is
      Res : CUresult;
      Dev : aliased CUdevice;
   begin
      Res := cuInit(0);
      Assert (Res = CUDA_SUCCESS, "cuInit fail");

      Res := cuDeviceGet(Dev'Access, 0);
      Assert (Res = CUDA_SUCCESS, "cuDeviceGet fail");

      Res := cuCtxCreate(Ctx'Access, CU_CTX_SCHED_AUTO, Dev);
      Assert (Res = CUDA_SUCCESS, "cuCtxCreate fail");

      -- Load our new PTX file
      declare
         Ptx_Path : char_array := To_C ("kernels/test_ops.ptx");
      begin
         Res := cuModuleLoad(Module'Access, Ptx_Path);
         Assert (Res = CUDA_SUCCESS, "cuModuleLoad fail (check if kernels/test_ops.ptx exists)");
      end;
      
      -- Initialize Dev_Ptr to 0 (safety)
      Dev_Ptr := 0; 
   end Setup_Context_And_Module;

   -------------------------------------------------------------------------
   -- Test 1: Memory Allocation & Transfer
   -------------------------------------------------------------------------
   procedure Test_Mem_Alloc (T : in out Test_Case) is
      Ctx     : aliased CUcontext;
      Dev_Ptr : aliased CUdeviceptr;
      Module  : aliased CUmodule; -- Unused here but part of setup
      Res     : CUresult;
      
      Value_To_Write : aliased int := 123456;
      Value_Read_Back : aliased int := 0;
   begin
      Setup_Context_And_Module (Ctx, Module, Dev_Ptr);

      -- 1. Alloc 4 bytes
      Res := cuMemAlloc(Dev_Ptr'Access, Value_To_Write'Size / 8);
      Assert (Res = CUDA_SUCCESS, "cuMemAlloc fail");
      Assert (Dev_Ptr /= 0, "Null pointer returned");

      -- 2. Write Host -> Device
      -- Dev_Ptr passed by Value (Handle)
      Res := cuMemcpyHtoD(Dev_Ptr, Value_To_Write'Address, Value_To_Write'Size / 8);
      Assert (Res = CUDA_SUCCESS, "cuMemcpyHtoD fail");

      -- 3. Read Device -> Host
      Res := cuMemcpyDtoH(Value_Read_Back'Address, Dev_Ptr, Value_Read_Back'Size / 8);
      Assert (Res = CUDA_SUCCESS, "cuMemcpyDtoH fail");

      -- 4. Verify
      Assert (Value_Read_Back = 123456, 
              "Memory integrity fail. Expected 123456, got: " & int'Image(Value_Read_Back));

      -- Cleanup
      Res := cuMemFree(Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Mem_Alloc;

   -------------------------------------------------------------------------
   -- Test 2: Kernel Arguments (Sum 8 Ints)
   -------------------------------------------------------------------------
   procedure Test_Kernel_Args (T : in out Test_Case) is
      Ctx     : aliased CUcontext;
      Dev_Ptr : aliased CUdeviceptr; -- Output buffer
      Module  : aliased CUmodule;
      Func    : aliased CUfunction;
      Res     : CUresult;

      -- Arguments
      A : aliased int := 1;
      B : aliased int := 8;
      C : aliased int := 5;
      D : aliased int := 4;
      E : aliased int := 3;
      F : aliased int := 6;
      G : aliased int := 7;
      H : aliased int := 2;
      
      Result_Host : aliased int := 0;
      Expected_Sum : constant int := 36; -- 1+2+...+8

      -- Params Array
      type Param_Array is array (0 .. 8) of System.Address;
      Params : aliased Param_Array;
   begin
      Setup_Context_And_Module (Ctx, Module, Dev_Ptr);

      -- Get Function
      declare
         Func_Name : char_array := To_C ("test_sum_8_ints");
      begin
         Res := cuModuleGetFunction(Func'Access, Module, Func_Name);
         Assert (Res = CUDA_SUCCESS, "Function test_sum_8_ints not found");
      end;

      -- Alloc Output (1 int)
      Res := cuMemAlloc(Dev_Ptr'Access, Result_Host'Size / 8);
      Assert (Res = CUDA_SUCCESS, "Alloc fail");

      -- Prepare Arguments
      -- Notice: We pass 'Address of the variable holding the value
      -- For Dev_Ptr, it is the address of the 64-bit handle variable
      Params(0) := A'Address;
      Params(1) := B'Address;
      Params(2) := C'Address;
      Params(3) := D'Address;
      Params(4) := E'Address;
      Params(5) := F'Address;
      Params(6) := G'Address;
      Params(7) := H'Address;
      Params(8) := Dev_Ptr'Address; -- Pointer to the pointer handle

      -- Launch (1 thread)
      Res := cuLaunchKernel
        (f              => Func,
         gridDimX       => 1, gridDimY => 1, gridDimZ => 1,
         blockDimX      => 1, blockDimY => 1, blockDimZ => 1,
         sharedMemBytes => 0,
         hStream        => CUstream (System.Null_Address),
         kernelParams   => Params'Address,
         extra          => System.Null_Address);
      Assert (Res = CUDA_SUCCESS, "Launch fail: " & CUresult'Image(Res));

      Res := cuCtxSynchronize;
      Assert (Res = CUDA_SUCCESS, "Sync fail");

      -- Read Result
      Res := cuMemcpyDtoH(Result_Host'Address, Dev_Ptr, Result_Host'Size / 8);
      
      Assert (Result_Host = Expected_Sum, 
              "Sum Invalid. Expected 36, got: " & int'Image(Result_Host));

      -- Cleanup
      Res := cuMemFree(Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Kernel_Args;

   -------------------------------------------------------------------------
   -- Test 3: Pixel Buffer Pattern
   -------------------------------------------------------------------------
   procedure Test_Pixel_Buffer (T : in out Test_Case) is
      Ctx     : aliased CUcontext;
      Dev_Ptr : aliased CUdeviceptr;
      Module  : aliased CUmodule;
      Func    : aliased CUfunction;
      Res     : CUresult;

      -- Buffer Dimensions
      W : aliased int := 16;
      H : aliased int := 16;
      Num_Pixels : constant int := 16 * 16;
      Buffer_Size : constant size_t := size_t(Num_Pixels * 4); -- 4 bytes per int

      -- Host Buffer for validation
      type Host_Buffer_Type is array (0 .. Num_Pixels - 1) of aliased int;
      Result_Buffer : aliased Host_Buffer_Type := (others => 0);

      type Param_Array is array (0 .. 2) of System.Address;
      Params : aliased Param_Array;
   begin
      Setup_Context_And_Module (Ctx, Module, Dev_Ptr);

      -- Get Function
      declare
         Func_Name : char_array := To_C ("test_fill_buffer");
      begin
         Res := cuModuleGetFunction(Func'Access, Module, Func_Name);
         Assert (Res = CUDA_SUCCESS, "Function test_fill_buffer not found");
      end;

      -- Alloc VRAM
      Res := cuMemAlloc(Dev_Ptr'Access, Buffer_Size);
      Assert (Res = CUDA_SUCCESS, "Alloc fail");

      -- Prepare Args
      Params(0) := W'Address;
      Params(1) := H'Address;
      Params(2) := Dev_Ptr'Address;

      -- Launch 
      -- We need enough threads. Let's use 1 block of 16x16 threads.
      Res := cuLaunchKernel
        (f              => Func,
         gridDimX       => 1, gridDimY => 1, gridDimZ => 1,
         blockDimX      => unsigned(W), blockDimY => unsigned(H), blockDimZ => 1,
         sharedMemBytes => 0,
         hStream        => CUstream (System.Null_Address),
         kernelParams   => Params'Address,
         extra          => System.Null_Address);
      Assert (Res = CUDA_SUCCESS, "Launch fail: " & CUresult'Image(Res));

      Res := cuCtxSynchronize;
      Assert (Res = CUDA_SUCCESS, "Sync fail");

      -- Read Back
      Res := cuMemcpyDtoH(Result_Buffer(0)'Address, Dev_Ptr, Buffer_Size);
      Assert (Res = CUDA_SUCCESS, "Readback fail");

      -- Verify Pattern
      -- Logic in kernel: output[y*w + x] = y*w + x (linear index)
      for I in 0 .. Num_Pixels - 1 loop
         Assert (Result_Buffer(I) = int(I), 
                 "Pixel Mismatch at index " & int'Image(int(I)) & 
                 ". Expected " & int'Image(int(I)) & 
                 ", got " & int'Image(Result_Buffer(I)));
      end loop;

      Ada.Text_IO.Put_Line ("    [INFO] Verified 256 pixels pattern successfully.");

      -- Cleanup
      Res := cuMemFree(Dev_Ptr);
      Res := cuCtxDestroy(Ctx);
   end Test_Pixel_Buffer;
   
   -------------------------------------------------------------------------
   -- Legacy Test (Optional, included for compatibility)
   -------------------------------------------------------------------------
   procedure Test_Kernel_Launch (T : in out Test_Case) is
   begin
      -- Reuse the Args test logic or keep simple
      Test_Kernel_Args(T);
   end Test_Kernel_Launch;

end Test_CUDA_Kernel;