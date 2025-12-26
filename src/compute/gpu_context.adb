with Interfaces.C; use Interfaces.C;
with Ada.Text_IO;
with System;

package body GPU_Context is

   -------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------
   procedure Initialize (Self : in out CUDA_Context) is
      Res     : CUresult;
      Dev     : aliased CUdevice;
      --  Dev_Cnt : aliased int;
   begin
      if Self.Initialized then
         return; -- Idempotency
      end if;

      -- 1. Initialize Driver
      Res := cuInit(0);
      if Res /= CUDA_SUCCESS then
         raise Program_Error with "cuInit failed: " & CUresult'Image(Res);
      end if;

      -- 2. Find Device
      Res := cuDeviceGet(Dev'Unchecked_Access, 0);
      if Res /= CUDA_SUCCESS then
         raise Program_Error with "cuDeviceGet failed: " & CUresult'Image(Res);
      end if;

      -- 3. Create Context
      Res := cuCtxCreate(Self.Handle'Unchecked_Access, CU_CTX_SCHED_AUTO, Dev);
      if Res /= CUDA_SUCCESS then
         raise Program_Error with "cuCtxCreate failed: " & CUresult'Image(Res);
      end if;

      Self.Initialized := True;
      Ada.Text_IO.Put_Line ("[GPU] Context Initialized on Device 0");
   end Initialize;

   -------------------------------------------------------------------------
   -- Finalize
   -------------------------------------------------------------------------
   procedure Finalize (Self : in out CUDA_Context) is
      Res : CUresult;
   begin
      if Self.Initialized then
         Res := cuCtxDestroy(Self.Handle);
         if Res /= CUDA_SUCCESS then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, 
               "[GPU] Warning: cuCtxDestroy failed: " & CUresult'Image(Res));
         else
            Ada.Text_IO.Put_Line ("[GPU] Context Destroyed");
         end if;
         Self.Initialized := False;
      end if;
   end Finalize;

   -------------------------------------------------------------------------
   -- Get_Handle
   -------------------------------------------------------------------------
   function Get_Handle (Self : CUDA_Context) return CUcontext is
   begin
      return Self.Handle;
   end Get_Handle;

end GPU_Context;