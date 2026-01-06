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
      -- Dev is aliased, passed by access (OUT)
      Res := cuDeviceGet(Dev'Access, 0);
      if Res /= CUDA_SUCCESS then
         raise Program_Error with "cuDeviceGet failed: " & CUresult'Image(Res);
      end if;

      -- 3. Create Context
      -- Handle is aliased in the record, passed by access (OUT)
      Res := cuCtxCreate(Self.Handle'Access, CU_CTX_SCHED_AUTO, Dev);
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
         -- Handle is passed by Value (IN)
         Res := cuCtxDestroy(Self.Handle);
         
         if Res /= CUDA_SUCCESS then
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, 
               "[GPU] Warning: cuCtxDestroy failed: " & CUresult'Image(Res));
         else
            Ada.Text_IO.Put_Line ("[GPU] Context Destroyed");
         end if;
         
         Self.Initialized := False;
         Self.Handle := CUcontext (System.Null_Address);
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