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
with CUDA_Driver_API; use CUDA_Driver_API;
with Interfaces.C;      use Interfaces.C;
with System;

package body Test_CUDA_Init is

   procedure Test_Driver_Initialization (T : in out Test_Case) is
      Res     : CUresult;
      Dev     : aliased CUdevice;
      Ctx     : aliased CUcontext;
      Dev_Ptr : aliased CUdeviceptr; -- Now Unsigned_64
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
      -- Returns pointer in Dev_Ptr via access
      Res := cuMemAlloc(Dev_Ptr'Access, 64);
      Assert (Res = CUDA_SUCCESS, "cuMemAlloc failed!");
      
      -- Basic check: Pointer should not be 0
      Assert (Dev_Ptr /= 0, "Allocated pointer is NULL!");

      -- 5. Cleanup
      -- Pass by Value
      Res := cuMemFree(Dev_Ptr);
      Assert (Res = CUDA_SUCCESS, "cuMemFree failed!");

      Res := cuCtxDestroy(Ctx);
      Assert (Res = CUDA_SUCCESS, "cuCtxDestroy failed!");
   end Test_Driver_Initialization;

end Test_CUDA_Init;