--------------------------------------------------------------------------------
--  
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

with GNAT.Directory_Operations;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Text_IO;

package body CUDA_Engine is

   -------------------------------------------------------------------------
   -- Initialize
   -------------------------------------------------------------------------
   overriding procedure Initialize (Self : in out Engine) is
      Res      : CUresult;
      -- Use relative path; build system must ensure kernels/ is present in CWD
      Ptx_Path : char_array := To_C ("kernels/mandelbrot_fp64.ptx");
      Func_Name: char_array := To_C ("mandelbrot_fp64_kernel");
   begin
      if Self.Initialized then
         return;
      end if;

      Ada.Text_IO.Put_Line ("[CUDA_Engine] Loading PTX Module from: kernels/mandelbrot_fp64.ptx");

      -- Load Module
      Res := cuModuleLoad (Self.Module'Access, Ptx_Path);
      if Res /= CUDA_SUCCESS then
         -- Improved Diagnostics
         Ada.Text_IO.Put_Line ("[CUDA_Engine] ERROR: Failed to load PTX module.");
         Ada.Text_IO.Put_Line ("    Error Code: " & CUresult'Image(Res));
         Ada.Text_IO.Put_Line ("    CWD: " & GNAT.Directory_Operations.Get_Current_Dir);
         
         if Res = 218 then
            Ada.Text_IO.Put_Line ("    Hint: Error 218 usually means PTX Architecture/Version mismatch.");
            Ada.Text_IO.Put_Line ("          Ensure kernels/mandelbrot_fp64.ptx targets sm_35+ and version 6.0+.");
         end if;
         
         raise Program_Error with "Failed to load PTX module: " & CUresult'Image(Res);
      end if;

      -- Get Function Handle
      Res := cuModuleGetFunction (Self.Kernel'Access, Self.Module, Func_Name);
      if Res /= CUDA_SUCCESS then
         raise Program_Error with "Failed to get kernel function: " & CUresult'Image(Res);
      end if;

      Self.Initialized := True;
      Ada.Text_IO.Put_Line ("[CUDA_Engine] Initialized Successfully.");
   end Initialize;

   -------------------------------------------------------------------------
   -- Render_Tile
   -------------------------------------------------------------------------
   overriding procedure Render_Tile 
     (Self          : in out Engine; 
      Desc          : Render_Interface.Tile_Description; 
      Target_Buffer : System.Address) 
   is
      Res            : CUresult;
      Resource       : CUgraphicsResource := CUgraphicsResource (Target_Buffer);
      Mapped_Ptr     : aliased CUdeviceptr;
      Size_Ignored   : aliased size_t;
      
      -- Kernel Parameters
      Step     : Long_Float;
      Min_X    : Long_Float;
      Min_Y    : Long_Float;
      
      -- Arguments for Launch
      Arg_Min_X    : aliased Long_Float;
      Arg_Min_Y    : aliased Long_Float;
      Arg_Step     : aliased Long_Float;
      Arg_Width    : aliased int := Desc.Width;
      Arg_Max_Iter : aliased int := 1024; -- Hardcoded for Task 3.1
      Arg_Out_Ptr  : aliased CUdeviceptr;
      
      type Param_Array is array (0 .. 5) of System.Address;
      Params : aliased Param_Array;
      
      Block_Size : constant unsigned := 16;
      Grid_W     : unsigned;
      Grid_H     : unsigned;
   begin
      -- 1. Map Resource (Per-Tile Mapping Constraint)
      Res := cuGraphicsMapResources (1, Resource'Address, CUstream(System.Null_Address));
      if Res /= CUDA_SUCCESS then
         Ada.Text_IO.Put_Line ("[CUDA_Engine] Map Failed: " & CUresult'Image(Res));
         -- We do not raise here to allow resilience loop to continue, but log error
         return; 
      end if;

      Res := cuGraphicsResourceGetMappedPointer (Mapped_Ptr'Access, Size_Ignored'Access, Resource);
      if Res /= CUDA_SUCCESS then
         -- Must Unmap if GetPointer fails
         Res := cuGraphicsUnmapResources (1, Resource'Address, CUstream(System.Null_Address));
         Ada.Text_IO.Put_Line ("[CUDA_Engine] GetMappedPointer Failed: " & CUresult'Image(Res));
         return;
      end if;

      -- 2. Calculate Math Params
      -- Step = 4.0 / Zoom (Assumption: Zoom 1.0 covers range 4.0 units)
      if Desc.Zoom > 0.0 then
         Step := 4.0 / Desc.Zoom;
      else
         Step := 0.01; -- Fallback safety
      end if;
      
      -- Min_X = World_X - (Width/2) * Step
      Min_X := Desc.World_X - (Long_Float(Desc.Width) * 0.5) * Step;
      Min_Y := Desc.World_Y - (Long_Float(Desc.Height) * 0.5) * Step;

      -- 3. Prepare Arguments
      Arg_Min_X   := Min_X;
      Arg_Min_Y   := Min_Y;
      Arg_Step    := Step;
      Arg_Out_Ptr := Mapped_Ptr;

      Params(0) := Arg_Min_X'Address;
      Params(1) := Arg_Min_Y'Address;
      Params(2) := Arg_Step'Address;
      Params(3) := Arg_Width'Address;
      Params(4) := Arg_Max_Iter'Address;
      Params(5) := Arg_Out_Ptr'Address; 

      -- 4. Calculate Grid
      Grid_W := (unsigned(Desc.Width) + Block_Size - 1) / Block_Size;
      Grid_H := (unsigned(Desc.Height) + Block_Size - 1) / Block_Size;

      -- 5. Launch Kernel
      Res := cuLaunchKernel
        (f              => Self.Kernel,
         gridDimX       => Grid_W,
         gridDimY       => Grid_H,
         gridDimZ       => 1,
         blockDimX      => Block_Size,
         blockDimY      => Block_Size,
         blockDimZ      => 1,
         sharedMemBytes => 0,
         hStream        => CUstream(System.Null_Address),
         kernelParams   => Params'Address,
         extra          => System.Null_Address);

      if Res /= CUDA_SUCCESS then
         Ada.Text_IO.Put_Line ("[CUDA_Engine] Kernel Launch Failed: " & CUresult'Image(Res));
         -- Attempt Unmap before returning
         Res := cuGraphicsUnmapResources (1, Resource'Address, CUstream(System.Null_Address));
         return; 
      end if;
      
      -- 6. Unmap
      Res := cuGraphicsUnmapResources (1, Resource'Address, CUstream(System.Null_Address));
   end Render_Tile;

   -------------------------------------------------------------------------
   -- Finalize
   -------------------------------------------------------------------------
   overriding procedure Finalize (Self : in out Engine) is
   begin
      null; 
   end Finalize;

end CUDA_Engine;