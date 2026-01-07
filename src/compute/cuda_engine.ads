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

with Render_Interface;
with GPU_Context;
with CUDA_Driver_API; use CUDA_Driver_API;
with Interfaces.C; use Interfaces.C;
with System;

package CUDA_Engine is
   
   type Engine is new Render_Interface.Compute_Engine with private;

   -- Initializes the Engine (Loads PTX). 
   -- Pre: GPU_Context.Initialize has been called.
   overriding procedure Initialize (Self : in out Engine);

   -- Renders a tile using CUDA.
   -- Target_Buffer: Must be a CUgraphicsResource (System.Address) for the PBO.
   overriding procedure Render_Tile 
     (Self          : in out Engine; 
      Desc          : Render_Interface.Tile_Description; 
      Target_Buffer : System.Address);

   overriding procedure Finalize (Self : in out Engine);

private
   type Engine is new Render_Interface.Compute_Engine with record
       Module      : aliased CUmodule;
       Kernel      : aliased CUfunction;
       Initialized : Boolean := False;
   end record;

end CUDA_Engine;
