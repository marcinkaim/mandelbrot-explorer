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

with System;
with Interfaces.C; use Interfaces.C;

package Render_Interface is
   pragma Pure;

   type Tile_Description is record
      Offset_X : int;
      Offset_Y : int;
      Width    : int;
      Height   : int;
      World_X  : Long_Float;
      World_Y  : Long_Float;
      Zoom     : Long_Float;
   end record;

   type Compute_Engine is limited interface;

   procedure Initialize 
     (Self : in out Compute_Engine) is abstract;

   procedure Render_Tile
     (Self          : in out Compute_Engine;
      Desc          : Tile_Description;
      Target_Buffer : System.Address) is abstract;

   procedure Finalize (Self : in out Compute_Engine) is abstract;

end Render_Interface;