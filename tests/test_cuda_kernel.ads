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

with AUnit.Test_Fixtures;

package Test_CUDA_Kernel is
   type Test_Case is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Kernel_Launch (T : in out Test_Case);

   procedure Test_Mem_Alloc (T : in out Test_Case);

   procedure Test_Kernel_Args (T : in out Test_Case);

   procedure Test_Pixel_Buffer (T : in out Test_Case);
end Test_CUDA_Kernel;