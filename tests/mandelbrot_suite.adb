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

with Test_CUDA_Engine;
with Test_CUDA_Init;
with Test_CUDA_Kernel;
with Test_CUDA_Graph;
with AUnit.Test_Caller;

package body Mandelbrot_Suite is
   use AUnit.Test_Suites;

   package Init_Caller is new AUnit.Test_Caller (Test_CUDA_Init.Test_Case);
   package Kernel_Caller is new AUnit.Test_Caller (Test_CUDA_Kernel.Test_Case);
   package Graph_Caller is new AUnit.Test_Caller (Test_CUDA_Graph.Test_Case);
   package CUDA_Engine_Caller is new AUnit.Test_Caller (Test_CUDA_Engine.Test_Case);

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      -- Init Tests
      Result.Add_Test 
        (Init_Caller.Create ("CUDA Driver Init & MemAlloc", Test_CUDA_Init.Test_Driver_Initialization'Access));

      -- Kernel Tests (NEW)
      Result.Add_Test 
        (Kernel_Caller.Create ("CUDA Mem Alloc & Transfer", Test_CUDA_Kernel.Test_Mem_Alloc'Access));
      
      Result.Add_Test 
        (Kernel_Caller.Create ("CUDA Kernel Args (Sum 8 Ints)", Test_CUDA_Kernel.Test_Kernel_Args'Access));
      
      Result.Add_Test 
        (Kernel_Caller.Create ("CUDA Pixel Buffer Fill", Test_CUDA_Kernel.Test_Pixel_Buffer'Access));

      -- Graph Tests
      Result.Add_Test
        (Graph_Caller.Create ("CUDA Graph Execution (Simple)", Test_CUDA_Graph.Test_Simple_Graph_Execution'Access));
      
      Result.Add_Test
        (Graph_Caller.Create ("CUDA Graph Execution (Dependent Chain)", Test_CUDA_Graph.Test_Dependent_Graph_Execution'Access));

      Result.Add_Test
        (CUDA_Engine_Caller.Create ("CUDA Engine (FP64)", Test_CUDA_Engine.Test_FP64_Render'Access));

      return Result;
   end Suite;

end Mandelbrot_Suite;