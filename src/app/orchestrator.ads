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
with Render_Interface;
with GPU_Context;
with SDL2_Thin;    use SDL2_Thin;

package Orchestrator is

   type Controller is tagged limited private;

   procedure Initialize (Self : in out Controller);
   procedure Run_UI_Loop (Self : in out Controller);
   procedure Shutdown (Self : in out Controller);

private

   type Job_Buffer_Array is array (1 .. 100) of Render_Interface.Tile_Description;

   -- Protected Job Queue (Thread-Safe)
   protected type Job_Queue is
      procedure Push (Job : Render_Interface.Tile_Description);
      entry Pop (Job : out Render_Interface.Tile_Description);
      function Length return Natural;
   private
      Jobs : Job_Buffer_Array;
      Head : Natural := 1;
      Tail : Natural := 1;
      Count : Natural := 0;
   end Job_Queue;

   type Job_Queue_Access is access all Job_Queue;
   pragma No_Strict_Aliasing (Job_Queue_Access);

   type Context_Access   is access all GPU_Context.CUDA_Context;
   pragma No_Strict_Aliasing (Context_Access);

   -- The Worker Task
   task type Compute_Worker is
      entry Start (Queue   : Job_Queue_Access;
                   Context : Context_Access);
      entry Pause;
      entry Resume;
      
      entry Stop;
   end Compute_Worker;

   type Controller is tagged limited record
      -- Concurrency & Compute
      Queue   : aliased Job_Queue;
      Context : aliased GPU_Context.CUDA_Context; -- Orchestrator owns the context data
      Worker  : Compute_Worker;

      -- Resources (Handles)
      Window_Handle  : SDL_Window_Ptr := null;
      GL_Context     : SDL_GLContext  := SDL_GLContext (System.Null_Address);

      -- OpenGL Objects
      VAO_Handle     : aliased unsigned := 0;
      VBO_Handle     : aliased unsigned := 0;
      PBO_Handle     : aliased unsigned := 0; -- GL Pixel Buffer Object
      Texture_Handle : aliased unsigned := 0;
      Program_Handle : unsigned := 0;

      -- GL Texture Atlas
      
      -- CUDA Resource Handle (Registered PBO)
      CUDA_PBO_Resource : System.Address := System.Null_Address;

      -- UI State
      Width      : int := 800;
      Height     : int := 600;
      Zoom_Level : Long_Float := 1.0;
      Is_Running : Boolean := False;
   end record;

end Orchestrator;