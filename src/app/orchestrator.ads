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
      -- Worker startuje i otrzymuje referencje do Kolejki oraz Kontekstu GPU
      entry Start (Queue   : Job_Queue_Access;
                   Context : Context_Access);
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
      VAO_Handle     : unsigned := 0;
      PBO_Handle     : unsigned := 0; -- GL Pixel Buffer Object
      Texture_Handle : unsigned := 0; -- GL Texture Atlas
      
      -- CUDA Resource Handle (Registered PBO)
      CUDA_PBO_Resource : System.Address := System.Null_Address;

      -- UI State
      Width      : int := 800;
      Height     : int := 600;
      Zoom_Level : Long_Float := 1.0;
      Is_Running : Boolean := False;
   end record;

end Orchestrator;