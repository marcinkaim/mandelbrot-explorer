with System;
with Interfaces.C; use Interfaces.C;

package Render_Interface is
   pragma Pure;

   -- Describes a rectangular region to render
   type Tile_Description is record
      -- Screen/Buffer coordinates
      Offset_X : int;
      Offset_Y : int;
      Width    : int;
      Height   : int;
      
      -- Fractal math coordinates (Top-Left of the tile)
      World_X  : Long_Float;
      World_Y  : Long_Float;
      Zoom     : Long_Float;
   end record;

   -- Abstract interface for any compute backend (Double, Quad, Oct).
   type Compute_Engine is limited interface;

   -- Initialize internal resources (modules, streams).
   procedure Initialize 
     (Self : in out Compute_Engine) is abstract;

   -- The workhorse. Renders a specific tile into the destination buffer.
   -- Target_Buffer must be a valid CUDA Device Pointer (CUdeviceptr) 
   -- obtained by mapping a Registered GL Buffer Object.
   -- The implementation assumes this memory is resident on the GPU.
   procedure Render_Tile
     (Self          : in out Compute_Engine;
      Desc          : Tile_Description;
      Target_Buffer : System.Address) is abstract;

   -- Clean up.
   procedure Finalize (Self : in out Compute_Engine) is abstract;

end Render_Interface;
