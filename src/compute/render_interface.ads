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