with System;
with CUDA_Driver_API; use CUDA_Driver_API;

package GPU_Context is

   type CUDA_Context is tagged limited private;

   -- Initializes CUDA Driver, selects Device(0), and creates a Context.
   -- Raises Program_Error if any step fails.
   procedure Initialize (Self : in out CUDA_Context);

   -- Destroys the context and frees resources.
   procedure Finalize (Self : in out CUDA_Context);

   -- Accessor to the raw handle
   function Get_Handle (Self : CUDA_Context) return CUcontext;

private
   -- CUcontext is now a distinct type (new System.Address)
   -- Initialize to 0 (System.Null_Address equivalent for our new type)
   type CUDA_Context is tagged limited record
      Handle      : aliased CUcontext := CUcontext (System.Null_Address);
      Initialized : Boolean := False;
   end record;

end GPU_Context;