with Ada.Text_IO;
with Ada.Exceptions;
with Orchestrator;

procedure Main is
   App : Orchestrator.Controller;
begin
   Ada.Text_IO.Put_Line ("[Main] Booting Mandelbrot Explorer...");

   begin
      App.Initialize;

      App.Run_UI_Loop;

      App.Shutdown;

      Ada.Text_IO.Put_Line ("[Main] Exited cleanly.");
      
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, 
            "[Main] CRITICAL CRASH: " & Ada.Exceptions.Exception_Information (Error));
         
         begin
            App.Shutdown;
         exception
            when others => null;
         end;
   end;
end Main;