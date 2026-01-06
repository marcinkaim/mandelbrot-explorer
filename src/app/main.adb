--------------------------------------------------------------------------------
--  
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