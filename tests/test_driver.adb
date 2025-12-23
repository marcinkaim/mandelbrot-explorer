with AUnit.Reporter.Text;
with AUnit.Run;
with Mandelbrot_Suite; -- Ten pakiet stworzymy za chwilę

procedure Test_Driver is
   use AUnit.Run;
   use AUnit.Reporter.Text;

   procedure Run_Tests is new Test_Runner (Mandelbrot_Suite.Suite);
   Reporter : Text_Reporter;
begin
   Set_Use_ANSI_Colors (Reporter, True);
   Run_Tests (Reporter);
end Test_Driver;