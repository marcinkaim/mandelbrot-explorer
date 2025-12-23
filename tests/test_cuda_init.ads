with AUnit.Test_Fixtures;

package Test_CUDA_Init is
   type Test_Case is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Driver_Initialization (T : in out Test_Case);
end Test_CUDA_Init;