with AUnit.Test_Fixtures;

package Test_CUDA_Graph is
   type Test_Case is new AUnit.Test_Fixtures.Test_Fixture with null record;
   procedure Test_Simple_Graph_Execution (T : in out Test_Case);
   procedure Test_Dependent_Graph_Execution (T : in out Test_Case);
end Test_CUDA_Graph;