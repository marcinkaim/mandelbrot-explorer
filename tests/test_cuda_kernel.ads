with AUnit.Test_Fixtures;

package Test_CUDA_Kernel is
   type Test_Case is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Kernel_Launch (T : in out Test_Case);

   procedure Test_Mem_Alloc (T : in out Test_Case);

   procedure Test_Kernel_Args (T : in out Test_Case);

   procedure Test_Pixel_Buffer (T : in out Test_Case);
end Test_CUDA_Kernel;