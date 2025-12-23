with Test_CUDA_Init;
with Test_CUDA_Kernel;
with AUnit.Test_Caller;

package body Mandelbrot_Suite is
   use AUnit.Test_Suites;

   package Init_Caller is new AUnit.Test_Caller (Test_CUDA_Init.Test_Case);
   package Kernel_Caller is new AUnit.Test_Caller (Test_CUDA_Kernel.Test_Case);

   function Suite return Access_Test_Suite is
      Result : constant Access_Test_Suite := new Test_Suite;
   begin
      Result.Add_Test 
        (Init_Caller.Create ("CUDA Driver Init & MemAlloc", Test_CUDA_Init.Test_Driver_Initialization'Access));
      
      Result.Add_Test 
        (Kernel_Caller.Create ("CUDA Kernel Launch (PTX)", Test_CUDA_Kernel.Test_Kernel_Launch'Access));

      return Result;
   end Suite;
end Mandelbrot_Suite;