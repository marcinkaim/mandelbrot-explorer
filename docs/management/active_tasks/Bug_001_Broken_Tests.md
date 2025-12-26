# BUG REPORT: 001 - Fix Unit Test Execution

## 1. Context
After the implementation of Task 2.1 (Orchestrator), the CI/CD pipeline command `make test` fails.
The build logs indicate that the container runtime cannot find the executable `/mandelbrot-explorer/build/bin/test_driver`.

## 2. Diagnosis
* **Cause:** The project file `mandelbrot_explorer.gpr` currently defines `for Main use ("main.adb", "mandelbrot_suite.adb");`. This produces a binary named `mandelbrot_suite`.
* **Conflict:** The `Makefile` target `test` explicitly calls `.../build/bin/test_driver`.
* **Obsolescence:** `test_driver.adb` seems to be the old test runner, while `mandelbrot_suite.adb` is the new AUnit runner.

## 3. Objective
Restore the functionality of `make test`. All unit tests (CUDA bindings) must pass inside the container.

## 4. Key Responsibilities
1.  **Analyze Test Runners:** Determine if we should keep `test_driver.adb` or fully migrate to `mandelbrot_suite.adb`.
    * *Recommendation:* Use `mandelbrot_suite.adb` as the standard AUnit entry point.
2.  **Fix Makefile:** Update the `test` target to execute the correct binary.
3.  **Clean GPR:** Ensure `mandelbrot_explorer.gpr` builds the correct test binary.
4.  **Verify CUDA Context:** Ensure that running tests does not conflict with `GPU_Context` singleton if it's initialized differently in tests vs app.

## 5. Deliverables
* Updated `Makefile`.
* Updated `mandelbrot_explorer.gpr` (if needed).
* Successful execution log of `make test`.