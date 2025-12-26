# TASK ORDER: 3.1 Concrete Double-Precision Engine (CUDA)

## 1. Context Injection (Required Reading)
* `docs/management/00_PROJECT_CONTEXT.md`
* `docs/adr/0003-CUDA-driver-api.md`
* `docs/adr/0005-system-architecture.md` (PBO Interop details)
* `src/compute/render_interface.ads` (The interface to implement)

## 2. Objective
Implement the first real compute engine: `Double_Engine`. It uses hardware `double` precision to render the Mandelbrot set into a PBO.

## 3. Key Responsibilities
1.  **PTX Kernel (`kernels/mandelbrot_double.ptx`):**
    * Function: `mandelbrot_render_tile`.
    * Params: `Target_Ptr` (u64), `Width`, `Height`, `Center_X`, `Center_Y`, `Zoom`, `Max_Iter`.
    * Logic: Map thread ID to pixel -> Calculate Complex Coords -> Iterate -> Write ARGB Color.
2.  **Ada Engine (`src/compute/double_engine.ads/adb`):**
    * Implement `Render_Interface.Compute_Engine`.
    * **Initialize:** Load the PTX module.
    * **Render_Tile:**
        * Receive `Target_Buffer` (which is a mapped PBO Device Pointer from Orchestrator).
        * Update CUDA Graph or Kernel Params.
        * Launch execution.

## 4. Deliverables
* `src/compute/double_engine.ads` & `.adb`.
* `kernels/mandelbrot_double.ptx`.

## 5. Definition of Done
* Unit Test (`tests/test_double_engine.adb`) initializes the engine and renders a tile into a dummy CUDA buffer (simulating a PBO), verifying non-zero output.