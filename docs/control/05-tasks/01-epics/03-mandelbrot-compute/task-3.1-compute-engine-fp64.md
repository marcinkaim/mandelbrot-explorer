# TASK DEFINITION

## METADATA
* **Task ID:** task-3.1-compute-engine-fp64
* **Epic:** Epic 3: The Mandelbrot Engine
* **Type:** Feature
* **Status:** PENDING
* **Priority:** Critical

## 1. Objective
Implement the Tier 1 Compute Engine using NVIDIA CUDA (FP64). This includes writing the PTX kernel assembly for the Mandelbrot iteration and the corresponding high-level Ada wrapper implementing the `Render_Interface.Compute_Engine` interface.

## 2. Context Injection (Required Reading)
* `docs/control/01-requirements/REQ-001-functional-core.md` (Math formulas & Raw Data principle)
* `docs/control/04-architecture/ADR-0006-safe-interop-boundaries.md` (Safety rules for System.Address)
* `docs/control/04-architecture/ADR-0007-hardware-resilience.md` (Exception handling strategy)
* `src/compute/render_interface.ads` (The interface to implement)

## 3. Key Responsibilities

### 3.1. The Kernel (`kernels/mandelbrot_fp64.ptx`)
* Implement the kernel in PTX assembly manually to ensure optimal register usage.
* **Algorithm:** Escape time algorithm with smooth coloring potential ($\nu$).
* **Precision:** Standard IEEE 754 Double Precision (`f64`).
* **Optimization:** Apply loop unrolling for the main iteration loop.
* **Signature:** Must accept parameters strictly matching the host wrapper (Coordinate limits, Steps, Output Pointer, Width).

### 3.2. The Wrapper (`src/compute/cuda_engine.ads/adb`)
* Implement a tagged type `CUDA_Engine` implementing `Render_Interface.Compute_Engine`.
* **Initialization:**
    * Load the PTX module using `cuModuleLoad`.
    * Retrieve the kernel function handle.
* **Rendering (`Render_Tile`):**
    * **Constraint (Arch Directive):** Implement "Per-Tile Mapping". The PBO must be mapped (`cuGraphicsMapResources`), written to by the kernel, and immediately unmapped (`cuGraphicsUnmapResources`) within the scope of a single tile render. **Do not** hold a persistent mapping across frames to avoid resizing deadlocks.
    * **Configuration:** Use a hardcoded Block Dimension of **16x16** (256 threads) defined as a named constant in `GPU_Context`. Do not use magic numbers.

## 4. Constraints & Non-Functional Requirements
* **Safety:** All usage of `System.Address` or pointer arithmetic must be located in the package body and tagged with `-- SAFETY: ADR-0006`.
* **Resilience:** If `cuLaunchKernel` fails (e.g., TDR watchdog), catch the error code and raise a typed `GPU_Context.Device_Error` exception to allow fallback handling.
* **Testing:** Integration with `Test_CUDA_Kernel` is required.

## 5. Definition of Done (DoD)
* [ ] `mandelbrot_fp64.ptx` compiles and loads successfully.
* [ ] `CUDA_Engine` compiles without warnings.
* [ ] Unit test confirms kernel writes valid data to a dummy buffer.
* [ ] Action Report generated with evidence of successful kernel launch.
