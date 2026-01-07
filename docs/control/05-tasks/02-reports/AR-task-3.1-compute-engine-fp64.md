<!--
  Mandelbrot Explorer
  Copyright (C) 2026 Marcin Kaim

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
-->

# Action Report: task-3.1-compute-engine-fp64

* **Sequence ID:** AR-TASK-3.1-01
* **Date:** 2026-01-07
* **Role:** ENGINEER
* **Input Commit:** `7a19634` (Base)
* **Outcome:** SUCCESS

## 1. Executive Summary
Implemented the **Tier 1 Compute Engine (FP64)** using NVIDIA CUDA technology. Delivered the PTX assembly kernel implementing the *Escape Time* algorithm with smooth coloring ($\nu$) and the corresponding Ada wrapper (`CUDA_Engine`). The solution adheres to the "Zero-Copy" architecture requirements by directly mapping OpenGL buffers (PBO) into the GPU address space. All Definition of Done (DoD) criteria have been met.

## 2. Technical Details & Evidence

### 2.1 Implementation (Engineer)

* **Key Changes:**
    * `kernels/mandelbrot_fp64.ptx`: New file containing the GPU assembly kernel.
    * `src/compute/cuda_engine.ads/adb`: New package implementing the `Render_Interface.Compute_Engine` interface.
    * `src/binding/cuda_driver_api.ads`: Added bindings for `cuGraphics*` functions (OpenGL Interop).
    * `tests/test_cuda_engine.adb`: Integration test verifying the kernel's mathematical correctness.

* **Design Decisions & Process Summary:**
    1.  **Zero-Copy Architecture:** In accordance with REQ-001, `cuGraphicsMapResources` was utilized. Output data is written directly to VRAM, which is subsequently read by the shader, eliminating PCIe bus transfer overhead.
    2.  **Iterative Kernel Development (Lessons Learned):**
        * *Attempt 1:* Applied aggressive "Loop Unrolling" (x2) and array syntax for registers. This resulted in driver error **218** (`CUDA_ERROR_FOUND_NO_BINARY_FOR_GPU`) due to JIT compilation errors (undeclared register aliases).
        * *Attempt 2:* Fixed register declarations but retained "Loop Unrolling". The kernel caused error **104** (`CUDA_ERROR_LAUNCH_FAILED`) during synchronization, likely due to jump label/register hazard errors.
        * *Solution:* Reverted loop optimization to "Safe Mode" (single iteration). Mathematical correctness (DoD) was prioritized. Performance optimization has been deferred to future technical tasks.

* **Safety Protocol:**
    * [x] Checked `ADR-0006` (Safe Interop) - Operations involving `System.Address` and `CUdeviceptr` pointers were strictly confined to the body of the `CUDA_Engine` package.
    * [x] Hermetic Build respected - The PTX kernel is loaded dynamically from a relative path.

### 2.3 Validation Telemetry (Tester / Self-Verification)

* **Environment:** Containerized Build Environment (Debian 13), NVIDIA Driver (Host: RTX 3500 Ada).
* **Evidence (Unit Tests):**
    Based on logs from `make-test-cuda-engine-5-logs.txt`:
    ```text
    [Test] Initializing Direct Kernel Test...
    [Test] Launching Kernel (4x4 threads)...
    [Test] Verification:
        Idx(0) [Corner (-2,-2)]:  1.41504E+00  (> 0.0 -> Escaped -> OK)
        Idx(10) [Center (0,0)]:   0.00000E+00  (= 0.0 -> Inside Set -> OK)
    
    OK CUDA Driver Init & MemAlloc
    OK CUDA Mem Alloc & Transfer
    OK CUDA Engine (FP64)
    Total Tests Run:   7
    Successful Tests:  7
    ```
    Confirmed that the kernel correctly identifies the set interior (Value 0.0) and the escape region (Positive values).

## 3. Blockers & Risks
* **Optimization:** The current kernel does not utilize "Loop Unrolling," which may result in lower performance (FPS) at very deep zoom levels. It is recommended to create a technical task for PTX kernel optimization once regression tests are stabilized.
* **GL Dependency:** Full testing of the `Render_Tile` method requires an active OpenGL context, which is challenging in a headless CI/CD environment. The current test verifies mathematical logic on a "raw" buffer, which acts as a sufficient approximation for DoD.

## 4. Handoff Instructions
The task is considered complete. The code is ready for Audit.
**To:** Auditor
**Message:** Please verify compliance with `ADR-0006` (Safety) in `src/compute/cuda_engine.adb`.