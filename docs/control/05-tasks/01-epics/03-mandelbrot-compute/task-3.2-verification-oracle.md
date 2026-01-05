# TASK DEFINITION

## METADATA
* **Task ID:** task-3.2-verification-oracle
* **Epic:** Epic 3: The Mandelbrot Engine
* **Type:** Feature / Quality Assurance
* **Status:** PENDING
* **Priority:** High

## 1. Objective
Implement a "Source of Truth" verification suite to satisfy REQ-003. This mechanism will validate the mathematical correctness of the GPU Compute Engine by comparing its output against a reference CPU implementation.

## 2. Context Injection (Required Reading)
* `docs/control/01-requirements/REQ-003-non-functional.md` (Integrity requirements)
* `src/compute/render_interface.ads`

## 3. Key Responsibilities

### 3.1. Reference Implementation
* Create a pure Ada function (running on CPU) that calculates the Mandelbrot potential for a given complex coordinate.
* **Constraint:** Do not use external libraries or optimizations (SIMD). Focus on readability and mathematical correctness over speed.

### 3.2. AUnit Integration (`tests/mandelbrot_suite.adb`)
* Implement a new test case `Test_Mathematical_Correctness`.
* **Logic:**
    1. Initialize the `CUDA_Engine` (Task 3.1 artifact).
    2. Render a small tile (e.g., 16x16 pixels) on the GPU.
    3. Calculate the expected values for the same coordinates using the Reference CPU implementation.
    4. Assert that `abs(GPU_Value - CPU_Value) < Epsilon`.
    5. **Epsilon:** Defined as $1.0 \times 10^{-12}$ to account for FMA (Fused Multiply-Add) differences.

## 4. Constraints & Non-Functional Requirements
* **Standardization:** This test must run as part of the standard `make test` suite in the CI/CD pipeline.
* **Dependencies:** Must depend on Task 3.1 completion.

## 5. Definition of Done (DoD)
* [ ] Reference implementation matches mathematical definition.
* [ ] `make test` runs the comparison.
* [ ] Test passes with the defined Epsilon tolerance.
