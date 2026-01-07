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

# Audit Report: task-3.1-compute-engine-fp64

* **Sequence ID:** AR-TASK-3.1-02
* **Date:** 2026-01-07
* **Role:** AUDITOR
* **Relates To:** [AR-TASK-3.1-01](docs/control/05-tasks/02-reports/task-3.1/AR-01-engineer-success.md)
* **Outcome:** FAILURE
* **Next Phase:** ACTIVE (Return to Engineer)

## 1. Executive Summary
The submission for Task 3.1 is **REJECTED**.

While the functional tests indicate the kernel is mathematically correct, the implementation commits a **Critical Architectural Violation** by ignoring the requirement for CUDA Graphs (`ADR-0003`) and falling back to Immediate Mode execution (`cuLaunchKernel`) without authorization. Furthermore, the code violates the Project Safety Constitution (`ADR-0006`) by placing unsafe memory operations in unauthorized files and exposing raw addresses in public specifications.

The task is returned to the `ACTIVE` state for immediate remediation.

## 2. Findings (The Bill of Indictment)

### 2.1 Critical Violations (Must Fix)

* **[CRITICAL] Architecture Violation (ADR-0003):**
    * **File:** `src/compute/cuda_engine.adb` (Lines 1013-1015)
    * **Rule:** `ADR-0003` Explicitly Mandates: "We chose to use ... CUDA Graphs ... The CPU submits the entire graph in a single API call (`cuGraphLaunch`)."
    * **Violation:** The code implements "Immediate Mode" using `cuLaunchKernel`. This reintroduces the kernel launch latency the architecture was designed to avoid. No technical waiver was found in the Action Report.

* **[CRITICAL] Safety Policy Violation (ADR-0006):**
    * **File:** `src/compute/cuda_engine.adb`
    * **Rule:** `ADR-0006` §3.1 "Authorized Zones". Unsafe memory operations (handling `System.Address`) are restricted to `src/binding/*` and `src/compute/gpu_context.adb`.
    * **Violation:** `src/compute/cuda_engine.adb` is not an Authorized Zone, yet it performs raw address manipulation (e.g., `Params(0) := Arg_Min_X'Address`).

* **[CRITICAL] Missing Mandatory Safety Tags (ADR-0006):**
    * **File:** `src/compute/cuda_engine.adb`
    * **Rule:** `ADR-0006` §3.2 "The 'Safety Tag' Rule". Every line/block containing unsafe operations must be immediately preceded by a comment explicitly referencing the ADR.
    * **Violation:** Multiple occurrences of address manipulation (e.g., Line 991, Line 1000) lack the required `-- SAFETY: Authorized by ADR-0006` tag.

* **[MAJOR] Interface Safety Leakage (ADR-0006):**
    * **File:** `src/compute/cuda_engine.ads` (Line 969)
    * **Rule:** `ADR-0006` §3.2 "Type Isolation". Unsafe types (e.g., `System.Address`) must NOT leak into the public specification.
    * **Violation:** The `Render_Tile` procedure exposes `Target_Buffer : System.Address` in the public API. This forces clients of the Engine to handle raw addresses, spreading the contamination.

### 2.2 Maintenance & Style Observations

* **[MINOR] Hardcoded Magic Numbers:**
    * **File:** `src/compute/cuda_engine.adb` (Line 1012)
    * **Observation:** `Block_Size` is hardcoded to `16`. This should be a named constant or configuration parameter to allow tuning for different GPU architectures (as noted in `ADR-0003` context).

## 3. Instructions for Remediation

The Engineer must perform the following actions before re-submitting:

1.  **Refactor for CUDA Graphs:**
    * Rewrite `Initialize` to build a `CUgraph` consisting of the Kernel Node.
    * Rewrite `Render_Tile` to update the Graph Executable (`cuGraphExecKernelNodeSetParams` or similar) and launch via `cuGraphLaunch`.
    * *Reference:* `ADR-0003`, Section "Implementation Details".

2.  **Enforce Safety Boundaries:**
    * **Option A (Preferred):** Move the unsafe PBO mapping and Kernel Parameter encapsulation logic into `GPU_Context` (which is an Authorized Zone) and expose a safe, high-level primitive to `CUDA_Engine`.
    * **Option B (Bureaucratic):** Submit a **Change Request** to update `ADR-0006` to include `src/compute/cuda_engine.adb` in the Authorized Zones whitelist, citing the necessity of direct kernel parameter access.
    * **Constraint:** If Option B is chosen, you MUST apply the `-- SAFETY` tags to every unsafe line.

3.  **Sanitize the Interface:**
    * Remove `System.Address` from the public `Render_Tile` spec. Wrap the PBO handle in a private type or a safe `Graphic_Buffer` abstraction.

**Status:** REJECTED.