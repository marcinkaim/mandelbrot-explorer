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

# ADR-0007: Hardware Resilience & Compute Fallback Strategy

* **Status:** ACCEPTED
* **Date:** 2026-01-04
* **Author:** Architect
* **Deciders:** Architect, Engineer
* **Consulted:** DevOps (Runtime Dependencies)

## 1. Context and Problem Statement

The Mandelbrot Explorer is designed as a High-Performance Computing (HPC) application leveraging NVIDIA CUDA (`ADR-0003`) for massive parallelism.

However, external hardware dependencies are inherently fragile. The system may run in environments where:
1.  **No NVIDIA GPU is present** (e.g., standard laptops, CI/CD runners).
2.  **Drivers are missing/broken** (e.g., kernel update mismatch).
3.  **CUDA Initialization fails** (e.g., insufficient video memory).

Previously, `REQ-003` (v1) mandated "Hardware Resilience". With the refinement of requirements, we need a concrete architectural strategy to ensure the application does not crash or refuse to start in these scenarios.

**The Question:** How should the system behave when the primary compute accelerator is unavailable?

## 2. Decision Drivers

* **User Experience:** The application must launch and be usable "out of the box" without configuration tweaking.
* **Reliability:** A driver failure must not cause a Segment Violation (Segfault) or an Unhandled Exception.
* **Testability:** We need to verify core logic (Zoom, Navigation, Coloring) even on machines without GPUs (CI Pipeline).

## 3. Decision

We decide to implement a **Dynamic Dual-Engine Architecture** based on the **Strategy Pattern**.

### 3.1. The "Fallback" Protocol

The system will implement a strict initialization hierarchy:
1.  **Attempt:** Initialize CUDA Context (`src/compute/gpu_context`).
2.  **On Failure:** Catch the exception/error code, log a warning to `stderr`.
3.  **Fallback:** Automatically instantiate the CPU Compute Engine (`src/compute/cpu_engine`).

### 3.2. Abstraction Layer

To support this transparency, the `Orchestrator` (`src/app/orchestrator`) must not depend directly on CUDA packages. Instead, we introduce a polymorphic interface:

```ada
type Compute_Engine is interface;

procedure Render_Frame
  (Engine : in out Compute_Engine;
   Params : in     Render_Parameters;
   Output :    out Pixel_Buffer) is abstract;

```

Two implementations will exist:

* `GPU_Engine`: Proxies calls to CUDA Kernels (via `ADR-0006` boundaries).
* `CPU_Engine`: Uses Ada Tasking (Ravenscar-compatible) for multi-threaded calculation on the CPU.

### 3.3. Zero-crash Guarantee

The entry point (`main.adb`) is responsible for the probe mechanism. It acts as a "Circuit Breaker". If `GPU_Engine` raises a `Device_Not_Found` exception during construction, `main.adb` **must** catch it and swap the pointer to `CPU_Engine`.

## 4. Consequences

### Positive

* **Total Resilience:** The application works on 100% of x86_64 Linux machines, regardless of GPU status.
* **CI/CD Enablement:** We can run functional integration tests on standard GitHub Actions runners (which lack GPUs).
* **Debuggability:** Developers can force CPU mode to debug logic errors without GPU complexity.

### Negative

* **Code Duplication:** We must maintain two implementations of the Mandelbrot algorithm (one in CUDA PTX, one in Ada). They must produce visually identical results.
* **Performance Gap:** The CPU engine will be significantly slower (orders of magnitude). The UI must handle this latency gracefully (as per `REQ-003` async UI requirement).

## 5. Compliance Verification

* **Test Case 1 (Hardware):** Run on a machine with RTX GPU -> Verify `nvidia-smi` usage.
* **Test Case 2 (Software Simulation):** Set env var `FORCE_CPU_MODE=1` -> Verify app starts and renders frame (slowly).
* **Test Case 3 (CI):** Build and run unit tests in Docker container without `--gpus` flag -> Must pass.
