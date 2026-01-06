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

# TASK ORDER: 1.1 Ada Binding to CUDA Driver API

## 1. Context Injection (Required Reading)
* `docs/management/00_PROJECT_CONTEXT.md`
* `docs/management/02_ARCHITECTURE_DECISIONS.md` (See ADR-003)

## 2. Objective
Implement a thin, type-safe Ada binding to the NVIDIA CUDA Driver API using `Interfaces.C`.

## 3. Inputs
* **Scope:** Subset of API: `cuInit`, `cuDeviceGet`, `cuCtxCreate`, `cuModuleLoad`, `cuLaunchKernel`, `cuMemAlloc`, `cuMemCpy`.
* **Style:** Thin binding. 1:1 mapping to C types.

## 4. Deliverables
1. `src/binding/cuda_driver_api.ads`:
   * Define distinct types for `CUdevice`, `CUcontext`, `CUmodule` (do not use raw Integer/Address for all to ensure type safety).
   * Import functions using `Pragma Import`.

2. `src/compute/gpu_context.ads` (Spec only):
   * Propose a high-level RAII type `CUDA_Context` that initializes CUDA in `Initialize` and destroys context in `Finalize`.

## 5. Definition of Done
* `alr build` succeeds inside the container.
* The binding spec strictly matches the types defined in standard `cuda.h`.