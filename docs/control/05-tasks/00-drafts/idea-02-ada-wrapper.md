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

# IDEA: Ada Compute Engine Wrapper (Double Precision)

## METADATA
* **Author:** User
* **Date:** 2026-01-05
* **Status:** NEW

## 1. The Concept (What?)
Implementation of a concrete `Double_Engine` class (implementing the `Render_Interface.Compute_Engine` interface) that manages the lifecycle of the PTX kernel and the mapping of OpenGL resources (PBO) into the CUDA address space.

## 2. Motivation (Why?)
A PTX kernel is useless without a driver. We need code in Ada 2022 that:
1. Loads the PTX module (`cuModuleLoad`) via the Driver API.
2. Registers the OpenGL PBO with CUDA (`cuGraphicsGLRegisterBuffer`) to enable the Zero-Copy path described in `ADR-0005`.
3. Launches the kernel (`cuLaunchKernel`) asynchronously without blocking the UI thread.
4. Enforces type safety at the interop boundaries, strictly adhering to the "Authorized Zones" policy defined in `ADR-0006`.

## 3. Rough Sketch
* **Package:** `Compute.Double_Engine`.
* **Private State:** Holds `CUmodule`, `CUfunction`, and the `PBO -> CUdeviceptr` mapping.
* **Safety:** Any usage of `System.Address` must be explicitly tagged with `-- SAFETY: ADR-0006`.
* **Config:** Thread block parametrization (Block/Grid Dim) should be configurable or automatically tuned to the GPU topology.
