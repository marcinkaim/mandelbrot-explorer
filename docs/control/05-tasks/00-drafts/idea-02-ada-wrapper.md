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
