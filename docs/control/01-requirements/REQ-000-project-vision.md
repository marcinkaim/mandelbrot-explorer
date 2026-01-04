# REQ-000: PROJECT VISION & CONSTITUTION

| Attribute | Details |
| :--- | :--- |
| **Document ID** | REQ-000 |
| **Status** | DRAFT |
| **Scope** | Global (Philosophy, Goals, Identity) |
| **Context** | Project Foundation |

## 1. Project Identity & Purpose

**Mandelbrot Explorer** is not merely a fractal visualization tool; it is a **Software Engineering Experiment** designed to demonstrate that high-integrity software practices (Ada 2022/SPARK) can coexist with extreme hardware performance (NVIDIA CUDA/PTX) without compromise.

### 1.1. Nature of the Project
* **Educational:** The project serves as a reference implementation for advanced topics such as:
    * Interfacing high-level languages (Ada) with low-level driver APIs (CUDA Driver API).
    * Managing hybrid memory models (Host RAM vs. Device VRAM) safely.
    * Implementing arbitrary-precision arithmetic on bare metal (PTX).
* **Experimental:** The project pushes the boundaries of "Consumer HPC," attempting to achieve real-time Deep Zoom capabilities on standard workstation hardware (Hybrid Laptop GPUs) using novel architectural patterns like Async Tiled Rendering.

### 1.2. The Core Conflict resolution
This project addresses the tension between **Safety** and **Speed**.
* *Thesis:* System languages (C/C++) sacrifice safety for speed.
* *Antithesis:* Safe languages (Ada/Rust) introduce overhead or abstraction layers that hinder raw GPU access.
* *Synthesis (Our Goal):* We aim to prove that by using **Zero-Cost Abstractions** and strict **Strong Typing** at the binding layer, we can write safe, readable code that compiles down to instructions as efficient as optimized C++, while eliminating entire classes of runtime errors.

## 2. Core Philosophies (First Principles)

### 2.1. Safety First (The "No Segfault" Rule)
* **Strict Typing:** We reject the use of `void*` (or `System.Address`) as a universal hammer. Handles to GPU resources (`CUcontext`, `CUdeviceptr`, `PBO_Handle`) must be distinct Ada types to prevent logical errors at compile time.
* **No Unchecked Conversion:** The codebase must avoid `Ada.Unchecked_Conversion`. Data marshaling between Host and Device must rely on explicitly defined, interoperable record layouts (`Convention => C`) rather than raw memory casting.

### 2.2. Zero-Copy Architecture
* **Data Gravity:** Data should move as little as possible. If a pixel is calculated on the dGPU, it must stay in VRAM. It should be displayed via OpenGL Interop (PBO) without ever crossing the PCIe bus back to System RAM (unless explicit CPU fallback is requested).

### 2.3. Hermetic Engineering
* **Reproducibility:** The build environment is as important as the source code. The project must build identically on any host machine running a compatible container runtime, independent of the host's local library versions (excluding the kernel-space GPU driver).

## 3. Strategic Goals

### 3.1. The "Deep Zoom" Capability
The system must handle three distinct precision tiers seamlessly:
1.  **Hardware Double (FP64):** Utilizing raw silicon performance for standard zooms ($10^{0}$ to $10^{14}$).
2.  **Emulated Quad-Double:** Software-defined arithmetic for deep exploration ($10^{14}$ to $10^{28}$).
3.  **Arbitrary Precision:** An extensible engine for "infinite" zoom, constrained only by compute time, not data types.

### 3.2. Consumer-Grade Smoothness (The UX Target)
Despite the heavy computational load (Deep Zoom), the application must behave like a modern map application:
* **60 FPS UI:** The User Interface thread is hard-real-time. It never blocks waiting for a calculation.
* **Visual Persistence:** The user never stares at a black screen. Old data is geometrically transformed (scaled/panned) to provide context while new data is computed.

### 3.3. Hybrid Rendering Implementation
The project aims to implement a true hybrid engine where:
* **Logic & Orchestration** run on the CPU (Ada).
* **Number Crunching** runs on the dGPU (CUDA) or CPU SIMD (AVX/SSE) as a fallback.
* **Coloring & Rasterization** run on the iGPU/dGPU (OpenGL Shaders), decoupling the mathematical result from its artistic presentation.

## 4. Target Audience

This project is built for:
* **Systems Engineers** interested in Ada 2022 features and non-standard GPU bindings.
* **HPC Developers** looking for alternatives to C++ for orchestration logic.
* **Educators & Students** studying the interaction between Kernel Space drivers and User Space applications.