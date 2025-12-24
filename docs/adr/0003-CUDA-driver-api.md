# ADR-003: Use CUDA Driver API & CUDA Graphs

* **Status:** Accepted
* **Date:** 2025-12-24
* **Deciders:** Marcin Kaim
* **Consulted:** NVIDIA CUDA Documentation, AdaCore Documentation
* **Technical Context:** High-performance GPGPU orchestration in Ada 2022.

## Context and Problem Statement

We need to harness the computational power of NVIDIA GPUs to render the Mandelbrot set at deep zoom levels (up to 2^192) with high frame rates (60+ FPS). The application logic is written in Ada 2022. We must choose an interaction model with the GPU that:
1.  Integrates cleanly with Ada without requiring complex C++ build shims or the NVIDIA C Compiler (`nvcc`) on the host system during the build process.
2.  Minimizes CPU overhead (latency) when submitting dependent kernels (e.g., Compute -> Color -> Render).
3.  Provides fine-grained control over GPU contexts and memory addressing (Unified Addressing).

The standard **CUDA Runtime API** (`cudart`) heavily relies on C++ syntax extensions (`<<<...>>>`) and implicit initialization, which is difficult to bind directly to Ada and hides low-level details required for optimization.

## Decision Drivers

* **Hermetic Build:** The host system (and build container) should not require a full CUDA C++ toolchain installation to compile the Ada host code.
* **Performance (Latency):** Submitting multiple kernels sequentially from the CPU introduces "Launch Latency" (~5-20µs per kernel), which is significant at high frame rates.
* **Type Safety:** We want to wrap raw GPU handles in Ada's strong type system to prevent resource leaks and invalid access.

## Considered Options

1.  **CUDA Runtime API (`cudart`):** High-level, easy to use in C++, but requires `nvcc` for host code or complex wrapper libraries. Hard to bind to Ada due to C++ mangling and hidden state.
2.  **CUDA Driver API (`libcuda`):** Low-level, C-style API. Exposes handles (`CUdevice`, `CUcontext`) directly. Can be loaded dynamically. Matches Ada's `Interfaces.C` capabilities perfectly.
3.  **OpenCL:** Vendor-neutral, but performance on NVIDIA hardware historically lags behind native CUDA, and the ecosystem is less robust for deep optimization (PTX).

## Decision

We chose to use the **CUDA Driver API (`libcuda.so`)** combined with **CUDA Graphs**.

### 1. Driver API Strategy
We interact with the GPU by dynamically loading `libcuda.so`. We define a thin Ada binding (`src/binding/cuda_driver_api.ads`) that maps C functions directly.
* **PTX Loading:** We compile kernels to PTX (Parallel Thread Execution) assembly offline. The Ada application loads this text/binary data at runtime using `cuModuleLoad`. This removes the need for `nvcc` at runtime.
* **Context Management:** We manually manage the `CUcontext` life-cycle using an RAII wrapper in Ada (`GPU_Context`).

### 2. Execution Strategy: CUDA Graphs
Instead of submitting kernels individually ("Immediate Mode"), we use **CUDA Graphs**:
* **Construction:** We build a Directed Acyclic Graph (DAG) of operations (e.g., Kernel A -> Kernel B).
* **Instantiation:** The driver optimizes and validates this graph once.
* **Launch:** The CPU submits the entire graph in a single API call (`cuGraphLaunch`). This offloads the synchronization of dependent kernels to the GPU hardware scheduler.

## Consequences

### Positive
* **Zero-Overhead Binding:** Ada communicates directly with the driver. No intermediate C++ layer.
* **Build Simplicity:** The Ada compiler (`gnat`) does not need to know about CUDA. It only sees standard C interfaces.
* **Performance:** CUDA Graphs minimize kernel launch latency and CPU usage, ensuring high throughput.
* **Safety:** Ada's distinct types prevent mixing `CUdevice` and `CUcontext` handles at compile time.

### Negative
* **Complexity:** We must manually manage contexts, modules, and memory allocation. It is more verbose than the Runtime API.
* **Boilerplate:** CUDA Graphs require verbose setup code (defining node parameters, dependencies, arrays of pointers) compared to a simple function call.
* **Debugging:** Errors in graph construction (e.g., struct alignment) can result in cryptic `CUDA_ERROR_ILLEGAL_ADDRESS` at runtime, requiring strict layout control in Ada records.

## Implementation Details

* **Binding:** Located in `src/binding/cuda_driver_api.ads`.
* **Struct Alignment:** `CUDA_KERNEL_NODE_PARAMS` must have `Convention => C` and manual `Record Representation Clauses` to match the driver's binary layout (padding issues).
* **Testing:** Verification is performed via AUnit tests running in a container with GPU passthrough (`make test`).