# REQ-003: NON-FUNCTIONAL REQUIREMENTS & ENGINEERING STANDARDS

| Attribute | Details |
| :--- | :--- |
| **Document ID** | REQ-003 |
| **Status** | DRAFT |
| **Scope** | Safety, Performance, Infrastructure |
| **Context** | Engineering Quality Gates |

## 1. Safety & Type System Integrity

The project adheres to the **SPARK/Ada High-Integrity** philosophy. We reject the C/C++ approach of treating memory as a raw byte array.

### 1.1. The "Strong Typing" Directive (CUDA Binding)
* **Strict Prohibition:** The use of `Ada.Unchecked_Conversion` is **FORBIDDEN** within the codebase, especially for interfacing with the CUDA Driver API.
* **Mechanism:** Interoperability must be achieved via:
    * **Distinct Handle Types:** Handles (e.g., `CUcontext`, `CUdeviceptr`, `CUmodule`) must be defined as distinct derived types (e.g., `type CUcontext is new System.Address`), not aliases. This ensures the compiler catches logic errors (e.g., passing a Context where a Device is expected).
    * **Convention C:** Data structures shared with GPU kernels or C libraries must utilize `pragma Convention (C)` and explicit record representation clauses to guarantee binary compatibility without casting.
    * **Access Types:** Pointers to device memory must be handled via specific access types (e.g., `type Device_Ptr is access all ...`) or strictly controlled `System.Address` passing, never by integer casting.

### 1.2. Resource Acquisition Is Initialization (RAII)
* **Requirement:** Manual memory management (malloc/free style) is prohibited in high-level logic.
* **Implementation:** All external resources (OpenGL Textures, CUDA Contexts, PBOs) must be wrapped in **Ada Limited Controlled Types**.
* **Guarantee:** Resources must be automatically released (Finalize) when the owning object goes out of scope, ensuring zero leaks even during exception propagation.

## 2. Performance Constraints (Real-Time)

### 2.1. The 16.6ms UI Budget
The User Interface (Orchestrator Task) operates under Hard Real-Time constraints.
* **Frame Time:** The rendering loop must complete within **16.6ms** (60 Hz) or **6.9ms** (144 Hz) depending on monitor refresh rate.
* **Non-Blocking:** No operation in the UI thread (specifically CUDA API calls like `cuCtxSynchronize` or `cuMemcpy`) is allowed to block for undefined periods. Long-running operations must be offloaded to the Compute Worker.

### 2.2. Zero-Copy Architecture
* **Data Flow Constraint:** Transferring pixel data from the Compute Engine (dGPU) to the Display Engine (iGPU/dGPU) must not involve the Host CPU RAM (System Memory).
* **Implementation:** Strict usage of **OpenGL Pixel Buffer Objects (PBO)** registered with CUDA (`cuGraphicsGLRegisterBuffer`).
* **Validation:** Profiling must show minimal PCIe bus traffic during deep zoom rendering.

### 2.3. Startup Latency
* **Cold Start:** The application must launch and display the initial fractal state (Zoom 1.0) within **2.0 seconds** of execution command.
* **Shader Compilation:** Shaders must be pre-compiled or cached if compilation exceeds the startup budget.

## 3. Infrastructure & Deployment

### 3.1. Hermetic Build System
* **Containerization:** The build process must be fully encapsulated in a container (Podman/Docker).
* **Host Independence:** The build must succeed on any Linux host with a compatible container runtime, **without** requiring the NVIDIA CUDA Toolkit to be installed on the host OS (only the Kernel Driver is required).
* **UID/GID Mapping:** The build process must respect the host user's file permissions (no root-owned artifacts in the output directory).

### 3.2. Hardware Resilience
* **Prime Offload Support:** The application must correctly detect and utilize the discrete NVIDIA GPU on hybrid laptops (`__NV_PRIME_RENDER_OFFLOAD=1`).
* **Graceful Degradation:**
    * If CUDA cannot be initialized (driver missing/mismatch), the application must automatically fall back to the CPU Compute Engine.
    * It must **not crash** with a segmentation fault or uncaught exception. A visible error/warning log is required.

## 4. Code Quality & Standards

### 4.1. Documentation
* **Single Source of Truth:** Code must never contradict the Architecture Decision Records (ADRs) or Requirements documents.
* **EFR (Engineer's Field Report):** Every merged task must include a report validating that these constraints (especially strict typing and memory safety) were verified.

### 4.2. Testing Strategy
* **Unit Tests:** Must run inside the container with GPU passthrough enabled.
* **Sanitizers:** Debug builds should utilize GNAT validity checks (`-gnatVa`) and stack overflow checks to catch runtime anomalies early.