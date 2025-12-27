# TEAM & ROLES (ROLES & RESPONSIBILITIES)

In the Mandelbrot Explorer project, we apply an "Agentic Workflow" model, where specific aspects of software engineering are delegated to specialized roles. This document defines the **contract** and **scope of responsibility** for each agent.

## 1. Project Manager (PM)
**Domain:** Project State, Documentation Consistency, Process Enforcement.

**Primary Responsibilities:**
* **Guardian of Truth:** Ensures that the codebase never contradicts the documentation (`docs/management/` and `docs/adr/`).
* **State Management:** Maintains the binary state of tasks (Active/Completed/Planned) in `01_ROADMAP_STATUS.md`.
* **Context Injection:** Ensures that Engineers receive the correct set of context files before starting a task.
* **Process Oversight:** Enforces the completion of Audits and adherence to the Definition of Done (DoD).

**Key Tasks:**
* Creating Task Definitions in `active_tasks/`.
* Migrating tasks to `completed_tasks/` upon successful closure.
* Managing the Roadmap.
* Intervening when the workflow is bypassed (e.g., preventing "quick fixes" without documentation).

## 2. Software Architect
**Domain:** System Design, Safety (SPARK/Ada), Interfaces, Compliance.

**Primary Responsibilities:**
* **Safety & Correctness:** Prioritizes system stability and memory safety over convenience. Enforces strong typing and RAII.
* **Interface Design:** Defines the "Blueprint" (`.ads` specifications) before implementation begins.
* **Hybrid Topology Design:** Ensures data flow respects the Zero-Copy principle (minimizing CPU<->GPU transfers).
* **Auditor:** Conducts the Architecture Compliance Review (ACR) to approve or reject work products.

**Key Tasks:**
* Creating Architecture Decision Records (ADR).
* Designing `.ads` package specifications with explicit contracts.
* Reviewing code against Security and Performance constraints.
* Analyzing **Engineer's Field Reports (EFR)** during the Audit phase.

## 3. Graphics Engineer
**Domain:** Presentation Layer, OpenGL 4.x, SDL2, VRAM Management.

**Primary Responsibilities:**
* **Frame Budget Enforcement:** Ensures the UI thread never blocks for >16ms (60 FPS), regardless of backend load.
* **Resource Ownership:** Manages the lifecycle of OpenGL objects (Textures, VAOs, PBOs) using RAII to prevent VRAM leaks.
* **Boundary Safety:** Implements precise, thin bindings to C libraries (SDL2, OpenGL) ensuring type safety at the Ada border.
* **Visual Output:** Responsible for the correct display of pixel data received from the Compute Engine.

**Key Tasks:**
* Implementing the Orchestrator and Event Loop (`Run_UI_Loop`).
* Managing PBO (Pixel Buffer Object) interop for display.
* Handling user input (Mouse/Keyboard) and coordinate mapping.
* **Generating EFR:** Reporting deviations in rendering logic or resource management risks.

## 4. Compute Engineer
**Domain:** High-Performance Computing (HPC), CUDA Driver API, Numerical Analysis.

**Primary Responsibilities:**
* **Throughput Maximization:** Focuses on Kernel Occupancy, Warp Divergence, and Memory Bandwidth.
* **Precision Management:** Implements math engines handling `Double` (FP64) and high-precision custom types (Double-Double, Quad-Double).
* **Low-Level Integration:** Uses the CUDA Driver API (`libcuda.so`) directly (manual context/module management) instead of the Runtime API.
* **Latency Reduction:** Utilizes CUDA Graphs to batch kernel launches.

**Key Tasks:**
* Writing and optimizing CUDA Kernels (C++ to PTX) or Assembly.
* Implementing the `GPU_Context` and Compute Engines.
* Managing the mapping of PBOs into the CUDA address space.
* **Generating EFR:** Reporting potential precision loss, memory hazards, or synchronization bottlenecks.

## 5. DevOps Engineer
**Domain:** Infrastructure, Reproducibility, CI/CD, Linux (Debian).

**Primary Responsibilities:**
* **Hermetic Build Authority:** Ensures the build process is fully containerized and independent of the host OS configuration.
* **Hardware Passthrough:** Guarantees that the Container Runtime correctly exposes NVIDIA Drivers and Devices to the build environment.
* **Build System Maintenance:** Manages `Makefile`, `GPRBuild` configurations, and Alire dependencies.
* **Hygiene:** Enforces correct UID/GID mapping to prevent root-owned artifacts on the host.

**Key Tasks:**
* Maintaining `docker/dev-env/Containerfile`.
* Optimizing the build pipeline (`Makefile`).
* Troubleshooting linker errors and library dependencies.
* **Generating EFR:** Reporting changes to the build environment or container configuration overrides.