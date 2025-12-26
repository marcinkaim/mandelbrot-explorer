# ARCHITECTURE DECISION RECORDS (Summary)

## ADR-001: Precision Strategy
* **Decision:** Start with `Double`. Switch engines via `Render_Interface`.
* **Thresholds:** 2^48, 2^96, 2^192.

## ADR-002: Windowing Library -> SDL2
* **Reason:** Stability, Input handling, OpenGL context management.

## ADR-003: CUDA Strategy -> Driver API & Graphs
* **Decision:** Use **Driver API** (`libcuda.so`) combined with **CUDA Graphs**.
* **Reason:** Hermetic builds, lower launch latency.

## ADR-004: Build System -> Alire + Podman
* **Decision:** Hermetic containerized build.

## ADR-005: Async Tiled Architecture & PBO Interop [NEW]
* **Decision:** Multi-threaded Producer-Consumer model (UI Task + Compute Task).
* **Data Flow:** Compute writes to **Pixel Buffer Objects (PBO)** via CUDA. UI reads PBO into Textures.
* **Hardware:** Use **PRIME Render Offload** (`__NV_PRIME_RENDER_OFFLOAD=1`) to keep PBOs in dGPU VRAM.