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

# ADR-005: Multi-Threaded Tiled Architecture & Interop

* **Status:** Accepted
* **Date:** 2025-12-25
* **Context:** Task 1.5
* **Deciders:** Marcin Kaim

## Context and Problem Statement
Deep Zoom computations (up to 2^192 precision) exceed the 16ms frame budget. A single-threaded approach freezes the UI. The target hardware is a hybrid laptop (Intel iGPU + NVIDIA dGPU), requiring efficient data transfer between the calculation core and the display.

## Decision 1: Async Tiled Rendering (Producer-Consumer)

We will implement a multi-threaded architecture using Ada Tasks.

### Concurrency Model
1.  **Main Task (UI/Consumer):**
    * Owns SDL2 Window & OpenGL Context.
    * Maintains the **Tile Cache** (GL Textures).
    * Calculates visible tiles based on Zoom/Pan.
    * Draws available tiles immediately.
    * Submits missing tile jobs to the Queue.
2.  **Worker Task (Compute/Producer):**
    * Owns `GPU_Context` (CUDA).
    * Processes the `Job_Queue` (Priority: Distance from center).
    * Executes CUDA Graphs to render tiles into PBOs.

## Decision 2: CUDA/GL Interop via PBO

To minimize PCIe bandwidth usage, we will use **Pixel Buffer Objects (PBO)**.

### Data Flow
1.  **Alloc:** OpenGL allocates a PBO.
2.  **Register:** PBO is registered with CUDA (`cuGraphicsGLRegisterBuffer`).
3.  **Compute:** Worker maps the PBO -> Writes Pixels via Kernel -> Unmaps.
4.  **Display:** Main Task uses `glTexSubImage2D` binding the PBO as `GL_PIXEL_UNPACK_BUFFER` to update the texture atlas on the GPU.

## Decision 3: Hardware Topology Strategy

[cite_start]To respect the requirement of utilizing the dGPU [cite: 5] while displaying on the laptop screen:
* The application SHOULD be launched with **PRIME Render Offload** (`__NV_PRIME_RENDER_OFFLOAD=1`).
* This ensures both OpenGL and CUDA contexts reside on the NVIDIA GPU, making PBO interop essentially zero-copy (internal VRAM transfer).
* The final image copy to the iGPU (for display) is handled by the Window Manager/Compositor, not our application code.

## Component Diagram (Updated)

```text
       [ User Input ]
             |
             v
+--------------------------+          +-------------------------+
| Orchestrator (Main Task) |          |      Worker Task        |
| - SDL_PollEvent          | --Push-->| - GPU_Context (CUDA)    |
| - GL Texture Atlas       |          | - cuGraphLaunch         |
| - Draws Frame            |          | - Maps/Unmaps PBO       |
+--------------------------+          +-------------------------+
             ^                                     |
             | (GlTexSubImage from PBO)            | (Writes to PBO)
             |                                     v
      +-------------------------------------------------------+
      |                VRAM (NVIDIA RTX 3500)                 |
      | +-------------------+       +-----------------------+ |
      | | GL Texture (RGBA) | <---- |  Shared PBO (Buffer)  | |
      | +-------------------+       +-----------------------+ |
      +-------------------------------------------------------+
```

## Consequences (Analysis)

### Positive
* **Decoupled UX:** The UI runs at monitor refresh rate (e.g., 60Hz/144Hz) regardless of fractal calculation speed. Deep zoom (which might take 200ms/frame) does not freeze the mouse or window dragging.
* **Zero-Copy Bandwidth:** Using PBOs with `PRIME Render Offload` ensures that heavy pixel data stays in VRAM (On-Chip Memory Transfer). We avoid the bottleneck of moving megabytes of RGBA data over the PCIe bus every frame.
* **Scalability:** The tile-based approach allows us to easily implement "Progressive Refinement" (showing a blurry upscaled parent tile while waiting for the sharp tile) later.

### Negative
* **Synchronization Complexity:** We strictly rely on Ada Protected Objects (`Job_Queue`) to prevent race conditions. The Orchestrator must carefully manage the lifecycle of tiles to avoid overwriting a PBO that is currently being mapped by CUDA.
* **Memory Overhead:** We need to allocate VRAM for the "Texture Atlas" (cache of visible tiles) and PBOs. This is higher than a simple "Front/Back Buffer" approach.
* **Latency:** There is a minimal 1-frame latency between calculation completion and display, as the GL draw call happens in the main loop iteration *after* the Worker signals completion.
* **Context Management Risk:** Strictly separating OpenGL (Main Task) and CUDA (Worker Task) requires disciplined context handling. The Worker must *never* touch GL functions directly, and the Main Task must *never* touch CUDA functions that operate on the specific stream being used by the worker without sync.