# TASK ORDER: 1.5 High-Level System Architecture Definition

## 1. Context Injection (Required Reading)
* `docs/management/00_PROJECT_CONTEXT.md`
* `docs/adr/0003-CUDA-driver-api.md` (Understand existing constraints)
* `src/compute/gpu_context.ads` (Understand available tools)

## 2. Objective
Stop coding. Start designing.
We have the GPU driver (Task 1.1). We have the vision (Context). Now we need the **Blueprint**.
You must design the software architecture that connects the **User Input** (SDL2), the **Logic Core** (Zoom/Coordinates), and the **Render Engine** (CUDA Graphs).

## 3. Key Architectural Questions to Answer
You must analyze and document decisions for the following problems:
1.  **The Render Loop Flow:** How exactly does a frame get on screen?
    * *Draft Idea:* Input -> Update State -> Update CUDA Graph Params -> Launch Graph -> Map Resource to OpenGL -> Swap Buffers.
2.  **Resource Sharing (CUDA <-> OpenGL):**
    * How do we display the result buffer efficiently? (e.g., CUDA/GL Interop with Pixel Buffer Objects - PBO).
3.  **The "Orchestrator" Pattern:**
    * Who owns the `GPU_Context`? Who owns the `SDL_Window`?
    * How do we prevent "God Objects"?
4.  **Dynamic Precision Handling:**
    * Refine the strategy for Task 1.2. How does the Orchestrator switch from `Double_Engine` to `Quad_Engine` at runtime without restarting the app?

## 4. Deliverables
1.  **`docs/adr/0005-system-architecture.md`:**
    * A comprehensive Architecture Decision Record describing the **Component Diagram** (text-based or Mermaid).
    * Description of the **Data Flow** for a single frame.
    * Decision on the **Concurrency Model** (Single threaded Ada task? Separate Render Task?).

2.  **`src/app/orchestrator.ads` (Specification):**
    * The package spec that defines the high-level `Controller` type.
    * It should expose methods like `Initialize`, `Run_Loop`, `Handle_Input`.

3.  **`src/compute/render_interface.ads` (Specification):**
    * An abstract interface that any compute engine (Double/Quad) must implement to be pluggable into the Orchestrator.

## 5. Definition of Done
* The Architecture Document (ADR-005) is approved.
* The `.ads` specifications compile and clearly show the dependencies between modules.
* We have a clear path to implement SDL2 (Task 2.1) and Math Traits (Task 1.2) fitting into this design.