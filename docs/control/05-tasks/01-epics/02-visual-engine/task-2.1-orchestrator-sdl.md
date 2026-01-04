# TASK ORDER: 2.1 Orchestrator Implementation & SDL2/GL Backend

## 1. Context Injection (Required Reading)
* `docs/management/00_PROJECT_CONTEXT.md`
* `docs/adr/0005-system-architecture.md` (Crucial for threading model)
* `src/app/orchestrator.ads` (The spec you must implement)

## 2. Objective
Implement the body of the `Orchestrator` package (`.adb`) and the necessary SDL2/OpenGL thin bindings.
This task brings the "Visual Engine" to life. It opens the window, starts the rendering thread (Main Task), and initializes the `Job_Queue`.

## 3. Key Responsibilities
1.  **SDL2 & OpenGL Init:**
    * Initialize SDL Video Subsystem.
    * Set GL Attributes (Core Profile 4.x, Double Buffer).
    * Create Window ("Mandelbrot Explorer", Resizable).
    * Create OpenGL Context.
2.  **Resource Allocation (PBO & Textures):**
    * Allocate a Texture Atlas (RGBA8888) to hold the fractal tiles.
    * Allocate a **Pixel Buffer Object (PBO)** using `glGenBuffers`. This is the shared memory for CUDA.
3.  **Concurrency Implementation:**
    * Implement `protected body Job_Queue` (Push/Pop logic).
    * Implement `task body Compute_Worker`. For this task, the worker can be a **dummy consumer** (just pop a job, wait 10ms, print "Job Done", and loop). We will plug in CUDA in Task 3.1.
4.  **The Render Loop (`Run_UI_Loop`):**
    * Poll SDL Events (Quit, Keys).
    * **Simulate** pushing jobs when Keys (Zoom/Pan) are pressed.
    * Draw the Texture to the screen (Full-screen Quad).
    * Swap Buffers.

## 4. Deliverables
1.  **`src/binding/sdl2_thin.ads`:** Minimal bindings (Window, Events, GL Context).
2.  **`src/binding/opengl_thin.ads`:** Minimal bindings (Textures, Buffers, PBO constants).
3.  **`src/app/orchestrator.adb`:** The main implementation.

## 5. Definition of Done
* `alr build` succeeds.
* Running the app opens a window.
* The app logs "Worker Started".
* Pressing keys logs "Job Pushed" (simulating navigation).
* Closing the window terminates the app cleanly (Worker stops).