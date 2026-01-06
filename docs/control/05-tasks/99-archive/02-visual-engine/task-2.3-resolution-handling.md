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

# TASK ORDER: 2.3 Dynamic Resolution & Window Resizing

## 1. Context Injection (Required Reading)
* `docs/management/00_PROJECT_CONTEXT.md` (Responsiveness & UX)
* `docs/audits/001-Task-2.1-Review.md` (Finding WARN-02)
* `src/app/orchestrator.adb` (Current event loop implementation)

## 2. Objective
Eliminate hardcoded 800x600 resolution. Implement SDL2 window resize event handling and dynamic update of OpenGL Viewport, Textures, and PBO resources.

## 3. Key Responsibilities
1.  **SDL Event Handling:**
    * In `Run_UI_Loop`, implement handling for `SDL_WINDOWEVENT` with the `SDL_WINDOWEVENT_RESIZED` subtype.
    * Dynamically update `Self.Width` and `Self.Height` in the `Controller` record.
2.  **OpenGL State Management:**
    * Invoke `glViewport` with new dimensions after every resize event.
3.  **Resource Reallocation:**
    * Implement a safe sequence to release existing Texture and PBO handles and reallocate them with new dimensions.
    * Ensure synchronization with the `Compute_Worker` task to prevent buffer reallocation while the GPU is potentially mapping the resource (use existing protected `Job_Queue` or a dedicated synchronization flag).

## 4. Deliverables
* Updated `src/app/orchestrator.adb` with resize logic.
* Updated `src/binding/sdl2_thin.ads` (if missing constants for window events).

## 5. Definition of Done
* `make build` succeeds without warnings.
* The application window can be resized manually; the fractal render area (quad) scales correctly to the new window size.
* No OpenGL errors or artifacts are observed during rapid window resizing.
