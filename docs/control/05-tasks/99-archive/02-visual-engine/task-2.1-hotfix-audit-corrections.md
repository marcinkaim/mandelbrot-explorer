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

# TASK ORDER: 2.1 (Hotfix) - Architecture Compliance Corrections

## 1. Context Injection
* `docs/management/00_PROJECT_CONTEXT.md`
* `docs/audits/001-Task-2.1-Review.md` (The Audit Report - Source of Truth for this task)
* `src/app/orchestrator.adb` (Current implementation)

## 2. Objective
Address the Critical Findings (`CRIT-01`, `CRIT-02`) and Warning (`WARN-01`) identified by the Software Architect in Audit 001. Ensure the `Orchestrator` is memory-safe and resource-leak free.

## 3. Key Responsibilities

### Fix CRIT-02: Type Safety in Concurrency
* **Target:** `src/app/orchestrator.adb` (Worker Start & Private Helpers).
* **Action:** Remove `Ada.Unchecked_Conversion` for `Job_Queue_Access` and `Context_Access`.
* **Method:** Pass the access types directly to the Worker task entry. Ensure visibility rules allow this (Types defined in spec private part are visible in body).

### Fix WARN-01 & CRIT-01: Resource Management (Cleanup)
* **Target:** `src/binding/opengl_thin.ads` & `src/app/orchestrator.adb`.
* **Action 1:** Import missing OpenGL deletion functions in `OpenGL_Thin`:
    * `glDeleteVertexArrays` (Check existing), `glDeleteTextures`, `glDeleteProgram`.
* **Action 2:** Implement a proper cleanup sequence in `Orchestrator.Shutdown`:
    * Delete VAO, VBO, PBO, Textures.
    * Delete Shader Program.
    * *Order matters:* Delete GL resources -> Destroy GL Context -> Destroy Window -> SDL Quit.

## 4. Deliverables
* Updated `src/app/orchestrator.adb`.
* Updated `src/app/orchestrator.ads` (if type visibility changes are needed).
* Updated `src/binding/opengl_thin.ads`.

## 5. Definition of Done
* `make build` compiles without errors.
* The application runs and shuts down cleanly (no segfaults during `Shutdown`).
* **Source Code Verification:** No `Unchecked_Conversion` from `System.Address` used for internal Ada types (`Job_Queue`, `Context`).
