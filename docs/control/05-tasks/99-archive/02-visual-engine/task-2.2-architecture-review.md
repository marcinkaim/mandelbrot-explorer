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

# TASK ORDER: 2.2 Architecture Compliance Review (ACR)

## 1. Context
Task 2.1 introduced significant complexity: manual memory management (`System.Address`), `Unchecked_Conversion`, `Unchecked_Access`, and Ada Tasking logic interop with C libraries (SDL2/OpenGL).
Before we build the heavy Compute Engine (Task 3.1), we must ensure the foundation is solid and safe.

## 2. Objective
Perform a code audit of the `Orchestrator`, `SDL2_Thin`, `OpenGL_Thin`, and `GPU_Context` packages.
Document findings in a new document type: **Architecture Compliance Review**.

## 3. Key Responsibilities
1.  **Safety Audit:**
    * Review usage of `Unchecked_Access`. Are we creating dangling pointers passed to GL/CUDA?
    * Review `Job_Queue` implementation for potential deadlocks or race conditions.
    * Check for Memory Leaks (e.g., `To_C` vs `New_String` usage pattern).
2.  **Architecture Verification (ADR-005):**
    * Does the implementation respect the Producer-Consumer model?
    * Is PBO usage correct for the planned Zero-Copy CUDA interop?
3.  **Documentation:**
    * Create directory `docs/audits`.
    * Create file `docs/audits/001-Task-2.1-Review.md`.
    * List **Findings** (Critical, Warning, Info) and **Recommendations**.

## 4. Deliverables
* `docs/audits/001-Task-2.1-Review.md` containing the analysis.
* (Optional) List of immediate "Hotfix" tasks if critical issues are found.