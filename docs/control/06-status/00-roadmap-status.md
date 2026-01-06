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

# PROJECT ROADMAP & STATUS

## 🟢 BASELINE: FOUNDATION [ARCHIVED]
> *Legacy Phase completed before documentation refactor (2025-01-04).*
> *See `docs/control/05-tasks/99-archive/` for historical artifacts.*

* [x] **Epic 0: Scaffolding** (Container, Makefile, Repo structure)
* [x] **Epic 1: Core Architecture** (CUDA Driver Bindings, ADR-003, ADR-005)
* [x] **Epic 2: Visual Engine** (SDL2, OpenGL Interop, Orchestrator)

---

## 🟡 EPIC 3: THE MANDELBROT ENGINE [ACTIVE]
**Goal:** Implement the concrete compute engines and connect them to the Visual Orchestrator.

| ID | Name | Priority | Status | Owner |
| :--- | :--- | :--- | :--- | :--- |
| **3.1** | Compute Engine (FP64) | Critical | **ACTIVE** | **Engineer** |
| **3.2** | Verification Oracle | High | **PENDING** | - |

---

## 🔴 MAINTENANCE & ANOMALIES [BUFFER]
**Goal:** Fix regressions and blocked states.

| ID | Name | Priority | Status | Owner |
| :--- | :--- | :--- | :--- | :--- |
| **00X** | *(No active bugs)* | - | - | - |