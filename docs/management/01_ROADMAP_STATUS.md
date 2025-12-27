# PROJECT ROADMAP & STATUS

## 🟢 EPIC 0: Project Scaffolding & CI/CD [DONE]
* [x] Task 0.1: Build Environment & Repo Init.

## 🟢 EPIC 1: The Core (Compute & Architecture) [DONE]
* [x] Task 1.1: Ada Binding to CUDA Driver API (+ Graphs).
* [x] Task 1.5: High-Level System Architecture (ADR-005).

## 🟡 EPIC 2: The Visual Engine (Orchestration) [IN PROGRESS]
**Goal:** Windowing, OpenGL context, and thread management.
* [x] Task 2.1: Orchestrator Implementation & SDL2/GL Backend.
* [x] Task 2.2: Architecture Compliance Review (Audit).
* [x] Task 2.1 (Hotfix): Architecture Compliance Corrections.
* [ ] **Task 2.3: Dynamic Resolution & Window Resizing.**
    * *Status:* ACTIVE.

## 🔴 MAINTENANCE & BUGS [ACTIVE]
* [x] Bug 001: Fix Unit Test Build & Execution Configuration.

## 🟡 EPIC 3: The Mandelbrot Engine (Compute) [READY]
**Goal:** First fractal render.
* [ ] Task 3.1: Concrete Double-Precision Engine.
    * *Context:* Plug into Orchestrator.

## ⚪ EPIC 4: CPU Fallback (SSE Optimized) [PLANNED]
## ⚪ EPIC 5: Deep Zoom (Multi-Precision PTX) [PLANNED]
