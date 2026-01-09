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

# Action Report: [TASK-ID]

* **Sequence ID:** AR-[XX] (e.g., AR-01, AR-02)
* **Relates To:** [TASK-ID | RFC-ID | AR-XX] (Preceding documents essential for this context)
* **Date:** YYYY-MM-DD
* **Role:** [ENGINEER | DEVOPS | AUDITOR | TESTER]
* **Input Commit:** [Hash before action]
* **Outcome:** [SUCCESS | FAILURE | NEEDS_INFO]

## 1. Executive Summary
> **Outcome:** Briefly describe the final result. What was achieved?
> **Process & Reasoning:** Summarize the execution path. What logic was applied? What obstacles were encountered and how were they overcome? (Focus on the *transition function*—how we got from A to B, not just the final state B).
> *Example:* "Implemented double-buffering. Initially attempted using a single mutex, but high contention led to 40% performance drop (see obstacle). Pivoted to atomic pointers for swap chain (reasoning), restoring expected framerate."

## 2. Technical Details & Evidence (The "Meat")
> Fill the subsection relevant to your role. Delete or leave others empty.

### 2.1 Implementation (Engineer / DevOps)
* **Key Changes:** (List modified components/files)
* **Design Decisions:** (Why did you choose this path? Reference ADRs)
* **Safety Protocol:**
    * [ ] Checked `ADR-0006` (Safe Interop) - all unsafe blocks tagged?
    * [ ] Hermetic Build respected?

### 2.2 Inspection Findings (Auditor)
* **Compliance Checklist:**
    * `REQ-003` (Type Safety): [PASS/FAIL]
    * `ADR-0006` (Interop Boundaries): [PASS/FAIL] - Verified no leak of `System.Address`.
    * `ADR-0007` (Resilience): [PASS/FAIL]
* **Static Analysis:** (Output summary from GNATprove / compiler warnings)

### 2.3 Validation Telemetry (Tester)
* **Environment:** (OS, Kernel, GPU Driver)
* **Metrics:**
    * GPU Usage: `[Paste nvidia-smi output or %]`
    * Frame Time: `XX.X ms`
* **Defects:**
    * [ ] No critical defects found.
    * [ ] Defects found! See attached **Bug Report**: `BUG-[ID]`

## 3. Blockers & Risks
> Are there any impediments preventing the next step in the loop?