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

* **Sequence ID:** AR-[XX] (e.g., AR-01, AR-02 - check previous files to increment)
* **Relates To:** [TASK-ID | RFC-ID | AR-XX]
* **Date:** 2026-XX-XX
* **Role:** [ENGINEER | DEVOPS | AUDITOR | TESTER]
* **Input Commit:** [Git Hash before this action]
* **Outcome:** [SUCCESS | FAILURE | NEEDS_INFO]

## 1. Executive Summary
> **Outcome:** [Briefly describe the final result. What was achieved?]
> **Process & Reasoning:** [Summarize the execution path. Focus on the transition function—how we got from A to B. If obstacles were encountered, explain the pivot logic.]

## 2. Technical Details & Evidence
> (Agent: Keep only the section below relevant to your Role. Delete the others.)

### 2.1 Implementation (Engineer / DevOps)
* **Key Changes:** * [List modified components/files]
* **Design Decisions:** * [Why did you choose this path? Reference ADRs if applicable]
* **Safety Protocol:**
    * [ ] Checked `ADR-0006` (Safe Interop) - all unsafe blocks tagged?
    * [ ] Hermetic Build respected?

### 2.2 Inspection Findings (Auditor)
* **Compliance Checklist:**
    * `REQ-003` (Type Safety): [PASS/FAIL]
    * `ADR-0006` (Interop Boundaries): [PASS/FAIL] - Verified no leak of `System.Address`.
    * `ADR-0007` (Resilience): [PASS/FAIL]
* **Static Analysis:** * [Output summary from GNATprove / compiler warnings]

### 2.3 Validation Telemetry (Tester)
* **Environment:** [OS, Kernel, GPU Driver version]
* **Metrics:**
    * GPU Usage: `[Paste nvidia-smi output or %]`
    * Frame Time: `XX.X ms`
* **Validation Outcome:** [PASS | FAIL]

#### Defect Details (Fill only if Outcome == FAIL)
> **Merging Bug Report into Action Report for atomic context.**
* **Severity:** [Critical | Major | Minor]
* **Symptoms:** * [What is happening? Paste Error Logs / Screenshots description]
* **Reproduction Steps:**
    1. [Step 1]
    2. [Step 2]
* **Expected vs Actual:**
    * *Expected:* [What should happen?]
    * *Actual:* [What happened instead?]
* **Hypothesis:** * [Initial analysis of the root cause. Is it a Race Condition? Memory Leak? Logic Error?]

## 3. Blockers & Risks
> [Are there any impediments preventing the next step in the loop? If Outcome is FAILURE, this is likely 'Yes'.]