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

# Action Plan: [TASK-ID]

## 0. Traceability & Meta
* **Sequence ID:** PLAN-[XX] (e.g., PLAN-01, PLAN-02-fix)
* **Task:** [TASK-ID] (The invariant parent task, e.g., TASK-3.1)
* **Trigger/Context:** [DOCUMENT-ID]
    * *Standard:* Same as Task ID (if first run).
    * *Fix Loop:* Reference to the Rejecting AR (e.g., AR-02-auditor-reject).
    * *Unblocked:* Reference to Architect's Decision/RFC (e.g., RFC-12 or ADR-05).
* **Role:** [ENGINEER | DEVOPS | TESTER]
* **Date:** 2026-XX-XX
* **Input Commit:** [Git Hash base for this plan]

## 1. Strategy & Approach
> **Objective:** [Restate the goal in technical terms specific to this step]
> **Constraint Check:** [Confirm alignment with specific ADRs or Requirements pertinent to this step, e.g., "Strict adherence to ADR-0006 for interop"]
> **Hypothesis:** [If applicable (Tester), what are we proving? If Engineer, what pattern are we applying?]

## 2. Execution Steps (The Algorithm)
> (Agent: Define discrete, atomic steps. Each step should ideally correspond to a potential git commit or a logical verification unit.)

### Step 1: [Short Name, e.g., "Generate Thin Binding Spec"]
* **Action:** [Detailed description of the operation]
* **Target Files:**
    * `src/...`
* **Verification:** [How will this specific step be verified? e.g., "Code compiles", "GPR builds"]

### Step 2: [Short Name, e.g., "Implement Safety Wrapper"]
* **Action:** ...
* **Target Files:** ...
* **Verification:** ...

### Step 3: [Short Name, e.g., "Integration Test"]
* **Action:** ...
* **Target Files:** ...
* **Verification:** ...

## 3. Verification Strategy (Definition of Success)
> [How do we confirm the entire plan succeeded before moving to Reporting?]
* **Automated Checks:** [e.g., `alr test`, `gnatprove`]
* **Manual Verification:** [e.g., "Inspect visual output for artifacts"]

## 4. Rollback & Contingency
> [What if execution fails mid-plan?]
* **Checkpoint:** [e.g., "If Step 2 fails, revert to commit X"]
* **Risks:** [Identify potential blockers, e.g., "Library version mismatch"]
