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

# REQ-003: Non-Functional Requirements (Safety & Integrity)

* **Type:** Non-Functional
* **Priority:** CRITICAL
* **Parent:** REQ-000 (Project Vision)

## 1. Safety & Type System Integrity
The system must leverage the Ada/SPARK type system to guarantee absence of runtime errors in the core logic.

### 1.1 The "Strong Typing" Directive
* **Requirement:** The codebase must rely on strong typing to enforce logical correctness at compile time.
* **Strict Prohibition:** The use of `Ada.Unchecked_Conversion`, `System.Address`, and `System.Storage_Elements` is **FORBIDDEN** in the Core Application Logic (`src/app`) and Domain Logic.
* **Exceptions:**
    * Direct memory manipulation is **permitted only** within:
        1.  `src/binding/*` (Thin Bindings to C APIs).
        2.  `src/compute/gpu_context.adb` (GPU Resource Management).
    * **Condition:** Every usage in these zones must be explicitly authorized by an **Accepted ADR** (specifically **ADR-0006**) and documented with a `-- SAFETY: ...` comment tag.
    * **Verification:** The Auditor must reject any Pull Request containing unsafe constructs outside of these authorized zones.

### 1.2 Hermetic State
* **Requirement:** Global mutable state is prohibited. All state must be encapsulated within Task objects or protected objects.

### 1.3 Mathematical Verification Strategy [UPDATED]
* **Requirement:** To mitigate the risk of silent numerical errors in GPU hardware or PTX kernels, the system must implement an automated "Verification Oracle".
* **Mechanism:** A CPU-based reference implementation (Source of Truth) must be available in the test suite.
* **Validation:** Every release build must pass a comparison test where GPU results match CPU results within a tolerance of $\epsilon = 1.0 \times 10^{-12}$.
* **Traceability:** Implemented by `task-3.2-verification-oracle`.

## 2. Performance Limits
* **Requirement:** The system must maintain responsiveness during heavy computation.
* **Metric:** UI thread must never freeze for > 16ms (60 FPS target).
* **Metric:** Compute kernels must be asynchronous (Non-blocking UI).

## 3. Deployment Constraints
* **OS:** Debian 13 (Trixie) or newer.
* **Hardware:** NVIDIA GPU with proprietary drivers (535+).
* **Compiler:** GNAT FSF 13+ / SPARK 2014 compatible.

## 4. Legal & Compliance
* **Requirement:** The codebase must maintain strict compliance with the GPLv3 license.
* **Validation:** All manual source files must contain the standard GPLv3 header preamble.
* **Automation:** Compliance must be verified automatically during the CI/CD process (Auditor role).