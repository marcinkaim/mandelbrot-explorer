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

## 2. Performance Limits
* **Requirement:** The system must maintain responsiveness during heavy computation.
* **Metric:** UI thread must never freeze for > 16ms (60 FPS target).
* **Metric:** Compute kernels must be asynchronous (Non-blocking UI).

## 3. Deployment Constraints
* **OS:** Debian 13 (Trixie) or newer.
* **Hardware:** NVIDIA GPU with proprietary drivers (535+).
* **Compiler:** GNAT FSF 13+ / SPARK 2014 compatible.