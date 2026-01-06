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

# ADR-0006: Safe Interop Boundaries for Hardware Acceleration

* **Status:** ACCEPTED
* **Date:** 2026-01-04
* **Author:** Architect
* **Deciders:** Architect, Engineer, Auditor
* **Consulted:** DevOps (Build System), Auditor (Validation)

## 1. Context and Problem Statement

The project adheres to strict safety standards defined in `REQ-003` (Non-Functional Requirements), which explicitly prohibits the use of `Ada.Unchecked_Conversion` and direct address manipulation (`System.Address`) to prevent memory corruption and type confusion.

However, the Mandelbrot Explorer requires high-performance interaction with the NVIDIA GPU via the CUDA Driver API (`libcuda.so`). The CUDA API is a C-based interface that operates on:
1.  Raw pointers (`void*`, `CUdeviceptr`).
2.  Opaque handles (integers acting as references).
3.  Direct memory mapping between Host (CPU) and Device (GPU).

**The Conflict:** Strict adherence to `REQ-003` makes it mathematically impossible to invoke `cuLaunchKernel` or manage GPU memory, as these operations require type-casting Ada's strong types into C's untyped pointers. We face a deadlock: strict safety prevents functionality.

## 2. Decision Drivers

* **Necessity:** GPU acceleration is a core functional requirement (`REQ-001`).
* **Reality:** Hardware drivers interact via raw memory addresses, not high-level Ada types.
* **Containment:** We must bridge the gap without "polluting" the core application logic with unsafe code.

## 3. Decision

We decide to **relax the prohibition of `Ada.Unchecked_Conversion` and `System.Address`**, subject to the following strict constraints. This constitutes the "Safe Interop Boundary" policy.

### 3.1. Authorized Zones
Unsafe memory operations are **ONLY** permitted within the following file scopes:

1.  **`src/binding/*`**: Files responsible for direct mapping of C API signatures to Ada specs (Thin Bindings).
2.  **`src/compute/gpu_context.adb`**: The specific body responsible for managing the lifecycle of CUDA Contexts and raw memory pointers.

### 3.2. Mandatory Protocols
Any usage of `Unchecked_Conversion`, `System.Address`, or `System.Storage_Elements` in the authorized zones must adhere to the following rules:

1.  **The "Safety Tag" Rule:** Every line or block containing an unsafe operation must be immediately preceded by a comment explicitly referencing this ADR:
    ```ada
    -- SAFETY: Authorized by ADR-0006 (Hardware Interop)
    function To_Void_Ptr is new Ada.Unchecked_Conversion (Job_Record_Access, System.Address);
    ```
2.  **Type Isolation:** Unsafe types (e.g., `Void_Ptr`) must NOT leak into the public specification (`.ads`) of high-level packages. They must be encapsulated within the package body or private part.

### 3.3. Explicit Prohibition Elsewhere
Usage of these constructs in `src/app/*` (Business Logic) or `src/ui/*` (Presentation Layer) remains **STRICTLY FORBIDDEN**. The Auditor is instructed to reject any PR violating this rule.

## 4. Consequences

### Positive
* **Enables Functionality:** Removes the blocker for implementing Task 3.1 (Double Engine Compute).
* **Clarifies Rules:** Engineers now have a clear pattern for implementing low-level drivers.
* **Simplifies Audit:** Auditors can `grep` for "ADR-0006" to verify compliance.

### Negative
* **Local Risk:** The authorized zones contain code that is not memory-safe by default. Bugs here can cause Segfaults.
* **Mitigation:** These zones require higher testing coverage (100% statement coverage) and rigorous manual code review, as automated proofs (SPARK) may not be applicable.

## 5. Compliance Verification

* **Static Analysis:** The `Auditor` agent will scan for `Unchecked_Conversion`. If found outside the Authorized Zones, the build fails.
* **Manual Review:** Reviewers must verify that the `SAFETY` tag is present and the conversion is logically sound.
