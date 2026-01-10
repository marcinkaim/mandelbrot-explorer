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

# TASK DEFINITION

## METADATA
* **Task ID:** [task-X.Y-kebab-case-name]
* **Epic:** [Parent Epic Name]
* **Type:** [Feature | Refactor | Documentation]
* **Status:** [DRAFT | PENDING | ACTIVE | DONE]
* **Priority:** [Critical | High | Medium | Low]

## 1. Objective
[Clear, one-sentence goal of the task.]

## 2. Context Injection (Required Reading)
[List of files the Agent MUST read before starting. The "Sacred" Requirements.]
* `docs/control/01-requirements/REQ-XXX.md`
* `docs/control/04-architecture/ADR-XXX.md`
* `src/related/module.ads`

## 3. Key Responsibilities
[Step-by-step instructions or functional requirements.]
1.  [Requirement 1]
2.  [Requirement 2]

## 4. Constraints & Non-Functional Requirements
* **Safety:** [e.g., No Unchecked_Conversion allowed]
* **Performance:** [e.g., Zero-copy via PBO]
* **Tooling:** [e.g., Use SPARK provers]
* **Protocol Compliance:** ALL changes must be delivered as a **Semantic Diff** (P-001).

## 5. Definition of Done (DoD)
* [ ] Code compiles without warnings.
* [ ] Unit tests implemented and passing.
* [ ] Action Report (AR) generated.
* [ ] [Specific verification step]