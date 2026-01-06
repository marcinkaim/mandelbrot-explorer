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

# AGENTIC SQUAD: ROLES & CAPABILITIES

## 1. Philosophy: The Separation of Concerns
The project utilizes a specialized **Multi-Agent System** where responsibilities are strictly compartmentalized. This prevents "context bleeding" (e.g., an Engineer ignoring safety rules to fix a bug) and ensures a system of Checks and Balances.

* **Principle of Least Privilege:** Agents have write access only to specific artifacts.
* **Adversarial Collaboration:** The *Auditor* is explicitly designed to mistrust the *Engineer*. The *Architect* challenges the *Analyst*. This friction generates quality.
* **Context Injection:** Roles are defined as universal archetypes. Project-specific knowledge (Language: Ada, Tech: CUDA) is injected via `01-requirements` and `04-architecture`, not hardcoded into the role's identity.

## 2. Role Matrix

| Role | Focus Area | Primary Loop | Key Artifacts (Output) |
| :--- | :--- | :--- | :--- |
| **01 Manager** | Scheduling & State | Global | `roadmap-status`, Assignment |
| **02 Analyst** | Requirements & RFC | Definition (Ph1) | `Task Definition`, `RFC` |
| **03 Architect** | Strategy & Safety | Definition (Ph1) | `ADR`, `Impact Analysis` |
| **04 Engineer** | Implementation | Execution (Ph2) | `Source Code`, `Action Report` |
| **05 DevOps** | Infrastructure | Support | `Containerfile`, `Makefile` |
| **06 Auditor** | Compliance (Static) | Execution (Ph2) | `AR (Audit Result)` |
| **07 Tester** | Validation (Dynamic) | Execution (Ph2) | `AR (Test Result)` |

## 3. Role Definitions

### 3.1. The Management Layer
* **Manager (The Kernel):** The operating system of the project. It does not "think" creatively; it schedules. It ensures the `01-epics` buffer is fed and that the `roadmap` reflects reality. It resolves deadlocks (e.g., when Engineer reports `BLOCKED`).

### 3.2. The Strategy Layer (Definition Loop)
* **Analyst (The Translator):** Converts vague User intent into structured, unambiguous specifications. Responsible for the RFC process in `00-drafts`.
* **Architect (The Gatekeeper):** Responsible for the *integrity* of the system. Ensures that new features do not violate existing constraints (ADRs) or safety standards. Has Veto power in Phase 1.

### 3.3. The Execution Layer (Execution Loop)
* **Engineer (The Builder):** The primary worker. Focuses on solving the local problem defined in the Task. Must be hermetic—relies only on provided context.
* **DevOps (The Enabler):** Ensures the factory works. Maintains the build system, containers, and CI/CD pipelines.

### 3.4. The Quality Assurance Layer
* **Auditor (The Sceptic):** Performs *Static Analysis*. Reads the code before it runs. Checks for style, forbidden patterns, and adherence to ADRs.
* **Tester (The User Proxy):** Performs *Dynamic Analysis*. Runs the code. Checks if the "Definition of Done" is met in the runtime environment.

## 4. Universal vs. Project Context

To maintain reusability, Role Prompts are structured in two layers:

1.  **The Base Layer (Universal):**
    * Defined in `03-system-prompt.md`.
    * Contains: "You are an Auditor. You verify code. You do not trust comments."
    * *Reusable across projects.*

2.  **The Context Layer (Project Specific):**
    * Injected at runtime from `docs/control/`.
    * Contains: "We use Ada 2022. We use CUDA Driver API. No Unchecked_Conversion."
    * *Specific to Mandelbrot Explorer.*

## 5. Interaction Protocol
Agents do not communicate via chat history (which is volatile). They communicate via **Artifacts**:
1.  **Instruction:** Manager points to a `Task Definition`.
2.  **Handoff:** Agent produces an `Action Report` (AR).
3.  **Feedback:** Reviewer (Auditor/Architect) annotates the AR or Task.
