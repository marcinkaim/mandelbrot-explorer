# CHANGE REQUEST (CR)

## METADATA
* **CR ID:** [CR-YYYYMMDD-topic]
* **Requester:** [User | Architect | Engineer]
* **Target Phase:** [Definition Loop | Execution Loop]
* **Priority:** [Critical | Standard | Low]
* **Status:** [PROPOSED | APPROVED | REJECTED]

## 1. Target Artifacts
[List the immutable documents that need to be modified.]
* `docs/control/01-requirements/REQ-XXX.md`
* `docs/control/05-tasks/01-epics/01-core/task-1.1-example.md`

## 2. Reason for Change (The "Why")
[Explain the motivation. Is it a missed requirement? A technical impossibility? A UX improvement?]
* **Context:** ...
* **Justification:** ...

## 3. Proposed Change (The "What")
[Describe the delta. Use "Before/After" if possible.]
* **Current State:** ...
* **Desired State:** ...

## 4. Impact Analysis (Crucial)
[Agenci must analyze this section carefully.]
* **Architecture:** Does this contradict any ADR?
* **Scope:** Does this invalidate existing `DONE` tasks?
* **Safety:** Does this introduce new risks (e.g., relaxing type safety)?

## 5. Decision Record
* **Decision:** [APPROVED / REJECTED]
* **Approver:** [Architect / Manager]
* **Action Items:**
    1. Update `REQ-XXX`.
    2. Create new Task `task-Y.Z`.
    3. Obsolete Task `task-A.B`.