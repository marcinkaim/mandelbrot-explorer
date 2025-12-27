# WORKFLOW: CONTEXT-DRIVEN AGENTIC DEVELOPMENT

The project is conducted in a hybrid model (Human + AI Agents). The core principle is that **Documentation is the Source of Truth**, and the code is its derivative.

## 1. Management Structure (The "Brain" Directory)
The project state is fully defined by the content of the `docs/management/` directory:
* `00_PROJECT_CONTEXT.md` - Core requirements and constraints (Specification).
* `01_ROADMAP_STATUS.md` - High-level status (What is done, what we are doing).
* `active_tasks/*.md` - Tasks currently being executed. Only a few at a time.
* `completed_tasks/*.md` - Archive of completed work.

## 2. Task Lifecycle

### Phase 1: Definition (Project Manager)
1.  **Analysis:** PM analyzes the Roadmap and creates a new task.
2.  **Task File:** A file is created, e.g., `docs/management/active_tasks/Task_2.2_Audit.md`.
    * Contains: Context, Objective, Key Responsibilities, Deliverables, Definition of Done (DoD).
3.  **Approval:** The Operator (Human) accepts the task content.

### Phase 2: Execution (Engineers / DevOps)
1.  **Context Loading:** The Operator starts the appropriate Agent (e.g., Graphics Engineer) and provides strict context files defined in the Task File.
2.  **Implementation:** The Agent generates code, configuration, or documentation.
3.  **Self-Verification & Reporting (The "Mirror" Step):**
    * Before submitting, the Agent MUST verify constraints from `00_PROJECT_CONTEXT.md`.
    * **MANDATORY:** The Agent appends an **Engineer's Field Report (EFR)** to the output (or directly updates the Task MD file).
    * **EFR Structure:**
        * **Deviations:** Explicit list of changes made against the original spec/ADR.
        * **Risk Assessment:** "I am least confident about X", "Potential memory leak in Y".
        * **Verification Method:** How was the code verified locally (beyond just compilation)?
4.  **Submission:** The Operator compiles the code (`make build`). If successful, the task moves to Phase 3.

### Phase 3: Architecture Audit (Software Architect)
*Crucial Step: Code must be reviewed before it is considered "Done".*
1.  **Context Loading:** Architect receives: The Code + The Task File + **The Engineer's Field Report**.
2.  **Review:** The Architect analyzes the code focusing on:
    * **Compliance:** Does it match `ADR` and `PROJECT_CONTEXT`?
    * **Honesty Check:** Does the code contain risks NOT mentioned in the EFR?
    * **Safety:** Are there memory leaks, race conditions, or type safety violations?
3.  **Decision:**
    * **Approve:** The task proceeds to *Phase 4*.
    * **Reject:** The task returns to *Phase 2* with a list of required fixes.

### Phase 4: Closure (Project Manager)
1.  **Check:** When the Architect approves and `Definition of Done` is met, the code is committed.
2.  **Archive:** PM moves the task file from `active_tasks` to `completed_tasks`.
3.  **Update:** PM updates `01_ROADMAP_STATUS.md`.

## 3. Guidelines
1.  **Single Source of Truth:** If the code contradicts the documentation (`ADR` or `Context`), the error is in the code (unless we are consciously updating the documentation).
2.  **Hermetic Context:** Every Agent must receive a full set of context files when starting work.
3.  **Audit First:** No complex task is moved to `completed_tasks` without an explicit approval from the Software Architect.
4.  **Silence is Liability:** Engineers must explicitly report hacks, shortcuts, or uncertainties in the **EFR**. If a bug is found in Audit that was not reported in EFR, the process has failed.