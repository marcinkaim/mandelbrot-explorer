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
    * Contains: Context, Objective, Key Responsibilities, Definition of Done (DoD).
3.  **Approval:** The Operator (Human) accepts the task content.

### Phase 2: Execution (Engineers)
1.  **Context Loading:** The Operator starts the appropriate Agent (e.g., Graphics Engineer) and provides:
    * `00_PROJECT_CONTEXT.md`
    * `02_ARCHITECTURE_DECISIONS.md`
    * The specific task file (e.g., `Task_2.1...md`).
    * Current source code (relevant files).
2.  **Implementation:** The Agent generates code or documentation.
3.  **Verification:** The Operator compiles the code (`make build`) and reports errors. The Agent fixes the code iteratively until it compiles and runs.

### Phase 3: Architecture Audit (Software Architect)
*Crucial Step: Code must be reviewed before it is considered "Done".*
1.  **Submission:** The Operator or Engineer signals that the implementation is ready for review.
2.  **Review:** The Architect analyzes the code focusing on:
    * **Compliance:** Does it match `ADR-005` and `PROJECT_CONTEXT`?
    * **Safety:** Are there memory leaks (`System.Address` misuse) or race conditions?
    * **Design:** Is the separation of concerns maintained?
3.  **Decision:**
    * **Approve:** The task proceeds to *Phase 4*.
    * **Reject:** The task returns to *Phase 2* with a list of required fixes (Recommendations).

### Phase 4: Closure (Project Manager)
1.  **Check:** When the Architect approves and `Definition of Done` is met, the code is committed.
2.  **Archive:** PM moves the task file from `active_tasks` to `completed_tasks`.
3.  **Update:** PM updates `01_ROADMAP_STATUS.md`.

## 3. Guidelines
1.  **Single Source of Truth:** If the code contradicts the documentation (`ADR` or `Context`), the error is in the code (unless we are consciously updating the documentation).
2.  **Hermetic Context:** Every Agent must receive a full set of context files when starting work. We do not assume the Agent "remembers" previous conversations.
3.  **Audit First:** No complex task is moved to `completed_tasks` without an explicit approval from the Software Architect role.