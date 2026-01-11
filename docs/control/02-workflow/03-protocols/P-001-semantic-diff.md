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

# PROTOCOL: P-001 - SEMANTIC DIFF

## 1. Philosophy: "Context over Coordinates"

The **Semantic Diff** protocol is designed to bridge the gap between AI reasoning and file system operations. Unlike standard `git diff` (which relies on brittle line numbers and exact binary hashes), Semantic Diff relies on **unique content matching**.

It mimics how a Lead Engineer describes changes to a peer: *"In file X, find the function Y and replace it with Z."*

## 2. The Patch Structure

The official, executable template for this protocol is located at:
`docs/control/02-workflow/02-document-templates/template-semantic-patch.md`

All Agents generating code or documentation modifications MUST strictly adhere to the structure defined in that template.

### 2.1. The Container
Every response starts with a global header and a global summary table listing all affected files and their operation status (Modified, Created, Deleted, Moved).

### 2.2. Operations
The body of the patch consists of per-file operations.

* **Modification:** Used when changing existing content. Relies on finding unique text blocks to replace.
* **Creation:** Used when creating a new file from scratch.
* **Deletion:** Used when removing a file entirely.
* **Renaming/Moving:** Used when changing a file's path.

---

## 3. Rules of Engagement (The "Constraints")

To ensure the patch applies correctly and remains human-readable, Agents must adhere to these rules:

### Rule 1: Uniqueness via Context
The content inside `Lines to remove` MUST be unique in the target file.
* **Bad:** Removing just `end loop;`. (Matches multiple places).
* **Good:** Removing `end loop;` along with the 2 preceding lines of specific logic.

### Rule 2: Narrative Context (The "Why")
For every `Modified` or `Created` block, the Agent MUST provide a **Summary of changes** section immediately following the file header.
* This summary must explain the *logic* and *intent* of the changes in that specific file.
* It serves as a "micro-documentation" for the Auditor/Reviewer.
* *Note:* This is not required for `Deleted` or `Moved` operations unless significant context is lost.

### Rule 3: Markdown Hygiene
* Always use triple backticks (```) for code blocks.
* Ensure the language tag (e.g., `ada`, `bash`, `markdown`) is specified.
* If modifying a Markdown file that contains code blocks, use standard backticks. The parser is expected to handle nested blocks contextually, or the Agent should maximize context to avoid ambiguity.

### Rule 4: Whitespace & Indentation
* The `Lines to add` block must preserve the correct indentation relative to the file structure.
* The parser applying the patch should use "Fuzzy Whitespace Matching" (ignore leading/trailing whitespace) to locate the `Lines to remove` block, but use exact values from `Lines to add` when writing.

### Rule 5: Metadata Exclusion
* **No License Headers:** Do NOT include copyright or license headers (e.g., GPL preambles) in created files. These are handled by the CI/CD pipeline (e.g., `scripts/ensure_license_headers.sh`). Focus purely on the content/logic.

### Rule 6: Batch Size Limits (Complexity Control)
To ensure reviewability and reduce hallucination risks ("Blast Radius"), patches must adhere to limits:
* **Creation:** Max 1 file per patch.
* **Modification:** Max 2 files per patch (if any content is modified).
* **Deletion/Move:** Max 10 files per patch (batch refactoring allowed).

## 4. Parser Logic (Reference Implementation Guide)

The tooling applying this patch operates as follows:

1. **Parse Header:** Identify the target file and operation type (Modified, Created, Deleted, Moved).
2. **For Modifications:**
    * Load target file into memory.
    * Extract the content of `Lines to remove`.
    * Perform a string search in the target file.
    * **Validation:**
        * If matches found == 0: **ERROR** (Context mismatch).
        * If matches found > 1: **ERROR** (Ambiguous context).
        * If matches found == 1: **APPLY** (Replace removing block with adding block).
3. **For Creation:** Write `Lines to add` to disk.
4. **For Deletion:** Remove file from disk.
5. **For Move:** Rename file on disk.
