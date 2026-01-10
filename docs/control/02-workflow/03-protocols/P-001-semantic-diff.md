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

Every response involving code or documentation modification MUST follow this Markdown structure:

### 2.1. The Container
The output starts with a global header and a summary.

```markdown
# Semantic Patch

## Summary
* **Modified**: `src/app/main.adb`
* **Created**: `docs/new_concept.md`
* **Deleted**: `legacy/old_script.sh`

```

### 2.2. Operations

After the summary, list operations file by file.

#### A. Modification

Used when changing existing content.

```markdown
## File `path/to/existing/file.ext` modified:

### Hunk 1:
#### Lines to remove:
```[lang]
[Original code block to be replaced]
[MUST contain enough context lines to be unique in the file]

```

#### Lines to add:

```[lang]
[New code block]

```

```

#### B. Creation
Used when creating a new file.

```markdown
## File `path/to/new/file.ext` created:

### Hunk 1:
#### Lines to remove:

#### Lines to add:
```[lang]
[Full content of the new file]

```

```

#### C. Deletion
Used when removing a file entirely. No code blocks required.

```markdown
## File `path/to/file.ext` deleted.

```

#### D. Renaming / Moving

Used when moving a file.

```markdown
## File `old/path/file.ext` moved to `new/path/file.ext`.

```

---

## 3. Rules of Engagement (The "3 Constraints")

To ensure the patch applies correctly, Agents must adhere to these rules:

### Rule 1: Uniqueness via Context

The content inside `Lines to remove` MUST be unique in the target file.

* **Bad:** Removing just `end loop;`. (Matches multiple places).
* **Good:** Removing `end loop;` along with the 2 preceding lines of specific logic.

### Rule 2: Markdown Hygiene

* Always use triple backticks (```) for code blocks.
* Ensure the language tag (e.g., `ada`, `bash`, `markdown`) is specified.
* If modifying a Markdown file that contains code blocks, use standard backticks. The parser is expected to handle nested blocks contextually or the Agent should maximize context to avoid ambiguity.

### Rule 3: Whitespace & Indentation

* The `Lines to add` block must preserve the correct indentation relative to the file structure.
* The parser applying the patch should use "Fuzzy Whitespace Matching" (ignore leading/trailing whitespace) to locate the `Lines to remove` block, but use exact values from `Lines to add` when writing.

### Rule 4: Metadata Exclusion
* **No License Headers:** Do NOT include copyright or license headers (e.g., GPL preambles) in created files. These are handled by the CI/CD pipeline (e.g., `scripts/ensure_license_headers.sh`). Focus purely on the content/logic.

### Rule 5: Batch Size Limits (Complexity Control)
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
