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

# Semantic Patch

## Summary
* **Modified**: `[path/to/modified/file.ext]`
* **Created**: `[path/to/new/file.ext]`
* **Deleted**: `[path/to/removed/file.ext]`
* **Moved**: `[old/path/file.ext] -> [new/path/file.ext]`

---

## File `[path/to/modified/file.ext]` modified:

### Summary of changes

[Brief explanation of the logic changes in this specific file. E.g., "Refactored the main loop to use generic iterators for thread-safety."]

### Change 1:

#### Lines to remove:

```[language_tag]
[Original code block to be replaced. MUST include sufficient surrounding context to be unique in the file.]

```

#### Lines to add:

```[language_tag]
[New code block. MUST preserve indentation relative to the file structure.]

```

### Change 2:

#### Lines to remove:

```[language_tag]
[...]

```

#### Lines to add:

```[language_tag]
[...]

```

---

## File `[path/to/new/file.ext]` created:

### Summary of changes

[...]

### Change 1:

#### Lines to remove:

#### Lines to add:

```[language_tag]
[Full content of the new file]

```

---

## File `[path/to/removed/file.ext]` deleted.

---

## File `[old/path/file.ext]` moved to `[new/path/file.ext]`.
