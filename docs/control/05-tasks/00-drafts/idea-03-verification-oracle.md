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

# IDEA: Mathematical Verification Oracle for Compute Engines

## METADATA
* **Author:** User
* **Date:** 2026-01-05
* **Status:** NEW

## 1. The Concept (What?)
Creation of a mechanism to verify the correctness of GPU computations by comparing them against a reference CPU implementation (Oracle) for specific checkpoints.

## 2. Motivation (Why?)
We are entering the domain of numerical computing where rounding errors or kernel bugs can produce visually plausible but mathematically incorrect results. `REQ-003` (Safety & Integrity) implies correctness. A manual Tester cannot verify if pixel (400,300) has a value of 154.32 or 154.99. We need an automated unit test.

## 3. Rough Sketch
* **Method:** An AUnit test case that launches the kernel for a small array (e.g., 16x16).
* **Validation:** The same set of coordinates is calculated by a slow, precise Ada implementation on the CPU.
* **Assertion:** GPU results must match CPU results within a defined epsilon (e.g., $10^{-12}$).
* **Integration:** This test should be added to `mandelbrot_suite.adb`.
