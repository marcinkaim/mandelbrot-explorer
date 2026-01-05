# IDEA: Hardware Double-Precision Kernel Implementation

## METADATA
* **Author:** User
* **Date:** 2026-01-05
* **Status:** NEW

## 1. The Concept (What?)
Creation of a highly optimized CUDA Kernel (written in PTX assembly) that performs the Mandelbrot set iteration using hardware `double` precision (FP64).

## 2. Motivation (Why?)
Currently, the repository only contains test kernels (`test_ops.ptx`). To fulfill `REQ-001` (Functional Core), we require a proper mathematical engine. We must leverage the FP64 units of the RTX 3500 to achieve smooth 60 FPS at zoom levels up to $10^{14}$. Relying on unoptimized C++ without control over the generated PTX instructions may introduce overhead or suboptimal register usage.

## 3. Rough Sketch
* **Input:** Center coordinates ($Cx, Cy$), Zoom, Max Iterations, Output Buffer Pointer.
* **Output:** An array of `double` (or `float`) values, where each value represents the normalized potential (Smooth Coloring: $n + 1 - \log_2(\log_2(|Z|))$).
* **Constraints:** The kernel **MUST NOT** write ARGB pixels. It writes raw scalar data to a mapped PBO buffer, adhering to the "Raw Data" principle defined in `REQ-001`.
* **Optimization:** Aggressive loop unrolling and minimization of local memory usage to prevent register spilling.
