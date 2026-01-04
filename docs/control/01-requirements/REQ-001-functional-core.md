# REQ-001: FUNCTIONAL CORE & COMPUTE ENGINE

| Attribute | Details |
| :--- | :--- |
| **Document ID** | REQ-001 |
| **Status** | DRAFT |
| **Scope** | Math, Compute Engines, Rendering Pipeline |
| **Context** | Core Logic |

## 1. The Computation Engine (Core Logic)

### 1.1. The "Raw Data" Principle
To ensure real-time visual adjustments without re-computation, a strict separation of concerns is enforced:
* **Constraint:** Compute Kernels (CUDA/CPU) **MUST NOT** perform color mapping or produce ARGB pixels.
* **Output:** The result of a compute job is a 2D array of **Scalar Values** (Normalized Iteration Counts) stored in a Pixel Buffer Object (PBO).
* **Format:** The intermediate buffer format shall be `GL_R32F` (Single Precision Floating Point) or `GL_R64F` (if required for extremely smooth gradients), representing the "escape velocity" of each point.

### 1.2. Mathematical Definition
The system implements the classic Mandelbrot set iteration for complex number $C = x + iy$ and starting $Z_0 = 0$.

1.  **Iteration:** $Z_{n+1} = Z_n^2 + C$
2.  **Escape Condition:** $|Z_n| > 2.0$ (or $|Z_n|^2 > 4.0$ for optimization).
3.  **Smooth Coloring Output ($\nu$):**
    To eliminate "banding" artifacts, the engine must calculate the continuous potential:
    $$\nu(Z, n) = n + 1 - \log_2(\log_2(|Z_n|))$$
    * Where $n$ is the iteration count at escape.
    * The value $\nu$ is written to the output buffer.

## 2. Precision Tiers (Deep Zoom Strategy)

The system must automatically switch between compute engines based on the current Zoom Level ($Scale^{-1}$) to mitigate floating-point precision loss.

### 2.1. Tier 1: Hardware Double (FP64)
* **Range:** $1.0$ to $\approx 1.0 \times 10^{14}$ (Limited by machine epsilon of IEEE 754 Double).
* **Technology:** Native GPU instructions (`fma.rn.f64` in PTX) or CPU `Float64`.
* **Performance Target:** Real-time (>60 FPS) on dGPU.

### 2.2. Tier 2: Emulated Quad-Double
* **Range:** $\approx 1.0 \times 10^{14}$ to $\approx 1.0 \times 10^{28}$.
* **Technology:** "Double-Double" arithmetic.
    * A number $X$ is represented as a sum of two hardware doubles: $X = x_{hi} + x_{lo}$, where $|x_{lo}| \le 0.5 \cdot \text{ulp}(x_{hi})$.
    * Operations (Add/Mul) are implemented in software (CUDA/Ada) using error-propagation algorithms (e.g., Knuth/Dekker).
* **Performance Target:** Interactive (>15 FPS).

### 2.3. Tier 3: Arbitrary Precision (Reference)
* **Range:** $> 1.0 \times 10^{28}$.
* **Technology:** Software Multi-Precision library (optimized specifically for Mandelbrot operations).
* **Performance Target:** Background rendering (Async).

## 3. Visualization Pipeline (Post-Processing)

Coloring is strictly a **presentation layer** responsibility, executed by the OpenGL Pipeline.

### 3.1. The Fragment Shader
The Orchestrator must bind the PBO (containing raw $\nu$ values) as a `GL_TEXTURE_2D` (format `GL_RED`). The Fragment Shader performs:
1.  **Sampling:** Read the scalar value $\nu$ from the texture.
2.  **Transfer Function:** Map $\nu$ to an RGBA color.
    * *Input:* $\nu$, `Color_Offset` (Uniform), `Palette_Mode` (Uniform).
    * *Output:* `FragColor`.

### 3.2. Palette Modes
The shader must support dynamic switching between:
* **Continuous (Procedural):** Mathematical gradients (e.g., Sine-wave based HSV) allowing for smooth "Color Cycling" animation by updating a uniform time/offset variable.
* **Discrete (Texture Lookup):** Mapping $\nu$ to UV coordinates of a 1D Gradient Texture provided by the host.

## 4. Hardware Topology & Fallbacks

### 4.1. Primary Mode: NVIDIA Prime (dGPU)
* **Compute:** CUDA Kernels execute on the NVIDIA dGPU.
* **Storage:** The PBO resides in dGPU VRAM.
* **Display:** OpenGL renders the textured quad on the dGPU. The final image is blitted to the iGPU (via Prime Offload) by the window manager.
* **Data Flow:** `CUDA (VRAM) -> PBO (VRAM) -> Texture (VRAM) -> Display`. **Zero-Copy.**

### 4.2. Fallback Mode: CPU Host
* **Trigger:** User request OR CUDA Initialization failure.
* **Compute:** Ada Tasking + SIMD (SSE/AVX) instructions on the CPU.
* **Storage:** The PBO is mapped into Host Memory (`glMapBuffer`).
* **Display:** CPU writes to mapped pointer -> Unmap -> OpenGL Uploads to VRAM.
* **Efficiency:** While slower due to PCIe transfer, strict usage of SIMD is required to maintain interactivity.