# RFC-01: CPU-Reference Verification Oracle

## METADATA
* **RFC ID:** RFC-01-verification-oracle
* **Parent Draft:** DRAFT-20260105-verification-oracle
* **Author:** Analyst
* **Type:** PROPOSAL

## 1. Context
Based on Idea-03. To satisfy `REQ-003` (Integrity), we cannot trust the GPU implicitly. We need a "Source of Truth".

## 2. Content
We propose extending the `Mandelbrot_Suite` with a new test category: `Test_Mathematical_Correctness`.

### Implementation Details
1.  **The Reference:** A pure Ada function `Mandelbrot_Reference (C : Complex) return Float` running on the CPU.
2.  **The Test:**
    * Initialize `GPU_Context`.
    * Alloc small output buffer (e.g., 16x16).
    * Run Kernel on specific coordinates.
    * Copy back results.
    * Compare each pixel: `abs(GPU_Val - CPU_Val) < Epsilon`.
3.  **Epsilon Definition:** $1.0 \times 10^{-12}$ (Allowing for FMA differences between CPU and GPU).

## 3. Open Questions for Architect
1.  **FMA Consistency:** GPU uses Fused Multiply-Add. If Ada on CPU doesn't use FMA, results might diverge slightly. Is $10^{-12}$ loose enough?
2.  **Performance:** This test will be slow. Should it run on every `make test` or only in `Validation` profile?

## 4. Recommendation
Approve as a mandatory blocking task before merging the Compute Engine.
