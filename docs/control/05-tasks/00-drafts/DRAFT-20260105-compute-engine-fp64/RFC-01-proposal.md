# RFC-01: FP64 Compute Engine Implementation Strategy

## METADATA
* **RFC ID:** RFC-01-compute-engine-fp64
* **Parent Draft:** DRAFT-20260105-compute-engine-fp64
* **Author:** Analyst
* **Type:** PROPOSAL

## 1. Context
Based on Idea-01 and Idea-02. We need to implement the Tier 1 rendering engine targeting the NVIDIA RTX 3500. This involves a low-level PTX kernel and a high-level Ada wrapper.

## 2. Content (The Specifications)

### A. The Kernel (`mandelbrot_fp64.ptx`)
* **Math:** Must implement the smooth coloring formula defined in `REQ-001`.
* **Precision:** Standard IEEE 754 Double Precision.
* **Optimization:** Loop unrolling mandated for the main iteration loop.
* **Interface:**
    1.  `param_c_min_re` (f64)
    2.  `param_c_min_im` (f64)
    3.  `param_step_re` (f64)
    4.  `param_step_im` (f64)
    5.  `param_max_iter` (u32)
    6.  `param_output_ptr` (u64 - Device Ptr)
    7.  `param_width` (u32)

### B. The Wrapper (`Compute.Double_Engine`)
* **Pattern:** Implements `Render_Interface.Compute_Engine`.
* **Lifecycle:**
    * `Initialize`: Loads PTX via `cuModuleLoad`.
    * `Render_Tile`: Maps PBO -> Gets Device Ptr -> Sets Params -> Launches Grid.
* **Safety:** Must use `ADR-0006` compliant unsafe conversions in the private body only.

## 3. Open Questions for Architect
1.  **Block Size:** Should we hardcode block dimensions (e.g., 16x16) or allow dynamic tuning?
2.  **PBO Mapping:** Do we map the PBO once per frame, or keep it mapped if possible? (Performance vs Safety trade-off).
3.  **Error Handling:** How do we propagate a CUDA launch failure to the UI thread?

## 4. Recommendation
Merge both ideas into a single Atomic Task to ensure binary compatibility between Host and Device code.
