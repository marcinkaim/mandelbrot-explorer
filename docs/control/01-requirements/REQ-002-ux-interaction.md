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

# REQ-002: UX, INPUT & VISUAL FLUIDITY

| Attribute | Details |
| :--- | :--- |
| **Document ID** | REQ-002 |
| **Status** | DRAFT |
| **Scope** | Input Handling, Animation, HUD |
| **Context** | User Experience |

## 1. The "Slippy Map" Paradigm (Visual Persistence)

The user experience must mirror modern mapping applications (e.g., Google Maps). The rendering loop is strictly decoupled from the computation loop to ensure 60 FPS fluidity regardless of calculation complexity.

### 1.1. Visual Persistence Rule
The screen must **never** be black or empty during navigation.
* **Requirement:** When the user Zooms In/Out or Pans, the currently available texture data must be geometrically transformed (scaled/translated) immediately via OpenGL matrices.
* **Progressive Refinement:**
    1.  **Stage 0 (Immediate):** Display the old texture (Parent Tile), scaled to the new view. (Visuals: Blurry/Pixelated).
    2.  **Stage 1 (Computing):** Background worker calculates the new, sharp tile.
    3.  **Stage 2 (Refinement):** Once the new tile is ready in the PBO, it replaces the placeholder.

### 1.2. Coordinate Systems
The system must maintain two synchronized coordinate spaces:
1.  **Screen Space:** Pixel coordinates ($0..Width$, $0..Height$).
2.  **World Space:** Complex plane coordinates ($Re$, $Im$).
* **Constraint:** All input events (clicks, scrolls, pinches) must be accurately mapped from Screen Space to World Space to determine the new anchor points.

## 2. Navigation Physics (Mandatory)

Navigation follows a physics-based model, treating the camera as a physical object with mass and friction.

### 2.1. Inertia (Momentum)
* **Requirement:** When a Pan gesture (Mouse Drag or Touchpad Scroll) ends, the view must not stop instantly. It must continue moving in the last vector's direction, decelerating over time.
* **Math Model:**
    $$v_{t+1} = v_t \cdot (1 - \mu)$$
    * Where $\mu$ is the coefficient of friction.
    * Velocity $v$ approaches zero asymptotically.
* **Interaction:** Any new input interaction immediately kills the residual momentum (sets $v=0$) before applying the new vector.

### 2.2. Zoom Physics
* **Smooth Transition:** Changes in Zoom Level must be interpolated over time (Logarithmic Interpolation), preventing "jumpy" transitions between zoom steps.

## 3. Input Specifications

The application must support three distinct input modalities simultaneously.

### 3.1. Mouse Interaction
* **Left Button Drag:** Pans the view (grabbing the map).
* **Scroll Wheel:** Zooms In/Out.
    * **Anchor:** The zoom must be centered on the **current mouse cursor position** in World Space, not the screen center.

### 3.2. Keyboard Interaction
* **WASD / Arrows:** Pan the view (Discrete steps).
* **Q / E** (or +/-): Zoom In/Out.
    * **Anchor:** Screen Center.
* **ESC:** Immediate graceful shutdown.

### 3.3. Touchpad Gestures (Multi-Touch)
To support the "Engineering Laptop" use case, native trackpad gestures are mandatory via SDL2 `SDL_MultiGestureEvent`.
* **Two-Finger Scroll:** Pans the view (Mapped to Inertia Physics).
* **Pinch-to-Zoom:**
    * **Action:** Spreading fingers zooms in; pinching zooms out.
    * **Anchor:** The center point between the two fingers must remain fixed in World Space during the transformation.
    * **Sensitivity:** The zoom rate must correspond 1:1 to the finger expansion ratio.

## 4. Heads-Up Display (HUD)

A minimal, non-intrusive text overlay must provide engineering metrics.

### 4.1. Metrics
The HUD must display:
1.  **Performance:**
    * `UI FPS`: Frame rate of the Render Loop (Target: 60+).
    * `Compute Time`: Time taken to calculate the last tile (ms).
2.  **Coordinates:**
    * `Re / Im`: Center point of the view.
    * `Zoom`: Current magnification (Scientific Notation, e.g., $4.2 \times 10^{18}$).
3.  **System State:**
    * `Backend`: Currently active engine (e.g., `CUDA FP64`, `CPU AVX`, `Emulated Quad`).
    * `Status`: "Rendering", "Idle", "Buffered".

### 4.2. Implementation
* The HUD renders as a separate UI layer (text rendering) on top of the fractal quad.
* It must utilize a monospaced font for readability of changing numbers.