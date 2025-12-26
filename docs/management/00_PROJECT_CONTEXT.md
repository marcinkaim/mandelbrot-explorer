# PROJEKT: MANDELBROT EXPLORER - SPECYFIKACJA I KONTEKST

## 1. Wizja Projektu
Wysokowydajna, interaktywna eksploracja zbioru Mandelbrota wykorzystująca hybrydową moc obliczeniową (CPU + NVIDIA dGPU). Projekt łączy bezpieczeństwo i inżynierię oprogramowania (Ada 2022) z surową wydajnością sprzętową (CUDA PTX oraz CPU SIMD).
Celem jest stworzenie referencyjnej implementacji "Deep Zoom" działającej płynnie (60 FPS UI) na architekturze laptopowej (Hybrid Graphics).

## 2. Stos Technologiczny (Hard Constraints)
* **OS:** Linux Debian 13 Trixie (Host & Container).
* **Build System:** Podman (Containerized) + Alire (`alr`) + Make.
    * *Zasada:* "Hermetic Build" – budowanie musi być powtarzalne bez względu na konfigurację hosta.
* **Język (Host):** Ada 2022 (GNAT FSF) – Zarządzanie stanem, UI, OpenGL, Orkiestracja.
* **Język (Device GPU):** CUDA PTX (Assembly) – Kernels.
* **Język (Device CPU):** Ada/C z wykorzystaniem instrukcji wektorowych (SSE/AVX).
* **API Bridge:** Custom Thin Binding do `libcuda.so` (Driver API).
* **Grafika:** SDL2 (Windowing/Input) + OpenGL 4.1 Core Profile.

## 3. Wymagania Funkcjonalne (Szczegółowe)

### 3.1. Precyzja Obliczeń (Precision Tiers)
System musi obsługiwać dynamiczne przełączanie silników obliczeniowych w zależności od poziomu powiększenia (`Zoom`), aby zachować precyzję wizualną:
1.  **Tier 1: Hardware Double (`FP64`)**
    * zakres: `1.0` do `1.0E+14` (ok. 2^48).
    * technologia: Natywne instrukcje GPU `fma.rn.f64` lub CPU `double`.
2.  **Tier 2: Quad-Double (Software Emulation)**
    * zakres: `1.0E+14` do `1.0E+28` (ok. 2^96).
    * technologia: Algorytm "Double-Double" (dwie liczby `double` reprezentujące jedną wartość).
3.  **Tier 3: Octa-Double / Arbitrary (Planned)**
    * zakres: powyżej `1.0E+28` (do 2^192).

### 3.2. Responsywność i UX (Decoupled Rendering)
Zgodnie z **ADR-005**, aplikacja musi realizować paradygmat "Async Tiled Rendering":
* **UI Thread (60 FPS):** Interfejs użytkownika (przesuwanie, zoomowanie) musi reagować natychmiastowo, niezależnie od obciążenia backendu obliczeniowego.
* **Progressive Refinement:** Podczas szybkiej nawigacji użytkownik widzi przybliżony obraz (lub starsze kafelki), które są podmieniane na wersje wysokiej jakości po zakończeniu obliczeń.
* **Zero-Freeze:** Obliczenia fraktala (które mogą trwać >100ms) nigdy nie mogą blokować pętli zdarzeń SDL.

### 3.3. Tryby Obliczeniowe (Compute Switching)
Aplikacja musi umożliwiać przełączanie backendu obliczeniowego w czasie rzeczywistym (Runtime Switching):
1.  **Tryb GPU (Primary):** Wykorzystanie NVIDIA CUDA i PBO.
2.  **Tryb CPU (Fallback/Alternative):**
    * Wykorzystywany w przypadku braku dostępności GPU lub na żądanie użytkownika.
    * **Wymóg Wydajności:** Implementacja musi wykorzystywać pełny potencjał procesora poprzez użycie instrukcji wektorowych **SSE** (SIMD) oraz wielowątkowości (Ada Tasking), unikając powolnych pętli skalarnych.

### 3.4. Kolorowanie i Wizualizacja
System musi implementować algorytmy mapowania wartości ucieczki (escape iterations) na kolory RGBA:
1.  **Continuous Potential (Smooth Coloring):** Eliminacja pasmowania (banding) przy użyciu logarytmicznego wygładzania iteracji: $\nu = n + 1 - \log_2(\log_2(|Z_n|))$.
2.  **Palety Cykliczne:** Możliwość zmiany offsetu palety w czasie rzeczywistym (Color Cycling).
3.  **Tryby:** Skala szarości (debug), Gradient dwukolorowy, Pełne spektrum (HSV).

### 3.5. Sterowanie (Input Mapping)
Standardowe mapowanie klawiszy dla nawigacji:
* `W` / `S`: Zoom In / Zoom Out (o stały współczynnik, np. 1.1x).
* `A` / `D` / `Q` / `E`: Przesuwanie kamery (Pan) w osiach X/Y.
* `ESC`: Natychmiastowe, czyste zamknięcie aplikacji i zwolnienie zasobów GPU.
* `SPACE` (Planowane): Przełączenie trybu CPU <-> GPU.

## 4. Wymagania Architektoniczne i Sprzętowe

### 4.1. Architektura Hybrydowa (NVIDIA Prime)
Ze względu na specyfikę laptopów inżynierskich (iGPU Intel + dGPU NVIDIA):
* Aplikacja musi uruchamiać się na dGPU (`__NV_PRIME_RENDER_OFFLOAD=1`).
* **PBO Interop:** Komunikacja między CUDA a OpenGL odbywa się przez Pixel Buffer Objects. Pamięć ta musi rezydować w VRAM dGPU. Kopiowanie danych do RAM hosta jest zabronione (chyba że dla celów debugowania lub w trybie CPU).

### 4.2. Współbieżność (Ada Tasking)
System wykorzystuje model **Producent-Konsument**:
* **Orchestrator (Main Task):** Zarządza stanem okna, inputem i wyświetlaniem tekstur.
* **Compute Worker (Task):** Odbiera zlecenia (`Tile_Description`) z kolejki `Job_Queue`. Abstrahuje, czy obliczenia wykonuje GPU czy CPU.
* **Synchronization:** Wykorzystanie obiektów chronionych (Ada Protected Objects) do bezpiecznej wymiany danych.

## 5. Strategia Rozwoju i Testowania
1.  **Unit Tests (AUnit):** Testowanie logiki hosta (kolejki, kontenery, konfiguracja).
2.  **Integration Tests:** Weryfikacja inicjalizacji CUDA i alokacji pamięci na GPU (wymaga dostępu do `/dev/nvidia*` w kontenerze).
3.  **Visual Inspection:** Weryfikacja poprawności matematycznej kerneli odbywa się poprzez renderowanie obrazów referencyjnych.