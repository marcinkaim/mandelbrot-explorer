# Mandelbrot Explorer

High-performance Mandelbrot set explorer built with **Ada 2022**, **CUDA**, and **SDL2**.

This project demonstrates the integration of high-integrity software engineering (Ada/SPARK) with high-performance computing (NVIDIA CUDA), managed via a hermetic containerized build system.

## 🚀 Technology Stack

* **Host Language:** Ada 2022 (GNAT FSF 14.x)
* **Device Kernel:** CUDA C++ / PTX
* **Graphics/Input:** SDL2 (via Thin/Thick Ada bindings)
* **Build System:** Alire (`alr`) inside Podman/Docker
* **Target OS:** Linux (Debian 13 Trixie target)

## 🛠️ Prerequisites

To build and run this project, you **do not** need to install Ada toolchains, GPRbuild, or the full CUDA toolkit on your host machine. The build environment is fully containerized.

**Host Requirements:**
* **Linux** (Kernel 5.x+)
* **NVIDIA GPU** (Pascal or newer recommended) with proprietary drivers installed.
* **NVIDIA Container Toolkit** (Critical):
  * Required to pass GPU access to containers.
  * Must be installed via your distribution's package manager (e.g., `apt` on Debian/Ubuntu) after adding the official NVIDIA repository.
  * *See official NVIDIA documentation for installation steps.*
* **Podman** (recommended) or Docker.
* **GNU Make**
* **Git**

## 🏗️ Build Instructions

### 1. Initialize Repository
Setup the required directory structure and configuration files.

```bash
make init

```

### 2. Create Build Environment

Build the builder container image (`mandelbrot-builder`). This image contains GNAT, GPRbuild, Alire, and CUDA integration headers.

```bash
make build-image

```

*Note: This step may take a few minutes as it downloads the base Debian image and installs dependencies.*

### 3. Compile the Application

Run the build process inside the container. This produces a statically linked binary.

```bash
make build

```

### 4. Verify (Unit Tests)

Execute the AUnit test suite inside the container with GPU passthrough enabled. This confirms that the Ada bindings can successfully initialize the CUDA Driver, load PTX kernels, and execute CUDA Graphs on your hardware.

```bash
make test

```

### 5. Run

The resulting artifact is located in `build/bin/`.

```bash
./build/bin/mandelbrot-explorer

```

## 📂 Repository Structure

```text
/mandelbrot-explorer
├── build/                  # Build artifacts (obj/ and bin/) - ignored by Git
├── debian/                 # Debian packaging metadata
├── docker/                 # Container definitions (Containerfile)
├── docs/                   # Documentation & Architecture Decision Records (ADR)
├── kernels/                # CUDA PTX/C++ kernels
├── resources/              # Shaders, assets, configuration
├── scripts/                # Helper scripts (git sync, CI/CD)
├── src/                    # Source Code
│   ├── app/                # Main application entry point
│   ├── binding/            # Bindings to SDL2 and CUDA
│   ├── compute/            # Computational logic
│   └── math/               # Math utilities
├── tests/                  # Test suites
├── Makefile                # Main entry point for build commands
├── alire.toml              # Alire dependency management
└── mandelbrot_explorer.gpr # GNAT Project File

```

## 🔄 Development Workflow

### Syncing with GitHub

This project uses a custom script to handle secure pushing to the remote repository.

1. Create a `.secrets` file in the root directory (this file is git-ignored):
```bash
export GITHUB_USER="your-username"
export GITHUB_REPO="mandelbrot-explorer"
export GITHUB_TOKEN="your-personal-access-token"

```


2. Push changes:
```bash
make push

```



## 📜 License

This project is licensed under the **MIT License**. See the `LICENSE` file for details.
