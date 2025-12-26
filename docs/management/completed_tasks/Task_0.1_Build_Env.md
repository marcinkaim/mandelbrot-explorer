**Task 0.1: Initialize Build Environment & Repository Structure**
**Objective:** Create the Docker-based build environment and the initial file structure tailored for Alire and Ada 2022.
**Inputs:**
* Folder Structure Specification as follows:
```text
 /mandelbrot-explorer
├── .vscode/                # VS Code configuration (tasks.json, launch.json)
├── build/                  # Build artifacts (mapped to RAM-disk in Podman)
│   ├── obj/                # Files .o and .ali (Ada Library Information) - important for GNAT
│   └── bin/                # Final executable.
├── debian/                 # Metadata for building package .deb (control, rules, compat)
├── dist/                   # Resulting packages .deb
├── docker/
├── docs/
│   ├── adr/                # Architecture Decision Records - detailed explanations for Double i SDL2
│   ├── manpages/           # Manual pages in .MD format to be transformed using pandoc
│   └── requirements/
├── kernels/                # PTX Code
├── resources/              # OpenGL shaders, icons, config files
├── scripts/
├── src/                    # Ada source code
│   ├── app/
│   ├── binding/            # "Interfacing" to SDL2 and CUDA (Thin/Thick specifications)
│   ├── compute/
│   └── math/
├── tests/
├── .secrets                # GitHub access configuration, used in scripts
├── .gitignore
├── README.md
├── Makefile                # Provides `make build`
├── mandelbrot.gpr          # Main project file
└── alire.toml              # Dependencies management
```
* Requirement: Use Alire (`alr`) for dependency management.
* Target OS: Debian 13 Trixie.


**Deliverables:**
1. `docker/dev-env/Containerfile`:
* Base: `debian:trixie-slim`.
* Install: `gnat`, `gprbuild`, `cuda-toolkit` (minimal), `git`, `gpg`, `libsdl2-dev`, `curl`/`wget` (for Alire installation).
* **Action:** Download and install the latest Alire binary.
* Config: Create a non-root user `builder`.

2. `init_repo.sh` (Bash script):
* Creates the exact folder tree: `.vscode`, `build/obj`, `build/bin`, `debian`, `dist`, `docs/adr`, `docs/manpages`, `kernels`, `resources`, `src/binding`, `src/compute`, `src/math`, `.secrets`.
* Generates a robust `.gitignore` (must ignore `.secrets`, `build/`, `alire/`, `*.ali`, `*.o`, `.DS_Store`).

3. `alire.toml`: Initialize an empty Alire crate named `mandelbrot_explorer`.

4. `Makefile`: provides `build` target which invokes podman image build.

**Definition of Done:**
* Running `make build` invokes `podman build -t mandelbrot-builder .` or similar command for building and succeeds.
* Running the init script creates the structure without errors.
* `alr build` (inside container) executes (even if there is no code yet, the project file exists).

**Technical Note for Container-Forge:**
Ensure that the build process respects the Host's UID/GID. The artifact generated in `build/bin` must be executable by the user on the Host machine, not just root inside the container. Consider using Podman's `--userns=keep-id` or passing the UID as a build argument to the `Makefile`.
