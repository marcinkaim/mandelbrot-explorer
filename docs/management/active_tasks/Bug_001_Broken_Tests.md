# BUG REPORT: 001 - Fix Unit Test Execution (NVIDIA UVM Host State)

## 1. Context

Running unit tests via `make test` fails during container initialization on local environments (Host: Debian/Linux).
While the build process (`make build`) succeeds and produces the `unit_tests` binary, the Container Runtime fails to provision the GPU devices.

**Error Log:**

```text
Error: preparing container ... failed to stat CDI host device "/dev/nvidia-uvm": no such file or directory

```

## 2. Diagnosis (Root Cause Analysis)

* **Root Cause:** The NVIDIA driver on the Host uses a "Lazy Loading" mechanism for the `nvidia-uvm` (Unified Virtual Memory) kernel module.
* **Trigger:** After a System Suspend/Resume cycle, the kernel resets the module state. The UVM module is not automatically reloaded until explicitly requested by a host process.
* **Impact:** The Container Runtime uses CDI (Container Device Interface) to inject device nodes. Since `/dev/nvidia-uvm` does not physically exist on the Host at the moment of container startup, the runtime throws a "Fast Fail" error.

## 3. Objective

Harden the CI/CD pipeline (`Makefile`) to gracefully handle the unstable state of the Host GPU driver and provide clear remediation instructions to the developer.

## 4. Key Responsibilities

1. **Update Makefile (Resilience & Diagnostics):**
* Implement a "Pre-flight check" in the `test` target.
* Instead of checking for file existence (which fails in Docker-in-Docker/Debox environments), execute a dry-run container command (e.g., `podman run ... true`) to verify if the Runtime can successfully provision the GPU.
* If the check fails, the Makefile must print a clear error message instructing the user to fix the Host state (e.g., "Run nvidia-modprobe...").


2. **Host Configuration (Documentation):**
* Define the procedure for persistent Host configuration (creating a `systemd` drop-in override for `nvidia-resume.service`).
* *Note:* The actual maintenance script is managed externally on the Host and **must not** be committed to the project repository.



## 5. Deliverables

* Updated `Makefile` containing the GPU pre-flight check logic.

## 6. Definition of Done

* Running `make test` when the Host driver is in a "dormant" state (post-suspend) provides a readable error message explaining the cause.
* Running `make test` works correctly once the Host is properly configured.