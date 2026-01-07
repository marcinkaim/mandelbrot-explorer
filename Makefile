################################################################################
#  Mandelbrot Explorer
#  Copyright (C) 2026 Marcin Kaim
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.
################################################################################

# Handles build environment lifecycle, artifact compilation, and git sync.

IMAGE_NAME = mandelbrot-builder
CONTAINER_TOOL ?= podman
PROJECT_ROOT = $(shell pwd)
SCRIPTS_DIR = scripts

# Detect Host UID/GID for proper permission mapping
HOST_UID = $(shell id -u)
HOST_GID = $(shell id -g)

GPU_FLAGS = --device nvidia.com/gpu=all --security-opt=label=disable

.PHONY: all license-check build-image build shell clean push test

all: license-check build

# 1. License Check
license-check:
	@echo "--- [LICENSE] Ensuring GPLv3 headers compliance ---"
	@./scripts/ensure_license_headers.sh

# 2. Build the Docker/Podman Image
build-image:
	@echo "[Make] Building container image: $(IMAGE_NAME)..."
	$(CONTAINER_TOOL) build \
		--build-arg USER_ID=$(HOST_UID) \
		--build-arg GROUP_ID=$(HOST_GID) \
		-t $(IMAGE_NAME) \
		-f docker/dev-env/Containerfile .

# 3. Compile Project (Static Link)
build:
	@echo "[Make] Compiling source code (Static)..."
	$(CONTAINER_TOOL) run --rm \
		--userns=keep-id \
		-v "$(PROJECT_ROOT):/mandelbrot-explorer:rw" \
		-w /mandelbrot-explorer \
		$(IMAGE_NAME) \
		alr build

rebuild: clean build

test:
	@echo "[Make] Verifying Host NVIDIA Drivers via Container Runtime..."
	@# Próbujemy uruchomić pusty kontener z żądaniem dostępu do GPU.
	@# Jeśli host nie ma załadowanego nvidia-uvm, to polecenie zwróci błąd CDI/Hook.
	@$(CONTAINER_TOOL) run --rm $(GPU_FLAGS) $(IMAGE_NAME) true > /dev/null 2>&1 || \
	(echo -e "\033[0;31m[ERROR] Host GPU not accessible via Container Runtime.\033[0m"; \
	 echo "       Reason: The Host machine cannot provide '/dev/nvidia-uvm' to the container."; \
	 echo "       Action: Run 'sudo nvidia-modprobe -u -c=0' on the HOST machine (not in VS Code)."; \
	 exit 1)
	
	@echo "[Make] GPU check passed. Running Unit Tests (AUnit + CUDA)..."
	$(CONTAINER_TOOL) run --rm \
		--userns=keep-id \
		$(GPU_FLAGS) \
		-v "$(PROJECT_ROOT):/mandelbrot-explorer:rw" \
		-w /mandelbrot-explorer \
		$(IMAGE_NAME) \
		alr exec -- ./build/bin/unit_tests

# 4. Interactive Shell for Debugging
shell:
	$(CONTAINER_TOOL) run --rm -it \
		--userns=keep-id \
		-v "$(PROJECT_ROOT):/mandelbrot-explorer:rw" \
		$(IMAGE_NAME) \
		/bin/bash

# 5. Git Sync / Push to GitHub
push: license-check
	@echo "[Make] Pushing to GitHub..."
	@bash $(SCRIPTS_DIR)/push_repo.sh

# 6. Cleanup
clean:
	@echo "[Make] Cleaning build artifacts..."
	rm -rf build/obj/* build/bin/*
	rm -rf alire/