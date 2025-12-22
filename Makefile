# Container-Forge Makefile
# Handles build environment lifecycle, artifact compilation, and git sync.

IMAGE_NAME = mandelbrot-builder
CONTAINER_TOOL ?= podman
PROJECT_ROOT = $(shell pwd)
SCRIPTS_DIR = scripts

# Detect Host UID/GID for proper permission mapping
HOST_UID = $(shell id -u)
HOST_GID = $(shell id -g)

.PHONY: all init build-image build shell clean push

all: build

# 1. Initialize Repository Structure
init:
	@echo "[Make] Initializing repository..."
	@bash init_repo.sh

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

# 4. Interactive Shell for Debugging
shell:
	$(CONTAINER_TOOL) run --rm -it \
		--userns=keep-id \
		-v "$(PROJECT_ROOT):/mandelbrot-explorer:rw" \
		$(IMAGE_NAME) \
		/bin/bash

# 5. Git Sync / Push to GitHub
push:
	@echo "[Make] Pushing to GitHub..."
	@bash $(SCRIPTS_DIR)/push_repo.sh

# 6. Cleanup
clean:
	@echo "[Make] Cleaning build artifacts..."
	rm -rf build/obj/* build/bin/*
	rm -rf alire/