#!/bin/bash
set -euo pipefail

# --- Configuration ---
PROJECT_NAME="mandelbrot_explorer"
GPR_FILE="mandelbrot_explorer.gpr"
ALIRE_TOML="alire.toml"
AUTHOR="Marcin Kaim"
MAINTAINER="Marcin Kaim <9829098+marcinkaim@users.noreply.github.com>"
MAINTAINER_LOGIN="marcinkaim"

echo "[INFO] Initializing repository structure for ${PROJECT_NAME}..."

# 1. Create Directory Structure
DIRS=(
    ".vscode"
    "build/obj"
    "build/bin"
    "debian"
    "dist"
    "docker/dev-env"
    "docs/adr"
    "docs/manpages"
    "docs/requirements"
    "kernels"
    "resources"
    "scripts"
    "src/app"
    "src/binding"
    "src/compute"
    "src/math"
    "tests"
)

for dir in "${DIRS[@]}"; do
    if [ ! -d "$dir" ]; then
        mkdir -p "$dir"
        echo "  [+] Created: $dir"
    else
        echo "  [.] Exists: $dir"
    fi
done

# 2. Generate .gitignore
echo "[INFO] Generating .gitignore..."
cat > .gitignore <<EOF
# Build artifacts
build/
*.o
*.ali
*.a
*.so
*.exe
obj/
bin/

# Alire dependencies and cache
alire/
alire.toml.lock

# System and IDE
.DS_Store
.vscode/
*.swp

# Secrets (Critical)
.secrets
EOF

# 3. Generate initial mandelbrot.gpr (GNAT Project File)
# Required for 'alr build' to work correctly.
if [ ! -f "$GPR_FILE" ]; then
    echo "[INFO] Generating skeleton ${GPR_FILE}..."
    cat > "$GPR_FILE" <<EOF
project Mandelbrot is
   for Source_Dirs use ("src/**");
   for Object_Dir use "build/obj";
   for Exec_Dir use "build/bin";
   for Main use ("main.adb");

   package Compiler is
      for Default_Switches ("Ada") use ("-g", "-O2", "-gnat2022");
   end Compiler;
end Mandelbrot;
EOF
    # We also need a dummy main.adb to allow compilation
    if [ ! -f "src/app/main.adb" ]; then
        echo "  [+] Creating dummy main.adb for build verification..."
        cat > "src/app/main.adb" <<EOF
with Ada.Text_IO;
procedure Main is
begin
   Ada.Text_IO.Put_Line ("Mandelbrot Explorer Initialized.");
end Main;
EOF
    fi
else
    echo "  [.] Exists: ${GPR_FILE}"
fi

# 4. Generate alire.toml
if [ ! -f "$ALIRE_TOML" ]; then
    echo "[INFO] Generating ${ALIRE_TOML}..."
    cat > "$ALIRE_TOML" <<EOF
name = "${PROJECT_NAME}"
description = "Mandelbrot set explorer using Ada 2022, SDL2 and CUDA"
version = "0.1.0-dev"

authors = ["${AUTHOR}"]
maintainers = ["${MAINTAINER}"]
maintainers-logins = ["${MAINTAINER_LOGIN}"]

[[depends-on]]
gnat = ">=12"
gprbuild = ">=22"
EOF
else
    echo "  [.] Exists: ${ALIRE_TOML}"
fi

# 5. Check for .secrets existence
if [ -f ".secrets" ]; then
    echo "[INFO] Found .secrets file. Ensure it is populated correctly."
else
    echo "[WARN] .secrets file missing. Please create it manually if needed."
fi

echo "[SUCCESS] Repository initialization complete."