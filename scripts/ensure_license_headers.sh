#!/bin/bash

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

# ------------------------------------------------------------------------------
# Automates the injection of GPLv3 license headers into source files.
#
# CONFIGURATION SOURCE (Priority Order):
# 1. 'alire.toml' Magic Comment:   # Title: My Custom Name
# 2. 'alire.toml' field:           description = "..." (Optional switch)
# 3. 'alire.toml' field:           name = "..." (Auto-formatted to Title Case)
#
# BEHAVIOR:
# - Idempotent: Checks if the header exists before adding it.
# - Safe: Preserves Shebangs (#!/bin/bash) in scripts.
# - Self-Preservation: Excludes itself from modification to avoid runtime errors.
# - Verbose: Reports actions to stdout for audit trails.
# - Smart Parsing: Reads metadata safely from TOML without external deps.
# - Dynamic Year: Uses $(date +%Y) for NEW headers only.
# - Loose Check: Ignores year when checking if header exists.
# ------------------------------------------------------------------------------

set -e  # Exit immediately if a command exits with a non-zero status.

# --- CONFIGURATION & SSOT ---

# Default values (Fallback)
YEAR=$(date +%Y) # Dynamic current year for NEW headers

# ANSI Colors for verbose output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Try to extract from alire.toml if it exists
ALIRE_MANIFEST="alire.toml"

echo -e "${BLUE}[INFO] Starting GPLv3 License Header Compliance Scan...${NC}"

# --- INTELLIGENT CONFIG LOAD ---

if [ -f "$ALIRE_MANIFEST" ]; then
    echo -e "       Reading configuration from ${ALIRE_MANIFEST}..."

    # 1. Extract Authors (authors = ["Name"])
    EXTRACTED_AUTHORS=$(grep '^authors =' "$ALIRE_MANIFEST" | sed -E 's/authors = \["(.*)"\]/\1/')
    if [ -n "$EXTRACTED_AUTHORS" ]; then
        OWNER="$EXTRACTED_AUTHORS"
    fi

    # 2. Extract Project Name (Priority Queue)
    
    # Priority A: Look for Magic Comment "# Title: Custom Name"
    MAGIC_TITLE=$(grep '^# Title:' "$ALIRE_MANIFEST" | sed -E 's/^# Title:[[:space:]]*(.*)/\1/')
    
    # Priority B: Look for Description (Warning: usually too long for a header title)
    # Uncomment the line below if you REALLY prefer description over name
    # RAW_DESC=$(grep '^description =' "$ALIRE_MANIFEST" | sed -E 's/description = "(.*)"/\1/')

    # Priority C: Look for Name (Technical ID)
    RAW_NAME=$(grep '^name =' "$ALIRE_MANIFEST" | sed -E 's/name = "(.*)"/\1/')

    if [ -n "$MAGIC_TITLE" ]; then
        # Use the custom title defined in comment
        PROJECT_NAME_DISPLAY="$MAGIC_TITLE"
        echo -e "       -> Mode: Magic Comment ('$PROJECT_NAME_DISPLAY')"
        
    elif [ -n "$RAW_DESC" ]; then
        # Use description (Not recommended, but supported)
        PROJECT_NAME_DISPLAY="$RAW_DESC"
        echo -e "       -> Mode: Description Field ('$PROJECT_NAME_DISPLAY')"
        
    elif [ -n "$RAW_NAME" ]; then
        # Fallback: Transform snake_case to Title Case
        CLEAN_NAME=${RAW_NAME//_/ }
        PROJECT_NAME_DISPLAY=$(echo "$CLEAN_NAME" | sed -e "s/\b\(.\)/\u\1/g")
        echo -e "       -> Mode: Name Transformation ('$RAW_NAME' -> '$PROJECT_NAME_DISPLAY')"
    else
        echo -e "${RED}[ERROR] Could not read Application Name from alire.toml.${NC}"
    fi
else
    PROJECT_NAME_DISPLAY="$PROJECT_NAME"
    echo -e "${RED}[ERROR] alire.toml not found.${NC}"
fi

# Helper to get absolute path (safe for both Linux and macOS/BSD usually)
SCRIPT_PATH=$(readlink -f "$0" 2>/dev/null || realpath "$0")

# Counters
FIXED_COUNT=0
SKIPPED_COUNT=0

echo -e "       Config: Owner='${OWNER}', Year='${YEAR}', Project='${PROJECT_NAME_DISPLAY}'"

# --- HEADER DEFINITIONS ---
# NOTE: We add '|| true' after read because read -d '' returns exit code 1 
# on EOF (End Of File) if the delimiter (NULL) is not found. 
# Without '|| true', 'set -e' would kill the script here.

# 1. ADA Style (-- )
read -r -d '' HEADER_ADA << EOM || true
--------------------------------------------------------------------------------
--  $PROJECT_NAME
--  Copyright (C) $YEAR $OWNER
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------
EOM

# 2. C/C++/CUDA/PTX Style (// )
read -r -d '' HEADER_CPP << EOM || true
//------------------------------------------------------------------------------
//  $PROJECT_NAME
//  Copyright (C) $YEAR $OWNER
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <https://www.gnu.org/licenses/>.
//------------------------------------------------------------------------------
EOM

# 3. Shell/Make/Docker Style (# )
read -r -d '' HEADER_SHELL << EOM || true
################################################################################
#  $PROJECT_NAME
#  Copyright (C) $YEAR $OWNER
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
EOM

# --- LOGIC ---

apply_header() {
    local file="$1"
    local style="$2"
    local header_content="$3"
    
    # SAFETY CHECK: Do not modify the running script itself
    local file_abs
    file_abs=$(readlink -f "$file" 2>/dev/null || realpath "$file")
    
    if [[ "$file_abs" == "$SCRIPT_PATH" ]]; then
        # We silently verify if self has header, if not, we warn but don't touch
        if ! grep -q "Copyright (C).*$OWNER" "$file"; then
            echo -e "${YELLOW}[WARN]${NC}   $file - Script is missing header! Please add manually."
        else
            SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
        fi
        return
    fi
    
    # Check if the file already contains the copyright notice
    if grep -q "Copyright (C).*$OWNER" "$file"; then
        SKIPPED_COUNT=$((SKIPPED_COUNT + 1))
        return
    fi

    echo -e "${YELLOW}[FIXing]${NC} $file - Injecting license header..."
    
    local temp_file
    temp_file=$(mktemp)

    if [[ "$style" == "SHELL" ]]; then
        # For Shell scripts, we MUST preserve the Shebang (#!/...) on the first line
        if head -n 1 "$file" | grep -q "^#\!/"; then
            # Copy Shebang
            head -n 1 "$file" > "$temp_file"
            echo "" >> "$temp_file"
            # Inject License
            echo "$header_content" >> "$temp_file"
            echo "" >> "$temp_file"
            # Append rest of file
            tail -n +2 "$file" >> "$temp_file"
        else
            echo "$header_content" > "$temp_file"
            echo "" >> "$temp_file"
            cat "$file" >> "$temp_file"
        fi
    else
        echo "$header_content" > "$temp_file"
        echo "" >> "$temp_file"
        cat "$file" >> "$temp_file"
    fi

    mv "$temp_file" "$file"
    chmod --reference="$file" "$file" 2>/dev/null || true

    FIXED_COUNT=$((FIXED_COUNT + 1))
}

# --- EXECUTION LOOP ---

TARGET_DIRS="src tests kernels docker scripts"
echo -e "${BLUE}[INFO] Scanning directories: $TARGET_DIRS ${NC}"

while read -r file; do
    case "$file" in
        *.ads|*.adb|*.gpr)
            apply_header "$file" "ADA" "$HEADER_ADA"
            ;;
        *.c|*.h|*.cpp|*.hpp|*.cu|*.ptx)
            apply_header "$file" "CPP" "$HEADER_CPP"
            ;;
        *.sh|*/Containerfile|*/Makefile|Makefile)
            apply_header "$file" "SHELL" "$HEADER_SHELL"
            ;;
        *)
            ;;
    esac
done < <(find $TARGET_DIRS -type f)

# Check root files
for root_file in Makefile mandelbrot_explorer.gpr; do
    if [[ -f "$root_file" ]]; then
        if [[ "$root_file" == "Makefile" ]]; then
            apply_header "$root_file" "SHELL" "$HEADER_SHELL"
        elif [[ "$root_file" == *.gpr ]]; then
            apply_header "$root_file" "ADA" "$HEADER_ADA"
        fi
    fi
done

# --- SUMMARY ---

echo -e "${BLUE}[INFO] Scan complete.${NC}"
if [ "$FIXED_COUNT" -gt 0 ]; then
    echo -e "       Stats: ${YELLOW}Fixed: $FIXED_COUNT${NC}, ${GREEN}Compliant: $SKIPPED_COUNT${NC}"
else
    echo -e "       Stats: ${GREEN}All $SKIPPED_COUNT files compliant.${NC}"
fi
