#!/bin/bash

# SuiteCRM COBOL Bridge - COBOL Compilation Pipeline
# This script compiles all COBOL programs with proper options and validation

set -e  # Exit on error

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
COBOL_SOURCE_DIR="${PROJECT_ROOT}/cobol-core"
COBOL_BIN_DIR="${PROJECT_ROOT}/compiled"
LOG_DIR="${PROJECT_ROOT}/logs"
BUILD_LOG="${LOG_DIR}/cobol-build-$(date +%Y%m%d-%H%M%S).log"

# Compilation options
COBC_FLAGS="-x -O2 -Wall -fmax-errors=10"
DEBUG_FLAGS="-g -ftraceall -fsource-location"
STATIC_FLAGS="-static"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Initialize
echo "========================================="
echo "SuiteCRM COBOL Bridge - Compilation Pipeline"
echo "========================================="
echo ""

# Create directories
mkdir -p "$COBOL_BIN_DIR"
mkdir -p "$LOG_DIR"

# Start logging
exec 1> >(tee -a "$BUILD_LOG")
exec 2>&1

echo "Build started at: $(date)"
echo "Source directory: $COBOL_SOURCE_DIR"
echo "Output directory: $COBOL_BIN_DIR"
echo ""

# Check for GnuCOBOL
if ! command -v cobc &> /dev/null; then
    echo -e "${RED}ERROR: GnuCOBOL compiler (cobc) not found!${NC}"
    echo "Please install GnuCOBOL:"
    echo "  Ubuntu/Debian: sudo apt-get install gnucobol"
    echo "  MacOS: brew install gnucobol"
    echo "  RHEL/CentOS: sudo yum install gnucobol"
    exit 1
fi

# Display compiler version
echo "Compiler version:"
cobc --version | head -n 1
echo ""

# Function to compile a single COBOL program
compile_program() {
    local source_file="$1"
    local filename=$(basename "$source_file")
    local program_name="${filename%.cob}"
    local output_file="${COBOL_BIN_DIR}/${program_name}"
    
    echo -n "Compiling ${filename}... "
    
    # Prepare compilation command
    local compile_cmd="cobc $COBC_FLAGS"
    
    # Add debug flags if requested
    if [[ "${DEBUG_BUILD:-false}" == "true" ]]; then
        compile_cmd="$compile_cmd $DEBUG_FLAGS"
    fi
    
    # Add static linking if requested
    if [[ "${STATIC_BUILD:-false}" == "true" ]]; then
        compile_cmd="$compile_cmd $STATIC_FLAGS"
    fi
    
    # Add monitoring and debugging support
    compile_cmd="$compile_cmd -I${COBOL_SOURCE_DIR}/copybooks"
    compile_cmd="$compile_cmd -L${COBOL_SOURCE_DIR}/libs"
    
    # Output file
    compile_cmd="$compile_cmd -o $output_file"
    
    # Source file
    compile_cmd="$compile_cmd $source_file"
    
    # Execute compilation
    if $compile_cmd 2>&1 | tee -a "$BUILD_LOG" | grep -E "(error|ERROR)" > /dev/null; then
        echo -e "${RED}FAILED${NC}"
        echo "See $BUILD_LOG for details"
        return 1
    else
        echo -e "${GREEN}OK${NC}"
        
        # Make executable
        chmod +x "$output_file"
        
        # Display program info
        local size=$(ls -lh "$output_file" | awk '{print $5}')
        echo "  Output: $output_file (size: $size)"
        
        # Validate the compiled program
        if [[ -x "$output_file" ]]; then
            echo "  Status: Executable created successfully"
        else
            echo -e "  Status: ${YELLOW}Warning - not executable${NC}"
        fi
        
        return 0
    fi
}

# Function to create monitoring wrapper
create_monitoring_wrapper() {
    local program_name="$1"
    local wrapper_file="${COBOL_BIN_DIR}/${program_name}-monitored"
    
    cat > "$wrapper_file" << 'EOF'
#!/bin/bash
# Monitoring wrapper for COBOL program

PROGRAM_NAME="$(basename "$0" -monitored)"
PROGRAM_PATH="$(dirname "$0")/${PROGRAM_NAME}"
EXECUTION_ID="$(uuidv4 2>/dev/null || echo "$$-$(date +%s)")"
MONITOR_FILE="${MONITOR_FILE:-/tmp/cobol-monitor-${EXECUTION_ID}.log}"
DEBUG_TRACE="${DEBUG_TRACE:-/tmp/cobol-debug-${EXECUTION_ID}.log}"

# Start monitoring
echo "{\"event\":\"START\",\"execution_id\":\"${EXECUTION_ID}\",\"timestamp\":$(date +%s),\"program\":\"${PROGRAM_NAME}\"}" >> "$MONITOR_FILE"

# Execute the actual program with monitoring
export MONITOR_FILE
export DEBUG_TRACE
export EXECUTION_ID
export PROGRAM_NAME

# Run with time measurement
START_TIME=$(date +%s%N)
"$PROGRAM_PATH" "$@"
EXIT_CODE=$?
END_TIME=$(date +%s%N)

# Calculate execution time in milliseconds
EXEC_TIME=$(( (END_TIME - START_TIME) / 1000000 ))

# End monitoring
echo "{\"event\":\"END\",\"execution_id\":\"${EXECUTION_ID}\",\"timestamp\":$(date +%s),\"duration\":${EXEC_TIME},\"exit_code\":${EXIT_CODE}}" >> "$MONITOR_FILE"

exit $EXIT_CODE
EOF
    
    chmod +x "$wrapper_file"
    echo "  Created monitoring wrapper: $wrapper_file"
}

# Function to create program metadata
create_program_metadata() {
    local program_name="$1"
    local source_file="$2"
    local metadata_file="${COBOL_BIN_DIR}/${program_name}.meta.json"
    
    # Extract program information
    local lines=$(wc -l < "$source_file")
    local has_monitoring=$(grep -q "MONITOR-FILE" "$source_file" && echo "true" || echo "false")
    local has_debug=$(grep -q "DEBUG-TRACE" "$source_file" && echo "true" || echo "false")
    local cloud_burst=$(grep -q "CLOUD-BURST-ELIGIBLE" "$source_file" && echo "true" || echo "false")
    
    cat > "$metadata_file" << EOF
{
  "program": "${program_name}",
  "source": "$(basename "$source_file")",
  "compiled": "$(date -u +"%Y-%m-%dT%H:%M:%SZ")",
  "compiler": "$(cobc --version | head -n 1)",
  "lines": ${lines},
  "features": {
    "monitoring": ${has_monitoring},
    "debug_trace": ${has_debug},
    "cloud_burst_eligible": ${cloud_burst}
  },
  "build_flags": "${COBC_FLAGS}",
  "checksum": "$(sha256sum "$source_file" | cut -d' ' -f1)"
}
EOF
    
    echo "  Created metadata: $metadata_file"
}

# Main compilation loop
echo "Scanning for COBOL programs..."
TOTAL_PROGRAMS=0
COMPILED_PROGRAMS=0
FAILED_PROGRAMS=0

# Find all COBOL source files
for cobol_file in "$COBOL_SOURCE_DIR"/*.cob "$COBOL_SOURCE_DIR"/*.cbl "$COBOL_SOURCE_DIR"/*.CBL "$COBOL_SOURCE_DIR"/*.COB; do
    # Skip if no files found
    [[ ! -f "$cobol_file" ]] && continue
    
    TOTAL_PROGRAMS=$((TOTAL_PROGRAMS + 1))
    
    echo ""
    echo "Processing: $(basename "$cobol_file")"
    echo "----------------------------------------"
    
    if compile_program "$cobol_file"; then
        COMPILED_PROGRAMS=$((COMPILED_PROGRAMS + 1))
        
        # Extract program name
        program_name="$(basename "$cobol_file" .cob)"
        program_name="$(basename "$program_name" .cbl)"
        program_name="$(basename "$program_name" .CBL)"
        program_name="$(basename "$program_name" .COB)"
        
        # Create monitoring wrapper
        create_monitoring_wrapper "$program_name"
        
        # Create metadata
        create_program_metadata "$program_name" "$cobol_file"
    else
        FAILED_PROGRAMS=$((FAILED_PROGRAMS + 1))
    fi
done

# Create index file
echo ""
echo "Creating program index..."
INDEX_FILE="${COBOL_BIN_DIR}/programs.json"

echo "{" > "$INDEX_FILE"
echo "  \"generated\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\"," >> "$INDEX_FILE"
echo "  \"programs\": [" >> "$INDEX_FILE"

first=true
for meta_file in "$COBOL_BIN_DIR"/*.meta.json; do
    [[ ! -f "$meta_file" ]] && continue
    
    if [[ "$first" != "true" ]]; then
        echo "," >> "$INDEX_FILE"
    fi
    first=false
    
    # Indent the metadata
    sed 's/^/    /' "$meta_file" | head -n -1 >> "$INDEX_FILE"
    echo "    }" >> "$INDEX_FILE"
done

echo "  ]" >> "$INDEX_FILE"
echo "}" >> "$INDEX_FILE"

echo "Created program index: $INDEX_FILE"

# Summary
echo ""
echo "========================================="
echo "Build Summary"
echo "========================================="
echo "Total programs found: $TOTAL_PROGRAMS"
echo -e "Successfully compiled: ${GREEN}$COMPILED_PROGRAMS${NC}"
echo -e "Failed to compile: ${RED}$FAILED_PROGRAMS${NC}"
echo ""
echo "Output directory: $COBOL_BIN_DIR"
echo "Build log: $BUILD_LOG"
echo ""

# Set build timestamp
date > "${COBOL_BIN_DIR}/.build-timestamp"

# Exit with error if any compilations failed
if [[ $FAILED_PROGRAMS -gt 0 ]]; then
    echo -e "${RED}Build completed with errors!${NC}"
    exit 1
else
    echo -e "${GREEN}Build completed successfully!${NC}"
    exit 0
fi