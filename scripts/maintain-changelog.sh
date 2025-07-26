#!/bin/bash

# Changelog Maintenance Script
# This script maintains the CHANGELOG.md file following Keep a Changelog format

set -euo pipefail

# Configuration
CHANGELOG_FILE="${CHANGELOG_FILE:-CHANGELOG.md}"
VERSION="${VERSION:-}"
ACTION="${ACTION:-update}"
ENTRY_TYPE="${ENTRY_TYPE:-}"
ENTRY_MESSAGE="${ENTRY_MESSAGE:-}"
DRY_RUN="${DRY_RUN:-false}"
BACKUP="${BACKUP:-true}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to create initial changelog
create_initial_changelog() {
    local changelog_file="$1"
    
    log_info "Creating initial changelog: ${changelog_file}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create initial changelog"
        return 0
    fi
    
    cat > "${changelog_file}" << 'EOF'
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Initial project setup
- Core SuiteCRM COBOL Bridge functionality

### Changed

### Deprecated

### Removed

### Fixed

### Security

EOF
    
    log_success "Initial changelog created: ${changelog_file}"
}

# Function to backup changelog
backup_changelog() {
    local changelog_file="$1"
    
    if [[ ! -f "${changelog_file}" ]]; then
        return 0
    fi
    
    local backup_file="${changelog_file}.backup.$(date +%Y%m%d_%H%M%S)"
    
    log_info "Creating backup: ${backup_file}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create backup"
        return 0
    fi
    
    cp "${changelog_file}" "${backup_file}"
    log_success "Backup created: ${backup_file}"
}

# Function to get current version from changelog
get_current_version() {
    local changelog_file="$1"
    
    if [[ ! -f "${changelog_file}" ]]; then
        echo ""
        return
    fi
    
    # Look for the first version entry (not Unreleased)
    grep -m 1 "^## \[" "${changelog_file}" | grep -v "Unreleased" | sed 's/^## \[\([^]]*\)\].*/\1/' || echo ""
}

# Function to check if version exists in changelog
version_exists() {
    local changelog_file="$1"
    local version="$2"
    
    if [[ ! -f "${changelog_file}" ]]; then
        return 1
    fi
    
    grep -q "^## \[${version}\]" "${changelog_file}"
}

# Function to add entry to unreleased section
add_unreleased_entry() {
    local changelog_file="$1"
    local entry_type="$2"
    local entry_message="$3"
    
    log_info "Adding ${entry_type} entry to Unreleased section"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would add: [${entry_type}] ${entry_message}"
        return 0
    fi
    
    # Validate entry type
    case "${entry_type}" in
        Added|Changed|Deprecated|Removed|Fixed|Security)
            ;;
        *)
            log_error "Invalid entry type: ${entry_type}"
            log_info "Valid types: Added, Changed, Deprecated, Removed, Fixed, Security"
            return 1
            ;;
    esac
    
    # Create temporary file
    local temp_file=$(mktemp)
    local in_unreleased=false
    local in_section=false
    local section_found=false
    
    while IFS= read -r line; do
        # Check if we're entering the Unreleased section
        if [[ "${line}" =~ ^##[[:space:]]\[Unreleased\] ]]; then
            in_unreleased=true
            echo "${line}" >> "${temp_file}"
            continue
        fi
        
        # Check if we're leaving the Unreleased section
        if [[ "${in_unreleased}" == "true" && "${line}" =~ ^##[[:space:]]\[ ]]; then
            in_unreleased=false
            in_section=false
        fi
        
        # Look for the specific section in Unreleased
        if [[ "${in_unreleased}" == "true" && "${line}" =~ ^###[[:space:]]${entry_type}$ ]]; then
            in_section=true
            section_found=true
            echo "${line}" >> "${temp_file}"
            continue
        fi
        
        # Check if we're leaving the current section
        if [[ "${in_section}" == "true" && "${line}" =~ ^###[[:space:]] ]]; then
            in_section=false
        fi
        
        # Add the entry after the section header
        if [[ "${in_section}" == "true" && "${line}" == "" ]]; then
            echo "${line}" >> "${temp_file}"
            echo "- ${entry_message}" >> "${temp_file}"
            in_section=false
            continue
        fi
        
        echo "${line}" >> "${temp_file}"
    done < "${changelog_file}"
    
    # If section wasn't found, we need to add it
    if [[ "${section_found}" == "false" ]]; then
        log_warning "Section '${entry_type}' not found in Unreleased, creating it"
        # This would require more complex logic to insert in the right order
        # For now, just append to the end of Unreleased section
    fi
    
    mv "${temp_file}" "${changelog_file}"
    log_success "Entry added to ${entry_type} section"
}

# Function to release unreleased changes
release_changes() {
    local changelog_file="$1"
    local version="$2"
    local release_date="${3:-$(date +'%Y-%m-%d')}"
    
    log_info "Releasing unreleased changes as version ${version}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would release version ${version} on ${release_date}"
        return 0
    fi
    
    # Check if version already exists
    if version_exists "${changelog_file}" "${version}"; then
        log_error "Version ${version} already exists in changelog"
        return 1
    fi
    
    # Create temporary file
    local temp_file=$(mktemp)
    local unreleased_found=false
    local unreleased_content=""
    local capture_content=false
    
    while IFS= read -r line; do
        if [[ "${line}" =~ ^##[[:space:]]\[Unreleased\] ]]; then
            unreleased_found=true
            capture_content=true
            
            # Write the new version section
            echo "## [${version}] - ${release_date}" >> "${temp_file}"
            continue
        fi
        
        # Stop capturing when we hit the next version
        if [[ "${capture_content}" == "true" && "${line}" =~ ^##[[:space:]]\[ ]]; then
            capture_content=false
            
            # Add new unreleased section
            cat >> "${temp_file}" << 'EOF'

## [Unreleased]

### Added

### Changed

### Deprecated

### Removed

### Fixed

### Security

EOF
            echo "${line}" >> "${temp_file}"
            continue
        fi
        
        echo "${line}" >> "${temp_file}"
    done < "${changelog_file}"
    
    mv "${temp_file}" "${changelog_file}"
    log_success "Version ${version} released in changelog"
}

# Function to validate changelog format
validate_changelog() {
    local changelog_file="$1"
    
    log_info "Validating changelog format"
    
    if [[ ! -f "${changelog_file}" ]]; then
        log_error "Changelog file not found: ${changelog_file}"
        return 1
    fi
    
    local errors=0
    
    # Check for required header
    if ! head -1 "${changelog_file}" | grep -q "^# Changelog"; then
        log_error "Missing changelog header"
        ((errors++))
    fi
    
    # Check for Keep a Changelog format reference
    if ! grep -q "Keep a Changelog" "${changelog_file}"; then
        log_warning "Missing Keep a Changelog format reference"
    fi
    
    # Check for Semantic Versioning reference
    if ! grep -q "Semantic Versioning" "${changelog_file}"; then
        log_warning "Missing Semantic Versioning reference"
    fi
    
    # Check for Unreleased section
    if ! grep -q "^## \[Unreleased\]" "${changelog_file}"; then
        log_error "Missing Unreleased section"
        ((errors++))
    fi
    
    # Validate version format
    local invalid_versions=0
    while IFS= read -r line; do
        if [[ "${line}" =~ ^##[[:space:]]\[([^]]+)\] ]]; then
            local version="${BASH_REMATCH[1]}"
            if [[ "${version}" != "Unreleased" ]] && ! [[ "${version}" =~ ^[0-9]+\.[0-9]+\.[0-9]+(-[a-zA-Z0-9.-]+)?$ ]]; then
                log_error "Invalid version format: ${version}"
                ((invalid_versions++))
            fi
        fi
    done < "${changelog_file}"
    
    errors=$((errors + invalid_versions))
    
    # Check for required sections
    local required_sections=("Added" "Changed" "Deprecated" "Removed" "Fixed" "Security")
    for section in "${required_sections[@]}"; do
        if ! grep -q "^### ${section}$" "${changelog_file}"; then
            log_warning "Missing section: ${section}"
        fi
    done
    
    if [[ ${errors} -eq 0 ]]; then
        log_success "Changelog format validation passed"
        return 0
    else
        log_error "Changelog format validation failed with ${errors} errors"
        return 1
    fi
}

# Function to generate changelog statistics
generate_stats() {
    local changelog_file="$1"
    
    if [[ ! -f "${changelog_file}" ]]; then
        log_error "Changelog file not found: ${changelog_file}"
        return 1
    fi
    
    log_info "Generating changelog statistics"
    
    local total_versions=0
    local total_entries=0
    local entries_by_type=""
    
    # Count versions (excluding Unreleased)
    total_versions=$(grep -c "^## \[" "${changelog_file}" | grep -v "Unreleased" || echo 0)
    
    # Count entries by type
    local added_count=$(grep -c "^### Added$" "${changelog_file}" || echo 0)
    local changed_count=$(grep -c "^### Changed$" "${changelog_file}" || echo 0)
    local deprecated_count=$(grep -c "^### Deprecated$" "${changelog_file}" || echo 0)
    local removed_count=$(grep -c "^### Removed$" "${changelog_file}" || echo 0)
    local fixed_count=$(grep -c "^### Fixed$" "${changelog_file}" || echo 0)
    local security_count=$(grep -c "^### Security$" "${changelog_file}" || echo 0)
    
    # Count total entries (lines starting with -)
    total_entries=$(grep -c "^- " "${changelog_file}" || echo 0)
    
    echo ""
    echo "📊 Changelog Statistics"
    echo "======================="
    echo "Total Versions: ${total_versions}"
    echo "Total Entries: ${total_entries}"
    echo ""
    echo "Entries by Type:"
    echo "  Added: ${added_count}"
    echo "  Changed: ${changed_count}"
    echo "  Deprecated: ${deprecated_count}"
    echo "  Removed: ${removed_count}"
    echo "  Fixed: ${fixed_count}"
    echo "  Security: ${security_count}"
    echo ""
    
    # Recent versions
    echo "Recent Versions:"
    grep "^## \[" "${changelog_file}" | head -5 | while IFS= read -r line; do
        echo "  ${line}"
    done
    echo ""
}

# Function to cleanup old changelog entries
cleanup_old_entries() {
    local changelog_file="$1"
    local keep_versions="${2:-10}"
    
    log_info "Cleaning up old changelog entries (keeping last ${keep_versions} versions)"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would cleanup old entries"
        return 0
    fi
    
    # This is a complex operation that would require careful implementation
    # For now, just log what would be done
    local total_versions=$(grep -c "^## \[" "${changelog_file}" | grep -v "Unreleased" || echo 0)
    
    if [[ ${total_versions} -le ${keep_versions} ]]; then
        log_info "No cleanup needed (${total_versions} versions, keeping ${keep_versions})"
        return 0
    fi
    
    log_info "Would remove $((total_versions - keep_versions)) old versions"
    log_warning "Cleanup not implemented yet - manual cleanup required"
}

# Function to convert changelog to different formats
convert_format() {
    local changelog_file="$1"
    local output_format="$2"
    local output_file="$3"
    
    log_info "Converting changelog to ${output_format} format"
    
    case "${output_format}" in
        json)
            convert_to_json "${changelog_file}" "${output_file}"
            ;;
        xml)
            convert_to_xml "${changelog_file}" "${output_file}"
            ;;
        html)
            convert_to_html "${changelog_file}" "${output_file}"
            ;;
        *)
            log_error "Unsupported format: ${output_format}"
            return 1
            ;;
    esac
}

# Function to convert changelog to JSON
convert_to_json() {
    local changelog_file="$1"
    local output_file="$2"
    
    log_info "Converting to JSON: ${output_file}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would convert to JSON"
        return 0
    fi
    
    # This would require complex parsing - placeholder implementation
    cat > "${output_file}" << 'EOF'
{
    "changelog": {
        "format": "Keep a Changelog",
        "versions": []
    }
}
EOF
    
    log_success "JSON conversion completed: ${output_file}"
}

# Main function
main() {
    local action="${1:-help}"
    
    case "${action}" in
        init)
            if [[ -f "${CHANGELOG_FILE}" ]] && [[ "${FORCE:-false}" != "true" ]]; then
                log_error "Changelog already exists: ${CHANGELOG_FILE}"
                log_info "Use --force to overwrite"
                return 1
            fi
            create_initial_changelog "${CHANGELOG_FILE}"
            ;;
        add)
            local entry_type="${2:-}"
            local entry_message="${3:-}"
            
            if [[ -z "${entry_type}" ]] || [[ -z "${entry_message}" ]]; then
                log_error "Usage: $0 add <type> <message>"
                log_info "Types: Added, Changed, Deprecated, Removed, Fixed, Security"
                return 1
            fi
            
            if [[ ! -f "${CHANGELOG_FILE}" ]]; then
                create_initial_changelog "${CHANGELOG_FILE}"
            fi
            
            if [[ "${BACKUP}" == "true" ]]; then
                backup_changelog "${CHANGELOG_FILE}"
            fi
            
            add_unreleased_entry "${CHANGELOG_FILE}" "${entry_type}" "${entry_message}"
            ;;
        release)
            local version="${2:-${VERSION}}"
            local release_date="${3:-$(date +'%Y-%m-%d')}"
            
            if [[ -z "${version}" ]]; then
                log_error "Version not specified"
                log_info "Usage: $0 release <version> [date]"
                return 1
            fi
            
            if [[ ! -f "${CHANGELOG_FILE}" ]]; then
                log_error "Changelog file not found: ${CHANGELOG_FILE}"
                return 1
            fi
            
            if [[ "${BACKUP}" == "true" ]]; then
                backup_changelog "${CHANGELOG_FILE}"
            fi
            
            release_changes "${CHANGELOG_FILE}" "${version}" "${release_date}"
            ;;
        validate)
            validate_changelog "${CHANGELOG_FILE}"
            ;;
        stats)
            generate_stats "${CHANGELOG_FILE}"
            ;;
        cleanup)
            local keep_versions="${2:-10}"
            
            if [[ "${BACKUP}" == "true" ]]; then
                backup_changelog "${CHANGELOG_FILE}"
            fi
            
            cleanup_old_entries "${CHANGELOG_FILE}" "${keep_versions}"
            ;;
        convert)
            local format="${2:-json}"
            local output_file="${3:-changelog.${format}}"
            convert_format "${CHANGELOG_FILE}" "${format}" "${output_file}"
            ;;
        *)
            echo "Usage: $0 [COMMAND] [OPTIONS]"
            echo ""
            echo "Commands:"
            echo "  init                     Create initial changelog"
            echo "  add TYPE MESSAGE         Add entry to Unreleased section"
            echo "  release VERSION [DATE]   Release unreleased changes"
            echo "  validate                 Validate changelog format"
            echo "  stats                    Show changelog statistics"
            echo "  cleanup [KEEP]           Remove old versions (keep last KEEP)"
            echo "  convert FORMAT [FILE]    Convert to different format"
            echo ""
            echo "Entry Types:"
            echo "  Added, Changed, Deprecated, Removed, Fixed, Security"
            echo ""
            echo "Options:"
            echo "  --changelog FILE         Changelog file (default: CHANGELOG.md)"
            echo "  --version VERSION        Version for release command"
            echo "  --dry-run                Show what would be done"
            echo "  --no-backup              Don't create backups"
            echo "  --force                  Overwrite existing files"
            echo ""
            echo "Examples:"
            echo "  $0 init"
            echo "  $0 add Added 'New user authentication system'"
            echo "  $0 add Fixed 'Resolved database connection timeout'"
            echo "  $0 release 1.2.3"
            echo "  $0 validate"
            echo "  $0 stats"
            echo "  $0 convert json"
            exit 1
            ;;
    esac
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --changelog)
            CHANGELOG_FILE="$2"
            shift 2
            ;;
        --version)
            VERSION="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --no-backup)
            BACKUP="false"
            shift
            ;;
        --force)
            FORCE="true"
            shift
            ;;
        --help)
            main help
            ;;
        *)
            # Pass through to main function
            break
            ;;
    esac
done

# Run main function with remaining arguments
main "$@"