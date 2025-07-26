#!/bin/bash

# Release Notes Generation Script
# This script generates comprehensive release notes for the SuiteCRM COBOL Bridge

set -euo pipefail

# Configuration
VERSION="${1:-}"
SINCE_TAG="${2:-}"
OUTPUT_FILE="${3:-RELEASE_NOTES.md}"
FORMAT="${FORMAT:-markdown}"
INCLUDE_CONTRIBUTORS="${INCLUDE_CONTRIBUTORS:-true}"
INCLUDE_STATS="${INCLUDE_STATS:-true}"
DRY_RUN="${DRY_RUN:-false}"

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

# Function to check prerequisites
check_prerequisites() {
    if ! command -v git >/dev/null 2>&1; then
        log_error "Git is not installed"
        exit 1
    fi
    
    if ! git rev-parse --git-dir >/dev/null 2>&1; then
        log_error "Not in a Git repository"
        exit 1
    fi
    
    if [[ -z "${VERSION}" ]]; then
        log_error "Version not specified"
        echo "Usage: $0 <version> [since_tag] [output_file]"
        exit 1
    fi
}

# Function to get the last release tag
get_last_release_tag() {
    if [[ -n "${SINCE_TAG}" ]]; then
        echo "${SINCE_TAG}"
        return
    fi
    
    # Try to get the last tag
    local last_tag
    last_tag=$(git describe --tags --abbrev=0 2>/dev/null || echo "")
    
    if [[ -z "${last_tag}" ]]; then
        # If no tags exist, use first commit
        last_tag=$(git rev-list --max-parents=0 HEAD)
        log_info "No previous tags found, using first commit: ${last_tag}"
    else
        log_info "Using last release tag: ${last_tag}"
    fi
    
    echo "${last_tag}"
}

# Function to categorize commits
categorize_commits() {
    local since_ref="$1"
    local commits
    
    if [[ "${since_ref}" =~ ^[0-9a-f]{40}$ ]]; then
        # It's a commit hash, use it as is
        commits=$(git log "${since_ref}..HEAD" --pretty=format:"%H|%s|%an|%ae|%ad" --date=short --no-merges)
    else
        # It's a tag, use it as is
        commits=$(git log "${since_ref}..HEAD" --pretty=format:"%H|%s|%an|%ae|%ad" --date=short --no-merges)
    fi
    
    # Initialize arrays
    declare -a breaking_changes=()
    declare -a features=()
    declare -a fixes=()
    declare -a performance=()
    declare -a refactoring=()
    declare -a documentation=()
    declare -a testing=()
    declare -a ci_cd=()
    declare -a dependencies=()
    declare -a other=()
    declare -a contributors=()
    
    # Parse commits
    while IFS='|' read -r hash subject author email date; do
        [[ -z "${hash}" ]] && continue
        
        # Track contributors
        if [[ ! " ${contributors[*]} " =~ " ${author} " ]]; then
            contributors+=("${author}")
        fi
        
        local commit_info="${subject} ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))"
        
        # Categorize based on conventional commit format
        if [[ "${subject}" =~ ^[a-zA-Z]+(\(.+\))?!: ]] || echo "${subject}" | grep -q "BREAKING CHANGE"; then
            breaking_changes+=("${commit_info}")
        elif [[ "${subject}" =~ ^feat(\(.+\))?: ]]; then
            features+=("${subject#feat*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^fix(\(.+\))?: ]]; then
            fixes+=("${subject#fix*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^perf(\(.+\))?: ]]; then
            performance+=("${subject#perf*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^refactor(\(.+\))?: ]]; then
            refactoring+=("${subject#refactor*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^docs(\(.+\))?: ]]; then
            documentation+=("${subject#docs*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^test(\(.+\))?: ]]; then
            testing+=("${subject#test*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^(ci|cd|build)(\(.+\))?: ]]; then
            ci_cd+=("${subject#*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        elif [[ "${subject}" =~ ^(chore|deps)(\(.+\))?: ]] && echo "${subject}" | grep -q -E "(bump|update|upgrade|dependency|dependencies)"; then
            dependencies+=("${subject#*: } ([${hash:0:7}](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${hash}))")
        else
            other+=("${commit_info}")
        fi
    done <<< "${commits}"
    
    # Export arrays as global variables
    export BREAKING_CHANGES_ARRAY=("${breaking_changes[@]}")
    export FEATURES_ARRAY=("${features[@]}")
    export FIXES_ARRAY=("${fixes[@]}")
    export PERFORMANCE_ARRAY=("${performance[@]}")
    export REFACTORING_ARRAY=("${refactoring[@]}")
    export DOCUMENTATION_ARRAY=("${documentation[@]}")
    export TESTING_ARRAY=("${testing[@]}")
    export CI_CD_ARRAY=("${ci_cd[@]}")
    export DEPENDENCIES_ARRAY=("${dependencies[@]}")
    export OTHER_ARRAY=("${other[@]}")
    export CONTRIBUTORS_ARRAY=("${contributors[@]}")
}

# Function to get repository statistics
get_repository_stats() {
    local since_ref="$1"
    
    # Get commit count
    local commit_count
    if [[ "${since_ref}" =~ ^[0-9a-f]{40}$ ]]; then
        commit_count=$(git rev-list --count "${since_ref}..HEAD")
    else
        commit_count=$(git rev-list --count "${since_ref}..HEAD")
    fi
    
    # Get file changes
    local files_changed
    if [[ "${since_ref}" =~ ^[0-9a-f]{40}$ ]]; then
        files_changed=$(git diff --name-only "${since_ref}..HEAD" | wc -l)
    else
        files_changed=$(git diff --name-only "${since_ref}..HEAD" | wc -l)
    fi
    
    # Get line changes
    local lines_stats
    if [[ "${since_ref}" =~ ^[0-9a-f]{40}$ ]]; then
        lines_stats=$(git diff --shortstat "${since_ref}..HEAD")
    else
        lines_stats=$(git diff --shortstat "${since_ref}..HEAD")
    fi
    
    # Parse line changes
    local insertions=0
    local deletions=0
    if [[ "${lines_stats}" =~ ([0-9]+)\ insertion ]]; then
        insertions=${BASH_REMATCH[1]}
    fi
    if [[ "${lines_stats}" =~ ([0-9]+)\ deletion ]]; then
        deletions=${BASH_REMATCH[1]}
    fi
    
    echo "${commit_count}|${files_changed}|${insertions}|${deletions}"
}

# Function to generate release notes header
generate_header() {
    local version="$1"
    local release_date="${2:-$(date +'%Y-%m-%d')}"
    
    cat << EOF
# Release Notes - Version ${version}

**Release Date:** ${release_date}

**Version:** ${version}

**Build:** ${GITHUB_RUN_NUMBER:-N/A}

**Commit:** [\`${GITHUB_SHA:0:7}\`](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/commit/${GITHUB_SHA:-HEAD})

---

EOF
}

# Function to generate release notes section
generate_section() {
    local title="$1"
    local emoji="$2"
    shift 2
    local items=("$@")
    
    if [[ ${#items[@]} -eq 0 ]]; then
        return
    fi
    
    echo "## ${emoji} ${title}"
    echo ""
    
    for item in "${items[@]}"; do
        echo "- ${item}"
    done
    
    echo ""
}

# Function to generate contributor section
generate_contributors() {
    local contributors=("$@")
    
    if [[ ${#contributors[@]} -eq 0 ]]; then
        return
    fi
    
    echo "## 👥 Contributors"
    echo ""
    echo "This release was made possible by the following contributors:"
    echo ""
    
    for contributor in "${contributors[@]}"; do
        # Try to get GitHub username if possible
        local github_username
        github_username=$(git log --author="${contributor}" --pretty=format:"%an <%ae>" | head -1 | grep -o '[^@]*@users.noreply.github.com' | cut -d'@' -f1 || echo "")
        
        if [[ -n "${github_username}" ]]; then
            echo "- [@${github_username}](https://github.com/${github_username}) (${contributor})"
        else
            echo "- ${contributor}"
        fi
    done
    
    echo ""
}

# Function to generate statistics section
generate_statistics() {
    local stats="$1"
    
    IFS='|' read -r commit_count files_changed insertions deletions <<< "${stats}"
    
    cat << EOF
## 📊 Release Statistics

| Metric | Value |
|--------|-------|
| Commits | ${commit_count} |
| Files Changed | ${files_changed} |
| Lines Added | +${insertions} |
| Lines Removed | -${deletions} |
| Net Change | $((insertions - deletions)) |

EOF
}

# Function to generate installation instructions
generate_installation() {
    local version="$1"
    
    cat << EOF
## 📦 Installation

### Docker Images

Pull the latest Docker images for this release:

\`\`\`bash
# Main application
docker pull ghcr.io/${GITHUB_REPOSITORY,,}-suitecrm:${version}

# API Gateway
docker pull ghcr.io/${GITHUB_REPOSITORY,,}-api-gateway:${version}

# COBOL Compiler
docker pull ghcr.io/${GITHUB_REPOSITORY,,}-cobol-compiler:${version}
\`\`\`

### Source Code

Download the source code:

- [Source code (tar.gz)](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/archive/v${version}.tar.gz)
- [Source code (zip)](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/archive/v${version}.zip)

### Binary Release

Download the pre-built binary package:

- [Binary release (tar.gz)](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/releases/download/v${version}/suitecrm-cobol-bridge-${version}-binary.tar.gz)

### Kubernetes Deployment

Deploy using Kubernetes:

\`\`\`bash
kubectl apply -f https://github.com/${GITHUB_REPOSITORY:-owner/repo}/releases/download/v${version}/kubernetes-manifests.yaml
\`\`\`

EOF
}

# Function to generate upgrade instructions
generate_upgrade_instructions() {
    local version="$1"
    
    cat << EOF
## ⬆️ Upgrade Instructions

### From Previous Version

1. **Backup your data:**
   \`\`\`bash
   ./scripts/migrate-database.sh backup
   \`\`\`

2. **Pull new images:**
   \`\`\`bash
   docker-compose pull
   \`\`\`

3. **Run database migrations:**
   \`\`\`bash
   ./scripts/migrate-database.sh migrate --environment=production
   \`\`\`

4. **Deploy new version:**
   \`\`\`bash
   ./scripts/blue-green-deploy.sh --version=${version}
   \`\`\`

### Breaking Changes

Please review the breaking changes section above and update your configuration accordingly.

EOF
}

# Function to generate footer
generate_footer() {
    local version="$1"
    
    cat << EOF
---

## 🔗 Links

- [Full Changelog](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/compare/v${version}...HEAD)
- [Release Page](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/releases/tag/v${version})
- [Documentation](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/wiki)
- [Issues](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/issues)

## 💬 Feedback

If you encounter any issues with this release, please:

1. Check the [known issues](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/issues?q=is%3Aissue+is%3Aopen+label%3Abug)
2. [Create a new issue](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/issues/new) if your problem is not listed
3. Join our [discussion forum](https://github.com/${GITHUB_REPOSITORY:-owner/repo}/discussions) for general questions

---

*Generated on $(date -u +'%Y-%m-%d %H:%M:%S UTC') by [Release Notes Generator](scripts/generate-release-notes.sh)*
EOF
}

# Main function to generate release notes
generate_release_notes() {
    local version="$1"
    local output_file="$2"
    
    log_info "Generating release notes for version ${version}"
    
    # Get the reference point
    local since_ref
    since_ref=$(get_last_release_tag)
    
    log_info "Analyzing commits since ${since_ref}"
    
    # Categorize commits
    categorize_commits "${since_ref}"
    
    # Get repository statistics
    local stats
    if [[ "${INCLUDE_STATS}" == "true" ]]; then
        stats=$(get_repository_stats "${since_ref}")
    fi
    
    # Generate the release notes
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would generate release notes to ${output_file}"
        output_file="/dev/stdout"
    fi
    
    {
        # Header
        generate_header "${version}"
        
        # Breaking changes (most important)
        if [[ ${#BREAKING_CHANGES_ARRAY[@]} -gt 0 ]]; then
            generate_section "Breaking Changes" "💥" "${BREAKING_CHANGES_ARRAY[@]}"
        fi
        
        # New features
        if [[ ${#FEATURES_ARRAY[@]} -gt 0 ]]; then
            generate_section "New Features" "✨" "${FEATURES_ARRAY[@]}"
        fi
        
        # Bug fixes
        if [[ ${#FIXES_ARRAY[@]} -gt 0 ]]; then
            generate_section "Bug Fixes" "🐛" "${FIXES_ARRAY[@]}"
        fi
        
        # Performance improvements
        if [[ ${#PERFORMANCE_ARRAY[@]} -gt 0 ]]; then
            generate_section "Performance Improvements" "⚡" "${PERFORMANCE_ARRAY[@]}"
        fi
        
        # Refactoring
        if [[ ${#REFACTORING_ARRAY[@]} -gt 0 ]]; then
            generate_section "Code Refactoring" "♻️" "${REFACTORING_ARRAY[@]}"
        fi
        
        # Documentation
        if [[ ${#DOCUMENTATION_ARRAY[@]} -gt 0 ]]; then
            generate_section "Documentation" "📚" "${DOCUMENTATION_ARRAY[@]}"
        fi
        
        # Testing
        if [[ ${#TESTING_ARRAY[@]} -gt 0 ]]; then
            generate_section "Testing" "🧪" "${TESTING_ARRAY[@]}"
        fi
        
        # CI/CD
        if [[ ${#CI_CD_ARRAY[@]} -gt 0 ]]; then
            generate_section "CI/CD" "🔧" "${CI_CD_ARRAY[@]}"
        fi
        
        # Dependencies
        if [[ ${#DEPENDENCIES_ARRAY[@]} -gt 0 ]]; then
            generate_section "Dependencies" "📦" "${DEPENDENCIES_ARRAY[@]}"
        fi
        
        # Other changes
        if [[ ${#OTHER_ARRAY[@]} -gt 0 ]]; then
            generate_section "Other Changes" "🔄" "${OTHER_ARRAY[@]}"
        fi
        
        # Statistics
        if [[ "${INCLUDE_STATS}" == "true" && -n "${stats}" ]]; then
            generate_statistics "${stats}"
        fi
        
        # Contributors
        if [[ "${INCLUDE_CONTRIBUTORS}" == "true" && ${#CONTRIBUTORS_ARRAY[@]} -gt 0 ]]; then
            generate_contributors "${CONTRIBUTORS_ARRAY[@]}"
        fi
        
        # Installation instructions
        generate_installation "${version}"
        
        # Upgrade instructions
        if [[ ${#BREAKING_CHANGES_ARRAY[@]} -gt 0 ]] || [[ ${#FEATURES_ARRAY[@]} -gt 0 ]]; then
            generate_upgrade_instructions "${version}"
        fi
        
        # Footer
        generate_footer "${version}"
        
    } > "${output_file}"
    
    if [[ "${DRY_RUN}" != "true" ]]; then
        log_success "Release notes generated: ${output_file}"
    fi
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --since)
            SINCE_TAG="$2"
            shift 2
            ;;
        --output)
            OUTPUT_FILE="$2"
            shift 2
            ;;
        --format)
            FORMAT="$2"
            shift 2
            ;;
        --no-contributors)
            INCLUDE_CONTRIBUTORS="false"
            shift
            ;;
        --no-stats)
            INCLUDE_STATS="false"
            shift
            ;;
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --help)
            echo "Usage: $0 <version> [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --since TAG           Generate notes since specific tag/commit"
            echo "  --output FILE         Output file (default: RELEASE_NOTES.md)"
            echo "  --format FORMAT       Output format (default: markdown)"
            echo "  --no-contributors     Don't include contributors section"
            echo "  --no-stats            Don't include statistics section"
            echo "  --dry-run             Print to stdout instead of file"
            echo "  --help                Show this help message"
            echo ""
            echo "Examples:"
            echo "  $0 1.2.3"
            echo "  $0 1.2.3 --since v1.2.2"
            echo "  $0 1.2.3 --output CHANGELOG.md"
            echo "  $0 1.2.3 --dry-run"
            exit 0
            ;;
        *)
            if [[ -z "${VERSION}" ]]; then
                VERSION="$1"
            fi
            shift
            ;;
    esac
done

# Check prerequisites and generate release notes
check_prerequisites
generate_release_notes "${VERSION}" "${OUTPUT_FILE}"