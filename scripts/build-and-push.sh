#!/bin/bash

# Build and Push Docker Images Script
# This script builds all container images and pushes them to the registry

set -euo pipefail

# Configuration
REGISTRY="${REGISTRY:-ghcr.io}"
REPO_NAME="${GITHUB_REPOSITORY:-suitecrm-cobol-bridge}"
VERSION="${VERSION:-latest}"
BUILD_CONTEXT="${BUILD_CONTEXT:-.}"
PUSH="${PUSH:-true}"
PLATFORM="${PLATFORM:-linux/amd64,linux/arm64}"

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

# Function to check if Docker Buildx is available
check_buildx() {
    if ! docker buildx version >/dev/null 2>&1; then
        log_error "Docker Buildx is not available. Please install or enable it."
        exit 1
    fi
    
    # Create builder if it doesn't exist
    if ! docker buildx inspect multiarch >/dev/null 2>&1; then
        log_info "Creating multi-platform builder..."
        docker buildx create --name multiarch --platform ${PLATFORM} --use
    else
        docker buildx use multiarch
    fi
}

# Function to authenticate with registry
authenticate_registry() {
    if [[ -n "${GITHUB_TOKEN:-}" ]]; then
        log_info "Authenticating with GitHub Container Registry..."
        echo "${GITHUB_TOKEN}" | docker login "${REGISTRY}" -u "${GITHUB_ACTOR}" --password-stdin
    elif [[ -n "${DOCKER_PASSWORD:-}" && -n "${DOCKER_USERNAME:-}" ]]; then
        log_info "Authenticating with Docker registry..."
        echo "${DOCKER_PASSWORD}" | docker login "${REGISTRY}" -u "${DOCKER_USERNAME}" --password-stdin
    else
        log_warning "No registry authentication credentials provided"
    fi
}

# Function to build and push an image
build_and_push_image() {
    local service_name="$1"
    local dockerfile="$2"
    local context="$3"
    local additional_tags="${4:-}"
    
    local image_name="${REGISTRY}/${REPO_NAME}-${service_name}"
    local build_args=()
    
    log_info "Building ${service_name} image..."
    log_info "Dockerfile: ${dockerfile}"
    log_info "Context: ${context}"
    log_info "Platform: ${PLATFORM}"
    
    # Prepare tags
    local tags=("${image_name}:${VERSION}")
    
    if [[ "${VERSION}" != "latest" ]]; then
        tags+=("${image_name}:latest")
    fi
    
    # Add any additional tags
    if [[ -n "${additional_tags}" ]]; then
        IFS=',' read -ra ADDITIONAL_TAGS <<< "${additional_tags}"
        for tag in "${ADDITIONAL_TAGS[@]}"; do
            tags+=("${image_name}:${tag}")
        done
    fi
    
    # Build tag arguments
    local tag_args=()
    for tag in "${tags[@]}"; do
        tag_args+=("--tag" "${tag}")
    done
    
    # Add build arguments based on service
    case "${service_name}" in
        "suitecrm")
            build_args+=(
                "--build-arg" "PHP_VERSION=8.1"
                "--build-arg" "APACHE_VERSION=2.4"
            )
            ;;
        "api-gateway")
            build_args+=(
                "--build-arg" "NODE_VERSION=18"
                "--build-arg" "APP_ENV=production"
            )
            ;;
        "cobol-compiler")
            build_args+=(
                "--build-arg" "GNUCOBOL_VERSION=3.2"
            )
            ;;
    esac
    
    # Build and push command
    local docker_command=(
        "docker" "buildx" "build"
        "--platform" "${PLATFORM}"
        "--file" "${dockerfile}"
        "${tag_args[@]}"
        "${build_args[@]}"
        "--cache-from" "type=gha"
        "--cache-to" "type=gha,mode=max"
        "--metadata-file" "/tmp/metadata-${service_name}.json"
    )
    
    if [[ "${PUSH}" == "true" ]]; then
        docker_command+=("--push")
    else
        docker_command+=("--load")
    fi
    
    docker_command+=("${context}")
    
    # Execute build
    if "${docker_command[@]}"; then
        log_success "Successfully built ${service_name} image"
        
        # Display image information
        if [[ -f "/tmp/metadata-${service_name}.json" ]]; then
            local digest=$(jq -r '.["containerimage.digest"]' "/tmp/metadata-${service_name}.json" 2>/dev/null || echo "unknown")
            log_info "Image digest: ${digest}"
        fi
        
        # List created tags
        for tag in "${tags[@]}"; do
            log_info "Tagged: ${tag}"
        done
    else
        log_error "Failed to build ${service_name} image"
        return 1
    fi
}

# Function to build all images
build_all_images() {
    local build_start_time=$(date +%s)
    
    log_info "Starting multi-service Docker build..."
    log_info "Registry: ${REGISTRY}"
    log_info "Repository: ${REPO_NAME}"
    log_info "Version: ${VERSION}"
    log_info "Platform: ${PLATFORM}"
    log_info "Push: ${PUSH}"
    
    # Build main SuiteCRM image
    build_and_push_image "suitecrm" "Dockerfile.suitecrm" "." "main"
    
    # Build COBOL compiler image
    build_and_push_image "cobol-compiler" "Dockerfile.cobol" "." "compiler"
    
    # Build API Gateway image
    build_and_push_image "api-gateway" "api-gateway/Dockerfile" "./api-gateway" "api"
    
    # Build Monitoring Dashboard image
    build_and_push_image "monitoring" "monitoring/Dockerfile" "./monitoring" "dashboard"
    
    # Build Business Rules Editor image
    build_and_push_image "business-rules" "business-rules/Dockerfile" "./business-rules" "rules"
    
    # Build Mobile API image
    build_and_push_image "mobile-api" "mobile-api/Dockerfile" "./mobile-api" "mobile"
    
    # Build Debugger image
    build_and_push_image "debugger" "debugger/Dockerfile" "./debugger" "debug"
    
    # Build Cloud Burst image
    build_and_push_image "cloud-burst" "cloud-burst/Dockerfile" "./cloud-burst" "cloud"
    
    local build_end_time=$(date +%s)
    local build_duration=$((build_end_time - build_start_time))
    
    log_success "All images built successfully in ${build_duration} seconds"
}

# Function to generate build manifest
generate_build_manifest() {
    local manifest_file="build-manifest.json"
    
    log_info "Generating build manifest..."
    
    cat > "${manifest_file}" << EOF
{
  "build_info": {
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%S.%3NZ)",
    "version": "${VERSION}",
    "git_commit": "${GITHUB_SHA:-unknown}",
    "git_branch": "${GITHUB_REF_NAME:-unknown}",
    "build_number": "${GITHUB_RUN_NUMBER:-unknown}",
    "builder": "${GITHUB_ACTOR:-$(whoami)}",
    "registry": "${REGISTRY}",
    "repository": "${REPO_NAME}",
    "platform": "${PLATFORM}"
  },
  "images": {
    "suitecrm": {
      "tag": "${REGISTRY}/${REPO_NAME}-suitecrm:${VERSION}",
      "dockerfile": "Dockerfile.suitecrm",
      "context": "."
    },
    "cobol_compiler": {
      "tag": "${REGISTRY}/${REPO_NAME}-cobol-compiler:${VERSION}",
      "dockerfile": "Dockerfile.cobol",
      "context": "."
    },
    "api_gateway": {
      "tag": "${REGISTRY}/${REPO_NAME}-api-gateway:${VERSION}",
      "dockerfile": "api-gateway/Dockerfile",
      "context": "./api-gateway"
    },
    "monitoring": {
      "tag": "${REGISTRY}/${REPO_NAME}-monitoring:${VERSION}",
      "dockerfile": "monitoring/Dockerfile",
      "context": "./monitoring"
    },
    "business_rules": {
      "tag": "${REGISTRY}/${REPO_NAME}-business-rules:${VERSION}",
      "dockerfile": "business-rules/Dockerfile",
      "context": "./business-rules"
    },
    "mobile_api": {
      "tag": "${REGISTRY}/${REPO_NAME}-mobile-api:${VERSION}",
      "dockerfile": "mobile-api/Dockerfile",
      "context": "./mobile-api"
    },
    "debugger": {
      "tag": "${REGISTRY}/${REPO_NAME}-debugger:${VERSION}",
      "dockerfile": "debugger/Dockerfile",
      "context": "./debugger"
    },
    "cloud_burst": {
      "tag": "${REGISTRY}/${REPO_NAME}-cloud-burst:${VERSION}",
      "dockerfile": "cloud-burst/Dockerfile",
      "context": "./cloud-burst"
    }
  }
}
EOF
    
    log_success "Build manifest generated: ${manifest_file}"
}

# Function to verify images
verify_images() {
    log_info "Verifying built images..."
    
    local images=(
        "${REGISTRY}/${REPO_NAME}-suitecrm:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-cobol-compiler:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-api-gateway:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-monitoring:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-business-rules:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-mobile-api:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-debugger:${VERSION}"
        "${REGISTRY}/${REPO_NAME}-cloud-burst:${VERSION}"
    )
    
    for image in "${images[@]}"; do
        if [[ "${PUSH}" == "true" ]]; then
            # Verify in registry
            if docker manifest inspect "${image}" >/dev/null 2>&1; then
                log_success "✓ ${image} verified in registry"
            else
                log_error "✗ ${image} not found in registry"
                return 1
            fi
        else
            # Verify locally
            if docker image inspect "${image}" >/dev/null 2>&1; then
                log_success "✓ ${image} verified locally"
            else
                log_error "✗ ${image} not found locally"
                return 1
            fi
        fi
    done
    
    log_success "All images verified successfully"
}

# Function to cleanup
cleanup() {
    log_info "Cleaning up temporary files..."
    rm -f /tmp/metadata-*.json
    
    # Clean up builder if we created it
    if docker buildx inspect multiarch >/dev/null 2>&1; then
        docker buildx stop multiarch || true
    fi
}

# Main execution
main() {
    log_info "Starting Docker build and push process..."
    
    # Set up signal handlers for cleanup
    trap cleanup EXIT INT TERM
    
    # Check prerequisites
    check_buildx
    
    # Authenticate if credentials are available
    if [[ "${PUSH}" == "true" ]]; then
        authenticate_registry
    fi
    
    # Build all images
    build_all_images
    
    # Generate build manifest
    generate_build_manifest
    
    # Verify images
    verify_images
    
    log_success "Docker build and push process completed successfully!"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --registry)
            REGISTRY="$2"
            shift 2
            ;;
        --repo)
            REPO_NAME="$2"
            shift 2
            ;;
        --version)
            VERSION="$2"
            shift 2
            ;;
        --platform)
            PLATFORM="$2"
            shift 2
            ;;
        --no-push)
            PUSH="false"
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --registry REGISTRY    Container registry (default: ghcr.io)"
            echo "  --repo REPO_NAME       Repository name (default: suitecrm-cobol-bridge)"
            echo "  --version VERSION      Image version tag (default: latest)"
            echo "  --platform PLATFORMS   Target platforms (default: linux/amd64,linux/arm64)"
            echo "  --no-push              Don't push images to registry"
            echo "  --help                 Show this help message"
            echo ""
            echo "Environment variables:"
            echo "  GITHUB_TOKEN          GitHub token for registry authentication"
            echo "  DOCKER_USERNAME       Docker registry username"
            echo "  DOCKER_PASSWORD       Docker registry password"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Run main function
main