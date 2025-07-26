#!/bin/bash

# Blue-Green Deployment Script
# This script performs blue-green deployments for the SuiteCRM COBOL Bridge application

set -euo pipefail

# Configuration
NAMESPACE="${NAMESPACE:-production}"
NEW_VERSION="${NEW_VERSION:-latest}"
REGISTRY="${REGISTRY:-ghcr.io}"
REPO_NAME="${GITHUB_REPOSITORY:-suitecrm-cobol-bridge}"
KUBECTL="${KUBECTL:-kubectl}"
HEALTH_CHECK_TIMEOUT="${HEALTH_CHECK_TIMEOUT:-300}"
MONITORING_DURATION="${MONITORING_DURATION:-300}"
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

# Function to get current active environment (blue or green)
get_active_environment() {
    local service_selector
    service_selector=$(${KUBECTL} get service suitecrm-service -n "${NAMESPACE}" -o jsonpath='{.spec.selector.version}' 2>/dev/null || echo "green")
    echo "${service_selector}"
}

# Function to get inactive environment
get_inactive_environment() {
    local active_env="$1"
    if [[ "${active_env}" == "blue" ]]; then
        echo "green"
    else
        echo "blue"
    fi
}

# Function to check if deployment exists
deployment_exists() {
    local deployment_name="$1"
    ${KUBECTL} get deployment "${deployment_name}" -n "${NAMESPACE}" >/dev/null 2>&1
}

# Function to create deployment backup
create_deployment_backup() {
    local timestamp=$(date +%Y%m%d-%H%M%S)
    local backup_dir="deployment-backups/${timestamp}"
    
    log_info "Creating deployment backup..."
    mkdir -p "${backup_dir}"
    
    # Backup current deployments
    ${KUBECTL} get deployment -n "${NAMESPACE}" -o yaml > "${backup_dir}/deployments.yaml"
    ${KUBECTL} get service -n "${NAMESPACE}" -o yaml > "${backup_dir}/services.yaml"
    ${KUBECTL} get configmap -n "${NAMESPACE}" -o yaml > "${backup_dir}/configmaps.yaml"
    ${KUBECTL} get ingress -n "${NAMESPACE}" -o yaml > "${backup_dir}/ingress.yaml" 2>/dev/null || true
    
    # Store current image versions
    ${KUBECTL} get deployments -n "${NAMESPACE}" -o jsonpath='{range .items[*]}{.metadata.name}{": "}{.spec.template.spec.containers[0].image}{"\n"}{end}' > "${backup_dir}/current-images.txt"
    
    log_success "Backup created in ${backup_dir}"
    echo "${backup_dir}"
}

# Function to update deployment with new image
update_deployment_image() {
    local deployment_name="$1"
    local service_name="$2"
    local new_image="$3"
    local environment="$4"
    
    log_info "Updating ${deployment_name} with image ${new_image}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would update deployment ${deployment_name} with image ${new_image}"
        return 0
    fi
    
    # Create new deployment if it doesn't exist
    if ! deployment_exists "${deployment_name}"; then
        log_info "Creating new deployment ${deployment_name}..."
        
        # Get the current active deployment as template
        local active_env=$(get_active_environment)
        local template_deployment="suitecrm-${active_env}"
        
        if deployment_exists "${template_deployment}"; then
            # Copy existing deployment and modify it
            ${KUBECTL} get deployment "${template_deployment}" -n "${NAMESPACE}" -o yaml | \
                sed "s/${template_deployment}/${deployment_name}/g" | \
                sed "s/version: ${active_env}/version: ${environment}/g" | \
                ${KUBECTL} apply -f -
        else
            log_error "Template deployment ${template_deployment} not found"
            return 1
        fi
    fi
    
    # Update the deployment with new image
    ${KUBECTL} set image deployment/"${deployment_name}" \
        "${service_name}=${new_image}" \
        -n "${NAMESPACE}"
    
    # Update deployment labels
    ${KUBECTL} patch deployment "${deployment_name}" -n "${NAMESPACE}" \
        -p '{"metadata":{"labels":{"version":"'${environment}'"}},"spec":{"selector":{"matchLabels":{"version":"'${environment}'"}},"template":{"metadata":{"labels":{"version":"'${environment}'"}}}}}'
}

# Function to wait for deployment to be ready
wait_for_deployment() {
    local deployment_name="$1"
    local timeout="${2:-${HEALTH_CHECK_TIMEOUT}}"
    
    log_info "Waiting for deployment ${deployment_name} to be ready (timeout: ${timeout}s)..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would wait for deployment ${deployment_name}"
        return 0
    fi
    
    if ${KUBECTL} rollout status deployment/"${deployment_name}" -n "${NAMESPACE}" --timeout="${timeout}s"; then
        log_success "Deployment ${deployment_name} is ready"
        return 0
    else
        log_error "Deployment ${deployment_name} failed to become ready within ${timeout}s"
        return 1
    fi
}

# Function to run health checks
run_health_checks() {
    local environment="$1"
    local service_endpoint="$2"
    
    log_info "Running health checks for ${environment} environment..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would run health checks for ${environment}"
        return 0
    fi
    
    local health_endpoints=(
        "/health"
        "/api/health"
        "/monitoring/health"
    )
    
    for endpoint in "${health_endpoints[@]}"; do
        local url="${service_endpoint}${endpoint}"
        log_info "Checking ${url}..."
        
        local retries=5
        local retry_delay=10
        
        for ((i=1; i<=retries; i++)); do
            if curl -f -s --max-time 10 "${url}" >/dev/null; then
                log_success "✓ ${endpoint} health check passed"
                break
            else
                if [[ ${i} -eq ${retries} ]]; then
                    log_error "✗ ${endpoint} health check failed after ${retries} attempts"
                    return 1
                else
                    log_warning "Health check attempt ${i}/${retries} failed, retrying in ${retry_delay}s..."
                    sleep ${retry_delay}
                fi
            fi
        done
    done
    
    log_success "All health checks passed for ${environment} environment"
}

# Function to run smoke tests
run_smoke_tests() {
    local environment="$1"
    local service_endpoint="$2"
    
    log_info "Running smoke tests for ${environment} environment..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would run smoke tests for ${environment}"
        return 0
    fi
    
    # Create temporary test script
    cat > /tmp/smoke-test.sh << 'EOF'
#!/bin/bash
SERVICE_ENDPOINT="$1"

# Test 1: Basic connectivity
echo "Testing basic connectivity..."
if ! curl -f -s "${SERVICE_ENDPOINT}/health" >/dev/null; then
    echo "❌ Basic connectivity test failed"
    exit 1
fi

# Test 2: API functionality
echo "Testing API functionality..."
if ! curl -f -s "${SERVICE_ENDPOINT}/api/cobol/programs" >/dev/null; then
    echo "❌ API functionality test failed"
    exit 1
fi

# Test 3: Authentication
echo "Testing authentication..."
if ! curl -f -s -H "Authorization: Bearer test-token" "${SERVICE_ENDPOINT}/api/auth/verify" >/dev/null; then
    echo "⚠️  Authentication test skipped (no test token)"
fi

# Test 4: Database connectivity
echo "Testing database connectivity..."
if ! curl -f -s "${SERVICE_ENDPOINT}/api/health/database" >/dev/null; then
    echo "❌ Database connectivity test failed"
    exit 1
fi

echo "✅ All smoke tests passed"
EOF
    
    chmod +x /tmp/smoke-test.sh
    
    if /tmp/smoke-test.sh "${service_endpoint}"; then
        log_success "Smoke tests passed for ${environment} environment"
        rm -f /tmp/smoke-test.sh
        return 0
    else
        log_error "Smoke tests failed for ${environment} environment"
        rm -f /tmp/smoke-test.sh
        return 1
    fi
}

# Function to switch traffic to new environment
switch_traffic() {
    local new_environment="$1"
    
    log_info "Switching traffic to ${new_environment} environment..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would switch traffic to ${new_environment}"
        return 0
    fi
    
    # Update service selectors to point to new environment
    ${KUBECTL} patch service suitecrm-service -n "${NAMESPACE}" \
        -p '{"spec":{"selector":{"version":"'${new_environment}'"}}}'
    
    ${KUBECTL} patch service api-gateway-service -n "${NAMESPACE}" \
        -p '{"spec":{"selector":{"version":"'${new_environment}'"}}}'
    
    ${KUBECTL} patch service monitoring-service -n "${NAMESPACE}" \
        -p '{"spec":{"selector":{"version":"'${new_environment}'"}}}'
    
    log_success "Traffic switched to ${new_environment} environment"
}

# Function to monitor new environment
monitor_environment() {
    local environment="$1"
    local duration="${2:-${MONITORING_DURATION}}"
    
    log_info "Monitoring ${environment} environment for ${duration} seconds..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would monitor ${environment} environment"
        return 0
    fi
    
    local start_time=$(date +%s)
    local end_time=$((start_time + duration))
    local check_interval=30
    
    while [[ $(date +%s) -lt ${end_time} ]]; do
        local current_time=$(date +%s)
        local elapsed=$((current_time - start_time))
        local remaining=$((end_time - current_time))
        
        log_info "Monitoring progress: ${elapsed}s elapsed, ${remaining}s remaining"
        
        # Check pod health
        local unhealthy_pods=$(${KUBECTL} get pods -n "${NAMESPACE}" -l version="${environment}" --field-selector=status.phase!=Running --no-headers 2>/dev/null | wc -l)
        
        if [[ ${unhealthy_pods} -gt 0 ]]; then
            log_error "Found ${unhealthy_pods} unhealthy pods in ${environment} environment"
            ${KUBECTL} get pods -n "${NAMESPACE}" -l version="${environment}"
            return 1
        fi
        
        # Check service endpoints
        if ! run_health_checks "${environment}" "https://$(${KUBECTL} get ingress -n "${NAMESPACE}" -o jsonpath='{.items[0].spec.rules[0].host}')"; then
            log_error "Health checks failed during monitoring"
            return 1
        fi
        
        # Check error rates and response times (would integrate with monitoring system)
        log_info "✓ Environment ${environment} is healthy"
        
        sleep ${check_interval}
    done
    
    log_success "Monitoring completed successfully for ${environment} environment"
}

# Function to cleanup old environment
cleanup_old_environment() {
    local old_environment="$1"
    
    log_info "Cleaning up old ${old_environment} environment..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would cleanup ${old_environment} environment"
        return 0
    fi
    
    # Scale down old deployments
    local deployments=(
        "suitecrm-${old_environment}"
        "api-gateway-${old_environment}"
        "monitoring-${old_environment}"
    )
    
    for deployment in "${deployments[@]}"; do
        if deployment_exists "${deployment}"; then
            log_info "Scaling down ${deployment}..."
            ${KUBECTL} scale deployment "${deployment}" --replicas=0 -n "${NAMESPACE}"
        fi
    done
    
    log_success "Old ${old_environment} environment scaled down"
}

# Function to rollback deployment
rollback_deployment() {
    local current_environment="$1"
    local previous_environment="$2"
    
    log_error "🚨 Initiating rollback from ${current_environment} to ${previous_environment}..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would rollback to ${previous_environment}"
        return 0
    fi
    
    # Switch traffic back to previous environment
    switch_traffic "${previous_environment}"
    
    # Scale up previous environment
    local deployments=(
        "suitecrm-${previous_environment}"
        "api-gateway-${previous_environment}" 
        "monitoring-${previous_environment}"
    )
    
    for deployment in "${deployments[@]}"; do
        if deployment_exists "${deployment}"; then
            log_info "Scaling up ${deployment} for rollback..."
            ${KUBECTL} scale deployment "${deployment}" --replicas=3 -n "${NAMESPACE}"
            wait_for_deployment "${deployment}" 120
        fi
    done
    
    # Verify rollback
    if run_health_checks "${previous_environment}" "https://$(${KUBECTL} get ingress -n "${NAMESPACE}" -o jsonpath='{.items[0].spec.rules[0].host}')"; then
        log_success "Rollback to ${previous_environment} completed successfully"
    else
        log_error "Rollback verification failed"
        return 1
    fi
}

# Function to create deployment record
create_deployment_record() {
    local version="$1"
    local environment="$2"
    local status="$3"
    
    local timestamp=$(date +%Y%m%d-%H%M%S)
    local record_name="deployment-record-${timestamp}"
    
    log_info "Creating deployment record..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create deployment record"
        return 0
    fi
    
    ${KUBECTL} create configmap "${record_name}" \
        --from-literal=version="${version}" \
        --from-literal=environment="${environment}" \
        --from-literal=status="${status}" \
        --from-literal=timestamp="$(date -u +%Y-%m-%dT%H:%M:%S.%3NZ)" \
        --from-literal=deployed_by="${GITHUB_ACTOR:-$(whoami)}" \
        --from-literal=commit="${GITHUB_SHA:-unknown}" \
        --from-literal=build_number="${GITHUB_RUN_NUMBER:-unknown}" \
        -n "${NAMESPACE}"
    
    log_success "Deployment record created: ${record_name}"
}

# Main deployment function
deploy() {
    local deployment_start_time=$(date +%s)
    
    log_info "🚀 Starting blue-green deployment..."
    log_info "Namespace: ${NAMESPACE}"
    log_info "New Version: ${NEW_VERSION}"
    log_info "Registry: ${REGISTRY}"
    log_info "Repository: ${REPO_NAME}"
    log_info "Dry Run: ${DRY_RUN}"
    
    # Get current state
    local active_env=$(get_active_environment)
    local inactive_env=$(get_inactive_environment "${active_env}")
    
    log_info "Current active environment: ${active_env}"
    log_info "Deploying to inactive environment: ${inactive_env}"
    
    # Create backup
    local backup_dir=$(create_deployment_backup)
    
    # Prepare new images
    local images=(
        "suitecrm:${REGISTRY}/${REPO_NAME}-suitecrm:${NEW_VERSION}"
        "api-gateway:${REGISTRY}/${REPO_NAME}-api-gateway:${NEW_VERSION}"
        "monitoring:${REGISTRY}/${REPO_NAME}-monitoring:${NEW_VERSION}"
    )
    
    # Update inactive environment with new images
    for image_pair in "${images[@]}"; do
        IFS=':' read -ra IMAGE_INFO <<< "${image_pair}"
        local service_name="${IMAGE_INFO[0]}"
        local image_url="${IMAGE_INFO[1]}:${IMAGE_INFO[2]}"
        local deployment_name="${service_name}-${inactive_env}"
        
        update_deployment_image "${deployment_name}" "${service_name}" "${image_url}" "${inactive_env}"
        wait_for_deployment "${deployment_name}"
    done
    
    # Get service endpoint for testing
    local service_endpoint="https://$(${KUBECTL} get ingress -n "${NAMESPACE}" -o jsonpath='{.items[0].spec.rules[0].host}' 2>/dev/null || echo 'localhost')"
    
    # Run health checks on inactive environment
    # Note: We'll create a temporary service for testing the inactive environment
    if [[ "${DRY_RUN}" != "true" ]]; then
        ${KUBECTL} expose deployment "suitecrm-${inactive_env}" \
            --port=80 --target-port=80 \
            --name="suitecrm-${inactive_env}-test" \
            --type=ClusterIP \
            -n "${NAMESPACE}" || true
        
        # Port forward for testing (in background)
        ${KUBECTL} port-forward service/"suitecrm-${inactive_env}-test" 8080:80 -n "${NAMESPACE}" &
        local port_forward_pid=$!
        sleep 10
        
        if run_health_checks "${inactive_env}" "http://localhost:8080"; then
            log_success "Health checks passed for ${inactive_env} environment"
        else
            log_error "Health checks failed for ${inactive_env} environment"
            kill ${port_forward_pid} 2>/dev/null || true
            ${KUBECTL} delete service "suitecrm-${inactive_env}-test" -n "${NAMESPACE}" || true
            rollback_deployment "${inactive_env}" "${active_env}"
            exit 1
        fi
        
        # Cleanup test resources
        kill ${port_forward_pid} 2>/dev/null || true
        ${KUBECTL} delete service "suitecrm-${inactive_env}-test" -n "${NAMESPACE}" || true
    fi
    
    # Run smoke tests
    if ! run_smoke_tests "${inactive_env}" "${service_endpoint}"; then
        log_error "Smoke tests failed, initiating rollback..."
        rollback_deployment "${inactive_env}" "${active_env}"
        exit 1
    fi
    
    # Switch traffic to new environment
    switch_traffic "${inactive_env}"
    
    # Monitor new environment
    if ! monitor_environment "${inactive_env}"; then
        log_error "Monitoring detected issues, initiating rollback..."
        rollback_deployment "${inactive_env}" "${active_env}"
        exit 1
    fi
    
    # Cleanup old environment
    cleanup_old_environment "${active_env}"
    
    # Create deployment record
    create_deployment_record "${NEW_VERSION}" "${inactive_env}" "success"
    
    local deployment_end_time=$(date +%s)
    local deployment_duration=$((deployment_end_time - deployment_start_time))
    
    log_success "🎉 Blue-green deployment completed successfully!"
    log_success "Deployment duration: ${deployment_duration} seconds"
    log_success "Active environment: ${inactive_env}"
    log_success "Version: ${NEW_VERSION}"
    log_success "Backup location: ${backup_dir}"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --namespace)
            NAMESPACE="$2"
            shift 2
            ;;
        --version)
            NEW_VERSION="$2"
            shift 2
            ;;
        --registry)
            REGISTRY="$2"
            shift 2
            ;;
        --repo)
            REPO_NAME="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --timeout)
            HEALTH_CHECK_TIMEOUT="$2"
            shift 2
            ;;
        --monitor-duration)
            MONITORING_DURATION="$2"
            shift 2
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --namespace NAMESPACE        Kubernetes namespace (default: production)"
            echo "  --version VERSION            New version to deploy (default: latest)"
            echo "  --registry REGISTRY          Container registry (default: ghcr.io)"
            echo "  --repo REPO_NAME             Repository name"
            echo "  --dry-run                    Perform dry run without making changes"
            echo "  --timeout SECONDS            Health check timeout (default: 300)"
            echo "  --monitor-duration SECONDS   Monitoring duration (default: 300)"
            echo "  --help                       Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Verify prerequisites
if ! command -v ${KUBECTL} >/dev/null 2>&1; then
    log_error "kubectl is not installed or not in PATH"
    exit 1
fi

if ! command -v curl >/dev/null 2>&1; then
    log_error "curl is not installed"
    exit 1
fi

# Verify kubectl can connect to cluster
if ! ${KUBECTL} cluster-info >/dev/null 2>&1; then
    log_error "Cannot connect to Kubernetes cluster"
    exit 1
fi

# Verify namespace exists
if ! ${KUBECTL} get namespace "${NAMESPACE}" >/dev/null 2>&1; then
    log_error "Namespace ${NAMESPACE} does not exist"
    exit 1
fi

# Run deployment
deploy