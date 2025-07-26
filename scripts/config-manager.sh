#!/bin/bash

# Configuration Management Script
# This script manages configuration across different environments

set -euo pipefail

# Configuration
ENVIRONMENT="${ENVIRONMENT:-development}"
CONFIG_DIR="${CONFIG_DIR:-config}"
SECRETS_DIR="${SECRETS_DIR:-secrets}"
OUTPUT_DIR="${OUTPUT_DIR:-generated-config}"
TEMPLATE_ENGINE="${TEMPLATE_ENGINE:-envsubst}"
VAULT_ADDR="${VAULT_ADDR:-}"
VAULT_TOKEN="${VAULT_TOKEN:-}"
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
    local missing_tools=()
    
    # Check for required tools
    if ! command -v envsubst >/dev/null 2>&1; then
        missing_tools+=("envsubst")
    fi
    
    if ! command -v yq >/dev/null 2>&1; then
        missing_tools+=("yq")
    fi
    
    if ! command -v jq >/dev/null 2>&1; then
        missing_tools+=("jq")
    fi
    
    if [[ ${#missing_tools[@]} -gt 0 ]]; then
        log_error "Missing required tools: ${missing_tools[*]}"
        log_info "Please install missing tools and try again"
        exit 1
    fi
}

# Function to load environment variables from file
load_env_file() {
    local env_file="$1"
    
    if [[ -f "${env_file}" ]]; then
        log_info "Loading environment variables from ${env_file}"
        
        # Source the file in a subshell to avoid affecting current environment
        set -a
        source "${env_file}"
        set +a
        
        log_success "Environment variables loaded from ${env_file}"
    else
        log_warning "Environment file not found: ${env_file}"
    fi
}

# Function to fetch secrets from HashiCorp Vault
fetch_vault_secrets() {
    local secret_path="$1"
    local output_file="$2"
    
    if [[ -z "${VAULT_ADDR}" ]] || [[ -z "${VAULT_TOKEN}" ]]; then
        log_warning "Vault configuration not provided, skipping secret fetch"
        return 0
    fi
    
    log_info "Fetching secrets from Vault path: ${secret_path}"
    
    if ! command -v vault >/dev/null 2>&1; then
        log_error "Vault CLI not installed"
        return 1
    fi
    
    # Set Vault environment
    export VAULT_ADDR="${VAULT_ADDR}"
    export VAULT_TOKEN="${VAULT_TOKEN}"
    
    if vault kv get -format=json "${secret_path}" > "${output_file}" 2>/dev/null; then
        log_success "Secrets fetched from Vault"
        
        # Parse secrets and export as environment variables
        jq -r '.data.data | to_entries[] | "export \(.key)=\(.value)"' "${output_file}" > "${output_file}.env"
        source "${output_file}.env"
        rm -f "${output_file}.env"
        
        return 0
    else
        log_error "Failed to fetch secrets from Vault"
        return 1
    fi
}

# Function to validate configuration
validate_config() {
    local config_file="$1"
    local schema_file="$2"
    
    log_info "Validating configuration: ${config_file}"
    
    if [[ ! -f "${config_file}" ]]; then
        log_error "Configuration file not found: ${config_file}"
        return 1
    fi
    
    # Validate JSON/YAML syntax
    local file_extension="${config_file##*.}"
    case "${file_extension}" in
        json)
            if ! jq . "${config_file}" >/dev/null 2>&1; then
                log_error "Invalid JSON syntax in ${config_file}"
                return 1
            fi
            ;;
        yaml|yml)
            if ! yq eval . "${config_file}" >/dev/null 2>&1; then
                log_error "Invalid YAML syntax in ${config_file}"
                return 1
            fi
            ;;
    esac
    
    # Validate against schema if provided
    if [[ -f "${schema_file}" ]]; then
        log_info "Validating against schema: ${schema_file}"
        
        case "${file_extension}" in
            json)
                if command -v ajv >/dev/null 2>&1; then
                    if ajv validate -s "${schema_file}" -d "${config_file}"; then
                        log_success "Configuration validation passed"
                    else
                        log_error "Configuration validation failed"
                        return 1
                    fi
                else
                    log_warning "ajv not installed, skipping schema validation"
                fi
                ;;
            yaml|yml)
                # Convert YAML to JSON for validation
                local temp_json="/tmp/config_temp.json"
                yq eval -o=json "${config_file}" > "${temp_json}"
                
                if command -v ajv >/dev/null 2>&1; then
                    if ajv validate -s "${schema_file}" -d "${temp_json}"; then
                        log_success "Configuration validation passed"
                    else
                        log_error "Configuration validation failed"
                        return 1
                    fi
                else
                    log_warning "ajv not installed, skipping schema validation"
                fi
                
                rm -f "${temp_json}"
                ;;
        esac
    fi
    
    log_success "Configuration validation completed"
}

# Function to process template file
process_template() {
    local template_file="$1"
    local output_file="$2"
    local variables_file="$3"
    
    log_info "Processing template: ${template_file}"
    
    if [[ ! -f "${template_file}" ]]; then
        log_error "Template file not found: ${template_file}"
        return 1
    fi
    
    # Load variables if file exists
    if [[ -f "${variables_file}" ]]; then
        load_env_file "${variables_file}"
    fi
    
    # Create output directory
    mkdir -p "$(dirname "${output_file}")"
    
    case "${TEMPLATE_ENGINE}" in
        envsubst)
            if envsubst < "${template_file}" > "${output_file}"; then
                log_success "Template processed successfully: ${output_file}"
            else
                log_error "Failed to process template: ${template_file}"
                return 1
            fi
            ;;
        *)
            log_error "Unsupported template engine: ${TEMPLATE_ENGINE}"
            return 1
            ;;
    esac
}

# Function to generate Kubernetes ConfigMaps
generate_configmap() {
    local config_name="$1"
    local config_files="$2"
    local namespace="${3:-default}"
    local output_file="$4"
    
    log_info "Generating ConfigMap: ${config_name}"
    
    local kubectl_args=(
        "create" "configmap" "${config_name}"
        "--namespace=${namespace}"
        "--dry-run=client"
        "-o" "yaml"
    )
    
    # Add config files
    IFS=',' read -ra FILES <<< "${config_files}"
    for file in "${FILES[@]}"; do
        if [[ -f "${file}" ]]; then
            kubectl_args+=("--from-file=${file}")
        else
            log_warning "Config file not found: ${file}"
        fi
    done
    
    if kubectl "${kubectl_args[@]}" > "${output_file}"; then
        log_success "ConfigMap generated: ${output_file}"
    else
        log_error "Failed to generate ConfigMap"
        return 1
    fi
}

# Function to generate Kubernetes Secrets
generate_secret() {
    local secret_name="$1"
    local secret_type="${2:-Opaque}"
    local namespace="${3:-default}"
    local output_file="$4"
    
    log_info "Generating Secret: ${secret_name}"
    
    local kubectl_args=(
        "create" "secret" "generic" "${secret_name}"
        "--type=${secret_type}"
        "--namespace=${namespace}"
        "--dry-run=client"
        "-o" "yaml"
    )
    
    # Add secrets from environment variables
    local secret_vars=(
        "DB_PASSWORD"
        "JWT_SECRET"
        "REDIS_PASSWORD"
        "API_KEY"
        "ENCRYPTION_KEY"
    )
    
    for var in "${secret_vars[@]}"; do
        if [[ -n "${!var:-}" ]]; then
            kubectl_args+=("--from-literal=${var}=${!var}")
        fi
    done
    
    if kubectl "${kubectl_args[@]}" > "${output_file}"; then
        log_success "Secret generated: ${output_file}"
        
        # Mask sensitive data in logs
        log_info "Secret contains ${#secret_vars[@]} entries"
    else
        log_error "Failed to generate Secret"
        return 1
    fi
}

# Function to encrypt sensitive files
encrypt_file() {
    local input_file="$1"
    local output_file="$2"
    local encryption_key="${3:-}"
    
    log_info "Encrypting file: ${input_file}"
    
    if [[ -z "${encryption_key}" ]]; then
        log_error "Encryption key not provided"
        return 1
    fi
    
    if ! command -v openssl >/dev/null 2>&1; then
        log_error "OpenSSL not installed"
        return 1
    fi
    
    if openssl enc -aes-256-cbc -salt -pbkdf2 -in "${input_file}" -out "${output_file}" -k "${encryption_key}"; then
        log_success "File encrypted: ${output_file}"
        
        # Set secure permissions
        chmod 600 "${output_file}"
    else
        log_error "Failed to encrypt file"
        return 1
    fi
}

# Function to decrypt sensitive files
decrypt_file() {
    local input_file="$1"
    local output_file="$2"
    local encryption_key="${3:-}"
    
    log_info "Decrypting file: ${input_file}"
    
    if [[ -z "${encryption_key}" ]]; then
        log_error "Encryption key not provided"
        return 1
    fi
    
    if ! command -v openssl >/dev/null 2>&1; then
        log_error "OpenSSL not installed"
        return 1
    fi
    
    if openssl enc -aes-256-cbc -d -pbkdf2 -in "${input_file}" -out "${output_file}" -k "${encryption_key}"; then
        log_success "File decrypted: ${output_file}"
        
        # Set secure permissions
        chmod 600 "${output_file}"
    else
        log_error "Failed to decrypt file"
        return 1
    fi
}

# Function to sync configuration to remote store
sync_config() {
    local config_path="$1"
    local remote_endpoint="$2"
    local sync_mode="${3:-push}"
    
    log_info "Syncing configuration: ${sync_mode} to ${remote_endpoint}"
    
    case "${remote_endpoint}" in
        s3://*)
            if ! command -v aws >/dev/null 2>&1; then
                log_error "AWS CLI not installed"
                return 1
            fi
            
            case "${sync_mode}" in
                push)
                    if aws s3 sync "${config_path}" "${remote_endpoint}" --delete; then
                        log_success "Configuration pushed to S3"
                    else
                        log_error "Failed to push configuration to S3"
                        return 1
                    fi
                    ;;
                pull)
                    if aws s3 sync "${remote_endpoint}" "${config_path}" --delete; then
                        log_success "Configuration pulled from S3"
                    else
                        log_error "Failed to pull configuration from S3"
                        return 1
                    fi
                    ;;
            esac
            ;;
        gs://*)
            if ! command -v gsutil >/dev/null 2>&1; then
                log_error "Google Cloud SDK not installed"
                return 1
            fi
            
            case "${sync_mode}" in
                push)
                    if gsutil -m rsync -r -d "${config_path}" "${remote_endpoint}"; then
                        log_success "Configuration pushed to GCS"
                    else
                        log_error "Failed to push configuration to GCS"
                        return 1
                    fi
                    ;;
                pull)
                    if gsutil -m rsync -r -d "${remote_endpoint}" "${config_path}"; then
                        log_success "Configuration pulled from GCS"
                    else
                        log_error "Failed to pull configuration from GCS"
                        return 1
                    fi
                    ;;
            esac
            ;;
        *)
            log_error "Unsupported remote endpoint: ${remote_endpoint}"
            return 1
            ;;
    esac
}

# Function to generate configuration for specific environment
generate_environment_config() {
    local env="$1"
    local base_config_dir="${CONFIG_DIR}/base"
    local env_config_dir="${CONFIG_DIR}/environments/${env}"
    local output_dir="${OUTPUT_DIR}/${env}"
    
    log_info "Generating configuration for environment: ${env}"
    
    # Create output directory
    mkdir -p "${output_dir}"
    
    # Load environment-specific variables
    local env_file="${env_config_dir}/.env"
    if [[ -f "${env_file}" ]]; then
        load_env_file "${env_file}"
    fi
    
    # Fetch secrets if Vault is configured
    if [[ -n "${VAULT_ADDR}" ]] && [[ -n "${VAULT_TOKEN}" ]]; then
        local vault_secrets_file="${output_dir}/vault-secrets.json"
        fetch_vault_secrets "secret/${env}/suitecrm-cobol-bridge" "${vault_secrets_file}"
    fi
    
    # Process template files
    local template_files=(
        "docker-compose.yml"
        "kubernetes/deployment.yaml"
        "nginx/nginx.conf"
        "api-gateway/config.json"
        "monitoring/prometheus.yml"
        "database/connection.json"
    )
    
    for template in "${template_files[@]}"; do
        local template_file="${base_config_dir}/${template}.template"
        local env_template_file="${env_config_dir}/${template}.template"
        local output_file="${output_dir}/${template}"
        local variables_file="${env_config_dir}/variables.env"
        
        # Use environment-specific template if it exists, otherwise use base template
        if [[ -f "${env_template_file}" ]]; then
            process_template "${env_template_file}" "${output_file}" "${variables_file}"
        elif [[ -f "${template_file}" ]]; then
            process_template "${template_file}" "${output_file}" "${variables_file}"
        else
            log_warning "Template not found: ${template}"
            continue
        fi
        
        # Validate generated configuration
        local schema_file="${base_config_dir}/schemas/${template}.schema.json"
        if [[ -f "${schema_file}" ]]; then
            validate_config "${output_file}" "${schema_file}"
        fi
    done
    
    # Generate Kubernetes resources
    local k8s_output_dir="${output_dir}/kubernetes"
    mkdir -p "${k8s_output_dir}"
    
    # Generate ConfigMaps
    local config_files="${output_dir}/api-gateway/config.json,${output_dir}/nginx/nginx.conf"
    generate_configmap "suitecrm-config" "${config_files}" "${env}" "${k8s_output_dir}/configmap.yaml"
    
    # Generate Secrets
    generate_secret "suitecrm-secrets" "Opaque" "${env}" "${k8s_output_dir}/secrets.yaml"
    
    log_success "Configuration generated for environment: ${env}"
    log_info "Output directory: ${output_dir}"
}

# Function to compare configurations between environments
compare_configs() {
    local env1="$1"
    local env2="$2"
    local config_file="$3"
    
    log_info "Comparing configuration '${config_file}' between ${env1} and ${env2}"
    
    local file1="${OUTPUT_DIR}/${env1}/${config_file}"
    local file2="${OUTPUT_DIR}/${env2}/${config_file}"
    
    if [[ ! -f "${file1}" ]]; then
        log_error "Configuration not found: ${file1}"
        return 1
    fi
    
    if [[ ! -f "${file2}" ]]; then
        log_error "Configuration not found: ${file2}"
        return 1
    fi
    
    echo ""
    echo "Configuration Comparison: ${config_file}"
    echo "Environment 1: ${env1}"
    echo "Environment 2: ${env2}"
    echo "=================================="
    
    if diff -u "${file1}" "${file2}"; then
        log_success "No differences found"
    else
        log_warning "Differences found between environments"
    fi
}

# Function to backup current configuration
backup_config() {
    local backup_name="${1:-$(date +%Y%m%d_%H%M%S)}"
    local backup_dir="config-backups/${backup_name}"
    
    log_info "Creating configuration backup: ${backup_name}"
    
    mkdir -p "${backup_dir}"
    
    # Backup configuration directories
    if [[ -d "${CONFIG_DIR}" ]]; then
        cp -r "${CONFIG_DIR}" "${backup_dir}/"
    fi
    
    if [[ -d "${OUTPUT_DIR}" ]]; then
        cp -r "${OUTPUT_DIR}" "${backup_dir}/"
    fi
    
    # Create backup metadata
    cat > "${backup_dir}/backup-info.json" << EOF
{
    "backup_name": "${backup_name}",
    "created_at": "$(date -u +%Y-%m-%dT%H:%M:%S.%3NZ)",
    "created_by": "${USER:-$(whoami)}",
    "environment": "${ENVIRONMENT}",
    "config_dir": "${CONFIG_DIR}",
    "output_dir": "${OUTPUT_DIR}",
    "git_commit": "${GITHUB_SHA:-$(git rev-parse HEAD 2>/dev/null || echo 'unknown')}"
}
EOF
    
    log_success "Configuration backup created: ${backup_dir}"
    
    # Compress backup
    if command -v tar >/dev/null 2>&1; then
        tar -czf "${backup_dir}.tar.gz" -C "config-backups" "${backup_name}"
        rm -rf "${backup_dir}"
        log_success "Backup compressed: ${backup_dir}.tar.gz"
    fi
}

# Function to restore configuration from backup
restore_config() {
    local backup_name="$1"
    local backup_file="config-backups/${backup_name}.tar.gz"
    local restore_dir="config-backups/${backup_name}"
    
    log_info "Restoring configuration from backup: ${backup_name}"
    
    if [[ ! -f "${backup_file}" ]]; then
        log_error "Backup file not found: ${backup_file}"
        return 1
    fi
    
    # Extract backup
    tar -xzf "${backup_file}" -C "config-backups"
    
    # Restore configuration
    if [[ -d "${restore_dir}/${CONFIG_DIR}" ]]; then
        rm -rf "${CONFIG_DIR}"
        cp -r "${restore_dir}/${CONFIG_DIR}" .
        log_success "Configuration restored from backup"
    else
        log_error "No configuration found in backup"
        return 1
    fi
    
    # Clean up extracted files
    rm -rf "${restore_dir}"
}

# Main function
main() {
    local command="${1:-generate}"
    
    case "${command}" in
        generate)
            check_prerequisites
            generate_environment_config "${ENVIRONMENT}"
            ;;
        validate)
            local config_file="${2:-}"
            local schema_file="${3:-}"
            if [[ -z "${config_file}" ]]; then
                log_error "Configuration file not specified"
                exit 1
            fi
            check_prerequisites
            validate_config "${config_file}" "${schema_file}"
            ;;
        compare)
            local env1="${2:-development}"
            local env2="${3:-production}"
            local config_file="${4:-docker-compose.yml}"
            check_prerequisites
            compare_configs "${env1}" "${env2}" "${config_file}"
            ;;
        sync)
            local config_path="${2:-${OUTPUT_DIR}}"
            local remote_endpoint="${3:-}"
            local sync_mode="${4:-push}"
            if [[ -z "${remote_endpoint}" ]]; then
                log_error "Remote endpoint not specified"
                exit 1
            fi
            sync_config "${config_path}" "${remote_endpoint}" "${sync_mode}"
            ;;
        encrypt)
            local input_file="${2:-}"
            local output_file="${3:-${input_file}.enc}"
            local encryption_key="${ENCRYPTION_KEY:-}"
            if [[ -z "${input_file}" ]]; then
                log_error "Input file not specified"
                exit 1
            fi
            encrypt_file "${input_file}" "${output_file}" "${encryption_key}"
            ;;
        decrypt)
            local input_file="${2:-}"
            local output_file="${3:-${input_file%.enc}}"
            local encryption_key="${ENCRYPTION_KEY:-}"
            if [[ -z "${input_file}" ]]; then
                log_error "Input file not specified"
                exit 1
            fi
            decrypt_file "${input_file}" "${output_file}" "${encryption_key}"
            ;;
        backup)
            local backup_name="${2:-}"
            backup_config "${backup_name}"
            ;;
        restore)
            local backup_name="${2:-}"
            if [[ -z "${backup_name}" ]]; then
                log_error "Backup name not specified"
                exit 1
            fi
            restore_config "${backup_name}"
            ;;
        *)
            echo "Usage: $0 [COMMAND] [OPTIONS]"
            echo ""
            echo "Commands:"
            echo "  generate                Generate configuration for environment"
            echo "  validate FILE [SCHEMA]  Validate configuration file"
            echo "  compare ENV1 ENV2 FILE  Compare configuration between environments"
            echo "  sync PATH REMOTE MODE   Sync configuration to/from remote store"
            echo "  encrypt FILE [OUTPUT]   Encrypt sensitive configuration file"
            echo "  decrypt FILE [OUTPUT]   Decrypt sensitive configuration file"
            echo "  backup [NAME]           Create configuration backup"
            echo "  restore NAME            Restore configuration from backup"
            echo ""
            echo "Environment Variables:"
            echo "  ENVIRONMENT             Target environment (default: development)"
            echo "  CONFIG_DIR              Configuration directory (default: config)"
            echo "  OUTPUT_DIR              Output directory (default: generated-config)"
            echo "  VAULT_ADDR              HashiCorp Vault address"
            echo "  VAULT_TOKEN             HashiCorp Vault token"
            echo "  ENCRYPTION_KEY          Encryption key for sensitive files"
            echo "  DRY_RUN                 Show what would be done (default: false)"
            exit 1
            ;;
    esac
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --environment)
            ENVIRONMENT="$2"
            shift 2
            ;;
        --config-dir)
            CONFIG_DIR="$2"
            shift 2
            ;;
        --output-dir)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        --vault-addr)
            VAULT_ADDR="$2"
            shift 2
            ;;
        --vault-token)
            VAULT_TOKEN="$2"
            shift 2
            ;;
        --encryption-key)
            ENCRYPTION_KEY="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN="true"
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