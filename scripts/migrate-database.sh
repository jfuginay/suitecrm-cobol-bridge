#!/bin/bash

# Database Migration Automation Script
# This script handles database migrations for the SuiteCRM COBOL Bridge application

set -euo pipefail

# Configuration
ENVIRONMENT="${ENVIRONMENT:-development}"
DB_HOST="${DB_HOST:-localhost}"
DB_PORT="${DB_PORT:-3306}"
DB_NAME="${DB_NAME:-suitecrm}"
DB_USER="${DB_USER:-suitecrm}"
DB_PASSWORD="${DB_PASSWORD:-}"
MIGRATIONS_DIR="${MIGRATIONS_DIR:-migrations}"
BACKUP_DIR="${BACKUP_DIR:-backups}"
DRY_RUN="${DRY_RUN:-false}"
ROLLBACK_VERSION="${ROLLBACK_VERSION:-}"
FORCE="${FORCE:-false}"

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

# Function to check if mysql client is available
check_mysql_client() {
    if ! command -v mysql >/dev/null 2>&1; then
        log_error "MySQL client is not installed"
        exit 1
    fi
}

# Function to test database connection
test_db_connection() {
    log_info "Testing database connection..."
    
    if [[ -z "${DB_PASSWORD}" ]]; then
        log_error "Database password not provided"
        exit 1
    fi
    
    if mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" -e "SELECT 1;" "${DB_NAME}" >/dev/null 2>&1; then
        log_success "Database connection successful"
    else
        log_error "Failed to connect to database"
        exit 1
    fi
}

# Function to create migration tracking table
create_migration_table() {
    log_info "Creating migration tracking table..."
    
    local sql="
    CREATE TABLE IF NOT EXISTS migration_history (
        id INT AUTO_INCREMENT PRIMARY KEY,
        version VARCHAR(50) NOT NULL UNIQUE,
        filename VARCHAR(255) NOT NULL,
        applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        applied_by VARCHAR(100),
        checksum VARCHAR(64),
        execution_time_ms INT,
        success BOOLEAN DEFAULT TRUE,
        error_message TEXT,
        INDEX idx_version (version),
        INDEX idx_applied_at (applied_at)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
    "
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create migration tracking table"
        return 0
    fi
    
    if mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" -e "${sql}"; then
        log_success "Migration tracking table created/verified"
    else
        log_error "Failed to create migration tracking table"
        exit 1
    fi
}

# Function to get applied migrations
get_applied_migrations() {
    mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" \
        -s -N -e "SELECT version FROM migration_history WHERE success = TRUE ORDER BY version;" 2>/dev/null || echo ""
}

# Function to get available migrations
get_available_migrations() {
    if [[ -d "${MIGRATIONS_DIR}" ]]; then
        find "${MIGRATIONS_DIR}" -name "*.sql" -type f | sort | while read -r file; do
            basename "${file}" .sql
        done
    fi
}

# Function to calculate file checksum
calculate_checksum() {
    local file="$1"
    if [[ -f "${file}" ]]; then
        sha256sum "${file}" | cut -d' ' -f1
    else
        echo ""
    fi
}

# Function to create database backup
create_backup() {
    local backup_timestamp=$(date +%Y%m%d_%H%M%S)
    local backup_file="${BACKUP_DIR}/backup_${ENVIRONMENT}_${backup_timestamp}.sql"
    
    log_info "Creating database backup..."
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would create backup: ${backup_file}"
        return 0
    fi
    
    mkdir -p "${BACKUP_DIR}"
    
    if mysqldump -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" \
        --single-transaction --routines --triggers --add-drop-table \
        "${DB_NAME}" > "${backup_file}"; then
        
        # Compress backup
        gzip "${backup_file}"
        backup_file="${backup_file}.gz"
        
        log_success "Database backup created: ${backup_file}"
        echo "${backup_file}"
    else
        log_error "Failed to create database backup"
        exit 1
    fi
}

# Function to apply a single migration
apply_migration() {
    local migration_file="$1"
    local version="$2"
    local start_time=$(date +%s%3N)
    
    log_info "Applying migration: ${version}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would apply migration: ${version}"
        log_info "[DRY RUN] File: ${migration_file}"
        return 0
    fi
    
    local checksum=$(calculate_checksum "${migration_file}")
    local applied_by="${USER:-$(whoami)}"
    
    # Apply the migration
    if mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" < "${migration_file}"; then
        local end_time=$(date +%s%3N)
        local execution_time=$((end_time - start_time))
        
        # Record successful migration
        local record_sql="
        INSERT INTO migration_history (version, filename, applied_by, checksum, execution_time_ms, success)
        VALUES ('${version}', '$(basename "${migration_file}")', '${applied_by}', '${checksum}', ${execution_time}, TRUE);
        "
        
        mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" -e "${record_sql}"
        
        log_success "✓ Migration ${version} applied successfully (${execution_time}ms)"
    else
        local end_time=$(date +%s%3N)
        local execution_time=$((end_time - start_time))
        local error_msg="Migration failed during execution"
        
        # Record failed migration
        local record_sql="
        INSERT INTO migration_history (version, filename, applied_by, checksum, execution_time_ms, success, error_message)
        VALUES ('${version}', '$(basename "${migration_file}")', '${applied_by}', '${checksum}', ${execution_time}, FALSE, '${error_msg}');
        "
        
        mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" -e "${record_sql}" || true
        
        log_error "✗ Migration ${version} failed"
        return 1
    fi
}

# Function to run migrations
run_migrations() {
    log_info "Starting database migrations for environment: ${ENVIRONMENT}"
    
    # Get current state
    local applied_migrations=($(get_applied_migrations))
    local available_migrations=($(get_available_migrations))
    
    if [[ ${#available_migrations[@]} -eq 0 ]]; then
        log_warning "No migration files found in ${MIGRATIONS_DIR}"
        return 0
    fi
    
    log_info "Applied migrations: ${#applied_migrations[@]}"
    log_info "Available migrations: ${#available_migrations[@]}"
    
    # Find pending migrations
    local pending_migrations=()
    for migration in "${available_migrations[@]}"; do
        local is_applied=false
        for applied in "${applied_migrations[@]}"; do
            if [[ "${migration}" == "${applied}" ]]; then
                is_applied=true
                break
            fi
        done
        
        if [[ "${is_applied}" == "false" ]]; then
            pending_migrations+=("${migration}")
        fi
    done
    
    if [[ ${#pending_migrations[@]} -eq 0 ]]; then
        log_success "Database is up to date. No migrations to apply."
        return 0
    fi
    
    log_info "Pending migrations: ${#pending_migrations[@]}"
    for migration in "${pending_migrations[@]}"; do
        log_info "  - ${migration}"
    done
    
    # Create backup before applying migrations
    if [[ "${ENVIRONMENT}" == "production" ]] || [[ "${FORCE}" == "true" ]]; then
        create_backup
    fi
    
    # Apply pending migrations
    local success_count=0
    local failure_count=0
    
    for migration in "${pending_migrations[@]}"; do
        local migration_file="${MIGRATIONS_DIR}/${migration}.sql"
        
        if [[ ! -f "${migration_file}" ]]; then
            log_error "Migration file not found: ${migration_file}"
            ((failure_count++))
            continue
        fi
        
        # Validate migration file
        if ! validate_migration_file "${migration_file}"; then
            log_error "Migration validation failed: ${migration}"
            ((failure_count++))
            continue
        fi
        
        if apply_migration "${migration_file}" "${migration}"; then
            ((success_count++))
        else
            ((failure_count++))
            
            if [[ "${FORCE}" != "true" ]]; then
                log_error "Migration failed. Stopping execution. Use --force to continue on errors."
                break
            fi
        fi
    done
    
    log_info "Migration summary:"
    log_info "  Successful: ${success_count}"
    log_info "  Failed: ${failure_count}"
    
    if [[ ${failure_count} -gt 0 ]]; then
        log_error "Some migrations failed"
        return 1
    else
        log_success "All migrations applied successfully"
        return 0
    fi
}

# Function to validate migration file
validate_migration_file() {
    local file="$1"
    
    # Check file size (not empty, not too large)
    if [[ ! -s "${file}" ]]; then
        log_error "Migration file is empty: ${file}"
        return 1
    fi
    
    local file_size=$(stat -f%z "${file}" 2>/dev/null || stat -c%s "${file}" 2>/dev/null || echo 0)
    if [[ ${file_size} -gt 104857600 ]]; then  # 100MB limit
        log_error "Migration file too large (>100MB): ${file}"
        return 1
    fi
    
    # Check for dangerous SQL patterns
    local dangerous_patterns=(
        "DROP DATABASE"
        "DROP SCHEMA"
        "TRUNCATE TABLE.*migration_history"
        "DELETE FROM.*migration_history"
    )
    
    for pattern in "${dangerous_patterns[@]}"; do
        if grep -i "${pattern}" "${file}" >/dev/null 2>&1; then
            log_error "Dangerous SQL pattern detected in ${file}: ${pattern}"
            return 1
        fi
    done
    
    # Check SQL syntax (basic)
    if ! mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" \
        --execute="SET sql_mode = 'STRICT_TRANS_TABLES'; $(cat "${file}")" \
        --dry-run "${DB_NAME}" >/dev/null 2>&1; then
        
        log_warning "SQL syntax validation failed for ${file} (may be a false positive)"
        if [[ "${FORCE}" != "true" ]]; then
            return 1
        fi
    fi
    
    return 0
}

# Function to show migration status
show_status() {
    log_info "Migration Status for ${ENVIRONMENT}"
    log_info "Database: ${DB_HOST}:${DB_PORT}/${DB_NAME}"
    
    local applied_migrations=($(get_applied_migrations))
    local available_migrations=($(get_available_migrations))
    
    echo ""
    echo "┌─────────────────────────────────────────────┬──────────┬─────────────────────┐"
    echo "│ Migration                                   │ Status   │ Applied At          │"
    echo "├─────────────────────────────────────────────┼──────────┼─────────────────────┤"
    
    for migration in "${available_migrations[@]}"; do
        local status="Pending"
        local applied_at="-"
        
        for applied in "${applied_migrations[@]}"; do
            if [[ "${migration}" == "${applied}" ]]; then
                status="Applied"
                applied_at=$(mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" \
                    -s -N -e "SELECT applied_at FROM migration_history WHERE version = '${migration}' AND success = TRUE;" 2>/dev/null || echo "-")
                break
            fi
        done
        
        printf "│ %-43s │ %-8s │ %-19s │\n" "${migration}" "${status}" "${applied_at}"
    done
    
    echo "└─────────────────────────────────────────────┴──────────┴─────────────────────┘"
    echo ""
    echo "Summary:"
    echo "  Total migrations: ${#available_migrations[@]}"
    echo "  Applied: ${#applied_migrations[@]}"
    echo "  Pending: $((${#available_migrations[@]} - ${#applied_migrations[@]}))"
}

# Function to rollback to a specific version
rollback_migration() {
    local target_version="$1"
    
    log_info "Rolling back database to version: ${target_version}"
    
    if [[ "${DRY_RUN}" == "true" ]]; then
        log_info "[DRY RUN] Would rollback to version: ${target_version}"
        return 0
    fi
    
    # Get applied migrations after target version
    local migrations_to_rollback=()
    readarray -t migrations_to_rollback < <(
        mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" \
            -s -N -e "SELECT version FROM migration_history WHERE version > '${target_version}' AND success = TRUE ORDER BY version DESC;"
    )
    
    if [[ ${#migrations_to_rollback[@]} -eq 0 ]]; then
        log_info "No migrations to rollback"
        return 0
    fi
    
    log_warning "This will rollback ${#migrations_to_rollback[@]} migrations:"
    for migration in "${migrations_to_rollback[@]}"; do
        log_warning "  - ${migration}"
    done
    
    if [[ "${FORCE}" != "true" ]]; then
        read -p "Are you sure you want to continue? (yes/no): " -r
        if [[ ! $REPLY =~ ^[Yy][Ee][Ss]$ ]]; then
            log_info "Rollback cancelled"
            return 0
        fi
    fi
    
    # Create backup before rollback
    create_backup
    
    # Check for rollback scripts
    local rollback_count=0
    for migration in "${migrations_to_rollback[@]}"; do
        local rollback_file="${MIGRATIONS_DIR}/rollback_${migration}.sql"
        
        if [[ -f "${rollback_file}" ]]; then
            log_info "Applying rollback: ${migration}"
            
            if mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" < "${rollback_file}"; then
                # Mark as rolled back in history
                mysql -h"${DB_HOST}" -P"${DB_PORT}" -u"${DB_USER}" -p"${DB_PASSWORD}" "${DB_NAME}" \
                    -e "UPDATE migration_history SET success = FALSE WHERE version = '${migration}';"
                
                log_success "✓ Rollback ${migration} completed"
                ((rollback_count++))
            else
                log_error "✗ Rollback ${migration} failed"
                return 1
            fi
        else
            log_error "Rollback script not found: ${rollback_file}"
            log_error "Manual rollback required for migration: ${migration}"
            return 1
        fi
    done
    
    log_success "Rollback completed. ${rollback_count} migrations rolled back."
}

# Function to generate migration template
generate_migration() {
    local description="$1"
    local timestamp=$(date +%Y%m%d_%H%M%S)
    local version="${timestamp}_$(echo "${description}" | tr ' ' '_' | tr '[:upper:]' '[:lower:]')"
    local migration_file="${MIGRATIONS_DIR}/${version}.sql"
    local rollback_file="${MIGRATIONS_DIR}/rollback_${version}.sql"
    
    mkdir -p "${MIGRATIONS_DIR}"
    
    cat > "${migration_file}" << EOF
-- Migration: ${description}
-- Version: ${version}
-- Created: $(date)
-- Author: ${USER:-$(whoami)}

-- WARNING: This migration will be applied automatically.
-- Make sure to test it thoroughly before committing.

-- Add your migration SQL here
-- Example:
-- CREATE TABLE example_table (
--     id INT AUTO_INCREMENT PRIMARY KEY,
--     name VARCHAR(255) NOT NULL,
--     created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
-- );

-- ALTER TABLE existing_table ADD COLUMN new_column VARCHAR(100);

-- INSERT INTO configuration (key, value) VALUES ('new_setting', 'default_value');
EOF

    cat > "${rollback_file}" << EOF
-- Rollback Migration: ${description}
-- Version: ${version}
-- Created: $(date)
-- Author: ${USER:-$(whoami)}

-- WARNING: This rollback will be applied automatically during rollback operations.
-- Make sure it properly reverses the changes made in the forward migration.

-- Add your rollback SQL here
-- Example (reverse of the migration above):
-- DROP TABLE IF EXISTS example_table;

-- ALTER TABLE existing_table DROP COLUMN new_column;

-- DELETE FROM configuration WHERE key = 'new_setting';
EOF
    
    log_success "Migration files created:"
    log_success "  Forward: ${migration_file}"
    log_success "  Rollback: ${rollback_file}"
    
    echo ""
    log_info "Next steps:"
    log_info "1. Edit the migration files with your SQL changes"
    log_info "2. Test the migration on a development database"
    log_info "3. Commit the files to version control"
    log_info "4. Deploy using: $0 --environment=<env>"
}

# Function to clean up old backups
cleanup_backups() {
    local days="${1:-30}"
    
    log_info "Cleaning up backups older than ${days} days..."
    
    if [[ ! -d "${BACKUP_DIR}" ]]; then
        log_info "No backup directory found"
        return 0
    fi
    
    local deleted_count=0
    while IFS= read -r -d '' file; do
        rm -f "${file}"
        ((deleted_count++))
        log_info "Deleted old backup: $(basename "${file}")"
    done < <(find "${BACKUP_DIR}" -name "backup_*.sql.gz" -type f -mtime +${days} -print0 2>/dev/null)
    
    log_success "Cleaned up ${deleted_count} old backup files"
}

# Main function
main() {
    case "${1:-migrate}" in
        migrate)
            check_mysql_client
            test_db_connection
            create_migration_table
            run_migrations
            ;;
        status)
            check_mysql_client
            test_db_connection
            create_migration_table
            show_status
            ;;
        rollback)
            if [[ -z "${ROLLBACK_VERSION}" ]]; then
                log_error "Rollback version not specified. Use --rollback-version=<version>"
                exit 1
            fi
            check_mysql_client
            test_db_connection
            create_migration_table
            rollback_migration "${ROLLBACK_VERSION}"
            ;;
        generate)
            if [[ -z "${2:-}" ]]; then
                log_error "Migration description required. Usage: $0 generate 'description'"
                exit 1
            fi
            generate_migration "$2"
            ;;
        backup)
            check_mysql_client
            test_db_connection
            create_backup
            ;;
        cleanup)
            cleanup_backups "${2:-30}"
            ;;
        *)
            echo "Usage: $0 [COMMAND] [OPTIONS]"
            echo ""
            echo "Commands:"
            echo "  migrate              Apply pending migrations (default)"
            echo "  status               Show migration status"
            echo "  rollback             Rollback to specified version"
            echo "  generate 'desc'      Generate new migration template"
            echo "  backup               Create database backup"
            echo "  cleanup [days]       Clean up old backups (default: 30 days)"
            echo ""
            echo "Options:"
            echo "  --environment ENV         Target environment (default: development)"
            echo "  --db-host HOST           Database host (default: localhost)"
            echo "  --db-port PORT           Database port (default: 3306)"
            echo "  --db-name NAME           Database name (default: suitecrm)"
            echo "  --db-user USER           Database user (default: suitecrm)"
            echo "  --db-password PASS       Database password"
            echo "  --migrations-dir DIR     Migrations directory (default: migrations)"
            echo "  --backup-dir DIR         Backup directory (default: backups)"
            echo "  --dry-run                Show what would be done"
            echo "  --rollback-version VER   Version to rollback to"
            echo "  --force                  Continue on errors/skip confirmations"
            echo ""
            echo "Environment Variables:"
            echo "  DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD"
            echo "  ENVIRONMENT, MIGRATIONS_DIR, BACKUP_DIR"
            exit 1
            ;;
    esac
}

# Parse command line arguments
COMMAND="${1:-migrate}"
shift || true

while [[ $# -gt 0 ]]; do
    case $1 in
        --environment)
            ENVIRONMENT="$2"
            shift 2
            ;;
        --db-host)
            DB_HOST="$2"
            shift 2
            ;;
        --db-port)
            DB_PORT="$2"
            shift 2
            ;;
        --db-name)
            DB_NAME="$2"
            shift 2
            ;;
        --db-user)
            DB_USER="$2"
            shift 2
            ;;
        --db-password)
            DB_PASSWORD="$2"
            shift 2
            ;;
        --migrations-dir)
            MIGRATIONS_DIR="$2"
            shift 2
            ;;
        --backup-dir)
            BACKUP_DIR="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --rollback-version)
            ROLLBACK_VERSION="$2"
            shift 2
            ;;
        --force)
            FORCE="true"
            shift
            ;;
        *)
            # Skip unknown options or pass them as arguments
            shift
            ;;
    esac
done

# Run main function with command
main "${COMMAND}"