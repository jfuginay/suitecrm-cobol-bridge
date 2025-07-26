#!/bin/bash

# SuiteCRM COBOL Bridge Database Migration Runner
# This script manages database migrations for the COBOL Bridge system

set -e

# Database connection parameters
DB_HOST="${DB_HOST:-mysql}"
DB_PORT="${DB_PORT:-3306}"
DB_NAME="${DB_NAME:-suitecrm}"
DB_USER="${DB_USER:-suitecrm}"
DB_PASSWORD="${DB_PASSWORD:-suitecrm123}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Migration directory
MIGRATION_DIR="$(dirname "$0")"

# Function to print colored output
log() {
    echo -e "${2}${1}${NC}"
}

# Function to execute SQL
execute_sql() {
    mysql -h"$DB_HOST" -P"$DB_PORT" -u"$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" -e "$1"
}

# Function to execute SQL file
execute_sql_file() {
    mysql -h"$DB_HOST" -P"$DB_PORT" -u"$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" < "$1"
}

# Function to check if migrations table exists
check_migrations_table() {
    result=$(mysql -h"$DB_HOST" -P"$DB_PORT" -u"$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" -e "SHOW TABLES LIKE 'schema_migrations';" -s 2>/dev/null || echo "")
    if [ -z "$result" ]; then
        return 1
    else
        return 0
    fi
}

# Function to get applied migrations
get_applied_migrations() {
    mysql -h"$DB_HOST" -P"$DB_PORT" -u"$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" \
        -e "SELECT version FROM schema_migrations ORDER BY version;" -s 2>/dev/null || echo ""
}

# Function to calculate file checksum
calculate_checksum() {
    if command -v sha256sum > /dev/null; then
        sha256sum "$1" | cut -d' ' -f1
    else
        shasum -a 256 "$1" | cut -d' ' -f1
    fi
}

# Main migration logic
main() {
    log "SuiteCRM COBOL Bridge Database Migration Runner" "$GREEN"
    log "=============================================" "$GREEN"
    
    # Check database connection
    log "\nChecking database connection..." "$YELLOW"
    if ! mysql -h"$DB_HOST" -P"$DB_PORT" -u"$DB_USER" -p"$DB_PASSWORD" -e "SELECT 1;" >/dev/null 2>&1; then
        log "ERROR: Cannot connect to database!" "$RED"
        exit 1
    fi
    log "Database connection successful!" "$GREEN"
    
    # Create migrations table if it doesn't exist
    if ! check_migrations_table; then
        log "\nCreating schema_migrations table..." "$YELLOW"
        execute_sql "CREATE TABLE schema_migrations (
            version VARCHAR(20) PRIMARY KEY,
            migration_name VARCHAR(255) NOT NULL,
            applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
            execution_time_ms INT UNSIGNED,
            checksum VARCHAR(64)
        ) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;"
        log "Schema migrations table created!" "$GREEN"
    fi
    
    # Get list of applied migrations
    applied_migrations=$(get_applied_migrations)
    
    # Find migration files
    log "\nScanning for migration files..." "$YELLOW"
    migration_count=0
    applied_count=0
    
    for migration_file in "$MIGRATION_DIR"/*.sql; do
        if [ -f "$migration_file" ]; then
            filename=$(basename "$migration_file")
            version=$(echo "$filename" | cut -d'_' -f1)
            
            # Skip if not a numbered migration
            if ! [[ "$version" =~ ^[0-9]+$ ]]; then
                continue
            fi
            
            migration_count=$((migration_count + 1))
            
            # Check if already applied
            if echo "$applied_migrations" | grep -q "^$version$"; then
                log "  ✓ $filename (already applied)" "$GREEN"
                applied_count=$((applied_count + 1))
            else
                log "\nApplying migration: $filename" "$YELLOW"
                
                # Calculate checksum
                checksum=$(calculate_checksum "$migration_file")
                
                # Record start time
                start_time=$(date +%s%3N)
                
                # Apply migration
                if execute_sql_file "$migration_file"; then
                    # Calculate execution time
                    end_time=$(date +%s%3N)
                    execution_time=$((end_time - start_time))
                    
                    # Record successful migration
                    execute_sql "INSERT INTO schema_migrations (version, migration_name, execution_time_ms, checksum) 
                                VALUES ('$version', '$filename', $execution_time, '$checksum');"
                    
                    log "  ✓ Migration $filename applied successfully! (${execution_time}ms)" "$GREEN"
                    applied_count=$((applied_count + 1))
                else
                    log "  ✗ ERROR: Failed to apply migration $filename!" "$RED"
                    exit 1
                fi
            fi
        fi
    done
    
    log "\nMigration Summary:" "$GREEN"
    log "  Total migrations: $migration_count" "$GREEN"
    log "  Applied migrations: $applied_count" "$GREEN"
    
    if [ "$migration_count" -eq "$applied_count" ]; then
        log "\nDatabase is up to date!" "$GREEN"
    fi
}

# Handle command line arguments
case "${1:-up}" in
    up)
        main
        ;;
    status)
        log "Applied Migrations:" "$GREEN"
        if check_migrations_table; then
            mysql -h"$DB_HOST" -P"$DB_PORT" -u"$DB_USER" -p"$DB_PASSWORD" "$DB_NAME" \
                -e "SELECT version, migration_name, applied_at, execution_time_ms 
                    FROM schema_migrations ORDER BY version;" --table
        else
            log "No migrations table found. Run './migrate.sh up' to initialize." "$YELLOW"
        fi
        ;;
    *)
        log "Usage: $0 [up|status]" "$YELLOW"
        log "  up     - Apply pending migrations" "$YELLOW"
        log "  status - Show migration status" "$YELLOW"
        exit 1
        ;;
esac