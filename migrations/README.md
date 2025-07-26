# Database Migrations

This directory contains database migration scripts for the SuiteCRM COBOL Bridge system.

## Overview

Migrations are SQL scripts that evolve the database schema over time. Each migration is numbered sequentially and tracked in the `schema_migrations` table.

## Migration Files

Current migrations:
- `001_initial_schema.sql` - Initial database schema
- `002_add_monitoring_indexes.sql` - Performance indexes for monitoring
- `003_add_ai_features.sql` - AI-powered code review tables
- `004_add_notification_system.sql` - Notification and alerting system
- `005_add_data_archival.sql` - Data retention and archival

## Running Migrations

### Using Docker Compose

When you start the system with `docker-compose up`, migrations are automatically applied via the MySQL init script.

### Manual Migration

Use the migration runner script:

```bash
# Apply pending migrations
./migrate.sh up

# Check migration status
./migrate.sh status
```

### Environment Variables

Configure database connection:
```bash
export DB_HOST=localhost
export DB_PORT=3306
export DB_NAME=suitecrm
export DB_USER=suitecrm
export DB_PASSWORD=suitecrm123
```

## Creating New Migrations

1. Create a new SQL file with the next sequential number:
   ```bash
   touch 006_your_migration_name.sql
   ```

2. Add your schema changes to the file

3. Include migration tracking at the end:
   ```sql
   INSERT INTO schema_migrations (version, migration_name) 
   VALUES ('006', '006_your_migration_name.sql');
   ```

## Best Practices

1. **Always test migrations** in a development environment first
2. **Make migrations idempotent** using `IF NOT EXISTS` clauses
3. **Include rollback instructions** in comments
4. **Keep migrations small** and focused on one change
5. **Never modify existing migrations** that have been applied

## Migration Naming Convention

```
XXX_description_of_change.sql
```

Where:
- `XXX` is a three-digit sequential number (001, 002, etc.)
- `description_of_change` describes what the migration does

## Rollback Strategy

While this system doesn't include automatic rollbacks, each migration should document its rollback procedure:

```sql
-- Migration: Add new column
ALTER TABLE cobol_programs ADD COLUMN new_field VARCHAR(100);

-- Rollback: Remove column
-- ALTER TABLE cobol_programs DROP COLUMN new_field;
```

## Schema Version Tracking

The `schema_migrations` table tracks:
- `version`: Migration number
- `migration_name`: Full filename
- `applied_at`: When the migration was applied
- `execution_time_ms`: How long it took
- `checksum`: SHA-256 hash of the migration file

## Troubleshooting

### Migration Fails

1. Check the error message
2. Fix the issue in the migration file
3. If partially applied, manually rollback changes
4. Re-run the migration

### Connection Issues

Verify database is running:
```bash
docker-compose ps mysql
```

Test connection:
```bash
mysql -h localhost -P 3306 -u suitecrm -p
```

### Permission Issues

Ensure the database user has appropriate permissions:
```sql
GRANT ALL PRIVILEGES ON suitecrm.* TO 'suitecrm'@'%';
```