# SuiteCRM COBOL Bridge Database Schema Documentation

## Overview

The SuiteCRM COBOL Bridge database schema is designed to support enterprise-grade integration between legacy COBOL systems and modern web applications. The schema follows best practices for performance, scalability, and maintainability.

## Database Design Principles

1. **Normalization**: Tables are normalized to 3NF to minimize redundancy
2. **Performance**: Strategic indexes and partitioning for high-volume tables
3. **Scalability**: Partitioned tables for time-series data
4. **Audit Trail**: Comprehensive logging and versioning
5. **Redis Integration**: Schema designed to work with Redis caching layer

## Core Database Tables

### 1. COBOL Program Management

#### `cobol_programs`
Central registry for all COBOL programs in the system.

**Key Fields:**
- `program_name`: Unique identifier for the COBOL program
- `program_type`: Classification (BATCH, ONLINE, REPORT, UTILITY)
- `status`: Current state of the program
- `metadata`: JSON field for flexible program metadata

**Indexes:**
- `idx_program_name`: Fast lookups by program name
- `idx_status`: Filter active/inactive programs
- `idx_program_type`: Group by program type

#### `cobol_program_parameters`
Defines input/output parameters for each COBOL program.

**Key Fields:**
- `parameter_type`: INPUT, OUTPUT, or INOUT
- `data_type`: COBOL PIC clause definition
- `validation_rules`: JSON-based validation configuration

### 2. Business Rules Engine

#### `business_rules`
Stores extracted and custom business rules.

**Key Fields:**
- `rule_definition`: JSON representation of rule logic
- `conditions`: Structured conditions for rule evaluation
- `actions`: Actions to take when rule matches
- `effective_date`/`expiry_date`: Time-based rule activation

**Features:**
- Version control through `business_rule_versions`
- Priority-based execution
- Tag-based categorization

### 3. Batch Job Management

#### `batch_jobs`
Defines batch job configurations and schedules.

**Key Fields:**
- `schedule_expression`: Cron format for scheduled jobs
- `timeout_seconds`: Maximum execution time
- `notification_config`: JSON configuration for alerts

#### `batch_job_executions`
Tracks individual job runs with real-time progress.

**Key Fields:**
- `execution_id`: UUID for distributed tracking
- `progress_percentage`: Real-time progress indicator
- `current_step`: Human-readable current operation

**Features:**
- Pause/resume capability
- Progress tracking
- Error recovery

### 4. Performance Monitoring

#### `execution_history`
Complete audit trail of all program executions.

**Partitioning Strategy:**
- Partitioned by quarter for optimal query performance
- Automatic partition management
- 90-day default retention

#### `performance_metrics`
Aggregated daily performance statistics.

**Key Metrics:**
- Execution counts and success rates
- Duration percentiles (P50, P95, P99)
- Resource usage tracking

### 5. Debug and Troubleshooting

#### `debug_sessions`
Manages time-travel debugging sessions.

**Features:**
- Multiple debug modes (STEP, BREAKPOINT, WATCH, REPLAY)
- Variable state tracking
- Breakpoint management

#### `debug_traces`
Detailed execution traces with microsecond precision.

**Key Fields:**
- `sequence_number`: Maintains trace order
- `variable_snapshots`: JSON snapshot of all variables
- `call_stack`: Complete call hierarchy

### 6. API and Security

#### `api_keys`
API key management with granular permissions.

**Security Features:**
- Hashed secrets
- Time-based validity
- Rate limiting per key
- Permission-based access control

#### `api_access_logs`
Comprehensive API usage logging.

**Partitioning:**
- Monthly partitions for scalability
- Automatic old partition cleanup

### 7. Cloud Burst Integration

#### `cloud_providers`
Cloud provider configurations for hybrid processing.

**Supported Providers:**
- AWS
- Azure
- Google Cloud Platform
- Private clouds

#### `cloud_burst_jobs`
Tracks jobs distributed to cloud providers.

**Features:**
- Cost tracking and estimation
- Performance comparison
- Automatic failover

### 8. Mobile API Support

#### `mobile_api_endpoints`
Auto-generated mobile endpoints from COBOL screens.

**Features:**
- Offline capability flags
- Field mapping configuration
- Platform-specific optimizations

## Performance Optimizations

### Indexes
Strategic indexes are placed on:
- Foreign key relationships
- Frequently queried columns
- Time-based queries
- Composite indexes for complex queries

### Partitioning
Large tables are partitioned by date:
- `execution_history`: Quarterly partitions
- `api_access_logs`: Monthly partitions
- `audit_logs`: Quarterly partitions

### Caching Strategy

The schema is designed to work with Redis caching:

```
Cache Keys:
- cobol:program:{program_name} - TTL: 3600s
- batch:active:{job_id} - TTL: 60s
- metrics:program:{program_id}:{date} - TTL: 300s
```

## Migration Strategy

### Version Control
All schema changes are tracked through migrations:
- Sequential numbering (001, 002, etc.)
- Rollback capability
- Checksum verification

### Migration Files
```
migrations/
├── 001_initial_schema.sql
├── 002_add_monitoring_indexes.sql
├── 003_add_ai_features.sql
├── 004_add_notification_system.sql
└── 005_add_data_archival.sql
```

## Data Retention

### Default Retention Policies
- Execution History: 90 days
- API Logs: 30 days  
- Debug Traces: 7 days
- Audit Logs: 365 days
- Performance Metrics: 90 days

### Archival Strategy
- Automatic archival to S3/cloud storage
- Compressed archives for space efficiency
- Configurable retention per table

## Security Considerations

1. **Encryption**: Sensitive fields marked for encryption
2. **Audit Trail**: Complete audit logging for compliance
3. **Access Control**: Granular permissions at API level
4. **Data Privacy**: PII fields identified and protected

## Monitoring and Maintenance

### Health Checks
Regular monitoring queries:
```sql
-- Check active batch jobs
SELECT * FROM v_active_batch_jobs;

-- Monitor program performance
SELECT * FROM v_program_performance 
WHERE success_rate < 95;
```

### Maintenance Tasks
1. Daily: Update performance metrics
2. Weekly: Analyze slow queries
3. Monthly: Partition maintenance
4. Quarterly: Archive old data

## Best Practices

1. **Always use prepared statements** to prevent SQL injection
2. **Leverage stored procedures** for complex operations
3. **Monitor index usage** and optimize as needed
4. **Use transactions** for data consistency
5. **Implement retry logic** for transient failures

## Future Enhancements

1. **Multi-tenant support**: Add tenant isolation
2. **Graph relationships**: Track program dependencies
3. **ML integration**: Store model predictions
4. **Event sourcing**: Complete state reconstruction
5. **Blockchain audit**: Immutable audit trail