-- SuiteCRM COBOL Bridge Database Schema
-- Version: 1.0.0
-- Description: Complete database schema for COBOL Bridge integration with SuiteCRM
-- Author: COBOL Bridge Team
-- Date: 2025-01-26

-- Create database if not exists
CREATE DATABASE IF NOT EXISTS suitecrm CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;
USE suitecrm;

-- =====================================================
-- COBOL Programs Registry and Metadata
-- =====================================================

-- Table: cobol_programs
-- Description: Registry of all COBOL programs managed by the bridge
CREATE TABLE IF NOT EXISTS cobol_programs (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    program_name VARCHAR(100) NOT NULL UNIQUE,
    program_type ENUM('BATCH', 'ONLINE', 'REPORT', 'UTILITY') NOT NULL DEFAULT 'BATCH',
    source_path VARCHAR(500) NOT NULL,
    compiled_path VARCHAR(500),
    description TEXT,
    version VARCHAR(20) NOT NULL DEFAULT '1.0.0',
    status ENUM('ACTIVE', 'INACTIVE', 'DEPRECATED', 'COMPILING', 'ERROR') NOT NULL DEFAULT 'INACTIVE',
    compile_options JSON,
    metadata JSON COMMENT 'Additional program metadata (copybooks, dependencies, etc)',
    last_compiled_at TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by INT UNSIGNED,
    INDEX idx_program_name (program_name),
    INDEX idx_status (status),
    INDEX idx_program_type (program_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: cobol_program_parameters
-- Description: Input/output parameters for COBOL programs
CREATE TABLE IF NOT EXISTS cobol_program_parameters (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    program_id INT UNSIGNED NOT NULL,
    parameter_name VARCHAR(100) NOT NULL,
    parameter_type ENUM('INPUT', 'OUTPUT', 'INOUT') NOT NULL,
    data_type VARCHAR(50) NOT NULL COMMENT 'COBOL data type (PIC clause)',
    position INT UNSIGNED NOT NULL,
    is_required BOOLEAN DEFAULT TRUE,
    default_value VARCHAR(500),
    validation_rules JSON,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    UNIQUE KEY uk_program_parameter (program_id, parameter_name),
    INDEX idx_program_params (program_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Business Rules Storage and Versioning
-- =====================================================

-- Table: business_rules
-- Description: Business rules extracted from COBOL programs
CREATE TABLE IF NOT EXISTS business_rules (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    rule_name VARCHAR(200) NOT NULL,
    rule_type ENUM('DECISION', 'VALIDATION', 'CALCULATION', 'WORKFLOW') NOT NULL,
    program_id INT UNSIGNED,
    rule_definition JSON NOT NULL COMMENT 'JSON representation of the rule logic',
    rule_expression TEXT COMMENT 'Human-readable rule expression',
    conditions JSON COMMENT 'Rule conditions in structured format',
    actions JSON COMMENT 'Rule actions in structured format',
    priority INT DEFAULT 100,
    is_active BOOLEAN DEFAULT TRUE,
    effective_date DATE,
    expiry_date DATE,
    tags JSON COMMENT 'Tags for categorization',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by INT UNSIGNED,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE SET NULL,
    INDEX idx_rule_name (rule_name),
    INDEX idx_rule_type (rule_type),
    INDEX idx_active_effective (is_active, effective_date, expiry_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: business_rule_versions
-- Description: Version history for business rules
CREATE TABLE IF NOT EXISTS business_rule_versions (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    rule_id INT UNSIGNED NOT NULL,
    version_number INT UNSIGNED NOT NULL,
    rule_definition JSON NOT NULL,
    change_summary TEXT,
    changed_by INT UNSIGNED,
    approved_by INT UNSIGNED,
    approval_date TIMESTAMP NULL,
    is_current BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (rule_id) REFERENCES business_rules(id) ON DELETE CASCADE,
    UNIQUE KEY uk_rule_version (rule_id, version_number),
    INDEX idx_rule_versions (rule_id, is_current)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Batch Job Tracking with Progress
-- =====================================================

-- Table: batch_jobs
-- Description: Batch job definitions and schedules
CREATE TABLE IF NOT EXISTS batch_jobs (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    job_name VARCHAR(100) NOT NULL UNIQUE,
    program_id INT UNSIGNED NOT NULL,
    job_type ENUM('SCHEDULED', 'ON_DEMAND', 'TRIGGERED') NOT NULL DEFAULT 'ON_DEMAND',
    schedule_expression VARCHAR(100) COMMENT 'Cron expression for scheduled jobs',
    input_parameters JSON,
    output_parameters JSON,
    timeout_seconds INT DEFAULT 3600,
    max_retries INT DEFAULT 3,
    retry_delay_seconds INT DEFAULT 300,
    is_active BOOLEAN DEFAULT TRUE,
    notification_config JSON COMMENT 'Email/webhook notification settings',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    INDEX idx_job_name (job_name),
    INDEX idx_job_type (job_type),
    INDEX idx_active_jobs (is_active)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: batch_job_executions
-- Description: Individual batch job execution tracking
CREATE TABLE IF NOT EXISTS batch_job_executions (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    job_id INT UNSIGNED NOT NULL,
    execution_id VARCHAR(36) NOT NULL UNIQUE COMMENT 'UUID for execution tracking',
    status ENUM('QUEUED', 'RUNNING', 'PAUSED', 'COMPLETED', 'FAILED', 'CANCELLED') NOT NULL DEFAULT 'QUEUED',
    progress_percentage DECIMAL(5,2) DEFAULT 0.00,
    current_step VARCHAR(200),
    total_steps INT UNSIGNED,
    completed_steps INT UNSIGNED DEFAULT 0,
    input_data JSON,
    output_data JSON,
    error_message TEXT,
    error_details JSON,
    started_at TIMESTAMP NULL,
    completed_at TIMESTAMP NULL,
    duration_seconds INT UNSIGNED GENERATED ALWAYS AS (TIMESTAMPDIFF(SECOND, started_at, completed_at)) STORED,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    created_by INT UNSIGNED,
    FOREIGN KEY (job_id) REFERENCES batch_jobs(id) ON DELETE CASCADE,
    INDEX idx_execution_id (execution_id),
    INDEX idx_job_status (job_id, status),
    INDEX idx_created_at (created_at),
    INDEX idx_status_created (status, created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: batch_job_progress
-- Description: Real-time progress updates for running jobs
CREATE TABLE IF NOT EXISTS batch_job_progress (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    execution_id VARCHAR(36) NOT NULL,
    timestamp TIMESTAMP(3) DEFAULT CURRENT_TIMESTAMP(3),
    progress_percentage DECIMAL(5,2) NOT NULL,
    current_record BIGINT UNSIGNED,
    total_records BIGINT UNSIGNED,
    records_processed BIGINT UNSIGNED,
    records_failed BIGINT UNSIGNED DEFAULT 0,
    current_operation VARCHAR(200),
    memory_usage_mb INT UNSIGNED,
    cpu_usage_percent DECIMAL(5,2),
    additional_metrics JSON,
    FOREIGN KEY (execution_id) REFERENCES batch_job_executions(execution_id) ON DELETE CASCADE,
    INDEX idx_execution_progress (execution_id, timestamp)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Execution History and Performance Metrics
-- =====================================================

-- Table: execution_history
-- Description: Complete history of all COBOL program executions
CREATE TABLE IF NOT EXISTS execution_history (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    program_id INT UNSIGNED NOT NULL,
    execution_id VARCHAR(36) NOT NULL UNIQUE,
    execution_type ENUM('DIRECT', 'BATCH', 'API', 'SCHEDULED') NOT NULL,
    start_time TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP(3),
    end_time TIMESTAMP(3) NULL,
    duration_ms INT UNSIGNED GENERATED ALWAYS AS (TIMESTAMPDIFF(MICROSECOND, start_time, end_time) / 1000) STORED,
    status ENUM('SUCCESS', 'FAILURE', 'TIMEOUT', 'CANCELLED') NOT NULL,
    input_parameters JSON,
    output_results JSON,
    error_code VARCHAR(50),
    error_message TEXT,
    user_id INT UNSIGNED,
    client_ip VARCHAR(45),
    request_source VARCHAR(100),
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    INDEX idx_program_history (program_id, start_time),
    INDEX idx_execution_time (start_time),
    INDEX idx_status_time (status, start_time)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
PARTITION BY RANGE (TO_DAYS(start_time)) (
    PARTITION p_history_2024 VALUES LESS THAN (TO_DAYS('2025-01-01')),
    PARTITION p_history_2025_q1 VALUES LESS THAN (TO_DAYS('2025-04-01')),
    PARTITION p_history_2025_q2 VALUES LESS THAN (TO_DAYS('2025-07-01')),
    PARTITION p_history_2025_q3 VALUES LESS THAN (TO_DAYS('2025-10-01')),
    PARTITION p_history_2025_q4 VALUES LESS THAN (TO_DAYS('2026-01-01')),
    PARTITION p_history_future VALUES LESS THAN MAXVALUE
);

-- Table: performance_metrics
-- Description: Aggregated performance metrics for programs
CREATE TABLE IF NOT EXISTS performance_metrics (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    program_id INT UNSIGNED NOT NULL,
    metric_date DATE NOT NULL,
    execution_count INT UNSIGNED DEFAULT 0,
    success_count INT UNSIGNED DEFAULT 0,
    failure_count INT UNSIGNED DEFAULT 0,
    avg_duration_ms DECIMAL(10,2),
    min_duration_ms INT UNSIGNED,
    max_duration_ms INT UNSIGNED,
    p50_duration_ms INT UNSIGNED,
    p95_duration_ms INT UNSIGNED,
    p99_duration_ms INT UNSIGNED,
    total_cpu_seconds DECIMAL(10,2),
    total_memory_mb BIGINT UNSIGNED,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    UNIQUE KEY uk_program_date (program_id, metric_date),
    INDEX idx_metric_date (metric_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Debug Traces and Timeline Data
-- =====================================================

-- Table: debug_sessions
-- Description: Debug session management
CREATE TABLE IF NOT EXISTS debug_sessions (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    session_id VARCHAR(36) NOT NULL UNIQUE,
    execution_id VARCHAR(36) NOT NULL,
    program_id INT UNSIGNED NOT NULL,
    debug_mode ENUM('STEP', 'BREAKPOINT', 'WATCH', 'REPLAY') NOT NULL,
    status ENUM('ACTIVE', 'PAUSED', 'COMPLETED', 'TERMINATED') NOT NULL DEFAULT 'ACTIVE',
    breakpoints JSON,
    watch_variables JSON,
    started_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    ended_at TIMESTAMP NULL,
    created_by INT UNSIGNED,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    INDEX idx_session_id (session_id),
    INDEX idx_execution_debug (execution_id),
    INDEX idx_status_time (status, started_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: debug_traces
-- Description: Detailed execution traces for debugging
CREATE TABLE IF NOT EXISTS debug_traces (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    session_id VARCHAR(36) NOT NULL,
    sequence_number BIGINT UNSIGNED NOT NULL,
    timestamp TIMESTAMP(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    trace_type ENUM('STATEMENT', 'VARIABLE', 'CALL', 'RETURN', 'EXCEPTION') NOT NULL,
    source_line INT UNSIGNED,
    source_file VARCHAR(200),
    statement_text TEXT,
    variable_snapshots JSON COMMENT 'Variable values at this point',
    call_stack JSON,
    memory_snapshot JSON,
    additional_data JSON,
    FOREIGN KEY (session_id) REFERENCES debug_sessions(session_id) ON DELETE CASCADE,
    INDEX idx_session_traces (session_id, sequence_number),
    INDEX idx_trace_time (timestamp)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- API Keys and Authentication
-- =====================================================

-- Table: api_keys
-- Description: API key management for external access
CREATE TABLE IF NOT EXISTS api_keys (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    api_key VARCHAR(64) NOT NULL UNIQUE,
    api_secret_hash VARCHAR(255) NOT NULL,
    client_name VARCHAR(100) NOT NULL,
    client_type ENUM('WEB', 'MOBILE', 'SERVICE', 'INTEGRATION') NOT NULL,
    permissions JSON COMMENT 'Allowed operations and programs',
    rate_limit_per_minute INT DEFAULT 60,
    is_active BOOLEAN DEFAULT TRUE,
    valid_from TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    valid_until TIMESTAMP NULL,
    last_used_at TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    created_by INT UNSIGNED,
    INDEX idx_api_key (api_key),
    INDEX idx_active_keys (is_active, valid_from, valid_until)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: api_access_logs
-- Description: API access logging for security and analytics
CREATE TABLE IF NOT EXISTS api_access_logs (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    api_key_id INT UNSIGNED NOT NULL,
    request_timestamp TIMESTAMP(3) DEFAULT CURRENT_TIMESTAMP(3),
    request_method VARCHAR(10) NOT NULL,
    request_path VARCHAR(500) NOT NULL,
    request_params JSON,
    response_status INT NOT NULL,
    response_time_ms INT UNSIGNED,
    ip_address VARCHAR(45),
    user_agent VARCHAR(500),
    error_message TEXT,
    FOREIGN KEY (api_key_id) REFERENCES api_keys(id) ON DELETE CASCADE,
    INDEX idx_api_key_time (api_key_id, request_timestamp),
    INDEX idx_request_time (request_timestamp)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
PARTITION BY RANGE (TO_DAYS(request_timestamp)) (
    PARTITION p_logs_2024 VALUES LESS THAN (TO_DAYS('2025-01-01')),
    PARTITION p_logs_2025_q1 VALUES LESS THAN (TO_DAYS('2025-04-01')),
    PARTITION p_logs_2025_q2 VALUES LESS THAN (TO_DAYS('2025-07-01')),
    PARTITION p_logs_2025_q3 VALUES LESS THAN (TO_DAYS('2025-10-01')),
    PARTITION p_logs_2025_q4 VALUES LESS THAN (TO_DAYS('2026-01-01')),
    PARTITION p_logs_future VALUES LESS THAN MAXVALUE
);

-- =====================================================
-- Cloud Burst Job Distribution Tracking
-- =====================================================

-- Table: cloud_providers
-- Description: Cloud provider configurations
CREATE TABLE IF NOT EXISTS cloud_providers (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    provider_name VARCHAR(50) NOT NULL UNIQUE,
    provider_type ENUM('AWS', 'AZURE', 'GCP', 'PRIVATE') NOT NULL,
    region VARCHAR(50) NOT NULL,
    instance_types JSON COMMENT 'Available instance types and specs',
    pricing_info JSON,
    credentials_encrypted TEXT,
    max_instances INT DEFAULT 10,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_provider_type (provider_type),
    INDEX idx_active_providers (is_active)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: cloud_burst_jobs
-- Description: Jobs distributed to cloud providers
CREATE TABLE IF NOT EXISTS cloud_burst_jobs (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    job_execution_id VARCHAR(36) NOT NULL,
    provider_id INT UNSIGNED NOT NULL,
    instance_id VARCHAR(100),
    instance_type VARCHAR(50),
    status ENUM('PROVISIONING', 'RUNNING', 'COMPLETED', 'FAILED', 'TERMINATED') NOT NULL DEFAULT 'PROVISIONING',
    workload_percentage DECIMAL(5,2) COMMENT 'Percentage of total workload',
    start_time TIMESTAMP NULL,
    end_time TIMESTAMP NULL,
    cost_estimate DECIMAL(10,4),
    actual_cost DECIMAL(10,4),
    performance_metrics JSON,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (job_execution_id) REFERENCES batch_job_executions(execution_id) ON DELETE CASCADE,
    FOREIGN KEY (provider_id) REFERENCES cloud_providers(id) ON DELETE CASCADE,
    INDEX idx_execution_cloud (job_execution_id),
    INDEX idx_provider_jobs (provider_id, status)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Mobile API Usage Statistics
-- =====================================================

-- Table: mobile_api_endpoints
-- Description: Mobile-specific API endpoints generated from COBOL
CREATE TABLE IF NOT EXISTS mobile_api_endpoints (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    endpoint_path VARCHAR(200) NOT NULL UNIQUE,
    program_id INT UNSIGNED NOT NULL,
    screen_name VARCHAR(100),
    component_type ENUM('LIST', 'FORM', 'REPORT', 'DASHBOARD') NOT NULL,
    field_mappings JSON COMMENT 'COBOL to mobile field mappings',
    validation_rules JSON,
    offline_capable BOOLEAN DEFAULT FALSE,
    cache_duration_seconds INT DEFAULT 300,
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    INDEX idx_endpoint_path (endpoint_path),
    INDEX idx_active_endpoints (is_active)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: mobile_usage_stats
-- Description: Mobile API usage statistics
CREATE TABLE IF NOT EXISTS mobile_usage_stats (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    endpoint_id INT UNSIGNED NOT NULL,
    usage_date DATE NOT NULL,
    platform ENUM('IOS', 'ANDROID', 'WEB') NOT NULL,
    request_count INT UNSIGNED DEFAULT 0,
    unique_devices INT UNSIGNED DEFAULT 0,
    avg_response_time_ms DECIMAL(10,2),
    error_count INT UNSIGNED DEFAULT 0,
    offline_sync_count INT UNSIGNED DEFAULT 0,
    data_volume_mb DECIMAL(10,2),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (endpoint_id) REFERENCES mobile_api_endpoints(id) ON DELETE CASCADE,
    UNIQUE KEY uk_endpoint_date_platform (endpoint_id, usage_date, platform),
    INDEX idx_usage_date (usage_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- System Configuration and Settings
-- =====================================================

-- Table: system_settings
-- Description: System-wide configuration settings
CREATE TABLE IF NOT EXISTS system_settings (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    setting_key VARCHAR(100) NOT NULL UNIQUE,
    setting_value TEXT,
    setting_type ENUM('STRING', 'NUMBER', 'BOOLEAN', 'JSON') NOT NULL DEFAULT 'STRING',
    category VARCHAR(50) NOT NULL,
    description TEXT,
    is_encrypted BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_category (category)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- =====================================================
-- Audit Logging
-- =====================================================

-- Table: audit_logs
-- Description: Comprehensive audit trail for all system actions
CREATE TABLE IF NOT EXISTS audit_logs (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    action_type VARCHAR(50) NOT NULL,
    entity_type VARCHAR(50) NOT NULL,
    entity_id VARCHAR(50) NOT NULL,
    action_details JSON,
    old_values JSON,
    new_values JSON,
    user_id INT UNSIGNED,
    ip_address VARCHAR(45),
    user_agent VARCHAR(500),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    INDEX idx_entity (entity_type, entity_id),
    INDEX idx_user_time (user_id, created_at),
    INDEX idx_created_at (created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci
PARTITION BY RANGE (TO_DAYS(created_at)) (
    PARTITION p_audit_2024 VALUES LESS THAN (TO_DAYS('2025-01-01')),
    PARTITION p_audit_2025_q1 VALUES LESS THAN (TO_DAYS('2025-04-01')),
    PARTITION p_audit_2025_q2 VALUES LESS THAN (TO_DAYS('2025-07-01')),
    PARTITION p_audit_2025_q3 VALUES LESS THAN (TO_DAYS('2025-10-01')),
    PARTITION p_audit_2025_q4 VALUES LESS THAN (TO_DAYS('2026-01-01')),
    PARTITION p_audit_future VALUES LESS THAN MAXVALUE
);

-- =====================================================
-- Initial Data and Configuration
-- =====================================================

-- Insert default system settings
INSERT INTO system_settings (setting_key, setting_value, setting_type, category, description) VALUES
('batch.default_timeout', '3600', 'NUMBER', 'BATCH', 'Default batch job timeout in seconds'),
('batch.max_retries', '3', 'NUMBER', 'BATCH', 'Maximum retry attempts for failed batch jobs'),
('api.rate_limit_default', '60', 'NUMBER', 'API', 'Default API rate limit per minute'),
('cloud.burst_threshold', '80', 'NUMBER', 'CLOUD', 'CPU threshold percentage for cloud burst activation'),
('debug.max_trace_size', '1000000', 'NUMBER', 'DEBUG', 'Maximum debug trace entries per session'),
('mobile.cache_duration', '300', 'NUMBER', 'MOBILE', 'Default mobile API cache duration in seconds'),
('monitoring.retention_days', '90', 'NUMBER', 'MONITORING', 'Performance metrics retention in days');

-- Insert sample cloud providers
INSERT INTO cloud_providers (provider_name, provider_type, region, instance_types, is_active) VALUES
('AWS US East', 'AWS', 'us-east-1', '{"t3.medium": {"vcpu": 2, "memory": 4}, "t3.large": {"vcpu": 2, "memory": 8}}', TRUE),
('Azure East US', 'AZURE', 'eastus', '{"Standard_B2s": {"vcpu": 2, "memory": 4}, "Standard_B2ms": {"vcpu": 2, "memory": 8}}', TRUE),
('GCP US Central', 'GCP', 'us-central1', '{"n1-standard-1": {"vcpu": 1, "memory": 3.75}, "n1-standard-2": {"vcpu": 2, "memory": 7.5}}', TRUE);

-- Create indexes for Redis caching patterns
-- These comments indicate which queries should be cached in Redis

-- Programs that are frequently accessed should be cached
-- CACHE KEY: cobol:program:{program_name}
-- CACHE TTL: 3600 seconds

-- Active batch jobs should be cached for real-time monitoring
-- CACHE KEY: batch:active:{job_id}
-- CACHE TTL: 60 seconds

-- Performance metrics for dashboard should be cached
-- CACHE KEY: metrics:program:{program_id}:{date}
-- CACHE TTL: 300 seconds

-- Create stored procedures for common operations
DELIMITER //

-- Procedure: sp_record_execution
CREATE PROCEDURE sp_record_execution(
    IN p_program_id INT,
    IN p_execution_id VARCHAR(36),
    IN p_execution_type VARCHAR(20),
    IN p_input_params JSON,
    IN p_user_id INT,
    IN p_client_ip VARCHAR(45),
    IN p_request_source VARCHAR(100)
)
BEGIN
    INSERT INTO execution_history (
        program_id, execution_id, execution_type, 
        input_parameters, user_id, client_ip, request_source,
        status, start_time
    ) VALUES (
        p_program_id, p_execution_id, p_execution_type,
        p_input_params, p_user_id, p_client_ip, p_request_source,
        'SUCCESS', CURRENT_TIMESTAMP(3)
    );
END //

-- Procedure: sp_update_job_progress
CREATE PROCEDURE sp_update_job_progress(
    IN p_execution_id VARCHAR(36),
    IN p_progress DECIMAL(5,2),
    IN p_current_step VARCHAR(200),
    IN p_completed_steps INT
)
BEGIN
    UPDATE batch_job_executions 
    SET progress_percentage = p_progress,
        current_step = p_current_step,
        completed_steps = p_completed_steps
    WHERE execution_id = p_execution_id;
    
    -- Also log to progress table for historical tracking
    INSERT INTO batch_job_progress (
        execution_id, progress_percentage, current_operation
    ) VALUES (
        p_execution_id, p_progress, p_current_step
    );
END //

DELIMITER ;

-- Create views for common queries
CREATE OR REPLACE VIEW v_active_batch_jobs AS
SELECT 
    bje.id,
    bje.execution_id,
    bj.job_name,
    cp.program_name,
    bje.status,
    bje.progress_percentage,
    bje.current_step,
    bje.started_at,
    TIMESTAMPDIFF(SECOND, bje.started_at, NOW()) as running_seconds
FROM batch_job_executions bje
JOIN batch_jobs bj ON bje.job_id = bj.id
JOIN cobol_programs cp ON bj.program_id = cp.id
WHERE bje.status IN ('RUNNING', 'PAUSED');

CREATE OR REPLACE VIEW v_program_performance AS
SELECT 
    cp.id,
    cp.program_name,
    pm.metric_date,
    pm.execution_count,
    pm.success_count,
    pm.failure_count,
    pm.avg_duration_ms,
    pm.p95_duration_ms,
    ROUND((pm.success_count / pm.execution_count) * 100, 2) as success_rate
FROM cobol_programs cp
JOIN performance_metrics pm ON cp.id = pm.program_id
WHERE pm.metric_date >= DATE_SUB(CURDATE(), INTERVAL 30 DAY);

-- Grant appropriate permissions (adjust as needed)
-- GRANT SELECT, INSERT, UPDATE ON suitecrm.* TO 'suitecrm'@'%';
-- GRANT EXECUTE ON PROCEDURE suitecrm.sp_record_execution TO 'suitecrm'@'%';
-- GRANT EXECUTE ON PROCEDURE suitecrm.sp_update_job_progress TO 'suitecrm'@'%';

-- Final setup completed
SELECT 'SuiteCRM COBOL Bridge database schema created successfully!' as status;