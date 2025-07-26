-- Migration: 005_add_data_archival.sql
-- Description: Add data archival and retention management
-- Version: 1.0.4
-- Date: 2025-01-26

-- Table: data_retention_policies
-- Description: Define retention policies for different data types
CREATE TABLE IF NOT EXISTS data_retention_policies (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    policy_name VARCHAR(100) NOT NULL UNIQUE,
    table_name VARCHAR(100) NOT NULL,
    retention_days INT UNSIGNED NOT NULL,
    archive_strategy ENUM('DELETE', 'ARCHIVE', 'COMPRESS') NOT NULL DEFAULT 'ARCHIVE',
    archive_location VARCHAR(500) COMMENT 'S3 bucket or archive path',
    is_active BOOLEAN DEFAULT TRUE,
    last_run_at TIMESTAMP NULL,
    next_run_at TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_table_policy (table_name, is_active),
    INDEX idx_next_run (next_run_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: archive_history
-- Description: Track archival operations
CREATE TABLE IF NOT EXISTS archive_history (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    policy_id INT UNSIGNED NOT NULL,
    operation_date DATE NOT NULL,
    records_processed INT UNSIGNED,
    records_archived INT UNSIGNED,
    records_deleted INT UNSIGNED,
    archive_size_mb DECIMAL(10,2),
    duration_seconds INT UNSIGNED,
    status ENUM('SUCCESS', 'PARTIAL', 'FAILED') NOT NULL,
    error_message TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (policy_id) REFERENCES data_retention_policies(id) ON DELETE CASCADE,
    INDEX idx_policy_date (policy_id, operation_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Insert default retention policies
INSERT INTO data_retention_policies (policy_name, table_name, retention_days, archive_strategy) VALUES
('execution_history_retention', 'execution_history', 90, 'ARCHIVE'),
('api_logs_retention', 'api_access_logs', 30, 'COMPRESS'),
('debug_traces_retention', 'debug_traces', 7, 'DELETE'),
('audit_logs_retention', 'audit_logs', 365, 'ARCHIVE'),
('job_progress_retention', 'batch_job_progress', 30, 'DELETE');

-- Create event scheduler for automated archival
DELIMITER //

CREATE EVENT IF NOT EXISTS e_daily_archival_check
ON SCHEDULE EVERY 1 DAY
STARTS (DATE(NOW()) + INTERVAL 1 DAY + INTERVAL 2 HOUR)
DO
BEGIN
    -- Update next run times for policies
    UPDATE data_retention_policies 
    SET next_run_at = DATE_ADD(NOW(), INTERVAL 1 DAY)
    WHERE is_active = TRUE 
    AND (next_run_at IS NULL OR next_run_at < NOW());
END //

DELIMITER ;

-- Record this migration
INSERT INTO schema_migrations (version, migration_name) 
VALUES ('005', '005_add_data_archival.sql');