-- Migration: 004_add_notification_system.sql
-- Description: Add notification and alerting system tables
-- Version: 1.0.3
-- Date: 2025-01-26

-- Table: notification_templates
-- Description: Templates for various notification types
CREATE TABLE IF NOT EXISTS notification_templates (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    template_name VARCHAR(100) NOT NULL UNIQUE,
    template_type ENUM('EMAIL', 'WEBHOOK', 'SMS', 'SLACK') NOT NULL,
    subject_template VARCHAR(500),
    body_template TEXT NOT NULL,
    variables JSON COMMENT 'Available template variables',
    is_active BOOLEAN DEFAULT TRUE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_template_type (template_type, is_active)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: notification_queue
-- Description: Queue for pending notifications
CREATE TABLE IF NOT EXISTS notification_queue (
    id BIGINT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    template_id INT UNSIGNED NOT NULL,
    recipient VARCHAR(500) NOT NULL,
    recipient_type ENUM('EMAIL', 'WEBHOOK_URL', 'PHONE', 'SLACK_CHANNEL') NOT NULL,
    variables JSON COMMENT 'Template variable values',
    priority ENUM('LOW', 'MEDIUM', 'HIGH', 'CRITICAL') DEFAULT 'MEDIUM',
    status ENUM('PENDING', 'SENDING', 'SENT', 'FAILED', 'CANCELLED') DEFAULT 'PENDING',
    retry_count INT DEFAULT 0,
    scheduled_at TIMESTAMP NULL,
    sent_at TIMESTAMP NULL,
    error_message TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (template_id) REFERENCES notification_templates(id) ON DELETE CASCADE,
    INDEX idx_status_priority (status, priority, scheduled_at),
    INDEX idx_created_at (created_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: alert_rules
-- Description: Configurable alert rules for system events
CREATE TABLE IF NOT EXISTS alert_rules (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    rule_name VARCHAR(200) NOT NULL,
    alert_type ENUM('BATCH_FAILURE', 'PERFORMANCE_DEGRADATION', 'ERROR_THRESHOLD', 'RESOURCE_LIMIT', 'CUSTOM') NOT NULL,
    condition_expression JSON NOT NULL COMMENT 'JSON expression for alert condition',
    threshold_value DECIMAL(10,2),
    evaluation_period_minutes INT DEFAULT 5,
    notification_template_id INT UNSIGNED,
    recipients JSON COMMENT 'List of recipients for this alert',
    is_active BOOLEAN DEFAULT TRUE,
    last_triggered_at TIMESTAMP NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (notification_template_id) REFERENCES notification_templates(id) ON DELETE SET NULL,
    INDEX idx_alert_type (alert_type, is_active),
    INDEX idx_last_triggered (last_triggered_at)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Insert default notification templates
INSERT INTO notification_templates (template_name, template_type, subject_template, body_template, variables) VALUES
('batch_job_failure', 'EMAIL', 'Batch Job Failed: {{job_name}}', 'The batch job {{job_name}} failed at {{failure_time}}.\n\nError: {{error_message}}\n\nExecution ID: {{execution_id}}', '["job_name", "failure_time", "error_message", "execution_id"]'),
('batch_job_completion', 'EMAIL', 'Batch Job Completed: {{job_name}}', 'The batch job {{job_name}} completed successfully.\n\nDuration: {{duration}}\nRecords Processed: {{records_processed}}', '["job_name", "duration", "records_processed"]'),
('performance_alert', 'WEBHOOK', NULL, '{"alert_type": "performance", "program": "{{program_name}}", "metric": "{{metric_name}}", "value": {{metric_value}}, "threshold": {{threshold_value}}}', '["program_name", "metric_name", "metric_value", "threshold_value"]');

-- Record this migration
INSERT INTO schema_migrations (version, migration_name) 
VALUES ('004', '004_add_notification_system.sql');