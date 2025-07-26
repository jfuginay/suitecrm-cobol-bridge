-- Migration: 002_add_monitoring_indexes.sql
-- Description: Add performance indexes for monitoring queries
-- Version: 1.0.1
-- Date: 2025-01-26

-- Add composite indexes for monitoring dashboard queries
CREATE INDEX idx_execution_program_time 
ON execution_history(program_id, start_time, status);

CREATE INDEX idx_batch_execution_created 
ON batch_job_executions(created_at, status);

-- Add covering index for API performance queries
CREATE INDEX idx_api_logs_covering 
ON api_access_logs(api_key_id, request_timestamp, response_status, response_time_ms);

-- Add index for cloud burst job monitoring
CREATE INDEX idx_cloud_burst_monitoring 
ON cloud_burst_jobs(status, start_time, provider_id);

-- Add index for mobile endpoint performance
CREATE INDEX idx_mobile_stats_performance 
ON mobile_usage_stats(endpoint_id, usage_date, avg_response_time_ms);

-- Add index for real-time job progress tracking
CREATE INDEX idx_job_progress_realtime 
ON batch_job_progress(execution_id, timestamp, progress_percentage);

-- Add index for rule version lookups
CREATE INDEX idx_rule_version_current 
ON business_rule_versions(rule_id, is_current, version_number);

-- Record this migration
INSERT INTO schema_migrations (version, migration_name) 
VALUES ('002', '002_add_monitoring_indexes.sql');