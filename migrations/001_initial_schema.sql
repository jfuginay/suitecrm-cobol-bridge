-- Migration: 001_initial_schema.sql
-- Description: Initial database schema for COBOL Bridge
-- Version: 1.0.0
-- Date: 2025-01-26

-- This migration creates the initial schema
-- It should match the init.sql file for fresh installations

-- Migration tracking table
CREATE TABLE IF NOT EXISTS schema_migrations (
    version VARCHAR(20) PRIMARY KEY,
    migration_name VARCHAR(255) NOT NULL,
    applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    execution_time_ms INT UNSIGNED,
    checksum VARCHAR(64)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Record this migration
INSERT INTO schema_migrations (version, migration_name) 
VALUES ('001', '001_initial_schema.sql');

-- The actual schema creation is handled by init.sql
-- This file exists for migration tracking purposes