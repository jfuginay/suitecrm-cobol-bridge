-- Migration: 003_add_ai_features.sql
-- Description: Add tables for AI-powered code review and analysis
-- Version: 1.0.2
-- Date: 2025-01-26

-- Table: ai_code_reviews
-- Description: AI-generated code reviews and suggestions
CREATE TABLE IF NOT EXISTS ai_code_reviews (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    program_id INT UNSIGNED NOT NULL,
    review_type ENUM('PERFORMANCE', 'SECURITY', 'MAINTAINABILITY', 'BEST_PRACTICES') NOT NULL,
    severity ENUM('INFO', 'WARNING', 'ERROR', 'CRITICAL') NOT NULL,
    line_number INT UNSIGNED,
    column_number INT UNSIGNED,
    code_snippet TEXT,
    issue_description TEXT NOT NULL,
    suggested_fix TEXT,
    confidence_score DECIMAL(3,2) COMMENT 'AI confidence score 0.00-1.00',
    is_resolved BOOLEAN DEFAULT FALSE,
    resolved_at TIMESTAMP NULL,
    resolved_by INT UNSIGNED,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    INDEX idx_program_reviews (program_id, is_resolved),
    INDEX idx_severity_type (severity, review_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: ai_pattern_library
-- Description: Detected patterns and anti-patterns in COBOL code
CREATE TABLE IF NOT EXISTS ai_pattern_library (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    pattern_name VARCHAR(200) NOT NULL,
    pattern_type ENUM('PATTERN', 'ANTI_PATTERN') NOT NULL,
    category VARCHAR(100) NOT NULL,
    description TEXT,
    example_code TEXT,
    recommendation TEXT,
    occurrence_count INT UNSIGNED DEFAULT 0,
    programs_affected JSON COMMENT 'List of program IDs where pattern is found',
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_pattern_type (pattern_type, category),
    INDEX idx_pattern_name (pattern_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Table: ai_test_suggestions
-- Description: AI-generated test cases for COBOL programs
CREATE TABLE IF NOT EXISTS ai_test_suggestions (
    id INT UNSIGNED AUTO_INCREMENT PRIMARY KEY,
    program_id INT UNSIGNED NOT NULL,
    test_name VARCHAR(200) NOT NULL,
    test_type ENUM('UNIT', 'INTEGRATION', 'EDGE_CASE', 'REGRESSION') NOT NULL,
    input_data JSON NOT NULL,
    expected_output JSON,
    test_rationale TEXT,
    coverage_improvement DECIMAL(5,2) COMMENT 'Expected coverage improvement percentage',
    is_implemented BOOLEAN DEFAULT FALSE,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (program_id) REFERENCES cobol_programs(id) ON DELETE CASCADE,
    INDEX idx_program_tests (program_id, is_implemented),
    INDEX idx_test_type (test_type)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- Record this migration
INSERT INTO schema_migrations (version, migration_name) 
VALUES ('003', '003_add_ai_features.sql');