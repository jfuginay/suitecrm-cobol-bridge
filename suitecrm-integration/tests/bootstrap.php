<?php
/**
 * PHPUnit Bootstrap for SuiteCRM COBOL Bridge Tests
 */

// Define constants required by SuiteCRM
define('sugarEntry', true);

// Set up basic environment
if (!defined('SUGAR_BASE_DIR')) {
    define('SUGAR_BASE_DIR', dirname(__DIR__));
}

// Mock SuiteCRM functions that may not be available during testing
if (!function_exists('create_guid')) {
    function create_guid() {
        return sprintf('%04x%04x-%04x-%04x-%04x-%04x%04x%04x',
            mt_rand(0, 0xffff), mt_rand(0, 0xffff),
            mt_rand(0, 0xffff),
            mt_rand(0, 0x0fff) | 0x4000,
            mt_rand(0, 0x3fff) | 0x8000,
            mt_rand(0, 0xffff), mt_rand(0, 0xffff), mt_rand(0, 0xffff)
        );
    }
}

// Load test helpers
require_once __DIR__ . '/helpers/TestHelper.php';
require_once __DIR__ . '/helpers/SugarMockFactory.php';

// Initialize autoloader for Composer dependencies
if (file_exists(dirname(__DIR__) . '/vendor/autoload.php')) {
    require_once dirname(__DIR__) . '/vendor/autoload.php';
}

// Set up test database configuration (using SQLite for tests)
$GLOBALS['sugar_config']['dbconfig'] = [
    'db_host_name' => ':memory:',
    'db_host_instance' => '',
    'db_user_name' => '',
    'db_password' => '',
    'db_name' => 'test_suitecrm',
    'db_type' => 'sqlite3',
    'db_port' => '',
    'db_manager' => 'SqliteManager'
];

// Set up test-specific configurations
$GLOBALS['sugar_config']['cobol_api_url'] = 'http://test-api:3000/api/v1';
$GLOBALS['sugar_config']['cobol_api_key'] = 'test-api-key-12345';
$GLOBALS['sugar_config']['cache_dir'] = sys_get_temp_dir() . '/suitecrm_test_cache';

// Create test cache directory
if (!is_dir($GLOBALS['sugar_config']['cache_dir'])) {
    mkdir($GLOBALS['sugar_config']['cache_dir'], 0755, true);
}

// Initialize test-specific globals
$GLOBALS['app_strings'] = [];
$GLOBALS['mod_strings'] = [];
$GLOBALS['current_language'] = 'en_us';

// Set timezone for consistent test results
date_default_timezone_set('UTC');

// Error reporting for tests
error_reporting(E_ALL & ~E_NOTICE & ~E_STRICT & ~E_DEPRECATED);
ini_set('display_errors', 1);

// Clean up function for tests
register_shutdown_function(function() {
    // Clean up test cache directory
    if (isset($GLOBALS['sugar_config']['cache_dir']) && 
        is_dir($GLOBALS['sugar_config']['cache_dir'])) {
        
        $files = glob($GLOBALS['sugar_config']['cache_dir'] . '/*');
        foreach($files as $file) {
            if (is_file($file)) {
                unlink($file);
            }
        }
        rmdir($GLOBALS['sugar_config']['cache_dir']);
    }
});

echo "SuiteCRM COBOL Bridge Test Bootstrap Loaded\n";
?>