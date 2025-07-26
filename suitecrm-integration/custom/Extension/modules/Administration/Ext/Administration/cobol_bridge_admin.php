<?php
/**
 * COBOL Bridge Administration Links
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$admin_option_defs = array();

// COBOL Bridge Settings
$admin_option_defs['Administration']['cobol_bridge_settings'] = array(
    'Administration',
    'LBL_COBOL_BRIDGE_SETTINGS',
    'LBL_COBOL_BRIDGE_SETTINGS_DESC',
    'index.php?module=Administration&action=COBOLBridgeSettings',
    'cobol_bridge'
);

// COBOL API Configuration
$admin_option_defs['Administration']['cobol_api_config'] = array(
    'Administration',
    'LBL_COBOL_API_CONFIG',
    'LBL_COBOL_API_CONFIG_DESC',
    'index.php?module=Administration&action=COBOLAPIConfig',
    'cobol_bridge'
);

// COBOL Program Management
$admin_option_defs['Administration']['cobol_program_management'] = array(
    'Administration',
    'LBL_COBOL_PROGRAM_MANAGEMENT',
    'LBL_COBOL_PROGRAM_MANAGEMENT_DESC',
    'index.php?module=COBOL_Programs&action=index',
    'cobol_bridge'
);

// COBOL Rule Management
$admin_option_defs['Administration']['cobol_rule_management'] = array(
    'Administration',
    'LBL_COBOL_RULE_MANAGEMENT',
    'LBL_COBOL_RULE_MANAGEMENT_DESC',
    'index.php?module=COBOL_Rules&action=index',
    'cobol_bridge'
);

// COBOL Job Management
$admin_option_defs['Administration']['cobol_job_management'] = array(
    'Administration',
    'LBL_COBOL_JOB_MANAGEMENT',
    'LBL_COBOL_JOB_MANAGEMENT_DESC',
    'index.php?module=COBOL_Jobs&action=index',
    'cobol_bridge'
);

// COBOL Monitoring Dashboard
$admin_option_defs['Administration']['cobol_monitoring'] = array(
    'Administration',
    'LBL_COBOL_MONITORING',
    'LBL_COBOL_MONITORING_DESC',
    'index.php?module=Administration&action=COBOLMonitoring',
    'cobol_bridge'
);

// COBOL Test Connection
$admin_option_defs['Administration']['cobol_test_connection'] = array(
    'Administration',
    'LBL_COBOL_TEST_CONNECTION',
    'LBL_COBOL_TEST_CONNECTION_DESC',
    'index.php?module=Administration&action=COBOLTestConnection',
    'cobol_bridge'
);

// Add section header
$admin_group_header[] = array(
    'LBL_COBOL_BRIDGE',
    '',
    false,
    $admin_option_defs,
    'LBL_COBOL_BRIDGE_DESCRIPTION'
);
?>