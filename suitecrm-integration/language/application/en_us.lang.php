<?php
/**
 * Application Level Language Strings for COBOL Bridge
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

// Module names
$app_list_strings['moduleList']['COBOL_Programs'] = 'COBOL Programs';
$app_list_strings['moduleList']['COBOL_Rules'] = 'COBOL Rules';
$app_list_strings['moduleList']['COBOL_Jobs'] = 'COBOL Jobs';

// Module singular names
$app_list_strings['moduleListSingular']['COBOL_Programs'] = 'COBOL Program';
$app_list_strings['moduleListSingular']['COBOL_Rules'] = 'COBOL Rule';
$app_list_strings['moduleListSingular']['COBOL_Jobs'] = 'COBOL Job';

// Record type display names
$app_list_strings['record_type_display']['COBOL_Programs'] = 'COBOL Program';
$app_list_strings['record_type_display']['COBOL_Rules'] = 'COBOL Rule';
$app_list_strings['record_type_display']['COBOL_Jobs'] = 'COBOL Job';

// Record type display notes
$app_list_strings['record_type_display_notes']['COBOL_Programs'] = 'COBOL Program';
$app_list_strings['record_type_display_notes']['COBOL_Rules'] = 'COBOL Rule';
$app_list_strings['record_type_display_notes']['COBOL_Jobs'] = 'COBOL Job';

// Menu items
$app_strings['LBL_COBOL_BRIDGE'] = 'COBOL Bridge';
$app_strings['LBL_COBOL_BRIDGE_DESCRIPTION'] = 'Manage COBOL programs, rules, and batch jobs';
$app_strings['LBL_COBOL_MONITOR_TITLE'] = 'COBOL Program Monitor';
$app_strings['LBL_BATCH_JOB_PROGRESS_TITLE'] = 'Batch Job Progress';

// Common labels
$app_strings['LBL_COBOL_API_SETTINGS'] = 'COBOL API Settings';
$app_strings['LBL_COBOL_API_URL'] = 'COBOL API URL';
$app_strings['LBL_COBOL_API_KEY'] = 'COBOL API Key';
$app_strings['LBL_COBOL_WEBSOCKET_URL'] = 'WebSocket URL';

// Status messages
$app_strings['LBL_COBOL_CONNECTION_SUCCESS'] = 'Successfully connected to COBOL API Gateway';
$app_strings['LBL_COBOL_CONNECTION_FAILED'] = 'Failed to connect to COBOL API Gateway';
$app_strings['LBL_COBOL_EXECUTION_SUCCESS'] = 'COBOL program executed successfully';
$app_strings['LBL_COBOL_EXECUTION_FAILED'] = 'COBOL program execution failed';

// Field labels
$app_strings['LBL_PANEL_TECHNICAL'] = 'Technical Details';
$app_strings['LBL_PANEL_EXECUTION'] = 'Execution Settings';
$app_strings['LBL_PANEL_MONITORING'] = 'Monitoring & Metrics';

// Dropdown lists already defined in module files, but we ensure they're globally available
if (!isset($app_list_strings['cobol_program_type_list'])) {
    $app_list_strings['cobol_program_type_list'] = array(
        'credit' => 'Credit Calculation',
        'payroll' => 'Payroll Processing',
        'interest' => 'Interest Calculation',
        'batch' => 'Batch Processing',
        'report' => 'Report Generation',
        'validation' => 'Data Validation',
        'conversion' => 'Data Conversion',
    );
}

if (!isset($app_list_strings['cobol_program_status_list'])) {
    $app_list_strings['cobol_program_status_list'] = array(
        'active' => 'Active',
        'inactive' => 'Inactive',
        'testing' => 'Testing',
        'deprecated' => 'Deprecated',
        'error' => 'Error',
    );
}

if (!isset($app_list_strings['cobol_rule_type_list'])) {
    $app_list_strings['cobol_rule_type_list'] = array(
        'validation' => 'Validation Rule',
        'calculation' => 'Calculation Rule',
        'conditional' => 'Conditional Logic',
        'lookup' => 'Lookup Table',
        'transformation' => 'Data Transformation',
        'aggregation' => 'Aggregation Rule',
        'workflow' => 'Workflow Rule',
    );
}

if (!isset($app_list_strings['cobol_rule_status_list'])) {
    $app_list_strings['cobol_rule_status_list'] = array(
        'active' => 'Active',
        'inactive' => 'Inactive',
        'draft' => 'Draft',
        'testing' => 'Testing',
        'deprecated' => 'Deprecated',
    );
}

if (!isset($app_list_strings['cobol_validation_status_list'])) {
    $app_list_strings['cobol_validation_status_list'] = array(
        'pending' => 'Pending Validation',
        'passed' => 'Validation Passed',
        'failed' => 'Validation Failed',
        'partial' => 'Partially Valid',
        'not_tested' => 'Not Tested',
    );
}

if (!isset($app_list_strings['cobol_job_type_list'])) {
    $app_list_strings['cobol_job_type_list'] = array(
        'DAILY_INTEREST' => 'Daily Interest Calculation',
        'PAYROLL_RUN' => 'Payroll Processing',
        'CREDIT_REVIEW' => 'Credit Review Batch',
        'MONTH_END' => 'Month End Processing',
        'YEAR_END' => 'Year End Processing',
        'DATA_MIGRATION' => 'Data Migration',
        'REPORT_GENERATION' => 'Report Generation',
        'BACKUP' => 'Backup Process',
        'VALIDATION' => 'Data Validation',
        'CUSTOM' => 'Custom Job',
    );
}

if (!isset($app_list_strings['cobol_job_status_list'])) {
    $app_list_strings['cobol_job_status_list'] = array(
        'pending' => 'Pending',
        'queued' => 'Queued',
        'running' => 'Running',
        'paused' => 'Paused',
        'completed' => 'Completed',
        'failed' => 'Failed',
        'cancelled' => 'Cancelled',
        'retrying' => 'Retrying',
    );
}
?>