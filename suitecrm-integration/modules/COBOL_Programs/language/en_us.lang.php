<?php
/**
 * COBOL_Programs Language File
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$mod_strings = array(
    'LBL_MODULE_NAME' => 'COBOL Programs',
    'LBL_MODULE_TITLE' => 'COBOL Programs',
    'LBL_MODULE_ID' => 'COBOL Programs',
    'LBL_SEARCH_FORM_TITLE' => 'COBOL Program Search',
    'LBL_LIST_FORM_TITLE' => 'COBOL Programs List',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Program',
    'LBL_ID' => 'ID',
    'LBL_DATE_ENTERED' => 'Date Created',
    'LBL_DATE_MODIFIED' => 'Date Modified',
    'LBL_MODIFIED' => 'Modified By',
    'LBL_MODIFIED_ID' => 'Modified By Id',
    'LBL_MODIFIED_NAME' => 'Modified By Name',
    'LBL_CREATED' => 'Created By',
    'LBL_CREATED_ID' => 'Created By Id',
    'LBL_DESCRIPTION' => 'Description',
    'LBL_DELETED' => 'Deleted',
    'LBL_NAME' => 'Program Name',
    'LBL_CREATED_USER' => 'Created by User',
    'LBL_MODIFIED_USER' => 'Modified by User',
    'LBL_LIST_NAME' => 'Name',
    'LBL_ASSIGNED_TO_ID' => 'Assigned User Id',
    'LBL_ASSIGNED_TO_NAME' => 'Assigned to',
    'LBL_HOMEPAGE_TITLE' => 'My COBOL Programs',
    'LNK_NEW_RECORD' => 'Create COBOL Program',
    'LNK_LIST' => 'View COBOL Programs',
    'LNK_IMPORT_COBOL_PROGRAMS' => 'Import COBOL Programs',
    'LBL_SEARCH_FORM_TITLE' => 'Search COBOL Programs',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'History',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activities',
    'LBL_COBOL_PROGRAMS_SUBPANEL_TITLE' => 'COBOL Programs',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Program',
    
    // Custom fields
    'LBL_PROGRAM_CODE' => 'Program Code',
    'LBL_PROGRAM_TYPE' => 'Program Type',
    'LBL_LAST_EXECUTION' => 'Last Execution',
    'LBL_EXECUTION_COUNT' => 'Execution Count',
    'LBL_AVERAGE_RUNTIME' => 'Average Runtime (seconds)',
    'LBL_STATUS' => 'Status',
    'LBL_VERSION' => 'Version',
    'LBL_INPUT_PARAMETERS' => 'Input Parameters',
    'LBL_OUTPUT_FORMAT' => 'Output Format',
    'LBL_ERROR_COUNT' => 'Error Count',
    'LBL_SUCCESS_RATE' => 'Success Rate (%)',
    
    // Actions
    'LBL_EXECUTE' => 'Execute Program',
    'LBL_VIEW_HISTORY' => 'View Execution History',
    'LBL_TEST_PROGRAM' => 'Test Program',
    'LBL_EXPORT_COBOL' => 'Export COBOL Code',
    
    // List view
    'LBL_LIST_PROGRAM_TYPE' => 'Type',
    'LBL_LIST_STATUS' => 'Status',
    'LBL_LIST_LAST_EXECUTION' => 'Last Run',
    'LBL_LIST_EXECUTION_COUNT' => 'Runs',
    'LBL_LIST_SUCCESS_RATE' => 'Success %',
);

// Dropdown lists
$app_list_strings['cobol_program_type_list'] = array(
    'credit' => 'Credit Calculation',
    'payroll' => 'Payroll Processing',
    'interest' => 'Interest Calculation',
    'batch' => 'Batch Processing',
    'report' => 'Report Generation',
    'validation' => 'Data Validation',
    'conversion' => 'Data Conversion',
);

$app_list_strings['cobol_program_status_list'] = array(
    'active' => 'Active',
    'inactive' => 'Inactive',
    'testing' => 'Testing',
    'deprecated' => 'Deprecated',
    'error' => 'Error',
);
?>