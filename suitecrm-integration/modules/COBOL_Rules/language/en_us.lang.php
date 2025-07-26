<?php
/**
 * COBOL_Rules Language File
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$mod_strings = array(
    'LBL_MODULE_NAME' => 'COBOL Rules',
    'LBL_MODULE_TITLE' => 'COBOL Rules',
    'LBL_MODULE_ID' => 'COBOL Rules',
    'LBL_SEARCH_FORM_TITLE' => 'COBOL Rule Search',
    'LBL_LIST_FORM_TITLE' => 'COBOL Rules List',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Rule',
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
    'LBL_NAME' => 'Rule Name',
    'LBL_CREATED_USER' => 'Created by User',
    'LBL_MODIFIED_USER' => 'Modified by User',
    'LBL_LIST_NAME' => 'Name',
    'LBL_ASSIGNED_TO_ID' => 'Assigned User Id',
    'LBL_ASSIGNED_TO_NAME' => 'Assigned to',
    'LBL_HOMEPAGE_TITLE' => 'My COBOL Rules',
    'LNK_NEW_RECORD' => 'Create COBOL Rule',
    'LNK_LIST' => 'View COBOL Rules',
    'LNK_IMPORT_COBOL_RULES' => 'Import COBOL Rules',
    'LBL_SEARCH_FORM_TITLE' => 'Search COBOL Rules',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'History',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activities',
    'LBL_COBOL_RULES_SUBPANEL_TITLE' => 'COBOL Rules',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Rule',
    
    // Custom fields
    'LBL_RULE_TYPE' => 'Rule Type',
    'LBL_SOURCE_PROGRAM' => 'Source Program',
    'LBL_RULE_DEFINITION' => 'Rule Definition',
    'LBL_VISUAL_REPRESENTATION' => 'Visual Representation',
    'LBL_CONDITIONS' => 'Conditions',
    'LBL_ACTIONS' => 'Actions',
    'LBL_PRIORITY' => 'Priority',
    'LBL_STATUS' => 'Status',
    'LBL_VERSION' => 'Version',
    'LBL_TEST_CASES' => 'Test Cases',
    'LBL_GENERATED_COBOL' => 'Generated COBOL Code',
    'LBL_IS_MODIFIED' => 'Modified',
    'LBL_VALIDATION_STATUS' => 'Validation Status',
    
    // Actions
    'LBL_EDIT_VISUAL' => 'Visual Editor',
    'LBL_GENERATE_COBOL' => 'Generate COBOL',
    'LBL_VALIDATE_RULE' => 'Validate Rule',
    'LBL_RUN_TESTS' => 'Run Test Cases',
    'LBL_VIEW_COBOL' => 'View COBOL Code',
    
    // List view
    'LBL_LIST_RULE_TYPE' => 'Type',
    'LBL_LIST_STATUS' => 'Status',
    'LBL_LIST_PRIORITY' => 'Priority',
    'LBL_LIST_IS_MODIFIED' => 'Modified',
    'LBL_LIST_VALIDATION_STATUS' => 'Validation',
    
    // Panels
    'LBL_PANEL_OVERVIEW' => 'Overview',
    'LBL_PANEL_RULE_DEFINITION' => 'Rule Definition',
    'LBL_PANEL_TESTING' => 'Testing & Validation',
    'LBL_PANEL_GENERATED_CODE' => 'Generated Code',
);

// Dropdown lists
$app_list_strings['cobol_rule_type_list'] = array(
    'validation' => 'Validation Rule',
    'calculation' => 'Calculation Rule',
    'conditional' => 'Conditional Logic',
    'lookup' => 'Lookup Table',
    'transformation' => 'Data Transformation',
    'aggregation' => 'Aggregation Rule',
    'workflow' => 'Workflow Rule',
);

$app_list_strings['cobol_rule_status_list'] = array(
    'active' => 'Active',
    'inactive' => 'Inactive',
    'draft' => 'Draft',
    'testing' => 'Testing',
    'deprecated' => 'Deprecated',
);

$app_list_strings['cobol_validation_status_list'] = array(
    'pending' => 'Pending Validation',
    'passed' => 'Validation Passed',
    'failed' => 'Validation Failed',
    'partial' => 'Partially Valid',
    'not_tested' => 'Not Tested',
);
?>