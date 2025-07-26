<?php
/**
 * COBOL_Jobs Language File
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$mod_strings = array(
    'LBL_MODULE_NAME' => 'COBOL Jobs',
    'LBL_MODULE_TITLE' => 'COBOL Jobs',
    'LBL_MODULE_ID' => 'COBOL Jobs',
    'LBL_SEARCH_FORM_TITLE' => 'COBOL Job Search',
    'LBL_LIST_FORM_TITLE' => 'COBOL Jobs List',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Job',
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
    'LBL_NAME' => 'Job Name',
    'LBL_CREATED_USER' => 'Created by User',
    'LBL_MODIFIED_USER' => 'Modified by User',
    'LBL_LIST_NAME' => 'Name',
    'LBL_ASSIGNED_TO_ID' => 'Assigned User Id',
    'LBL_ASSIGNED_TO_NAME' => 'Assigned to',
    'LBL_HOMEPAGE_TITLE' => 'My COBOL Jobs',
    'LNK_NEW_RECORD' => 'Create COBOL Job',
    'LNK_LIST' => 'View COBOL Jobs',
    'LNK_IMPORT_COBOL_JOBS' => 'Import COBOL Jobs',
    'LBL_SEARCH_FORM_TITLE' => 'Search COBOL Jobs',
    'LBL_HISTORY_SUBPANEL_TITLE' => 'History',
    'LBL_ACTIVITIES_SUBPANEL_TITLE' => 'Activities',
    'LBL_COBOL_JOBS_SUBPANEL_TITLE' => 'COBOL Jobs',
    'LBL_NEW_FORM_TITLE' => 'New COBOL Job',
    
    // Custom fields
    'LBL_JOB_TYPE' => 'Job Type',
    'LBL_JOB_ID' => 'External Job ID',
    'LBL_STATUS' => 'Status',
    'LBL_PROGRESS' => 'Progress (%)',
    'LBL_TOTAL_RECORDS' => 'Total Records',
    'LBL_PROCESSED_RECORDS' => 'Processed Records',
    'LBL_FAILED_RECORDS' => 'Failed Records',
    'LBL_START_TIME' => 'Start Time',
    'LBL_END_TIME' => 'End Time',
    'LBL_ESTIMATED_COMPLETION' => 'Estimated Completion',
    'LBL_INPUT_PARAMETERS' => 'Input Parameters',
    'LBL_OUTPUT_LOG' => 'Output Log',
    'LBL_ERROR_LOG' => 'Error Log',
    'LBL_PRIORITY' => 'Priority',
    'LBL_SCHEDULED_TIME' => 'Scheduled Time',
    'LBL_RETRY_COUNT' => 'Retry Count',
    'LBL_MAX_RETRIES' => 'Max Retries',
    'LBL_PARENT_JOB' => 'Parent Job',
    'LBL_IS_RESUMABLE' => 'Resumable',
    'LBL_CHECKPOINT_DATA' => 'Checkpoint Data',
    
    // Actions
    'LBL_PAUSE_JOB' => 'Pause Job',
    'LBL_RESUME_JOB' => 'Resume Job',
    'LBL_CANCEL_JOB' => 'Cancel Job',
    'LBL_VIEW_PROGRESS' => 'View Progress',
    'LBL_VIEW_OUTPUT' => 'View Output',
    'LBL_VIEW_ERRORS' => 'View Errors',
    'LBL_RETRY_JOB' => 'Retry Job',
    
    // List view
    'LBL_LIST_JOB_TYPE' => 'Type',
    'LBL_LIST_STATUS' => 'Status',
    'LBL_LIST_PROGRESS' => 'Progress',
    'LBL_LIST_START_TIME' => 'Started',
    'LBL_LIST_DURATION' => 'Duration',
    
    // Panels
    'LBL_PANEL_OVERVIEW' => 'Overview',
    'LBL_PANEL_PROGRESS' => 'Progress & Statistics',
    'LBL_PANEL_LOGS' => 'Logs & Output',
    'LBL_PANEL_SCHEDULING' => 'Scheduling & Retry',
    
    // Progress messages
    'LBL_JOB_SUBMITTED' => 'Job submitted successfully',
    'LBL_JOB_PAUSED' => 'Job paused',
    'LBL_JOB_RESUMED' => 'Job resumed',
    'LBL_JOB_CANCELLED' => 'Job cancelled',
    'LBL_JOB_COMPLETED' => 'Job completed',
    'LBL_JOB_FAILED' => 'Job failed',
    
    // Batch Job Progress Dashlet
    'LBL_BATCH_JOB_DASHLET_TITLE' => 'Batch Job Progress',
);

// Dropdown lists
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
?>