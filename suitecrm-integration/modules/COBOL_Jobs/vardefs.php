<?php
/**
 * COBOL_Jobs vardefs
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$dictionary['COBOL_Jobs'] = array(
    'table' => 'cobol_jobs',
    'audited' => true,
    'duplicate_merge' => true,
    'fields' => array(
        'id' => array(
            'name' => 'id',
            'vname' => 'LBL_ID',
            'type' => 'id',
            'required' => true,
            'reportable' => true,
        ),
        'name' => array(
            'name' => 'name',
            'vname' => 'LBL_NAME',
            'type' => 'name',
            'dbType' => 'varchar',
            'len' => '255',
            'unified_search' => true,
            'required' => true,
            'importable' => 'required',
        ),
        'date_entered' => array(
            'name' => 'date_entered',
            'vname' => 'LBL_DATE_ENTERED',
            'type' => 'datetime',
            'enable_range_search' => true,
            'options' => 'date_range_search_dom',
        ),
        'date_modified' => array(
            'name' => 'date_modified',
            'vname' => 'LBL_DATE_MODIFIED',
            'type' => 'datetime',
            'enable_range_search' => true,
            'options' => 'date_range_search_dom',
        ),
        'modified_user_id' => array(
            'name' => 'modified_user_id',
            'rname' => 'user_name',
            'id_name' => 'modified_user_id',
            'vname' => 'LBL_MODIFIED',
            'type' => 'assigned_user_name',
            'table' => 'users',
            'isnull' => 'false',
            'dbType' => 'id',
            'reportable' => true,
        ),
        'created_by' => array(
            'name' => 'created_by',
            'rname' => 'user_name',
            'id_name' => 'modified_user_id',
            'vname' => 'LBL_CREATED',
            'type' => 'assigned_user_name',
            'table' => 'users',
            'isnull' => 'false',
            'dbType' => 'id',
        ),
        'description' => array(
            'name' => 'description',
            'vname' => 'LBL_DESCRIPTION',
            'type' => 'text',
        ),
        'deleted' => array(
            'name' => 'deleted',
            'vname' => 'LBL_DELETED',
            'type' => 'bool',
            'default' => '0',
            'reportable' => false,
        ),
        'assigned_user_id' => array(
            'name' => 'assigned_user_id',
            'rname' => 'user_name',
            'id_name' => 'assigned_user_id',
            'vname' => 'LBL_ASSIGNED_TO_ID',
            'type' => 'relate',
            'table' => 'users',
            'module' => 'Users',
            'dbType' => 'id',
            'massupdate' => 0,
            'reportable' => true,
        ),
        'assigned_user_name' => array(
            'name' => 'assigned_user_name',
            'link' => 'assigned_user_link',
            'vname' => 'LBL_ASSIGNED_TO_NAME',
            'rname' => 'user_name',
            'type' => 'relate',
            'reportable' => false,
            'source' => 'non-db',
            'table' => 'users',
            'id_name' => 'assigned_user_id',
            'module' => 'Users',
            'duplicate_merge' => 'disabled',
        ),
        // Custom fields
        'job_type' => array(
            'name' => 'job_type',
            'vname' => 'LBL_JOB_TYPE',
            'type' => 'enum',
            'options' => 'cobol_job_type_list',
            'len' => 100,
            'required' => true,
        ),
        'job_id' => array(
            'name' => 'job_id',
            'vname' => 'LBL_JOB_ID',
            'type' => 'varchar',
            'len' => '100',
            'comment' => 'External job ID from COBOL system',
        ),
        'status' => array(
            'name' => 'status',
            'vname' => 'LBL_STATUS',
            'type' => 'enum',
            'options' => 'cobol_job_status_list',
            'len' => 100,
            'default' => 'pending',
        ),
        'progress' => array(
            'name' => 'progress',
            'vname' => 'LBL_PROGRESS',
            'type' => 'int',
            'default' => '0',
            'comment' => 'Progress percentage (0-100)',
        ),
        'total_records' => array(
            'name' => 'total_records',
            'vname' => 'LBL_TOTAL_RECORDS',
            'type' => 'int',
            'default' => '0',
        ),
        'processed_records' => array(
            'name' => 'processed_records',
            'vname' => 'LBL_PROCESSED_RECORDS',
            'type' => 'int',
            'default' => '0',
        ),
        'failed_records' => array(
            'name' => 'failed_records',
            'vname' => 'LBL_FAILED_RECORDS',
            'type' => 'int',
            'default' => '0',
        ),
        'start_time' => array(
            'name' => 'start_time',
            'vname' => 'LBL_START_TIME',
            'type' => 'datetime',
        ),
        'end_time' => array(
            'name' => 'end_time',
            'vname' => 'LBL_END_TIME',
            'type' => 'datetime',
        ),
        'estimated_completion' => array(
            'name' => 'estimated_completion',
            'vname' => 'LBL_ESTIMATED_COMPLETION',
            'type' => 'datetime',
        ),
        'input_parameters' => array(
            'name' => 'input_parameters',
            'vname' => 'LBL_INPUT_PARAMETERS',
            'type' => 'text',
            'comment' => 'JSON input parameters',
        ),
        'output_log' => array(
            'name' => 'output_log',
            'vname' => 'LBL_OUTPUT_LOG',
            'type' => 'text',
            'comment' => 'Job output log',
        ),
        'error_log' => array(
            'name' => 'error_log',
            'vname' => 'LBL_ERROR_LOG',
            'type' => 'text',
            'comment' => 'Job error log',
        ),
        'priority' => array(
            'name' => 'priority',
            'vname' => 'LBL_PRIORITY',
            'type' => 'int',
            'default' => '50',
            'comment' => 'Job priority (0-100)',
        ),
        'scheduled_time' => array(
            'name' => 'scheduled_time',
            'vname' => 'LBL_SCHEDULED_TIME',
            'type' => 'datetime',
        ),
        'retry_count' => array(
            'name' => 'retry_count',
            'vname' => 'LBL_RETRY_COUNT',
            'type' => 'int',
            'default' => '0',
        ),
        'max_retries' => array(
            'name' => 'max_retries',
            'vname' => 'LBL_MAX_RETRIES',
            'type' => 'int',
            'default' => '3',
        ),
        'parent_job_id' => array(
            'name' => 'parent_job_id',
            'vname' => 'LBL_PARENT_JOB',
            'type' => 'id',
            'comment' => 'ID of parent job for sub-jobs',
        ),
        'is_resumable' => array(
            'name' => 'is_resumable',
            'vname' => 'LBL_IS_RESUMABLE',
            'type' => 'bool',
            'default' => '1',
        ),
        'checkpoint_data' => array(
            'name' => 'checkpoint_data',
            'vname' => 'LBL_CHECKPOINT_DATA',
            'type' => 'text',
            'comment' => 'JSON checkpoint data for resuming',
        ),
    ),
    'relationships' => array(
        'cobol_jobs_parent' => array(
            'lhs_module' => 'COBOL_Jobs',
            'lhs_table' => 'cobol_jobs',
            'lhs_key' => 'id',
            'rhs_module' => 'COBOL_Jobs',
            'rhs_table' => 'cobol_jobs',
            'rhs_key' => 'parent_job_id',
            'relationship_type' => 'one-to-many',
        ),
    ),
    'indices' => array(
        array('name' => 'cobol_jobs_pk', 'type' => 'primary', 'fields' => array('id')),
        array('name' => 'idx_job_type', 'type' => 'index', 'fields' => array('job_type', 'deleted')),
        array('name' => 'idx_status', 'type' => 'index', 'fields' => array('status', 'deleted')),
        array('name' => 'idx_job_id', 'type' => 'index', 'fields' => array('job_id')),
        array('name' => 'idx_parent_job', 'type' => 'index', 'fields' => array('parent_job_id')),
    ),
);
?>