<?php
/**
 * COBOL_Jobs Detail View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Jobs';
$viewdefs[$module_name]['DetailView'] = array(
    'templateMeta' => array(
        'form' => array(
            'buttons' => array(
                'EDIT',
                'DUPLICATE',
                'DELETE',
                'FIND_DUPLICATES',
                array(
                    'customCode' => '{if $fields.status.value == "running"}<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Jobs&action=Pause&record={$fields.id.value}\';" value="{$MOD.LBL_PAUSE_JOB}">{/if}',
                ),
                array(
                    'customCode' => '{if $fields.status.value == "paused"}<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Jobs&action=Resume&record={$fields.id.value}\';" value="{$MOD.LBL_RESUME_JOB}">{/if}',
                ),
                array(
                    'customCode' => '{if $fields.status.value != "completed" && $fields.status.value != "failed" && $fields.status.value != "cancelled"}<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Jobs&action=Cancel&record={$fields.id.value}\';" value="{$MOD.LBL_CANCEL_JOB}">{/if}',
                ),
                array(
                    'customCode' => '{if $fields.status.value == "failed" && $fields.retry_count.value < $fields.max_retries.value}<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Jobs&action=Retry&record={$fields.id.value}\';" value="{$MOD.LBL_RETRY_JOB}">{/if}',
                ),
            ),
        ),
        'maxColumns' => '2',
        'widths' => array(
            array('label' => '10', 'field' => '30'),
            array('label' => '10', 'field' => '30'),
        ),
        'includes' => array(
            array('file' => 'custom/modules/COBOL_Jobs/js/detail.js'),
        ),
    ),
    'panels' => array(
        'LBL_PANEL_OVERVIEW' => array(
            array('name', 'assigned_user_name'),
            array('job_type', 'status'),
            array('job_id', 'priority'),
            array('scheduled_time', 'is_resumable'),
            array(
                array(
                    'name' => 'date_entered',
                    'customCode' => '{$fields.date_entered.value} {$APP.LBL_BY} {$fields.created_by_name.value}',
                    'label' => 'LBL_DATE_ENTERED',
                ),
                array(
                    'name' => 'date_modified',
                    'customCode' => '{$fields.date_modified.value} {$APP.LBL_BY} {$fields.modified_by_name.value}',
                    'label' => 'LBL_DATE_MODIFIED',
                ),
            ),
            array('description'),
        ),
        'LBL_PANEL_PROGRESS' => array(
            array(
                array(
                    'name' => 'progress',
                    'label' => 'LBL_PROGRESS',
                    'customCode' => '<div class="batch-job-progress" style="width: 300px;"><div class="batch-job-progress-bar" style="width: {$fields.progress.value}%">{$fields.progress.value}%</div></div>',
                ),
                'estimated_completion',
            ),
            array('start_time', 'end_time'),
            array('total_records', 'processed_records'),
            array('failed_records', ''),
            array(
                array(
                    'name' => 'duration',
                    'label' => 'LBL_LIST_DURATION',
                    'customCode' => '<span id="job_duration">{if $fields.end_time.value}Calculating...{else}In Progress{/if}</span>',
                ),
                '',
            ),
        ),
        'LBL_PANEL_LOGS' => array(
            array(
                array(
                    'name' => 'input_parameters',
                    'label' => 'LBL_INPUT_PARAMETERS',
                    'customCode' => '<pre>{$fields.input_parameters.value|@json_encode}</pre>',
                ),
            ),
            array(
                array(
                    'name' => 'output_log',
                    'label' => 'LBL_OUTPUT_LOG',
                    'customCode' => '<pre style="max-height: 300px; overflow-y: auto;">{$fields.output_log.value}</pre>',
                ),
            ),
            array(
                array(
                    'name' => 'error_log',
                    'label' => 'LBL_ERROR_LOG',
                    'customCode' => '<pre style="max-height: 300px; overflow-y: auto; color: red;">{$fields.error_log.value}</pre>',
                ),
            ),
        ),
        'LBL_PANEL_SCHEDULING' => array(
            array('retry_count', 'max_retries'),
            array('parent_job_id', ''),
            array(
                array(
                    'name' => 'checkpoint_data',
                    'label' => 'LBL_CHECKPOINT_DATA',
                    'customCode' => '<pre>{$fields.checkpoint_data.value|@json_encode}</pre>',
                ),
            ),
        ),
    ),
);
?>