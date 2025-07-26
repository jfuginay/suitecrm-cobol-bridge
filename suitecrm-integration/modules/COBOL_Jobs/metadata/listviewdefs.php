<?php
/**
 * COBOL_Jobs List View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Jobs';
$listViewDefs[$module_name] = array(
    'NAME' => array(
        'width' => '20%',
        'label' => 'LBL_NAME',
        'default' => true,
        'link' => true,
    ),
    'JOB_TYPE' => array(
        'width' => '15%',
        'label' => 'LBL_LIST_JOB_TYPE',
        'default' => true,
    ),
    'STATUS' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_STATUS',
        'default' => true,
        'customCode' => '<span class="batch-job-status-{$STATUS|lower}">{$STATUS}</span>',
    ),
    'PROGRESS' => array(
        'width' => '15%',
        'label' => 'LBL_LIST_PROGRESS',
        'default' => true,
        'customCode' => '<div class="batch-job-progress"><div class="batch-job-progress-bar" style="width: {$PROGRESS}%">{$PROGRESS}%</div></div>',
    ),
    'START_TIME' => array(
        'width' => '15%',
        'label' => 'LBL_LIST_START_TIME',
        'default' => true,
    ),
    'ESTIMATED_COMPLETION' => array(
        'width' => '15%',
        'label' => 'LBL_ESTIMATED_COMPLETION',
        'default' => false,
    ),
    'PROCESSED_RECORDS' => array(
        'width' => '10%',
        'label' => 'LBL_PROCESSED_RECORDS',
        'default' => false,
        'customCode' => '{$PROCESSED_RECORDS}/{$TOTAL_RECORDS}',
    ),
    'ASSIGNED_USER_NAME' => array(
        'width' => '10%',
        'label' => 'LBL_ASSIGNED_TO_NAME',
        'module' => 'Users',
        'id' => 'ASSIGNED_USER_ID',
        'default' => false,
    ),
);
?>