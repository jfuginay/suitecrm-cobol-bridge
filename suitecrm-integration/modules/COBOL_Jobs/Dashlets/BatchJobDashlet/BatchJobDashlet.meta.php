<?php
/**
 * Batch Job Dashlet Metadata
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

global $app_strings;

$dashletMeta['BatchJobDashlet'] = array(
    'module' => 'COBOL_Jobs',
    'title' => translate('LBL_BATCH_JOB_DASHLET_TITLE', 'COBOL_Jobs'),
    'description' => 'Monitor batch job progress and status in real-time',
    'icon' => 'icon_COBOL_Jobs_32.gif',
    'category' => 'Module Views',
    'reportDashlet' => true,
);
?>