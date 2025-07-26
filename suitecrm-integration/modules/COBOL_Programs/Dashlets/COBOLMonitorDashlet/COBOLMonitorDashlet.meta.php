<?php
/**
 * COBOL Monitor Dashlet Metadata
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

global $app_strings;

$dashletMeta['COBOLMonitorDashlet'] = array(
    'module' => 'COBOL_Programs',
    'title' => translate('LBL_COBOL_MONITOR_TITLE', 'COBOL_Programs'),
    'description' => 'Monitor COBOL program executions in real-time',
    'icon' => 'icon_COBOL_Programs_32.gif',
    'category' => 'Module Views',
    'reportDashlet' => true,
);
?>