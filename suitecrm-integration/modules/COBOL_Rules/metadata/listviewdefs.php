<?php
/**
 * COBOL_Rules List View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Rules';
$listViewDefs[$module_name] = array(
    'NAME' => array(
        'width' => '20%',
        'label' => 'LBL_NAME',
        'default' => true,
        'link' => true,
    ),
    'RULE_TYPE' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_RULE_TYPE',
        'default' => true,
    ),
    'STATUS' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_STATUS',
        'default' => true,
    ),
    'PRIORITY' => array(
        'width' => '5%',
        'label' => 'LBL_LIST_PRIORITY',
        'default' => true,
    ),
    'IS_MODIFIED' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_IS_MODIFIED',
        'default' => true,
        'customCode' => '{if $IS_MODIFIED}Yes{else}No{/if}',
    ),
    'VALIDATION_STATUS' => array(
        'width' => '15%',
        'label' => 'LBL_LIST_VALIDATION_STATUS',
        'default' => true,
    ),
    'ASSIGNED_USER_NAME' => array(
        'width' => '10%',
        'label' => 'LBL_ASSIGNED_TO_NAME',
        'module' => 'Users',
        'id' => 'ASSIGNED_USER_ID',
        'default' => true,
    ),
    'DATE_MODIFIED' => array(
        'width' => '10%',
        'label' => 'LBL_DATE_MODIFIED',
        'default' => false,
    ),
    'VERSION' => array(
        'width' => '5%',
        'label' => 'LBL_VERSION',
        'default' => false,
    ),
);
?>