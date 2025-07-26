<?php
/**
 * COBOL_Programs List View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Programs';
$listViewDefs[$module_name] = array(
    'NAME' => array(
        'width' => '20%',
        'label' => 'LBL_NAME',
        'default' => true,
        'link' => true,
    ),
    'PROGRAM_TYPE' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_PROGRAM_TYPE',
        'default' => true,
    ),
    'STATUS' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_STATUS',
        'default' => true,
    ),
    'LAST_EXECUTION' => array(
        'width' => '15%',
        'label' => 'LBL_LIST_LAST_EXECUTION',
        'default' => true,
    ),
    'EXECUTION_COUNT' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_EXECUTION_COUNT',
        'default' => true,
    ),
    'SUCCESS_RATE' => array(
        'width' => '10%',
        'label' => 'LBL_LIST_SUCCESS_RATE',
        'default' => true,
        'customCode' => '{$SUCCESS_RATE|number_format:1}%',
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
    'CREATED_BY_NAME' => array(
        'width' => '10%',
        'label' => 'LBL_CREATED',
        'default' => false,
    ),
    'VERSION' => array(
        'width' => '5%',
        'label' => 'LBL_VERSION',
        'default' => false,
    ),
);
?>