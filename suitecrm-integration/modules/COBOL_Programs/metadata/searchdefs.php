<?php
/**
 * COBOL_Programs Search Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Programs';
$searchdefs[$module_name] = array(
    'layout' => array(
        'basic_search' => array(
            'name' => array(
                'name' => 'name',
                'default' => true,
                'width' => '10%',
            ),
            'program_type' => array(
                'name' => 'program_type',
                'default' => true,
                'width' => '10%',
            ),
            'status' => array(
                'name' => 'status',
                'default' => true,
                'width' => '10%',
            ),
        ),
        'advanced_search' => array(
            'name' => array(
                'name' => 'name',
                'default' => true,
                'width' => '10%',
            ),
            'program_type' => array(
                'name' => 'program_type',
                'default' => true,
                'width' => '10%',
            ),
            'status' => array(
                'name' => 'status',
                'default' => true,
                'width' => '10%',
            ),
            'last_execution' => array(
                'name' => 'last_execution',
                'default' => true,
                'width' => '10%',
            ),
            'execution_count' => array(
                'name' => 'execution_count',
                'default' => true,
                'width' => '10%',
            ),
            'success_rate' => array(
                'name' => 'success_rate',
                'default' => true,
                'width' => '10%',
            ),
            'assigned_user_id' => array(
                'name' => 'assigned_user_id',
                'type' => 'enum',
                'label' => 'LBL_ASSIGNED_TO',
                'function' => array(
                    'name' => 'get_user_array',
                    'params' => array(false),
                ),
                'default' => true,
                'width' => '10%',
            ),
        ),
    ),
    'templateMeta' => array(
        'maxColumns' => '3',
        'widths' => array('label' => '10', 'field' => '30'),
    ),
);
?>