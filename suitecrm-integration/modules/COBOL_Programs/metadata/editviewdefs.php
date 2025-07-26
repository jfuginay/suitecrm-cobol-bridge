<?php
/**
 * COBOL_Programs Edit View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Programs';
$viewdefs[$module_name]['EditView'] = array(
    'templateMeta' => array(
        'maxColumns' => '2',
        'widths' => array(
            array('label' => '10', 'field' => '30'),
            array('label' => '10', 'field' => '30'),
        ),
        'javascript' => '<script type="text/javascript" src="custom/modules/COBOL_Programs/js/edit.js"></script>',
    ),
    'panels' => array(
        'default' => array(
            array('name', 'assigned_user_name'),
            array('program_type', 'status'),
            array('version', ''),
            array('description'),
        ),
        'LBL_PANEL_TECHNICAL' => array(
            array(
                array(
                    'name' => 'input_parameters',
                    'label' => 'LBL_INPUT_PARAMETERS',
                    'type' => 'text',
                    'rows' => '4',
                    'cols' => '60',
                ),
            ),
            array(
                array(
                    'name' => 'output_format',
                    'label' => 'LBL_OUTPUT_FORMAT',
                    'type' => 'text',
                    'rows' => '4',
                    'cols' => '60',
                ),
            ),
            array(
                array(
                    'name' => 'program_code',
                    'label' => 'LBL_PROGRAM_CODE',
                    'type' => 'text',
                    'rows' => '20',
                    'cols' => '80',
                ),
            ),
        ),
    ),
);
?>