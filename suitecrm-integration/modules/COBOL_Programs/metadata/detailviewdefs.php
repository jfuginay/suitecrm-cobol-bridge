<?php
/**
 * COBOL_Programs Detail View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Programs';
$viewdefs[$module_name]['DetailView'] = array(
    'templateMeta' => array(
        'form' => array(
            'buttons' => array(
                'EDIT',
                'DUPLICATE',
                'DELETE',
                'FIND_DUPLICATES',
                array(
                    'customCode' => '<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Programs&action=Execute&record={$fields.id.value}\';" value="{$MOD.LBL_EXECUTE}">',
                ),
                array(
                    'customCode' => '<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Programs&action=History&record={$fields.id.value}\';" value="{$MOD.LBL_VIEW_HISTORY}">',
                ),
            ),
        ),
        'maxColumns' => '2',
        'widths' => array(
            array('label' => '10', 'field' => '30'),
            array('label' => '10', 'field' => '30'),
        ),
        'includes' => array(
            array('file' => 'custom/modules/COBOL_Programs/js/detail.js'),
        ),
    ),
    'panels' => array(
        'default' => array(
            array('name', 'assigned_user_name'),
            array('program_type', 'status'),
            array('version', 'last_execution'),
            array('execution_count', 'success_rate'),
            array('average_runtime', 'error_count'),
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
        'LBL_PANEL_TECHNICAL' => array(
            array(
                array(
                    'name' => 'input_parameters',
                    'label' => 'LBL_INPUT_PARAMETERS',
                    'customCode' => '<pre>{$fields.input_parameters.value}</pre>',
                ),
            ),
            array(
                array(
                    'name' => 'output_format',
                    'label' => 'LBL_OUTPUT_FORMAT',
                    'customCode' => '<pre>{$fields.output_format.value}</pre>',
                ),
            ),
            array(
                array(
                    'name' => 'program_code',
                    'label' => 'LBL_PROGRAM_CODE',
                    'customCode' => '<pre style="max-height: 400px; overflow-y: auto;">{$fields.program_code.value}</pre>',
                ),
            ),
        ),
    ),
);
?>