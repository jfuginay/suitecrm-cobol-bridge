<?php
/**
 * COBOL_Rules Detail View Definition
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

$module_name = 'COBOL_Rules';
$viewdefs[$module_name]['DetailView'] = array(
    'templateMeta' => array(
        'form' => array(
            'buttons' => array(
                'EDIT',
                'DUPLICATE',
                'DELETE',
                'FIND_DUPLICATES',
                array(
                    'customCode' => '<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Rules&action=VisualEditor&record={$fields.id.value}\';" value="{$MOD.LBL_EDIT_VISUAL}">',
                ),
                array(
                    'customCode' => '<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Rules&action=GenerateCOBOL&record={$fields.id.value}\';" value="{$MOD.LBL_GENERATE_COBOL}">',
                ),
                array(
                    'customCode' => '<input type="button" class="button" onClick="javascript:window.location=\'index.php?module=COBOL_Rules&action=Validate&record={$fields.id.value}\';" value="{$MOD.LBL_VALIDATE_RULE}">',
                ),
            ),
        ),
        'maxColumns' => '2',
        'widths' => array(
            array('label' => '10', 'field' => '30'),
            array('label' => '10', 'field' => '30'),
        ),
    ),
    'panels' => array(
        'LBL_PANEL_OVERVIEW' => array(
            array('name', 'assigned_user_name'),
            array('rule_type', 'status'),
            array('priority', 'version'),
            array('is_modified', 'validation_status'),
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
        'LBL_PANEL_RULE_DEFINITION' => array(
            array(
                array(
                    'name' => 'rule_definition',
                    'label' => 'LBL_RULE_DEFINITION',
                    'customCode' => '<pre>{$fields.rule_definition.value}</pre>',
                ),
            ),
            array(
                array(
                    'name' => 'conditions',
                    'label' => 'LBL_CONDITIONS',
                    'customCode' => '<pre>{$fields.conditions.value|@json_encode}</pre>',
                ),
            ),
            array(
                array(
                    'name' => 'actions',
                    'label' => 'LBL_ACTIONS',
                    'customCode' => '<pre>{$fields.actions.value|@json_encode}</pre>',
                ),
            ),
        ),
        'LBL_PANEL_TESTING' => array(
            array(
                array(
                    'name' => 'test_cases',
                    'label' => 'LBL_TEST_CASES',
                    'customCode' => '<pre>{$fields.test_cases.value|@json_encode}</pre>',
                ),
            ),
        ),
        'LBL_PANEL_GENERATED_CODE' => array(
            array(
                array(
                    'name' => 'generated_cobol',
                    'label' => 'LBL_GENERATED_COBOL',
                    'customCode' => '<pre style="max-height: 400px; overflow-y: auto;">{$fields.generated_cobol.value}</pre>',
                ),
            ),
        ),
    ),
);
?>