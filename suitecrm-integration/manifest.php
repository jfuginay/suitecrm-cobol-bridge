<?php
/**
 * SuiteCRM COBOL Bridge Module Manifest
 * 
 * This module integrates all 6 modernization features into SuiteCRM
 */

$manifest = array(
    'acceptable_sugar_versions' => array(
        'regex_matches' => array('^(7\.[0-9]+|8\.[0-9]+)\.*'
        ),
    ),
    'acceptable_sugar_flavors' => array(
        'CE', 'PRO', 'ENT'
    ),
    'readme' => 'README.md',
    'key' => 'COBOL_BRIDGE',
    'author' => 'SuiteCRM COBOL Bridge Team',
    'description' => 'Complete COBOL modernization bridge with real-time monitoring, visual rule editing, mobile API, time-travel debugging, and cloud burst capabilities',
    'icon' => '',
    'is_uninstallable' => true,
    'name' => 'COBOL Bridge Pro',
    'published_date' => '2025-01-26',
    'type' => 'module',
    'version' => '2.0.0',
    'remove_tables' => 'prompt',
);

$installdefs = array(
    'id' => 'COBOL_BRIDGE',
    'beans' => array(
        array(
            'module' => 'COBOL_Programs',
            'class' => 'COBOL_Programs',
            'path' => 'modules/COBOL_Programs/COBOL_Programs.php',
            'tab' => true,
        ),
        array(
            'module' => 'COBOL_Rules',
            'class' => 'COBOL_Rules',
            'path' => 'modules/COBOL_Rules/COBOL_Rules.php',
            'tab' => true,
        ),
        array(
            'module' => 'COBOL_Jobs',
            'class' => 'COBOL_Jobs',
            'path' => 'modules/COBOL_Jobs/COBOL_Jobs.php',
            'tab' => true,
        ),
    ),
    'layoutdefs' => array(),
    'relationships' => array(),
    'image_dir' => '<basepath>/icons',
    'copy' => array(
        array(
            'from' => '<basepath>/modules/COBOL_Programs',
            'to' => 'modules/COBOL_Programs',
        ),
        array(
            'from' => '<basepath>/modules/COBOL_Rules',
            'to' => 'modules/COBOL_Rules',
        ),
        array(
            'from' => '<basepath>/modules/COBOL_Jobs',
            'to' => 'modules/COBOL_Jobs',
        ),
        array(
            'from' => '<basepath>/custom/include/CobolBridge',
            'to' => 'custom/include/CobolBridge',
        ),
        array(
            'from' => '<basepath>/custom/Extension/modules/Administration/Ext/Administration',
            'to' => 'custom/Extension/modules/Administration/Ext/Administration',
        ),
    ),
    'language' => array(
        array(
            'from' => '<basepath>/language/application/en_us.lang.php',
            'to_module' => 'application',
            'language' => 'en_us',
        ),
    ),
    'dashlets' => array(
        array(
            'name' => 'COBOL Monitor Dashlet',
            'from' => '<basepath>/modules/COBOL_Programs/Dashlets/COBOLMonitorDashlet',
            'to' => 'modules/COBOL_Programs/Dashlets/COBOLMonitorDashlet',
        ),
        array(
            'name' => 'Batch Job Progress Dashlet',
            'from' => '<basepath>/modules/COBOL_Jobs/Dashlets/BatchJobDashlet',
            'to' => 'modules/COBOL_Jobs/Dashlets/BatchJobDashlet',
        ),
    ),
    'custom_fields' => array(
        array(
            'name' => 'cobol_execution_time_c',
            'label' => 'COBOL Execution Time',
            'type' => 'float',
            'module' => 'AOS_Quotes',
            'default_value' => '0',
        ),
        array(
            'name' => 'cobol_calculated_c',
            'label' => 'COBOL Calculated',
            'type' => 'bool',
            'module' => 'AOS_Quotes',
            'default_value' => '0',
        ),
    ),
    'logic_hooks' => array(
        array(
            'module' => 'AOS_Quotes',
            'hook' => 'before_save',
            'order' => 99,
            'description' => 'COBOL Financial Calculations',
            'file' => 'custom/include/CobolBridge/QuoteCalculations.php',
            'class' => 'CobolQuoteCalculations',
            'function' => 'calculateWithCOBOL',
        ),
    ),
    'post_install' => array(
        '<basepath>/scripts/post_install.php',
    ),
    'post_uninstall' => array(
        '<basepath>/scripts/post_uninstall.php',
    ),
);
?>