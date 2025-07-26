<?php
/**
 * COBOL Bridge Post-Uninstall Script
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

/**
 * Post uninstall actions
 */
function post_uninstall()
{
    global $sugar_config, $db;
    
    // Remove configuration values
    require_once('modules/Configurator/Configurator.php');
    $configurator = new Configurator();
    $configurator->loadConfig();
    
    unset($configurator->config['cobol_api_url']);
    unset($configurator->config['cobol_api_key']);
    unset($configurator->config['cobol_websocket_enabled']);
    unset($configurator->config['cobol_real_time_monitoring']);
    unset($configurator->config['cobol_batch_job_timeout']);
    unset($configurator->config['cobol_max_retry_attempts']);
    
    $configurator->saveConfig();
    
    // Remove scheduled job
    $query = "UPDATE schedulers SET deleted = 1 
              WHERE job = 'function::monitorCOBOLBatchJobs' 
              AND deleted = 0";
    $db->query($query);
    
    // Note: We don't drop custom tables by default to preserve data
    // Uncomment below if you want to remove all data on uninstall
    
    /*
    // Drop custom tables
    $db->query("DROP TABLE IF EXISTS cobol_execution_history");
    $db->query("DROP TABLE IF EXISTS cobol_calculation_log");
    */
    
    // Clear caches
    require_once('modules/Administration/QuickRepairAndRebuild.php');
    $repair = new RepairAndClear();
    $repair->repairAndClearAll(array('clearAll'), array(translate('LBL_ALL_MODULES')), false, true);
    
    echo "COBOL Bridge uninstallation completed.<br>";
    echo "Note: Custom tables have been preserved. Drop them manually if needed.<br>";
}

// Execute post uninstall
post_uninstall();
?>