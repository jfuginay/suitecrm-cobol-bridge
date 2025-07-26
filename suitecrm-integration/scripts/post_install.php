<?php
/**
 * COBOL Bridge Post-Install Script
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

/**
 * Post install actions
 */
function post_install()
{
    global $sugar_config, $db;
    
    // Set default configuration values
    if (!isset($sugar_config['cobol_api_url'])) {
        require_once('modules/Configurator/Configurator.php');
        $configurator = new Configurator();
        $configurator->loadConfig();
        
        $configurator->config['cobol_api_url'] = 'http://localhost:3000/api/v1';
        $configurator->config['cobol_api_key'] = '';
        $configurator->config['cobol_websocket_enabled'] = true;
        $configurator->config['cobol_real_time_monitoring'] = true;
        $configurator->config['cobol_batch_job_timeout'] = 3600; // 1 hour
        $configurator->config['cobol_max_retry_attempts'] = 3;
        
        $configurator->saveConfig();
    }
    
    // Create custom tables for execution history
    $query = "CREATE TABLE IF NOT EXISTS cobol_execution_history (
        id CHAR(36) NOT NULL PRIMARY KEY,
        name VARCHAR(255),
        date_entered DATETIME,
        date_modified DATETIME,
        program_id CHAR(36) NOT NULL,
        input_parameters TEXT,
        output_response TEXT,
        http_code INT,
        execution_time FLOAT,
        status VARCHAR(50),
        error_message TEXT,
        deleted TINYINT(1) DEFAULT 0,
        INDEX idx_program_id (program_id, deleted),
        INDEX idx_date_entered (date_entered),
        INDEX idx_status (status, deleted)
    )";
    $db->query($query);
    
    // Create custom tables for COBOL calculation logs
    $query = "CREATE TABLE IF NOT EXISTS cobol_calculation_log (
        id CHAR(36) NOT NULL PRIMARY KEY,
        quote_id CHAR(36) NOT NULL,
        user_id CHAR(36) NOT NULL,
        calculation_date DATETIME NOT NULL,
        input_parameters TEXT,
        output_result TEXT,
        execution_time FLOAT,
        status VARCHAR(50),
        INDEX idx_quote_id (quote_id),
        INDEX idx_calculation_date (calculation_date)
    )";
    $db->query($query);
    
    // Create scheduled job for batch job monitoring
    require_once('modules/SchedulersJobs/SchedulersJob.php');
    require_once('modules/Schedulers/Scheduler.php');
    
    $scheduler = new Scheduler();
    $scheduler->name = 'COBOL Batch Job Monitor';
    $scheduler->job = 'function::monitorCOBOLBatchJobs';
    $scheduler->date_time_start = date('Y-m-d H:i:s');
    $scheduler->date_time_end = '2099-12-31 23:59:59';
    $scheduler->job_interval = '*/5::*::*::*::*'; // Every 5 minutes
    $scheduler->status = 'Active';
    $scheduler->created_by = '1';
    $scheduler->modified_user_id = '1';
    $scheduler->catch_up = 0;
    $scheduler->save();
    
    // Create sample COBOL programs
    createSamplePrograms();
    
    // Clear caches
    require_once('modules/Administration/QuickRepairAndRebuild.php');
    $repair = new RepairAndClear();
    $repair->repairAndClearAll(array('clearAll'), array(translate('LBL_ALL_MODULES')), false, true);
    
    echo "COBOL Bridge installation completed successfully!<br>";
    echo "Please configure the COBOL API settings in Admin > COBOL Bridge > API Configuration<br>";
}

/**
 * Create sample COBOL programs
 */
function createSamplePrograms()
{
    require_once('modules/COBOL_Programs/COBOL_Programs.php');
    
    // Credit Calculation Program
    $program = new COBOL_Programs();
    $program->name = 'Credit Approval Calculator';
    $program->program_type = 'credit';
    $program->status = 'active';
    $program->version = '1.0';
    $program->description = 'Calculates credit approval based on customer financial data';
    $program->input_parameters = json_encode(array(
        'customer_income' => 'number',
        'credit_amount' => 'number',
        'credit_term' => 'number',
        'existing_debt' => 'number',
    ));
    $program->output_format = json_encode(array(
        'approved' => 'boolean',
        'credit_limit' => 'number',
        'interest_rate' => 'number',
        'monthly_payment' => 'number',
    ));
    $program->program_code = file_get_contents(dirname(__FILE__) . '/../samples/credit_calculator.cob');
    $program->save();
    
    // Payroll Processing Program
    $program = new COBOL_Programs();
    $program->name = 'Payroll Processor';
    $program->program_type = 'payroll';
    $program->status = 'active';
    $program->version = '1.0';
    $program->description = 'Processes employee payroll including taxes and deductions';
    $program->input_parameters = json_encode(array(
        'employee_id' => 'string',
        'gross_salary' => 'number',
        'tax_rate' => 'number',
        'deductions' => 'array',
    ));
    $program->output_format = json_encode(array(
        'net_pay' => 'number',
        'tax_amount' => 'number',
        'total_deductions' => 'number',
    ));
    $program->save();
    
    // Interest Calculation Program
    $program = new COBOL_Programs();
    $program->name = 'Interest Calculator';
    $program->program_type = 'interest';
    $program->status = 'active';
    $program->version = '1.0';
    $program->description = 'Calculates compound interest for various financial products';
    $program->input_parameters = json_encode(array(
        'principal' => 'number',
        'rate' => 'number',
        'time' => 'number',
        'compound_frequency' => 'string',
    ));
    $program->output_format = json_encode(array(
        'interest' => 'number',
        'total_amount' => 'number',
        'effective_rate' => 'number',
    ));
    $program->save();
}

// Schedule the monitorCOBOLBatchJobs function
function monitorCOBOLBatchJobs()
{
    require_once('modules/COBOL_Jobs/COBOL_Jobs.php');
    
    global $db;
    
    // Get all running jobs
    $query = "SELECT id FROM cobol_jobs 
              WHERE status IN ('running', 'queued') 
              AND deleted = 0";
    
    $result = $db->query($query);
    
    while ($row = $db->fetchByAssoc($result)) {
        $job = new COBOL_Jobs();
        if ($job->retrieve($row['id'])) {
            // Update job status from API
            $job->updateStatus();
        }
    }
    
    return true;
}

// Execute post install
post_install();
?>