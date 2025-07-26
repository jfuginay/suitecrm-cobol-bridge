<?php
/**
 * COBOL_Execution_History
 * 
 * Tracks execution history for COBOL programs
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

class COBOL_Execution_History extends Basic
{
    public $new_schema = true;
    public $module_dir = 'COBOL_Programs';
    public $object_name = 'COBOL_Execution_History';
    public $table_name = 'cobol_execution_history';
    public $importable = false;
    
    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $program_id;
    public $input_parameters;
    public $output_response;
    public $http_code;
    public $execution_time;
    public $status;
    public $error_message;
    public $deleted;
    
    public function __construct()
    {
        parent::__construct();
    }
    
    /**
     * Get related program
     */
    public function getProgram()
    {
        if (!empty($this->program_id)) {
            require_once('modules/COBOL_Programs/COBOL_Programs.php');
            $program = new COBOL_Programs();
            $program->retrieve($this->program_id);
            return $program;
        }
        return null;
    }
}
?>