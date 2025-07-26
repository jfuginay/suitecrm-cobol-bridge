<?php
/**
 * COBOL_Programs Module
 * 
 * Manages COBOL programs and their execution history
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

class COBOL_Programs extends Basic
{
    public $new_schema = true;
    public $module_dir = 'COBOL_Programs';
    public $object_name = 'COBOL_Programs';
    public $table_name = 'cobol_programs';
    public $importable = true;
    public $disable_row_level_security = true;

    public $id;
    public $name;
    public $date_entered;
    public $date_modified;
    public $modified_user_id;
    public $modified_by_name;
    public $created_by;
    public $created_by_name;
    public $description;
    public $deleted;
    public $created_by_link;
    public $modified_user_link;
    public $assigned_user_id;
    public $assigned_user_name;
    public $assigned_user_link;

    // Custom fields
    public $program_code;
    public $program_type;
    public $last_execution;
    public $execution_count;
    public $average_runtime;
    public $status;
    public $version;
    public $input_parameters;
    public $output_format;
    public $error_count;
    public $success_rate;

    public function __construct()
    {
        parent::__construct();
    }

    public function bean_implements($interface)
    {
        switch ($interface) {
            case 'ACL':
                return true;
        }
        return false;
    }

    /**
     * Execute a COBOL program through the API Gateway
     */
    public function execute($parameters = array())
    {
        global $sugar_config;
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/cobol/' . strtolower($this->program_type) . '/calculate';
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($parameters));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        // Log execution
        $this->logExecution($parameters, $response, $httpCode);
        
        return array(
            'success' => $httpCode == 200,
            'response' => json_decode($response, true),
            'httpCode' => $httpCode
        );
    }

    /**
     * Log program execution
     */
    protected function logExecution($parameters, $response, $httpCode)
    {
        require_once('modules/COBOL_Programs/COBOL_Execution_History.php');
        
        $history = new COBOL_Execution_History();
        $history->program_id = $this->id;
        $history->name = 'Execution at ' . date('Y-m-d H:i:s');
        $history->input_parameters = json_encode($parameters);
        $history->output_response = $response;
        $history->http_code = $httpCode;
        $history->execution_time = microtime(true);
        $history->status = ($httpCode == 200) ? 'success' : 'failed';
        $history->save();
        
        // Update program statistics
        $this->last_execution = date('Y-m-d H:i:s');
        $this->execution_count = (int)$this->execution_count + 1;
        if ($httpCode != 200) {
            $this->error_count = (int)$this->error_count + 1;
        }
        $this->success_rate = (($this->execution_count - $this->error_count) / $this->execution_count) * 100;
        $this->save();
    }

    /**
     * Get execution history for this program
     */
    public function getExecutionHistory($limit = 10)
    {
        global $db;
        
        $query = "SELECT * FROM cobol_execution_history 
                  WHERE program_id = '{$this->id}' AND deleted = 0 
                  ORDER BY date_entered DESC 
                  LIMIT {$limit}";
        
        $result = $db->query($query);
        $history = array();
        
        while ($row = $db->fetchByAssoc($result)) {
            $history[] = $row;
        }
        
        return $history;
    }
}
?>