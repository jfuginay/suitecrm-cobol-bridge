<?php
/**
 * COBOL_Jobs Module
 * 
 * Manages batch job executions with real-time progress tracking
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

class COBOL_Jobs extends Basic
{
    public $new_schema = true;
    public $module_dir = 'COBOL_Jobs';
    public $object_name = 'COBOL_Jobs';
    public $table_name = 'cobol_jobs';
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
    public $job_type;
    public $job_id;
    public $status;
    public $progress;
    public $total_records;
    public $processed_records;
    public $failed_records;
    public $start_time;
    public $end_time;
    public $estimated_completion;
    public $input_parameters;
    public $output_log;
    public $error_log;
    public $priority;
    public $scheduled_time;
    public $retry_count;
    public $max_retries;
    public $parent_job_id;
    public $is_resumable;
    public $checkpoint_data;

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
     * Submit a new batch job
     */
    public function submit($jobType, $parameters = array())
    {
        global $sugar_config;
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/cobol/batch/process';
        
        $jobData = array(
            'jobType' => $jobType,
            'parameters' => $parameters,
            'priority' => $this->priority ?: 50,
            'suitecrm_job_id' => $this->id,
        );
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($jobData));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 202) {
            $result = json_decode($response, true);
            $this->job_id = $result['jobId'];
            $this->status = $result['status'];
            $this->job_type = $jobType;
            $this->input_parameters = json_encode($parameters);
            $this->start_time = date('Y-m-d H:i:s');
            $this->save();
            return true;
        }
        
        return false;
    }

    /**
     * Update job status from API
     */
    public function updateStatus()
    {
        global $sugar_config;
        
        if (empty($this->job_id)) {
            return false;
        }
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/cobol/batch/status/' . $this->job_id;
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 200) {
            $result = json_decode($response, true);
            $data = $result['data'];
            
            $this->status = $data['status'];
            $this->progress = $data['progress'];
            $this->total_records = $data['totalRecords'];
            $this->processed_records = $data['processedRecords'];
            $this->failed_records = $data['failedRecords'];
            
            if ($data['status'] == 'completed' || $data['status'] == 'failed') {
                $this->end_time = date('Y-m-d H:i:s');
            }
            
            if (!empty($data['estimatedCompletion'])) {
                $this->estimated_completion = date('Y-m-d H:i:s', strtotime($data['estimatedCompletion']));
            }
            
            if (!empty($data['output'])) {
                $this->output_log = json_encode($data['output']);
            }
            
            if (!empty($data['errors'])) {
                $this->error_log = json_encode($data['errors']);
            }
            
            $this->save();
            return true;
        }
        
        return false;
    }

    /**
     * Pause a running job
     */
    public function pause()
    {
        global $sugar_config;
        
        if (empty($this->job_id) || $this->status != 'running') {
            return false;
        }
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/cobol/batch/pause/' . $this->job_id;
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 200) {
            $this->status = 'paused';
            $this->save();
            return true;
        }
        
        return false;
    }

    /**
     * Resume a paused job
     */
    public function resume()
    {
        global $sugar_config;
        
        if (empty($this->job_id) || $this->status != 'paused') {
            return false;
        }
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/cobol/batch/resume/' . $this->job_id;
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 200) {
            $this->status = 'running';
            $this->save();
            return true;
        }
        
        return false;
    }

    /**
     * Cancel a job
     */
    public function cancel()
    {
        global $sugar_config;
        
        if (empty($this->job_id) || in_array($this->status, array('completed', 'failed', 'cancelled'))) {
            return false;
        }
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/cobol/batch/cancel/' . $this->job_id;
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 200) {
            $this->status = 'cancelled';
            $this->end_time = date('Y-m-d H:i:s');
            $this->save();
            return true;
        }
        
        return false;
    }

    /**
     * Get child jobs
     */
    public function getChildJobs()
    {
        global $db;
        
        $query = "SELECT * FROM cobol_jobs 
                  WHERE parent_job_id = '{$this->id}' AND deleted = 0 
                  ORDER BY date_entered ASC";
        
        $result = $db->query($query);
        $children = array();
        
        while ($row = $db->fetchByAssoc($result)) {
            $child = new COBOL_Jobs();
            $child->populateFromRow($row);
            $children[] = $child;
        }
        
        return $children;
    }
}
?>