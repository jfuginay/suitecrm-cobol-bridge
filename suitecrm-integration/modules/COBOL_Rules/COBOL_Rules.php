<?php
/**
 * COBOL_Rules Module
 * 
 * Manages business rules extracted from COBOL programs
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

class COBOL_Rules extends Basic
{
    public $new_schema = true;
    public $module_dir = 'COBOL_Rules';
    public $object_name = 'COBOL_Rules';
    public $table_name = 'cobol_rules';
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
    public $rule_type;
    public $source_program_id;
    public $rule_definition;
    public $visual_representation;
    public $conditions;
    public $actions;
    public $priority;
    public $status;
    public $version;
    public $test_cases;
    public $generated_cobol;
    public $is_modified;
    public $validation_status;

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
     * Generate COBOL code from the visual rule definition
     */
    public function generateCOBOL()
    {
        global $sugar_config;
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/rules/generate-cobol';
        
        $ruleData = array(
            'name' => $this->name,
            'type' => $this->rule_type,
            'conditions' => json_decode($this->conditions, true),
            'actions' => json_decode($this->actions, true),
            'priority' => $this->priority,
        );
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($ruleData));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 200) {
            $result = json_decode($response, true);
            $this->generated_cobol = $result['cobol'];
            $this->is_modified = false;
            $this->save();
            return true;
        }
        
        return false;
    }

    /**
     * Validate the rule against test cases
     */
    public function validate()
    {
        global $sugar_config;
        
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $apiKey = isset($sugar_config['cobol_api_key']) ? $sugar_config['cobol_api_key'] : '';
        
        $endpoint = $apiUrl . '/rules/validate';
        
        $validationData = array(
            'rule' => array(
                'conditions' => json_decode($this->conditions, true),
                'actions' => json_decode($this->actions, true),
            ),
            'test_cases' => json_decode($this->test_cases, true),
        );
        
        $ch = curl_init($endpoint);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
        curl_setopt($ch, CURLOPT_POST, true);
        curl_setopt($ch, CURLOPT_POSTFIELDS, json_encode($validationData));
        curl_setopt($ch, CURLOPT_HTTPHEADER, array(
            'Content-Type: application/json',
            'Authorization: Bearer ' . $apiKey
        ));
        
        $response = curl_exec($ch);
        $httpCode = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        
        if ($httpCode == 200) {
            $result = json_decode($response, true);
            $this->validation_status = $result['status'];
            $this->save();
            return $result;
        }
        
        return array('status' => 'error', 'message' => 'Validation failed');
    }

    /**
     * Export rule as JSON for visual editor
     */
    public function exportForEditor()
    {
        return array(
            'id' => $this->id,
            'name' => $this->name,
            'type' => $this->rule_type,
            'conditions' => json_decode($this->conditions, true),
            'actions' => json_decode($this->actions, true),
            'visual' => json_decode($this->visual_representation, true),
            'priority' => $this->priority,
            'test_cases' => json_decode($this->test_cases, true),
        );
    }

    /**
     * Import rule from visual editor
     */
    public function importFromEditor($data)
    {
        if (isset($data['name'])) $this->name = $data['name'];
        if (isset($data['type'])) $this->rule_type = $data['type'];
        if (isset($data['conditions'])) $this->conditions = json_encode($data['conditions']);
        if (isset($data['actions'])) $this->actions = json_encode($data['actions']);
        if (isset($data['visual'])) $this->visual_representation = json_encode($data['visual']);
        if (isset($data['priority'])) $this->priority = $data['priority'];
        if (isset($data['test_cases'])) $this->test_cases = json_encode($data['test_cases']);
        
        $this->is_modified = true;
        $this->validation_status = 'pending';
        
        return $this->save();
    }
}
?>