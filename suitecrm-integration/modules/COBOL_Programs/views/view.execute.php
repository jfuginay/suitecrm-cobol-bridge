<?php
/**
 * Execute View for COBOL_Programs
 * 
 * Provides interface to execute COBOL programs
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/views/view.detail.php');

class COBOL_ProgramsViewExecute extends ViewDetail
{
    public function display()
    {
        global $mod_strings, $app_strings;
        
        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        
        // Get input parameter schema
        $inputSchema = json_decode($this->bean->input_parameters, true);
        $this->ss->assign('input_schema', $inputSchema);
        
        // Get recent executions
        $history = $this->bean->getExecutionHistory(5);
        $this->ss->assign('recent_executions', $history);
        
        // Handle form submission
        if (!empty($_POST['execute'])) {
            $this->executeProgram();
        }
        
        // Display template
        echo $this->ss->fetch('custom/modules/COBOL_Programs/tpls/execute.tpl');
    }
    
    protected function executeProgram()
    {
        $parameters = array();
        
        // Collect input parameters from POST
        foreach ($_POST as $key => $value) {
            if (strpos($key, 'param_') === 0) {
                $paramName = substr($key, 6);
                $parameters[$paramName] = $value;
            }
        }
        
        // Execute the program
        $result = $this->bean->execute($parameters);
        
        if ($result['success']) {
            SugarApplication::appendSuccessMessage('Program executed successfully');
            $this->ss->assign('execution_result', $result['response']);
        } else {
            SugarApplication::appendErrorMessage('Program execution failed: ' . $result['httpCode']);
            $this->ss->assign('execution_error', $result['response']);
        }
    }
}
?>