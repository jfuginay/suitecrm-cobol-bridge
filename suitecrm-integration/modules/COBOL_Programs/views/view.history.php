<?php
/**
 * History View for COBOL_Programs
 * 
 * Shows execution history for a COBOL program
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/views/view.detail.php');

class COBOL_ProgramsViewHistory extends ViewDetail
{
    public function display()
    {
        global $mod_strings, $app_strings, $timedate;
        
        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        
        // Get execution history
        $history = $this->bean->getExecutionHistory(50);
        
        // Process history records
        foreach ($history as &$record) {
            $record['input_parameters'] = json_decode($record['input_parameters'], true);
            $record['output_response'] = json_decode($record['output_response'], true);
            $record['date_entered_formatted'] = $timedate->to_display_date_time($record['date_entered']);
            
            // Calculate execution time
            if (!empty($record['execution_time'])) {
                $record['execution_time_formatted'] = number_format($record['execution_time'], 3) . ' seconds';
            }
        }
        
        $this->ss->assign('execution_history', $history);
        $this->ss->assign('program', $this->bean);
        
        // Display template
        echo $this->ss->fetch('custom/modules/COBOL_Programs/tpls/history.tpl');
    }
}
?>