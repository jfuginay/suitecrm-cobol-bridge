<?php
/**
 * Visual Editor View for COBOL_Rules
 * 
 * Provides visual interface for editing business rules
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/MVC/View/views/view.detail.php');

class COBOL_RulesViewVisualEditor extends ViewDetail
{
    public function display()
    {
        global $mod_strings, $app_strings, $sugar_config;
        
        $this->ss->assign('MOD', $mod_strings);
        $this->ss->assign('APP', $app_strings);
        
        // Get API URL for React app
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        $businessRulesUrl = str_replace(':3000', ':3001', $apiUrl); // Business rules app on different port
        
        $this->ss->assign('business_rules_url', $businessRulesUrl);
        $this->ss->assign('rule_data', $this->bean->exportForEditor());
        
        // Handle save from visual editor
        if (!empty($_POST['rule_data'])) {
            $this->saveFromEditor();
        }
        
        // Include React app styles
        echo '<link rel="stylesheet" href="' . $businessRulesUrl . '/static/css/main.css">';
        
        // Display template
        echo $this->ss->fetch('custom/modules/COBOL_Rules/tpls/visualeditor.tpl');
        
        // Include React app script
        echo '<script src="' . $businessRulesUrl . '/static/js/bundle.js"></script>';
        echo '<script>
            window.COBOL_RULE_DATA = ' . json_encode($this->bean->exportForEditor()) . ';
            window.COBOL_API_URL = "' . $apiUrl . '";
            window.SUGAR_URL = "' . $GLOBALS['sugar_config']['site_url'] . '";
            window.RULE_ID = "' . $this->bean->id . '";
        </script>';
    }
    
    protected function saveFromEditor()
    {
        $ruleData = json_decode($_POST['rule_data'], true);
        
        if ($this->bean->importFromEditor($ruleData)) {
            SugarApplication::appendSuccessMessage('Rule saved successfully');
            
            // Generate COBOL if requested
            if (!empty($_POST['generate_cobol'])) {
                if ($this->bean->generateCOBOL()) {
                    SugarApplication::appendSuccessMessage('COBOL code generated successfully');
                } else {
                    SugarApplication::appendErrorMessage('Failed to generate COBOL code');
                }
            }
            
            // Redirect to detail view
            SugarApplication::redirect('index.php?module=COBOL_Rules&action=DetailView&record=' . $this->bean->id);
        } else {
            SugarApplication::appendErrorMessage('Failed to save rule');
        }
    }
}
?>