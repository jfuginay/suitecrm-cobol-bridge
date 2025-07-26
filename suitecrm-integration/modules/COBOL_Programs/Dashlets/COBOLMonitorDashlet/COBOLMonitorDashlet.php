<?php
/**
 * COBOL Monitor Dashlet
 * 
 * Real-time monitoring of COBOL program executions
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/Dashlets/DashletGeneric.php');

class COBOLMonitorDashlet extends DashletGeneric
{
    public $displayColumns = array(
        'name',
        'program_type',
        'status',
        'last_execution',
        'execution_count',
        'success_rate',
    );
    
    public function __construct($id, $def = null)
    {
        global $current_user, $app_strings;
        
        parent::__construct($id, $def);
        
        if (empty($def['title'])) {
            $this->title = translate('LBL_COBOL_MONITOR_TITLE', 'COBOL_Programs');
        }
        
        $this->searchFields = array(
            'name' => array(),
            'program_type' => array(),
            'status' => array(),
        );
        
        $this->columns = array(
            'name' => array(
                'width' => '20%',
                'label' => 'LBL_NAME',
                'link' => true,
                'default' => true,
            ),
            'program_type' => array(
                'width' => '15%',
                'label' => 'LBL_PROGRAM_TYPE',
                'default' => true,
            ),
            'status' => array(
                'width' => '10%',
                'label' => 'LBL_STATUS',
                'default' => true,
            ),
            'last_execution' => array(
                'width' => '20%',
                'label' => 'LBL_LAST_EXECUTION',
                'default' => true,
            ),
            'execution_count' => array(
                'width' => '15%',
                'label' => 'LBL_EXECUTION_COUNT',
                'default' => true,
            ),
            'success_rate' => array(
                'width' => '20%',
                'label' => 'LBL_SUCCESS_RATE',
                'default' => true,
                'customCode' => '{$SUCCESS_RATE|number_format:1}%',
            ),
        );
    }
    
    public function display()
    {
        global $sugar_config;
        
        // Add real-time monitoring JavaScript
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        
        $javascript = <<<EOQ
<script type="text/javascript">
(function() {
    var dashletId = '{$this->id}';
    var apiUrl = '{$apiUrl}';
    
    // WebSocket connection for real-time updates
    if (window.WebSocket) {
        var wsUrl = apiUrl.replace('http', 'ws').replace('/api/v1', '') + '/ws';
        var ws = new WebSocket(wsUrl);
        
        ws.onmessage = function(event) {
            var data = JSON.parse(event.data);
            if (data.type === 'execution_update') {
                // Refresh dashlet on execution updates
                SUGAR.mySugar.retrieveDashlet(dashletId);
            }
        };
    }
    
    // Periodic refresh every 30 seconds
    setInterval(function() {
        SUGAR.mySugar.retrieveDashlet(dashletId);
    }, 30000);
})();
</script>
EOQ;
        
        return parent::display() . $javascript;
    }
    
    protected function buildWhere()
    {
        $where = parent::buildWhere();
        
        // Only show active programs by default
        if (empty($this->filters['status'])) {
            if (!empty($where)) {
                $where .= ' AND ';
            }
            $where .= "status = 'active'";
        }
        
        return $where;
    }
}
?>