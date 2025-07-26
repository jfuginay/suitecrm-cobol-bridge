<?php
/**
 * Batch Job Progress Dashlet
 * 
 * Real-time monitoring of batch job executions
 */

if (!defined('sugarEntry') || !sugarEntry) die('Not A Valid Entry Point');

require_once('include/Dashlets/DashletGeneric.php');

class BatchJobDashlet extends DashletGeneric
{
    public $displayColumns = array(
        'name',
        'job_type',
        'status',
        'progress',
        'start_time',
        'estimated_completion',
    );
    
    public function __construct($id, $def = null)
    {
        global $current_user, $app_strings;
        
        parent::__construct($id, $def);
        
        if (empty($def['title'])) {
            $this->title = translate('LBL_BATCH_JOB_DASHLET_TITLE', 'COBOL_Jobs');
        }
        
        $this->searchFields = array(
            'name' => array(),
            'job_type' => array(),
            'status' => array(),
        );
        
        $this->columns = array(
            'name' => array(
                'width' => '20%',
                'label' => 'LBL_NAME',
                'link' => true,
                'default' => true,
            ),
            'job_type' => array(
                'width' => '15%',
                'label' => 'LBL_JOB_TYPE',
                'default' => true,
            ),
            'status' => array(
                'width' => '10%',
                'label' => 'LBL_STATUS',
                'default' => true,
            ),
            'progress' => array(
                'width' => '15%',
                'label' => 'LBL_PROGRESS',
                'default' => true,
                'customCode' => '{$PROGRESS}%',
            ),
            'start_time' => array(
                'width' => '20%',
                'label' => 'LBL_START_TIME',
                'default' => true,
            ),
            'estimated_completion' => array(
                'width' => '20%',
                'label' => 'LBL_ESTIMATED_COMPLETION',
                'default' => true,
            ),
        );
    }
    
    public function display()
    {
        global $sugar_config;
        
        // Add real-time monitoring with progress bars
        $apiUrl = isset($sugar_config['cobol_api_url']) ? $sugar_config['cobol_api_url'] : 'http://localhost:3000/api/v1';
        
        $javascript = <<<EOQ
<style>
.batch-job-progress {
    width: 100%;
    height: 20px;
    background-color: #f0f0f0;
    border-radius: 10px;
    overflow: hidden;
    margin: 5px 0;
}

.batch-job-progress-bar {
    height: 100%;
    background-color: #4CAF50;
    transition: width 0.5s ease-in-out;
    text-align: center;
    line-height: 20px;
    color: white;
}

.batch-job-status-running {
    color: #2196F3;
    font-weight: bold;
}

.batch-job-status-completed {
    color: #4CAF50;
    font-weight: bold;
}

.batch-job-status-failed {
    color: #f44336;
    font-weight: bold;
}

.batch-job-status-paused {
    color: #FF9800;
    font-weight: bold;
}
</style>

<script type="text/javascript">
(function() {
    var dashletId = '{$this->id}';
    var apiUrl = '{$apiUrl}';
    var updateInterval;
    
    function updateProgressBars() {
        // Update progress bars for all running jobs
        var rows = document.querySelectorAll('#dashlet_entire_' + dashletId + ' .list tr');
        
        rows.forEach(function(row) {
            var progressCell = row.querySelector('td:nth-child(4)');
            if (progressCell && progressCell.textContent.includes('%')) {
                var progress = parseInt(progressCell.textContent);
                var progressHtml = '<div class="batch-job-progress">' +
                    '<div class="batch-job-progress-bar" style="width: ' + progress + '%">' +
                    progress + '%</div></div>';
                progressCell.innerHTML = progressHtml;
            }
            
            // Style status cells
            var statusCell = row.querySelector('td:nth-child(3)');
            if (statusCell) {
                var status = statusCell.textContent.trim().toLowerCase();
                statusCell.className = 'batch-job-status-' + status;
            }
        });
    }
    
    // WebSocket connection for real-time updates
    if (window.WebSocket) {
        var wsUrl = apiUrl.replace('http', 'ws').replace('/api/v1', '') + '/ws';
        var ws = new WebSocket(wsUrl);
        
        ws.onmessage = function(event) {
            var data = JSON.parse(event.data);
            if (data.type === 'job_update') {
                // Refresh dashlet on job updates
                SUGAR.mySugar.retrieveDashlet(dashletId);
            }
        };
    }
    
    // Update progress bars on load
    setTimeout(updateProgressBars, 100);
    
    // Periodic refresh every 10 seconds for active jobs
    updateInterval = setInterval(function() {
        var hasActiveJobs = false;
        var rows = document.querySelectorAll('#dashlet_entire_' + dashletId + ' .list tr');
        
        rows.forEach(function(row) {
            var statusCell = row.querySelector('td:nth-child(3)');
            if (statusCell) {
                var status = statusCell.textContent.trim().toLowerCase();
                if (status === 'running' || status === 'queued') {
                    hasActiveJobs = true;
                }
            }
        });
        
        if (hasActiveJobs) {
            SUGAR.mySugar.retrieveDashlet(dashletId);
        }
    }, 10000);
    
    // Clean up on dashlet removal
    YAHOO.util.Event.on(window, 'beforeunload', function() {
        if (updateInterval) {
            clearInterval(updateInterval);
        }
    });
})();
</script>
EOQ;
        
        return parent::display() . $javascript;
    }
    
    protected function buildWhere()
    {
        $where = parent::buildWhere();
        
        // Show only recent jobs by default
        if (!empty($where)) {
            $where .= ' AND ';
        }
        $where .= "(status IN ('running', 'queued', 'paused') OR 
                    DATE_SUB(NOW(), INTERVAL 24 HOUR) <= start_time)";
        
        return $where;
    }
    
    public function displayOptions()
    {
        $this->currentSearchFields = array('status' => array(''), 'job_type' => array(''));
        return parent::displayOptions();
    }
}
?>