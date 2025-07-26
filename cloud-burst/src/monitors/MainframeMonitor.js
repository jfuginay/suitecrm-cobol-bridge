const EventEmitter = require('events');
const ibmdb = require('ibm_db');
const { SSH } = require('node-ssh');
const logger = require('../utils/logger');
const config = require('../config/config');

class MainframeMonitor extends EventEmitter {
  constructor(options = {}) {
    super();
    
    this.options = {
      host: process.env.MAINFRAME_HOST || 'mainframe.company.com',
      port: process.env.MAINFRAME_SSH_PORT || 22,
      username: process.env.MAINFRAME_USER,
      password: process.env.MAINFRAME_PASSWORD,
      db2Config: {
        database: process.env.DB2_DATABASE || 'PRODDB',
        hostname: process.env.DB2_HOST || 'mainframe.company.com',
        port: process.env.DB2_PORT || 50000,
        uid: process.env.DB2_USER,
        pwd: process.env.DB2_PASSWORD,
        protocol: 'TCPIP'
      },
      pollInterval: 10000, // 10 seconds
      thresholds: {
        cpuHigh: 85,
        memoryHigh: 90,
        queueDepthHigh: 100
      },
      ...options
    };
    
    this.ssh = new SSH();
    this.db2Connection = null;
    this.isConnected = false;
    this.pollTimer = null;
    
    this.metrics = {
      lastCheck: null,
      consecutiveFailures: 0,
      totalChecks: 0
    };
  }
  
  async connect() {
    logger.info('Connecting to mainframe...');
    
    try {
      // Establish SSH connection
      await this.ssh.connect({
        host: this.options.host,
        port: this.options.port,
        username: this.options.username,
        password: this.options.password
      });
      
      // Establish DB2 connection
      const db2ConnStr = this._buildDB2ConnectionString();
      this.db2Connection = await this._connectDB2(db2ConnStr);
      
      this.isConnected = true;
      logger.info('Successfully connected to mainframe');
      
      // Start monitoring
      this._startMonitoring();
      
      this.emit('connected');
    } catch (error) {
      logger.error('Failed to connect to mainframe:', error);
      throw error;
    }
  }
  
  async disconnect() {
    logger.info('Disconnecting from mainframe...');
    
    // Stop monitoring
    if (this.pollTimer) {
      clearInterval(this.pollTimer);
      this.pollTimer = null;
    }
    
    // Close connections
    if (this.db2Connection) {
      try {
        await this.db2Connection.close();
      } catch (error) {
        logger.error('Error closing DB2 connection:', error);
      }
    }
    
    if (this.ssh.isConnected()) {
      this.ssh.dispose();
    }
    
    this.isConnected = false;
    this.emit('disconnected');
  }
  
  _buildDB2ConnectionString() {
    const { database, hostname, port, uid, pwd, protocol } = this.options.db2Config;
    return `DATABASE=${database};HOSTNAME=${hostname};PORT=${port};PROTOCOL=${protocol};UID=${uid};PWD=${pwd};`;
  }
  
  async _connectDB2(connectionString) {
    return new Promise((resolve, reject) => {
      ibmdb.open(connectionString, (err, conn) => {
        if (err) {
          reject(err);
        } else {
          resolve(conn);
        }
      });
    });
  }
  
  _startMonitoring() {
    this.pollTimer = setInterval(async () => {
      try {
        await this._performCheck();
        this.metrics.consecutiveFailures = 0;
      } catch (error) {
        logger.error('Monitoring check failed:', error);
        this.metrics.consecutiveFailures++;
        
        if (this.metrics.consecutiveFailures >= 3) {
          this.emit('connection_lost', { error });
          // Attempt reconnection
          await this._attemptReconnect();
        }
      }
    }, this.options.pollInterval);
  }
  
  async _performCheck() {
    this.metrics.totalChecks++;
    this.metrics.lastCheck = new Date();
    
    const status = await this.getSystemStatus();
    
    // Check thresholds
    const alerts = [];
    
    if (status.cpu.utilization > this.options.thresholds.cpuHigh) {
      alerts.push({ type: 'cpu_high', value: status.cpu.utilization });
    }
    
    if (status.memory.utilization > this.options.thresholds.memoryHigh) {
      alerts.push({ type: 'memory_high', value: status.memory.utilization });
    }
    
    if (status.batchQueue.depth > this.options.thresholds.queueDepthHigh) {
      alerts.push({ type: 'queue_high', value: status.batchQueue.depth });
    }
    
    // Emit events
    this.emit('queue_status', status);
    
    if (alerts.length > 0) {
      this.emit('high_load', { alerts, status });
    }
  }
  
  async getSystemStatus() {
    const [cpuStatus, memoryStatus, queueStatus, jobStatus] = await Promise.all([
      this._getCPUStatus(),
      this._getMemoryStatus(),
      this._getQueueStatus(),
      this._getJobStatus()
    ]);
    
    return {
      timestamp: new Date().toISOString(),
      cpu: cpuStatus,
      memory: memoryStatus,
      batchQueue: queueStatus,
      jobs: jobStatus
    };
  }
  
  async _getCPUStatus() {
    // Execute MVS command to get CPU utilization
    const result = await this.ssh.execCommand('D M=CPU');
    
    // Parse the output (simplified - actual parsing would be more complex)
    const lines = result.stdout.split('\n');
    let totalUtilization = 0;
    let cpuCount = 0;
    
    for (const line of lines) {
      if (line.includes('CPU')) {
        const match = line.match(/(\d+)%/);
        if (match) {
          totalUtilization += parseInt(match[1]);
          cpuCount++;
        }
      }
    }
    
    return {
      utilization: cpuCount > 0 ? totalUtilization / cpuCount : 0,
      count: cpuCount,
      raw: result.stdout
    };
  }
  
  async _getMemoryStatus() {
    // Execute MVS command to get memory status
    const result = await this.ssh.execCommand('D M=STOR');
    
    // Parse memory information
    const match = result.stdout.match(/AVAILABLE:\s*(\d+)M.*USED:\s*(\d+)M/);
    
    if (match) {
      const available = parseInt(match[1]);
      const used = parseInt(match[2]);
      const total = available + used;
      
      return {
        utilization: (used / total) * 100,
        available: available,
        used: used,
        total: total
      };
    }
    
    return {
      utilization: 0,
      available: 0,
      used: 0,
      total: 0
    };
  }
  
  async _getQueueStatus() {
    // Query JES2 for job queue information
    const query = `
      SELECT 
        COUNT(*) as total_jobs,
        SUM(CASE WHEN STATUS = 'WAITING' THEN 1 ELSE 0 END) as waiting,
        SUM(CASE WHEN STATUS = 'EXECUTING' THEN 1 ELSE 0 END) as executing,
        AVG(WAIT_TIME) as avg_wait_time
      FROM SYSIBM.SYSJOBS
      WHERE SUBSYSTEM = 'JES2'
    `;
    
    try {
      const result = await this._queryDB2(query);
      
      return {
        depth: result[0].TOTAL_JOBS || 0,
        waiting: result[0].WAITING || 0,
        executing: result[0].EXECUTING || 0,
        avgWaitTime: result[0].AVG_WAIT_TIME || 0,
        maxDepth: 1000 // Configurable
      };
    } catch (error) {
      logger.error('Failed to query queue status:', error);
      return {
        depth: 0,
        waiting: 0,
        executing: 0,
        avgWaitTime: 0,
        maxDepth: 1000
      };
    }
  }
  
  async _getJobStatus() {
    // Get detailed job information
    const query = `
      SELECT 
        JOBNAME,
        JOBID,
        STATUS,
        PRIORITY,
        CPU_TIME,
        ELAPSED_TIME,
        PROGRAM_NAME
      FROM SYSIBM.SYSJOBS
      WHERE STATUS IN ('WAITING', 'EXECUTING')
      ORDER BY PRIORITY DESC, SUBMIT_TIME ASC
      FETCH FIRST 100 ROWS ONLY
    `;
    
    try {
      const jobs = await this._queryDB2(query);
      
      return {
        active: jobs.filter(j => j.STATUS === 'EXECUTING'),
        waiting: jobs.filter(j => j.STATUS === 'WAITING'),
        total: jobs.length
      };
    } catch (error) {
      logger.error('Failed to query job status:', error);
      return {
        active: [],
        waiting: [],
        total: 0
      };
    }
  }
  
  async getEligibleJobs() {
    // Query for jobs that can be offloaded to cloud
    const query = `
      SELECT 
        j.JOBID,
        j.JOBNAME,
        j.PROGRAM_NAME,
        j.PRIORITY,
        j.ESTIMATED_CPU,
        j.ESTIMATED_MEMORY,
        p.CLOUD_ELIGIBLE,
        p.PARALLEL_SAFE,
        p.DATA_REQUIREMENTS
      FROM SYSIBM.SYSJOBS j
      JOIN SYSIBM.SYSPROGRAMS p ON j.PROGRAM_NAME = p.PROGRAM_NAME
      WHERE j.STATUS = 'WAITING'
        AND p.CLOUD_ELIGIBLE = 'Y'
        AND p.PARALLEL_SAFE = 'Y'
      ORDER BY j.PRIORITY DESC, j.SUBMIT_TIME ASC
    `;
    
    try {
      const jobs = await this._queryDB2(query);
      
      // Filter and prepare jobs for cloud execution
      return jobs.map(job => ({
        id: job.JOBID,
        name: job.JOBNAME,
        program: job.PROGRAM_NAME,
        priority: job.PRIORITY,
        requirements: {
          cpu: job.ESTIMATED_CPU || 1,
          memory: job.ESTIMATED_MEMORY || 512,
          data: this._parseDataRequirements(job.DATA_REQUIREMENTS)
        },
        metadata: {
          submittedAt: new Date(),
          source: 'mainframe'
        }
      }));
    } catch (error) {
      logger.error('Failed to get eligible jobs:', error);
      return [];
    }
  }
  
  _parseDataRequirements(requirements) {
    if (!requirements) return [];
    
    try {
      return JSON.parse(requirements);
    } catch {
      // Fallback to simple parsing
      return requirements.split(',').map(r => r.trim());
    }
  }
  
  async returnJob(jobId, error) {
    // Return failed job to mainframe queue
    const updateQuery = `
      UPDATE SYSIBM.SYSJOBS
      SET STATUS = 'WAITING',
          CLOUD_ATTEMPT_COUNT = CLOUD_ATTEMPT_COUNT + 1,
          LAST_ERROR = ?,
          LAST_ERROR_TIME = CURRENT_TIMESTAMP
      WHERE JOBID = ?
    `;
    
    try {
      await this._executeDB2(updateQuery, [error.message || 'Unknown error', jobId]);
      logger.info(`Job ${jobId} returned to mainframe queue`);
    } catch (err) {
      logger.error(`Failed to return job ${jobId}:`, err);
      throw err;
    }
  }
  
  async notifyBatchComplete(batchId) {
    // Update batch status in mainframe
    const updateQuery = `
      UPDATE SYSIBM.SYSBATCHES
      SET STATUS = 'COMPLETED',
          COMPLETION_TIME = CURRENT_TIMESTAMP,
          CLOUD_PROCESSED = 'Y'
      WHERE BATCH_ID = ?
    `;
    
    try {
      await this._executeDB2(updateQuery, [batchId]);
      
      // Trigger any dependent jobs
      await this._triggerDependentJobs(batchId);
      
      logger.info(`Batch ${batchId} marked as complete on mainframe`);
    } catch (error) {
      logger.error(`Failed to notify batch completion for ${batchId}:`, error);
      throw error;
    }
  }
  
  async _triggerDependentJobs(batchId) {
    const query = `
      SELECT JOBID
      FROM SYSIBM.SYSJOB_DEPENDENCIES
      WHERE DEPENDS_ON_BATCH = ?
        AND STATUS = 'WAITING_DEPENDENCY'
    `;
    
    const dependentJobs = await this._queryDB2(query, [batchId]);
    
    for (const job of dependentJobs) {
      await this._executeDB2(
        'UPDATE SYSIBM.SYSJOBS SET STATUS = \'WAITING\' WHERE JOBID = ?',
        [job.JOBID]
      );
    }
  }
  
  async _queryDB2(query, params = []) {
    return new Promise((resolve, reject) => {
      this.db2Connection.query(query, params, (err, data) => {
        if (err) {
          reject(err);
        } else {
          resolve(data);
        }
      });
    });
  }
  
  async _executeDB2(query, params = []) {
    return new Promise((resolve, reject) => {
      this.db2Connection.prepare(query, (err, stmt) => {
        if (err) {
          reject(err);
          return;
        }
        
        stmt.execute(params, (err, result) => {
          if (err) {
            reject(err);
          } else {
            resolve(result);
          }
          stmt.close();
        });
      });
    });
  }
  
  async _attemptReconnect() {
    logger.info('Attempting to reconnect to mainframe...');
    
    try {
      await this.disconnect();
      await new Promise(resolve => setTimeout(resolve, 5000)); // Wait 5 seconds
      await this.connect();
      logger.info('Successfully reconnected to mainframe');
    } catch (error) {
      logger.error('Failed to reconnect:', error);
      this.emit('reconnect_failed', { error });
    }
  }
}

module.exports = { MainframeMonitor };