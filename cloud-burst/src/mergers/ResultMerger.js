const EventEmitter = require('events');
const Bull = require('bull');
const { DB2Connector } = require('../connectors/DB2Connector');
const logger = require('../utils/logger');
const config = require('../config/config');

class ResultMerger extends EventEmitter {
  constructor(options = {}) {
    super();
    
    this.options = {
      batchTimeout: 60000, // 1 minute
      maxBatchSize: 100,
      mergeStrategy: 'sequential', // sequential, parallel, sorted
      tempDatasetPrefix: 'CLOUD.TEMP',
      finalDatasetPrefix: 'CLOUD.RESULTS',
      cleanupTempDatasets: true,
      retryAttempts: 3,
      retryDelay: 5000,
      ...options
    };
    
    this.resultQueue = null;
    this.db2Connector = null;
    this.activeBatches = new Map();
    this.completedBatches = new Map();
    this.isInitialized = false;
    
    this.metrics = {
      totalBatches: 0,
      completedBatches: 0,
      failedBatches: 0,
      totalRecordsMerged: 0,
      avgMergeTime: 0
    };
  }
  
  async initialize() {
    logger.info('Initializing Result Merger...');
    
    try {
      // Initialize DB2 connector
      this.db2Connector = new DB2Connector();
      await this.db2Connector.initialize();
      
      // Initialize result queue
      this.resultQueue = new Bull('merge-queue', {
        redis: {
          port: 6379,
          host: process.env.REDIS_URL?.split('://')[1]?.split(':')[0] || 'localhost'
        }
      });
      
      // Setup queue processors
      this._setupQueueProcessors();
      
      // Start batch monitoring
      this._startBatchMonitoring();
      
      this.isInitialized = true;
      logger.info('Result Merger initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize Result Merger:', error);
      throw error;
    }
  }
  
  _setupQueueProcessors() {
    // Process individual results
    this.resultQueue.process('result', async (job) => {
      return await this._processResult(job.data);
    });
    
    // Process batch merges
    this.resultQueue.process('batch-merge', async (job) => {
      return await this._processBatchMerge(job.data);
    });
    
    // Handle events
    this.resultQueue.on('completed', (job, result) => {
      logger.debug(`Queue job ${job.id} completed`);
    });
    
    this.resultQueue.on('failed', (job, err) => {
      logger.error(`Queue job ${job.id} failed:`, err);
    });
  }
  
  _startBatchMonitoring() {
    // Check for completed batches periodically
    this.batchMonitor = setInterval(() => {
      this._checkBatches();
    }, 10000); // Every 10 seconds
  }
  
  async queueResult(resultData) {
    const { jobId, result, workerId, completedAt } = resultData;
    
    logger.info(`Queueing result for job ${jobId}`);
    
    try {
      // Determine batch ID
      const batchId = this._getBatchId(resultData.jobData);
      
      // Add to result queue
      await this.resultQueue.add('result', {
        jobId,
        batchId,
        result,
        workerId,
        completedAt,
        jobData: resultData.jobData
      });
      
      // Track batch
      this._trackBatch(batchId, jobId);
      
    } catch (error) {
      logger.error(`Failed to queue result for job ${jobId}:`, error);
      throw error;
    }
  }
  
  _getBatchId(jobData) {
    // Extract batch ID from job data
    // This could be based on various criteria:
    // - Explicit batch ID in job data
    // - Job submission time window
    // - Job type/program
    
    if (jobData.batchId) {
      return jobData.batchId;
    }
    
    // Create batch ID based on program and time window
    const timeWindow = Math.floor(Date.now() / (5 * 60 * 1000)); // 5-minute windows
    return `${jobData.program}_${timeWindow}`;
  }
  
  _trackBatch(batchId, jobId) {
    if (!this.activeBatches.has(batchId)) {
      this.activeBatches.set(batchId, {
        id: batchId,
        jobs: new Set(),
        results: new Map(),
        startTime: new Date(),
        status: 'collecting'
      });
      
      this.metrics.totalBatches++;
    }
    
    const batch = this.activeBatches.get(batchId);
    batch.jobs.add(jobId);
  }
  
  async _processResult(resultData) {
    const { jobId, batchId, result, workerId } = resultData;
    
    logger.debug(`Processing result for job ${jobId} in batch ${batchId}`);
    
    try {
      // Store result temporarily
      const tempDataset = await this._storeTemporaryResult(jobId, result);
      
      // Update batch tracking
      const batch = this.activeBatches.get(batchId);
      if (batch) {
        batch.results.set(jobId, {
          tempDataset,
          workerId,
          storedAt: new Date()
        });
        
        // Check if batch is complete
        if (batch.results.size === batch.jobs.size) {
          await this._triggerBatchMerge(batchId);
        }
      }
      
      return { success: true, tempDataset };
    } catch (error) {
      logger.error(`Failed to process result for job ${jobId}:`, error);
      throw error;
    }
  }
  
  async _storeTemporaryResult(jobId, result) {
    const tempDatasetName = `${this.options.tempDatasetPrefix}.J${jobId}`;
    
    try {
      // Create temporary dataset
      await this.db2Connector.query(`
        CREATE TABLE ${tempDatasetName} (
          RECORD_NUM INTEGER NOT NULL,
          RECORD_DATA VARCHAR(32000),
          CREATED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (RECORD_NUM)
        )
      `);
      
      // Store results
      if (Array.isArray(result)) {
        // Batch insert for array results
        const records = result.map((data, index) => [
          index + 1,
          typeof data === 'string' ? data : JSON.stringify(data)
        ]);
        
        await this.db2Connector.batchInsert(
          tempDatasetName,
          ['RECORD_NUM', 'RECORD_DATA'],
          records
        );
      } else if (typeof result === 'string') {
        // Single record for string results
        await this.db2Connector.query(
          `INSERT INTO ${tempDatasetName} (RECORD_NUM, RECORD_DATA) VALUES (?, ?)`,
          [1, result]
        );
      } else {
        // JSON stringify for objects
        await this.db2Connector.query(
          `INSERT INTO ${tempDatasetName} (RECORD_NUM, RECORD_DATA) VALUES (?, ?)`,
          [1, JSON.stringify(result)]
        );
      }
      
      logger.debug(`Stored temporary result in ${tempDatasetName}`);
      return tempDatasetName;
      
    } catch (error) {
      logger.error(`Failed to store temporary result for job ${jobId}:`, error);
      throw error;
    }
  }
  
  async _triggerBatchMerge(batchId) {
    logger.info(`Triggering merge for batch ${batchId}`);
    
    const batch = this.activeBatches.get(batchId);
    if (!batch) {
      logger.error(`Batch ${batchId} not found`);
      return;
    }
    
    batch.status = 'merging';
    
    // Queue batch merge job
    await this.resultQueue.add('batch-merge', {
      batchId,
      jobs: Array.from(batch.jobs),
      results: Array.from(batch.results.entries()),
      strategy: this.options.mergeStrategy
    });
  }
  
  async _processBatchMerge(mergeData) {
    const { batchId, jobs, results, strategy } = mergeData;
    
    logger.info(`Merging batch ${batchId} with ${results.length} results`);
    
    const startTime = Date.now();
    
    try {
      // Create final dataset
      const finalDatasetName = `${this.options.finalDatasetPrefix}.B${batchId.replace(/[^A-Z0-9]/g, '')}`;
      
      await this.db2Connector.query(`
        CREATE TABLE ${finalDatasetName} (
          BATCH_ID VARCHAR(100),
          JOB_ID VARCHAR(50),
          RECORD_NUM INTEGER,
          RECORD_DATA VARCHAR(32000),
          SOURCE_WORKER VARCHAR(100),
          MERGED_AT TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (JOB_ID, RECORD_NUM)
        )
      `);
      
      // Merge based on strategy
      switch (strategy) {
        case 'sequential':
          await this._mergeSequential(batchId, results, finalDatasetName);
          break;
          
        case 'parallel':
          await this._mergeParallel(batchId, results, finalDatasetName);
          break;
          
        case 'sorted':
          await this._mergeSorted(batchId, results, finalDatasetName);
          break;
          
        default:
          throw new Error(`Unknown merge strategy: ${strategy}`);
      }
      
      // Update batch status
      const batch = this.activeBatches.get(batchId);
      if (batch) {
        batch.status = 'completed';
        batch.finalDataset = finalDatasetName;
        batch.completedAt = new Date();
        
        this.activeBatches.delete(batchId);
        this.completedBatches.set(batchId, batch);
      }
      
      // Cleanup temporary datasets
      if (this.options.cleanupTempDatasets) {
        await this._cleanupTempDatasets(results);
      }
      
      // Update metrics
      const mergeTime = Date.now() - startTime;
      this._updateMetrics(mergeTime, results.length);
      
      // Emit completion event
      this.emit('merge_completed', {
        batchId,
        finalDataset: finalDatasetName,
        recordCount: results.length,
        mergeTime
      });
      
      logger.info(`Batch ${batchId} merged successfully to ${finalDatasetName}`);
      
      return { success: true, finalDataset: finalDatasetName };
      
    } catch (error) {
      logger.error(`Failed to merge batch ${batchId}:`, error);
      
      // Update batch status
      const batch = this.activeBatches.get(batchId);
      if (batch) {
        batch.status = 'failed';
        batch.error = error.message;
        this.metrics.failedBatches++;
      }
      
      throw error;
    }
  }
  
  async _mergeSequential(batchId, results, finalDatasetName) {
    logger.debug(`Sequential merge for batch ${batchId}`);
    
    let globalRecordNum = 1;
    
    for (const [jobId, resultInfo] of results) {
      const { tempDataset, workerId } = resultInfo;
      
      // Copy records from temporary dataset
      const insertQuery = `
        INSERT INTO ${finalDatasetName} (BATCH_ID, JOB_ID, RECORD_NUM, RECORD_DATA, SOURCE_WORKER)
        SELECT 
          '${batchId}',
          '${jobId}',
          ROW_NUMBER() OVER (ORDER BY RECORD_NUM) + ${globalRecordNum - 1},
          RECORD_DATA,
          '${workerId}'
        FROM ${tempDataset}
        ORDER BY RECORD_NUM
      `;
      
      const result = await this.db2Connector.query(insertQuery);
      
      // Update global record counter
      const countResult = await this.db2Connector.query(
        `SELECT COUNT(*) as CNT FROM ${tempDataset}`
      );
      globalRecordNum += countResult[0].CNT;
    }
  }
  
  async _mergeParallel(batchId, results, finalDatasetName) {
    logger.debug(`Parallel merge for batch ${batchId}`);
    
    // Process multiple results in parallel
    const PARALLEL_BATCH_SIZE = 5;
    
    for (let i = 0; i < results.length; i += PARALLEL_BATCH_SIZE) {
      const batch = results.slice(i, i + PARALLEL_BATCH_SIZE);
      
      const mergePromises = batch.map(async ([jobId, resultInfo]) => {
        const { tempDataset, workerId } = resultInfo;
        
        // Calculate offset for this job
        const offset = i * 10000; // Assuming max 10000 records per job
        
        const insertQuery = `
          INSERT INTO ${finalDatasetName} (BATCH_ID, JOB_ID, RECORD_NUM, RECORD_DATA, SOURCE_WORKER)
          SELECT 
            '${batchId}',
            '${jobId}',
            RECORD_NUM + ${offset},
            RECORD_DATA,
            '${workerId}'
          FROM ${tempDataset}
        `;
        
        return await this.db2Connector.query(insertQuery);
      });
      
      await Promise.all(mergePromises);
    }
  }
  
  async _mergeSorted(batchId, results, finalDatasetName) {
    logger.debug(`Sorted merge for batch ${batchId}`);
    
    // Create a view that unions all temporary datasets with sorting
    const unionParts = results.map(([jobId, resultInfo], index) => {
      const { tempDataset, workerId } = resultInfo;
      return `
        SELECT 
          '${batchId}' as BATCH_ID,
          '${jobId}' as JOB_ID,
          RECORD_NUM,
          RECORD_DATA,
          '${workerId}' as SOURCE_WORKER,
          ${index} as SOURCE_ORDER
        FROM ${tempDataset}
      `;
    });
    
    const unionQuery = unionParts.join(' UNION ALL ');
    
    // Insert sorted results
    const insertQuery = `
      INSERT INTO ${finalDatasetName} (BATCH_ID, JOB_ID, RECORD_NUM, RECORD_DATA, SOURCE_WORKER)
      SELECT 
        BATCH_ID,
        JOB_ID,
        ROW_NUMBER() OVER (ORDER BY SOURCE_ORDER, RECORD_NUM) as RECORD_NUM,
        RECORD_DATA,
        SOURCE_WORKER
      FROM (${unionQuery}) AS COMBINED
    `;
    
    await this.db2Connector.query(insertQuery);
  }
  
  async _cleanupTempDatasets(results) {
    logger.debug('Cleaning up temporary datasets');
    
    const cleanupPromises = results.map(async ([jobId, resultInfo]) => {
      try {
        await this.db2Connector.query(`DROP TABLE ${resultInfo.tempDataset}`);
        logger.debug(`Dropped temporary dataset ${resultInfo.tempDataset}`);
      } catch (error) {
        logger.warn(`Failed to drop temporary dataset ${resultInfo.tempDataset}:`, error.message);
      }
    });
    
    await Promise.allSettled(cleanupPromises);
  }
  
  async _checkBatches() {
    // Check for stale batches that need to be merged
    const now = Date.now();
    
    for (const [batchId, batch] of this.activeBatches) {
      if (batch.status === 'collecting') {
        const batchAge = now - batch.startTime.getTime();
        
        // Check if batch has timed out or reached max size
        if (batchAge > this.options.batchTimeout || 
            batch.jobs.size >= this.options.maxBatchSize) {
          
          logger.info(`Batch ${batchId} timeout/size limit reached, triggering merge`);
          await this._triggerBatchMerge(batchId);
        }
      }
    }
  }
  
  _updateMetrics(mergeTime, recordCount) {
    this.metrics.completedBatches++;
    this.metrics.totalRecordsMerged += recordCount;
    
    // Update average merge time
    const currentAvg = this.metrics.avgMergeTime;
    const totalBatches = this.metrics.completedBatches;
    this.metrics.avgMergeTime = ((currentAvg * (totalBatches - 1)) + mergeTime) / totalBatches;
  }
  
  async getBatchStatus(batchId) {
    // Check active batches
    if (this.activeBatches.has(batchId)) {
      return this.activeBatches.get(batchId);
    }
    
    // Check completed batches
    if (this.completedBatches.has(batchId)) {
      return this.completedBatches.get(batchId);
    }
    
    return null;
  }
  
  async getMetrics() {
    return {
      ...this.metrics,
      activeBatches: this.activeBatches.size,
      queueStats: {
        waiting: await this.resultQueue.getWaitingCount(),
        active: await this.resultQueue.getActiveCount(),
        completed: await this.resultQueue.getCompletedCount(),
        failed: await this.resultQueue.getFailedCount()
      }
    };
  }
  
  async shutdown() {
    logger.info('Shutting down Result Merger...');
    
    // Stop batch monitoring
    if (this.batchMonitor) {
      clearInterval(this.batchMonitor);
    }
    
    // Process any remaining batches
    for (const [batchId, batch] of this.activeBatches) {
      if (batch.results.size > 0) {
        logger.info(`Processing remaining batch ${batchId} before shutdown`);
        await this._triggerBatchMerge(batchId);
      }
    }
    
    // Wait for queue to empty
    await this.resultQueue.close();
    
    // Shutdown DB2 connector
    await this.db2Connector.shutdown();
    
    logger.info('Result Merger shutdown complete');
  }
}

module.exports = { ResultMerger };