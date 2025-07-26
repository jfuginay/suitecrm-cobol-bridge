const EventEmitter = require('events');
const { MainframeMonitor } = require('../monitors/MainframeMonitor');
const { InstanceManager } = require('../cloud/InstanceManager');
const { JobDistributor } = require('../distributors/JobDistributor');
const { ResultMerger } = require('../mergers/ResultMerger');
const { CostOptimizer } = require('../utils/costOptimizer');
const logger = require('../utils/logger');
const config = require('../config/config');

class CloudBurstScheduler extends EventEmitter {
  constructor(options = {}) {
    super();
    
    this.options = {
      checkInterval: 30000, // 30 seconds
      scaleThreshold: 0.8,  // 80% mainframe utilization
      scaleDownThreshold: 0.3, // 30% utilization
      maxCloudInstances: 50,
      minCloudInstances: 0,
      cooldownPeriod: 300000, // 5 minutes
      ...options
    };
    
    this.isRunning = false;
    this.lastScaleAction = null;
    this.metrics = {
      jobsScheduled: 0,
      jobsCompleted: 0,
      jobsFailed: 0,
      cloudInstancesActive: 0,
      totalCost: 0
    };
    
    // Initialize components
    this.mainframeMonitor = new MainframeMonitor();
    this.instanceManager = new InstanceManager();
    this.jobDistributor = new JobDistributor();
    this.resultMerger = new ResultMerger();
    this.costOptimizer = new CostOptimizer();
    
    this._setupEventHandlers();
  }
  
  _setupEventHandlers() {
    // Mainframe monitor events
    this.mainframeMonitor.on('high_load', this._handleHighLoad.bind(this));
    this.mainframeMonitor.on('queue_status', this._handleQueueStatus.bind(this));
    
    // Instance manager events
    this.instanceManager.on('instance_ready', this._handleInstanceReady.bind(this));
    this.instanceManager.on('instance_terminated', this._handleInstanceTerminated.bind(this));
    
    // Job distributor events
    this.jobDistributor.on('job_assigned', this._handleJobAssigned.bind(this));
    this.jobDistributor.on('job_completed', this._handleJobCompleted.bind(this));
    this.jobDistributor.on('job_failed', this._handleJobFailed.bind(this));
    
    // Result merger events
    this.resultMerger.on('merge_completed', this._handleMergeCompleted.bind(this));
  }
  
  async start() {
    if (this.isRunning) {
      logger.warn('CloudBurstScheduler is already running');
      return;
    }
    
    logger.info('Starting CloudBurst Scheduler...');
    this.isRunning = true;
    
    try {
      // Initialize all components
      await this.mainframeMonitor.connect();
      await this.instanceManager.initialize();
      await this.jobDistributor.initialize();
      await this.resultMerger.initialize();
      
      // Start monitoring loop
      this._startMonitoringLoop();
      
      logger.info('CloudBurst Scheduler started successfully');
      this.emit('started');
    } catch (error) {
      logger.error('Failed to start CloudBurst Scheduler:', error);
      this.isRunning = false;
      throw error;
    }
  }
  
  async stop() {
    if (!this.isRunning) {
      logger.warn('CloudBurstScheduler is not running');
      return;
    }
    
    logger.info('Stopping CloudBurst Scheduler...');
    this.isRunning = false;
    
    // Stop monitoring loop
    if (this.monitoringInterval) {
      clearInterval(this.monitoringInterval);
    }
    
    // Gracefully shutdown all components
    await this.mainframeMonitor.disconnect();
    await this.instanceManager.shutdown();
    await this.jobDistributor.shutdown();
    await this.resultMerger.shutdown();
    
    logger.info('CloudBurst Scheduler stopped');
    this.emit('stopped');
  }
  
  _startMonitoringLoop() {
    this.monitoringInterval = setInterval(async () => {
      try {
        await this._checkAndScale();
      } catch (error) {
        logger.error('Error in monitoring loop:', error);
      }
    }, this.options.checkInterval);
  }
  
  async _checkAndScale() {
    const status = await this.mainframeMonitor.getSystemStatus();
    const queueDepth = status.batchQueue.depth;
    const cpuUtilization = status.cpu.utilization;
    const memoryUtilization = status.memory.utilization;
    
    // Calculate overall utilization
    const utilization = Math.max(
      cpuUtilization,
      memoryUtilization,
      queueDepth / status.batchQueue.maxDepth
    );
    
    logger.debug(`System utilization: ${(utilization * 100).toFixed(2)}%`);
    
    // Check if we need to scale
    if (utilization > this.options.scaleThreshold) {
      await this._scaleUp(status);
    } else if (utilization < this.options.scaleDownThreshold) {
      await this._scaleDown(status);
    }
    
    // Optimize costs
    await this._optimizeCosts();
  }
  
  async _scaleUp(status) {
    // Check cooldown period
    if (this._isInCooldown()) {
      logger.debug('Still in cooldown period, skipping scale up');
      return;
    }
    
    const currentInstances = await this.instanceManager.getActiveInstanceCount();
    
    if (currentInstances >= this.options.maxCloudInstances) {
      logger.warn('Already at maximum cloud instances');
      return;
    }
    
    // Calculate how many instances to add
    const queueDepth = status.batchQueue.depth;
    const avgJobsPerInstance = 10; // Configurable
    const desiredInstances = Math.ceil(queueDepth / avgJobsPerInstance);
    const instancesToAdd = Math.min(
      desiredInstances - currentInstances,
      this.options.maxCloudInstances - currentInstances,
      5 // Max 5 instances at a time
    );
    
    if (instancesToAdd > 0) {
      logger.info(`Scaling up: adding ${instancesToAdd} cloud instances`);
      
      try {
        const newInstances = await this.instanceManager.scaleUp(instancesToAdd);
        this.lastScaleAction = new Date();
        this.emit('scaled_up', { instances: newInstances });
      } catch (error) {
        logger.error('Failed to scale up:', error);
        this.emit('scale_failed', { error, action: 'up' });
      }
    }
  }
  
  async _scaleDown(status) {
    // Check cooldown period
    if (this._isInCooldown()) {
      logger.debug('Still in cooldown period, skipping scale down');
      return;
    }
    
    const currentInstances = await this.instanceManager.getActiveInstanceCount();
    
    if (currentInstances <= this.options.minCloudInstances) {
      logger.debug('Already at minimum cloud instances');
      return;
    }
    
    // Calculate how many instances to remove
    const idleInstances = await this.instanceManager.getIdleInstances();
    const instancesToRemove = Math.min(
      idleInstances.length,
      Math.floor(currentInstances / 2) // Remove at most half
    );
    
    if (instancesToRemove > 0) {
      logger.info(`Scaling down: removing ${instancesToRemove} cloud instances`);
      
      try {
        await this.instanceManager.scaleDown(instancesToRemove);
        this.lastScaleAction = new Date();
        this.emit('scaled_down', { count: instancesToRemove });
      } catch (error) {
        logger.error('Failed to scale down:', error);
        this.emit('scale_failed', { error, action: 'down' });
      }
    }
  }
  
  _isInCooldown() {
    if (!this.lastScaleAction) return false;
    
    const timeSinceLastScale = Date.now() - this.lastScaleAction.getTime();
    return timeSinceLastScale < this.options.cooldownPeriod;
  }
  
  async _handleHighLoad(data) {
    logger.warn('High mainframe load detected:', data);
    
    // Immediate response to high load
    if (!this._isInCooldown()) {
      await this._scaleUp(data.status);
    }
    
    // Start distributing eligible jobs
    const eligibleJobs = await this.mainframeMonitor.getEligibleJobs();
    await this.jobDistributor.distributeJobs(eligibleJobs);
  }
  
  async _handleQueueStatus(status) {
    // Update metrics
    this.metrics.cloudInstancesActive = await this.instanceManager.getActiveInstanceCount();
    
    // Emit status update
    this.emit('status_update', {
      mainframe: status,
      cloud: {
        instances: this.metrics.cloudInstancesActive,
        jobsScheduled: this.metrics.jobsScheduled,
        jobsCompleted: this.metrics.jobsCompleted
      }
    });
  }
  
  async _handleInstanceReady(instance) {
    logger.info(`Cloud instance ready: ${instance.id}`);
    
    // Register instance with job distributor
    await this.jobDistributor.registerWorker(instance);
    
    // Check for pending jobs
    const pendingJobs = await this.jobDistributor.getPendingJobs();
    if (pendingJobs.length > 0) {
      await this.jobDistributor.assignJobsToWorker(instance, pendingJobs);
    }
  }
  
  async _handleInstanceTerminated(instance) {
    logger.info(`Cloud instance terminated: ${instance.id}`);
    
    // Unregister from job distributor
    await this.jobDistributor.unregisterWorker(instance);
    
    // Reassign any running jobs
    const runningJobs = await this.jobDistributor.getJobsByWorker(instance.id);
    if (runningJobs.length > 0) {
      logger.info(`Reassigning ${runningJobs.length} jobs from terminated instance`);
      await this.jobDistributor.reassignJobs(runningJobs);
    }
  }
  
  async _handleJobAssigned(data) {
    this.metrics.jobsScheduled++;
    logger.info(`Job ${data.jobId} assigned to worker ${data.workerId}`);
  }
  
  async _handleJobCompleted(data) {
    this.metrics.jobsCompleted++;
    logger.info(`Job ${data.jobId} completed successfully`);
    
    // Queue result for merging
    await this.resultMerger.queueResult(data);
  }
  
  async _handleJobFailed(data) {
    this.metrics.jobsFailed++;
    logger.error(`Job ${data.jobId} failed:`, data.error);
    
    // Attempt retry or return to mainframe
    const shouldRetry = await this.jobDistributor.shouldRetry(data.jobId);
    if (shouldRetry) {
      await this.jobDistributor.retryJob(data.jobId);
    } else {
      await this.mainframeMonitor.returnJob(data.jobId, data.error);
    }
  }
  
  async _handleMergeCompleted(data) {
    logger.info(`Results merged for batch ${data.batchId}`);
    
    // Notify mainframe of completion
    await this.mainframeMonitor.notifyBatchComplete(data.batchId);
  }
  
  async _optimizeCosts() {
    const optimization = await this.costOptimizer.analyze({
      instances: await this.instanceManager.getAllInstances(),
      jobs: await this.jobDistributor.getJobMetrics(),
      costs: this.metrics.totalCost
    });
    
    if (optimization.recommendations.length > 0) {
      logger.info('Cost optimization recommendations:', optimization.recommendations);
      
      // Apply automated optimizations
      for (const rec of optimization.recommendations) {
        if (rec.autoApply) {
          await this._applyOptimization(rec);
        }
      }
    }
  }
  
  async _applyOptimization(recommendation) {
    switch (recommendation.type) {
      case 'SWITCH_INSTANCE_TYPE':
        await this.instanceManager.switchInstanceType(
          recommendation.instanceId,
          recommendation.newType
        );
        break;
        
      case 'USE_SPOT_INSTANCES':
        await this.instanceManager.enableSpotInstances(recommendation.percentage);
        break;
        
      case 'CONSOLIDATE_WORKLOAD':
        await this.jobDistributor.consolidateWorkload(recommendation.targetUtilization);
        break;
        
      default:
        logger.warn(`Unknown optimization type: ${recommendation.type}`);
    }
  }
  
  getMetrics() {
    return {
      ...this.metrics,
      uptime: this.isRunning ? Date.now() - this.startTime : 0,
      lastScaleAction: this.lastScaleAction
    };
  }
}

module.exports = { CloudBurstScheduler };