const EventEmitter = require('events');
const Bull = require('bull');
const Redis = require('redis');
const { PQueue } = require('p-queue');
const logger = require('../utils/logger');
const config = require('../config/config');

class JobDistributor extends EventEmitter {
  constructor(options = {}) {
    super();
    
    this.options = {
      redisUrl: process.env.REDIS_URL || 'redis://localhost:6379',
      maxRetries: 3,
      retryDelay: 5000,
      jobTimeout: 300000, // 5 minutes
      concurrentJobsPerWorker: 2,
      priorityLevels: {
        critical: 100,
        high: 75,
        normal: 50,
        low: 25
      },
      ...options
    };
    
    this.redisClient = null;
    this.jobQueue = null;
    this.resultQueue = null;
    this.workers = new Map();
    this.jobAssignments = new Map();
    this.jobMetrics = new Map();
    this.isInitialized = false;
    
    // Processing queue for job distribution
    this.distributionQueue = new PQueue({ 
      concurrency: 10,
      interval: 1000,
      intervalCap: 50
    });
  }
  
  async initialize() {
    logger.info('Initializing Job Distributor...');
    
    try {
      // Connect to Redis
      this.redisClient = Redis.createClient({
        url: this.options.redisUrl
      });
      
      await this.redisClient.connect();
      
      // Initialize job queues
      this.jobQueue = new Bull('cobol-jobs', {
        redis: {
          port: 6379,
          host: this.options.redisUrl.split('://')[1].split(':')[0]
        }
      });
      
      this.resultQueue = new Bull('cobol-results', {
        redis: {
          port: 6379,
          host: this.options.redisUrl.split('://')[1].split(':')[0]
        }
      });
      
      // Setup queue event handlers
      this._setupQueueHandlers();
      
      // Load pending jobs
      await this._loadPendingJobs();
      
      this.isInitialized = true;
      logger.info('Job Distributor initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize Job Distributor:', error);
      throw error;
    }
  }
  
  _setupQueueHandlers() {
    // Job queue events
    this.jobQueue.on('completed', (job, result) => {
      this._handleJobCompleted(job, result);
    });
    
    this.jobQueue.on('failed', (job, err) => {
      this._handleJobFailed(job, err);
    });
    
    this.jobQueue.on('stalled', (job) => {
      logger.warn(`Job ${job.id} stalled`);
      this._handleJobStalled(job);
    });
    
    // Result queue events
    this.resultQueue.on('completed', (job) => {
      logger.debug(`Result processed for job ${job.id}`);
    });
  }
  
  async _loadPendingJobs() {
    const waitingJobs = await this.jobQueue.getWaiting();
    const activeJobs = await this.jobQueue.getActive();
    
    logger.info(`Loaded ${waitingJobs.length} waiting jobs and ${activeJobs.length} active jobs`);
    
    // Rebuild job assignments
    for (const job of activeJobs) {
      if (job.data.workerId) {
        this.jobAssignments.set(job.id, job.data.workerId);
      }
    }
  }
  
  async distributeJobs(jobs) {
    logger.info(`Distributing ${jobs.length} jobs`);
    
    const distributionPromises = jobs.map(job => 
      this.distributionQueue.add(() => this._distributeJob(job))
    );
    
    const results = await Promise.allSettled(distributionPromises);
    
    const successful = results.filter(r => r.status === 'fulfilled').length;
    const failed = results.filter(r => r.status === 'rejected').length;
    
    logger.info(`Successfully distributed ${successful} jobs, ${failed} failed`);
    
    return {
      successful,
      failed,
      total: jobs.length
    };
  }
  
  async _distributeJob(job) {
    try {
      // Add job to queue with priority
      const priority = this._calculatePriority(job);
      
      const queuedJob = await this.jobQueue.add('process', {
        id: job.id,
        name: job.name,
        program: job.program,
        requirements: job.requirements,
        metadata: job.metadata,
        submittedAt: new Date().toISOString(),
        source: 'mainframe'
      }, {
        priority: priority,
        attempts: this.options.maxRetries,
        backoff: {
          type: 'exponential',
          delay: this.options.retryDelay
        },
        timeout: this.options.jobTimeout
      });
      
      logger.debug(`Job ${job.id} added to queue with priority ${priority}`);
      
      // Try to assign immediately if workers available
      await this._tryAssignJob(queuedJob);
      
      return queuedJob;
    } catch (error) {
      logger.error(`Failed to distribute job ${job.id}:`, error);
      throw error;
    }
  }
  
  _calculatePriority(job) {
    const priorityMap = {
      critical: this.options.priorityLevels.critical,
      high: this.options.priorityLevels.high,
      normal: this.options.priorityLevels.normal,
      low: this.options.priorityLevels.low
    };
    
    // Use job priority if available, otherwise calculate based on requirements
    if (job.priority && priorityMap[job.priority]) {
      return priorityMap[job.priority];
    }
    
    // Calculate based on resource requirements and wait time
    let priority = this.options.priorityLevels.normal;
    
    // Boost priority for resource-intensive jobs
    if (job.requirements.cpu > 4 || job.requirements.memory > 8192) {
      priority += 10;
    }
    
    // Boost priority for jobs that have been waiting
    if (job.metadata.submittedAt) {
      const waitTime = Date.now() - new Date(job.metadata.submittedAt).getTime();
      const hoursWaiting = waitTime / (1000 * 60 * 60);
      priority += Math.min(hoursWaiting * 5, 25); // Max 25 point boost
    }
    
    return Math.min(priority, 100); // Cap at 100
  }
  
  async _tryAssignJob(job) {
    const availableWorkers = this._getAvailableWorkers();
    
    if (availableWorkers.length === 0) {
      logger.debug('No available workers for immediate assignment');
      return;
    }
    
    // Find best worker for job
    const worker = this._selectBestWorker(availableWorkers, job.data.requirements);
    
    if (worker) {
      await this._assignJobToWorker(job, worker);
    }
  }
  
  _getAvailableWorkers() {
    const available = [];
    
    for (const [workerId, worker] of this.workers) {
      if (worker.status === 'active' && 
          worker.activeJobs < this.options.concurrentJobsPerWorker) {
        available.push(worker);
      }
    }
    
    return available;
  }
  
  _selectBestWorker(workers, requirements) {
    // Score workers based on current load and capabilities
    const scoredWorkers = workers.map(worker => {
      let score = 100;
      
      // Penalize based on current load
      score -= (worker.activeJobs / this.options.concurrentJobsPerWorker) * 30;
      
      // Penalize if resources are tight
      const cpuAvailable = worker.resources.cpu - worker.usedResources.cpu;
      const memoryAvailable = worker.resources.memory - worker.usedResources.memory;
      
      if (cpuAvailable < requirements.cpu) {
        score -= 50;
      }
      
      if (memoryAvailable < requirements.memory) {
        score -= 50;
      }
      
      // Bonus for matching instance type
      if (worker.instanceType === this._getOptimalInstanceType(requirements)) {
        score += 20;
      }
      
      // Bonus for low latency to mainframe
      if (worker.metrics && worker.metrics.mainframeLatency < 10) {
        score += 10;
      }
      
      return { worker, score };
    });
    
    // Sort by score and return best
    scoredWorkers.sort((a, b) => b.score - a.score);
    
    return scoredWorkers[0]?.score > 0 ? scoredWorkers[0].worker : null;
  }
  
  _getOptimalInstanceType(requirements) {
    // Map requirements to instance types
    if (requirements.cpu >= 8 || requirements.memory >= 16384) {
      return 't3.xlarge';
    } else if (requirements.cpu >= 4 || requirements.memory >= 8192) {
      return 't3.large';
    } else {
      return 't3.medium';
    }
  }
  
  async registerWorker(instance) {
    logger.info(`Registering worker ${instance.id}`);
    
    const worker = {
      id: instance.id,
      instance: instance,
      status: 'active',
      activeJobs: 0,
      completedJobs: 0,
      failedJobs: 0,
      resources: {
        cpu: this._getInstanceCPU(instance.type),
        memory: this._getInstanceMemory(instance.type)
      },
      usedResources: {
        cpu: 0,
        memory: 0
      },
      instanceType: instance.type,
      registeredAt: new Date(),
      lastHeartbeat: new Date(),
      metrics: {
        avgJobDuration: 0,
        successRate: 100,
        mainframeLatency: 0
      }
    };
    
    this.workers.set(instance.id, worker);
    
    // Start heartbeat monitoring
    this._startWorkerHeartbeat(worker);
    
    // Check for pending jobs
    await this._assignPendingJobsToWorker(worker);
    
    this.emit('worker_registered', worker);
  }
  
  _getInstanceCPU(instanceType) {
    // Map instance types to CPU counts
    const cpuMap = {
      't3.medium': 2,
      't3.large': 2,
      't3.xlarge': 4,
      't3.2xlarge': 8,
      'container': 2
    };
    
    return cpuMap[instanceType] || 2;
  }
  
  _getInstanceMemory(instanceType) {
    // Map instance types to memory (MB)
    const memoryMap = {
      't3.medium': 4096,
      't3.large': 8192,
      't3.xlarge': 16384,
      't3.2xlarge': 32768,
      'container': 4096
    };
    
    return memoryMap[instanceType] || 4096;
  }
  
  _startWorkerHeartbeat(worker) {
    const heartbeatInterval = setInterval(async () => {
      try {
        const response = await fetch(`http://${worker.instance.privateIp}:8080/heartbeat`);
        
        if (response.ok) {
          const status = await response.json();
          worker.lastHeartbeat = new Date();
          worker.activeJobs = status.activeJobs || 0;
          worker.metrics = { ...worker.metrics, ...status.metrics };
        } else {
          throw new Error(`Heartbeat failed with status ${response.status}`);
        }
      } catch (error) {
        logger.warn(`Worker ${worker.id} heartbeat failed:`, error.message);
        
        // Mark worker as unhealthy after 3 missed heartbeats
        const timeSinceLastHeartbeat = Date.now() - worker.lastHeartbeat.getTime();
        if (timeSinceLastHeartbeat > 30000) { // 30 seconds
          worker.status = 'unhealthy';
          clearInterval(heartbeatInterval);
          await this._handleUnhealthyWorker(worker);
        }
      }
    }, 10000); // Every 10 seconds
    
    worker.heartbeatInterval = heartbeatInterval;
  }
  
  async _assignPendingJobsToWorker(worker) {
    const waitingJobs = await this.jobQueue.getWaiting();
    const jobsToAssign = [];
    
    for (const job of waitingJobs) {
      if (jobsToAssign.length >= this.options.concurrentJobsPerWorker) {
        break;
      }
      
      // Check if worker can handle job requirements
      const requirements = job.data.requirements;
      const cpuAvailable = worker.resources.cpu - worker.usedResources.cpu;
      const memoryAvailable = worker.resources.memory - worker.usedResources.memory;
      
      if (cpuAvailable >= requirements.cpu && memoryAvailable >= requirements.memory) {
        jobsToAssign.push(job);
      }
    }
    
    for (const job of jobsToAssign) {
      await this._assignJobToWorker(job, worker);
    }
  }
  
  async _assignJobToWorker(job, worker) {
    try {
      logger.info(`Assigning job ${job.id} to worker ${worker.id}`);
      
      // Update job data
      job.data.workerId = worker.id;
      job.data.assignedAt = new Date().toISOString();
      
      // Send job to worker
      const response = await fetch(`http://${worker.instance.privateIp}:8080/job`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          jobId: job.id,
          jobData: job.data,
          timeout: this.options.jobTimeout
        })
      });
      
      if (!response.ok) {
        throw new Error(`Worker rejected job: ${response.statusText}`);
      }
      
      // Update tracking
      this.jobAssignments.set(job.id, worker.id);
      worker.activeJobs++;
      worker.usedResources.cpu += job.data.requirements.cpu;
      worker.usedResources.memory += job.data.requirements.memory;
      
      // Initialize job metrics
      this.jobMetrics.set(job.id, {
        workerId: worker.id,
        assignedAt: new Date(),
        startedAt: null,
        completedAt: null,
        status: 'assigned'
      });
      
      // Move job to active
      await job.moveToActive();
      
      this.emit('job_assigned', {
        jobId: job.id,
        workerId: worker.id,
        requirements: job.data.requirements
      });
    } catch (error) {
      logger.error(`Failed to assign job ${job.id} to worker ${worker.id}:`, error);
      
      // Return job to queue
      await job.moveToWaiting();
      
      throw error;
    }
  }
  
  async unregisterWorker(instance) {
    logger.info(`Unregistering worker ${instance.id}`);
    
    const worker = this.workers.get(instance.id);
    if (!worker) {
      logger.warn(`Worker ${instance.id} not found`);
      return;
    }
    
    // Stop heartbeat
    if (worker.heartbeatInterval) {
      clearInterval(worker.heartbeatInterval);
    }
    
    // Remove from tracking
    this.workers.delete(instance.id);
    
    this.emit('worker_unregistered', worker);
  }
  
  async getJobsByWorker(workerId) {
    const jobs = [];
    
    for (const [jobId, assignedWorkerId] of this.jobAssignments) {
      if (assignedWorkerId === workerId) {
        const job = await this.jobQueue.getJob(jobId);
        if (job) {
          jobs.push(job);
        }
      }
    }
    
    return jobs;
  }
  
  async reassignJobs(jobs) {
    logger.info(`Reassigning ${jobs.length} jobs`);
    
    for (const job of jobs) {
      try {
        // Remove worker assignment
        delete job.data.workerId;
        delete job.data.assignedAt;
        
        // Remove from tracking
        this.jobAssignments.delete(job.id);
        
        // Move back to waiting
        await job.moveToWaiting();
        
        // Try to assign to another worker
        await this._tryAssignJob(job);
      } catch (error) {
        logger.error(`Failed to reassign job ${job.id}:`, error);
      }
    }
  }
  
  async _handleJobCompleted(job, result) {
    logger.info(`Job ${job.id} completed successfully`);
    
    const workerId = this.jobAssignments.get(job.id);
    const worker = this.workers.get(workerId);
    
    if (worker) {
      // Update worker stats
      worker.activeJobs--;
      worker.completedJobs++;
      worker.usedResources.cpu -= job.data.requirements.cpu;
      worker.usedResources.memory -= job.data.requirements.memory;
      
      // Update success rate
      const totalJobs = worker.completedJobs + worker.failedJobs;
      worker.metrics.successRate = (worker.completedJobs / totalJobs) * 100;
    }
    
    // Update job metrics
    const metrics = this.jobMetrics.get(job.id);
    if (metrics) {
      metrics.completedAt = new Date();
      metrics.status = 'completed';
      metrics.duration = metrics.completedAt - metrics.startedAt;
    }
    
    // Clean up tracking
    this.jobAssignments.delete(job.id);
    
    // Queue result for merging
    await this.resultQueue.add('merge', {
      jobId: job.id,
      jobData: job.data,
      result: result,
      workerId: workerId,
      completedAt: new Date().toISOString()
    });
    
    this.emit('job_completed', {
      jobId: job.id,
      workerId: workerId,
      result: result,
      metrics: metrics
    });
  }
  
  async _handleJobFailed(job, error) {
    logger.error(`Job ${job.id} failed:`, error);
    
    const workerId = this.jobAssignments.get(job.id);
    const worker = this.workers.get(workerId);
    
    if (worker) {
      // Update worker stats
      worker.activeJobs--;
      worker.failedJobs++;
      worker.usedResources.cpu -= job.data.requirements.cpu;
      worker.usedResources.memory -= job.data.requirements.memory;
      
      // Update success rate
      const totalJobs = worker.completedJobs + worker.failedJobs;
      worker.metrics.successRate = (worker.completedJobs / totalJobs) * 100;
    }
    
    // Update job metrics
    const metrics = this.jobMetrics.get(job.id);
    if (metrics) {
      metrics.completedAt = new Date();
      metrics.status = 'failed';
      metrics.error = error.message;
    }
    
    // Clean up tracking
    this.jobAssignments.delete(job.id);
    
    this.emit('job_failed', {
      jobId: job.id,
      workerId: workerId,
      error: error,
      attempts: job.attemptsMade,
      willRetry: job.attemptsMade < this.options.maxRetries
    });
  }
  
  async _handleJobStalled(job) {
    const workerId = this.jobAssignments.get(job.id);
    const worker = this.workers.get(workerId);
    
    if (worker) {
      logger.warn(`Job ${job.id} stalled on worker ${worker.id}`);
      
      // Check worker health
      const isHealthy = await this._checkWorkerHealth(worker);
      
      if (!isHealthy) {
        // Reassign job
        await this.reassignJobs([job]);
      }
    }
  }
  
  async _checkWorkerHealth(worker) {
    try {
      const response = await fetch(`http://${worker.instance.privateIp}:8080/health`, {
        timeout: 5000
      });
      
      return response.ok;
    } catch {
      return false;
    }
  }
  
  async _handleUnhealthyWorker(worker) {
    logger.warn(`Worker ${worker.id} marked as unhealthy`);
    
    // Get all jobs assigned to this worker
    const jobs = await this.getJobsByWorker(worker.id);
    
    // Reassign jobs
    if (jobs.length > 0) {
      await this.reassignJobs(jobs);
    }
    
    // Unregister worker
    await this.unregisterWorker(worker.instance);
    
    this.emit('worker_unhealthy', worker);
  }
  
  async shouldRetry(jobId) {
    const job = await this.jobQueue.getJob(jobId);
    
    if (!job) {
      return false;
    }
    
    return job.attemptsMade < this.options.maxRetries;
  }
  
  async retryJob(jobId) {
    const job = await this.jobQueue.getJob(jobId);
    
    if (!job) {
      throw new Error(`Job ${jobId} not found`);
    }
    
    logger.info(`Retrying job ${jobId} (attempt ${job.attemptsMade + 1})`);
    
    await job.retry();
  }
  
  async getPendingJobs() {
    return await this.jobQueue.getWaiting();
  }
  
  async getJobMetrics() {
    const waiting = await this.jobQueue.getWaitingCount();
    const active = await this.jobQueue.getActiveCount();
    const completed = await this.jobQueue.getCompletedCount();
    const failed = await this.jobQueue.getFailedCount();
    
    return {
      waiting,
      active,
      completed,
      failed,
      total: waiting + active + completed + failed,
      workers: {
        total: this.workers.size,
        active: Array.from(this.workers.values()).filter(w => w.status === 'active').length,
        unhealthy: Array.from(this.workers.values()).filter(w => w.status === 'unhealthy').length
      },
      avgJobDuration: this._calculateAvgJobDuration(),
      successRate: completed > 0 ? (completed / (completed + failed)) * 100 : 0
    };
  }
  
  _calculateAvgJobDuration() {
    const completedJobs = Array.from(this.jobMetrics.values())
      .filter(m => m.status === 'completed' && m.duration);
    
    if (completedJobs.length === 0) {
      return 0;
    }
    
    const totalDuration = completedJobs.reduce((sum, m) => sum + m.duration, 0);
    return totalDuration / completedJobs.length;
  }
  
  async consolidateWorkload(targetUtilization) {
    logger.info(`Consolidating workload to ${targetUtilization}% utilization`);
    
    // Analyze current worker utilization
    const workers = Array.from(this.workers.values())
      .filter(w => w.status === 'active')
      .map(w => ({
        ...w,
        utilization: (w.activeJobs / this.options.concurrentJobsPerWorker) * 100
      }))
      .sort((a, b) => a.utilization - b.utilization);
    
    // Find underutilized workers
    const underutilized = workers.filter(w => w.utilization < targetUtilization);
    
    if (underutilized.length === 0) {
      logger.info('No underutilized workers found');
      return;
    }
    
    // Redistribute jobs from underutilized workers
    for (const worker of underutilized) {
      const jobs = await this.getJobsByWorker(worker.id);
      
      if (jobs.length > 0) {
        // Find workers with capacity
        const targetWorkers = workers.filter(w => 
          w.id !== worker.id && 
          w.utilization < targetUtilization
        );
        
        if (targetWorkers.length > 0) {
          // Reassign jobs
          for (const job of jobs) {
            const targetWorker = targetWorkers[0];
            await this._reassignJobToWorker(job, worker, targetWorker);
          }
        }
      }
    }
  }
  
  async _reassignJobToWorker(job, fromWorker, toWorker) {
    try {
      // Notify current worker to stop job
      await fetch(`http://${fromWorker.instance.privateIp}:8080/job/${job.id}/stop`, {
        method: 'POST'
      });
      
      // Assign to new worker
      await this._assignJobToWorker(job, toWorker);
      
      logger.info(`Reassigned job ${job.id} from worker ${fromWorker.id} to ${toWorker.id}`);
    } catch (error) {
      logger.error(`Failed to reassign job ${job.id}:`, error);
    }
  }
  
  async shutdown() {
    logger.info('Shutting down Job Distributor...');
    
    // Stop accepting new jobs
    await this.jobQueue.pause();
    
    // Wait for active jobs to complete
    const activeJobs = await this.jobQueue.getActive();
    if (activeJobs.length > 0) {
      logger.info(`Waiting for ${activeJobs.length} active jobs to complete...`);
      await new Promise(resolve => setTimeout(resolve, 30000)); // Wait up to 30 seconds
    }
    
    // Clean up workers
    for (const worker of this.workers.values()) {
      if (worker.heartbeatInterval) {
        clearInterval(worker.heartbeatInterval);
      }
    }
    
    // Close queues
    await this.jobQueue.close();
    await this.resultQueue.close();
    
    // Disconnect Redis
    await this.redisClient.disconnect();
    
    logger.info('Job Distributor shutdown complete');
  }
}

module.exports = { JobDistributor };