const { v4: uuidv4 } = require('uuid');
const logger = require('./logger');

class JobQueue {
  constructor(redisClient) {
    this.redis = redisClient;
    this.queueName = 'cobol:jobs';
    this.processingQueue = 'cobol:processing';
    this.completedQueue = 'cobol:completed';
    this.failedQueue = 'cobol:failed';
  }

  // Add job to queue
  async add(jobData) {
    const job = {
      id: jobData.jobId || uuidv4(),
      ...jobData,
      status: 'queued',
      createdAt: new Date().toISOString(),
      attempts: 0
    };

    await this.redis.lPush(this.queueName, JSON.stringify(job));
    await this.redis.setEx(`job:${job.id}`, 86400, JSON.stringify(job)); // 24 hour TTL
    
    logger.info('Job added to queue', { jobId: job.id, type: job.jobType });
    return job;
  }

  // Get next job from queue
  async getNext() {
    const jobData = await this.redis.rpoplpush(this.queueName, this.processingQueue);
    if (!jobData) return null;

    const job = JSON.parse(jobData);
    job.status = 'processing';
    job.startedAt = new Date().toISOString();
    
    await this.updateJobStatus(job.id, job);
    return job;
  }

  // Mark job as completed
  async complete(jobId, result) {
    const job = await this.getJob(jobId);
    if (!job) throw new Error('Job not found');

    job.status = 'completed';
    job.completedAt = new Date().toISOString();
    job.result = result;
    job.duration = Date.now() - new Date(job.startedAt).getTime();

    await this.redis.lRem(this.processingQueue, 1, JSON.stringify(job));
    await this.redis.lPush(this.completedQueue, JSON.stringify(job));
    await this.updateJobStatus(jobId, job);
    
    logger.info('Job completed', { jobId, duration: job.duration });
    return job;
  }

  // Mark job as failed
  async fail(jobId, error) {
    const job = await this.getJob(jobId);
    if (!job) throw new Error('Job not found');

    job.status = 'failed';
    job.failedAt = new Date().toISOString();
    job.error = error.message || error;
    job.attempts++;

    if (job.attempts < 3) {
      // Retry the job
      job.status = 'queued';
      await this.redis.lRem(this.processingQueue, 1, JSON.stringify(job));
      await this.redis.lPush(this.queueName, JSON.stringify(job));
      logger.info('Job requeued for retry', { jobId, attempts: job.attempts });
    } else {
      // Move to failed queue
      await this.redis.lRem(this.processingQueue, 1, JSON.stringify(job));
      await this.redis.lPush(this.failedQueue, JSON.stringify(job));
      logger.error('Job failed permanently', { jobId, error: job.error });
    }

    await this.updateJobStatus(jobId, job);
    return job;
  }

  // Get job status
  async getJobStatus(jobId) {
    const jobData = await this.redis.get(`job:${jobId}`);
    return jobData ? JSON.parse(jobData) : null;
  }

  // Update job status
  async updateJobStatus(jobId, job) {
    await this.redis.setEx(`job:${jobId}`, 86400, JSON.stringify(job));
  }

  // Get job by ID
  async getJob(jobId) {
    return await this.getJobStatus(jobId);
  }

  // Get queue statistics
  async getStats() {
    const [queued, processing, completed, failed] = await Promise.all([
      this.redis.lLen(this.queueName),
      this.redis.lLen(this.processingQueue),
      this.redis.lLen(this.completedQueue),
      this.redis.lLen(this.failedQueue)
    ]);

    return {
      queued,
      processing,
      completed,
      failed,
      total: queued + processing + completed + failed
    };
  }

  // Clean old jobs
  async cleanup(olderThanDays = 7) {
    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - olderThanDays);

    // Clean completed queue
    const completedJobs = await this.redis.lRange(this.completedQueue, 0, -1);
    for (const jobData of completedJobs) {
      const job = JSON.parse(jobData);
      if (new Date(job.completedAt) < cutoffDate) {
        await this.redis.lRem(this.completedQueue, 1, jobData);
        await this.redis.del(`job:${job.id}`);
      }
    }

    // Clean failed queue
    const failedJobs = await this.redis.lRange(this.failedQueue, 0, -1);
    for (const jobData of failedJobs) {
      const job = JSON.parse(jobData);
      if (new Date(job.failedAt) < cutoffDate) {
        await this.redis.lRem(this.failedQueue, 1, jobData);
        await this.redis.del(`job:${job.id}`);
      }
    }

    logger.info('Job queue cleanup completed', { olderThanDays });
  }
}

// Job processor
class JobProcessor {
  constructor(jobQueue, cobolWrapper) {
    this.queue = jobQueue;
    this.cobolWrapper = cobolWrapper;
    this.isRunning = false;
    this.processingInterval = null;
  }

  // Start processing jobs
  start(intervalMs = 5000) {
    if (this.isRunning) return;
    
    this.isRunning = true;
    logger.info('Job processor started');

    this.processingInterval = setInterval(async () => {
      await this.processNext();
    }, intervalMs);

    // Process immediately
    this.processNext();
  }

  // Stop processing jobs
  stop() {
    if (!this.isRunning) return;
    
    this.isRunning = false;
    if (this.processingInterval) {
      clearInterval(this.processingInterval);
      this.processingInterval = null;
    }
    
    logger.info('Job processor stopped');
  }

  // Process next job in queue
  async processNext() {
    if (!this.isRunning) return;

    try {
      const job = await this.queue.getNext();
      if (!job) return;

      logger.info('Processing job', { jobId: job.id, type: job.jobType });

      switch (job.jobType) {
        case 'DAILY_INTEREST':
          await this.processDailyInterest(job);
          break;
        case 'PAYROLL_RUN':
          await this.processPayrollRun(job);
          break;
        case 'CREDIT_REVIEW':
          await this.processCreditReview(job);
          break;
        default:
          throw new Error(`Unknown job type: ${job.jobType}`);
      }
    } catch (error) {
      logger.error('Job processing error', error);
    }
  }

  // Process daily interest calculation
  async processDailyInterest(job) {
    try {
      // Simulate batch processing
      const accounts = job.parameters.accounts || [];
      const results = [];

      for (const account of accounts) {
        const result = await this.cobolWrapper.executeCobolProgram('INTEREST01', {
          accountId: account.id,
          balance: account.balance,
          rate: account.interestRate
        });
        results.push(result);
      }

      await this.queue.complete(job.id, { 
        processed: results.length,
        results: results
      });
    } catch (error) {
      await this.queue.fail(job.id, error);
    }
  }

  // Process payroll run
  async processPayrollRun(job) {
    try {
      // Simulate payroll processing
      const employees = job.parameters.employees || [];
      const results = [];

      for (const employee of employees) {
        const result = await this.cobolWrapper.executeCobolProgram('PAYROLL01', {
          employeeId: employee.id,
          hoursWorked: employee.hours,
          hourlyRate: employee.rate
        });
        results.push(result);
      }

      await this.queue.complete(job.id, { 
        processed: results.length,
        totalPayroll: results.reduce((sum, r) => sum + r.netPay, 0)
      });
    } catch (error) {
      await this.queue.fail(job.id, error);
    }
  }

  // Process credit review
  async processCreditReview(job) {
    try {
      // Simulate credit review
      const customers = job.parameters.customers || [];
      const results = [];

      for (const customer of customers) {
        const result = await this.cobolWrapper.executeCobolProgram('CREDIT01', {
          customerId: customer.id,
          income: customer.income,
          creditScore: customer.creditScore
        });
        results.push(result);
      }

      await this.queue.complete(job.id, { 
        reviewed: results.length,
        approved: results.filter(r => r.approved).length
      });
    } catch (error) {
      await this.queue.fail(job.id, error);
    }
  }
}

module.exports = { JobQueue, JobProcessor };