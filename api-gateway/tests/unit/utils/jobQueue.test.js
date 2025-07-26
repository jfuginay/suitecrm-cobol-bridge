const { JobQueue, JobProcessor } = require('../../../utils/jobQueue');

// Mock Redis client
const mockRedisClient = {
  get: jest.fn(),
  set: jest.fn(),
  del: jest.fn(),
  lpush: jest.fn(),
  brpop: jest.fn(),
  llen: jest.fn(),
  hset: jest.fn(),
  hget: jest.fn(),
  hgetall: jest.fn(),
  hdel: jest.fn(),
  expire: jest.fn(),
  exists: jest.fn()
};

// Mock logger
jest.mock('../../../utils/logger', () => ({
  info: jest.fn(),
  warn: jest.fn(),
  error: jest.fn(),
  debug: jest.fn()
}));

// Mock websocket
jest.mock('../../../utils/websocket', () => ({
  broadcast: jest.fn()
}));

describe('JobQueue', () => {
  let jobQueue;

  beforeEach(() => {
    jobQueue = new JobQueue(mockRedisClient);
    jest.clearAllMocks();
  });

  describe('add', () => {
    it('should add job to queue successfully', async () => {
      const jobData = {
        jobId: 'test-job-123',
        jobType: 'CREDIT_CALCULATION',
        parameters: { customerId: 'CUST001' },
        userId: 'user123'
      };

      mockRedisClient.lpush.mockResolvedValue(1);
      mockRedisClient.hset.mockResolvedValue(1);
      mockRedisClient.expire.mockResolvedValue(1);

      await jobQueue.add(jobData);

      expect(mockRedisClient.lpush).toHaveBeenCalledWith(
        'job_queue:pending',
        JSON.stringify(jobData)
      );
      expect(mockRedisClient.hset).toHaveBeenCalledWith(
        `job:${jobData.jobId}`,
        expect.objectContaining({
          status: 'pending',
          data: JSON.stringify(jobData),
          createdAt: expect.any(String)
        })
      );
      expect(mockRedisClient.expire).toHaveBeenCalledWith(
        `job:${jobData.jobId}`,
        86400 // 24 hours
      );
    });

    it('should assign job ID if not provided', async () => {
      const jobData = {
        jobType: 'PAYROLL_CALCULATION',
        parameters: { employeeId: 'EMP001' }
      };

      mockRedisClient.lpush.mockResolvedValue(1);
      mockRedisClient.hset.mockResolvedValue(1);
      mockRedisClient.expire.mockResolvedValue(1);

      await jobQueue.add(jobData);

      const storedData = JSON.parse(mockRedisClient.lpush.mock.calls[0][1]);
      expect(storedData.jobId).toBeDefined();
      expect(storedData.jobId).toMatch(/^[0-9a-f-]{36}$/); // UUID format
    });

    it('should set job priority', async () => {
      const jobData = {
        jobId: 'high-priority-job',
        jobType: 'URGENT_CALCULATION',
        priority: 'high'
      };

      mockRedisClient.lpush.mockResolvedValue(1);
      mockRedisClient.hset.mockResolvedValue(1);
      mockRedisClient.expire.mockResolvedValue(1);

      await jobQueue.add(jobData);

      // High priority jobs should go to priority queue
      expect(mockRedisClient.lpush).toHaveBeenCalledWith(
        'job_queue:priority',
        JSON.stringify(jobData)
      );
    });

    it('should handle Redis errors', async () => {
      const jobData = { jobType: 'TEST' };
      mockRedisClient.lpush.mockRejectedValue(new Error('Redis connection failed'));

      await expect(jobQueue.add(jobData)).rejects.toThrow('Redis connection failed');
    });
  });

  describe('getJobStatus', () => {
    it('should return job status successfully', async () => {
      const jobId = 'test-job-123';
      const jobStatus = {
        status: 'processing',
        progress: '50',
        startedAt: '2023-12-01T10:00:00Z',
        data: JSON.stringify({ jobType: 'CREDIT_CALCULATION' })
      };

      mockRedisClient.hgetall.mockResolvedValue(jobStatus);

      const result = await jobQueue.getJobStatus(jobId);

      expect(result).toEqual({
        id: jobId,
        status: 'processing',
        progress: 50,
        startedAt: '2023-12-01T10:00:00Z',
        data: { jobType: 'CREDIT_CALCULATION' }
      });
      expect(mockRedisClient.hgetall).toHaveBeenCalledWith(`job:${jobId}`);
    });

    it('should return null for non-existent job', async () => {
      const jobId = 'non-existent-job';
      mockRedisClient.hgetall.mockResolvedValue(null);

      const result = await jobQueue.getJobStatus(jobId);

      expect(result).toBeNull();
    });

    it('should handle malformed job data', async () => {
      const jobId = 'malformed-job';
      const jobStatus = {
        status: 'processing',
        data: 'invalid json{'
      };

      mockRedisClient.hgetall.mockResolvedValue(jobStatus);

      const result = await jobQueue.getJobStatus(jobId);

      expect(result.data).toBeNull(); // Should handle JSON parse error gracefully
    });
  });

  describe('updateJobStatus', () => {
    it('should update job status successfully', async () => {
      const jobId = 'test-job-123';
      const updates = {
        status: 'processing',
        progress: 75,
        message: 'Processing payment calculations'
      };

      mockRedisClient.hset.mockResolvedValue(1);

      await jobQueue.updateJobStatus(jobId, updates);

      expect(mockRedisClient.hset).toHaveBeenCalledWith(
        `job:${jobId}`,
        expect.objectContaining({
          status: 'processing',
          progress: '75',
          message: 'Processing payment calculations',
          updatedAt: expect.any(String)
        })
      );
    });

    it('should broadcast status updates via WebSocket', async () => {
      const websocket = require('../../../utils/websocket');
      const jobId = 'test-job-123';
      const updates = { status: 'completed', progress: 100 };

      mockRedisClient.hset.mockResolvedValue(1);

      await jobQueue.updateJobStatus(jobId, updates);

      expect(websocket.broadcast).toHaveBeenCalledWith('job:status:update', {
        jobId,
        status: 'completed',
        progress: 100,
        timestamp: expect.any(String)
      });
    });
  });

  describe('getQueueStats', () => {
    it('should return queue statistics', async () => {
      mockRedisClient.llen
        .mockResolvedValueOnce(5) // pending
        .mockResolvedValueOnce(2) // priority
        .mockResolvedValueOnce(3); // processing

      const stats = await jobQueue.getQueueStats();

      expect(stats).toEqual({
        pending: 5,
        priority: 2,
        processing: 3,
        total: 10
      });
    });

    it('should handle Redis errors in stats', async () => {
      mockRedisClient.llen.mockRejectedValue(new Error('Redis error'));

      const stats = await jobQueue.getQueueStats();

      expect(stats).toEqual({
        pending: 0,
        priority: 0,
        processing: 0,
        total: 0
      });
    });
  });

  describe('cleanup', () => {
    it('should clean up expired jobs', async () => {
      const expiredJobs = ['job:expired-1', 'job:expired-2'];
      
      // Mock scan operation to find expired jobs
      mockRedisClient.scan = jest.fn().mockResolvedValue([
        '0', // cursor
        expiredJobs
      ]);
      
      mockRedisClient.hget.mockResolvedValue('2023-01-01T00:00:00Z'); // Old timestamp
      mockRedisClient.hdel.mockResolvedValue(1);

      await jobQueue.cleanup();

      expect(mockRedisClient.hdel).toHaveBeenCalledTimes(2);
    });
  });
});

describe('JobProcessor', () => {
  let jobProcessor;
  let mockJobQueue;
  let mockExecuteCobolProgram;

  beforeEach(() => {
    mockJobQueue = {
      getNext: jest.fn(),
      updateJobStatus: jest.fn(),
      markCompleted: jest.fn(),
      markFailed: jest.fn()
    };

    mockExecuteCobolProgram = jest.fn();

    jobProcessor = new JobProcessor(mockJobQueue, {
      executeCobolProgram: mockExecuteCobolProgram
    });

    jest.clearAllMocks();
  });

  describe('start', () => {
    it('should start processing jobs', async () => {
      const job = {
        jobId: 'test-job-123',
        jobType: 'CREDIT_CALCULATION',
        parameters: { customerId: 'CUST001' }
      };

      mockJobQueue.getNext
        .mockResolvedValueOnce(job)
        .mockResolvedValue(null); // Stop after first job

      mockExecuteCobolProgram.mockResolvedValue({
        output: 'calculation result',
        executionTime: 1500
      });

      jobProcessor.start();

      // Wait for processing
      await new Promise(resolve => setTimeout(resolve, 100));

      expect(mockJobQueue.updateJobStatus).toHaveBeenCalledWith(
        'test-job-123',
        expect.objectContaining({
          status: 'processing',
          startedAt: expect.any(String)
        })
      );
    });

    it('should handle job processing errors', async () => {
      const job = {
        jobId: 'failing-job',
        jobType: 'CREDIT_CALCULATION',
        parameters: { customerId: 'INVALID' }
      };

      mockJobQueue.getNext
        .mockResolvedValueOnce(job)
        .mockResolvedValue(null);

      mockExecuteCobolProgram.mockRejectedValue(new Error('COBOL execution failed'));

      jobProcessor.start();

      await new Promise(resolve => setTimeout(resolve, 100));

      expect(mockJobQueue.updateJobStatus).toHaveBeenCalledWith(
        'failing-job',
        expect.objectContaining({
          status: 'failed',
          error: 'COBOL execution failed'
        })
      );
    });

    it('should respect concurrency limits', async () => {
      const jobs = Array(10).fill().map((_, i) => ({
        jobId: `job-${i}`,
        jobType: 'SLOW_CALCULATION',
        parameters: {}
      }));

      let getNextCallCount = 0;
      mockJobQueue.getNext.mockImplementation(() => {
        if (getNextCallCount < jobs.length) {
          return Promise.resolve(jobs[getNextCallCount++]);
        }
        return Promise.resolve(null);
      });

      mockExecuteCobolProgram.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 200))
      );

      jobProcessor = new JobProcessor(mockJobQueue, {
        executeCobolProgram: mockExecuteCobolProgram
      }, { concurrency: 3 });

      jobProcessor.start();

      // Check that only max concurrent jobs are processed
      await new Promise(resolve => setTimeout(resolve, 50));
      
      // Should have started processing but not exceed concurrency
      expect(mockExecuteCobolProgram).toHaveBeenCalledTimes(3);
    });
  });

  describe('stop', () => {
    it('should stop processing gracefully', async () => {
      jobProcessor.start();
      
      mockJobQueue.getNext.mockImplementation(() => 
        new Promise(resolve => setTimeout(() => resolve(null), 1000))
      );

      jobProcessor.stop();

      expect(jobProcessor.isRunning).toBe(false);
    });

    it('should wait for current jobs to complete', async () => {
      const job = {
        jobId: 'long-running-job',
        jobType: 'SLOW_CALCULATION',
        parameters: {}
      };

      mockJobQueue.getNext
        .mockResolvedValueOnce(job)
        .mockResolvedValue(null);

      mockExecuteCobolProgram.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 500))
      );

      jobProcessor.start();

      // Wait a bit for job to start
      await new Promise(resolve => setTimeout(resolve, 50));

      const stopPromise = jobProcessor.stop();

      // Job should still be processing
      expect(jobProcessor.activeJobs.size).toBe(1);

      await stopPromise;

      // Job should be completed
      expect(jobProcessor.activeJobs.size).toBe(0);
    });
  });

  describe('processJob', () => {
    it('should process credit calculation jobs', async () => {
      const job = {
        jobId: 'credit-job',
        jobType: 'CREDIT_CALCULATION',
        parameters: {
          customerId: 'CUST001',
          creditAmount: 50000,
          income: 75000,
          existingDebt: 10000
        }
      };

      const expectedResult = {
        output: 'Y000045000000...',
        executionTime: 1200,
        approved: true
      };

      mockExecuteCobolProgram.mockResolvedValue(expectedResult);

      const result = await jobProcessor.processJob(job);

      expect(mockExecuteCobolProgram).toHaveBeenCalledWith(
        'CREDIT01',
        expect.any(String) // COBOL formatted input
      );
      expect(result).toEqual(expectedResult);
    });

    it('should process payroll calculation jobs', async () => {
      const job = {
        jobId: 'payroll-job',
        jobType: 'PAYROLL_CALCULATION',
        parameters: {
          employeeId: 'EMP001',
          hoursWorked: 40,
          hourlyRate: 25.00
        }
      };

      const expectedResult = {
        output: '0001000000...',
        executionTime: 800,
        grossPay: 1000.00
      };

      mockExecuteCobolProgram.mockResolvedValue(expectedResult);

      const result = await jobProcessor.processJob(job);

      expect(mockExecuteCobolProgram).toHaveBeenCalledWith(
        'PAYROLL01',
        expect.any(String)
      );
      expect(result).toEqual(expectedResult);
    });

    it('should handle unsupported job types', async () => {
      const job = {
        jobId: 'unknown-job',
        jobType: 'UNKNOWN_TYPE',
        parameters: {}
      };

      await expect(jobProcessor.processJob(job))
        .rejects.toThrow('Unsupported job type: UNKNOWN_TYPE');
    });

    it('should handle job timeout', async () => {
      const job = {
        jobId: 'timeout-job',
        jobType: 'CREDIT_CALCULATION',
        parameters: { customerId: 'CUST001' }
      };

      mockExecuteCobolProgram.mockImplementation(() => 
        new Promise(resolve => setTimeout(resolve, 10000)) // 10 seconds
      );

      jobProcessor = new JobProcessor(mockJobQueue, {
        executeCobolProgram: mockExecuteCobolProgram
      }, { timeout: 1000 }); // 1 second timeout

      await expect(jobProcessor.processJob(job))
        .rejects.toThrow('Job timeout');
    });
  });

  describe('getStats', () => {
    it('should return processor statistics', () => {
      jobProcessor.activeJobs.set('job1', { startTime: Date.now() });
      jobProcessor.activeJobs.set('job2', { startTime: Date.now() });
      jobProcessor.completedJobs = 15;
      jobProcessor.failedJobs = 3;

      const stats = jobProcessor.getStats();

      expect(stats).toEqual({
        isRunning: false,
        activeJobs: 2,
        completedJobs: 15,
        failedJobs: 3,
        uptime: expect.any(Number)
      });
    });
  });

  describe('Error Recovery', () => {
    it('should retry failed jobs', async () => {
      const job = {
        jobId: 'retry-job',
        jobType: 'CREDIT_CALCULATION',
        parameters: { customerId: 'CUST001' },
        retryCount: 0
      };

      mockExecuteCobolProgram
        .mockRejectedValueOnce(new Error('Temporary failure'))
        .mockResolvedValue({ output: 'success' });

      mockJobQueue.getNext
        .mockResolvedValueOnce(job)
        .mockResolvedValue(null);

      jobProcessor = new JobProcessor(mockJobQueue, {
        executeCobolProgram: mockExecuteCobolProgram
      }, { maxRetries: 3 });

      jobProcessor.start();

      await new Promise(resolve => setTimeout(resolve, 200));

      // Should have retried the job
      expect(mockExecuteCobolProgram).toHaveBeenCalledTimes(2);
    });

    it('should give up after max retries', async () => {
      const job = {
        jobId: 'max-retry-job',
        jobType: 'CREDIT_CALCULATION',
        parameters: { customerId: 'CUST001' },
        retryCount: 3
      };

      mockExecuteCobolProgram.mockRejectedValue(new Error('Persistent failure'));

      mockJobQueue.getNext
        .mockResolvedValueOnce(job)
        .mockResolvedValue(null);

      jobProcessor = new JobProcessor(mockJobQueue, {
        executeCobolProgram: mockExecuteCobolProgram
      }, { maxRetries: 3 });

      jobProcessor.start();

      await new Promise(resolve => setTimeout(resolve, 100));

      expect(mockJobQueue.updateJobStatus).toHaveBeenCalledWith(
        'max-retry-job',
        expect.objectContaining({
          status: 'failed',
          error: 'Persistent failure'
        })
      );
    });
  });
});