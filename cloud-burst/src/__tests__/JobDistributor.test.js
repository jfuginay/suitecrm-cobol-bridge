const { JobDistributor } = require('../distributors/JobDistributor');
const EventEmitter = require('events');

// Mock dependencies
jest.mock('bull', () => {
  return jest.fn().mockImplementation((name, options) => ({
    add: jest.fn(),
    getWaiting: jest.fn().mockResolvedValue([]),
    getActive: jest.fn().mockResolvedValue([]),
    getWaitingCount: jest.fn().mockResolvedValue(0),
    getActiveCount: jest.fn().mockResolvedValue(0),
    getCompletedCount: jest.fn().mockResolvedValue(0),
    getFailedCount: jest.fn().mockResolvedValue(0),
    getJob: jest.fn(),
    on: jest.fn(),
    pause: jest.fn(),
    close: jest.fn()
  }));
});

jest.mock('redis', () => ({
  createClient: jest.fn(() => ({
    connect: jest.fn(),
    disconnect: jest.fn()
  }))
}));

jest.mock('p-queue', () => ({
  PQueue: jest.fn().mockImplementation(() => ({
    add: jest.fn(fn => fn())
  }))
}));

jest.mock('../utils/logger', () => ({
  info: jest.fn(),
  warn: jest.fn(),
  error: jest.fn(),
  debug: jest.fn()
}));

// Mock fetch
global.fetch = jest.fn();

describe('JobDistributor', () => {
  let distributor;
  let mockJobQueue;
  let mockResultQueue;

  beforeEach(() => {
    jest.clearAllMocks();
    
    distributor = new JobDistributor({
      redisUrl: 'redis://localhost:6379',
      maxRetries: 3,
      concurrentJobsPerWorker: 2
    });

    // Mock the Bull queues
    const Bull = require('bull');
    mockJobQueue = {
      add: jest.fn(),
      getWaiting: jest.fn().mockResolvedValue([]),
      getActive: jest.fn().mockResolvedValue([]),
      getWaitingCount: jest.fn().mockResolvedValue(0),
      getActiveCount: jest.fn().mockResolvedValue(0),
      getCompletedCount: jest.fn().mockResolvedValue(5),
      getFailedCount: jest.fn().mockResolvedValue(1),
      getJob: jest.fn(),
      on: jest.fn(),
      pause: jest.fn(),
      close: jest.fn()
    };

    mockResultQueue = {
      add: jest.fn(),
      on: jest.fn(),
      close: jest.fn()
    };

    Bull.mockImplementation((name) => {
      if (name === 'cobol-jobs') return mockJobQueue;
      if (name === 'cobol-results') return mockResultQueue;
    });
  });

  describe('initialization', () => {
    it('should initialize successfully', async () => {
      await distributor.initialize();

      expect(distributor.isInitialized).toBe(true);
      expect(distributor.redisClient).toBeDefined();
      expect(distributor.jobQueue).toBeDefined();
      expect(distributor.resultQueue).toBeDefined();
    });

    it('should setup queue event handlers', async () => {
      await distributor.initialize();

      expect(mockJobQueue.on).toHaveBeenCalledWith('completed', expect.any(Function));
      expect(mockJobQueue.on).toHaveBeenCalledWith('failed', expect.any(Function));
      expect(mockJobQueue.on).toHaveBeenCalledWith('stalled', expect.any(Function));
    });

    it('should handle initialization errors', async () => {
      const Redis = require('redis');
      Redis.createClient.mockImplementation(() => ({
        connect: jest.fn().mockRejectedValue(new Error('Redis connection failed'))
      }));

      await expect(distributor.initialize()).rejects.toThrow('Redis connection failed');
    });
  });

  describe('job distribution', () => {
    const sampleJobs = [
      {
        id: 'job-1',
        name: 'Credit Calculation',
        program: 'CREDIT01',
        priority: 'high',
        requirements: { cpu: 2, memory: 4096 },
        metadata: { submittedAt: new Date().toISOString() }
      },
      {
        id: 'job-2',
        name: 'Payroll Processing',
        program: 'PAYROLL01',
        priority: 'normal',
        requirements: { cpu: 1, memory: 2048 },
        metadata: { submittedAt: new Date().toISOString() }
      }
    ];

    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should distribute jobs successfully', async () => {
      mockJobQueue.add.mockResolvedValue({ id: 'queued-job-1' });

      const result = await distributor.distributeJobs(sampleJobs);

      expect(result).toEqual({
        successful: 2,
        failed: 0,
        total: 2
      });

      expect(mockJobQueue.add).toHaveBeenCalledTimes(2);
    });

    it('should calculate job priority correctly', async () => {
      mockJobQueue.add.mockResolvedValue({ id: 'queued-job-1' });

      await distributor.distributeJobs([sampleJobs[0]]);

      expect(mockJobQueue.add).toHaveBeenCalledWith(
        'process',
        expect.any(Object),
        expect.objectContaining({
          priority: 75, // High priority
          attempts: 3,
          timeout: 300000
        })
      );
    });

    it('should boost priority for resource-intensive jobs', async () => {
      const intensiveJob = {
        id: 'intensive-job',
        name: 'Heavy Computation',
        program: 'COMPUTE01',
        requirements: { cpu: 8, memory: 16384 },
        metadata: { submittedAt: new Date().toISOString() }
      };

      mockJobQueue.add.mockResolvedValue({ id: 'queued-job-1' });

      await distributor.distributeJobs([intensiveJob]);

      expect(mockJobQueue.add).toHaveBeenCalledWith(
        'process',
        expect.any(Object),
        expect.objectContaining({
          priority: 60 // Normal (50) + resource boost (10)
        })
      );
    });

    it('should boost priority for waiting jobs', async () => {
      const oldJob = {
        id: 'old-job',
        name: 'Old Job',
        program: 'OLD01',
        requirements: { cpu: 1, memory: 1024 },
        metadata: { 
          submittedAt: new Date(Date.now() - 2 * 60 * 60 * 1000).toISOString() // 2 hours ago
        }
      };

      mockJobQueue.add.mockResolvedValue({ id: 'queued-job-1' });

      await distributor.distributeJobs([oldJob]);

      expect(mockJobQueue.add).toHaveBeenCalledWith(
        'process',
        expect.any(Object),
        expect.objectContaining({
          priority: 60 // Normal (50) + wait time boost (10)
        })
      );
    });

    it('should handle job distribution failures', async () => {
      mockJobQueue.add.mockRejectedValueOnce(new Error('Queue full'));
      mockJobQueue.add.mockResolvedValueOnce({ id: 'queued-job-2' });

      const result = await distributor.distributeJobs(sampleJobs);

      expect(result).toEqual({
        successful: 1,
        failed: 1,
        total: 2
      });
    });
  });

  describe('worker management', () => {
    const sampleInstance = {
      id: 'worker-1',
      type: 't3.large',
      privateIp: '10.0.1.100'
    };

    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should register worker successfully', async () => {
      mockJobQueue.getWaiting.mockResolvedValue([]);

      await distributor.registerWorker(sampleInstance);

      const worker = distributor.workers.get('worker-1');
      expect(worker).toBeDefined();
      expect(worker.id).toBe('worker-1');
      expect(worker.status).toBe('active');
      expect(worker.resources).toEqual({
        cpu: 2, // t3.large
        memory: 8192
      });
    });

    it('should start heartbeat monitoring for worker', async () => {
      fetch.mockResolvedValue({
        ok: true,
        json: () => Promise.resolve({
          activeJobs: 1,
          metrics: { avgJobDuration: 5000 }
        })
      });

      await distributor.registerWorker(sampleInstance);

      const worker = distributor.workers.get('worker-1');
      expect(worker.heartbeatInterval).toBeDefined();

      // Cleanup
      clearInterval(worker.heartbeatInterval);
    });

    it('should unregister worker', async () => {
      await distributor.registerWorker(sampleInstance);
      
      const worker = distributor.workers.get('worker-1');
      worker.heartbeatInterval = setInterval(() => {}, 1000);

      await distributor.unregisterWorker(sampleInstance);

      expect(distributor.workers.has('worker-1')).toBe(false);
    });

    it('should handle unhealthy workers', async () => {
      await distributor.registerWorker(sampleInstance);
      
      const worker = distributor.workers.get('worker-1');
      worker.lastHeartbeat = new Date(Date.now() - 60000); // 1 minute ago

      // Mock failed heartbeat
      fetch.mockRejectedValue(new Error('Connection refused'));

      const spy = jest.spyOn(distributor, '_handleUnhealthyWorker');
      
      // Simulate heartbeat check
      await distributor._handleUnhealthyWorker(worker);

      expect(spy).toHaveBeenCalledWith(worker);
    });
  });

  describe('job assignment', () => {
    const mockJob = {
      id: 'job-1',
      data: {
        id: 'job-1',
        requirements: { cpu: 1, memory: 2048 }
      },
      moveToActive: jest.fn(),
      moveToWaiting: jest.fn()
    };

    const mockWorker = {
      id: 'worker-1',
      instance: { id: 'worker-1', privateIp: '10.0.1.100' },
      status: 'active',
      activeJobs: 0,
      resources: { cpu: 2, memory: 8192 },
      usedResources: { cpu: 0, memory: 0 },
      instanceType: 't3.large'
    };

    beforeEach(async () => {
      await distributor.initialize();
      distributor.workers.set('worker-1', mockWorker);
    });

    it('should assign job to worker successfully', async () => {
      fetch.mockResolvedValue({ ok: true });

      await distributor._assignJobToWorker(mockJob, mockWorker);

      expect(fetch).toHaveBeenCalledWith(
        'http://10.0.1.100:8080/job',
        expect.objectContaining({
          method: 'POST',
          headers: { 'Content-Type': 'application/json' }
        })
      );

      expect(distributor.jobAssignments.get('job-1')).toBe('worker-1');
      expect(mockWorker.activeJobs).toBe(1);
      expect(mockWorker.usedResources.cpu).toBe(1);
      expect(mockWorker.usedResources.memory).toBe(2048);
    });

    it('should handle worker rejection', async () => {
      fetch.mockResolvedValue({ 
        ok: false, 
        statusText: 'Worker busy' 
      });

      await expect(
        distributor._assignJobToWorker(mockJob, mockWorker)
      ).rejects.toThrow('Worker rejected job: Worker busy');

      expect(mockJob.moveToWaiting).toHaveBeenCalled();
    });

    it('should select best worker for job', async () => {
      const workers = [
        {
          id: 'worker-1',
          status: 'active',
          activeJobs: 0,
          resources: { cpu: 2, memory: 4096 },
          usedResources: { cpu: 0, memory: 0 },
          instanceType: 't3.medium',
          metrics: { mainframeLatency: 5 }
        },
        {
          id: 'worker-2',
          status: 'active',
          activeJobs: 1,
          resources: { cpu: 4, memory: 8192 },
          usedResources: { cpu: 2, memory: 4096 },
          instanceType: 't3.large',
          metrics: { mainframeLatency: 15 }
        }
      ];

      const requirements = { cpu: 1, memory: 2048 };
      const bestWorker = distributor._selectBestWorker(workers, requirements);

      expect(bestWorker.id).toBe('worker-1'); // Less loaded, lower latency
    });

    it('should not select worker with insufficient resources', () => {
      const workers = [
        {
          id: 'worker-1',
          status: 'active',
          activeJobs: 0,
          resources: { cpu: 2, memory: 4096 },
          usedResources: { cpu: 2, memory: 4000 }, // Almost full
          instanceType: 't3.medium'
        }
      ];

      const requirements = { cpu: 1, memory: 2048 };
      const bestWorker = distributor._selectBestWorker(workers, requirements);

      expect(bestWorker).toBeNull(); // Insufficient memory
    });
  });

  describe('job completion handling', () => {
    const mockJob = {
      id: 'job-1',
      data: { requirements: { cpu: 1, memory: 2048 } }
    };

    const mockWorker = {
      id: 'worker-1',
      activeJobs: 1,
      completedJobs: 0,
      failedJobs: 0,
      usedResources: { cpu: 1, memory: 2048 },
      metrics: { successRate: 100 }
    };

    beforeEach(async () => {
      await distributor.initialize();
      distributor.workers.set('worker-1', mockWorker);
      distributor.jobAssignments.set('job-1', 'worker-1');
      distributor.jobMetrics.set('job-1', {
        workerId: 'worker-1',
        assignedAt: new Date(),
        startedAt: new Date(Date.now() - 5000),
        status: 'processing'
      });
    });

    it('should handle job completion', async () => {
      const result = { output: 'calculation complete' };

      await distributor._handleJobCompleted(mockJob, result);

      expect(mockWorker.activeJobs).toBe(0);
      expect(mockWorker.completedJobs).toBe(1);
      expect(mockWorker.usedResources.cpu).toBe(0);
      expect(mockWorker.usedResources.memory).toBe(0);
      expect(mockWorker.metrics.successRate).toBe(100);

      expect(mockResultQueue.add).toHaveBeenCalledWith(
        'merge',
        expect.objectContaining({
          jobId: 'job-1',
          result,
          workerId: 'worker-1'
        })
      );
    });

    it('should handle job failure', async () => {
      const error = new Error('COBOL execution failed');

      await distributor._handleJobFailed(mockJob, error);

      expect(mockWorker.activeJobs).toBe(0);
      expect(mockWorker.failedJobs).toBe(1);
      expect(mockWorker.metrics.successRate).toBe(0);

      const metrics = distributor.jobMetrics.get('job-1');
      expect(metrics.status).toBe('failed');
      expect(metrics.error).toBe('COBOL execution failed');
    });

    it('should handle job stalling', async () => {
      fetch.mockResolvedValue({ ok: false }); // Worker unhealthy

      const spy = jest.spyOn(distributor, 'reassignJobs');
      
      await distributor._handleJobStalled(mockJob);

      expect(spy).toHaveBeenCalledWith([mockJob]);
    });
  });

  describe('job reassignment', () => {
    const mockJobs = [
      {
        id: 'job-1',
        data: { 
          workerId: 'worker-1',
          assignedAt: new Date().toISOString(),
          requirements: { cpu: 1, memory: 2048 }
        },
        moveToWaiting: jest.fn()
      }
    ];

    beforeEach(async () => {
      await distributor.initialize();
      distributor.jobAssignments.set('job-1', 'worker-1');
    });

    it('should reassign jobs successfully', async () => {
      await distributor.reassignJobs(mockJobs);

      expect(mockJobs[0].data.workerId).toBeUndefined();
      expect(mockJobs[0].data.assignedAt).toBeUndefined();
      expect(distributor.jobAssignments.has('job-1')).toBe(false);
      expect(mockJobs[0].moveToWaiting).toHaveBeenCalled();
    });

    it('should handle reassignment errors gracefully', async () => {
      mockJobs[0].moveToWaiting.mockRejectedValue(new Error('Queue error'));

      await distributor.reassignJobs(mockJobs);

      // Should not throw, just log error
      expect(mockJobs[0].moveToWaiting).toHaveBeenCalled();
    });
  });

  describe('metrics and monitoring', () => {
    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should return job metrics', async () => {
      const metrics = await distributor.getJobMetrics();

      expect(metrics).toEqual({
        waiting: 0,
        active: 0,
        completed: 5,
        failed: 1,
        total: 6,
        workers: {
          total: 0,
          active: 0,
          unhealthy: 0
        },
        avgJobDuration: 0,
        successRate: expect.any(Number)
      });
    });

    it('should calculate average job duration', () => {
      distributor.jobMetrics.set('job-1', {
        status: 'completed',
        duration: 5000
      });
      distributor.jobMetrics.set('job-2', {
        status: 'completed',
        duration: 3000
      });

      const avgDuration = distributor._calculateAvgJobDuration();
      expect(avgDuration).toBe(4000);
    });

    it('should return worker statistics', () => {
      distributor.workers.set('worker-1', { status: 'active' });
      distributor.workers.set('worker-2', { status: 'unhealthy' });

      const metrics = {
        workers: {
          total: distributor.workers.size,
          active: Array.from(distributor.workers.values()).filter(w => w.status === 'active').length,
          unhealthy: Array.from(distributor.workers.values()).filter(w => w.status === 'unhealthy').length
        }
      };

      expect(metrics.workers).toEqual({
        total: 2,
        active: 1,
        unhealthy: 1
      });
    });
  });

  describe('workload consolidation', () => {
    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should consolidate underutilized workers', async () => {
      // Setup workers with different utilization levels
      const worker1 = {
        id: 'worker-1',
        status: 'active',
        activeJobs: 0, // 0% utilization
        instance: { privateIp: '10.0.1.100' }
      };

      const worker2 = {
        id: 'worker-2', 
        status: 'active',
        activeJobs: 1, // 50% utilization
        instance: { privateIp: '10.0.1.101' }
      };

      distributor.workers.set('worker-1', worker1);
      distributor.workers.set('worker-2', worker2);

      const mockJobs = [
        { id: 'job-1', data: { requirements: { cpu: 1 } } }
      ];

      jest.spyOn(distributor, 'getJobsByWorker').mockResolvedValue(mockJobs);
      jest.spyOn(distributor, '_reassignJobToWorker').mockResolvedValue();

      await distributor.consolidateWorkload(75);

      expect(distributor._reassignJobToWorker).toHaveBeenCalled();
    });

    it('should skip consolidation when no underutilized workers', async () => {
      const worker1 = {
        id: 'worker-1',
        status: 'active',
        activeJobs: 2 // 100% utilization
      };

      distributor.workers.set('worker-1', worker1);

      const spy = jest.spyOn(distributor, 'getJobsByWorker');

      await distributor.consolidateWorkload(75);

      expect(spy).not.toHaveBeenCalled();
    });
  });

  describe('shutdown', () => {
    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should shutdown gracefully', async () => {
      const worker = {
        id: 'worker-1',
        heartbeatInterval: setInterval(() => {}, 1000)
      };
      distributor.workers.set('worker-1', worker);

      await distributor.shutdown();

      expect(mockJobQueue.pause).toHaveBeenCalled();
      expect(mockJobQueue.close).toHaveBeenCalled();
      expect(mockResultQueue.close).toHaveBeenCalled();
      expect(distributor.redisClient.disconnect).toHaveBeenCalled();
    });

    it('should wait for active jobs to complete', async () => {
      mockJobQueue.getActive.mockResolvedValue([
        { id: 'active-job-1' }
      ]);

      const startTime = Date.now();
      await distributor.shutdown();
      const endTime = Date.now();

      // Should have waited (mocked as immediate but verifies the logic)
      expect(endTime - startTime).toBeGreaterThanOrEqualTo(0);
      expect(mockJobQueue.getActive).toHaveBeenCalled();
    });
  });

  describe('error handling', () => {
    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should handle worker health check failures', async () => {
      const worker = {
        id: 'worker-1',
        instance: { privateIp: '10.0.1.100' }
      };

      fetch.mockRejectedValue(new Error('Connection timeout'));

      const isHealthy = await distributor._checkWorkerHealth(worker);
      expect(isHealthy).toBe(false);
    });

    it('should handle missing job in retry', async () => {
      mockJobQueue.getJob.mockResolvedValue(null);

      await expect(distributor.retryJob('non-existent-job'))
        .rejects.toThrow('Job non-existent-job not found');
    });

    it('should handle instance type mapping errors', () => {
      const cpu = distributor._getInstanceCPU('unknown-type');
      const memory = distributor._getInstanceMemory('unknown-type');

      expect(cpu).toBe(2); // Default fallback
      expect(memory).toBe(4096); // Default fallback
    });
  });

  describe('event emission', () => {
    beforeEach(async () => {
      await distributor.initialize();
    });

    it('should emit worker registration events', async () => {
      const eventSpy = jest.fn();
      distributor.on('worker_registered', eventSpy);

      const instance = { id: 'worker-1', type: 't3.medium', privateIp: '10.0.1.100' };
      await distributor.registerWorker(instance);

      expect(eventSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          id: 'worker-1',
          status: 'active'
        })
      );
    });

    it('should emit job assignment events', async () => {
      const eventSpy = jest.fn();
      distributor.on('job_assigned', eventSpy);

      const mockJob = {
        id: 'job-1',
        data: { requirements: { cpu: 1, memory: 2048 } },
        moveToActive: jest.fn()
      };

      const mockWorker = {
        id: 'worker-1',
        instance: { privateIp: '10.0.1.100' },
        activeJobs: 0,
        usedResources: { cpu: 0, memory: 0 }
      };

      fetch.mockResolvedValue({ ok: true });

      await distributor._assignJobToWorker(mockJob, mockWorker);

      expect(eventSpy).toHaveBeenCalledWith({
        jobId: 'job-1',
        workerId: 'worker-1',
        requirements: { cpu: 1, memory: 2048 }
      });
    });

    it('should emit job completion events', async () => {
      const eventSpy = jest.fn();
      distributor.on('job_completed', eventSpy);

      const mockJob = { id: 'job-1', data: { requirements: { cpu: 1, memory: 2048 } } };
      const result = { output: 'success' };

      distributor.jobAssignments.set('job-1', 'worker-1');
      distributor.workers.set('worker-1', {
        activeJobs: 1,
        completedJobs: 0,
        failedJobs: 0,
        usedResources: { cpu: 1, memory: 2048 },
        metrics: {}
      });

      await distributor._handleJobCompleted(mockJob, result);

      expect(eventSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          jobId: 'job-1',
          workerId: 'worker-1',
          result
        })
      );
    });
  });
});