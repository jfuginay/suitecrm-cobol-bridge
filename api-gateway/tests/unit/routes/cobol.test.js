const request = require('supertest');
const express = require('express');
const cobolRoutes = require('../../../routes/cobol');
const { CobolDataConverter } = require('../../../utils/cobolWrapper');

// Mock dependencies
jest.mock('../../../middleware/auth', () => ({
  authenticateToken: (req, res, next) => {
    req.user = { id: 1, username: 'testuser', role: 'user' };
    next();
  }
}));

jest.mock('../../../middleware/rateLimiter', () => ({
  rateLimiter: (req, res, next) => next()
}));

jest.mock('../../../middleware/cache', () => ({
  cacheMiddleware: () => (req, res, next) => next()
}));

jest.mock('../../../utils/cobolWrapper', () => ({
  executeCobolProgram: jest.fn(),
  CobolDataConverter: {
    toCreditInput: jest.fn(),
    fromCreditOutput: jest.fn(),
    toPayrollInput: jest.fn(),
    fromPayrollOutput: jest.fn(),
    toInterestInput: jest.fn(),
    fromInterestOutput: jest.fn()
  }
}));

jest.mock('../../../utils/logger', () => ({
  info: jest.fn(),
  error: jest.fn()
}));

describe('COBOL Routes', () => {
  let app;
  let mockJobQueue;

  beforeEach(() => {
    app = express();
    app.use(express.json());
    
    // Mock job queue
    mockJobQueue = {
      add: jest.fn().mockResolvedValue(),
      getJobStatus: jest.fn()
    };
    
    app.locals.jobQueue = mockJobQueue;
    app.use('/api/v1/cobol', cobolRoutes);
    
    jest.clearAllMocks();
  });

  describe('POST /credit/calculate', () => {
    const validCreditData = {
      customerId: 'CUST001',
      creditAmount: 50000,
      income: 75000,
      existingDebt: 10000
    };

    it('should calculate credit successfully', async () => {
      const mockCobolInput = 'CUST001   000050000...';
      const mockCobolOutput = 'Y000045000000...';
      const mockResult = {
        approved: true,
        creditLimit: 45000,
        interestRate: 5.5,
        monthlyPayment: 850,
        riskScore: 350
      };

      CobolDataConverter.toCreditInput.mockReturnValue(mockCobolInput);
      CobolDataConverter.fromCreditOutput.mockReturnValue(mockResult);
      
      const { executeCobolProgram } = require('../../../utils/cobolWrapper');
      executeCobolProgram.mockResolvedValue(mockCobolOutput);

      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send(validCreditData)
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: mockResult,
        timestamp: expect.any(String)
      });

      expect(CobolDataConverter.toCreditInput).toHaveBeenCalledWith(validCreditData);
      expect(executeCobolProgram).toHaveBeenCalledWith('CREDIT01', mockCobolInput);
      expect(CobolDataConverter.fromCreditOutput).toHaveBeenCalledWith(mockCobolOutput);
    });

    it('should validate required fields', async () => {
      const invalidData = {
        customerId: 'CUST001',
        // Missing required fields
      };

      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body).toHaveProperty('errors');
      expect(response.body.errors).toBeInstanceOf(Array);
    });

    it('should validate field types', async () => {
      const invalidData = {
        customerId: 'CUST001',
        creditAmount: 'not-a-number',
        income: 75000,
        existingDebt: 10000
      };

      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'creditAmount',
            msg: expect.stringContaining('Invalid value')
          })
        ])
      );
    });

    it('should validate minimum values', async () => {
      const invalidData = {
        customerId: 'CUST001',
        creditAmount: -1000,
        income: -500,
        existingDebt: -100
      };

      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors.length).toBeGreaterThan(0);
    });

    it('should handle COBOL execution errors', async () => {
      const { executeCobolProgram } = require('../../../utils/cobolWrapper');
      executeCobolProgram.mockRejectedValue(new Error('COBOL execution failed'));

      CobolDataConverter.toCreditInput.mockReturnValue('test input');

      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send(validCreditData)
        .expect(500);

      expect(response.body).toEqual({
        success: false,
        error: 'Credit calculation failed',
        message: 'COBOL execution failed'
      });
    });
  });

  describe('POST /payroll/calculate', () => {
    const validPayrollData = {
      employeeId: 'EMP001',
      hoursWorked: 40.5,
      hourlyRate: 25.75,
      deductions: {
        federal: 150.00,
        state: 50.00,
        insurance: 75.25
      }
    };

    it('should calculate payroll successfully', async () => {
      const mockCobolInput = 'EMP001    004050...';
      const mockCobolOutput = '0001042875...';
      const mockResult = {
        grossPay: 1042.88,
        deductions: {
          federal: 208.58,
          state: 52.14,
          insurance: 75.25
        },
        netPay: 706.91
      };

      CobolDataConverter.toPayrollInput.mockReturnValue(mockCobolInput);
      CobolDataConverter.fromPayrollOutput.mockReturnValue(mockResult);
      
      const { executeCobolProgram } = require('../../../utils/cobolWrapper');
      executeCobolProgram.mockResolvedValue(mockCobolOutput);

      const response = await request(app)
        .post('/api/v1/cobol/payroll/calculate')
        .send(validPayrollData)
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: mockResult,
        timestamp: expect.any(String)
      });

      expect(CobolDataConverter.toPayrollInput).toHaveBeenCalledWith(validPayrollData);
      expect(executeCobolProgram).toHaveBeenCalledWith('PAYROLL01', mockCobolInput);
      expect(CobolDataConverter.fromPayrollOutput).toHaveBeenCalledWith(mockCobolOutput);
    });

    it('should validate hours worked limits', async () => {
      const invalidData = {
        ...validPayrollData,
        hoursWorked: 200 // More than 168 hours in a week
      };

      const response = await request(app)
        .post('/api/v1/cobol/payroll/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'hoursWorked'
          })
        ])
      );
    });

    it('should require deductions object', async () => {
      const invalidData = {
        employeeId: 'EMP001',
        hoursWorked: 40,
        hourlyRate: 25.75
        // Missing deductions
      };

      const response = await request(app)
        .post('/api/v1/cobol/payroll/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'deductions'
          })
        ])
      );
    });
  });

  describe('POST /interest/calculate', () => {
    const validInterestData = {
      principal: 100000,
      rate: 5.5,
      term: 30
    };

    it('should calculate interest successfully', async () => {
      const mockCobolInput = '000010000000...';
      const mockCobolOutput = '000019758200...';
      const mockResult = {
        totalInterest: 197582.00,
        totalAmount: 297582.00,
        monthlyPayment: 567.89
      };

      CobolDataConverter.toInterestInput.mockReturnValue(mockCobolInput);
      CobolDataConverter.fromInterestOutput.mockReturnValue(mockResult);
      
      const { executeCobolProgram } = require('../../../utils/cobolWrapper');
      executeCobolProgram.mockResolvedValue(mockCobolOutput);

      const response = await request(app)
        .post('/api/v1/cobol/interest/calculate')
        .send(validInterestData)
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: mockResult,
        timestamp: expect.any(String)
      });

      expect(CobolDataConverter.toInterestInput).toHaveBeenCalledWith(validInterestData);
      expect(executeCobolProgram).toHaveBeenCalledWith('INTEREST01', mockCobolInput);
      expect(CobolDataConverter.fromInterestOutput).toHaveBeenCalledWith(mockCobolOutput);
    });

    it('should validate rate limits', async () => {
      const invalidData = {
        ...validInterestData,
        rate: 150 // Rate over 100%
      };

      const response = await request(app)
        .post('/api/v1/cobol/interest/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'rate'
          })
        ])
      );
    });

    it('should validate term as integer', async () => {
      const invalidData = {
        ...validInterestData,
        term: 30.5 // Should be integer
      };

      const response = await request(app)
        .post('/api/v1/cobol/interest/calculate')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'term'
          })
        ])
      );
    });
  });

  describe('POST /batch/process', () => {
    const validBatchData = {
      jobType: 'DAILY_INTEREST',
      parameters: {
        date: '2023-12-01',
        accounts: ['ACC001', 'ACC002']
      }
    };

    it('should queue batch job successfully', async () => {
      mockJobQueue.add.mockResolvedValue();

      const response = await request(app)
        .post('/api/v1/cobol/batch/process')
        .send(validBatchData)
        .expect(202);

      expect(response.body).toEqual({
        success: true,
        jobId: expect.any(String),
        status: 'queued',
        message: 'Batch job queued for processing',
        statusUrl: expect.stringMatching(/\/api\/v1\/cobol\/batch\/status\//)
      });

      expect(mockJobQueue.add).toHaveBeenCalledWith({
        jobId: expect.any(String),
        jobType: 'DAILY_INTEREST',
        parameters: validBatchData.parameters,
        userId: 1
      });
    });

    it('should validate job type', async () => {
      const invalidData = {
        jobType: 'INVALID_JOB',
        parameters: {}
      };

      const response = await request(app)
        .post('/api/v1/cobol/batch/process')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'jobType'
          })
        ])
      );
    });

    it('should require parameters object', async () => {
      const invalidData = {
        jobType: 'DAILY_INTEREST'
        // Missing parameters
      };

      const response = await request(app)
        .post('/api/v1/cobol/batch/process')
        .send(invalidData)
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'parameters'
          })
        ])
      );
    });

    it('should handle job queue errors', async () => {
      mockJobQueue.add.mockRejectedValue(new Error('Queue is full'));

      const response = await request(app)
        .post('/api/v1/cobol/batch/process')
        .send(validBatchData)
        .expect(500);

      expect(response.body).toEqual({
        success: false,
        error: 'Failed to queue batch job',
        message: 'Queue is full'
      });
    });
  });

  describe('GET /batch/status/:jobId', () => {
    it('should return job status successfully', async () => {
      const jobId = 'test-job-123';
      const jobStatus = {
        id: jobId,
        status: 'processing',
        progress: 50,
        startTime: new Date().toISOString(),
        estimatedCompletion: new Date(Date.now() + 60000).toISOString()
      };

      mockJobQueue.getJobStatus.mockResolvedValue(jobStatus);

      const response = await request(app)
        .get(`/api/v1/cobol/batch/status/${jobId}`)
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: jobStatus
      });

      expect(mockJobQueue.getJobStatus).toHaveBeenCalledWith(jobId);
    });

    it('should return 404 for non-existent job', async () => {
      const jobId = 'non-existent-job';
      mockJobQueue.getJobStatus.mockResolvedValue(null);

      const response = await request(app)
        .get(`/api/v1/cobol/batch/status/${jobId}`)
        .expect(404);

      expect(response.body).toEqual({
        success: false,
        error: 'Job not found'
      });
    });

    it('should handle job queue errors', async () => {
      const jobId = 'test-job-123';
      mockJobQueue.getJobStatus.mockRejectedValue(new Error('Database connection failed'));

      const response = await request(app)
        .get(`/api/v1/cobol/batch/status/${jobId}`)
        .expect(500);

      expect(response.body).toEqual({
        success: false,
        error: 'Failed to retrieve job status',
        message: 'Database connection failed'
      });
    });
  });

  describe('POST /execute/:programName', () => {
    beforeEach(() => {
      process.env.ALLOWED_COBOL_PROGRAMS = 'TEST_PROG,CUSTOM_CALC';
    });

    afterEach(() => {
      delete process.env.ALLOWED_COBOL_PROGRAMS;
    });

    it('should execute allowed custom program', async () => {
      const programName = 'TEST_PROG';
      const inputData = { value1: 100, value2: 200 };
      const expectedOutput = 'RESULT: 300';

      const { executeCobolProgram } = require('../../../utils/cobolWrapper');
      executeCobolProgram.mockResolvedValue(expectedOutput);

      const response = await request(app)
        .post(`/api/v1/cobol/execute/${programName}`)
        .send({ input: inputData })
        .expect(200);

      expect(response.body).toEqual({
        success: true,
        data: expectedOutput,
        timestamp: expect.any(String)
      });

      expect(executeCobolProgram).toHaveBeenCalledWith(programName, inputData);
    });

    it('should reject non-allowed programs', async () => {
      const programName = 'MALICIOUS_PROG';
      const inputData = { value: 100 };

      const response = await request(app)
        .post(`/api/v1/cobol/execute/${programName}`)
        .send({ input: inputData })
        .expect(403);

      expect(response.body).toEqual({
        success: false,
        error: 'Program not allowed'
      });
    });

    it('should require input object', async () => {
      const programName = 'TEST_PROG';

      const response = await request(app)
        .post(`/api/v1/cobol/execute/${programName}`)
        .send({}) // Missing input
        .expect(400);

      expect(response.body.errors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            path: 'input'
          })
        ])
      );
    });

    it('should handle program execution errors', async () => {
      const programName = 'TEST_PROG';
      const inputData = { value: 100 };

      const { executeCobolProgram } = require('../../../utils/cobolWrapper');
      executeCobolProgram.mockRejectedValue(new Error('Program crashed'));

      const response = await request(app)
        .post(`/api/v1/cobol/execute/${programName}`)
        .send({ input: inputData })
        .expect(500);

      expect(response.body).toEqual({
        success: false,
        error: 'Program execution failed',
        message: 'Program crashed'
      });
    });
  });

  describe('Error Handling', () => {
    it('should handle JSON parsing errors', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send('invalid json')
        .set('Content-Type', 'application/json')
        .expect(400);

      // Express should handle JSON parsing errors before reaching our routes
    });

    it('should handle missing content-type header', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/credit/calculate')
        .send('customerId=CUST001&creditAmount=50000')
        .expect(400);

      // Should fail validation since body won't be parsed as JSON
    });
  });

  describe('Route Protection', () => {
    it('should protect all routes with authentication', () => {
      // This test verifies that all routes use the authenticateToken middleware
      // The actual authentication testing is done in the auth middleware tests
      const routeLayer = cobolRoutes.stack.find(layer => 
        layer.name === 'authenticateToken'
      );
      expect(routeLayer).toBeDefined();
    });

    it('should apply rate limiting to all routes', () => {
      // This test verifies that all routes use the rate limiter middleware
      const routeLayer = cobolRoutes.stack.find(layer => 
        layer.name === 'rateLimiter'
      );
      expect(routeLayer).toBeDefined();
    });
  });
});