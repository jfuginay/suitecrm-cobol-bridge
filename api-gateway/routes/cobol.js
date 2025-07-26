const express = require('express');
const router = express.Router();
const { body, validationResult } = require('express-validator');
const { authenticateToken } = require('../middleware/auth');
const { rateLimiter } = require('../middleware/rateLimiter');
const { executeCobolProgram, CobolDataConverter } = require('../utils/cobolWrapper');
const logger = require('../utils/logger');
const { cacheMiddleware } = require('../middleware/cache');

// Apply authentication and rate limiting to all COBOL routes
router.use(authenticateToken);
router.use(rateLimiter);

// Credit calculation endpoint
router.post('/credit/calculate',
  [
    body('customerId').isString().notEmpty(),
    body('creditAmount').isNumeric().isFloat({ min: 0 }),
    body('income').isNumeric().isFloat({ min: 0 }),
    body('existingDebt').isNumeric().isFloat({ min: 0 })
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { customerId, creditAmount, income, existingDebt } = req.body;
      
      // Convert JSON to COBOL format
      const cobolData = CobolDataConverter.toCreditInput({
        customerId,
        creditAmount,
        income,
        existingDebt
      });

      // Execute COBOL program
      const result = await executeCobolProgram('CREDIT01', cobolData);
      
      // Convert COBOL output to JSON
      const jsonResult = CobolDataConverter.fromCreditOutput(result);
      
      logger.info('Credit calculation completed', { customerId, result: jsonResult });
      
      res.json({
        success: true,
        data: jsonResult,
        timestamp: new Date().toISOString()
      });
    } catch (error) {
      logger.error('Credit calculation error', error);
      res.status(500).json({
        success: false,
        error: 'Credit calculation failed',
        message: error.message
      });
    }
  }
);

// Payroll calculation endpoint
router.post('/payroll/calculate',
  [
    body('employeeId').isString().notEmpty(),
    body('hoursWorked').isNumeric().isFloat({ min: 0, max: 168 }),
    body('hourlyRate').isNumeric().isFloat({ min: 0 }),
    body('deductions').isObject()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { employeeId, hoursWorked, hourlyRate, deductions } = req.body;
      
      // Convert JSON to COBOL format
      const cobolData = CobolDataConverter.toPayrollInput({
        employeeId,
        hoursWorked,
        hourlyRate,
        deductions
      });

      // Execute COBOL program
      const result = await executeCobolProgram('PAYROLL01', cobolData);
      
      // Convert COBOL output to JSON
      const jsonResult = CobolDataConverter.fromPayrollOutput(result);
      
      logger.info('Payroll calculation completed', { employeeId, result: jsonResult });
      
      res.json({
        success: true,
        data: jsonResult,
        timestamp: new Date().toISOString()
      });
    } catch (error) {
      logger.error('Payroll calculation error', error);
      res.status(500).json({
        success: false,
        error: 'Payroll calculation failed',
        message: error.message
      });
    }
  }
);

// Interest calculation endpoint with caching
router.post('/interest/calculate',
  cacheMiddleware(300), // Cache for 5 minutes
  [
    body('principal').isNumeric().isFloat({ min: 0 }),
    body('rate').isNumeric().isFloat({ min: 0, max: 100 }),
    body('term').isNumeric().isInt({ min: 1 })
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { principal, rate, term } = req.body;
      
      // Convert JSON to COBOL format
      const cobolData = CobolDataConverter.toInterestInput({
        principal,
        rate,
        term
      });

      // Execute COBOL program
      const result = await executeCobolProgram('INTEREST01', cobolData);
      
      // Convert COBOL output to JSON
      const jsonResult = CobolDataConverter.fromInterestOutput(result);
      
      logger.info('Interest calculation completed', { principal, rate, term, result: jsonResult });
      
      res.json({
        success: true,
        data: jsonResult,
        timestamp: new Date().toISOString()
      });
    } catch (error) {
      logger.error('Interest calculation error', error);
      res.status(500).json({
        success: false,
        error: 'Interest calculation failed',
        message: error.message
      });
    }
  }
);

// Batch processing endpoint
router.post('/batch/process',
  [
    body('jobType').isString().isIn(['DAILY_INTEREST', 'PAYROLL_RUN', 'CREDIT_REVIEW']),
    body('parameters').isObject()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { jobType, parameters } = req.body;
      const jobId = require('uuid').v4();
      
      // Queue batch job for async processing
      await req.app.locals.jobQueue.add({
        jobId,
        jobType,
        parameters,
        userId: req.user.id
      });
      
      logger.info('Batch job queued', { jobId, jobType });
      
      res.status(202).json({
        success: true,
        jobId,
        status: 'queued',
        message: 'Batch job queued for processing',
        statusUrl: `/api/v1/cobol/batch/status/${jobId}`
      });
    } catch (error) {
      logger.error('Batch processing error', error);
      res.status(500).json({
        success: false,
        error: 'Failed to queue batch job',
        message: error.message
      });
    }
  }
);

// Batch job status endpoint
router.get('/batch/status/:jobId',
  async (req, res) => {
    try {
      const { jobId } = req.params;
      const jobStatus = await req.app.locals.jobQueue.getJobStatus(jobId);
      
      if (!jobStatus) {
        return res.status(404).json({
          success: false,
          error: 'Job not found'
        });
      }
      
      res.json({
        success: true,
        data: jobStatus
      });
    } catch (error) {
      logger.error('Job status error', error);
      res.status(500).json({
        success: false,
        error: 'Failed to retrieve job status',
        message: error.message
      });
    }
  }
);

// Generic COBOL program execution endpoint (for custom programs)
router.post('/execute/:programName',
  [
    body('input').isObject()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { programName } = req.params;
      const { input } = req.body;
      
      // Validate program name against whitelist
      const allowedPrograms = process.env.ALLOWED_COBOL_PROGRAMS?.split(',') || [];
      if (!allowedPrograms.includes(programName)) {
        return res.status(403).json({
          success: false,
          error: 'Program not allowed'
        });
      }
      
      // Execute COBOL program
      const result = await executeCobolProgram(programName, input);
      
      logger.info('COBOL program executed', { programName, userId: req.user.id });
      
      res.json({
        success: true,
        data: result,
        timestamp: new Date().toISOString()
      });
    } catch (error) {
      logger.error('COBOL execution error', error);
      res.status(500).json({
        success: false,
        error: 'Program execution failed',
        message: error.message
      });
    }
  }
);

module.exports = router;