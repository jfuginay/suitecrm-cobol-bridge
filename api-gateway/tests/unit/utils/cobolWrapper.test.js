const { spawn } = require('child_process');
const fs = require('fs').promises;
const path = require('path');
const os = require('os');

const {
  executeCobolProgram,
  checkCobolRuntime,
  compileCobolProgram,
  listCobolPrograms,
  validateCobolData,
  CobolDataConverter,
  MEMORY_LIMITS,
  SECURITY_CONFIG,
  DEBUG_CONFIG
} = require('../../../utils/cobolWrapper');

// Mock dependencies
jest.mock('child_process');
jest.mock('fs', () => ({
  promises: {
    access: jest.fn(),
    stat: jest.fn(),
    mkdir: jest.fn(),
    writeFile: jest.fn(),
    readFile: jest.fn(),
    readdir: jest.fn(),
    rm: jest.fn(),
    watch: jest.fn()
  }
}));
jest.mock('../../../utils/logger', () => ({
  info: jest.fn(),
  warn: jest.fn(),
  error: jest.fn(),
  debug: jest.fn()
}));
jest.mock('../../../utils/websocket', () => ({
  broadcast: jest.fn()
}));
jest.mock('../../../utils/metrics', () => ({
  recordCobolExecution: jest.fn()
}));

describe('COBOL Wrapper', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    
    // Set default environment variables
    process.env.COBOL_BIN_PATH = '/test/compiled';
    process.env.COBOL_TEMP_PATH = '/test/temp';
    process.env.COBOL_RUNTIME_PATH = '/usr/bin/cobc';
    process.env.COBOL_ALLOWED_PROGRAMS = 'CREDIT01,PAYROLL01,INTEREST01';
  });

  afterEach(() => {
    // Clean up environment variables
    delete process.env.COBOL_BIN_PATH;
    delete process.env.COBOL_TEMP_PATH;
    delete process.env.COBOL_RUNTIME_PATH;
    delete process.env.COBOL_ALLOWED_PROGRAMS;
  });

  describe('executeCobolProgram', () => {
    it('should execute COBOL program successfully', async () => {
      const mockProcess = {
        pid: 1234,
        stdout: { on: jest.fn() },
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      spawn.mockReturnValue(mockProcess);
      fs.stat.mockResolvedValue({
        isFile: () => true,
        size: 1024,
        mtime: new Date(),
        mode: 0o755
      });
      fs.mkdir.mockResolvedValue();
      fs.writeFile.mockResolvedValue();
      fs.readFile.mockResolvedValue('output data');
      fs.watch.mockReturnValue({ close: jest.fn() });

      // Simulate successful process execution
      setTimeout(() => {
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(0, null);
      }, 0);

      const result = await executeCobolProgram('CREDIT01', 'input data');

      expect(result).toHaveProperty('output', 'output data');
      expect(result).toHaveProperty('executionId');
      expect(result).toHaveProperty('programName', 'CREDIT01');
      expect(result).toHaveProperty('exitCode', 0);
      expect(spawn).toHaveBeenCalled();
    });

    it('should reject execution of non-allowed programs', async () => {
      await expect(executeCobolProgram('MALICIOUS_PROG', 'input'))
        .rejects.toThrow('Program MALICIOUS_PROG is not in allowed programs list');
    });

    it('should reject programs with invalid names', async () => {
      await expect(executeCobolProgram('PROG; rm -rf /', 'input'))
        .rejects.toThrow('Invalid program name format');
    });

    it('should handle program execution failure', async () => {
      const mockProcess = {
        pid: 1234,
        stdout: { on: jest.fn() },
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      spawn.mockReturnValue(mockProcess);
      fs.stat.mockResolvedValue({
        isFile: () => true,
        size: 1024,
        mtime: new Date(),
        mode: 0o755
      });
      fs.mkdir.mockResolvedValue();
      fs.writeFile.mockResolvedValue();
      fs.watch.mockReturnValue({ close: jest.fn() });

      // Simulate process failure
      setTimeout(() => {
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(1, null);
      }, 0);

      await expect(executeCobolProgram('CREDIT01', 'input data'))
        .rejects.toThrow('COBOL program exited with code 1');
    });

    it('should handle non-existent program', async () => {
      fs.stat.mockRejectedValue({ code: 'ENOENT' });

      await expect(executeCobolProgram('NONEXISTENT', 'input'))
        .rejects.toThrow('COBOL program not found: NONEXISTENT');
    });

    it('should enforce file size limits', async () => {
      fs.stat.mockResolvedValue({
        isFile: () => true,
        size: 200 * 1024 * 1024, // 200MB - too large
        mtime: new Date(),
        mode: 0o755
      });

      await expect(executeCobolProgram('CREDIT01', 'input'))
        .rejects.toThrow('Program file too large');
    });

    it('should create secure temporary directory', async () => {
      const mockProcess = {
        pid: 1234,
        stdout: { on: jest.fn() },
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      spawn.mockReturnValue(mockProcess);
      fs.stat.mockResolvedValue({
        isFile: () => true,
        size: 1024,
        mtime: new Date(),
        mode: 0o755
      });
      fs.mkdir.mockResolvedValue();
      fs.writeFile.mockResolvedValue();
      fs.readFile.mockResolvedValue('output');
      fs.watch.mockReturnValue({ close: jest.fn() });

      setTimeout(() => {
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(0, null);
      }, 0);

      await executeCobolProgram('CREDIT01', 'input');

      expect(fs.mkdir).toHaveBeenCalledWith(
        expect.stringMatching(/\/test\/temp\/cobol-exec\//),
        { recursive: true, mode: 0o700 }
      );
    });

    it('should enforce input file size limits', async () => {
      const largeInput = 'x'.repeat(200 * 1024 * 1024); // 200MB

      fs.stat.mockResolvedValue({
        isFile: () => true,
        size: 1024,
        mtime: new Date(),
        mode: 0o755
      });
      fs.mkdir.mockResolvedValue();

      await expect(executeCobolProgram('CREDIT01', largeInput))
        .rejects.toThrow('Input file too large');
    });
  });

  describe('checkCobolRuntime', () => {
    it('should return runtime status when available', async () => {
      const mockProcess = {
        stdout: { on: jest.fn() },
        on: jest.fn()
      };

      fs.access.mockResolvedValue();
      spawn.mockReturnValue(mockProcess);

      // Simulate version output
      setTimeout(() => {
        const dataCallback = mockProcess.stdout.on.mock.calls.find(call => call[0] === 'data')[1];
        dataCallback('GnuCOBOL 3.1.2');
        
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(0);
      }, 0);

      const result = await checkCobolRuntime();

      expect(result).toEqual({
        status: 'available',
        runtime: '/usr/bin/cobc',
        version: 'GnuCOBOL 3.1.2'
      });
    });

    it('should return unavailable when runtime not found', async () => {
      fs.access.mockRejectedValue(new Error('File not found'));

      const result = await checkCobolRuntime();

      expect(result).toEqual({
        status: 'unavailable',
        error: 'File not found'
      });
    });

    it('should handle version check failure', async () => {
      const mockProcess = {
        stdout: { on: jest.fn() },
        on: jest.fn()
      };

      fs.access.mockResolvedValue();
      spawn.mockReturnValue(mockProcess);

      // Simulate version command failure
      setTimeout(() => {
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(1);
      }, 0);

      const result = await checkCobolRuntime();

      expect(result).toEqual({
        status: 'unavailable',
        error: 'Failed to get COBOL version'
      });
    });
  });

  describe('compileCobolProgram', () => {
    it('should compile COBOL program successfully', async () => {
      const mockProcess = {
        stdout: { on: jest.fn() },
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      spawn.mockReturnValue(mockProcess);

      // Simulate successful compilation
      setTimeout(() => {
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(0);
      }, 0);

      const result = await compileCobolProgram('/source/test.cob');

      expect(result).toHaveProperty('success', true);
      expect(result).toHaveProperty('sourcePath', '/source/test.cob');
      expect(result).toHaveProperty('outputPath');
      expect(result).toHaveProperty('compilationTime');
      expect(spawn).toHaveBeenCalledWith(
        'cobc',
        expect.arrayContaining(['-x', '-O2', '-Wall', '-fmax-errors=10']),
        expect.any(Object)
      );
    });

    it('should handle compilation failure', async () => {
      const mockProcess = {
        stdout: { on: jest.fn() },
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      spawn.mockReturnValue(mockProcess);

      // Simulate compilation failure
      setTimeout(() => {
        const stderrCallback = mockProcess.stderr.on.mock.calls.find(call => call[0] === 'data')[1];
        stderrCallback('Syntax error on line 10');
        
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(1);
      }, 0);

      await expect(compileCobolProgram('/source/test.cob'))
        .rejects.toThrow('Compilation failed: Syntax error on line 10');
    });

    it('should add debug flags when debug option is enabled', async () => {
      const mockProcess = {
        stdout: { on: jest.fn() },
        stderr: { on: jest.fn() },
        on: jest.fn()
      };

      spawn.mockReturnValue(mockProcess);

      setTimeout(() => {
        const exitCallback = mockProcess.on.mock.calls.find(call => call[0] === 'exit')[1];
        exitCallback(0);
      }, 0);

      await compileCobolProgram('/source/test.cob', { debug: true });

      expect(spawn).toHaveBeenCalledWith(
        'cobc',
        expect.arrayContaining(['-g', '-ftraceall', '-fsource-location']),
        expect.any(Object)
      );
    });
  });

  describe('listCobolPrograms', () => {
    it('should list executable COBOL programs', async () => {
      fs.readdir.mockResolvedValue(['prog1', 'prog2', 'README.txt']);
      fs.stat
        .mockResolvedValueOnce({
          isFile: () => true,
          size: 1024,
          mtime: new Date('2023-01-01'),
          mode: 0o755 // executable
        })
        .mockResolvedValueOnce({
          isFile: () => true,
          size: 2048,
          mtime: new Date('2023-01-02'),
          mode: 0o755 // executable
        })
        .mockResolvedValueOnce({
          isFile: () => true,
          size: 512,
          mtime: new Date('2023-01-03'),
          mode: 0o644 // not executable
        });

      const result = await listCobolPrograms();

      expect(result).toHaveLength(2);
      expect(result[0]).toEqual({
        name: 'prog1',
        size: 1024,
        modified: new Date('2023-01-01'),
        cloudBurstEligible: true
      });
      expect(result[1]).toEqual({
        name: 'prog2',
        size: 2048,
        modified: new Date('2023-01-02'),
        cloudBurstEligible: true
      });
    });

    it('should handle directory read errors', async () => {
      fs.readdir.mockRejectedValue(new Error('Permission denied'));

      const result = await listCobolPrograms();

      expect(result).toEqual([]);
    });
  });

  describe('validateCobolData', () => {
    const testSchema = [
      { name: 'id', width: 10, type: 'alphanumeric', required: true },
      { name: 'amount', width: 8, type: 'number', required: true },
      { name: 'name', width: 20, type: 'alpha', required: false }
    ];

    it('should validate correct data format', () => {
      const data = 'CUST001   00100000JOHN SMITH          ';
      const errors = validateCobolData(data, testSchema);
      expect(errors).toHaveLength(0);
    });

    it('should detect invalid numeric fields', () => {
      const data = 'CUST001   INVALID1JOHN SMITH          ';
      const errors = validateCobolData(data, testSchema);
      expect(errors).toContain('Field amount must be numeric');
    });

    it('should detect invalid alphabetic fields', () => {
      const data = 'CUST001   00100000JOHN SMITH123       ';
      const errors = validateCobolData(data, testSchema);
      expect(errors).toContain('Field name must be alphabetic');
    });

    it('should detect short required fields', () => {
      const shortData = 'CUST001';
      const errors = validateCobolData(shortData, testSchema);
      expect(errors).toContain('Field amount is too short');
    });
  });

  describe('CobolDataConverter', () => {
    describe('Credit Input/Output Conversion', () => {
      it('should convert credit input data correctly', () => {
        const inputData = {
          customerId: 'CUST001',
          creditAmount: 50000.00,
          income: 75000.50,
          existingDebt: 10000.25
        };

        const result = CobolDataConverter.toCreditInput(inputData);

        expect(result).toMatch(/^CUST001\s{3}000005000000000007500050000001000025$/);
      });

      it('should convert credit output data correctly', () => {
        const outputData = 'Y000045000000000000005250000008500000000003500';
        
        const result = CobolDataConverter.fromCreditOutput(outputData);

        expect(result).toEqual({
          approved: true,
          creditLimit: 450000.00,
          interestRate: 5.25,
          monthlyPayment: 85000.00,
          riskScore: 350
        });
      });
    });

    describe('Payroll Input/Output Conversion', () => {
      it('should convert payroll input data correctly', () => {
        const inputData = {
          employeeId: 'EMP001',
          hoursWorked: 40.5,
          hourlyRate: 25.75,
          deductions: {
            federal: 150.00,
            state: 50.00,
            insurance: 75.25
          }
        };

        const result = CobolDataConverter.toPayrollInput(inputData);

        expect(result).toMatch(/^EMP001\s{4}004050002575000015000000005000000007525$/);
      });

      it('should convert payroll output data correctly', () => {
        const outputData = '0001042875000026457500000525000000752500000268525';
        
        const result = CobolDataConverter.fromPayrollOutput(outputData);

        expect(result).toEqual({
          grossPay: 10428.75,
          deductions: {
            federal: 264.58,
            state: 5.25,
            insurance: 75.25
          },
          netPay: 2685.25
        });
      });
    });

    describe('Interest Calculation Conversion', () => {
      it('should convert interest input data correctly', () => {
        const inputData = {
          principal: 100000.00,
          rate: 5.5,
          term: 30
        };

        const result = CobolDataConverter.toInterestInput(inputData);

        expect(result).toMatch(/^000010000000000055030$/);
      });

      it('should convert interest output data correctly', () => {
        const outputData = '000019758200000029758200000056789';
        
        const result = CobolDataConverter.fromInterestOutput(outputData);

        expect(result).toEqual({
          totalInterest: 197582.00,
          totalAmount: 297582.00,
          monthlyPayment: 567.89
        });
      });
    });

    describe('Advanced Field Formatting', () => {
      it('should handle signed numeric fields', () => {
        const field = { name: 'amount', picture: 'S9(8)V99', width: 10 };
        
        const positive = CobolDataConverter.formatFieldAdvanced(1234.56, field);
        expect(positive).toMatch(/.*\{$/); // Positive sign
        
        const negative = CobolDataConverter.formatFieldAdvanced(-1234.56, field);
        expect(negative).toMatch(/.*\}$/); // Negative sign
      });

      it('should parse signed numeric fields', () => {
        const field = { name: 'amount', picture: 'S9(8)V99' };
        
        const positive = CobolDataConverter.parseFieldAdvanced('000012345{', field);
        expect(positive).toBeCloseTo(1234.5);
        
        const negative = CobolDataConverter.parseFieldAdvanced('000012345}', field);
        expect(negative).toBeCloseTo(-1234.5);
      });

      it('should handle COMP-3 packed decimal fields', () => {
        const field = { name: 'amount', picture: '9(8)V99 COMP-3', width: 5 };
        
        const result = CobolDataConverter.formatFieldAdvanced(1234.56, field);
        expect(result).toHaveLength(5);
        
        const parsed = CobolDataConverter.parseFieldAdvanced(result, field);
        expect(parsed).toBeCloseTo(1234.56);
      });

      it('should handle boolean fields', () => {
        const field = { name: 'flag', picture: 'X', width: 1, boolean: true };
        
        expect(CobolDataConverter.parseFieldAdvanced('Y', field)).toBe(true);
        expect(CobolDataConverter.parseFieldAdvanced('N', field)).toBe(false);
        expect(CobolDataConverter.parseFieldAdvanced('1', field)).toBe(true);
        expect(CobolDataConverter.parseFieldAdvanced('0', field)).toBe(false);
      });

      it('should get correct field lengths from picture clauses', () => {
        expect(CobolDataConverter.getFieldLength({ picture: '9(10)' })).toBe(10);
        expect(CobolDataConverter.getFieldLength({ picture: 'X(25)' })).toBe(25);
        expect(CobolDataConverter.getFieldLength({ picture: '9999V99' })).toBe(6);
        expect(CobolDataConverter.getFieldLength({ picture: '9(8) COMP-3' })).toBe(5);
        expect(CobolDataConverter.getFieldLength({ picture: '9(4) COMP' })).toBe(2);
      });
    });

    describe('Data Schema Validation', () => {
      it('should validate complex data schemas', () => {
        const schema = [
          { name: 'recordType', picture: 'X(2)', width: 2 },
          { name: 'customerId', picture: 'X(10)', width: 10 },
          { name: 'amount', picture: 'S9(8)V99', width: 10 },
          { name: 'approved', picture: 'X', width: 1, boolean: true }
        ];

        const data = {
          recordType: 'CR',
          customerId: 'CUST001',
          amount: -1500.75,
          approved: true
        };

        const cobolData = CobolDataConverter.toCobolData(data, schema);
        const parsedData = CobolDataConverter.fromCobolData(cobolData, schema);

        expect(parsedData.recordType).toBe('CR');
        expect(parsedData.customerId).toBe('CUST001');
        expect(parsedData.amount).toBeCloseTo(-1500.75);
        expect(parsedData.approved).toBe(true);
      });

      it('should handle missing optional fields', () => {
        const schema = [
          { name: 'required', picture: 'X(5)', width: 5 },
          { name: 'optional', picture: 'X(10)', width: 10, defaultValue: 'DEFAULT' }
        ];

        const data = { required: 'TEST' };
        const cobolData = CobolDataConverter.toCobolData(data, schema);
        const parsedData = CobolDataConverter.fromCobolData(cobolData, schema);

        expect(parsedData.required).toBe('TEST');
        expect(parsedData.optional).toBe('DEFAULT');
      });
    });
  });

  describe('Configuration Constants', () => {
    it('should export memory limits configuration', () => {
      expect(MEMORY_LIMITS).toHaveProperty('maxHeapSize');
      expect(MEMORY_LIMITS).toHaveProperty('maxStackSize');
      expect(MEMORY_LIMITS).toHaveProperty('maxFileSize');
    });

    it('should export security configuration', () => {
      expect(SECURITY_CONFIG).toHaveProperty('allowedPrograms');
      expect(SECURITY_CONFIG).toHaveProperty('sandboxEnabled');
      expect(SECURITY_CONFIG).toHaveProperty('maxExecutionTime');
    });

    it('should export debug configuration', () => {
      expect(DEBUG_CONFIG).toHaveProperty('enabled');
      expect(DEBUG_CONFIG).toHaveProperty('verbosity');
      expect(DEBUG_CONFIG).toHaveProperty('captureVariables');
    });
  });
});