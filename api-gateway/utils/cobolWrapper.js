const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs').promises;
const logger = require('./logger');
const { v4: uuidv4 } = require('uuid');

// COBOL program execution wrapper
const executeCobolProgram = async (programName, inputData) => {
  const executionId = uuidv4();
  const startTime = Date.now();
  
  try {
    logger.info('COBOL execution started', { executionId, programName });
    
    // Validate program exists
    const programPath = path.join(process.env.COBOL_BIN_PATH || '../cobol/bin', programName);
    try {
      await fs.access(programPath);
    } catch (error) {
      throw new Error(`COBOL program not found: ${programName}`);
    }
    
    // Create temporary input file
    const tempInputPath = path.join(process.env.TEMP_PATH || '/tmp', `${executionId}.dat`);
    await fs.writeFile(tempInputPath, inputData);
    
    // Execute COBOL program
    const result = await new Promise((resolve, reject) => {
      const cobolProcess = spawn(programPath, [tempInputPath], {
        env: {
          ...process.env,
          COBOL_INPUT: tempInputPath,
          COBOL_OUTPUT: `${tempInputPath}.out`
        },
        timeout: parseInt(process.env.COBOL_TIMEOUT || '30000')
      });
      
      let stdout = '';
      let stderr = '';
      
      cobolProcess.stdout.on('data', (data) => {
        stdout += data.toString();
      });
      
      cobolProcess.stderr.on('data', (data) => {
        stderr += data.toString();
      });
      
      cobolProcess.on('exit', async (code) => {
        if (code !== 0) {
          reject(new Error(`COBOL program exited with code ${code}: ${stderr}`));
          return;
        }
        
        try {
          // Read output file
          const outputPath = `${tempInputPath}.out`;
          const output = await fs.readFile(outputPath, 'utf8');
          
          // Clean up temp files
          await Promise.all([
            fs.unlink(tempInputPath).catch(() => {}),
            fs.unlink(outputPath).catch(() => {})
          ]);
          
          resolve(output);
        } catch (error) {
          reject(error);
        }
      });
      
      cobolProcess.on('error', (error) => {
        reject(error);
      });
    });
    
    const executionTime = Date.now() - startTime;
    logger.info('COBOL execution completed', { executionId, programName, executionTime });
    
    return result;
  } catch (error) {
    const executionTime = Date.now() - startTime;
    logger.error('COBOL execution failed', { executionId, programName, executionTime, error: error.message });
    throw error;
  }
};

// Check COBOL runtime status
const checkCobolRuntime = async () => {
  try {
    const cobolPath = process.env.COBOL_RUNTIME_PATH || '/usr/bin/cobc';
    await fs.access(cobolPath);
    
    // Try to get version
    const version = await new Promise((resolve, reject) => {
      const versionProcess = spawn(cobolPath, ['--version'], {
        timeout: 5000
      });
      
      let output = '';
      versionProcess.stdout.on('data', (data) => {
        output += data.toString();
      });
      
      versionProcess.on('exit', (code) => {
        if (code === 0) {
          resolve(output.trim());
        } else {
          reject(new Error('Failed to get COBOL version'));
        }
      });
      
      versionProcess.on('error', reject);
    });
    
    return {
      status: 'available',
      runtime: cobolPath,
      version: version
    };
  } catch (error) {
    return {
      status: 'unavailable',
      error: error.message
    };
  }
};

// COBOL data format converter
class CobolDataConverter {
  // Convert JSON to COBOL fixed-format data
  static toCreditInput(data) {
    const { customerId, creditAmount, income, existingDebt } = data;
    
    // COBOL expects fixed-width fields
    const formatField = (value, width, type = 'string') => {
      const str = String(value);
      if (type === 'number') {
        return str.padStart(width, '0');
      }
      return str.padEnd(width, ' ');
    };
    
    // Format according to CREDIT01.cbl input structure
    const formatted = 
      formatField(customerId, 10) +
      formatField(Math.round(creditAmount * 100), 12, 'number') +
      formatField(Math.round(income * 100), 12, 'number') +
      formatField(Math.round(existingDebt * 100), 12, 'number');
    
    return formatted;
  }
  
  // Convert COBOL output to JSON
  static fromCreditOutput(data) {
    // Parse fixed-width COBOL output
    const approved = data.substring(0, 1) === 'Y';
    const creditLimit = parseInt(data.substring(1, 13)) / 100;
    const interestRate = parseInt(data.substring(13, 18)) / 100;
    const monthlyPayment = parseInt(data.substring(18, 30)) / 100;
    const riskScore = parseInt(data.substring(30, 33));
    
    return {
      approved,
      creditLimit,
      interestRate,
      monthlyPayment,
      riskScore
    };
  }
  
  // Convert JSON to COBOL payroll input
  static toPayrollInput(data) {
    const { employeeId, hoursWorked, hourlyRate, deductions } = data;
    
    const formatField = (value, width, type = 'string') => {
      const str = String(value);
      if (type === 'number') {
        return str.padStart(width, '0');
      }
      return str.padEnd(width, ' ');
    };
    
    // Format according to PAYROLL01.cbl input structure
    const formatted = 
      formatField(employeeId, 10) +
      formatField(Math.round(hoursWorked * 100), 6, 'number') +
      formatField(Math.round(hourlyRate * 100), 8, 'number') +
      formatField(Math.round((deductions.federal || 0) * 100), 8, 'number') +
      formatField(Math.round((deductions.state || 0) * 100), 8, 'number') +
      formatField(Math.round((deductions.insurance || 0) * 100), 8, 'number');
    
    return formatted;
  }
  
  // Convert COBOL payroll output to JSON
  static fromPayrollOutput(data) {
    const grossPay = parseInt(data.substring(0, 10)) / 100;
    const federalTax = parseInt(data.substring(10, 20)) / 100;
    const stateTax = parseInt(data.substring(20, 30)) / 100;
    const insurance = parseInt(data.substring(30, 40)) / 100;
    const netPay = parseInt(data.substring(40, 50)) / 100;
    
    return {
      grossPay,
      deductions: {
        federal: federalTax,
        state: stateTax,
        insurance
      },
      netPay
    };
  }
  
  // Convert JSON to COBOL interest input
  static toInterestInput(data) {
    const { principal, rate, term } = data;
    
    const formatField = (value, width, type = 'string') => {
      const str = String(value);
      if (type === 'number') {
        return str.padStart(width, '0');
      }
      return str.padEnd(width, ' ');
    };
    
    const formatted = 
      formatField(Math.round(principal * 100), 12, 'number') +
      formatField(Math.round(rate * 100), 6, 'number') +
      formatField(term, 4, 'number');
    
    return formatted;
  }
  
  // Convert COBOL interest output to JSON
  static fromInterestOutput(data) {
    const totalInterest = parseInt(data.substring(0, 12)) / 100;
    const totalAmount = parseInt(data.substring(12, 24)) / 100;
    const monthlyPayment = parseInt(data.substring(24, 36)) / 100;
    
    return {
      totalInterest,
      totalAmount,
      monthlyPayment
    };
  }
  
  // Generic converter for custom programs
  static toCobolFormat(data, schema) {
    let result = '';
    
    for (const field of schema) {
      const value = data[field.name];
      const formatted = this.formatField(value, field.width, field.type);
      result += formatted;
    }
    
    return result;
  }
  
  static fromCobolFormat(data, schema) {
    const result = {};
    let position = 0;
    
    for (const field of schema) {
      const rawValue = data.substring(position, position + field.width);
      result[field.name] = this.parseField(rawValue, field.type);
      position += field.width;
    }
    
    return result;
  }
  
  static formatField(value, width, type = 'string') {
    const str = String(value);
    if (type === 'number' || type === 'decimal') {
      return str.padStart(width, '0');
    }
    return str.padEnd(width, ' ');
  }
  
  static parseField(value, type) {
    switch (type) {
      case 'number':
        return parseInt(value);
      case 'decimal':
        return parseInt(value) / 100;
      case 'boolean':
        return value.trim() === 'Y';
      default:
        return value.trim();
    }
  }
}

module.exports = {
  executeCobolProgram,
  checkCobolRuntime,
  CobolDataConverter
};