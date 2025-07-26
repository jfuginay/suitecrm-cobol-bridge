const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs').promises;
const logger = require('./logger');
const { v4: uuidv4 } = require('uuid');
const os = require('os');
const crypto = require('crypto');
const metrics = require('./metrics');
const websocket = require('./websocket');

// Memory management configuration
const MEMORY_LIMITS = {
  maxHeapSize: process.env.COBOL_MAX_HEAP || '512M',
  maxStackSize: process.env.COBOL_MAX_STACK || '64M',
  maxFileSize: process.env.COBOL_MAX_FILE_SIZE || 104857600 // 100MB
};

// Security configuration
const SECURITY_CONFIG = {
  allowedPrograms: process.env.COBOL_ALLOWED_PROGRAMS?.split(',') || [],
  sandboxEnabled: process.env.COBOL_SANDBOX_ENABLED !== 'false',
  maxExecutionTime: parseInt(process.env.COBOL_MAX_EXECUTION_TIME || '300000'), // 5 minutes
  allowedFileOperations: ['read', 'write', 'temp'],
  blockedSystemCalls: ['exec', 'fork', 'system']
};

// Debug trace configuration
const DEBUG_CONFIG = {
  enabled: process.env.COBOL_DEBUG_ENABLED === 'true',
  verbosity: process.env.COBOL_DEBUG_VERBOSITY || 'info',
  captureVariables: process.env.COBOL_CAPTURE_VARIABLES === 'true',
  maxTraceSize: parseInt(process.env.COBOL_MAX_TRACE_SIZE || '10485760') // 10MB
};

// COBOL program execution wrapper with enhanced features
const executeCobolProgram = async (programName, inputData, options = {}) => {
  const executionId = uuidv4();
  const startTime = Date.now();
  const executionContext = {
    id: executionId,
    program: programName,
    startTime,
    userId: options.userId,
    sessionId: options.sessionId,
    priority: options.priority || 'normal',
    cloudBurstEligible: false,
    debugTrace: [],
    metrics: {
      memoryUsage: {},
      cpuUsage: {},
      ioOperations: 0
    }
  };
  
  try {
    logger.info('COBOL execution started', { executionId, programName });
    
    // Send real-time monitoring event
    websocket.broadcast('cobol:execution:start', {
      executionId,
      programName,
      timestamp: new Date().toISOString()
    });
    
    // Security validation
    await validateProgramSecurity(programName, executionContext);
    
    // Validate program exists and is compiled
    const programPath = await getProgramPath(programName);
    const programInfo = await validateProgram(programPath, executionContext);
    
    // Check cloud burst eligibility
    executionContext.cloudBurstEligible = await checkCloudBurstEligibility(programInfo, inputData);
    
    // Create secure temporary directory
    const tempDir = await createSecureTempDir(executionId);
    const tempInputPath = path.join(tempDir, 'input.dat');
    const tempOutputPath = path.join(tempDir, 'output.dat');
    const monitorPath = path.join(tempDir, 'monitor.log');
    const debugTracePath = path.join(tempDir, 'debug-trace.log');
    
    // Write input data with validation
    await writeSecureFile(tempInputPath, inputData, executionContext);
    
    // Initialize monitoring files
    await fs.writeFile(monitorPath, '');
    if (DEBUG_CONFIG.enabled) {
      await fs.writeFile(debugTracePath, '');
    }
    
    // Prepare sandboxed execution environment
    const sandboxEnv = createSandboxEnvironment(executionContext, {
      inputPath: tempInputPath,
      outputPath: tempOutputPath,
      monitorPath,
      debugTracePath
    });
    
    // Execute COBOL program with enhanced monitoring
    const result = await new Promise((resolve, reject) => {
      const cobolProcess = spawn(programPath, [], {
        env: sandboxEnv,
        cwd: tempDir,
        timeout: SECURITY_CONFIG.maxExecutionTime,
        // Security restrictions
        uid: process.env.COBOL_RUN_UID ? parseInt(process.env.COBOL_RUN_UID) : undefined,
        gid: process.env.COBOL_RUN_GID ? parseInt(process.env.COBOL_RUN_GID) : undefined,
        // Resource limits
        maxBuffer: MEMORY_LIMITS.maxFileSize
      });
      
      // Monitor process metrics
      const metricsInterval = setInterval(() => {
        collectProcessMetrics(cobolProcess.pid, executionContext);
      }, 1000);
      
      let stdout = '';
      let stderr = '';
      let monitorData = '';
      let debugTraceData = '';
      
      // Real-time output streaming
      cobolProcess.stdout.on('data', (data) => {
        const chunk = data.toString();
        stdout += chunk;
        
        // Stream to monitoring dashboard
        websocket.broadcast('cobol:output:stream', {
          executionId,
          type: 'stdout',
          data: chunk,
          timestamp: new Date().toISOString()
        });
      });
      
      cobolProcess.stderr.on('data', (data) => {
        const chunk = data.toString();
        stderr += chunk;
        
        // Stream errors to monitoring
        websocket.broadcast('cobol:output:stream', {
          executionId,
          type: 'stderr',
          data: chunk,
          timestamp: new Date().toISOString()
        });
      });
      
      // Monitor file watchers for real-time updates
      const monitorWatcher = fs.watch(monitorPath, async (eventType) => {
        if (eventType === 'change') {
          const newData = await fs.readFile(monitorPath, 'utf8');
          const updates = newData.slice(monitorData.length);
          monitorData = newData;
          
          // Parse and broadcast monitoring events
          parseMonitoringEvents(updates, executionContext);
        }
      });
      
      if (DEBUG_CONFIG.enabled) {
        const debugWatcher = fs.watch(debugTracePath, async (eventType) => {
          if (eventType === 'change') {
            const newData = await fs.readFile(debugTracePath, 'utf8');
            const updates = newData.slice(debugTraceData.length);
            debugTraceData = newData;
            
            // Parse and store debug trace
            parseDebugTrace(updates, executionContext);
          }
        });
      }
      
      cobolProcess.on('exit', async (code, signal) => {
        clearInterval(metricsInterval);
        monitorWatcher.close();
        if (DEBUG_CONFIG.enabled && debugWatcher) {
          debugWatcher.close();
        }
        
        const executionTime = Date.now() - startTime;
        
        // Record execution metrics
        metrics.recordCobolExecution(programName, executionTime / 1000, code === 0);
        
        if (code !== 0) {
          const error = new Error(`COBOL program exited with code ${code}${signal ? ` (signal: ${signal})` : ''}: ${stderr}`);
          
          // Send failure notification
          websocket.broadcast('cobol:execution:failed', {
            executionId,
            programName,
            error: error.message,
            executionTime,
            timestamp: new Date().toISOString()
          });
          
          reject(error);
          return;
        }
        
        try {
          // Read all output files
          const [output, finalMonitor, finalDebugTrace] = await Promise.all([
            fs.readFile(tempOutputPath, 'utf8'),
            fs.readFile(monitorPath, 'utf8'),
            DEBUG_CONFIG.enabled ? fs.readFile(debugTracePath, 'utf8') : Promise.resolve('')
          ]);
          
          // Parse final monitoring data
          if (finalMonitor.length > monitorData.length) {
            parseMonitoringEvents(finalMonitor.slice(monitorData.length), executionContext);
          }
          
          // Parse final debug trace
          if (DEBUG_CONFIG.enabled && finalDebugTrace.length > debugTraceData.length) {
            parseDebugTrace(finalDebugTrace.slice(debugTraceData.length), executionContext);
          }
          
          // Generate execution report
          const executionReport = {
            output,
            executionId,
            programName,
            executionTime,
            exitCode: code,
            metrics: executionContext.metrics,
            cloudBurstEligible: executionContext.cloudBurstEligible,
            debugTraceAvailable: DEBUG_CONFIG.enabled && executionContext.debugTrace.length > 0,
            warnings: extractWarnings(stderr),
            timestamp: new Date().toISOString()
          };
          
          // Store debug trace if enabled
          if (DEBUG_CONFIG.enabled && executionContext.debugTrace.length > 0) {
            await storeDebugTrace(executionId, executionContext.debugTrace);
          }
          
          // Clean up temp directory
          await cleanupTempDir(tempDir);
          
          // Send success notification
          websocket.broadcast('cobol:execution:complete', {
            executionId,
            programName,
            executionTime,
            timestamp: new Date().toISOString()
          });
          
          resolve(executionReport);
        } catch (error) {
          // Clean up on error
          await cleanupTempDir(tempDir).catch(() => {});
          reject(error);
        }
      });
      
      cobolProcess.on('error', (error) => {
        clearInterval(metricsInterval);
        monitorWatcher.close();
        if (DEBUG_CONFIG.enabled && debugWatcher) {
          debugWatcher.close();
        }
        
        // Clean up on error
        cleanupTempDir(tempDir).catch(() => {});
        
        // Send error notification
        websocket.broadcast('cobol:execution:error', {
          executionId,
          programName,
          error: error.message,
          timestamp: new Date().toISOString()
        });
        
        reject(error);
      });
    });
    
    logger.info('COBOL execution completed', { 
      executionId, 
      programName, 
      executionTime: result.executionTime,
      cloudBurstEligible: result.cloudBurstEligible 
    });
    
    return result;
  } catch (error) {
    const executionTime = Date.now() - startTime;
    logger.error('COBOL execution failed', { 
      executionId, 
      programName, 
      executionTime, 
      error: error.message 
    });
    
    // Record failed execution metric
    metrics.recordCobolExecution(programName, executionTime / 1000, false);
    
    throw error;
  }
};

// Helper functions for enhanced runtime features

// Validate program security
const validateProgramSecurity = async (programName, context) => {
  // Check if program is in allowed list
  if (SECURITY_CONFIG.allowedPrograms.length > 0 && 
      !SECURITY_CONFIG.allowedPrograms.includes(programName)) {
    throw new Error(`Program ${programName} is not in allowed programs list`);
  }
  
  // Validate program name for injection attacks
  if (!/^[a-zA-Z0-9_-]+$/.test(programName)) {
    throw new Error('Invalid program name format');
  }
  
  logger.info('Program security validated', { programName, executionId: context.id });
};

// Get validated program path
const getProgramPath = async (programName) => {
  const binPath = process.env.COBOL_BIN_PATH || '/compiled';
  const programPath = path.join(binPath, programName);
  
  // Ensure path doesn't escape the bin directory
  const resolvedPath = path.resolve(programPath);
  const resolvedBinPath = path.resolve(binPath);
  
  if (!resolvedPath.startsWith(resolvedBinPath)) {
    throw new Error('Invalid program path');
  }
  
  return resolvedPath;
};

// Validate compiled program
const validateProgram = async (programPath, context) => {
  try {
    const stats = await fs.stat(programPath);
    
    // Check file permissions
    if (!stats.isFile()) {
      throw new Error('Program path is not a file');
    }
    
    // Check file size
    if (stats.size > 100 * 1024 * 1024) { // 100MB limit
      throw new Error('Program file too large');
    }
    
    // Get program metadata
    const metadata = {
      size: stats.size,
      modified: stats.mtime,
      executable: (stats.mode & 0o111) !== 0
    };
    
    logger.info('Program validated', { 
      programPath, 
      metadata, 
      executionId: context.id 
    });
    
    return metadata;
  } catch (error) {
    if (error.code === 'ENOENT') {
      throw new Error(`COBOL program not found: ${path.basename(programPath)}`);
    }
    throw error;
  }
};

// Check cloud burst eligibility
const checkCloudBurstEligibility = async (programInfo, inputData) => {
  // Check if input is large enough to benefit from cloud burst
  const inputSize = Buffer.byteLength(inputData, 'utf8');
  const isLargeInput = inputSize > 1024 * 1024; // 1MB
  
  // Check if program supports parallel processing
  // This would typically check program metadata or configuration
  const supportsParallel = true; // Simplified for now
  
  return isLargeInput && supportsParallel;
};

// Create secure temporary directory
const createSecureTempDir = async (executionId) => {
  const tempRoot = process.env.COBOL_TEMP_PATH || os.tmpdir();
  const tempDir = path.join(tempRoot, 'cobol-exec', executionId);
  
  await fs.mkdir(tempDir, { recursive: true, mode: 0o700 });
  
  return tempDir;
};

// Write file with security checks
const writeSecureFile = async (filePath, data, context) => {
  // Check file size
  const size = Buffer.byteLength(data, 'utf8');
  if (size > MEMORY_LIMITS.maxFileSize) {
    throw new Error(`Input file too large: ${size} bytes`);
  }
  
  await fs.writeFile(filePath, data, { mode: 0o600 });
  
  context.metrics.ioOperations++;
};

// Create sandboxed environment
const createSandboxEnvironment = (context, paths) => {
  const env = {
    // COBOL runtime environment
    COB_LIBRARY_PATH: process.env.COB_LIBRARY_PATH || '/usr/lib/gnucobol',
    COB_CONFIG_DIR: process.env.COB_CONFIG_DIR || '/usr/share/gnucobol/config',
    COB_RUNTIME_CONFIG: process.env.COB_RUNTIME_CONFIG || 'default.conf',
    
    // File paths
    COBOL_INPUT: paths.inputPath,
    COBOL_OUTPUT: paths.outputPath,
    MONITOR_FILE: paths.monitorPath,
    DEBUG_TRACE: paths.debugTracePath,
    
    // Execution context
    EXECUTION_ID: context.id,
    PROGRAM_NAME: context.program,
    USER_ID: context.userId || 'anonymous',
    SESSION_ID: context.sessionId || 'none',
    
    // Security settings
    COB_FILE_PATH: paths.tempDir,
    COB_DISABLE_WARNINGS: 'N',
    COB_SET_DEBUG: DEBUG_CONFIG.enabled ? 'Y' : 'N',
    
    // Performance settings
    COB_PHYSICAL_CANCEL: 'Y',
    COB_PRE_LOAD: 'Y',
    
    // Cloud burst markers
    CLOUD_BURST_ELIGIBLE: context.cloudBurstEligible ? 'Y' : 'N',
    PRIORITY: context.priority,
    
    // Remove potentially dangerous environment variables
    LD_PRELOAD: '',
    LD_LIBRARY_PATH: process.env.COB_LIBRARY_PATH || ''
  };
  
  // Add debug settings if enabled
  if (DEBUG_CONFIG.enabled) {
    env.COB_TRACE_FILE = paths.debugTracePath;
    env.COB_TRACE_FORMAT = 'JSON';
    env.COB_DEBUG_LOG = paths.debugTracePath;
  }
  
  return env;
};

// Collect process metrics
const collectProcessMetrics = async (pid, context) => {
  if (!pid) return;
  
  try {
    // Read process stats from /proc (Linux)
    const statPath = `/proc/${pid}/stat`;
    const statusPath = `/proc/${pid}/status`;
    
    if (process.platform === 'linux') {
      const [stat, status] = await Promise.all([
        fs.readFile(statPath, 'utf8').catch(() => ''),
        fs.readFile(statusPath, 'utf8').catch(() => '')
      ]);
      
      // Parse memory usage from status
      const vmRssMatch = status.match(/VmRSS:\s+(\d+)\s+kB/);
      if (vmRssMatch) {
        context.metrics.memoryUsage.rss = parseInt(vmRssMatch[1]) * 1024;
      }
      
      // Parse CPU usage from stat
      const statFields = stat.split(' ');
      if (statFields.length > 14) {
        context.metrics.cpuUsage.user = parseInt(statFields[13]);
        context.metrics.cpuUsage.system = parseInt(statFields[14]);
      }
    }
    
    // Broadcast metrics update
    websocket.broadcast('cobol:metrics:update', {
      executionId: context.id,
      metrics: context.metrics,
      timestamp: new Date().toISOString()
    });
  } catch (error) {
    // Ignore errors in metrics collection
  }
};

// Parse monitoring events from COBOL output
const parseMonitoringEvents = (data, context) => {
  const lines = data.split('\n').filter(line => line.trim());
  
  for (const line of lines) {
    try {
      const event = JSON.parse(line);
      
      // Broadcast monitoring event
      websocket.broadcast('cobol:monitor:event', {
        executionId: context.id,
        event,
        timestamp: new Date().toISOString()
      });
      
      // Update context metrics if applicable
      if (event.type === 'METRIC') {
        Object.assign(context.metrics, event.data);
      }
    } catch (error) {
      // Handle non-JSON monitoring output
      logger.debug('Non-JSON monitoring output', { line, executionId: context.id });
    }
  }
};

// Parse debug trace data
const parseDebugTrace = (data, context) => {
  const lines = data.split('\n').filter(line => line.trim());
  
  for (const line of lines) {
    try {
      const trace = JSON.parse(line);
      
      // Add to debug trace with size limit
      context.debugTrace.push(trace);
      
      // Trim old entries if size exceeded
      const traceSize = JSON.stringify(context.debugTrace).length;
      if (traceSize > DEBUG_CONFIG.maxTraceSize) {
        context.debugTrace.shift();
      }
      
      // Broadcast debug event for live debugging
      if (DEBUG_CONFIG.verbosity === 'verbose') {
        websocket.broadcast('cobol:debug:trace', {
          executionId: context.id,
          trace,
          timestamp: new Date().toISOString()
        });
      }
    } catch (error) {
      // Handle non-JSON debug output
      logger.debug('Non-JSON debug output', { line, executionId: context.id });
    }
  }
};

// Extract warnings from stderr
const extractWarnings = (stderr) => {
  const warnings = [];
  const lines = stderr.split('\n');
  
  for (const line of lines) {
    if (line.includes('WARNING') || line.includes('WARN')) {
      warnings.push(line.trim());
    }
  }
  
  return warnings;
};

// Store debug trace for time-travel debugging
const storeDebugTrace = async (executionId, trace) => {
  const traceDir = process.env.DEBUG_TRACE_PATH || path.join(os.tmpdir(), 'cobol-debug-traces');
  await fs.mkdir(traceDir, { recursive: true });
  
  const tracePath = path.join(traceDir, `${executionId}.json`);
  await fs.writeFile(tracePath, JSON.stringify(trace, null, 2));
  
  logger.info('Debug trace stored', { executionId, tracePath, entries: trace.length });
};

// Clean up temporary directory
const cleanupTempDir = async (tempDir) => {
  try {
    await fs.rm(tempDir, { recursive: true, force: true });
  } catch (error) {
    logger.warn('Failed to cleanup temp directory', { tempDir, error: error.message });
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

// Enhanced COBOL data format converter with support for complex types
class CobolDataConverter {
  // COBOL Picture clause patterns
  static PIC_PATTERNS = {
    NUMERIC: /^9+$/,
    NUMERIC_SIGNED: /^S9+$/,
    NUMERIC_DECIMAL: /^9*V9+$/,
    NUMERIC_SIGNED_DECIMAL: /^S9*V9+$/,
    ALPHA: /^A+$/,
    ALPHANUMERIC: /^X+$/,
    COMP3: /COMP-3$/,
    COMP: /COMP$/
  };

  // Convert JSON to COBOL fixed-format data with enhanced type support
  static toCobolData(data, schema) {
    let result = '';
    
    for (const field of schema) {
      const value = data[field.name] ?? field.defaultValue ?? '';
      const formatted = this.formatFieldAdvanced(value, field);
      result += formatted;
    }
    
    return result;
  }
  
  // Convert COBOL data to JSON with enhanced type support
  static fromCobolData(data, schema) {
    const result = {};
    let position = 0;
    
    for (const field of schema) {
      const length = this.getFieldLength(field);
      const rawValue = data.substring(position, position + length);
      result[field.name] = this.parseFieldAdvanced(rawValue, field);
      position += length;
    }
    
    return result;
  }
  
  // Format field based on COBOL picture clause
  static formatFieldAdvanced(value, field) {
    const picture = field.picture || field.pic || '';
    const length = field.width || this.getFieldLength(field);
    
    // Handle COMP-3 (packed decimal)
    if (picture.includes('COMP-3')) {
      return this.packDecimal(value, length);
    }
    
    // Handle COMP (binary)
    if (picture.includes('COMP') && !picture.includes('COMP-3')) {
      return this.toBinary(value, length);
    }
    
    // Handle signed numbers
    if (picture.startsWith('S')) {
      return this.formatSigned(value, picture, length);
    }
    
    // Handle decimal numbers
    if (picture.includes('V')) {
      return this.formatDecimal(value, picture, length);
    }
    
    // Handle standard picture clauses
    return this.formatStandard(value, picture, length);
  }
  
  // Parse field based on COBOL picture clause
  static parseFieldAdvanced(rawValue, field) {
    const picture = field.picture || field.pic || '';
    
    // Handle COMP-3 (packed decimal)
    if (picture.includes('COMP-3')) {
      return this.unpackDecimal(rawValue, picture);
    }
    
    // Handle COMP (binary)
    if (picture.includes('COMP') && !picture.includes('COMP-3')) {
      return this.fromBinary(rawValue, picture);
    }
    
    // Handle signed numbers
    if (picture.startsWith('S')) {
      return this.parseSigned(rawValue, picture);
    }
    
    // Handle decimal numbers
    if (picture.includes('V')) {
      return this.parseDecimal(rawValue, picture);
    }
    
    // Handle boolean flags
    if (field.type === 'boolean' || picture === 'X' || field.boolean) {
      return rawValue.trim() === 'Y' || rawValue.trim() === '1';
    }
    
    // Handle standard fields
    return this.parseStandard(rawValue, picture, field.type);
  }
  
  // Get field length from picture clause
  static getFieldLength(field) {
    if (field.width) return field.width;
    
    const picture = field.picture || field.pic || '';
    
    // Extract length from picture clause
    const match = picture.match(/[9AX](\((\d+)\))?/g);
    if (!match) return 0;
    
    let length = 0;
    for (const part of match) {
      const repeatMatch = part.match(/\((\d+)\)/);
      if (repeatMatch) {
        length += parseInt(repeatMatch[1]);
      } else {
        length += 1;
      }
    }
    
    // Adjust for COMP fields
    if (picture.includes('COMP-3')) {
      length = Math.ceil((length + 1) / 2);
    } else if (picture.includes('COMP')) {
      length = length <= 4 ? 2 : length <= 9 ? 4 : 8;
    }
    
    return length;
  }
  
  // Format signed number
  static formatSigned(value, picture, length) {
    const num = parseFloat(value) || 0;
    const isNegative = num < 0;
    const absValue = Math.abs(num);
    
    if (picture.includes('V')) {
      // Signed decimal
      const decimalPlaces = picture.split('V')[1].replace(/[^9]/g, '').length;
      const multiplier = Math.pow(10, decimalPlaces);
      const intValue = Math.round(absValue * multiplier);
      let formatted = intValue.toString().padStart(length - 1, '0');
      
      // Add sign in last position (COBOL convention)
      formatted += isNegative ? '}' : '{';
      return formatted;
    } else {
      // Signed integer
      const formatted = absValue.toString().padStart(length - 1, '0');
      return formatted + (isNegative ? '-' : '+');
    }
  }
  
  // Parse signed number
  static parseSigned(value, picture) {
    const lastChar = value.slice(-1);
    const numPart = value.slice(0, -1);
    
    // Check for overpunch sign
    const overpunchSigns = {
      '{': ['+', 0], 'A': ['+', 1], 'B': ['+', 2], 'C': ['+', 3], 'D': ['+', 4],
      'E': ['+', 5], 'F': ['+', 6], 'G': ['+', 7], 'H': ['+', 8], 'I': ['+', 9],
      '}': ['-', 0], 'J': ['-', 1], 'K': ['-', 2], 'L': ['-', 3], 'M': ['-', 4],
      'N': ['-', 5], 'O': ['-', 6], 'P': ['-', 7], 'Q': ['-', 8], 'R': ['-', 9]
    };
    
    let sign = 1;
    let lastDigit = '';
    
    if (overpunchSigns[lastChar]) {
      [sign, lastDigit] = overpunchSigns[lastChar];
      sign = sign === '+' ? 1 : -1;
    } else if (lastChar === '-') {
      sign = -1;
    }
    
    const baseNum = parseInt(numPart + lastDigit) || 0;
    
    if (picture.includes('V')) {
      const decimalPlaces = picture.split('V')[1].replace(/[^9]/g, '').length;
      return (baseNum / Math.pow(10, decimalPlaces)) * sign;
    }
    
    return baseNum * sign;
  }
  
  // Format decimal number
  static formatDecimal(value, picture, length) {
    const decimalPos = picture.indexOf('V');
    const beforeDecimal = picture.substring(0, decimalPos).replace(/[^9]/g, '').length;
    const afterDecimal = picture.substring(decimalPos + 1).replace(/[^9]/g, '').length;
    
    const num = parseFloat(value) || 0;
    const multiplier = Math.pow(10, afterDecimal);
    const intValue = Math.round(Math.abs(num) * multiplier);
    
    return intValue.toString().padStart(length, '0');
  }
  
  // Parse decimal number
  static parseDecimal(value, picture) {
    const decimalPos = picture.indexOf('V');
    const afterDecimal = picture.substring(decimalPos + 1).replace(/[^9]/g, '').length;
    
    const intValue = parseInt(value) || 0;
    return intValue / Math.pow(10, afterDecimal);
  }
  
  // Format standard fields (numeric, alpha, alphanumeric)
  static formatStandard(value, picture, length) {
    if (this.PIC_PATTERNS.NUMERIC.test(picture)) {
      // Numeric field
      const num = parseInt(value) || 0;
      return num.toString().padStart(length, '0');
    } else if (this.PIC_PATTERNS.ALPHA.test(picture)) {
      // Alphabetic field
      const alpha = value.toString().replace(/[^A-Za-z ]/g, '');
      return alpha.padEnd(length, ' ').toUpperCase();
    } else {
      // Alphanumeric field
      return value.toString().padEnd(length, ' ');
    }
  }
  
  // Parse standard fields
  static parseStandard(value, picture, type) {
    if (this.PIC_PATTERNS.NUMERIC.test(picture)) {
      return parseInt(value) || 0;
    } else if (type === 'number') {
      return parseFloat(value) || 0;
    } else {
      return value.trim();
    }
  }
  
  // Pack decimal for COMP-3 storage
  static packDecimal(value, length) {
    const num = Math.abs(parseInt(value * 100)) || 0;
    const sign = value < 0 ? 'D' : 'C';
    const digits = num.toString().padStart(length * 2 - 1, '0') + sign;
    
    let packed = '';
    for (let i = 0; i < digits.length; i += 2) {
      const byte = parseInt(digits.substr(i, 2), 16);
      packed += String.fromCharCode(byte);
    }
    
    return packed;
  }
  
  // Unpack decimal from COMP-3 storage
  static unpackDecimal(packed, picture) {
    let digits = '';
    for (let i = 0; i < packed.length; i++) {
      const byte = packed.charCodeAt(i);
      digits += byte.toString(16).padStart(2, '0');
    }
    
    const sign = digits.slice(-1);
    const value = parseInt(digits.slice(0, -1)) || 0;
    const isNegative = sign.toLowerCase() === 'd';
    
    if (picture.includes('V')) {
      const decimalPlaces = picture.split('V')[1].replace(/[^9]/g, '').length;
      return (value / Math.pow(10, decimalPlaces)) * (isNegative ? -1 : 1);
    }
    
    return value * (isNegative ? -1 : 1);
  }
  
  // Convert to binary for COMP storage
  static toBinary(value, length) {
    const num = parseInt(value) || 0;
    const buffer = Buffer.alloc(length);
    
    if (length === 2) {
      buffer.writeInt16BE(num);
    } else if (length === 4) {
      buffer.writeInt32BE(num);
    } else {
      // For 8 bytes, use BigInt
      buffer.writeBigInt64BE(BigInt(num));
    }
    
    return buffer.toString('binary');
  }
  
  // Convert from binary COMP storage
  static fromBinary(binary, picture) {
    const buffer = Buffer.from(binary, 'binary');
    
    if (buffer.length === 2) {
      return buffer.readInt16BE();
    } else if (buffer.length === 4) {
      return buffer.readInt32BE();
    } else {
      return Number(buffer.readBigInt64BE());
    }
  }
  
  // Legacy method mappings for backward compatibility
  static toCreditInput(data) {
    const schema = [
      { name: 'customerId', picture: 'X(10)', width: 10 },
      { name: 'creditAmount', picture: '9(10)V99', width: 12 },
      { name: 'income', picture: '9(10)V99', width: 12 },
      { name: 'existingDebt', picture: '9(10)V99', width: 12 }
    ];
    return this.toCobolData(data, schema);
  }
  
  static fromCreditOutput(data) {
    const schema = [
      { name: 'approved', picture: 'X', width: 1, boolean: true },
      { name: 'creditLimit', picture: '9(10)V99', width: 12 },
      { name: 'interestRate', picture: '9(3)V99', width: 5 },
      { name: 'monthlyPayment', picture: '9(10)V99', width: 12 },
      { name: 'riskScore', picture: '999', width: 3 }
    ];
    return this.fromCobolData(data, schema);
  }
  
  static toPayrollInput(data) {
    const schema = [
      { name: 'employeeId', picture: 'X(10)', width: 10 },
      { name: 'hoursWorked', picture: '9(4)V99', width: 6 },
      { name: 'hourlyRate', picture: '9(6)V99', width: 8 },
      { name: 'deductions.federal', picture: '9(6)V99', width: 8 },
      { name: 'deductions.state', picture: '9(6)V99', width: 8 },
      { name: 'deductions.insurance', picture: '9(6)V99', width: 8 }
    ];
    
    // Flatten nested data
    const flatData = {
      employeeId: data.employeeId,
      hoursWorked: data.hoursWorked,
      hourlyRate: data.hourlyRate,
      'deductions.federal': data.deductions?.federal || 0,
      'deductions.state': data.deductions?.state || 0,
      'deductions.insurance': data.deductions?.insurance || 0
    };
    
    return this.toCobolData(flatData, schema);
  }
  
  static fromPayrollOutput(data) {
    const schema = [
      { name: 'grossPay', picture: '9(8)V99', width: 10 },
      { name: 'federalTax', picture: '9(8)V99', width: 10 },
      { name: 'stateTax', picture: '9(8)V99', width: 10 },
      { name: 'insurance', picture: '9(8)V99', width: 10 },
      { name: 'netPay', picture: '9(8)V99', width: 10 }
    ];
    
    const result = this.fromCobolData(data, schema);
    
    // Restructure to match expected format
    return {
      grossPay: result.grossPay,
      deductions: {
        federal: result.federalTax,
        state: result.stateTax,
        insurance: result.insurance
      },
      netPay: result.netPay
    };
  }
  
  static toInterestInput(data) {
    const schema = [
      { name: 'principal', picture: '9(10)V99', width: 12 },
      { name: 'rate', picture: '9(4)V99', width: 6 },
      { name: 'term', picture: '9999', width: 4 }
    ];
    return this.toCobolData(data, schema);
  }
  
  static fromInterestOutput(data) {
    const schema = [
      { name: 'totalInterest', picture: '9(10)V99', width: 12 },
      { name: 'totalAmount', picture: '9(10)V99', width: 12 },
      { name: 'monthlyPayment', picture: '9(10)V99', width: 12 }
    ];
    return this.fromCobolData(data, schema);
  }
  
  // Legacy generic methods for backward compatibility
  static toCobolFormat(data, schema) {
    return this.toCobolData(data, schema);
  }
  
  static fromCobolFormat(data, schema) {
    return this.fromCobolData(data, schema);
  }
  
  static formatField(value, width, type = 'string') {
    const field = { width, type, picture: type === 'number' ? `9(${width})` : `X(${width})` };
    return this.formatFieldAdvanced(value, field);
  }
  
  static parseField(value, type) {
    const field = { type, picture: type === 'number' ? '9(10)' : 'X(10)' };
    return this.parseFieldAdvanced(value, field);
  }
}

// Compile COBOL source code
const compileCobolProgram = async (sourcePath, options = {}) => {
  const compilerId = uuidv4();
  const startTime = Date.now();
  
  try {
    logger.info('COBOL compilation started', { compilerId, sourcePath });
    
    const sourceFile = path.basename(sourcePath);
    const outputName = options.outputName || sourceFile.replace(/\.cob$/i, '');
    const outputPath = path.join(process.env.COBOL_BIN_PATH || '/compiled', outputName);
    
    // Prepare compilation flags
    const flags = [
      '-x', // Create executable
      '-O2', // Optimization level 2
      '-Wall', // All warnings
      '-fmax-errors=10', // Stop after 10 errors
    ];
    
    if (options.debug) {
      flags.push('-g', '-ftraceall', '-fsource-location');
    }
    
    if (options.static) {
      flags.push('-static');
    }
    
    // Add output file
    flags.push('-o', outputPath);
    
    // Add source file
    flags.push(sourcePath);
    
    // Execute compilation
    const cobc = process.env.COBOL_COMPILER || 'cobc';
    const compileProcess = spawn(cobc, flags, {
      timeout: 60000 // 1 minute timeout
    });
    
    let stdout = '';
    let stderr = '';
    
    compileProcess.stdout.on('data', (data) => {
      stdout += data.toString();
    });
    
    compileProcess.stderr.on('data', (data) => {
      stderr += data.toString();
    });
    
    return new Promise((resolve, reject) => {
      compileProcess.on('exit', (code) => {
        const compilationTime = Date.now() - startTime;
        
        if (code !== 0) {
          logger.error('COBOL compilation failed', { 
            compilerId, 
            sourcePath, 
            compilationTime, 
            stderr 
          });
          reject(new Error(`Compilation failed: ${stderr}`));
          return;
        }
        
        logger.info('COBOL compilation completed', { 
          compilerId, 
          sourcePath, 
          outputPath, 
          compilationTime 
        });
        
        resolve({
          compilerId,
          sourcePath,
          outputPath,
          compilationTime,
          warnings: extractWarnings(stderr),
          success: true
        });
      });
      
      compileProcess.on('error', (error) => {
        reject(error);
      });
    });
  } catch (error) {
    logger.error('COBOL compilation error', { compilerId, sourcePath, error: error.message });
    throw error;
  }
};

// Get list of available COBOL programs
const listCobolPrograms = async () => {
  const binPath = process.env.COBOL_BIN_PATH || '/compiled';
  
  try {
    const files = await fs.readdir(binPath);
    const programs = [];
    
    for (const file of files) {
      const filePath = path.join(binPath, file);
      const stats = await fs.stat(filePath);
      
      if (stats.isFile() && (stats.mode & 0o111) !== 0) {
        programs.push({
          name: file,
          size: stats.size,
          modified: stats.mtime,
          cloudBurstEligible: stats.size < 10 * 1024 * 1024 // Programs under 10MB
        });
      }
    }
    
    return programs;
  } catch (error) {
    logger.error('Failed to list COBOL programs', { error: error.message });
    return [];
  }
};

// Validate COBOL data format
const validateCobolData = (data, schema) => {
  const errors = [];
  let position = 0;
  
  for (const field of schema) {
    const fieldData = data.substring(position, position + field.width);
    
    // Check field width
    if (fieldData.length < field.width && field.required) {
      errors.push(`Field ${field.name} is too short`);
    }
    
    // Validate field type
    switch (field.type) {
      case 'number':
      case 'decimal':
        if (!/^\d+$/.test(fieldData.trim())) {
          errors.push(`Field ${field.name} must be numeric`);
        }
        break;
      case 'alpha':
        if (!/^[A-Za-z\s]*$/.test(fieldData)) {
          errors.push(`Field ${field.name} must be alphabetic`);
        }
        break;
      case 'alphanumeric':
        // Any character is valid
        break;
    }
    
    position += field.width;
  }
  
  return errors;
};

module.exports = {
  executeCobolProgram,
  checkCobolRuntime,
  compileCobolProgram,
  listCobolPrograms,
  validateCobolData,
  CobolDataConverter,
  // Export configurations for external use
  MEMORY_LIMITS,
  SECURITY_CONFIG,
  DEBUG_CONFIG
};