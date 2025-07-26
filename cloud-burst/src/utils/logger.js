const winston = require('winston');
const path = require('path');

// Create logs directory if it doesn't exist
const fs = require('fs');
const logDir = path.join(__dirname, '../../logs');
if (!fs.existsSync(logDir)) {
  fs.mkdirSync(logDir, { recursive: true });
}

// Custom format for console output
const consoleFormat = winston.format.combine(
  winston.format.colorize(),
  winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
  winston.format.errors({ stack: true }),
  winston.format.printf(({ timestamp, level, message, stack, ...meta }) => {
    let msg = `${timestamp} [${level}]: ${message}`;
    
    if (Object.keys(meta).length > 0) {
      msg += ` ${JSON.stringify(meta)}`;
    }
    
    if (stack) {
      msg += `\n${stack}`;
    }
    
    return msg;
  })
);

// Custom format for file output
const fileFormat = winston.format.combine(
  winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss' }),
  winston.format.errors({ stack: true }),
  winston.format.json()
);

// Create logger instance
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  
  transports: [
    // Console transport
    new winston.transports.Console({
      format: consoleFormat
    }),
    
    // File transport for all logs
    new winston.transports.File({
      filename: path.join(logDir, 'cloud-burst.log'),
      format: fileFormat,
      maxsize: 10 * 1024 * 1024, // 10MB
      maxFiles: 10,
      tailable: true
    }),
    
    // File transport for errors only
    new winston.transports.File({
      filename: path.join(logDir, 'cloud-burst-error.log'),
      level: 'error',
      format: fileFormat,
      maxsize: 10 * 1024 * 1024, // 10MB
      maxFiles: 5,
      tailable: true
    })
  ],
  
  // Handle exceptions
  exceptionHandlers: [
    new winston.transports.File({
      filename: path.join(logDir, 'cloud-burst-exceptions.log'),
      format: fileFormat
    })
  ],
  
  // Handle promise rejections
  rejectionHandlers: [
    new winston.transports.File({
      filename: path.join(logDir, 'cloud-burst-rejections.log'),
      format: fileFormat
    })
  ]
});

// Add performance logging
logger.startTimer = () => {
  const start = Date.now();
  return {
    done: (message, meta = {}) => {
      const duration = Date.now() - start;
      logger.info(message, { ...meta, duration: `${duration}ms` });
    }
  };
};

// Add structured logging helpers
logger.logJob = (action, jobId, data = {}) => {
  logger.info(`Job ${action}: ${jobId}`, {
    type: 'job',
    action,
    jobId,
    ...data
  });
};

logger.logInstance = (action, instanceId, data = {}) => {
  logger.info(`Instance ${action}: ${instanceId}`, {
    type: 'instance',
    action,
    instanceId,
    ...data
  });
};

logger.logMetric = (metric, value, data = {}) => {
  logger.debug(`Metric: ${metric}`, {
    type: 'metric',
    metric,
    value,
    ...data
  });
};

module.exports = logger;