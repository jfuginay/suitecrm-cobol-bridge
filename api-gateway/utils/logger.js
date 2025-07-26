const winston = require('winston');
const path = require('path');

// Define log levels
const levels = {
  error: 0,
  warn: 1,
  info: 2,
  http: 3,
  verbose: 4,
  debug: 5,
  silly: 6
};

// Define colors for each level
const colors = {
  error: 'red',
  warn: 'yellow',
  info: 'green',
  http: 'magenta',
  verbose: 'cyan',
  debug: 'blue',
  silly: 'gray'
};

winston.addColors(colors);

// Define log format
const format = winston.format.combine(
  winston.format.timestamp({ format: 'YYYY-MM-DD HH:mm:ss.SSS' }),
  winston.format.errors({ stack: true }),
  winston.format.splat(),
  winston.format.json()
);

// Define transports
const transports = [
  // Console transport
  new winston.transports.Console({
    format: winston.format.combine(
      winston.format.colorize({ all: true }),
      winston.format.printf(
        info => `${info.timestamp} ${info.level}: ${info.message} ${info.stack || ''}`
      )
    )
  }),
  
  // File transport for all logs
  new winston.transports.File({
    filename: path.join(process.env.LOG_PATH || 'logs', 'combined.log'),
    maxsize: 10485760, // 10MB
    maxFiles: 5,
    format
  }),
  
  // File transport for errors
  new winston.transports.File({
    filename: path.join(process.env.LOG_PATH || 'logs', 'error.log'),
    level: 'error',
    maxsize: 10485760, // 10MB
    maxFiles: 5,
    format
  })
];

// Create logger instance
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  levels,
  format,
  transports,
  exitOnError: false
});

// Stream for Morgan HTTP logging
logger.stream = {
  write: (message) => {
    logger.http(message.trim());
  }
};

// Add query functionality for retrieving logs
logger.query = async (options = {}) => {
  return new Promise((resolve, reject) => {
    const queryOptions = {
      from: options.from || new Date() - 24 * 60 * 60 * 1000,
      until: options.until || new Date(),
      limit: options.limit || 100,
      start: 0,
      order: options.order || 'desc',
      fields: options.fields
    };
    
    logger.query(queryOptions, (err, results) => {
      if (err) {
        reject(err);
      } else {
        resolve(results.file || []);
      }
    });
  });
};

// Production logging adjustments
if (process.env.NODE_ENV === 'production') {
  // Remove console transport in production
  logger.remove(transports[0]);
  
  // Add daily rotate file transport
  const DailyRotateFile = require('winston-daily-rotate-file');
  
  logger.add(new DailyRotateFile({
    filename: path.join(process.env.LOG_PATH || 'logs', 'application-%DATE%.log'),
    datePattern: 'YYYY-MM-DD',
    zippedArchive: true,
    maxSize: '20m',
    maxFiles: '14d',
    format
  }));
}

// Custom log methods
logger.logRequest = (req, responseTime, statusCode) => {
  const message = `${req.method} ${req.originalUrl}`;
  const meta = {
    method: req.method,
    url: req.originalUrl,
    ip: req.ip,
    userAgent: req.get('user-agent'),
    responseTime,
    statusCode,
    userId: req.user?.id
  };
  
  if (statusCode >= 500) {
    logger.error(message, meta);
  } else if (statusCode >= 400) {
    logger.warn(message, meta);
  } else {
    logger.info(message, meta);
  }
};

logger.logError = (error, req = null) => {
  const errorInfo = {
    message: error.message,
    stack: error.stack,
    code: error.code,
    name: error.name
  };
  
  if (req) {
    errorInfo.request = {
      method: req.method,
      url: req.originalUrl,
      ip: req.ip,
      userId: req.user?.id
    };
  }
  
  logger.error('Application error', errorInfo);
};

module.exports = logger;