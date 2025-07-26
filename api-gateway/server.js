const express = require('express');
const helmet = require('helmet');
const cors = require('cors');
const compression = require('compression');
const morgan = require('morgan');
const swaggerUi = require('swagger-ui-express');
const YAML = require('yamljs');
const WebSocket = require('ws');
const http = require('http');
const path = require('path');
const winston = require('winston');
const expressWinston = require('express-winston');
const { promisify } = require('util');
const redis = require('redis');
require('express-async-errors');
require('dotenv').config();

// Import custom modules
const cobolRoutes = require('./routes/cobol');
const monitoringRoutes = require('./routes/monitoring');
const authRoutes = require('./routes/auth');
const { errorHandler } = require('./middleware/errorHandler');
const { setupWebSocket } = require('./utils/websocket');
const logger = require('./utils/logger');
const { JobQueue, JobProcessor } = require('./utils/jobQueue');
const { executeCobolProgram } = require('./utils/cobolWrapper');

// Initialize Express app
const app = express();
const server = http.createServer(app);
const wss = new WebSocket.Server({ server });

// Redis client setup
const redisClient = redis.createClient({
  url: process.env.REDIS_URL || 'redis://localhost:6379'
});

redisClient.on('error', (err) => logger.error('Redis Client Error', err));
redisClient.connect();

// Initialize job queue
const jobQueue = new JobQueue(redisClient);
const jobProcessor = new JobProcessor(jobQueue, { executeCobolProgram });

// Store in app locals for access in routes
app.locals.redisClient = redisClient;
app.locals.jobQueue = jobQueue;
app.locals.wss = wss;

// Global middleware
app.use(helmet());
app.use(cors({
  origin: process.env.ALLOWED_ORIGINS?.split(',') || '*',
  credentials: true
}));
app.use(compression());
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true, limit: '10mb' }));

// Request logging
app.use(expressWinston.logger({
  winstonInstance: logger,
  meta: true,
  msg: 'HTTP {{req.method}} {{req.url}}',
  expressFormat: true,
  colorize: false,
  ignoreRoute: function (req, res) { return false; }
}));

// Morgan logging for development
if (process.env.NODE_ENV === 'development') {
  app.use(morgan('dev'));
}

// Trust proxy
app.set('trust proxy', 1);

// API Documentation
const swaggerDocument = YAML.load(path.join(__dirname, 'swagger.yaml'));
app.use('/api-docs', swaggerUi.serve, swaggerUi.setup(swaggerDocument));

// Health check endpoint
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    environment: process.env.NODE_ENV
  });
});

// Routes
app.use('/api/v1/auth', authRoutes);
app.use('/api/v1/cobol', cobolRoutes);
app.use('/api/v1/monitoring', monitoringRoutes);

// WebSocket setup
setupWebSocket(wss);

// 404 handler
app.use((req, res) => {
  res.status(404).json({
    error: 'Not Found',
    message: `Cannot ${req.method} ${req.url}`,
    timestamp: new Date().toISOString()
  });
});

// Error logging
app.use(expressWinston.errorLogger({
  winstonInstance: logger
}));

// Global error handler
app.use(errorHandler);

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received: closing HTTP server');
  
  // Stop job processor
  jobProcessor.stop();
  
  server.close(() => {
    logger.info('HTTP server closed');
  });
  
  await redisClient.quit();
  process.exit(0);
});

// Start server
const PORT = process.env.PORT || 3000;
const HOST = process.env.HOST || '0.0.0.0';

server.listen(PORT, HOST, () => {
  logger.info(`API Gateway running on http://${HOST}:${PORT}`);
  logger.info(`API Documentation available at http://${HOST}:${PORT}/api-docs`);
  logger.info(`Environment: ${process.env.NODE_ENV || 'development'}`);
  
  // Start job processor
  jobProcessor.start();
  logger.info('Job processor started');
});

module.exports = { app, server, redisClient };