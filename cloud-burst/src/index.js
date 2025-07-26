const express = require('express');
const { createServer } = require('http');
const { Server } = require('socket.io');
const { CloudBurstScheduler } = require('./scheduler/CloudBurstScheduler');
const logger = require('./utils/logger');
const config = require('./config/config');
const promClient = require('prom-client');

// Create Express app
const app = express();
const httpServer = createServer(app);
const io = new Server(httpServer, {
  cors: {
    origin: process.env.CORS_ORIGIN || '*',
    methods: ['GET', 'POST']
  }
});

// Middleware
app.use(express.json());
app.use(express.urlencoded({ extended: true }));

// Health check endpoint
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    version: process.env.npm_package_version || '1.0.0'
  });
});

// Metrics endpoint
const register = new promClient.Registry();
promClient.collectDefaultMetrics({ register });

app.get('/metrics', async (req, res) => {
  try {
    res.set('Content-Type', register.contentType);
    res.end(await register.metrics());
  } catch (error) {
    res.status(500).end(error);
  }
});

// Initialize CloudBurst Scheduler
let scheduler = null;

async function initializeScheduler() {
  logger.info('Initializing CloudBurst system...');
  
  try {
    scheduler = new CloudBurstScheduler(config.scheduler);
    
    // Setup event handlers
    scheduler.on('started', () => {
      logger.info('CloudBurst Scheduler started successfully');
      io.emit('scheduler:started');
    });
    
    scheduler.on('stopped', () => {
      logger.info('CloudBurst Scheduler stopped');
      io.emit('scheduler:stopped');
    });
    
    scheduler.on('scaled_up', (data) => {
      logger.info('Scaled up:', data);
      io.emit('scheduler:scaled_up', data);
    });
    
    scheduler.on('scaled_down', (data) => {
      logger.info('Scaled down:', data);
      io.emit('scheduler:scaled_down', data);
    });
    
    scheduler.on('status_update', (status) => {
      io.emit('scheduler:status', status);
    });
    
    // Start the scheduler
    await scheduler.start();
    
    // Setup API routes
    setupAPIRoutes();
    
    // Setup WebSocket handlers
    setupWebSocketHandlers();
    
  } catch (error) {
    logger.error('Failed to initialize CloudBurst system:', error);
    process.exit(1);
  }
}

function setupAPIRoutes() {
  // Get scheduler status
  app.get('/api/status', (req, res) => {
    if (!scheduler) {
      return res.status(503).json({ error: 'Scheduler not initialized' });
    }
    
    res.json({
      isRunning: scheduler.isRunning,
      metrics: scheduler.getMetrics(),
      timestamp: new Date().toISOString()
    });
  });
  
  // Manual scaling endpoints
  app.post('/api/scale/up', async (req, res) => {
    try {
      const { count = 1 } = req.body;
      await scheduler.instanceManager.scaleUp(count);
      res.json({ success: true, message: `Scaling up ${count} instances` });
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  app.post('/api/scale/down', async (req, res) => {
    try {
      const { count = 1 } = req.body;
      await scheduler.instanceManager.scaleDown(count);
      res.json({ success: true, message: `Scaling down ${count} instances` });
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  // Job management
  app.get('/api/jobs', async (req, res) => {
    try {
      const metrics = await scheduler.jobDistributor.getJobMetrics();
      res.json(metrics);
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  app.post('/api/jobs/submit', async (req, res) => {
    try {
      const job = req.body;
      await scheduler.jobDistributor.distributeJobs([job]);
      res.json({ success: true, message: 'Job submitted' });
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  // Instance management
  app.get('/api/instances', async (req, res) => {
    try {
      const instances = await scheduler.instanceManager.getAllInstances();
      res.json(instances);
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  // Cost analysis
  app.get('/api/costs', async (req, res) => {
    try {
      const report = scheduler.costOptimizer.generateCostReport();
      res.json(report);
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  app.post('/api/costs/optimize', async (req, res) => {
    try {
      const { recommendationId } = req.body;
      const recommendation = scheduler.costOptimizer.recommendations.find(
        r => r.id === recommendationId
      );
      
      if (!recommendation) {
        return res.status(404).json({ error: 'Recommendation not found' });
      }
      
      const result = await scheduler.costOptimizer.applyRecommendation(recommendation);
      res.json(result);
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  // Scheduler control
  app.post('/api/scheduler/start', async (req, res) => {
    try {
      await scheduler.start();
      res.json({ success: true, message: 'Scheduler started' });
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
  
  app.post('/api/scheduler/stop', async (req, res) => {
    try {
      await scheduler.stop();
      res.json({ success: true, message: 'Scheduler stopped' });
    } catch (error) {
      res.status(500).json({ error: error.message });
    }
  });
}

function setupWebSocketHandlers() {
  io.on('connection', (socket) => {
    logger.info('WebSocket client connected:', socket.id);
    
    // Send initial status
    socket.emit('scheduler:status', {
      isRunning: scheduler.isRunning,
      metrics: scheduler.getMetrics()
    });
    
    // Handle client requests
    socket.on('status:request', () => {
      socket.emit('scheduler:status', {
        isRunning: scheduler.isRunning,
        metrics: scheduler.getMetrics()
      });
    });
    
    socket.on('instances:request', async () => {
      const instances = await scheduler.instanceManager.getAllInstances();
      socket.emit('instances:update', instances);
    });
    
    socket.on('jobs:request', async () => {
      const metrics = await scheduler.jobDistributor.getJobMetrics();
      socket.emit('jobs:update', metrics);
    });
    
    socket.on('disconnect', () => {
      logger.info('WebSocket client disconnected:', socket.id);
    });
  });
}

// Error handling
app.use((err, req, res, next) => {
  logger.error('Unhandled error:', err);
  res.status(500).json({
    error: 'Internal server error',
    message: err.message
  });
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM received, starting graceful shutdown...');
  
  if (scheduler) {
    await scheduler.stop();
  }
  
  httpServer.close(() => {
    logger.info('HTTP server closed');
    process.exit(0);
  });
});

process.on('SIGINT', async () => {
  logger.info('SIGINT received, starting graceful shutdown...');
  
  if (scheduler) {
    await scheduler.stop();
  }
  
  httpServer.close(() => {
    logger.info('HTTP server closed');
    process.exit(0);
  });
});

// Start the server
async function start() {
  await initializeScheduler();
  
  const port = config.server.port;
  const host = config.server.host;
  
  httpServer.listen(port, host, () => {
    logger.info(`CloudBurst server listening on http://${host}:${port}`);
    logger.info(`Metrics available at http://${host}:${port}/metrics`);
    logger.info(`Health check at http://${host}:${port}/health`);
  });
}

// Export for testing
module.exports = { app, scheduler };

// Start if run directly
if (require.main === module) {
  start().catch(error => {
    logger.error('Failed to start CloudBurst:', error);
    process.exit(1);
  });
}