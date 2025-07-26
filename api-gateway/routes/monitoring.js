const express = require('express');
const router = express.Router();
const os = require('os');
const { register } = require('prom-client');
const { authenticateToken, requireAdmin } = require('../middleware/auth');
const logger = require('../utils/logger');
const { getSystemMetrics } = require('../utils/metrics');

// Public health check
router.get('/health', async (req, res) => {
  try {
    const health = {
      status: 'healthy',
      timestamp: new Date().toISOString(),
      uptime: process.uptime(),
      environment: process.env.NODE_ENV,
      version: process.env.npm_package_version || '1.0.0'
    };
    
    // Check Redis connection
    try {
      await req.app.locals.redisClient.ping();
      health.redis = 'connected';
    } catch (error) {
      health.redis = 'disconnected';
      health.status = 'degraded';
    }
    
    res.json(health);
  } catch (error) {
    logger.error('Health check error', error);
    res.status(503).json({
      status: 'unhealthy',
      error: error.message
    });
  }
});

// Detailed health check (authenticated)
router.get('/health/detailed', authenticateToken, async (req, res) => {
  try {
    const health = {
      status: 'healthy',
      timestamp: new Date().toISOString(),
      uptime: process.uptime(),
      environment: process.env.NODE_ENV,
      version: process.env.npm_package_version || '1.0.0',
      system: {
        platform: os.platform(),
        release: os.release(),
        totalMemory: os.totalmem(),
        freeMemory: os.freemem(),
        cpus: os.cpus().length,
        loadAverage: os.loadavg()
      },
      process: {
        pid: process.pid,
        memoryUsage: process.memoryUsage(),
        cpuUsage: process.cpuUsage()
      }
    };
    
    // Check dependencies
    const dependencies = {};
    
    // Redis check
    try {
      const start = Date.now();
      await req.app.locals.redisClient.ping();
      dependencies.redis = {
        status: 'connected',
        latency: Date.now() - start
      };
    } catch (error) {
      dependencies.redis = {
        status: 'disconnected',
        error: error.message
      };
      health.status = 'degraded';
    }
    
    // COBOL runtime check
    try {
      const { checkCobolRuntime } = require('../utils/cobolWrapper');
      const cobolStatus = await checkCobolRuntime();
      dependencies.cobol = cobolStatus;
    } catch (error) {
      dependencies.cobol = {
        status: 'error',
        error: error.message
      };
      health.status = 'degraded';
    }
    
    health.dependencies = dependencies;
    
    res.json(health);
  } catch (error) {
    logger.error('Detailed health check error', error);
    res.status(503).json({
      status: 'unhealthy',
      error: error.message
    });
  }
});

// Prometheus metrics endpoint
router.get('/metrics', authenticateToken, requireAdmin, async (req, res) => {
  try {
    res.set('Content-Type', register.contentType);
    const metrics = await register.metrics();
    res.end(metrics);
  } catch (error) {
    logger.error('Metrics error', error);
    res.status(500).json({
      error: 'Failed to generate metrics'
    });
  }
});

// System statistics
router.get('/stats', authenticateToken, async (req, res) => {
  try {
    const stats = await getSystemMetrics();
    res.json(stats);
  } catch (error) {
    logger.error('Stats error', error);
    res.status(500).json({
      error: 'Failed to generate statistics'
    });
  }
});

// Recent logs (admin only)
router.get('/logs', authenticateToken, requireAdmin, async (req, res) => {
  try {
    const { level = 'info', limit = 100 } = req.query;
    const logs = await logger.query({
      limit: parseInt(limit),
      order: 'desc',
      level: level
    });
    
    res.json({
      count: logs.length,
      logs: logs
    });
  } catch (error) {
    logger.error('Log retrieval error', error);
    res.status(500).json({
      error: 'Failed to retrieve logs'
    });
  }
});

// Active connections
router.get('/connections', authenticateToken, requireAdmin, async (req, res) => {
  try {
    const connections = {
      http: {
        active: req.app._connectionCount || 0
      },
      websocket: {
        active: req.app.locals.wss?.clients?.size || 0
      },
      redis: {
        connected: req.app.locals.redisClient?.isOpen || false
      }
    };
    
    res.json(connections);
  } catch (error) {
    logger.error('Connection stats error', error);
    res.status(500).json({
      error: 'Failed to retrieve connection statistics'
    });
  }
});

// Performance metrics
router.get('/performance', authenticateToken, async (req, res) => {
  try {
    const { timeRange = '1h' } = req.query;
    const performanceData = await getPerformanceMetrics(timeRange);
    
    res.json({
      timeRange,
      metrics: performanceData
    });
  } catch (error) {
    logger.error('Performance metrics error', error);
    res.status(500).json({
      error: 'Failed to retrieve performance metrics'
    });
  }
});

// Error rate monitoring
router.get('/errors', authenticateToken, requireAdmin, async (req, res) => {
  try {
    const { timeRange = '1h' } = req.query;
    const errorStats = await getErrorStatistics(timeRange);
    
    res.json({
      timeRange,
      errors: errorStats
    });
  } catch (error) {
    logger.error('Error statistics error', error);
    res.status(500).json({
      error: 'Failed to retrieve error statistics'
    });
  }
});

// Helper functions
async function getPerformanceMetrics(timeRange) {
  // Implementation would connect to your metrics storage
  return {
    avgResponseTime: 45,
    p95ResponseTime: 120,
    p99ResponseTime: 250,
    requestsPerSecond: 150,
    successRate: 99.5
  };
}

async function getErrorStatistics(timeRange) {
  // Implementation would query error logs
  return {
    totalErrors: 42,
    errorsByType: {
      '4xx': 30,
      '5xx': 12
    },
    topErrors: [
      { code: 400, count: 15, message: 'Invalid request' },
      { code: 401, count: 10, message: 'Unauthorized' },
      { code: 500, count: 8, message: 'Internal server error' }
    ]
  };
}

module.exports = router;