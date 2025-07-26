const client = require('prom-client');
const logger = require('./logger');

// Create a Registry
const register = new client.Registry();

// Add default labels
register.setDefaultLabels({
  app: 'suitecrm-cobol-api-gateway'
});

// Enable default metrics
client.collectDefaultMetrics({ register });

// Custom metrics
const httpRequestDuration = new client.Histogram({
  name: 'http_request_duration_seconds',
  help: 'Duration of HTTP requests in seconds',
  labelNames: ['method', 'route', 'status_code'],
  buckets: [0.1, 0.5, 1, 2, 5]
});

const httpRequestTotal = new client.Counter({
  name: 'http_requests_total',
  help: 'Total number of HTTP requests',
  labelNames: ['method', 'route', 'status_code']
});

const cobolExecutionDuration = new client.Histogram({
  name: 'cobol_execution_duration_seconds',
  help: 'Duration of COBOL program executions in seconds',
  labelNames: ['program', 'status'],
  buckets: [0.5, 1, 2, 5, 10, 30]
});

const cobolExecutionTotal = new client.Counter({
  name: 'cobol_executions_total',
  help: 'Total number of COBOL program executions',
  labelNames: ['program', 'status']
});

const activeConnections = new client.Gauge({
  name: 'active_connections',
  help: 'Number of active connections',
  labelNames: ['type']
});

const cacheHits = new client.Counter({
  name: 'cache_hits_total',
  help: 'Total number of cache hits'
});

const cacheMisses = new client.Counter({
  name: 'cache_misses_total',
  help: 'Total number of cache misses'
});

const authAttempts = new client.Counter({
  name: 'auth_attempts_total',
  help: 'Total number of authentication attempts',
  labelNames: ['status']
});

const rateLimitHits = new client.Counter({
  name: 'rate_limit_hits_total',
  help: 'Total number of rate limit hits',
  labelNames: ['endpoint']
});

// Register all metrics
register.registerMetric(httpRequestDuration);
register.registerMetric(httpRequestTotal);
register.registerMetric(cobolExecutionDuration);
register.registerMetric(cobolExecutionTotal);
register.registerMetric(activeConnections);
register.registerMetric(cacheHits);
register.registerMetric(cacheMisses);
register.registerMetric(authAttempts);
register.registerMetric(rateLimitHits);

// Middleware to collect HTTP metrics
const metricsMiddleware = (req, res, next) => {
  const start = Date.now();
  
  res.on('finish', () => {
    const duration = (Date.now() - start) / 1000;
    const route = req.route?.path || req.path;
    const labels = {
      method: req.method,
      route: route,
      status_code: res.statusCode
    };
    
    httpRequestDuration.observe(labels, duration);
    httpRequestTotal.inc(labels);
  });
  
  next();
};

// Get system metrics
const getSystemMetrics = async () => {
  try {
    const metrics = await register.getMetricsAsJSON();
    
    // Extract key metrics
    const httpRequests = metrics.find(m => m.name === 'http_requests_total');
    const cobolExecutions = metrics.find(m => m.name === 'cobol_executions_total');
    const processUptime = metrics.find(m => m.name === 'process_uptime_seconds');
    const memoryUsage = metrics.find(m => m.name === 'nodejs_heap_size_used_bytes');
    
    return {
      uptime: processUptime?.values[0]?.value || 0,
      totalRequests: httpRequests?.values.reduce((sum, v) => sum + v.value, 0) || 0,
      totalCobolExecutions: cobolExecutions?.values.reduce((sum, v) => sum + v.value, 0) || 0,
      memoryUsage: memoryUsage?.values[0]?.value || 0,
      timestamp: new Date().toISOString()
    };
  } catch (error) {
    logger.error('Error getting system metrics', error);
    return null;
  }
};

// Record COBOL execution metrics
const recordCobolExecution = (program, duration, success) => {
  const labels = {
    program,
    status: success ? 'success' : 'failure'
  };
  
  cobolExecutionDuration.observe(labels, duration);
  cobolExecutionTotal.inc(labels);
};

// Update connection metrics
const updateConnectionMetrics = (type, count) => {
  activeConnections.set({ type }, count);
};

// Record cache metrics
const recordCacheHit = () => cacheHits.inc();
const recordCacheMiss = () => cacheMisses.inc();

// Record auth metrics
const recordAuthAttempt = (success) => {
  authAttempts.inc({ status: success ? 'success' : 'failure' });
};

// Record rate limit hits
const recordRateLimitHit = (endpoint) => {
  rateLimitHits.inc({ endpoint });
};

module.exports = {
  register,
  metricsMiddleware,
  getSystemMetrics,
  recordCobolExecution,
  updateConnectionMetrics,
  recordCacheHit,
  recordCacheMiss,
  recordAuthAttempt,
  recordRateLimitHit
};