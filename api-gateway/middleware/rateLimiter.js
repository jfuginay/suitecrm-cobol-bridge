const rateLimit = require('express-rate-limit');
const RedisStore = require('rate-limit-redis');
const logger = require('../utils/logger');

// Create Redis store for rate limiting
const createRedisStore = (redisClient) => {
  return new RedisStore({
    client: redisClient,
    prefix: 'rate_limit:'
  });
};

// General API rate limiter
const rateLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
  message: {
    error: 'Too many requests',
    message: 'You have exceeded the rate limit. Please try again later.',
    retryAfter: 15
  },
  standardHeaders: true,
  legacyHeaders: false,
  handler: (req, res) => {
    logger.warn('Rate limit exceeded', {
      ip: req.ip,
      path: req.path,
      userId: req.user?.id
    });
    res.status(429).json({
      error: 'Too many requests',
      message: 'You have exceeded the rate limit. Please try again later.',
      retryAfter: 15
    });
  },
  skip: (req) => {
    // Skip rate limiting for admin users
    return req.user?.role === 'admin';
  }
});

// Strict rate limiter for auth endpoints
const authRateLimiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 5, // limit each IP to 5 requests per windowMs
  skipSuccessfulRequests: true, // Don't count successful requests
  message: {
    error: 'Too many authentication attempts',
    message: 'Please wait before trying again.',
    retryAfter: 15
  },
  standardHeaders: true,
  legacyHeaders: false,
  handler: (req, res) => {
    logger.warn('Auth rate limit exceeded', {
      ip: req.ip,
      path: req.path
    });
    res.status(429).json({
      error: 'Too many authentication attempts',
      message: 'Please wait before trying again.',
      retryAfter: 15
    });
  }
});

// COBOL execution rate limiter
const cobolRateLimiter = rateLimit({
  windowMs: 1 * 60 * 1000, // 1 minute
  max: 20, // limit each user to 20 COBOL executions per minute
  keyGenerator: (req) => {
    // Rate limit by user ID instead of IP for authenticated requests
    return req.user?.id || req.ip;
  },
  message: {
    error: 'Too many COBOL executions',
    message: 'Please reduce the frequency of your requests.',
    retryAfter: 1
  },
  skip: (req) => {
    // Skip rate limiting for admin users
    return req.user?.role === 'admin';
  }
});

// Create custom rate limiter
const createRateLimiter = (options) => {
  return rateLimit({
    windowMs: options.windowMs || 15 * 60 * 1000,
    max: options.max || 100,
    message: options.message || {
      error: 'Too many requests',
      message: 'Rate limit exceeded'
    },
    keyGenerator: options.keyGenerator || ((req) => req.ip),
    skip: options.skip,
    handler: options.handler || ((req, res) => {
      res.status(429).json(options.message || {
        error: 'Too many requests',
        message: 'Rate limit exceeded'
      });
    })
  });
};

// Dynamic rate limiter based on user tier
const tieredRateLimiter = (req, res, next) => {
  const userTier = req.user?.tier || 'free';
  
  const limits = {
    free: { windowMs: 15 * 60 * 1000, max: 50 },
    basic: { windowMs: 15 * 60 * 1000, max: 200 },
    premium: { windowMs: 15 * 60 * 1000, max: 1000 },
    enterprise: { windowMs: 15 * 60 * 1000, max: 5000 }
  };
  
  const tierLimit = limits[userTier];
  const limiter = createRateLimiter(tierLimit);
  
  limiter(req, res, next);
};

module.exports = {
  rateLimiter,
  authRateLimiter,
  cobolRateLimiter,
  createRateLimiter,
  tieredRateLimiter,
  createRedisStore
};