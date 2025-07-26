const logger = require('../utils/logger');

// Cache middleware using Redis
const cacheMiddleware = (duration = 300) => {
  return async (req, res, next) => {
    // Only cache GET requests
    if (req.method !== 'GET') {
      return next();
    }
    
    // Generate cache key
    const cacheKey = `cache:${req.originalUrl}:${req.user?.id || 'anonymous'}`;
    
    try {
      // Try to get from cache
      const cached = await req.app.locals.redisClient.get(cacheKey);
      
      if (cached) {
        logger.debug('Cache hit', { key: cacheKey });
        const data = JSON.parse(cached);
        return res.json(data);
      }
      
      // Cache miss - continue to route handler
      logger.debug('Cache miss', { key: cacheKey });
      
      // Override res.json to cache the response
      const originalJson = res.json;
      res.json = function(data) {
        // Cache the response
        req.app.locals.redisClient.setEx(cacheKey, duration, JSON.stringify(data))
          .catch(err => logger.error('Cache set error', err));
        
        // Call original json method
        return originalJson.call(this, data);
      };
      
      next();
    } catch (error) {
      logger.error('Cache middleware error', error);
      // Continue without caching on error
      next();
    }
  };
};

// Clear cache for specific patterns
const clearCache = async (redisClient, pattern) => {
  try {
    const keys = await redisClient.keys(`cache:${pattern}*`);
    if (keys.length > 0) {
      await redisClient.del(keys);
      logger.info('Cache cleared', { pattern, count: keys.length });
    }
  } catch (error) {
    logger.error('Cache clear error', error);
  }
};

// Cache invalidation middleware
const invalidateCache = (patterns = []) => {
  return async (req, res, next) => {
    // After successful modification, invalidate related caches
    const originalJson = res.json;
    res.json = function(data) {
      if (res.statusCode < 300) {
        // Invalidate cache patterns
        patterns.forEach(pattern => {
          clearCache(req.app.locals.redisClient, pattern)
            .catch(err => logger.error('Cache invalidation error', err));
        });
      }
      return originalJson.call(this, data);
    };
    next();
  };
};

module.exports = {
  cacheMiddleware,
  clearCache,
  invalidateCache
};