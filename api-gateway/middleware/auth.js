const jwt = require('jsonwebtoken');
const logger = require('../utils/logger');

// JWT authentication middleware
const authenticateToken = async (req, res, next) => {
  try {
    const authHeader = req.headers['authorization'];
    const token = authHeader && authHeader.split(' ')[1];
    
    // Check for API key authentication
    const apiKey = req.headers['x-api-key'];
    if (apiKey) {
      return authenticateApiKey(req, res, next, apiKey);
    }
    
    if (!token) {
      return res.status(401).json({
        error: 'Access token required'
      });
    }
    
    jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key', (err, user) => {
      if (err) {
        logger.warn('Invalid token attempt', { error: err.message });
        return res.status(403).json({
          error: 'Invalid or expired token'
        });
      }
      
      req.user = user;
      next();
    });
  } catch (error) {
    logger.error('Authentication error', error);
    res.status(500).json({
      error: 'Authentication failed'
    });
  }
};

// API key authentication
const authenticateApiKey = async (req, res, next, apiKey) => {
  try {
    const keyData = await req.app.locals.redisClient.get(`api_key:${apiKey}`);
    
    if (!keyData) {
      return res.status(401).json({
        error: 'Invalid API key'
      });
    }
    
    const { userId } = JSON.parse(keyData);
    req.user = { id: userId, type: 'api_key' };
    next();
  } catch (error) {
    logger.error('API key authentication error', error);
    res.status(500).json({
      error: 'Authentication failed'
    });
  }
};

// Admin role middleware
const requireAdmin = (req, res, next) => {
  if (req.user && req.user.role === 'admin') {
    next();
  } else {
    res.status(403).json({
      error: 'Admin access required'
    });
  }
};

// Permission-based middleware
const requirePermission = (permission) => {
  return (req, res, next) => {
    if (!req.user) {
      return res.status(401).json({
        error: 'Authentication required'
      });
    }
    
    // Check user permissions (in production, this would query a database)
    const userPermissions = getUserPermissions(req.user.role);
    
    if (userPermissions.includes(permission)) {
      next();
    } else {
      res.status(403).json({
        error: `Missing required permission: ${permission}`
      });
    }
  };
};

// Get permissions based on role
function getUserPermissions(role) {
  const permissions = {
    admin: ['read', 'write', 'delete', 'admin'],
    user: ['read', 'write'],
    guest: ['read']
  };
  
  return permissions[role] || [];
}

// Optional authentication (doesn't fail if no token)
const optionalAuth = (req, res, next) => {
  const authHeader = req.headers['authorization'];
  const token = authHeader && authHeader.split(' ')[1];
  
  if (token) {
    jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key', (err, user) => {
      if (!err) {
        req.user = user;
      }
    });
  }
  
  next();
};

module.exports = {
  authenticateToken,
  requireAdmin,
  requirePermission,
  optionalAuth
};