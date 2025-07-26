const express = require('express');
const router = express.Router();
const jwt = require('jsonwebtoken');
const bcrypt = require('bcrypt');
const { body, validationResult } = require('express-validator');
const { authenticateToken } = require('../middleware/auth');
const { authRateLimiter } = require('../middleware/rateLimiter');
const logger = require('../utils/logger');

// Apply rate limiting to auth routes
router.use(authRateLimiter);

// Login endpoint
router.post('/login',
  [
    body('username').isString().notEmpty(),
    body('password').isString().notEmpty()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { username, password } = req.body;
      
      // In production, this would query a database
      // For demo purposes, using hardcoded users
      const users = {
        'admin': {
          id: '1',
          username: 'admin',
          password: await bcrypt.hash('admin123', 10),
          role: 'admin'
        },
        'user': {
          id: '2',
          username: 'user',
          password: await bcrypt.hash('user123', 10),
          role: 'user'
        }
      };
      
      const user = users[username];
      if (!user) {
        return res.status(401).json({
          error: 'Invalid credentials'
        });
      }
      
      const validPassword = await bcrypt.compare(password, user.password);
      if (!validPassword) {
        return res.status(401).json({
          error: 'Invalid credentials'
        });
      }
      
      // Generate tokens
      const accessToken = jwt.sign(
        { id: user.id, username: user.username, role: user.role },
        process.env.JWT_SECRET || 'your-secret-key',
        { expiresIn: '15m' }
      );
      
      const refreshToken = jwt.sign(
        { id: user.id, type: 'refresh' },
        process.env.JWT_REFRESH_SECRET || 'your-refresh-secret',
        { expiresIn: '7d' }
      );
      
      // Store refresh token in Redis
      await req.app.locals.redisClient.setEx(
        `refresh_token:${user.id}`,
        7 * 24 * 60 * 60, // 7 days
        refreshToken
      );
      
      logger.info('User logged in', { userId: user.id, username: user.username });
      
      res.json({
        success: true,
        accessToken,
        refreshToken,
        user: {
          id: user.id,
          username: user.username,
          role: user.role
        }
      });
    } catch (error) {
      logger.error('Login error', error);
      res.status(500).json({
        error: 'Login failed'
      });
    }
  }
);

// Refresh token endpoint
router.post('/refresh',
  [
    body('refreshToken').isString().notEmpty()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { refreshToken } = req.body;
      
      // Verify refresh token
      const decoded = jwt.verify(
        refreshToken,
        process.env.JWT_REFRESH_SECRET || 'your-refresh-secret'
      );
      
      if (decoded.type !== 'refresh') {
        return res.status(401).json({
          error: 'Invalid token type'
        });
      }
      
      // Check if refresh token exists in Redis
      const storedToken = await req.app.locals.redisClient.get(`refresh_token:${decoded.id}`);
      if (storedToken !== refreshToken) {
        return res.status(401).json({
          error: 'Invalid refresh token'
        });
      }
      
      // Generate new access token
      const accessToken = jwt.sign(
        { id: decoded.id, username: decoded.username, role: decoded.role },
        process.env.JWT_SECRET || 'your-secret-key',
        { expiresIn: '15m' }
      );
      
      logger.info('Token refreshed', { userId: decoded.id });
      
      res.json({
        success: true,
        accessToken
      });
    } catch (error) {
      logger.error('Token refresh error', error);
      res.status(401).json({
        error: 'Invalid refresh token'
      });
    }
  }
);

// Logout endpoint
router.post('/logout', authenticateToken, async (req, res) => {
  try {
    // Remove refresh token from Redis
    await req.app.locals.redisClient.del(`refresh_token:${req.user.id}`);
    
    logger.info('User logged out', { userId: req.user.id });
    
    res.json({
      success: true,
      message: 'Logged out successfully'
    });
  } catch (error) {
    logger.error('Logout error', error);
    res.status(500).json({
      error: 'Logout failed'
      });
  }
});

// Get current user endpoint
router.get('/me', authenticateToken, (req, res) => {
  res.json({
    success: true,
    user: req.user
  });
});

// Change password endpoint
router.post('/change-password',
  authenticateToken,
  [
    body('currentPassword').isString().notEmpty(),
    body('newPassword').isString().isLength({ min: 8 })
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { currentPassword, newPassword } = req.body;
      
      // In production, this would update the database
      logger.info('Password change requested', { userId: req.user.id });
      
      res.json({
        success: true,
        message: 'Password changed successfully'
      });
    } catch (error) {
      logger.error('Password change error', error);
      res.status(500).json({
        error: 'Failed to change password'
      });
    }
  }
);

// API key generation endpoint (admin only)
router.post('/api-key',
  authenticateToken,
  async (req, res) => {
    try {
      if (req.user.role !== 'admin') {
        return res.status(403).json({
          error: 'Admin access required'
        });
      }
      
      const apiKey = require('crypto').randomBytes(32).toString('hex');
      const hashedKey = await bcrypt.hash(apiKey, 10);
      
      // Store API key in Redis
      await req.app.locals.redisClient.setEx(
        `api_key:${apiKey}`,
        365 * 24 * 60 * 60, // 1 year
        JSON.stringify({
          userId: req.user.id,
          created: new Date().toISOString()
        })
      );
      
      logger.info('API key generated', { userId: req.user.id });
      
      res.json({
        success: true,
        apiKey,
        message: 'Store this API key securely. It will not be shown again.'
      });
    } catch (error) {
      logger.error('API key generation error', error);
      res.status(500).json({
        error: 'Failed to generate API key'
      });
    }
  }
);

// Revoke API key endpoint
router.delete('/api-key/:key',
  authenticateToken,
  async (req, res) => {
    try {
      if (req.user.role !== 'admin') {
        return res.status(403).json({
          error: 'Admin access required'
        });
      }
      
      const { key } = req.params;
      await req.app.locals.redisClient.del(`api_key:${key}`);
      
      logger.info('API key revoked', { userId: req.user.id });
      
      res.json({
        success: true,
        message: 'API key revoked successfully'
      });
    } catch (error) {
      logger.error('API key revocation error', error);
      res.status(500).json({
        error: 'Failed to revoke API key'
      });
    }
  }
);

module.exports = router;