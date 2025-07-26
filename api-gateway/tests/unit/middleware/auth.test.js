const jwt = require('jsonwebtoken');
const { authenticateToken, requireAdmin, requirePermission, optionalAuth } = require('../../../middleware/auth');

// Mock dependencies
jest.mock('../../../utils/logger', () => ({
  warn: jest.fn(),
  error: jest.fn()
}));

describe('Auth Middleware', () => {
  let req, res, next;

  beforeEach(() => {
    req = {
      headers: {},
      app: {
        locals: {
          redisClient: {
            get: jest.fn()
          }
        }
      }
    };
    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn()
    };
    next = jest.fn();
    
    // Clear all mocks
    jest.clearAllMocks();
    
    // Set test JWT secret
    process.env.JWT_SECRET = 'test-secret-key';
  });

  afterEach(() => {
    delete process.env.JWT_SECRET;
  });

  describe('authenticateToken', () => {
    it('should authenticate valid JWT token', async () => {
      const testUser = { id: 1, username: 'testuser', role: 'user' };
      const token = jwt.sign(testUser, 'test-secret-key');
      
      req.headers['authorization'] = `Bearer ${token}`;
      
      await authenticateToken(req, res, next);
      
      expect(req.user).toEqual(testUser);
      expect(next).toHaveBeenCalled();
      expect(res.status).not.toHaveBeenCalled();
    });

    it('should reject missing token', async () => {
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(401);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Access token required'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should reject invalid token', async () => {
      req.headers['authorization'] = 'Bearer invalid-token';
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Invalid or expired token'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should authenticate with valid API key', async () => {
      const apiKey = 'test-api-key';
      const keyData = { userId: 'user123' };
      
      req.headers['x-api-key'] = apiKey;
      req.app.locals.redisClient.get.mockResolvedValue(JSON.stringify(keyData));
      
      await authenticateToken(req, res, next);
      
      expect(req.user).toEqual({ id: 'user123', type: 'api_key' });
      expect(next).toHaveBeenCalled();
      expect(req.app.locals.redisClient.get).toHaveBeenCalledWith(`api_key:${apiKey}`);
    });

    it('should reject invalid API key', async () => {
      const apiKey = 'invalid-api-key';
      
      req.headers['x-api-key'] = apiKey;
      req.app.locals.redisClient.get.mockResolvedValue(null);
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(401);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Invalid API key'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should handle Redis errors gracefully', async () => {
      const apiKey = 'test-api-key';
      
      req.headers['x-api-key'] = apiKey;
      req.app.locals.redisClient.get.mockRejectedValue(new Error('Redis connection failed'));
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Authentication failed'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should handle expired JWT token', async () => {
      const testUser = { id: 1, username: 'testuser', role: 'user' };
      const expiredToken = jwt.sign(testUser, 'test-secret-key', { expiresIn: '-1h' });
      
      req.headers['authorization'] = `Bearer ${expiredToken}`;
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Invalid or expired token'
      });
      expect(next).not.toHaveBeenCalled();
    });
  });

  describe('requireAdmin', () => {
    it('should allow admin users', () => {
      req.user = { id: 1, role: 'admin' };
      
      requireAdmin(req, res, next);
      
      expect(next).toHaveBeenCalled();
      expect(res.status).not.toHaveBeenCalled();
    });

    it('should reject non-admin users', () => {
      req.user = { id: 1, role: 'user' };
      
      requireAdmin(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Admin access required'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should reject unauthenticated requests', () => {
      requireAdmin(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Admin access required'
      });
      expect(next).not.toHaveBeenCalled();
    });
  });

  describe('requirePermission', () => {
    it('should allow users with required permission', () => {
      req.user = { id: 1, role: 'admin' };
      const middleware = requirePermission('read');
      
      middleware(req, res, next);
      
      expect(next).toHaveBeenCalled();
      expect(res.status).not.toHaveBeenCalled();
    });

    it('should reject users without required permission', () => {
      req.user = { id: 1, role: 'guest' };
      const middleware = requirePermission('write');
      
      middleware(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Missing required permission: write'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should reject unauthenticated requests', () => {
      const middleware = requirePermission('read');
      
      middleware(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(401);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Authentication required'
      });
      expect(next).not.toHaveBeenCalled();
    });

    it('should handle unknown roles gracefully', () => {
      req.user = { id: 1, role: 'unknown' };
      const middleware = requirePermission('read');
      
      middleware(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Missing required permission: read'
      });
      expect(next).not.toHaveBeenCalled();
    });
  });

  describe('optionalAuth', () => {
    it('should set user when valid token provided', () => {
      const testUser = { id: 1, username: 'testuser', role: 'user' };
      const token = jwt.sign(testUser, 'test-secret-key');
      
      req.headers['authorization'] = `Bearer ${token}`;
      
      optionalAuth(req, res, next);
      
      expect(req.user).toEqual(testUser);
      expect(next).toHaveBeenCalled();
    });

    it('should continue without user when no token provided', () => {
      optionalAuth(req, res, next);
      
      expect(req.user).toBeUndefined();
      expect(next).toHaveBeenCalled();
    });

    it('should continue without user when invalid token provided', () => {
      req.headers['authorization'] = 'Bearer invalid-token';
      
      optionalAuth(req, res, next);
      
      expect(req.user).toBeUndefined();
      expect(next).toHaveBeenCalled();
    });
  });

  describe('Edge Cases', () => {
    it('should handle malformed authorization header', async () => {
      req.headers['authorization'] = 'NotBearer token';
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(401);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Access token required'
      });
    });

    it('should handle empty authorization header', async () => {
      req.headers['authorization'] = '';
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(401);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Access token required'
      });
    });

    it('should handle exception during token verification', async () => {
      // Mock jwt.verify to throw an exception
      const originalVerify = jwt.verify;
      jwt.verify = jest.fn().mockImplementation(() => {
        throw new Error('Unexpected error');
      });
      
      req.headers['authorization'] = 'Bearer some-token';
      
      await authenticateToken(req, res, next);
      
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Authentication failed'
      });
      
      // Restore original function
      jwt.verify = originalVerify;
    });
  });
});