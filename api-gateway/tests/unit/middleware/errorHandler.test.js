const { errorHandler } = require('../../../middleware/errorHandler'); 

// Mock logger
jest.mock('../../../utils/logger', () => ({
  error: jest.fn()
}));

describe('Error Handler Middleware', () => {
  let req, res, next;

  beforeEach(() => {
    req = {
      method: 'POST',
      url: '/api/v1/test',
      headers: {},
      body: {}
    };
    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn(),
      headersSent: false
    };
    next = jest.fn();
    
    jest.clearAllMocks();
  });

  it('should handle validation errors (400)', () => {
    const error = {
      name: 'ValidationError',
      message: 'Invalid input data',
      details: [
        { field: 'email', message: 'Invalid email format' },
        { field: 'age', message: 'Must be a positive number' }
      ]
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Validation Error',
      message: 'Invalid input data',
      details: error.details,
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle authentication errors (401)', () => {
    const error = {
      name: 'AuthenticationError',
      message: 'Invalid token provided'
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Authentication Error',
      message: 'Invalid token provided',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle authorization errors (403)', () => {
    const error = {
      name: 'AuthorizationError',
      message: 'Insufficient permissions'
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(403);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Authorization Error',
      message: 'Insufficient permissions',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle not found errors (404)', () => {
    const error = {
      name: 'NotFoundError',
      message: 'Resource not found'
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Not Found',
      message: 'Resource not found',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle rate limit errors (429)', () => {
    const error = {
      name: 'RateLimitError',
      message: 'Too many requests',
      retryAfter: 60
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(429);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Rate Limit Exceeded',
      message: 'Too many requests',
      retryAfter: 60,
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle COBOL execution errors (502)', () => {
    const error = {
      name: 'CobolExecutionError',
      message: 'COBOL program failed to execute',
      programName: 'CREDIT01',
      exitCode: 1
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(502);
    expect(res.json).toHaveBeenCalledWith({
      error: 'COBOL Execution Error',
      message: 'COBOL program failed to execute',
      programName: 'CREDIT01',
      exitCode: 1,
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle generic errors (500)', () => {
    const error = new Error('Unexpected server error');

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Internal Server Error',
      message: 'An unexpected error occurred',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should include stack trace in development environment', () => {
    const originalEnv = process.env.NODE_ENV;
    process.env.NODE_ENV = 'development';

    const error = new Error('Test error');
    error.stack = 'Error: Test error\n    at test.js:1:1';

    errorHandler(error, req, res, next);

    expect(res.json).toHaveBeenCalledWith(
      expect.objectContaining({
        stack: error.stack
      })
    );

    process.env.NODE_ENV = originalEnv;
  });

  it('should not include stack trace in production environment', () => {
    const originalEnv = process.env.NODE_ENV;
    process.env.NODE_ENV = 'production';

    const error = new Error('Test error');
    error.stack = 'Error: Test error\n    at test.js:1:1';

    errorHandler(error, req, res, next);

    expect(res.json).toHaveBeenCalledWith(
      expect.not.objectContaining({
        stack: expect.any(String)
      })
    );

    process.env.NODE_ENV = originalEnv;
  });

  it('should not send response if headers already sent', () => {
    res.headersSent = true;
    const error = new Error('Test error');

    errorHandler(error, req, res, next);

    expect(res.status).not.toHaveBeenCalled();
    expect(res.json).not.toHaveBeenCalled();
    expect(next).toHaveBeenCalledWith(error);
  });

  it('should handle errors with custom status codes', () => {
    const error = new Error('Custom error');
    error.statusCode = 418; // I'm a teapot
    error.status = 418;

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(418);
    expect(res.json).toHaveBeenCalledWith(
      expect.objectContaining({
        error: 'Custom error',
        message: 'Custom error'
      })
    );
  });

  it('should handle MongoDB duplicate key errors', () => {
    const error = {
      name: 'MongoError',
      code: 11000,
      message: 'Duplicate key error',
      keyPattern: { email: 1 },
      keyValue: { email: 'test@example.com' }
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(409);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Duplicate Key Error',
      message: 'Resource already exists',
      field: 'email',
      value: 'test@example.com',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle JSON Web Token errors', () => {
    const error = {
      name: 'JsonWebTokenError',
      message: 'invalid token'
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Invalid Token',
      message: 'The provided token is invalid',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle token expired errors', () => {
    const error = {
      name: 'TokenExpiredError',
      message: 'jwt expired',
      expiredAt: new Date()
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(401);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Token Expired',
      message: 'Your session has expired. Please login again.',
      expiredAt: error.expiredAt,
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle Redis connection errors', () => {
    const error = {
      name: 'RedisError',
      message: 'Connection lost',
      code: 'ECONNREFUSED'
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(503);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Service Unavailable',
      message: 'Cache service is temporarily unavailable',
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should handle timeout errors', () => {
    const error = {
      name: 'TimeoutError',
      message: 'Operation timed out',
      timeout: 30000
    };

    errorHandler(error, req, res, next);

    expect(res.status).toHaveBeenCalledWith(408);
    expect(res.json).toHaveBeenCalledWith({
      error: 'Request Timeout',
      message: 'The operation took too long to complete',
      timeout: 30000,
      timestamp: expect.any(String),
      path: '/api/v1/test'
    });
  });

  it('should log errors with appropriate level', () => {
    const logger = require('../../../utils/logger');
    const error = new Error('Test error');

    errorHandler(error, req, res, next);

    expect(logger.error).toHaveBeenCalledWith('Unhandled error', {
      error: error.message,
      stack: error.stack,
      method: req.method,
      url: req.url,
      userAgent: req.headers['user-agent'],
      timestamp: expect.any(String)
    });
  });

  it('should include request ID if available', () => {
    req.id = 'req-123-456';
    const error = new Error('Test error');

    errorHandler(error, req, res, next);

    expect(res.json).toHaveBeenCalledWith(
      expect.objectContaining({
        requestId: 'req-123-456'
      })
    );
  });

  it('should handle circular JSON references in error objects', () => {
    const error = new Error('Circular reference error');
    error.circular = error; // Create circular reference

    expect(() => {
      errorHandler(error, req, res, next);
    }).not.toThrow();

    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalled();
  });

  it('should sanitize sensitive information from error messages', () => {
    const error = new Error('Database connection failed: password=secret123');

    errorHandler(error, req, res, next);

    expect(res.json).toHaveBeenCalledWith(
      expect.objectContaining({
        message: 'An unexpected error occurred' // Should not expose sensitive info
      })
    );
  });
});