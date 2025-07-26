const express = require('express');
const cors = require('cors');
const bodyParser = require('body-parser');
const helmet = require('helmet');
const compression = require('compression');
const rateLimit = require('express-rate-limit');
const http = require('http');
const socketIo = require('socket.io');
const winston = require('winston');
const jwt = require('jsonwebtoken');
require('dotenv').config();

// Import custom modules
const ReactNativeGenerator = require('./generators/ReactNativeGenerator');
const TypeScriptGenerator = require('./generators/TypeScriptGenerator');
const EBCDICConverter = require('./converters/EBCDICConverter');
const PICValidator = require('./validators/PICValidator');
const OfflineSync = require('./sync/OfflineSync');
const PushNotifications = require('./services/PushNotifications');

// Configure logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' }),
    new winston.transports.Console({
      format: winston.format.simple()
    })
  ]
});

// Initialize Express app
const app = express();
const server = http.createServer(app);
const io = socketIo(server, {
  cors: {
    origin: process.env.MOBILE_APP_URL || '*',
    methods: ['GET', 'POST']
  }
});

// Middleware
app.use(helmet());
app.use(compression());
app.use(cors({
  origin: process.env.MOBILE_APP_URL || '*',
  credentials: true
}));
app.use(bodyParser.json({ limit: '10mb' }));
app.use(bodyParser.urlencoded({ extended: true, limit: '10mb' }));

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100 // limit each IP to 100 requests per windowMs
});
app.use('/api/', limiter);

// Initialize services
const reactNativeGenerator = new ReactNativeGenerator();
const typeScriptGenerator = new TypeScriptGenerator();
const ebcdicConverter = new EBCDICConverter();
const picValidator = new PICValidator();
const offlineSync = new OfflineSync(io);
const pushNotifications = new PushNotifications();

// Authentication middleware
const authenticateToken = (req, res, next) => {
  const authHeader = req.headers['authorization'];
  const token = authHeader && authHeader.split(' ')[1];

  if (!token) {
    return res.status(401).json({ error: 'Access token required' });
  }

  jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key', (err, user) => {
    if (err) {
      return res.status(403).json({ error: 'Invalid token' });
    }
    req.user = user;
    next();
  });
};

// API Routes

// Health check
app.get('/health', (req, res) => {
  res.json({ 
    status: 'healthy', 
    timestamp: new Date().toISOString(),
    services: {
      database: offlineSync.isDatabaseConnected(),
      push: pushNotifications.isConfigured()
    }
  });
});

// Generate React Native components from COBOL screens
app.post('/api/generate/components', authenticateToken, async (req, res) => {
  try {
    const { cobolScreen, screenName, options = {} } = req.body;
    
    if (!cobolScreen || !screenName) {
      return res.status(400).json({ error: 'COBOL screen definition and screen name required' });
    }

    const component = await reactNativeGenerator.generateComponent(cobolScreen, screenName, options);
    
    logger.info(`Generated React Native component for screen: ${screenName}`);
    res.json({ 
      success: true, 
      component,
      screenName 
    });
  } catch (error) {
    logger.error('Component generation error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Generate TypeScript interfaces from COPYBOOK
app.post('/api/generate/types', authenticateToken, async (req, res) => {
  try {
    const { copybook, namespace = 'CobolTypes' } = req.body;
    
    if (!copybook) {
      return res.status(400).json({ error: 'COPYBOOK definition required' });
    }

    const types = await typeScriptGenerator.generateFromCopybook(copybook, namespace);
    
    logger.info(`Generated TypeScript interfaces for namespace: ${namespace}`);
    res.json({ 
      success: true, 
      types,
      namespace 
    });
  } catch (error) {
    logger.error('TypeScript generation error:', error);
    res.status(500).json({ error: error.message });
  }
});

// EBCDIC to UTF-8 conversion
app.post('/api/convert/ebcdic', authenticateToken, (req, res) => {
  try {
    const { data, encoding = 'IBM037' } = req.body;
    
    if (!data) {
      return res.status(400).json({ error: 'Data required for conversion' });
    }

    const converted = ebcdicConverter.toUTF8(data, encoding);
    
    res.json({ 
      success: true, 
      original: data,
      converted,
      encoding 
    });
  } catch (error) {
    logger.error('EBCDIC conversion error:', error);
    res.status(500).json({ error: error.message });
  }
});

// UTF-8 to EBCDIC conversion
app.post('/api/convert/utf8', authenticateToken, (req, res) => {
  try {
    const { data, encoding = 'IBM037' } = req.body;
    
    if (!data) {
      return res.status(400).json({ error: 'Data required for conversion' });
    }

    const converted = ebcdicConverter.toEBCDIC(data, encoding);
    
    res.json({ 
      success: true, 
      original: data,
      converted,
      encoding 
    });
  } catch (error) {
    logger.error('UTF-8 conversion error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Validate field against PIC clause
app.post('/api/validate/field', authenticateToken, (req, res) => {
  try {
    const { value, picClause, fieldName } = req.body;
    
    if (!picClause) {
      return res.status(400).json({ error: 'PIC clause required' });
    }

    const validation = picValidator.validate(value, picClause, fieldName);
    
    res.json({ 
      success: true, 
      ...validation
    });
  } catch (error) {
    logger.error('Validation error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Sync offline data
app.post('/api/sync/upload', authenticateToken, async (req, res) => {
  try {
    const { transactions, deviceId } = req.body;
    
    if (!transactions || !deviceId) {
      return res.status(400).json({ error: 'Transactions and device ID required' });
    }

    const result = await offlineSync.processTransactions(transactions, deviceId, req.user.id);
    
    logger.info(`Processed ${transactions.length} offline transactions from device ${deviceId}`);
    res.json({ 
      success: true, 
      ...result
    });
  } catch (error) {
    logger.error('Sync error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Get sync status
app.get('/api/sync/status/:deviceId', authenticateToken, async (req, res) => {
  try {
    const { deviceId } = req.params;
    const status = await offlineSync.getDeviceStatus(deviceId);
    
    res.json({ 
      success: true, 
      deviceId,
      ...status
    });
  } catch (error) {
    logger.error('Status error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Register device for push notifications
app.post('/api/push/register', authenticateToken, async (req, res) => {
  try {
    const { deviceToken, platform, deviceId } = req.body;
    
    if (!deviceToken || !platform || !deviceId) {
      return res.status(400).json({ error: 'Device token, platform, and device ID required' });
    }

    await pushNotifications.registerDevice(deviceToken, platform, deviceId, req.user.id);
    
    logger.info(`Registered device ${deviceId} for push notifications`);
    res.json({ 
      success: true,
      message: 'Device registered for push notifications'
    });
  } catch (error) {
    logger.error('Push registration error:', error);
    res.status(500).json({ error: error.message });
  }
});

// Send push notification
app.post('/api/push/send', authenticateToken, async (req, res) => {
  try {
    const { userId, title, body, data = {} } = req.body;
    
    if (!userId || !title || !body) {
      return res.status(400).json({ error: 'User ID, title, and body required' });
    }

    const result = await pushNotifications.sendToUser(userId, { title, body, data });
    
    logger.info(`Sent push notification to user ${userId}`);
    res.json({ 
      success: true,
      ...result
    });
  } catch (error) {
    logger.error('Push send error:', error);
    res.status(500).json({ error: error.message });
  }
});

// WebSocket connections for real-time updates
io.on('connection', (socket) => {
  logger.info('Mobile client connected:', socket.id);

  socket.on('authenticate', async (token) => {
    try {
      const decoded = jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key');
      socket.userId = decoded.id;
      socket.join(`user:${decoded.id}`);
      socket.emit('authenticated', { userId: decoded.id });
      logger.info(`Socket authenticated for user: ${decoded.id}`);
    } catch (error) {
      socket.emit('auth_error', { message: 'Invalid token' });
      socket.disconnect();
    }
  });

  socket.on('subscribe:updates', (data) => {
    if (!socket.userId) {
      socket.emit('error', { message: 'Not authenticated' });
      return;
    }
    
    const { entityType, entityId } = data;
    socket.join(`${entityType}:${entityId}`);
    logger.info(`User ${socket.userId} subscribed to ${entityType}:${entityId}`);
  });

  socket.on('sync:request', async (data) => {
    if (!socket.userId) {
      socket.emit('error', { message: 'Not authenticated' });
      return;
    }

    try {
      const updates = await offlineSync.getUpdatesForDevice(data.deviceId, data.lastSync);
      socket.emit('sync:updates', updates);
    } catch (error) {
      socket.emit('sync:error', { message: error.message });
    }
  });

  socket.on('disconnect', () => {
    logger.info('Mobile client disconnected:', socket.id);
  });
});

// Error handling middleware
app.use((err, req, res, next) => {
  logger.error('Unhandled error:', err);
  res.status(500).json({ 
    error: 'Internal server error',
    message: process.env.NODE_ENV === 'development' ? err.message : undefined
  });
});

// Start server
const PORT = process.env.PORT || 3002;
server.listen(PORT, () => {
  logger.info(`Mobile API server running on port ${PORT}`);
  logger.info(`WebSocket server ready for real-time connections`);
  
  // Initialize services
  offlineSync.initialize();
  pushNotifications.initialize();
});

module.exports = { app, io, server };