const WebSocket = require('ws');
const jwt = require('jsonwebtoken');
const logger = require('./logger');

// WebSocket connection handler
const setupWebSocket = (wss) => {
  // Track connections
  const connections = new Map();
  
  wss.on('connection', (ws, req) => {
    let userId = null;
    let connectionId = require('uuid').v4();
    
    logger.info('WebSocket connection established', { connectionId });
    
    // Authentication handler
    ws.on('message', async (message) => {
      try {
        const data = JSON.parse(message);
        
        // Handle authentication
        if (data.type === 'auth' && data.token) {
          try {
            const decoded = jwt.verify(data.token, process.env.JWT_SECRET || 'your-secret-key');
            userId = decoded.id;
            connections.set(connectionId, { ws, userId });
            
            ws.send(JSON.stringify({
              type: 'auth_success',
              userId,
              connectionId
            }));
            
            logger.info('WebSocket authenticated', { connectionId, userId });
          } catch (error) {
            ws.send(JSON.stringify({
              type: 'auth_error',
              error: 'Invalid token'
            }));
            ws.close();
          }
          return;
        }
        
        // Require authentication for other messages
        if (!userId) {
          ws.send(JSON.stringify({
            type: 'error',
            error: 'Not authenticated'
          }));
          return;
        }
        
        // Handle different message types
        switch (data.type) {
          case 'subscribe':
            handleSubscribe(ws, userId, data);
            break;
            
          case 'unsubscribe':
            handleUnsubscribe(ws, userId, data);
            break;
            
          case 'ping':
            ws.send(JSON.stringify({ type: 'pong' }));
            break;
            
          default:
            ws.send(JSON.stringify({
              type: 'error',
              error: 'Unknown message type'
            }));
        }
      } catch (error) {
        logger.error('WebSocket message error', error);
        ws.send(JSON.stringify({
          type: 'error',
          error: 'Invalid message format'
        }));
      }
    });
    
    // Error handler
    ws.on('error', (error) => {
      logger.error('WebSocket error', { connectionId, error: error.message });
    });
    
    // Close handler
    ws.on('close', () => {
      connections.delete(connectionId);
      logger.info('WebSocket connection closed', { connectionId, userId });
    });
    
    // Send initial connection message
    ws.send(JSON.stringify({
      type: 'connected',
      connectionId,
      requiresAuth: true
    }));
  });
  
  // Broadcast to specific users
  const broadcast = (userId, message) => {
    connections.forEach(({ ws, userId: connUserId }) => {
      if (connUserId === userId && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify(message));
      }
    });
  };
  
  // Broadcast to all authenticated users
  const broadcastAll = (message) => {
    connections.forEach(({ ws, userId }) => {
      if (userId && ws.readyState === WebSocket.OPEN) {
        ws.send(JSON.stringify(message));
      }
    });
  };
  
  return { broadcast, broadcastAll, connections };
};

// Handle subscription requests
const handleSubscribe = (ws, userId, data) => {
  const { channel } = data;
  
  // Store subscription info
  if (!ws.subscriptions) {
    ws.subscriptions = new Set();
  }
  ws.subscriptions.add(channel);
  
  ws.send(JSON.stringify({
    type: 'subscribed',
    channel
  }));
  
  logger.info('WebSocket subscription', { userId, channel });
};

// Handle unsubscribe requests
const handleUnsubscribe = (ws, userId, data) => {
  const { channel } = data;
  
  if (ws.subscriptions) {
    ws.subscriptions.delete(channel);
  }
  
  ws.send(JSON.stringify({
    type: 'unsubscribed',
    channel
  }));
  
  logger.info('WebSocket unsubscription', { userId, channel });
};

// Send real-time notifications
const sendNotification = (wss, userId, notification) => {
  wss.clients.forEach((client) => {
    if (client.readyState === WebSocket.OPEN && client.userId === userId) {
      client.send(JSON.stringify({
        type: 'notification',
        data: notification
      }));
    }
  });
};

// Send batch job updates
const sendJobUpdate = (wss, userId, jobId, status) => {
  wss.clients.forEach((client) => {
    if (client.readyState === WebSocket.OPEN && client.userId === userId) {
      client.send(JSON.stringify({
        type: 'job_update',
        jobId,
        status
      }));
    }
  });
};

module.exports = {
  setupWebSocket,
  sendNotification,
  sendJobUpdate
};