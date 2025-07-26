const admin = require('firebase-admin');
const { Expo } = require('expo-server-sdk');
const winston = require('winston');
const path = require('path');
const fs = require('fs').promises;

class PushNotifications {
  constructor() {
    this.expo = new Expo();
    this.firebaseApp = null;
    this.devices = new Map(); // In-memory device registry
    this.subscriptions = new Map(); // Topic subscriptions
    
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.simple(),
      transports: [new winston.transports.Console()]
    });
  }

  /**
   * Initialize push notification services
   */
  async initialize() {
    try {
      // Initialize Firebase Admin if credentials are available
      await this.initializeFirebase();
      
      // Load device registry from persistent storage
      await this.loadDeviceRegistry();
      
      this.logger.info('Push notification service initialized');
    } catch (error) {
      this.logger.error('Failed to initialize push notifications:', error);
    }
  }

  /**
   * Initialize Firebase Admin SDK
   */
  async initializeFirebase() {
    try {
      const serviceAccountPath = process.env.FIREBASE_SERVICE_ACCOUNT_PATH;
      
      if (serviceAccountPath) {
        const serviceAccount = JSON.parse(
          await fs.readFile(serviceAccountPath, 'utf8')
        );
        
        this.firebaseApp = admin.initializeApp({
          credential: admin.credential.cert(serviceAccount)
        });
        
        this.logger.info('Firebase Admin SDK initialized');
      } else {
        this.logger.warn('Firebase service account not configured');
      }
    } catch (error) {
      this.logger.error('Firebase initialization error:', error);
    }
  }

  /**
   * Load device registry from storage
   */
  async loadDeviceRegistry() {
    try {
      const registryPath = path.join(process.cwd(), 'data', 'device_registry.json');
      
      if (await this.fileExists(registryPath)) {
        const data = await fs.readFile(registryPath, 'utf8');
        const registry = JSON.parse(data);
        
        // Rebuild maps
        Object.entries(registry.devices || {}).forEach(([id, device]) => {
          this.devices.set(id, device);
        });
        
        Object.entries(registry.subscriptions || {}).forEach(([topic, devices]) => {
          this.subscriptions.set(topic, new Set(devices));
        });
        
        this.logger.info(`Loaded ${this.devices.size} registered devices`);
      }
    } catch (error) {
      this.logger.error('Failed to load device registry:', error);
    }
  }

  /**
   * Save device registry to storage
   */
  async saveDeviceRegistry() {
    try {
      const registryPath = path.join(process.cwd(), 'data', 'device_registry.json');
      await fs.mkdir(path.dirname(registryPath), { recursive: true });
      
      const registry = {
        devices: Object.fromEntries(this.devices),
        subscriptions: Object.fromEntries(
          Array.from(this.subscriptions.entries()).map(([topic, devices]) => [
            topic,
            Array.from(devices)
          ])
        )
      };
      
      await fs.writeFile(registryPath, JSON.stringify(registry, null, 2));
    } catch (error) {
      this.logger.error('Failed to save device registry:', error);
    }
  }

  /**
   * Register a device for push notifications
   * @param {string} token - Device push token
   * @param {string} platform - Platform (ios/android/expo)
   * @param {string} deviceId - Unique device ID
   * @param {string} userId - User ID
   */
  async registerDevice(token, platform, deviceId, userId) {
    try {
      // Validate token
      if (platform === 'expo' && !Expo.isExpoPushToken(token)) {
        throw new Error('Invalid Expo push token');
      }
      
      const device = {
        token,
        platform,
        deviceId,
        userId,
        registeredAt: new Date().toISOString(),
        lastSeen: new Date().toISOString()
      };
      
      this.devices.set(deviceId, device);
      
      // Subscribe to user topic
      this.subscribeToTopic(deviceId, `user:${userId}`);
      
      await this.saveDeviceRegistry();
      
      this.logger.info(`Registered device ${deviceId} for user ${userId}`);
      
      // Send welcome notification
      await this.sendWelcomeNotification(device);
      
      return device;
    } catch (error) {
      this.logger.error('Device registration error:', error);
      throw error;
    }
  }

  /**
   * Unregister a device
   * @param {string} deviceId - Device ID
   */
  async unregisterDevice(deviceId) {
    try {
      const device = this.devices.get(deviceId);
      if (device) {
        // Remove from all topics
        this.subscriptions.forEach((devices, topic) => {
          devices.delete(deviceId);
        });
        
        this.devices.delete(deviceId);
        await this.saveDeviceRegistry();
        
        this.logger.info(`Unregistered device ${deviceId}`);
      }
    } catch (error) {
      this.logger.error('Device unregistration error:', error);
      throw error;
    }
  }

  /**
   * Subscribe device to topic
   * @param {string} deviceId - Device ID
   * @param {string} topic - Topic name
   */
  subscribeToTopic(deviceId, topic) {
    if (!this.subscriptions.has(topic)) {
      this.subscriptions.set(topic, new Set());
    }
    this.subscriptions.get(topic).add(deviceId);
  }

  /**
   * Unsubscribe device from topic
   * @param {string} deviceId - Device ID
   * @param {string} topic - Topic name
   */
  unsubscribeFromTopic(deviceId, topic) {
    if (this.subscriptions.has(topic)) {
      this.subscriptions.get(topic).delete(deviceId);
    }
  }

  /**
   * Send notification to user
   * @param {string} userId - User ID
   * @param {object} notification - Notification data
   */
  async sendToUser(userId, notification) {
    const topic = `user:${userId}`;
    return this.sendToTopic(topic, notification);
  }

  /**
   * Send notification to topic
   * @param {string} topic - Topic name
   * @param {object} notification - Notification data
   */
  async sendToTopic(topic, notification) {
    const deviceIds = this.subscriptions.get(topic);
    
    if (!deviceIds || deviceIds.size === 0) {
      this.logger.warn(`No devices subscribed to topic: ${topic}`);
      return { sent: 0, failed: 0 };
    }
    
    const devices = Array.from(deviceIds)
      .map(id => this.devices.get(id))
      .filter(Boolean);
    
    return this.sendToDevices(devices, notification);
  }

  /**
   * Send notification to specific devices
   * @param {Array} devices - Array of device objects
   * @param {object} notification - Notification data
   */
  async sendToDevices(devices, notification) {
    const results = {
      sent: 0,
      failed: 0,
      errors: []
    };
    
    // Group by platform
    const platformGroups = this.groupDevicesByPlatform(devices);
    
    // Send to Expo devices
    if (platformGroups.expo.length > 0) {
      const expoResults = await this.sendExpoNotifications(
        platformGroups.expo,
        notification
      );
      results.sent += expoResults.sent;
      results.failed += expoResults.failed;
      results.errors.push(...expoResults.errors);
    }
    
    // Send to Firebase devices (iOS/Android)
    if (this.firebaseApp && (platformGroups.ios.length > 0 || platformGroups.android.length > 0)) {
      const firebaseResults = await this.sendFirebaseNotifications(
        [...platformGroups.ios, ...platformGroups.android],
        notification
      );
      results.sent += firebaseResults.sent;
      results.failed += firebaseResults.failed;
      results.errors.push(...firebaseResults.errors);
    }
    
    return results;
  }

  /**
   * Send Expo push notifications
   */
  async sendExpoNotifications(devices, notification) {
    const messages = devices.map(device => ({
      to: device.token,
      sound: 'default',
      title: notification.title,
      body: notification.body,
      data: notification.data || {},
      priority: notification.priority || 'high',
      channelId: 'default',
    }));
    
    const chunks = this.expo.chunkPushNotifications(messages);
    const results = { sent: 0, failed: 0, errors: [] };
    
    for (const chunk of chunks) {
      try {
        const tickets = await this.expo.sendPushNotificationsAsync(chunk);
        
        tickets.forEach((ticket, index) => {
          if (ticket.status === 'ok') {
            results.sent++;
          } else {
            results.failed++;
            results.errors.push({
              device: devices[index].deviceId,
              error: ticket.message
            });
            
            // Handle invalid tokens
            if (ticket.details?.error === 'DeviceNotRegistered') {
              this.unregisterDevice(devices[index].deviceId);
            }
          }
        });
      } catch (error) {
        this.logger.error('Expo notification error:', error);
        results.failed += chunk.length;
      }
    }
    
    return results;
  }

  /**
   * Send Firebase push notifications
   */
  async sendFirebaseNotifications(devices, notification) {
    const results = { sent: 0, failed: 0, errors: [] };
    
    if (!this.firebaseApp) {
      results.failed = devices.length;
      results.errors.push({ error: 'Firebase not configured' });
      return results;
    }
    
    const message = {
      notification: {
        title: notification.title,
        body: notification.body
      },
      data: notification.data || {},
      android: {
        priority: 'high',
        notification: {
          sound: 'default',
          clickAction: 'OPEN_ACTIVITY_1'
        }
      },
      apns: {
        payload: {
          aps: {
            sound: 'default',
            badge: notification.badge
          }
        }
      }
    };
    
    // Send to each device
    for (const device of devices) {
      try {
        await admin.messaging().send({
          ...message,
          token: device.token
        });
        results.sent++;
      } catch (error) {
        results.failed++;
        results.errors.push({
          device: device.deviceId,
          error: error.message
        });
        
        // Handle invalid tokens
        if (error.code === 'messaging/invalid-registration-token' ||
            error.code === 'messaging/registration-token-not-registered') {
          await this.unregisterDevice(device.deviceId);
        }
      }
    }
    
    return results;
  }

  /**
   * Send welcome notification
   */
  async sendWelcomeNotification(device) {
    const notification = {
      title: 'Welcome to SuiteCRM Mobile',
      body: 'Your device is now connected and ready to receive updates.',
      data: {
        type: 'welcome',
        timestamp: new Date().toISOString()
      }
    };
    
    await this.sendToDevices([device], notification);
  }

  /**
   * Send data sync notification
   */
  async sendSyncNotification(userId, syncResult) {
    const notification = {
      title: 'Data Sync Complete',
      body: `Synced ${syncResult.count} records successfully`,
      data: {
        type: 'sync_complete',
        ...syncResult
      }
    };
    
    await this.sendToUser(userId, notification);
  }

  /**
   * Send alert notification
   */
  async sendAlert(userId, alert) {
    const notification = {
      title: alert.title || 'System Alert',
      body: alert.message,
      data: {
        type: 'alert',
        severity: alert.severity || 'info',
        ...alert.data
      },
      priority: alert.severity === 'high' ? 'high' : 'normal'
    };
    
    await this.sendToUser(userId, notification);
  }

  /**
   * Group devices by platform
   */
  groupDevicesByPlatform(devices) {
    const groups = {
      expo: [],
      ios: [],
      android: []
    };
    
    devices.forEach(device => {
      if (device.platform === 'expo') {
        groups.expo.push(device);
      } else if (device.platform === 'ios') {
        groups.ios.push(device);
      } else if (device.platform === 'android') {
        groups.android.push(device);
      }
    });
    
    return groups;
  }

  /**
   * Update device last seen
   */
  updateDeviceActivity(deviceId) {
    const device = this.devices.get(deviceId);
    if (device) {
      device.lastSeen = new Date().toISOString();
      this.saveDeviceRegistry();
    }
  }

  /**
   * Clean up inactive devices
   */
  async cleanupInactiveDevices(daysInactive = 30) {
    const cutoffDate = new Date();
    cutoffDate.setDate(cutoffDate.getDate() - daysInactive);
    
    let removed = 0;
    
    for (const [deviceId, device] of this.devices) {
      const lastSeen = new Date(device.lastSeen);
      if (lastSeen < cutoffDate) {
        await this.unregisterDevice(deviceId);
        removed++;
      }
    }
    
    this.logger.info(`Removed ${removed} inactive devices`);
    return removed;
  }

  /**
   * Check if push notifications are configured
   */
  isConfigured() {
    return this.expo !== null || this.firebaseApp !== null;
  }

  /**
   * Get device statistics
   */
  getStatistics() {
    const stats = {
      totalDevices: this.devices.size,
      byPlatform: {},
      byUser: {},
      topics: this.subscriptions.size
    };
    
    for (const device of this.devices.values()) {
      // Count by platform
      stats.byPlatform[device.platform] = (stats.byPlatform[device.platform] || 0) + 1;
      
      // Count by user
      stats.byUser[device.userId] = (stats.byUser[device.userId] || 0) + 1;
    }
    
    return stats;
  }

  /**
   * Utility: Check if file exists
   */
  async fileExists(path) {
    try {
      await fs.access(path);
      return true;
    } catch {
      return false;
    }
  }
}

module.exports = PushNotifications;