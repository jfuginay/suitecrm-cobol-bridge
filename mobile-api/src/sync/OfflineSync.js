const sqlite3 = require('sqlite3').verbose();
const path = require('path');
const fs = require('fs').promises;
const crypto = require('crypto');
const winston = require('winston');

class OfflineSync {
  constructor(io) {
    this.io = io;
    this.db = null;
    this.syncInterval = null;
    
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.simple(),
      transports: [new winston.transports.Console()]
    });
  }

  /**
   * Initialize offline sync database
   */
  async initialize() {
    try {
      const dbPath = path.join(process.cwd(), 'data', 'offline_sync.db');
      
      // Ensure data directory exists
      await fs.mkdir(path.dirname(dbPath), { recursive: true });
      
      this.db = new sqlite3.Database(dbPath);
      
      await this.createTables();
      
      // Start sync monitoring
      this.startSyncMonitor();
      
      this.logger.info('Offline sync initialized');
    } catch (error) {
      this.logger.error('Failed to initialize offline sync:', error);
      throw error;
    }
  }

  /**
   * Create database tables
   */
  createTables() {
    return new Promise((resolve, reject) => {
      this.db.serialize(() => {
        // Devices table
        this.db.run(`
          CREATE TABLE IF NOT EXISTS devices (
            id TEXT PRIMARY KEY,
            user_id TEXT NOT NULL,
            platform TEXT NOT NULL,
            last_sync DATETIME,
            sync_token TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            updated_at DATETIME DEFAULT CURRENT_TIMESTAMP
          )
        `);

        // Transactions table
        this.db.run(`
          CREATE TABLE IF NOT EXISTS transactions (
            id TEXT PRIMARY KEY,
            device_id TEXT NOT NULL,
            user_id TEXT NOT NULL,
            entity_type TEXT NOT NULL,
            entity_id TEXT,
            operation TEXT NOT NULL,
            data TEXT NOT NULL,
            status TEXT DEFAULT 'pending',
            conflict_resolution TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            synced_at DATETIME,
            error_message TEXT,
            retry_count INTEGER DEFAULT 0,
            FOREIGN KEY (device_id) REFERENCES devices(id)
          )
        `);

        // Sync conflicts table
        this.db.run(`
          CREATE TABLE IF NOT EXISTS sync_conflicts (
            id TEXT PRIMARY KEY,
            transaction_id TEXT NOT NULL,
            server_data TEXT,
            client_data TEXT,
            conflict_type TEXT,
            resolution_strategy TEXT,
            resolved BOOLEAN DEFAULT 0,
            resolved_at DATETIME,
            resolved_by TEXT,
            created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
            FOREIGN KEY (transaction_id) REFERENCES transactions(id)
          )
        `);

        // Entity versions table (for optimistic locking)
        this.db.run(`
          CREATE TABLE IF NOT EXISTS entity_versions (
            entity_type TEXT NOT NULL,
            entity_id TEXT NOT NULL,
            version INTEGER NOT NULL DEFAULT 1,
            last_modified DATETIME DEFAULT CURRENT_TIMESTAMP,
            last_modified_by TEXT,
            PRIMARY KEY (entity_type, entity_id)
          )
        `);

        // Create indexes
        this.db.run(`CREATE INDEX IF NOT EXISTS idx_transactions_device ON transactions(device_id)`);
        this.db.run(`CREATE INDEX IF NOT EXISTS idx_transactions_status ON transactions(status)`);
        this.db.run(`CREATE INDEX IF NOT EXISTS idx_transactions_created ON transactions(created_at)`);
        
        resolve();
      });
    });
  }

  /**
   * Process offline transactions from mobile device
   * @param {Array} transactions - Array of transactions
   * @param {string} deviceId - Device ID
   * @param {string} userId - User ID
   * @returns {object} Processing results
   */
  async processTransactions(transactions, deviceId, userId) {
    const results = {
      processed: 0,
      succeeded: 0,
      failed: 0,
      conflicts: 0,
      errors: []
    };

    // Update device sync info
    await this.updateDeviceSync(deviceId, userId);

    for (const transaction of transactions) {
      try {
        const transactionId = this.generateTransactionId();
        
        // Store transaction
        await this.storeTransaction({
          id: transactionId,
          deviceId,
          userId,
          ...transaction
        });

        // Process based on operation type
        const result = await this.processTransaction(transactionId, transaction);
        
        if (result.success) {
          results.succeeded++;
          await this.markTransactionSynced(transactionId);
          
          // Notify other devices
          this.notifySync(userId, transaction.entityType, transaction.entityId, result.data);
        } else if (result.conflict) {
          results.conflicts++;
          await this.handleConflict(transactionId, result);
        } else {
          results.failed++;
          results.errors.push({
            transactionId,
            error: result.error
          });
          await this.markTransactionFailed(transactionId, result.error);
        }
        
        results.processed++;
      } catch (error) {
        this.logger.error('Transaction processing error:', error);
        results.failed++;
        results.errors.push({
          transaction,
          error: error.message
        });
      }
    }

    return results;
  }

  /**
   * Process individual transaction
   */
  async processTransaction(transactionId, transaction) {
    try {
      const { entityType, entityId, operation, data, version } = transaction;
      
      // Check for conflicts (optimistic locking)
      if (operation === 'UPDATE' || operation === 'DELETE') {
        const currentVersion = await this.getEntityVersion(entityType, entityId);
        if (currentVersion && currentVersion > version) {
          const serverData = await this.getServerData(entityType, entityId);
          return {
            success: false,
            conflict: true,
            serverVersion: currentVersion,
            serverData,
            clientData: data
          };
        }
      }

      // Process operation
      let result;
      switch (operation) {
        case 'CREATE':
          result = await this.createEntity(entityType, data);
          break;
        case 'UPDATE':
          result = await this.updateEntity(entityType, entityId, data);
          break;
        case 'DELETE':
          result = await this.deleteEntity(entityType, entityId);
          break;
        case 'BATCH':
          result = await this.processBatch(data);
          break;
        default:
          throw new Error(`Unknown operation: ${operation}`);
      }

      // Update entity version
      if (result.success && (operation === 'CREATE' || operation === 'UPDATE')) {
        await this.updateEntityVersion(entityType, result.entityId || entityId);
      }

      return result;
    } catch (error) {
      return {
        success: false,
        error: error.message
      };
    }
  }

  /**
   * Handle sync conflict
   */
  async handleConflict(transactionId, conflictData) {
    const conflictId = this.generateTransactionId();
    
    return new Promise((resolve, reject) => {
      this.db.run(
        `INSERT INTO sync_conflicts (id, transaction_id, server_data, client_data, conflict_type)
         VALUES (?, ?, ?, ?, ?)`,
        [
          conflictId,
          transactionId,
          JSON.stringify(conflictData.serverData),
          JSON.stringify(conflictData.clientData),
          'VERSION_MISMATCH'
        ],
        (err) => {
          if (err) reject(err);
          else resolve(conflictId);
        }
      );
    });
  }

  /**
   * Get updates for device since last sync
   * @param {string} deviceId - Device ID
   * @param {Date} lastSync - Last sync timestamp
   * @returns {object} Updates
   */
  async getUpdatesForDevice(deviceId, lastSync) {
    try {
      const device = await this.getDevice(deviceId);
      if (!device) {
        throw new Error('Device not found');
      }

      // Get all entities modified since last sync
      const updates = await this.getModifiedEntities(lastSync || device.last_sync);
      
      // Generate sync token
      const syncToken = this.generateSyncToken();
      await this.updateDeviceSyncToken(deviceId, syncToken);

      return {
        updates,
        syncToken,
        serverTime: new Date().toISOString()
      };
    } catch (error) {
      this.logger.error('Error getting updates:', error);
      throw error;
    }
  }

  /**
   * Get device status
   */
  async getDeviceStatus(deviceId) {
    const device = await this.getDevice(deviceId);
    const pendingCount = await this.getPendingTransactionCount(deviceId);
    const lastTransaction = await this.getLastTransaction(deviceId);
    
    return {
      device,
      pendingTransactions: pendingCount,
      lastTransaction,
      isOnline: true,
      serverTime: new Date().toISOString()
    };
  }

  /**
   * Database helper methods
   */
  storeTransaction(transaction) {
    return new Promise((resolve, reject) => {
      this.db.run(
        `INSERT INTO transactions (id, device_id, user_id, entity_type, entity_id, operation, data)
         VALUES (?, ?, ?, ?, ?, ?, ?)`,
        [
          transaction.id,
          transaction.deviceId,
          transaction.userId,
          transaction.entityType,
          transaction.entityId,
          transaction.operation,
          JSON.stringify(transaction.data)
        ],
        (err) => {
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  markTransactionSynced(transactionId) {
    return new Promise((resolve, reject) => {
      this.db.run(
        `UPDATE transactions SET status = 'synced', synced_at = CURRENT_TIMESTAMP WHERE id = ?`,
        [transactionId],
        (err) => {
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  markTransactionFailed(transactionId, error) {
    return new Promise((resolve, reject) => {
      this.db.run(
        `UPDATE transactions 
         SET status = 'failed', 
             error_message = ?, 
             retry_count = retry_count + 1 
         WHERE id = ?`,
        [error, transactionId],
        (err) => {
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  getDevice(deviceId) {
    return new Promise((resolve, reject) => {
      this.db.get(
        `SELECT * FROM devices WHERE id = ?`,
        [deviceId],
        (err, row) => {
          if (err) reject(err);
          else resolve(row);
        }
      );
    });
  }

  updateDeviceSync(deviceId, userId) {
    return new Promise((resolve, reject) => {
      this.db.run(
        `INSERT OR REPLACE INTO devices (id, user_id, platform, last_sync)
         VALUES (?, ?, ?, CURRENT_TIMESTAMP)`,
        [deviceId, userId, 'mobile'],
        (err) => {
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  getEntityVersion(entityType, entityId) {
    return new Promise((resolve, reject) => {
      this.db.get(
        `SELECT version FROM entity_versions WHERE entity_type = ? AND entity_id = ?`,
        [entityType, entityId],
        (err, row) => {
          if (err) reject(err);
          else resolve(row ? row.version : null);
        }
      );
    });
  }

  updateEntityVersion(entityType, entityId, userId = 'system') {
    return new Promise((resolve, reject) => {
      this.db.run(
        `INSERT OR REPLACE INTO entity_versions (entity_type, entity_id, version, last_modified_by)
         VALUES (?, ?, 
           COALESCE((SELECT version + 1 FROM entity_versions WHERE entity_type = ? AND entity_id = ?), 1),
           ?)`,
        [entityType, entityId, entityType, entityId, userId],
        (err) => {
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  getPendingTransactionCount(deviceId) {
    return new Promise((resolve, reject) => {
      this.db.get(
        `SELECT COUNT(*) as count FROM transactions WHERE device_id = ? AND status = 'pending'`,
        [deviceId],
        (err, row) => {
          if (err) reject(err);
          else resolve(row ? row.count : 0);
        }
      );
    });
  }

  getLastTransaction(deviceId) {
    return new Promise((resolve, reject) => {
      this.db.get(
        `SELECT * FROM transactions WHERE device_id = ? ORDER BY created_at DESC LIMIT 1`,
        [deviceId],
        (err, row) => {
          if (err) reject(err);
          else resolve(row);
        }
      );
    });
  }

  /**
   * Entity operations (implement based on your backend)
   */
  async createEntity(entityType, data) {
    // Implement actual creation logic
    // This is a placeholder
    return {
      success: true,
      entityId: this.generateTransactionId(),
      data
    };
  }

  async updateEntity(entityType, entityId, data) {
    // Implement actual update logic
    return {
      success: true,
      data
    };
  }

  async deleteEntity(entityType, entityId) {
    // Implement actual deletion logic
    return {
      success: true
    };
  }

  async processBatch(batchData) {
    // Implement batch processing
    return {
      success: true,
      processed: batchData.length
    };
  }

  async getServerData(entityType, entityId) {
    // Implement fetching server data
    return {};
  }

  async getModifiedEntities(since) {
    // Implement fetching modified entities
    return [];
  }

  /**
   * Real-time sync notifications
   */
  notifySync(userId, entityType, entityId, data) {
    if (this.io) {
      // Notify user's devices
      this.io.to(`user:${userId}`).emit('sync:update', {
        entityType,
        entityId,
        data,
        timestamp: new Date().toISOString()
      });

      // Notify entity subscribers
      this.io.to(`${entityType}:${entityId}`).emit('entity:update', {
        entityType,
        entityId,
        data,
        timestamp: new Date().toISOString()
      });
    }
  }

  /**
   * Sync monitoring
   */
  startSyncMonitor() {
    // Check for failed transactions every 5 minutes
    this.syncInterval = setInterval(() => {
      this.retryFailedTransactions();
    }, 5 * 60 * 1000);
  }

  async retryFailedTransactions() {
    try {
      const failed = await this.getFailedTransactions();
      
      for (const transaction of failed) {
        if (transaction.retry_count < 3) {
          await this.processTransaction(transaction.id, JSON.parse(transaction.data));
        }
      }
    } catch (error) {
      this.logger.error('Retry failed transactions error:', error);
    }
  }

  getFailedTransactions() {
    return new Promise((resolve, reject) => {
      this.db.all(
        `SELECT * FROM transactions 
         WHERE status = 'failed' AND retry_count < 3
         ORDER BY created_at ASC
         LIMIT 10`,
        (err, rows) => {
          if (err) reject(err);
          else resolve(rows || []);
        }
      );
    });
  }

  /**
   * Utility methods
   */
  generateTransactionId() {
    return crypto.randomBytes(16).toString('hex');
  }

  generateSyncToken() {
    return crypto.randomBytes(32).toString('base64');
  }

  updateDeviceSyncToken(deviceId, syncToken) {
    return new Promise((resolve, reject) => {
      this.db.run(
        `UPDATE devices SET sync_token = ? WHERE id = ?`,
        [syncToken, deviceId],
        (err) => {
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  /**
   * Check if database is connected
   */
  isDatabaseConnected() {
    return this.db !== null;
  }

  /**
   * Close database connection
   */
  close() {
    if (this.syncInterval) {
      clearInterval(this.syncInterval);
    }
    
    if (this.db) {
      this.db.close();
    }
  }
}

module.exports = OfflineSync;