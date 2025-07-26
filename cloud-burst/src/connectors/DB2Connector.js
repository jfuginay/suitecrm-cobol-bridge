const ibmdb = require('ibm_db');
const { Pool } = require('generic-pool');
const logger = require('../utils/logger');
const config = require('../config/config');

class DB2Connector {
  constructor(options = {}) {
    this.options = {
      database: process.env.DB2_DATABASE || 'PRODDB',
      hostname: process.env.DB2_HOST || 'mainframe.company.com',
      port: process.env.DB2_PORT || 50000,
      uid: process.env.DB2_USER,
      pwd: process.env.DB2_PASSWORD,
      protocol: 'TCPIP',
      poolSize: {
        min: 2,
        max: 10
      },
      connectionTimeout: 30000,
      idleTimeout: 600000, // 10 minutes
      ssl: process.env.DB2_SSL === 'true',
      sslServerCertificate: process.env.DB2_SSL_CERT,
      ...options
    };
    
    this.pool = null;
    this.isInitialized = false;
    this.metrics = {
      totalConnections: 0,
      activeConnections: 0,
      totalQueries: 0,
      failedQueries: 0,
      avgQueryTime: 0
    };
  }
  
  async initialize() {
    logger.info('Initializing DB2 Connector...');
    
    try {
      // Create connection pool
      this.pool = Pool({
        name: 'db2-pool',
        create: async () => {
          return await this._createConnection();
        },
        destroy: async (connection) => {
          await this._destroyConnection(connection);
        },
        validate: async (connection) => {
          return await this._validateConnection(connection);
        },
        max: this.options.poolSize.max,
        min: this.options.poolSize.min,
        idleTimeoutMillis: this.options.idleTimeout,
        acquireTimeoutMillis: this.options.connectionTimeout
      });
      
      // Test connection
      await this._testConnection();
      
      this.isInitialized = true;
      logger.info('DB2 Connector initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize DB2 Connector:', error);
      throw error;
    }
  }
  
  _buildConnectionString() {
    const {
      database,
      hostname,
      port,
      uid,
      pwd,
      protocol,
      ssl,
      sslServerCertificate
    } = this.options;
    
    let connectionString = `DATABASE=${database};HOSTNAME=${hostname};PORT=${port};PROTOCOL=${protocol};UID=${uid};PWD=${pwd};`;
    
    if (ssl) {
      connectionString += 'Security=SSL;';
      if (sslServerCertificate) {
        connectionString += `SSLServerCertificate=${sslServerCertificate};`;
      }
    }
    
    return connectionString;
  }
  
  async _createConnection() {
    const connectionString = this._buildConnectionString();
    
    return new Promise((resolve, reject) => {
      const startTime = Date.now();
      
      ibmdb.open(connectionString, (err, conn) => {
        if (err) {
          logger.error('Failed to create DB2 connection:', err);
          reject(err);
        } else {
          const connectionTime = Date.now() - startTime;
          logger.debug(`DB2 connection established in ${connectionTime}ms`);
          
          this.metrics.totalConnections++;
          this.metrics.activeConnections++;
          
          // Wrap connection with metadata
          const wrappedConn = {
            conn: conn,
            createdAt: new Date(),
            lastUsed: new Date(),
            queryCount: 0
          };
          
          resolve(wrappedConn);
        }
      });
    });
  }
  
  async _destroyConnection(wrappedConn) {
    try {
      await new Promise((resolve, reject) => {
        wrappedConn.conn.close((err) => {
          if (err) {
            reject(err);
          } else {
            resolve();
          }
        });
      });
      
      this.metrics.activeConnections--;
      logger.debug('DB2 connection closed');
    } catch (error) {
      logger.error('Error closing DB2 connection:', error);
    }
  }
  
  async _validateConnection(wrappedConn) {
    try {
      // Simple validation query
      await this._executeQuery(wrappedConn.conn, 'SELECT 1 FROM SYSIBM.SYSDUMMY1');
      return true;
    } catch (error) {
      logger.warn('Connection validation failed:', error);
      return false;
    }
  }
  
  async _testConnection() {
    const conn = await this.pool.acquire();
    
    try {
      const result = await this.query('SELECT CURRENT_TIMESTAMP FROM SYSIBM.SYSDUMMY1');
      logger.info('DB2 connection test successful:', result[0]);
    } finally {
      await this.pool.release(conn);
    }
  }
  
  async query(sql, params = []) {
    if (!this.isInitialized) {
      throw new Error('DB2 Connector not initialized');
    }
    
    const startTime = Date.now();
    let conn;
    
    try {
      conn = await this.pool.acquire();
      conn.lastUsed = new Date();
      conn.queryCount++;
      
      const result = await this._executeQuery(conn.conn, sql, params);
      
      const queryTime = Date.now() - startTime;
      this._updateMetrics(queryTime, true);
      
      logger.debug(`Query executed in ${queryTime}ms`);
      
      return result;
    } catch (error) {
      const queryTime = Date.now() - startTime;
      this._updateMetrics(queryTime, false);
      
      logger.error('Query failed:', error);
      throw error;
    } finally {
      if (conn) {
        await this.pool.release(conn);
      }
    }
  }
  
  async _executeQuery(conn, sql, params = []) {
    return new Promise((resolve, reject) => {
      if (params.length > 0) {
        // Prepared statement
        conn.prepare(sql, (err, stmt) => {
          if (err) {
            reject(err);
            return;
          }
          
          stmt.execute(params, (err, result, outparams) => {
            if (err) {
              reject(err);
            } else {
              // Fetch all results for SELECT queries
              if (sql.trim().toUpperCase().startsWith('SELECT')) {
                result.fetchAll((err, data) => {
                  stmt.close();
                  if (err) {
                    reject(err);
                  } else {
                    resolve(data);
                  }
                });
              } else {
                stmt.close();
                resolve(result);
              }
            }
          });
        });
      } else {
        // Direct query
        conn.query(sql, (err, data) => {
          if (err) {
            reject(err);
          } else {
            resolve(data);
          }
        });
      }
    });
  }
  
  async transaction(callback) {
    if (!this.isInitialized) {
      throw new Error('DB2 Connector not initialized');
    }
    
    const conn = await this.pool.acquire();
    
    try {
      // Begin transaction
      await this._executeQuery(conn.conn, 'BEGIN');
      
      // Execute callback with transaction connection
      const result = await callback({
        query: (sql, params) => this._executeQuery(conn.conn, sql, params),
        prepare: (sql) => this._prepareStatement(conn.conn, sql)
      });
      
      // Commit transaction
      await this._executeQuery(conn.conn, 'COMMIT');
      
      return result;
    } catch (error) {
      // Rollback transaction
      try {
        await this._executeQuery(conn.conn, 'ROLLBACK');
      } catch (rollbackError) {
        logger.error('Failed to rollback transaction:', rollbackError);
      }
      
      throw error;
    } finally {
      await this.pool.release(conn);
    }
  }
  
  async _prepareStatement(conn, sql) {
    return new Promise((resolve, reject) => {
      conn.prepare(sql, (err, stmt) => {
        if (err) {
          reject(err);
        } else {
          resolve({
            execute: (params) => new Promise((resolveExec, rejectExec) => {
              stmt.execute(params, (err, result) => {
                if (err) {
                  rejectExec(err);
                } else {
                  resolveExec(result);
                }
              });
            }),
            close: () => new Promise((resolveClose) => {
              stmt.close();
              resolveClose();
            })
          });
        }
      });
    });
  }
  
  async batchInsert(table, columns, rows) {
    if (rows.length === 0) {
      return;
    }
    
    const placeholders = columns.map(() => '?').join(', ');
    const sql = `INSERT INTO ${table} (${columns.join(', ')}) VALUES (${placeholders})`;
    
    const conn = await this.pool.acquire();
    
    try {
      const stmt = await this._prepareStatement(conn.conn, sql);
      
      for (const row of rows) {
        await stmt.execute(row);
      }
      
      await stmt.close();
      
      logger.info(`Batch inserted ${rows.length} rows into ${table}`);
    } finally {
      await this.pool.release(conn);
    }
  }
  
  async getDatasetInfo(datasetName) {
    const sql = `
      SELECT 
        NAME,
        DSORG,
        RECFM,
        LRECL,
        BLKSIZE,
        VOLSER,
        CREATION_DATE
      FROM SYSIBM.SYSCATALOG
      WHERE NAME = ?
    `;
    
    const results = await this.query(sql, [datasetName]);
    
    return results.length > 0 ? results[0] : null;
  }
  
  async readDataset(datasetName, options = {}) {
    const { limit = 1000, offset = 0 } = options;
    
    // This is a simplified example - actual implementation would use
    // appropriate DB2 utilities or file transfer mechanisms
    const sql = `
      SELECT RECORD_DATA
      FROM ${datasetName}
      ORDER BY RECORD_NUM
      OFFSET ? ROWS
      FETCH NEXT ? ROWS ONLY
    `;
    
    return await this.query(sql, [offset, limit]);
  }
  
  async writeDataset(datasetName, records) {
    // This would typically use DB2 LOAD utility or similar
    logger.info(`Writing ${records.length} records to dataset ${datasetName}`);
    
    return await this.batchInsert(datasetName, ['RECORD_DATA'], 
      records.map(r => [r])
    );
  }
  
  async callStoredProcedure(procedureName, params = []) {
    const placeholders = params.map(() => '?').join(', ');
    const sql = `CALL ${procedureName}(${placeholders})`;
    
    return await this.query(sql, params);
  }
  
  async getJobDataRequirements(jobId) {
    const sql = `
      SELECT 
        dr.DATASET_NAME,
        dr.ACCESS_TYPE,
        dr.RECORD_COUNT,
        ds.LRECL,
        ds.RECFM
      FROM SYSIBM.JOB_DATA_REQUIREMENTS dr
      JOIN SYSIBM.DATASETS ds ON dr.DATASET_NAME = ds.NAME
      WHERE dr.JOB_ID = ?
    `;
    
    return await this.query(sql, [jobId]);
  }
  
  async createTemporaryDataset(prefix, attributes = {}) {
    const tempName = `${prefix}.T${Date.now()}`;
    
    const sql = `
      CREATE TEMPORARY TABLE ${tempName} (
        RECORD_NUM INTEGER NOT NULL,
        RECORD_DATA VARCHAR(${attributes.lrecl || 80}),
        PRIMARY KEY (RECORD_NUM)
      )
    `;
    
    await this.query(sql);
    
    return tempName;
  }
  
  _updateMetrics(queryTime, success) {
    this.metrics.totalQueries++;
    
    if (!success) {
      this.metrics.failedQueries++;
    }
    
    // Update average query time
    const currentAvg = this.metrics.avgQueryTime;
    const totalQueries = this.metrics.totalQueries;
    this.metrics.avgQueryTime = ((currentAvg * (totalQueries - 1)) + queryTime) / totalQueries;
  }
  
  getMetrics() {
    return {
      ...this.metrics,
      poolStats: this.pool ? {
        size: this.pool.size,
        available: this.pool.available,
        borrowed: this.pool.borrowed,
        pending: this.pool.pending
      } : null
    };
  }
  
  async shutdown() {
    logger.info('Shutting down DB2 Connector...');
    
    if (this.pool) {
      await this.pool.drain();
      await this.pool.clear();
    }
    
    this.isInitialized = false;
    logger.info('DB2 Connector shutdown complete');
  }
}

// Singleton instance for shared use
let sharedConnector = null;

function getSharedConnector(options) {
  if (!sharedConnector) {
    sharedConnector = new DB2Connector(options);
  }
  return sharedConnector;
}

module.exports = { 
  DB2Connector,
  getSharedConnector
};