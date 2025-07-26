require('dotenv').config();

module.exports = {
  // Environment
  env: process.env.NODE_ENV || 'development',
  
  // Server configuration
  server: {
    port: process.env.PORT || 3000,
    host: process.env.HOST || '0.0.0.0'
  },
  
  // Mainframe configuration
  mainframe: {
    host: process.env.MAINFRAME_HOST || 'mainframe.company.com',
    sshPort: process.env.MAINFRAME_SSH_PORT || 22,
    username: process.env.MAINFRAME_USER,
    password: process.env.MAINFRAME_PASSWORD,
    keyPath: process.env.MAINFRAME_KEY_PATH
  },
  
  // DB2 configuration
  db2: {
    database: process.env.DB2_DATABASE || 'PRODDB',
    hostname: process.env.DB2_HOST || 'mainframe.company.com',
    port: process.env.DB2_PORT || 50000,
    uid: process.env.DB2_USER,
    pwd: process.env.DB2_PASSWORD,
    protocol: 'TCPIP',
    ssl: process.env.DB2_SSL === 'true',
    sslCertificate: process.env.DB2_SSL_CERT,
    poolSize: {
      min: parseInt(process.env.DB2_POOL_MIN) || 2,
      max: parseInt(process.env.DB2_POOL_MAX) || 10
    }
  },
  
  // Redis configuration
  redis: {
    url: process.env.REDIS_URL || 'redis://localhost:6379',
    password: process.env.REDIS_PASSWORD,
    db: parseInt(process.env.REDIS_DB) || 0,
    keyPrefix: 'cloudburst:'
  },
  
  // Cloud provider configuration
  cloud: {
    provider: process.env.CLOUD_PROVIDER || 'aws',
    multiCloud: process.env.MULTI_CLOUD === 'true',
    
    aws: {
      region: process.env.AWS_REGION || 'us-east-1',
      accessKeyId: process.env.AWS_ACCESS_KEY_ID,
      secretAccessKey: process.env.AWS_SECRET_ACCESS_KEY,
      instanceProfile: process.env.AWS_INSTANCE_PROFILE,
      vpcId: process.env.AWS_VPC_ID,
      subnetId: process.env.AWS_SUBNET_ID,
      securityGroupId: process.env.AWS_SECURITY_GROUP_ID,
      keyName: process.env.AWS_KEY_NAME || 'cobol-worker-key',
      imageId: process.env.AWS_IMAGE_ID || 'ami-cobol-worker'
    },
    
    azure: {
      subscriptionId: process.env.AZURE_SUBSCRIPTION_ID,
      tenantId: process.env.AZURE_TENANT_ID,
      clientId: process.env.AZURE_CLIENT_ID,
      clientSecret: process.env.AZURE_CLIENT_SECRET,
      resourceGroup: process.env.AZURE_RESOURCE_GROUP || 'rg-cobol-workers',
      location: process.env.AZURE_LOCATION || 'eastus',
      vnetName: process.env.AZURE_VNET_NAME,
      subnetName: process.env.AZURE_SUBNET_NAME,
      imageId: process.env.AZURE_IMAGE_ID
    },
    
    gcp: {
      projectId: process.env.GCP_PROJECT_ID,
      keyFile: process.env.GOOGLE_APPLICATION_CREDENTIALS,
      zone: process.env.GCP_ZONE || 'us-central1-a',
      network: process.env.GCP_NETWORK || 'default',
      subnetwork: process.env.GCP_SUBNETWORK,
      serviceAccount: process.env.GCP_SERVICE_ACCOUNT,
      imageId: process.env.GCP_IMAGE_ID
    }
  },
  
  // Scheduler configuration
  scheduler: {
    checkInterval: parseInt(process.env.SCHEDULER_CHECK_INTERVAL) || 30000,
    scaleThreshold: parseFloat(process.env.SCALE_THRESHOLD) || 0.8,
    scaleDownThreshold: parseFloat(process.env.SCALE_DOWN_THRESHOLD) || 0.3,
    maxCloudInstances: parseInt(process.env.MAX_CLOUD_INSTANCES) || 50,
    minCloudInstances: parseInt(process.env.MIN_CLOUD_INSTANCES) || 0,
    cooldownPeriod: parseInt(process.env.COOLDOWN_PERIOD) || 300000,
    useSpotInstances: process.env.USE_SPOT_INSTANCES === 'true',
    spotMaxPrice: process.env.SPOT_MAX_PRICE || '0.10'
  },
  
  // Job distribution configuration
  jobs: {
    maxRetries: parseInt(process.env.JOB_MAX_RETRIES) || 3,
    retryDelay: parseInt(process.env.JOB_RETRY_DELAY) || 5000,
    timeout: parseInt(process.env.JOB_TIMEOUT) || 300000,
    concurrentPerWorker: parseInt(process.env.JOBS_PER_WORKER) || 2
  },
  
  // Result merger configuration
  merger: {
    batchTimeout: parseInt(process.env.BATCH_TIMEOUT) || 60000,
    maxBatchSize: parseInt(process.env.MAX_BATCH_SIZE) || 100,
    mergeStrategy: process.env.MERGE_STRATEGY || 'sequential',
    tempDatasetPrefix: process.env.TEMP_DATASET_PREFIX || 'CLOUD.TEMP',
    finalDatasetPrefix: process.env.FINAL_DATASET_PREFIX || 'CLOUD.RESULTS',
    cleanupTempDatasets: process.env.CLEANUP_TEMP_DATASETS !== 'false'
  },
  
  // Monitoring configuration
  monitoring: {
    enabled: process.env.MONITORING_ENABLED !== 'false',
    metricsPort: parseInt(process.env.METRICS_PORT) || 9090,
    
    alerts: {
      email: process.env.ALERT_EMAIL,
      slack: {
        webhook: process.env.SLACK_WEBHOOK,
        channel: process.env.SLACK_CHANNEL || '#cloud-burst-alerts'
      },
      pagerduty: {
        key: process.env.PAGERDUTY_KEY,
        serviceId: process.env.PAGERDUTY_SERVICE_ID
      }
    },
    
    thresholds: {
      cpuHigh: parseInt(process.env.CPU_THRESHOLD_HIGH) || 85,
      memoryHigh: parseInt(process.env.MEMORY_THRESHOLD_HIGH) || 90,
      queueDepthHigh: parseInt(process.env.QUEUE_DEPTH_HIGH) || 100,
      instanceFailureRate: parseInt(process.env.INSTANCE_FAILURE_RATE) || 10
    }
  },
  
  // Security configuration
  security: {
    enableSSL: process.env.ENABLE_SSL === 'true',
    sslCert: process.env.SSL_CERT_PATH,
    sslKey: process.env.SSL_KEY_PATH,
    apiKey: process.env.API_KEY,
    jwtSecret: process.env.JWT_SECRET || 'cloudburst-secret-key',
    encryptData: process.env.ENCRYPT_DATA === 'true'
  },
  
  // Logging configuration
  logging: {
    level: process.env.LOG_LEVEL || 'info',
    format: process.env.LOG_FORMAT || 'json',
    maxFiles: parseInt(process.env.LOG_MAX_FILES) || 10,
    maxSize: process.env.LOG_MAX_SIZE || '10m'
  },
  
  // Feature flags
  features: {
    autoScaling: process.env.FEATURE_AUTO_SCALING !== 'false',
    costOptimization: process.env.FEATURE_COST_OPTIMIZATION !== 'false',
    multiCloud: process.env.FEATURE_MULTI_CLOUD === 'true',
    advancedScheduling: process.env.FEATURE_ADVANCED_SCHEDULING === 'true',
    predictiveScaling: process.env.FEATURE_PREDICTIVE_SCALING === 'true'
  }
};