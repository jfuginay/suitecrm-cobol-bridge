module.exports = {
  aws: {
    regions: {
      'us-east-1': {
        name: 'US East (N. Virginia)',
        endpoint: 'ec2.us-east-1.amazonaws.com',
        availabilityZones: ['us-east-1a', 'us-east-1b', 'us-east-1c', 'us-east-1d', 'us-east-1e', 'us-east-1f']
      },
      'us-west-2': {
        name: 'US West (Oregon)',
        endpoint: 'ec2.us-west-2.amazonaws.com',
        availabilityZones: ['us-west-2a', 'us-west-2b', 'us-west-2c', 'us-west-2d']
      },
      'eu-west-1': {
        name: 'EU (Ireland)',
        endpoint: 'ec2.eu-west-1.amazonaws.com',
        availabilityZones: ['eu-west-1a', 'eu-west-1b', 'eu-west-1c']
      }
    },
    instanceTypes: {
      't3.medium': {
        vcpu: 2,
        memory: 4,
        network: 'Up to 5 Gigabit',
        price: {
          onDemand: 0.0416,
          spot: 0.0125
        }
      },
      't3.large': {
        vcpu: 2,
        memory: 8,
        network: 'Up to 5 Gigabit',
        price: {
          onDemand: 0.0832,
          spot: 0.025
        }
      },
      't3.xlarge': {
        vcpu: 4,
        memory: 16,
        network: 'Up to 5 Gigabit',
        price: {
          onDemand: 0.1664,
          spot: 0.05
        }
      },
      't3.2xlarge': {
        vcpu: 8,
        memory: 32,
        network: 'Up to 5 Gigabit',
        price: {
          onDemand: 0.3328,
          spot: 0.10
        }
      },
      'c5.large': {
        vcpu: 2,
        memory: 4,
        network: 'Up to 10 Gigabit',
        price: {
          onDemand: 0.085,
          spot: 0.0255
        }
      },
      'c5.xlarge': {
        vcpu: 4,
        memory: 8,
        network: 'Up to 10 Gigabit',
        price: {
          onDemand: 0.17,
          spot: 0.051
        }
      },
      'c5.2xlarge': {
        vcpu: 8,
        memory: 16,
        network: 'Up to 10 Gigabit',
        price: {
          onDemand: 0.34,
          spot: 0.102
        }
      },
      'm5.large': {
        vcpu: 2,
        memory: 8,
        network: 'Up to 10 Gigabit',
        price: {
          onDemand: 0.096,
          spot: 0.0288
        }
      },
      'm5.xlarge': {
        vcpu: 4,
        memory: 16,
        network: 'Up to 10 Gigabit',
        price: {
          onDemand: 0.192,
          spot: 0.0576
        }
      }
    },
    services: {
      ec2: {
        quotas: {
          maxInstances: 100,
          maxSpotInstances: 50
        }
      },
      vpc: {
        defaultCIDR: '10.0.0.0/16'
      }
    }
  },
  
  azure: {
    regions: {
      'eastus': {
        name: 'East US',
        displayName: 'East US',
        regionalDisplayName: '(US) East US'
      },
      'westus2': {
        name: 'West US 2',
        displayName: 'West US 2',
        regionalDisplayName: '(US) West US 2'
      },
      'westeurope': {
        name: 'West Europe',
        displayName: 'West Europe',
        regionalDisplayName: '(Europe) West Europe'
      }
    },
    instanceTypes: {
      'Standard_B2s': {
        vcpu: 2,
        memory: 4,
        tempStorage: 8,
        price: {
          payAsYouGo: 0.0416,
          spot: 0.00832
        }
      },
      'Standard_B2ms': {
        vcpu: 2,
        memory: 8,
        tempStorage: 16,
        price: {
          payAsYouGo: 0.0832,
          spot: 0.01664
        }
      },
      'Standard_B4ms': {
        vcpu: 4,
        memory: 16,
        tempStorage: 32,
        price: {
          payAsYouGo: 0.1664,
          spot: 0.03328
        }
      },
      'Standard_D2s_v3': {
        vcpu: 2,
        memory: 8,
        tempStorage: 16,
        price: {
          payAsYouGo: 0.096,
          spot: 0.0192
        }
      },
      'Standard_D4s_v3': {
        vcpu: 4,
        memory: 16,
        tempStorage: 32,
        price: {
          payAsYouGo: 0.192,
          spot: 0.0384
        }
      },
      'Standard_D8s_v3': {
        vcpu: 8,
        memory: 32,
        tempStorage: 64,
        price: {
          payAsYouGo: 0.384,
          spot: 0.0768
        }
      }
    },
    services: {
      compute: {
        quotas: {
          maxCores: 200,
          maxSpotCores: 100
        }
      },
      network: {
        defaultAddressSpace: '10.0.0.0/16'
      }
    }
  },
  
  gcp: {
    regions: {
      'us-central1': {
        name: 'Iowa',
        zones: ['us-central1-a', 'us-central1-b', 'us-central1-c', 'us-central1-f']
      },
      'us-east1': {
        name: 'South Carolina',
        zones: ['us-east1-b', 'us-east1-c', 'us-east1-d']
      },
      'europe-west1': {
        name: 'Belgium',
        zones: ['europe-west1-b', 'europe-west1-c', 'europe-west1-d']
      }
    },
    machineTypes: {
      'n1-standard-1': {
        vcpu: 1,
        memory: 3.75,
        price: {
          onDemand: 0.0475,
          preemptible: 0.01
        }
      },
      'n1-standard-2': {
        vcpu: 2,
        memory: 7.5,
        price: {
          onDemand: 0.095,
          preemptible: 0.02
        }
      },
      'n1-standard-4': {
        vcpu: 4,
        memory: 15,
        price: {
          onDemand: 0.19,
          preemptible: 0.04
        }
      },
      'n1-standard-8': {
        vcpu: 8,
        memory: 30,
        price: {
          onDemand: 0.38,
          preemptible: 0.08
        }
      },
      'n2-standard-2': {
        vcpu: 2,
        memory: 8,
        price: {
          onDemand: 0.0971,
          preemptible: 0.0225
        }
      },
      'n2-standard-4': {
        vcpu: 4,
        memory: 16,
        price: {
          onDemand: 0.1942,
          preemptible: 0.045
        }
      },
      'n2-standard-8': {
        vcpu: 8,
        memory: 32,
        price: {
          onDemand: 0.3885,
          preemptible: 0.09
        }
      }
    },
    services: {
      compute: {
        quotas: {
          maxInstances: 100,
          maxPreemptibleInstances: 50
        }
      },
      network: {
        defaultSubnet: '10.0.0.0/20'
      }
    }
  },
  
  // Common configuration across providers
  common: {
    // Mapping of generic instance types to provider-specific
    instanceTypeMapping: {
      small: {
        aws: 't3.medium',
        azure: 'Standard_B2s',
        gcp: 'n1-standard-2'
      },
      medium: {
        aws: 't3.large',
        azure: 'Standard_B2ms',
        gcp: 'n1-standard-4'
      },
      large: {
        aws: 't3.xlarge',
        azure: 'Standard_B4ms',
        gcp: 'n1-standard-8'
      },
      xlarge: {
        aws: 't3.2xlarge',
        azure: 'Standard_D8s_v3',
        gcp: 'n2-standard-8'
      }
    },
    
    // Network configuration
    network: {
      allowedPorts: [22, 8080, 8443, 50000], // SSH, HTTP, HTTPS, DB2
      protocols: ['tcp', 'udp']
    },
    
    // Security configuration
    security: {
      enableEncryption: true,
      sshKeyAlgorithm: 'rsa',
      sshKeySize: 4096
    },
    
    // Tagging strategy
    tags: {
      required: ['Application', 'Environment', 'Owner', 'CostCenter'],
      optional: ['Project', 'Team', 'ExpirationDate']
    }
  },
  
  // Cost optimization settings
  costOptimization: {
    // Spot/Preemptible instance usage
    spotStrategy: {
      enabled: true,
      maxPercentage: 80,
      fallbackToOnDemand: true,
      bidStrategy: 'capacity-optimized'
    },
    
    // Auto-scaling policies
    autoScaling: {
      minInstances: 0,
      maxInstances: 100,
      targetUtilization: 70,
      scaleUpThreshold: 80,
      scaleDownThreshold: 30,
      cooldownPeriod: 300
    },
    
    // Instance right-sizing
    rightSizing: {
      enabled: true,
      checkInterval: 3600, // 1 hour
      cpuThreshold: {
        underutilized: 20,
        overutilized: 90
      },
      memoryThreshold: {
        underutilized: 30,
        overutilized: 85
      }
    },
    
    // Scheduled scaling
    scheduledScaling: {
      enabled: true,
      schedules: [
        {
          name: 'business-hours',
          cron: '0 8 * * 1-5', // 8 AM Mon-Fri
          minInstances: 5,
          maxInstances: 100
        },
        {
          name: 'after-hours',
          cron: '0 18 * * 1-5', // 6 PM Mon-Fri
          minInstances: 1,
          maxInstances: 20
        },
        {
          name: 'weekend',
          cron: '0 0 * * 0,6', // Midnight Sat-Sun
          minInstances: 0,
          maxInstances: 10
        }
      ]
    }
  },
  
  // Monitoring and alerting
  monitoring: {
    metrics: {
      cpu: {
        period: 60,
        statistic: 'Average',
        threshold: 80
      },
      memory: {
        period: 60,
        statistic: 'Average',
        threshold: 85
      },
      disk: {
        period: 300,
        statistic: 'Average',
        threshold: 90
      }
    },
    
    alerts: {
      email: process.env.ALERT_EMAIL || 'ops@company.com',
      slack: process.env.SLACK_WEBHOOK,
      pagerduty: process.env.PAGERDUTY_KEY
    }
  }
};