const EventEmitter = require('events');
const { EC2Client, RunInstancesCommand, TerminateInstancesCommand, DescribeInstancesCommand } = require('@aws-sdk/client-ec2');
const { ComputeManagementClient } = require('@azure/arm-compute');
const { DefaultAzureCredential } = require('@azure/identity');
const { Compute } = require('@google-cloud/compute');
const Docker = require('dockerode');
const logger = require('../utils/logger');
const config = require('../config/config');

class InstanceManager extends EventEmitter {
  constructor(options = {}) {
    super();
    
    this.options = {
      provider: process.env.CLOUD_PROVIDER || 'aws', // aws, azure, gcp, docker
      region: process.env.CLOUD_REGION || 'us-east-1',
      instanceType: process.env.INSTANCE_TYPE || 't3.large',
      imageId: process.env.COBOL_IMAGE_ID || 'ami-cobol-worker',
      securityGroup: process.env.SECURITY_GROUP || 'sg-cobol-workers',
      subnet: process.env.SUBNET_ID,
      keyName: process.env.KEY_NAME || 'cobol-worker-key',
      useSpotInstances: process.env.USE_SPOT_INSTANCES === 'true',
      spotMaxPrice: process.env.SPOT_MAX_PRICE || '0.10',
      tags: {
        Application: 'SuiteCRM-COBOL-Bridge',
        Component: 'CloudBurst',
        ManagedBy: 'InstanceManager'
      },
      ...options
    };
    
    this.instances = new Map();
    this.providers = {};
    this.isInitialized = false;
    
    // Initialize cloud providers
    this._initializeProviders();
  }
  
  _initializeProviders() {
    // AWS Provider
    if (this.options.provider === 'aws' || this.options.provider === 'multi') {
      this.providers.aws = {
        client: new EC2Client({ region: this.options.region }),
        type: 'aws'
      };
    }
    
    // Azure Provider
    if (this.options.provider === 'azure' || this.options.provider === 'multi') {
      const credential = new DefaultAzureCredential();
      this.providers.azure = {
        client: new ComputeManagementClient(
          credential,
          process.env.AZURE_SUBSCRIPTION_ID
        ),
        type: 'azure',
        resourceGroup: process.env.AZURE_RESOURCE_GROUP || 'rg-cobol-workers'
      };
    }
    
    // GCP Provider
    if (this.options.provider === 'gcp' || this.options.provider === 'multi') {
      this.providers.gcp = {
        client: new Compute(),
        type: 'gcp',
        zone: process.env.GCP_ZONE || 'us-central1-a'
      };
    }
    
    // Docker Provider (for local testing)
    if (this.options.provider === 'docker') {
      this.providers.docker = {
        client: new Docker(),
        type: 'docker'
      };
    }
  }
  
  async initialize() {
    logger.info('Initializing Instance Manager...');
    
    try {
      // Verify provider connectivity
      await this._verifyProviders();
      
      // Load existing instances
      await this._loadExistingInstances();
      
      this.isInitialized = true;
      logger.info('Instance Manager initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize Instance Manager:', error);
      throw error;
    }
  }
  
  async _verifyProviders() {
    const verifications = [];
    
    for (const [name, provider] of Object.entries(this.providers)) {
      verifications.push(this._verifyProvider(name, provider));
    }
    
    const results = await Promise.allSettled(verifications);
    
    const failed = results.filter(r => r.status === 'rejected');
    if (failed.length > 0) {
      logger.warn(`${failed.length} provider(s) failed verification`);
    }
  }
  
  async _verifyProvider(name, provider) {
    try {
      switch (provider.type) {
        case 'aws':
          await provider.client.send(new DescribeInstancesCommand({
            MaxResults: 1
          }));
          break;
          
        case 'azure':
          await provider.client.virtualMachines.list(provider.resourceGroup);
          break;
          
        case 'gcp':
          const zone = await provider.client.zone(provider.zone);
          await zone.getVMs({ maxResults: 1 });
          break;
          
        case 'docker':
          await provider.client.ping();
          break;
      }
      
      logger.info(`Provider ${name} verified successfully`);
    } catch (error) {
      logger.error(`Provider ${name} verification failed:`, error);
      throw error;
    }
  }
  
  async _loadExistingInstances() {
    logger.info('Loading existing instances...');
    
    const loadPromises = [];
    
    for (const [name, provider] of Object.entries(this.providers)) {
      loadPromises.push(this._loadProviderInstances(name, provider));
    }
    
    await Promise.all(loadPromises);
    
    logger.info(`Loaded ${this.instances.size} existing instances`);
  }
  
  async _loadProviderInstances(name, provider) {
    try {
      let instances = [];
      
      switch (provider.type) {
        case 'aws':
          instances = await this._loadAWSInstances(provider);
          break;
          
        case 'azure':
          instances = await this._loadAzureInstances(provider);
          break;
          
        case 'gcp':
          instances = await this._loadGCPInstances(provider);
          break;
          
        case 'docker':
          instances = await this._loadDockerInstances(provider);
          break;
      }
      
      instances.forEach(instance => {
        this.instances.set(instance.id, instance);
      });
    } catch (error) {
      logger.error(`Failed to load instances from ${name}:`, error);
    }
  }
  
  async _loadAWSInstances(provider) {
    const command = new DescribeInstancesCommand({
      Filters: [
        {
          Name: 'tag:Application',
          Values: ['SuiteCRM-COBOL-Bridge']
        },
        {
          Name: 'instance-state-name',
          Values: ['pending', 'running']
        }
      ]
    });
    
    const response = await provider.client.send(command);
    const instances = [];
    
    for (const reservation of response.Reservations || []) {
      for (const instance of reservation.Instances || []) {
        instances.push({
          id: instance.InstanceId,
          provider: 'aws',
          type: instance.InstanceType,
          state: instance.State.Name,
          privateIp: instance.PrivateIpAddress,
          publicIp: instance.PublicIpAddress,
          launchTime: instance.LaunchTime,
          spotInstance: instance.InstanceLifecycle === 'spot',
          metadata: {
            availabilityZone: instance.Placement.AvailabilityZone,
            subnetId: instance.SubnetId
          }
        });
      }
    }
    
    return instances;
  }
  
  async _loadAzureInstances(provider) {
    const vms = [];
    
    for await (const vm of provider.client.virtualMachines.list(provider.resourceGroup)) {
      if (vm.tags && vm.tags.Application === 'SuiteCRM-COBOL-Bridge') {
        vms.push({
          id: vm.vmId,
          provider: 'azure',
          type: vm.hardwareProfile.vmSize,
          state: vm.provisioningState,
          privateIp: vm.networkProfile.networkInterfaces[0].privateIPAddress,
          publicIp: vm.networkProfile.networkInterfaces[0].publicIPAddress,
          launchTime: vm.timeCreated,
          spotInstance: vm.priority === 'Spot',
          metadata: {
            location: vm.location,
            resourceGroup: provider.resourceGroup
          }
        });
      }
    }
    
    return vms;
  }
  
  async _loadGCPInstances(provider) {
    const zone = provider.client.zone(provider.zone);
    const [vms] = await zone.getVMs({
      filter: 'labels.application="suitecrm-cobol-bridge"'
    });
    
    return vms.map(vm => ({
      id: vm.id,
      provider: 'gcp',
      type: vm.metadata.machineType.split('/').pop(),
      state: vm.metadata.status,
      privateIp: vm.metadata.networkInterfaces[0].networkIP,
      publicIp: vm.metadata.networkInterfaces[0].accessConfigs?.[0]?.natIP,
      launchTime: new Date(vm.metadata.creationTimestamp),
      spotInstance: vm.metadata.scheduling?.preemptible || false,
      metadata: {
        zone: provider.zone,
        project: vm.metadata.project
      }
    }));
  }
  
  async _loadDockerInstances(provider) {
    const containers = await provider.client.listContainers({
      filters: {
        label: ['application=suitecrm-cobol-bridge']
      }
    });
    
    return containers.map(container => ({
      id: container.Id,
      provider: 'docker',
      type: 'container',
      state: container.State,
      privateIp: '127.0.0.1',
      publicIp: '127.0.0.1',
      launchTime: new Date(container.Created * 1000),
      spotInstance: false,
      metadata: {
        image: container.Image,
        ports: container.Ports
      }
    }));
  }
  
  async scaleUp(count) {
    logger.info(`Scaling up: launching ${count} new instances`);
    
    const launchPromises = [];
    const provider = this._selectProvider();
    
    for (let i = 0; i < count; i++) {
      launchPromises.push(this._launchInstance(provider));
    }
    
    const results = await Promise.allSettled(launchPromises);
    
    const successful = results
      .filter(r => r.status === 'fulfilled')
      .map(r => r.value);
    
    const failed = results.filter(r => r.status === 'rejected');
    
    if (failed.length > 0) {
      logger.error(`Failed to launch ${failed.length} instances`);
    }
    
    logger.info(`Successfully launched ${successful.length} instances`);
    
    return successful;
  }
  
  async _launchInstance(provider) {
    try {
      let instance;
      
      switch (provider.type) {
        case 'aws':
          instance = await this._launchAWSInstance(provider);
          break;
          
        case 'azure':
          instance = await this._launchAzureInstance(provider);
          break;
          
        case 'gcp':
          instance = await this._launchGCPInstance(provider);
          break;
          
        case 'docker':
          instance = await this._launchDockerInstance(provider);
          break;
          
        default:
          throw new Error(`Unknown provider type: ${provider.type}`);
      }
      
      // Register instance
      this.instances.set(instance.id, instance);
      
      // Setup instance
      await this._setupInstance(instance);
      
      // Emit event
      this.emit('instance_launched', instance);
      
      // Wait for instance to be ready
      await this._waitForInstanceReady(instance);
      
      this.emit('instance_ready', instance);
      
      return instance;
    } catch (error) {
      logger.error('Failed to launch instance:', error);
      throw error;
    }
  }
  
  async _launchAWSInstance(provider) {
    const params = {
      ImageId: this.options.imageId,
      InstanceType: this.options.instanceType,
      MinCount: 1,
      MaxCount: 1,
      SecurityGroupIds: [this.options.securityGroup],
      SubnetId: this.options.subnet,
      KeyName: this.options.keyName,
      TagSpecifications: [
        {
          ResourceType: 'instance',
          Tags: Object.entries(this.options.tags).map(([Key, Value]) => ({ Key, Value }))
        }
      ],
      UserData: Buffer.from(this._getUserData()).toString('base64')
    };
    
    // Configure spot instance if enabled
    if (this.options.useSpotInstances) {
      params.InstanceMarketOptions = {
        MarketType: 'spot',
        SpotOptions: {
          MaxPrice: this.options.spotMaxPrice,
          SpotInstanceType: 'one-time'
        }
      };
    }
    
    const command = new RunInstancesCommand(params);
    const response = await provider.client.send(command);
    
    const awsInstance = response.Instances[0];
    
    return {
      id: awsInstance.InstanceId,
      provider: 'aws',
      type: awsInstance.InstanceType,
      state: 'launching',
      privateIp: awsInstance.PrivateIpAddress,
      publicIp: awsInstance.PublicIpAddress,
      launchTime: new Date(),
      spotInstance: this.options.useSpotInstances,
      metadata: {
        availabilityZone: awsInstance.Placement.AvailabilityZone,
        subnetId: awsInstance.SubnetId
      }
    };
  }
  
  async _launchAzureInstance(provider) {
    const vmName = `cobol-worker-${Date.now()}`;
    
    const parameters = {
      location: process.env.AZURE_LOCATION || 'eastus',
      osProfile: {
        computerName: vmName,
        adminUsername: 'coboladmin',
        adminPassword: process.env.AZURE_VM_PASSWORD,
        customData: Buffer.from(this._getUserData()).toString('base64')
      },
      hardwareProfile: {
        vmSize: this.options.instanceType
      },
      storageProfile: {
        imageReference: {
          id: this.options.imageId
        },
        osDisk: {
          createOption: 'FromImage',
          managedDisk: {
            storageAccountType: 'StandardSSD_LRS'
          }
        }
      },
      networkProfile: {
        networkInterfaces: [
          {
            id: `/subscriptions/${process.env.AZURE_SUBSCRIPTION_ID}/resourceGroups/${provider.resourceGroup}/providers/Microsoft.Network/networkInterfaces/${vmName}-nic`
          }
        ]
      },
      tags: this.options.tags
    };
    
    if (this.options.useSpotInstances) {
      parameters.priority = 'Spot';
      parameters.evictionPolicy = 'Deallocate';
      parameters.billingProfile = {
        maxPrice: parseFloat(this.options.spotMaxPrice)
      };
    }
    
    const vm = await provider.client.virtualMachines.beginCreateOrUpdate(
      provider.resourceGroup,
      vmName,
      parameters
    );
    
    return {
      id: vm.vmId,
      provider: 'azure',
      type: parameters.hardwareProfile.vmSize,
      state: 'launching',
      privateIp: null, // Will be updated when ready
      publicIp: null,
      launchTime: new Date(),
      spotInstance: this.options.useSpotInstances,
      metadata: {
        location: parameters.location,
        resourceGroup: provider.resourceGroup,
        name: vmName
      }
    };
  }
  
  async _launchGCPInstance(provider) {
    const zone = provider.client.zone(provider.zone);
    const name = `cobol-worker-${Date.now()}`;
    
    const config = {
      name: name,
      machineType: `zones/${provider.zone}/machineTypes/${this.options.instanceType}`,
      disks: [
        {
          boot: true,
          initializeParams: {
            sourceImage: this.options.imageId
          }
        }
      ],
      networkInterfaces: [
        {
          network: 'global/networks/default',
          accessConfigs: [
            {
              type: 'ONE_TO_ONE_NAT',
              name: 'External NAT'
            }
          ]
        }
      ],
      metadata: {
        items: [
          {
            key: 'startup-script',
            value: this._getUserData()
          }
        ]
      },
      labels: {
        application: 'suitecrm-cobol-bridge',
        component: 'cloudburst'
      }
    };
    
    if (this.options.useSpotInstances) {
      config.scheduling = {
        preemptible: true,
        onHostMaintenance: 'TERMINATE',
        automaticRestart: false
      };
    }
    
    const [vm, operation] = await zone.createVM(name, config);
    await operation.promise();
    
    return {
      id: vm.id,
      provider: 'gcp',
      type: this.options.instanceType,
      state: 'launching',
      privateIp: vm.metadata.networkInterfaces[0].networkIP,
      publicIp: vm.metadata.networkInterfaces[0].accessConfigs?.[0]?.natIP,
      launchTime: new Date(),
      spotInstance: this.options.useSpotInstances,
      metadata: {
        zone: provider.zone,
        name: name
      }
    };
  }
  
  async _launchDockerInstance(provider) {
    const container = await provider.client.createContainer({
      Image: this.options.imageId || 'cobol-worker:latest',
      name: `cobol-worker-${Date.now()}`,
      Labels: {
        application: 'suitecrm-cobol-bridge',
        component: 'cloudburst'
      },
      Env: [
        `MAINFRAME_HOST=${process.env.MAINFRAME_HOST}`,
        `DB2_HOST=${process.env.DB2_HOST}`,
        `WORKER_ID=${Date.now()}`
      ],
      HostConfig: {
        AutoRemove: false,
        RestartPolicy: {
          Name: 'unless-stopped'
        }
      }
    });
    
    await container.start();
    
    const info = await container.inspect();
    
    return {
      id: info.Id,
      provider: 'docker',
      type: 'container',
      state: 'running',
      privateIp: info.NetworkSettings.IPAddress,
      publicIp: 'localhost',
      launchTime: new Date(),
      spotInstance: false,
      metadata: {
        image: info.Config.Image,
        ports: info.NetworkSettings.Ports
      }
    };
  }
  
  _getUserData() {
    return `#!/bin/bash
# COBOL Worker Setup Script

# Update system
apt-get update -y

# Install dependencies
apt-get install -y gnucobol libdb2 nodejs npm git

# Clone worker code
git clone https://github.com/company/cobol-worker.git /opt/cobol-worker

# Install Node.js dependencies
cd /opt/cobol-worker
npm install

# Configure worker
cat > /opt/cobol-worker/.env << EOF
MAINFRAME_HOST=${process.env.MAINFRAME_HOST}
DB2_HOST=${process.env.DB2_HOST}
DB2_PORT=${process.env.DB2_PORT}
DB2_DATABASE=${process.env.DB2_DATABASE}
WORKER_TYPE=cloud
CLOUD_PROVIDER=${this.options.provider}
EOF

# Start worker service
npm start
`;
  }
  
  async _setupInstance(instance) {
    logger.info(`Setting up instance ${instance.id}`);
    
    // Wait for network connectivity
    await this._waitForNetwork(instance);
    
    // Configure monitoring
    await this._setupMonitoring(instance);
    
    // Register with scheduler
    await this._registerInstance(instance);
  }
  
  async _waitForInstanceReady(instance) {
    const maxAttempts = 30;
    const delayMs = 10000; // 10 seconds
    
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      try {
        const ready = await this._checkInstanceReady(instance);
        if (ready) {
          logger.info(`Instance ${instance.id} is ready`);
          return;
        }
      } catch (error) {
        logger.debug(`Instance ${instance.id} not ready yet (attempt ${attempt})`);
      }
      
      if (attempt < maxAttempts) {
        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
    }
    
    throw new Error(`Instance ${instance.id} failed to become ready`);
  }
  
  async _checkInstanceReady(instance) {
    // Provider-specific readiness checks
    switch (instance.provider) {
      case 'aws':
        return await this._checkAWSInstanceReady(instance);
      case 'azure':
        return await this._checkAzureInstanceReady(instance);
      case 'gcp':
        return await this._checkGCPInstanceReady(instance);
      case 'docker':
        return await this._checkDockerInstanceReady(instance);
      default:
        return false;
    }
  }
  
  async _checkAWSInstanceReady(instance) {
    const command = new DescribeInstancesCommand({
      InstanceIds: [instance.id]
    });
    
    const response = await this.providers.aws.client.send(command);
    const awsInstance = response.Reservations[0]?.Instances[0];
    
    if (awsInstance && awsInstance.State.Name === 'running') {
      // Update instance info
      instance.state = 'running';
      instance.privateIp = awsInstance.PrivateIpAddress;
      instance.publicIp = awsInstance.PublicIpAddress;
      
      // Check if worker service is responding
      return await this._checkWorkerService(instance);
    }
    
    return false;
  }
  
  async _checkWorkerService(instance) {
    try {
      const response = await fetch(`http://${instance.privateIp}:8080/health`);
      return response.ok;
    } catch {
      return false;
    }
  }
  
  async scaleDown(count) {
    logger.info(`Scaling down: terminating ${count} instances`);
    
    // Get idle instances
    const idleInstances = await this.getIdleInstances();
    const instancesToTerminate = idleInstances.slice(0, count);
    
    if (instancesToTerminate.length === 0) {
      logger.warn('No idle instances to terminate');
      return;
    }
    
    const terminatePromises = instancesToTerminate.map(instance => 
      this._terminateInstance(instance)
    );
    
    const results = await Promise.allSettled(terminatePromises);
    
    const successful = results.filter(r => r.status === 'fulfilled').length;
    const failed = results.filter(r => r.status === 'rejected').length;
    
    if (failed > 0) {
      logger.error(`Failed to terminate ${failed} instances`);
    }
    
    logger.info(`Successfully terminated ${successful} instances`);
  }
  
  async _terminateInstance(instance) {
    logger.info(`Terminating instance ${instance.id}`);
    
    try {
      // Notify instance of termination
      await this._notifyTermination(instance);
      
      // Provider-specific termination
      switch (instance.provider) {
        case 'aws':
          await this._terminateAWSInstance(instance);
          break;
        case 'azure':
          await this._terminateAzureInstance(instance);
          break;
        case 'gcp':
          await this._terminateGCPInstance(instance);
          break;
        case 'docker':
          await this._terminateDockerInstance(instance);
          break;
      }
      
      // Remove from tracking
      this.instances.delete(instance.id);
      
      // Emit event
      this.emit('instance_terminated', instance);
    } catch (error) {
      logger.error(`Failed to terminate instance ${instance.id}:`, error);
      throw error;
    }
  }
  
  async _terminateAWSInstance(instance) {
    const command = new TerminateInstancesCommand({
      InstanceIds: [instance.id]
    });
    
    await this.providers.aws.client.send(command);
  }
  
  async getActiveInstanceCount() {
    return Array.from(this.instances.values())
      .filter(i => i.state === 'running')
      .length;
  }
  
  async getIdleInstances() {
    const instances = Array.from(this.instances.values())
      .filter(i => i.state === 'running');
    
    const idleInstances = [];
    
    for (const instance of instances) {
      const isIdle = await this._checkInstanceIdle(instance);
      if (isIdle) {
        idleInstances.push(instance);
      }
    }
    
    return idleInstances;
  }
  
  async _checkInstanceIdle(instance) {
    try {
      const response = await fetch(`http://${instance.privateIp}:8080/status`);
      const status = await response.json();
      return status.activeJobs === 0;
    } catch {
      return true; // Assume idle if can't connect
    }
  }
  
  async getAllInstances() {
    return Array.from(this.instances.values());
  }
  
  async switchInstanceType(instanceId, newType) {
    logger.info(`Switching instance ${instanceId} to type ${newType}`);
    
    const instance = this.instances.get(instanceId);
    if (!instance) {
      throw new Error(`Instance ${instanceId} not found`);
    }
    
    // Stop instance
    await this._stopInstance(instance);
    
    // Change instance type
    await this._changeInstanceType(instance, newType);
    
    // Start instance
    await this._startInstance(instance);
    
    // Wait for ready
    await this._waitForInstanceReady(instance);
  }
  
  async enableSpotInstances(percentage) {
    logger.info(`Enabling spot instances for ${percentage}% of fleet`);
    
    this.options.useSpotInstances = true;
    this.options.spotPercentage = percentage;
    
    // Future scale operations will use spot instances
  }
  
  async shutdown() {
    logger.info('Shutting down Instance Manager...');
    
    // Terminate all instances
    const allInstances = Array.from(this.instances.values());
    
    for (const instance of allInstances) {
      try {
        await this._terminateInstance(instance);
      } catch (error) {
        logger.error(`Failed to terminate instance ${instance.id}:`, error);
      }
    }
    
    logger.info('Instance Manager shutdown complete');
  }
  
  _selectProvider() {
    // Simple round-robin for now
    const providers = Object.values(this.providers);
    return providers[Math.floor(Math.random() * providers.length)];
  }
  
  async _waitForNetwork(instance) {
    // Implementation depends on networking setup
    logger.debug(`Waiting for network connectivity for instance ${instance.id}`);
  }
  
  async _setupMonitoring(instance) {
    // Setup CloudWatch/Azure Monitor/Stackdriver
    logger.debug(`Setting up monitoring for instance ${instance.id}`);
  }
  
  async _registerInstance(instance) {
    // Register with service discovery
    logger.debug(`Registering instance ${instance.id} with scheduler`);
  }
  
  async _notifyTermination(instance) {
    try {
      await fetch(`http://${instance.privateIp}:8080/shutdown`, { method: 'POST' });
    } catch {
      // Instance may already be down
    }
  }
}

module.exports = { InstanceManager };