const cloudProviders = require('../config/cloudProviders');
const logger = require('./logger');

class CostOptimizer {
  constructor(options = {}) {
    this.options = {
      analysisInterval: 3600000, // 1 hour
      recommendationThreshold: 0.1, // 10% potential savings
      lookbackPeriod: 604800000, // 7 days
      ...options
    };
    
    this.costHistory = [];
    this.recommendations = [];
    this.lastAnalysis = null;
  }
  
  async analyze(data) {
    const { instances, jobs, costs } = data;
    
    logger.info('Running cost optimization analysis...');
    
    const analysis = {
      timestamp: new Date(),
      currentCosts: await this._calculateCurrentCosts(instances),
      projectedCosts: await this._projectCosts(instances, jobs),
      utilizationMetrics: await this._analyzeUtilization(instances, jobs),
      recommendations: []
    };
    
    // Generate recommendations based on analysis
    analysis.recommendations = await this._generateRecommendations(analysis);
    
    // Store analysis results
    this.lastAnalysis = analysis;
    this.recommendations = analysis.recommendations;
    
    // Track cost history
    this._trackCostHistory(analysis.currentCosts);
    
    logger.info(`Cost analysis complete. Found ${analysis.recommendations.length} recommendations`);
    
    return analysis;
  }
  
  async _calculateCurrentCosts(instances) {
    const costs = {
      total: 0,
      byProvider: {},
      byInstanceType: {},
      byPricingModel: {
        onDemand: 0,
        spot: 0,
        reserved: 0
      }
    };
    
    for (const instance of instances) {
      const hourlyRate = this._getInstanceHourlyRate(instance);
      const runningHours = this._calculateRunningHours(instance);
      const instanceCost = hourlyRate * runningHours;
      
      // Update totals
      costs.total += instanceCost;
      
      // By provider
      if (!costs.byProvider[instance.provider]) {
        costs.byProvider[instance.provider] = 0;
      }
      costs.byProvider[instance.provider] += instanceCost;
      
      // By instance type
      if (!costs.byInstanceType[instance.type]) {
        costs.byInstanceType[instance.type] = 0;
      }
      costs.byInstanceType[instance.type] += instanceCost;
      
      // By pricing model
      if (instance.spotInstance) {
        costs.byPricingModel.spot += instanceCost;
      } else {
        costs.byPricingModel.onDemand += instanceCost;
      }
    }
    
    return costs;
  }
  
  _getInstanceHourlyRate(instance) {
    const provider = instance.provider;
    const instanceType = instance.type;
    
    let rate = 0;
    
    switch (provider) {
      case 'aws':
        const awsType = cloudProviders.aws.instanceTypes[instanceType];
        if (awsType) {
          rate = instance.spotInstance ? awsType.price.spot : awsType.price.onDemand;
        }
        break;
        
      case 'azure':
        const azureType = cloudProviders.azure.instanceTypes[instanceType];
        if (azureType) {
          rate = instance.spotInstance ? azureType.price.spot : azureType.price.payAsYouGo;
        }
        break;
        
      case 'gcp':
        const gcpType = cloudProviders.gcp.machineTypes[instanceType];
        if (gcpType) {
          rate = instance.spotInstance ? gcpType.price.preemptible : gcpType.price.onDemand;
        }
        break;
        
      case 'docker':
        rate = 0; // Local Docker is free
        break;
    }
    
    return rate;
  }
  
  _calculateRunningHours(instance) {
    const now = Date.now();
    const launchTime = new Date(instance.launchTime).getTime();
    const runningMs = now - launchTime;
    return runningMs / (1000 * 60 * 60); // Convert to hours
  }
  
  async _projectCosts(instances, jobs) {
    // Project costs for next 24 hours based on current usage patterns
    const projections = {
      next24Hours: 0,
      next7Days: 0,
      next30Days: 0,
      breakdown: {
        compute: 0,
        storage: 0,
        network: 0
      }
    };
    
    // Calculate average job processing rate
    const avgJobsPerHour = this._calculateAverageJobRate(jobs);
    const avgInstancesNeeded = Math.ceil(avgJobsPerHour / 10); // Assume 10 jobs per instance per hour
    
    // Project compute costs
    const avgHourlyRate = this._calculateAverageHourlyRate(instances);
    projections.next24Hours = avgInstancesNeeded * avgHourlyRate * 24;
    projections.next7Days = projections.next24Hours * 7;
    projections.next30Days = projections.next24Hours * 30;
    
    projections.breakdown.compute = projections.next24Hours * 0.8; // 80% compute
    projections.breakdown.storage = projections.next24Hours * 0.1; // 10% storage
    projections.breakdown.network = projections.next24Hours * 0.1; // 10% network
    
    return projections;
  }
  
  _calculateAverageJobRate(jobs) {
    const completedJobs = jobs.completed || 0;
    const timeWindow = 24; // hours
    return completedJobs / timeWindow;
  }
  
  _calculateAverageHourlyRate(instances) {
    if (instances.length === 0) return 0;
    
    const totalRate = instances.reduce((sum, instance) => {
      return sum + this._getInstanceHourlyRate(instance);
    }, 0);
    
    return totalRate / instances.length;
  }
  
  async _analyzeUtilization(instances, jobs) {
    const utilization = {
      overall: 0,
      byInstance: [],
      underutilized: [],
      overutilized: []
    };
    
    for (const instance of instances) {
      const metrics = await this._getInstanceMetrics(instance);
      
      const instanceUtil = {
        id: instance.id,
        type: instance.type,
        provider: instance.provider,
        cpu: metrics.cpu || 0,
        memory: metrics.memory || 0,
        activeJobs: metrics.activeJobs || 0,
        utilization: (metrics.cpu + metrics.memory) / 2
      };
      
      utilization.byInstance.push(instanceUtil);
      
      if (instanceUtil.utilization < 30) {
        utilization.underutilized.push(instanceUtil);
      } else if (instanceUtil.utilization > 80) {
        utilization.overutilized.push(instanceUtil);
      }
    }
    
    // Calculate overall utilization
    if (utilization.byInstance.length > 0) {
      utilization.overall = utilization.byInstance.reduce((sum, i) => sum + i.utilization, 0) / 
                           utilization.byInstance.length;
    }
    
    return utilization;
  }
  
  async _getInstanceMetrics(instance) {
    // In production, this would fetch real metrics from CloudWatch/Azure Monitor/Stackdriver
    // For now, return simulated metrics
    return {
      cpu: Math.random() * 100,
      memory: Math.random() * 100,
      activeJobs: Math.floor(Math.random() * 5)
    };
  }
  
  async _generateRecommendations(analysis) {
    const recommendations = [];
    
    // 1. Spot instance recommendations
    const spotRecommendation = this._analyzeSpotOpportunities(analysis);
    if (spotRecommendation) {
      recommendations.push(spotRecommendation);
    }
    
    // 2. Right-sizing recommendations
    const rightSizingRecs = this._analyzeRightSizing(analysis);
    recommendations.push(...rightSizingRecs);
    
    // 3. Consolidation recommendations
    const consolidationRecs = this._analyzeConsolidation(analysis);
    recommendations.push(...consolidationRecs);
    
    // 4. Scheduling recommendations
    const schedulingRecs = this._analyzeScheduling(analysis);
    recommendations.push(...schedulingRecs);
    
    // 5. Provider optimization
    const providerRecs = this._analyzeProviderOptimization(analysis);
    recommendations.push(...providerRecs);
    
    // Sort by potential savings
    recommendations.sort((a, b) => b.potentialSavings - a.potentialSavings);
    
    return recommendations;
  }
  
  _analyzeSpotOpportunities(analysis) {
    const spotPercentage = analysis.currentCosts.byPricingModel.spot / analysis.currentCosts.total;
    const onDemandCosts = analysis.currentCosts.byPricingModel.onDemand;
    
    if (spotPercentage < 0.5 && onDemandCosts > 100) {
      const potentialSavings = onDemandCosts * 0.7; // Assume 70% savings with spot
      
      return {
        type: 'USE_SPOT_INSTANCES',
        title: 'Increase Spot Instance Usage',
        description: `Currently only ${(spotPercentage * 100).toFixed(1)}% of instances are spot. Increasing to 80% could save significant costs.`,
        potentialSavings: potentialSavings,
        impact: 'medium',
        effort: 'low',
        autoApply: true,
        percentage: 80,
        priority: 'high'
      };
    }
    
    return null;
  }
  
  _analyzeRightSizing(analysis) {
    const recommendations = [];
    
    for (const instance of analysis.utilizationMetrics.underutilized) {
      if (instance.utilization < 20) {
        const currentRate = this._getInstanceHourlyRate({ 
          provider: instance.provider, 
          type: instance.type 
        });
        
        const smallerType = this._getSmallerInstanceType(instance.provider, instance.type);
        if (smallerType) {
          const newRate = this._getInstanceHourlyRate({ 
            provider: instance.provider, 
            type: smallerType 
          });
          
          const potentialSavings = (currentRate - newRate) * 24 * 30; // Monthly savings
          
          recommendations.push({
            type: 'SWITCH_INSTANCE_TYPE',
            title: `Downsize instance ${instance.id}`,
            description: `Instance is only ${instance.utilization.toFixed(1)}% utilized. Switch from ${instance.type} to ${smallerType}`,
            instanceId: instance.id,
            currentType: instance.type,
            newType: smallerType,
            potentialSavings: potentialSavings,
            impact: 'low',
            effort: 'medium',
            autoApply: true,
            priority: 'medium'
          });
        }
      }
    }
    
    for (const instance of analysis.utilizationMetrics.overutilized) {
      if (instance.utilization > 90) {
        const largerType = this._getLargerInstanceType(instance.provider, instance.type);
        if (largerType) {
          recommendations.push({
            type: 'SWITCH_INSTANCE_TYPE',
            title: `Upsize instance ${instance.id}`,
            description: `Instance is ${instance.utilization.toFixed(1)}% utilized and may be constrained. Consider upgrading to ${largerType}`,
            instanceId: instance.id,
            currentType: instance.type,
            newType: largerType,
            potentialSavings: -100, // Will cost more but improve performance
            impact: 'high',
            effort: 'medium',
            autoApply: false,
            priority: 'high'
          });
        }
      }
    }
    
    return recommendations;
  }
  
  _getSmallerInstanceType(provider, currentType) {
    // Map to smaller instance types
    const downsizeMap = {
      aws: {
        't3.2xlarge': 't3.xlarge',
        't3.xlarge': 't3.large',
        't3.large': 't3.medium',
        'c5.2xlarge': 'c5.xlarge',
        'c5.xlarge': 'c5.large',
        'm5.xlarge': 'm5.large'
      },
      azure: {
        'Standard_D8s_v3': 'Standard_D4s_v3',
        'Standard_D4s_v3': 'Standard_D2s_v3',
        'Standard_B4ms': 'Standard_B2ms',
        'Standard_B2ms': 'Standard_B2s'
      },
      gcp: {
        'n2-standard-8': 'n2-standard-4',
        'n2-standard-4': 'n2-standard-2',
        'n1-standard-8': 'n1-standard-4',
        'n1-standard-4': 'n1-standard-2',
        'n1-standard-2': 'n1-standard-1'
      }
    };
    
    return downsizeMap[provider]?.[currentType];
  }
  
  _getLargerInstanceType(provider, currentType) {
    // Map to larger instance types
    const upsizeMap = {
      aws: {
        't3.medium': 't3.large',
        't3.large': 't3.xlarge',
        't3.xlarge': 't3.2xlarge',
        'c5.large': 'c5.xlarge',
        'c5.xlarge': 'c5.2xlarge',
        'm5.large': 'm5.xlarge'
      },
      azure: {
        'Standard_B2s': 'Standard_B2ms',
        'Standard_B2ms': 'Standard_B4ms',
        'Standard_D2s_v3': 'Standard_D4s_v3',
        'Standard_D4s_v3': 'Standard_D8s_v3'
      },
      gcp: {
        'n1-standard-1': 'n1-standard-2',
        'n1-standard-2': 'n1-standard-4',
        'n1-standard-4': 'n1-standard-8',
        'n2-standard-2': 'n2-standard-4',
        'n2-standard-4': 'n2-standard-8'
      }
    };
    
    return upsizeMap[provider]?.[currentType];
  }
  
  _analyzeConsolidation(analysis) {
    const recommendations = [];
    
    if (analysis.utilizationMetrics.overall < 50 && 
        analysis.utilizationMetrics.byInstance.length > 5) {
      
      const potentialReduction = Math.floor(analysis.utilizationMetrics.byInstance.length * 0.3);
      const avgRate = this._calculateAverageHourlyRate(analysis.utilizationMetrics.byInstance);
      const potentialSavings = potentialReduction * avgRate * 24 * 30;
      
      recommendations.push({
        type: 'CONSOLIDATE_WORKLOAD',
        title: 'Consolidate Underutilized Instances',
        description: `Overall utilization is only ${analysis.utilizationMetrics.overall.toFixed(1)}%. Consolidating workloads could reduce instance count by ${potentialReduction}`,
        targetUtilization: 70,
        potentialSavings: potentialSavings,
        impact: 'medium',
        effort: 'medium',
        autoApply: true,
        priority: 'medium'
      });
    }
    
    return recommendations;
  }
  
  _analyzeScheduling(analysis) {
    const recommendations = [];
    
    // Check if there's a pattern of low utilization during certain hours
    const currentHour = new Date().getHours();
    const isBusinessHours = currentHour >= 8 && currentHour <= 18;
    
    if (!isBusinessHours && analysis.utilizationMetrics.overall > 30) {
      recommendations.push({
        type: 'IMPLEMENT_SCHEDULING',
        title: 'Implement Off-Hours Scaling',
        description: 'Reduce instance count during non-business hours when workload is typically lower',
        schedule: {
          businessHours: { min: 5, max: 100 },
          afterHours: { min: 1, max: 20 },
          weekend: { min: 0, max: 10 }
        },
        potentialSavings: analysis.currentCosts.total * 0.3, // Assume 30% savings
        impact: 'high',
        effort: 'medium',
        autoApply: false,
        priority: 'medium'
      });
    }
    
    return recommendations;
  }
  
  _analyzeProviderOptimization(analysis) {
    const recommendations = [];
    
    // Compare costs across providers
    const providerCosts = analysis.currentCosts.byProvider;
    const providers = Object.keys(providerCosts);
    
    if (providers.length > 1) {
      // Find most expensive provider
      const mostExpensive = providers.reduce((max, provider) => 
        providerCosts[provider] > providerCosts[max] ? provider : max
      );
      
      const cheapest = providers.reduce((min, provider) => 
        providerCosts[provider] < providerCosts[min] ? provider : min
      );
      
      if (mostExpensive !== cheapest) {
        const costDifference = providerCosts[mostExpensive] - providerCosts[cheapest];
        const percentDifference = (costDifference / providerCosts[mostExpensive]) * 100;
        
        if (percentDifference > 20) {
          recommendations.push({
            type: 'OPTIMIZE_PROVIDER_MIX',
            title: `Shift workload from ${mostExpensive} to ${cheapest}`,
            description: `${mostExpensive} is ${percentDifference.toFixed(1)}% more expensive than ${cheapest} for similar workloads`,
            fromProvider: mostExpensive,
            toProvider: cheapest,
            potentialSavings: costDifference * 0.5, // Assume 50% can be shifted
            impact: 'medium',
            effort: 'high',
            autoApply: false,
            priority: 'low'
          });
        }
      }
    }
    
    return recommendations;
  }
  
  _trackCostHistory(currentCosts) {
    this.costHistory.push({
      timestamp: new Date(),
      costs: currentCosts
    });
    
    // Keep only last 7 days of history
    const cutoff = Date.now() - this.options.lookbackPeriod;
    this.costHistory = this.costHistory.filter(entry => 
      entry.timestamp.getTime() > cutoff
    );
  }
  
  async applyRecommendation(recommendation) {
    logger.info(`Applying recommendation: ${recommendation.title}`);
    
    try {
      switch (recommendation.type) {
        case 'USE_SPOT_INSTANCES':
          return { 
            success: true, 
            action: 'enable_spot',
            percentage: recommendation.percentage 
          };
          
        case 'SWITCH_INSTANCE_TYPE':
          return {
            success: true,
            action: 'resize_instance',
            instanceId: recommendation.instanceId,
            newType: recommendation.newType
          };
          
        case 'CONSOLIDATE_WORKLOAD':
          return {
            success: true,
            action: 'consolidate',
            targetUtilization: recommendation.targetUtilization
          };
          
        case 'IMPLEMENT_SCHEDULING':
          return {
            success: true,
            action: 'schedule',
            schedule: recommendation.schedule
          };
          
        case 'OPTIMIZE_PROVIDER_MIX':
          return {
            success: true,
            action: 'rebalance_providers',
            fromProvider: recommendation.fromProvider,
            toProvider: recommendation.toProvider
          };
          
        default:
          throw new Error(`Unknown recommendation type: ${recommendation.type}`);
      }
    } catch (error) {
      logger.error(`Failed to apply recommendation: ${error.message}`);
      return { success: false, error: error.message };
    }
  }
  
  getCostTrends() {
    if (this.costHistory.length < 2) {
      return null;
    }
    
    const trends = {
      hourly: [],
      daily: [],
      weekly: []
    };
    
    // Calculate hourly trends
    for (let i = 1; i < Math.min(24, this.costHistory.length); i++) {
      const current = this.costHistory[this.costHistory.length - 1];
      const previous = this.costHistory[this.costHistory.length - 1 - i];
      
      trends.hourly.push({
        timestamp: current.timestamp,
        cost: current.costs.total,
        change: current.costs.total - previous.costs.total,
        changePercent: ((current.costs.total - previous.costs.total) / previous.costs.total) * 100
      });
    }
    
    return trends;
  }
  
  generateCostReport() {
    if (!this.lastAnalysis) {
      return null;
    }
    
    return {
      summary: {
        currentMonthlyCost: this.lastAnalysis.currentCosts.total * 24 * 30,
        projectedMonthlyCost: this.lastAnalysis.projectedCosts.next30Days,
        potentialSavings: this.recommendations.reduce((sum, r) => sum + r.potentialSavings, 0),
        utilizationRate: this.lastAnalysis.utilizationMetrics.overall
      },
      breakdown: {
        byProvider: this.lastAnalysis.currentCosts.byProvider,
        byInstanceType: this.lastAnalysis.currentCosts.byInstanceType,
        byPricingModel: this.lastAnalysis.currentCosts.byPricingModel
      },
      recommendations: this.recommendations.map(r => ({
        title: r.title,
        description: r.description,
        potentialSavings: r.potentialSavings,
        priority: r.priority,
        autoApplicable: r.autoApply
      })),
      trends: this.getCostTrends()
    };
  }
}

module.exports = { CostOptimizer };