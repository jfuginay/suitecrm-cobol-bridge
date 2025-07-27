/**
 * Load Simulator for Cloud Burst Demo
 * Generates high load to trigger cloud burst functionality
 */

const axios = require('axios');

class LoadSimulator {
  constructor(apiUrl = 'http://localhost:3000') {
    this.apiUrl = apiUrl;
  }

  /**
   * Generate high load by submitting multiple COBOL jobs
   * @param {number} jobCount - Number of concurrent jobs to submit
   */
  async generateHighLoad(jobCount = 50) {
    console.log(`🚀 Generating high load with ${jobCount} concurrent jobs...`);
    
    const jobs = [];
    const startTime = Date.now();

    // Create different types of jobs to simulate real workload
    const jobTypes = [
      {
        program: 'enhanced-financial-calc',
        params: { loan_amount: 250000, interest_rate: 3.5, term_years: 30 }
      },
      {
        program: 'credit_calculator',
        params: { credit_score: 750, income: 85000, debt: 15000 }
      },
      {
        program: 'batch-processor',
        params: { batch_size: 1000, process_type: 'MONTHLY_BILLING' }
      },
      {
        program: 'report-generator',
        params: { report_type: 'QUARTERLY', department: 'FINANCE' }
      }
    ];

    // Submit jobs
    for (let i = 0; i < jobCount; i++) {
      const jobType = jobTypes[i % jobTypes.length];
      const job = this.submitJob({
        ...jobType,
        jobId: `LOAD_TEST_${i + 1}`,
        priority: Math.random() > 0.8 ? 'HIGH' : 'NORMAL'
      });
      jobs.push(job);
      
      // Stagger submissions slightly to be more realistic
      if (i % 5 === 0) {
        await new Promise(resolve => setTimeout(resolve, 100));
      }
    }

    console.log('⏳ Waiting for all jobs to complete...');
    
    try {
      const results = await Promise.allSettled(jobs);
      const endTime = Date.now();
      const duration = (endTime - startTime) / 1000;

      // Calculate statistics
      const successful = results.filter(r => r.status === 'fulfilled').length;
      const failed = results.filter(r => r.status === 'rejected').length;

      console.log('\\n📊 Load Test Results:');
      console.log(`   Total Jobs: ${jobCount}`);
      console.log(`   Successful: ${successful}`);
      console.log(`   Failed: ${failed}`);
      console.log(`   Duration: ${duration.toFixed(2)} seconds`);
      console.log(`   Throughput: ${(jobCount / duration).toFixed(2)} jobs/second`);
      
      return {
        totalJobs: jobCount,
        successful,
        failed,
        duration,
        throughput: jobCount / duration
      };
    } catch (error) {
      console.error('❌ Error during load test:', error);
      throw error;
    }
  }

  /**
   * Submit a single job
   */
  async submitJob(jobData) {
    try {
      const response = await axios.post(`${this.apiUrl}/api/cobol/execute`, jobData, {
        headers: { 'Content-Type': 'application/json' },
        timeout: 300000 // 5 minute timeout for long-running jobs
      });
      return response.data;
    } catch (error) {
      console.error(`Failed to submit job ${jobData.jobId}:`, error.message);
      throw error;
    }
  }

  /**
   * Monitor cloud burst activation
   */
  async monitorCloudBurst() {
    try {
      const response = await axios.get(`${this.apiUrl}/api/cloud-burst/status`);
      const status = response.data;
      
      console.log('\\n☁️  Cloud Burst Status:');
      console.log(`   Active: ${status.active ? 'YES' : 'NO'}`);
      console.log(`   Cloud Instances: ${status.cloudInstances || 0}`);
      console.log(`   Jobs in Cloud: ${status.cloudJobs || 0}`);
      console.log(`   Estimated Savings: $${status.estimatedSavings || 0}`);
      
      return status;
    } catch (error) {
      console.error('Failed to get cloud burst status:', error.message);
    }
  }
}

module.exports = new LoadSimulator();

// If run directly, execute a demo
if (require.main === module) {
  const simulator = new LoadSimulator();
  
  (async () => {
    console.log('🎯 Starting Cloud Burst Load Simulation Demo\\n');
    
    // Start monitoring in the background
    const monitorInterval = setInterval(() => {
      simulator.monitorCloudBurst();
    }, 5000);
    
    // Generate high load
    await simulator.generateHighLoad(50);
    
    // Final status check
    setTimeout(async () => {
      clearInterval(monitorInterval);
      await simulator.monitorCloudBurst();
      console.log('\\n✅ Demo completed!');
      process.exit(0);
    }, 10000);
  })();
}