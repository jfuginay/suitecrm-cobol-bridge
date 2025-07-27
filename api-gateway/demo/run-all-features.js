#!/usr/bin/env node

/**
 * Automated Demo Runner - Shows all 6 features
 * Run this to demonstrate the entire system automatically
 */

const axios = require('axios');
const WebSocket = require('ws');
const chalk = require('chalk');

const API_URL = process.env.API_URL || 'http://localhost:3000';
const WS_URL = process.env.WS_URL || 'ws://localhost:3000';

class DemoRunner {
  constructor() {
    this.results = [];
  }

  async sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  log(message, type = 'info') {
    const timestamp = new Date().toISOString().split('T')[1].split('.')[0];
    const colors = {
      info: chalk.blue,
      success: chalk.green,
      error: chalk.red,
      warning: chalk.yellow,
      feature: chalk.magenta.bold
    };
    console.log(`[${timestamp}] ${colors[type](message)}`);
  }

  async feature1_realtimeMonitoring() {
    this.log('\\n=== FEATURE 1: Real-Time Batch Job Monitoring ===', 'feature');
    
    try {
      // Connect to WebSocket for real-time updates
      const ws = new WebSocket(`${WS_URL}/monitoring`);
      
      ws.on('open', () => {
        this.log('Connected to real-time monitoring WebSocket', 'success');
      });

      ws.on('message', (data) => {
        const update = JSON.parse(data);
        this.log(`Real-time update: ${update.type} - ${update.message}`);
      });

      // Execute a COBOL program
      this.log('Executing enhanced-financial-calc with monitoring...');
      const response = await axios.post(`${API_URL}/api/cobol/execute`, {
        program: 'enhanced-financial-calc',
        params: {
          loan_amount: 250000,
          interest_rate: 3.5,
          term_years: 30
        },
        options: {
          monitor: true,
          debug: false
        }
      });

      this.log(`Job ID: ${response.data.jobId}`, 'success');
      this.log('Check http://localhost:8081 to see real-time progress!');
      
      await this.sleep(5000);
      ws.close();
      
      this.results.push({ feature: 1, status: 'success' });
    } catch (error) {
      this.log(`Feature 1 error: ${error.message}`, 'error');
      this.results.push({ feature: 1, status: 'failed', error: error.message });
    }
  }

  async feature2_aiCodeReview() {
    this.log('\\n=== FEATURE 2: AI-Powered Code Review ===', 'feature');
    
    try {
      this.log('Requesting AI review for enhanced-financial-calc.cob...');
      const response = await axios.post(`${API_URL}/api/ai/review`, {
        program: 'enhanced-financial-calc',
        analysisType: ['performance', 'patterns', 'edge-cases', 'test-generation']
      });

      const review = response.data;
      this.log('AI Review Results:', 'success');
      this.log(`  Performance Score: ${review.performanceScore}/100`);
      this.log(`  Code Quality: ${review.codeQuality}`);
      this.log(`  Suggestions: ${review.suggestions.length} found`);
      this.log(`  Generated Tests: ${review.generatedTests.length}`);
      
      if (review.suggestions.length > 0) {
        this.log('\\nTop suggestion: ' + review.suggestions[0].description);
      }
      
      this.results.push({ feature: 2, status: 'success' });
    } catch (error) {
      this.log(`Feature 2 error: ${error.message}`, 'error');
      this.results.push({ feature: 2, status: 'failed', error: error.message });
    }
  }

  async feature3_visualRuleEditor() {
    this.log('\\n=== FEATURE 3: Visual Business Rule Editor ===', 'feature');
    
    try {
      this.log('Extracting business rules from COBOL...');
      const extractResponse = await axios.post(`${API_URL}/api/rules/extract`, {
        program: 'credit_calculator',
        includeDecisionTrees: true
      });

      const rules = extractResponse.data.rules;
      this.log(`Extracted ${rules.length} business rules`, 'success');
      
      // Modify a rule
      this.log('Modifying credit score threshold rule...');
      const modifiedRule = {
        ...rules[0],
        conditions: {
          ...rules[0].conditions,
          creditScoreThreshold: 720 // Changed from 700
        }
      };

      const updateResponse = await axios.put(`${API_URL}/api/rules/${rules[0].id}`, modifiedRule);
      
      this.log('Rule updated successfully!', 'success');
      this.log('Visit http://localhost:8082 to see the visual editor');
      
      this.results.push({ feature: 3, status: 'success' });
    } catch (error) {
      this.log(`Feature 3 error: ${error.message}`, 'error');
      this.results.push({ feature: 3, status: 'failed', error: error.message });
    }
  }

  async feature4_mobileApiGeneration() {
    this.log('\\n=== FEATURE 4: Instant Mobile API Generation ===', 'feature');
    
    try {
      this.log('Generating mobile components from COBOL screen...');
      const screenDef = require('../demo/sample-screen.json');
      
      const response = await axios.post('http://localhost:3001/api/generate', screenDef);
      const generated = response.data;
      
      this.log('Generated mobile artifacts:', 'success');
      this.log(`  React Native Components: ${generated.components.length}`);
      this.log(`  TypeScript Interfaces: ${generated.interfaces.length}`);
      this.log(`  API Endpoints: ${generated.endpoints.length}`);
      this.log(`  Validation Rules: ${generated.validations.length}`);
      
      this.log('\\nSample component preview:');
      this.log(generated.components[0].preview.substring(0, 200) + '...');
      
      this.results.push({ feature: 4, status: 'success' });
    } catch (error) {
      this.log(`Feature 4 error: ${error.message}`, 'error');
      this.results.push({ feature: 4, status: 'failed', error: error.message });
    }
  }

  async feature5_timeTravelDebugging() {
    this.log('\\n=== FEATURE 5: Time-Travel Debugging ===', 'feature');
    
    try {
      this.log('Executing COBOL program with debugging enabled...');
      const response = await axios.post(`${API_URL}/api/cobol/execute`, {
        program: 'enhanced-financial-calc',
        params: {
          loan_amount: 100000,
          interest_rate: 5.0,
          term_years: 15
        },
        options: {
          debug: true,
          recordExecution: true
        }
      });

      const debugSessionId = response.data.debugSessionId;
      this.log(`Debug session created: ${debugSessionId}`, 'success');
      
      await this.sleep(2000);
      
      // Get execution timeline
      this.log('Fetching execution timeline...');
      const timelineResponse = await axios.get(`${API_URL}/api/debug/timeline/${debugSessionId}`);
      const timeline = timelineResponse.data;
      
      this.log(`Timeline captured: ${timeline.steps.length} execution steps`);
      this.log(`Variables tracked: ${timeline.variables.length}`);
      this.log('Visit http://localhost:8083 to explore the timeline');
      
      // Demonstrate replay with modified input
      this.log('\\nReplaying with modified interest rate...');
      const replayResponse = await axios.post(`${API_URL}/api/debug/replay/${debugSessionId}`, {
        modifiedParams: {
          interest_rate: 4.5 // Changed from 5.0
        }
      });
      
      this.log('Replay complete! Differences detected in:', 'success');
      replayResponse.data.differences.forEach(diff => {
        this.log(`  - ${diff.variable}: ${diff.original} → ${diff.modified}`);
      });
      
      this.results.push({ feature: 5, status: 'success' });
    } catch (error) {
      this.log(`Feature 5 error: ${error.message}`, 'error');
      this.results.push({ feature: 5, status: 'failed', error: error.message });
    }
  }

  async feature6_hybridCloudBurst() {
    this.log('\\n=== FEATURE 6: Hybrid Cloud Burst ===', 'feature');
    
    try {
      this.log('Checking current mainframe load...');
      const statusResponse = await axios.get(`${API_URL}/api/cloud-burst/status`);
      this.log(`Current load: ${statusResponse.data.mainframeLoad}%`);
      
      this.log('\\nGenerating high load to trigger cloud burst...');
      const loadSimulator = require('../utils/loadSimulator');
      
      // Start monitoring cloud burst in background
      const monitorInterval = setInterval(async () => {
        try {
          const status = await axios.get(`${API_URL}/api/cloud-burst/status`);
          if (status.data.active) {
            this.log(`☁️  Cloud burst ACTIVE: ${status.data.cloudInstances} instances, ${status.data.cloudJobs} jobs`, 'warning');
          }
        } catch (e) {}
      }, 3000);
      
      // Generate load
      await loadSimulator.generateHighLoad(30);
      
      clearInterval(monitorInterval);
      
      // Get final statistics
      const finalStatus = await axios.get(`${API_URL}/api/cloud-burst/statistics`);
      const stats = finalStatus.data;
      
      this.log('\\nCloud Burst Results:', 'success');
      this.log(`  Total jobs processed: ${stats.totalJobs}`);
      this.log(`  Jobs sent to cloud: ${stats.cloudJobs}`);
      this.log(`  Peak cloud instances: ${stats.peakInstances}`);
      this.log(`  Estimated cost savings: $${stats.costSavings}`);
      this.log(`  Processing time reduction: ${stats.timeReduction}%`);
      
      this.results.push({ feature: 6, status: 'success' });
    } catch (error) {
      this.log(`Feature 6 error: ${error.message}`, 'error');
      this.results.push({ feature: 6, status: 'failed', error: error.message });
    }
  }

  async runFullDemo() {
    console.log(chalk.cyan.bold(`
╔═══════════════════════════════════════════════════════════╗
║     SuiteCRM COBOL Bridge - Automated Demo Runner         ║
║                                                           ║
║  This will demonstrate all 6 revolutionary features       ║
║  Total estimated time: 5-10 minutes                       ║
╚═══════════════════════════════════════════════════════════╝
`));

    const startTime = Date.now();

    // Run all features
    await this.feature1_realtimeMonitoring();
    await this.sleep(2000);
    
    await this.feature2_aiCodeReview();
    await this.sleep(2000);
    
    await this.feature3_visualRuleEditor();
    await this.sleep(2000);
    
    await this.feature4_mobileApiGeneration();
    await this.sleep(2000);
    
    await this.feature5_timeTravelDebugging();
    await this.sleep(2000);
    
    await this.feature6_hybridCloudBurst();

    // Summary
    const duration = Math.round((Date.now() - startTime) / 1000);
    const successful = this.results.filter(r => r.status === 'success').length;
    const failed = this.results.filter(r => r.status === 'failed').length;

    console.log(chalk.cyan.bold(`\\n
╔═══════════════════════════════════════════════════════════╗
║                    DEMO SUMMARY                           ║
╚═══════════════════════════════════════════════════════════╝
`));

    this.log(`Total Features Demonstrated: ${this.results.length}`, 'info');
    this.log(`Successful: ${successful}`, 'success');
    this.log(`Failed: ${failed}`, failed > 0 ? 'error' : 'info');
    this.log(`Total Duration: ${duration} seconds`, 'info');

    console.log('\\nFeature Results:');
    this.results.forEach(r => {
      const icon = r.status === 'success' ? '✅' : '❌';
      const message = r.status === 'success' ? 'Success' : `Failed: ${r.error}`;
      console.log(`  ${icon} Feature ${r.feature}: ${message}`);
    });

    console.log(chalk.yellow.bold(`
\\nNext Steps:
1. Visit each service URL to explore the features interactively
2. Check the logs: docker-compose logs -f
3. View metrics at http://localhost:3006 (Grafana)
4. Read the full documentation in README.md

Thank you for trying SuiteCRM COBOL Bridge! 🚀
`));
  }
}

// Run the demo
const demo = new DemoRunner();
demo.runFullDemo().catch(error => {
  console.error(chalk.red('\\nDemo failed with error:'), error);
  process.exit(1);
});