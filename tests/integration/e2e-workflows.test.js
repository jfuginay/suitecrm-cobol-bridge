const { test, expect } = require('@playwright/test');
const axios = require('axios');
const WebSocket = require('ws');

/**
 * End-to-End Integration Tests for SuiteCRM COBOL Bridge
 * 
 * These tests validate complete workflows from SuiteCRM frontend
 * through to COBOL program execution and result handling.
 */

const config = {
    apiBaseUrl: process.env.API_BASE_URL || 'http://localhost:3000/api/v1',
    suitecrmUrl: process.env.SUITECRM_URL || 'http://localhost:8080',
    websocketUrl: process.env.WS_URL || 'ws://localhost:3000',
    timeout: 30000
};

let authToken = null;

test.describe('End-to-End COBOL Bridge Workflows', () => {
    test.beforeAll(async () => {
        // Authenticate with API Gateway
        try {
            const response = await axios.post(`${config.apiBaseUrl}/auth/login`, {
                username: 'admin',
                password: 'admin123'
            });
            authToken = response.data.accessToken;
        } catch (error) {
            console.warn('API authentication failed, some tests may be skipped');
        }
    });

    test.describe('Quote Calculation Workflow', () => {
        test('should calculate quote totals using COBOL financial engine', async ({ page }) => {
            // Step 1: Navigate to SuiteCRM quotes module
            await page.goto(`${config.suitecrmUrl}/index.php?module=AOS_Quotes&action=EditView`);
            
            // Step 2: Create a new quote
            await page.fill('#name', 'Test Quote - E2E Integration');
            await page.selectOption('#billing_account_id', { label: 'Test Customer' });
            
            // Step 3: Add line items
            await page.click('#add_product_line');
            await page.fill('input[name="product_qty"]', '2');
            await page.fill('input[name="product_unit_price"]', '199.99');
            await page.selectOption('select[name="product_discount_type"]', 'percentage');
            await page.fill('input[name="product_discount"]', '10');
            
            // Step 4: Set quote-level parameters
            await page.fill('#shipping_amount', '25.50');
            await page.fill('#tax_amount', '8.5');
            
            // Step 5: Save quote (triggers COBOL calculation)
            await page.click('#SAVE');
            
            // Step 6: Wait for COBOL calculation to complete
            await page.waitForSelector('.cobol-calculated', { timeout: config.timeout });
            
            // Step 7: Verify calculated totals
            const subtotal = await page.textContent('#subtotal_amount');
            const total = await page.textContent('#total_amount');
            
            expect(parseFloat(subtotal)).toBeGreaterThan(0);
            expect(parseFloat(total)).toBeGreaterThan(parseFloat(subtotal));
            
            // Step 8: Verify COBOL execution indicator
            const cobolIndicator = await page.textContent('.cobol-calculated-indicator');
            expect(cobolIndicator).toContain('Calculated using COBOL');
        });

        test('should handle COBOL service unavailability gracefully', async ({ page }) => {
            // Simulate COBOL service down by blocking API calls
            await page.route(`${config.apiBaseUrl}/cobol/**`, route => {
                route.fulfill({
                    status: 503,
                    body: JSON.stringify({
                        success: false,
                        error: 'COBOL service temporarily unavailable'
                    })
                });
            });
            
            await page.goto(`${config.suitecrmUrl}/index.php?module=AOS_Quotes&action=EditView`);
            
            await page.fill('#name', 'Test Quote - Service Down');
            await page.click('#SAVE');
            
            // Should still save quote without COBOL calculations
            await page.waitForSelector('.alert-warning');
            const warning = await page.textContent('.alert-warning');
            expect(warning).toContain('COBOL calculation failed');
            
            // Quote should still be saved
            const quoteId = await page.getAttribute('#id', 'value');
            expect(quoteId).toBeTruthy();
        });
    });

    test.describe('Credit Assessment Workflow', () => {
        test('should perform complete credit assessment via API', async () => {
            if (!authToken) {
                test.skip('API authentication required');
            }

            // Step 1: Submit credit calculation request
            const creditRequest = {
                customerId: 'CUST-E2E-001',
                creditAmount: 50000,
                income: 75000,
                existingDebt: 15000,
                metadata: {
                    requestSource: 'integration-test',
                    timestamp: new Date().toISOString()
                }
            };

            const response = await axios.post(
                `${config.apiBaseUrl}/cobol/credit/calculate`,
                creditRequest,
                {
                    headers: { Authorization: `Bearer ${authToken}` },
                    timeout: config.timeout
                }
            );

            // Step 2: Verify response structure
            expect(response.status).toBe(200);
            expect(response.data.success).toBe(true);
            expect(response.data.data).toHaveProperty('approved');
            expect(response.data.data).toHaveProperty('creditLimit');
            expect(response.data.data).toHaveProperty('interestRate');
            expect(response.data.data).toHaveProperty('riskScore');

            // Step 3: Validate calculation logic
            const result = response.data.data;
            if (result.approved) {
                expect(result.creditLimit).toBeGreaterThan(0);
                expect(result.interestRate).toBeGreaterThan(0);
                expect(result.riskScore).toBeGreaterThanOrEqual(0);
                expect(result.riskScore).toBeLessThanOrEqual(1000);
            }

            // Step 4: Verify execution time is reasonable
            expect(response.data.executionTime).toBeLessThan(10000); // Less than 10 seconds
        });

        test('should handle batch credit processing', async () => {
            if (!authToken) {
                test.skip('API authentication required');
            }

            // Step 1: Submit batch job
            const batchRequest = {
                jobType: 'CREDIT_BATCH_PROCESSING',
                parameters: {
                    customers: [
                        { id: 'CUST001', income: 65000, existingDebt: 10000, requestedAmount: 35000 },
                        { id: 'CUST002', income: 85000, existingDebt: 20000, requestedAmount: 55000 },
                        { id: 'CUST003', income: 45000, existingDebt: 30000, requestedAmount: 25000 }
                    ],
                    processingDate: new Date().toISOString()
                }
            };

            const submitResponse = await axios.post(
                `${config.apiBaseUrl}/cobol/batch/process`,
                batchRequest,
                {
                    headers: { Authorization: `Bearer ${authToken}` }
                }
            );

            expect(submitResponse.status).toBe(202);
            expect(submitResponse.data.jobId).toBeTruthy();

            const jobId = submitResponse.data.jobId;

            // Step 2: Poll for job completion
            let jobCompleted = false;
            let attempts = 0;
            const maxAttempts = 30;

            while (!jobCompleted && attempts < maxAttempts) {
                await new Promise(resolve => setTimeout(resolve, 1000)); // Wait 1 second
                
                const statusResponse = await axios.get(
                    `${config.apiBaseUrl}/cobol/batch/status/${jobId}`,
                    {
                        headers: { Authorization: `Bearer ${authToken}` }
                    }
                );

                const status = statusResponse.data.data.status;
                jobCompleted = status === 'completed' || status === 'failed';
                attempts++;
            }

            expect(jobCompleted).toBe(true);

            // Step 3: Verify batch results
            const finalStatusResponse = await axios.get(
                `${config.apiBaseUrl}/cobol/batch/status/${jobId}`,
                {
                    headers: { Authorization: `Bearer ${authToken}` }
                }
            );

            expect(finalStatusResponse.data.data.status).toBe('completed');
            expect(finalStatusResponse.data.data.results).toBeDefined();
            expect(finalStatusResponse.data.data.results.length).toBe(3);
        });
    });

    test.describe('Real-time Monitoring Workflow', () => {
        test('should receive real-time COBOL execution updates via WebSocket', async () => {
            return new Promise((resolve, reject) => {
                const ws = new WebSocket(config.websocketUrl);
                const receivedEvents = [];
                let executionId = null;

                ws.on('open', async () => {
                    console.log('WebSocket connected');

                    // Trigger a COBOL calculation
                    try {
                        const response = await axios.post(
                            `${config.apiBaseUrl}/cobol/interest/calculate`,
                            {
                                principal: 100000,
                                rate: 5.5,
                                term: 30
                            },
                            {
                                headers: { Authorization: `Bearer ${authToken}` }
                            }
                        );
                        
                        console.log('COBOL calculation triggered');
                    } catch (error) {
                        console.error('Failed to trigger calculation:', error.message);
                        ws.close();
                        reject(error);
                    }
                });

                ws.on('message', (data) => {
                    try {
                        const event = JSON.parse(data.toString());
                        receivedEvents.push(event);

                        if (event.type === 'cobol:execution:start') {
                            executionId = event.data.executionId;
                            console.log('Execution started:', executionId);
                        } else if (event.type === 'cobol:execution:complete' && 
                                 event.data.executionId === executionId) {
                            console.log('Execution completed');
                            
                            // Verify we received the expected events
                            const startEvent = receivedEvents.find(e => 
                                e.type === 'cobol:execution:start' && 
                                e.data.executionId === executionId
                            );
                            const completeEvent = receivedEvents.find(e => 
                                e.type === 'cobol:execution:complete' && 
                                e.data.executionId === executionId
                            );

                            try {
                                expect(startEvent).toBeTruthy();
                                expect(completeEvent).toBeTruthy();
                                expect(completeEvent.data.executionTime).toBeGreaterThan(0);
                                
                                ws.close();
                                resolve();
                            } catch (assertionError) {
                                ws.close();
                                reject(assertionError);
                            }
                        }
                    } catch (parseError) {
                        console.error('Failed to parse WebSocket message:', parseError);
                    }
                });

                ws.on('error', (error) => {
                    console.error('WebSocket error:', error);
                    reject(error);
                });

                ws.on('close', () => {
                    console.log('WebSocket disconnected');
                });

                // Timeout after 30 seconds
                setTimeout(() => {
                    ws.close();
                    reject(new Error('WebSocket test timed out'));
                }, config.timeout);
            });
        });

        test('should display execution metrics in monitoring dashboard', async ({ page }) => {
            await page.goto(`${config.suitecrmUrl}/index.php?module=COBOL_Programs&action=index`);
            
            // Wait for dashboard to load
            await page.waitForSelector('.cobol-monitoring-dashboard');
            
            // Verify key metrics are displayed
            await expect(page.locator('.metric-total-executions')).toBeVisible();
            await expect(page.locator('.metric-success-rate')).toBeVisible();
            await expect(page.locator('.metric-avg-execution-time')).toBeVisible();
            
            // Verify recent executions list
            await expect(page.locator('.recent-executions-list')).toBeVisible();
            
            // Check for real-time updates indicator
            const lastUpdated = await page.textContent('.last-updated-indicator');
            expect(lastUpdated).toMatch(/\d{2}:\d{2}:\d{2}/); // Time format
        });
    });

    test.describe('Business Rules Integration', () => {
        test('should execute complex business rule workflow', async ({ page }) => {
            // Navigate to business rules editor
            await page.goto(`${config.suitecrmUrl}/index.php?module=COBOL_Rules&action=EditView`);
            
            // Create a complex business rule
            await page.fill('#name', 'Credit Approval Rule - E2E Test');
            await page.fill('#description', 'Complex rule for credit approval based on multiple criteria');
            
            // Set up decision tree structure
            await page.click('.add-decision-node');
            await page.fill('.condition-field', 'CUSTOMER_INCOME');
            await page.selectOption('.condition-operator', '>=');
            await page.fill('.condition-value', '50000');
            
            // Add nested conditions
            await page.click('.add-nested-condition');
            await page.fill('.nested-condition-field', 'EXISTING_DEBT');
            await page.selectOption('.nested-condition-operator', '<');
            await page.fill('.nested-condition-value', '25000');
            
            // Define actions
            await page.click('.add-action-true');
            await page.selectOption('.action-type', 'APPROVE_CREDIT');
            await page.fill('.action-credit-limit', '75000');
            
            await page.click('.add-action-false');
            await page.selectOption('.action-type', 'REQUIRE_MANUAL_REVIEW');
            
            // Save rule
            await page.click('#SAVE');
            
            // Test rule execution
            await page.click('.test-rule-button');
            
            // Provide test data
            await page.fill('#test-customer-income', '60000');
            await page.fill('#test-existing-debt', '20000');
            await page.fill('#test-requested-amount', '50000');
            
            await page.click('.execute-test');
            
            // Verify test results
            await page.waitForSelector('.test-results');
            const result = await page.textContent('.test-result-decision');
            expect(result).toContain('APPROVE_CREDIT');
            
            const creditLimit = await page.textContent('.test-result-credit-limit');
            expect(creditLimit).toContain('75000');
        });

        test('should generate COBOL code from business rules', async ({ page }) => {
            await page.goto(`${config.suitecrmUrl}/index.php?module=COBOL_Rules&action=DetailView&record=test-rule-123`);
            
            // Generate COBOL code
            await page.click('.generate-cobol-button');
            
            // Wait for code generation
            await page.waitForSelector('.generated-cobol-code', { timeout: 10000 });
            
            // Verify generated code structure
            const generatedCode = await page.textContent('.generated-cobol-code');
            expect(generatedCode).toContain('IDENTIFICATION DIVISION');
            expect(generatedCode).toContain('DATA DIVISION');
            expect(generatedCode).toContain('PROCEDURE DIVISION');
            expect(generatedCode).toContain('CUSTOMER-INCOME');
            expect(generatedCode).toContain('EXISTING-DEBT');
            
            // Test compilation of generated code
            await page.click('.compile-test-button');
            await page.waitForSelector('.compilation-result');
            
            const compilationResult = await page.textContent('.compilation-result');
            expect(compilationResult).toContain('SUCCESS');
        });
    });

    test.describe('Cloud Burst Integration', () => {
        test('should distribute large COBOL job to cloud workers', async () => {
            if (!authToken) {
                test.skip('API authentication required');
            }

            // Submit a large processing job that should trigger cloud burst
            const largeJobRequest = {
                jobType: 'LARGE_BATCH_PROCESSING',
                parameters: {
                    recordCount: 10000,
                    processingType: 'FINANCIAL_CALCULATIONS',
                    priority: 'high',
                    cloudBurstEligible: true
                }
            };

            const response = await axios.post(
                `${config.apiBaseUrl}/cobol/batch/process`,
                largeJobRequest,
                {
                    headers: { Authorization: `Bearer ${authToken}` }
                }
            );

            expect(response.status).toBe(202);
            const jobId = response.data.jobId;

            // Monitor job distribution
            let jobStatus = 'queued';
            let attempts = 0;
            const maxAttempts = 60; // 1 minute

            while (jobStatus !== 'completed' && jobStatus !== 'failed' && attempts < maxAttempts) {
                await new Promise(resolve => setTimeout(resolve, 1000));
                
                const statusResponse = await axios.get(
                    `${config.apiBaseUrl}/cobol/batch/status/${jobId}`,
                    {
                        headers: { Authorization: `Bearer ${authToken}` }
                    }
                );

                jobStatus = statusResponse.data.data.status;
                
                if (statusResponse.data.data.cloudBurstActive) {
                    console.log('Cloud burst activated for job:', jobId);
                    expect(statusResponse.data.data.activeWorkers).toBeGreaterThan(1);
                }
                
                attempts++;
            }

            expect(jobStatus).toBe('completed');
        });
    });

    test.describe('Error Handling and Recovery', () => {
        test('should handle COBOL program crashes gracefully', async () => {
            if (!authToken) {
                test.skip('API authentication required');
            }

            // Submit a request that will cause a COBOL program to crash
            const crashRequest = {
                customerId: 'CRASH_TEST',
                creditAmount: -1, // Invalid negative amount
                income: 0,
                existingDebt: -1000
            };

            try {
                const response = await axios.post(
                    `${config.apiBaseUrl}/cobol/credit/calculate`,
                    crashRequest,
                    {
                        headers: { Authorization: `Bearer ${authToken}` },
                        timeout: 5000
                    }
                );

                // Should return error response, not crash
                expect(response.data.success).toBe(false);
                expect(response.data.error).toBeTruthy();
                
            } catch (error) {
                // HTTP error is also acceptable
                expect(error.response.status).toBeGreaterThanOrEqual(400);
                expect(error.response.status).toBeLessThan(600);
            }
        });

        test('should retry failed COBOL executions', async () => {
            if (!authToken) {
                test.skip('API authentication required');
            }

            // Submit a request that may fail initially
            const retryRequest = {
                jobType: 'FLAKY_CALCULATION',
                parameters: {
                    simulateFailure: true,
                    retryCount: 0
                }
            };

            const response = await axios.post(
                `${config.apiBaseUrl}/cobol/batch/process`,
                retryRequest,
                {
                    headers: { Authorization: `Bearer ${authToken}` }
                }
            );

            const jobId = response.data.jobId;

            // Monitor for retry attempts
            let retryAttempts = 0;
            let jobCompleted = false;
            let attempts = 0;
            const maxAttempts = 30;

            while (!jobCompleted && attempts < maxAttempts) {
                await new Promise(resolve => setTimeout(resolve, 2000));
                
                const statusResponse = await axios.get(
                    `${config.apiBaseUrl}/cobol/batch/status/${jobId}`,
                    {
                        headers: { Authorization: `Bearer ${authToken}` }
                    }
                );

                const status = statusResponse.data.data;
                
                if (status.retryAttempts > retryAttempts) {
                    retryAttempts = status.retryAttempts;
                    console.log(`Job retry attempt: ${retryAttempts}`);
                }
                
                jobCompleted = status.status === 'completed' || status.status === 'failed';
                attempts++;
            }

            expect(retryAttempts).toBeGreaterThan(0);
            expect(jobCompleted).toBe(true);
        });
    });

    test.describe('Performance and Load Testing', () => {
        test('should handle concurrent COBOL executions', async () => {
            if (!authToken) {
                test.skip('API authentication required');
            }

            const concurrentRequests = 10;
            const requests = [];

            // Create multiple concurrent requests
            for (let i = 0; i < concurrentRequests; i++) {
                const request = axios.post(
                    `${config.apiBaseUrl}/cobol/interest/calculate`,
                    {
                        principal: 50000 + (i * 1000),
                        rate: 4.5 + (i * 0.1),
                        term: 25 + i
                    },
                    {
                        headers: { Authorization: `Bearer ${authToken}` }
                    }
                );
                requests.push(request);
            }

            // Execute all requests concurrently
            const startTime = Date.now();
            const responses = await Promise.allSettled(requests);
            const endTime = Date.now();

            // Verify all requests completed
            const successful = responses.filter(r => r.status === 'fulfilled').length;
            const failed = responses.filter(r => r.status === 'rejected').length;

            expect(successful).toBeGreaterThanOrEqual(concurrentRequests * 0.8); // 80% success rate
            expect(endTime - startTime).toBeLessThan(30000); // Complete within 30 seconds

            console.log(`Concurrent execution results: ${successful} successful, ${failed} failed`);
            console.log(`Total execution time: ${endTime - startTime}ms`);
        });
    });
});

// Helper functions
async function waitForJobCompletion(jobId, timeout = 30000) {
    const startTime = Date.now();
    
    while (Date.now() - startTime < timeout) {
        const response = await axios.get(
            `${config.apiBaseUrl}/cobol/batch/status/${jobId}`,
            {
                headers: { Authorization: `Bearer ${authToken}` }
            }
        );
        
        const status = response.data.data.status;
        if (status === 'completed' || status === 'failed') {
            return response.data.data;
        }
        
        await new Promise(resolve => setTimeout(resolve, 1000));
    }
    
    throw new Error(`Job ${jobId} did not complete within ${timeout}ms`);
}

async function setupTestData() {
    // Create test customers, products, etc.
    // This would integrate with SuiteCRM API to set up test data
    console.log('Setting up test data...');
}

async function cleanupTestData() {
    // Clean up test data after tests
    console.log('Cleaning up test data...');
}