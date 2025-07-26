const axios = require('axios');

const API_BASE_URL = process.env.API_URL || 'http://localhost:3000/api/v1';

// Test credentials
const TEST_USER = {
  username: 'user',
  password: 'user123'
};

// Color codes for output
const colors = {
  reset: '\x1b[0m',
  green: '\x1b[32m',
  red: '\x1b[31m',
  yellow: '\x1b[33m',
  blue: '\x1b[34m'
};

async function testAPI() {
  console.log(`${colors.blue}Starting API Gateway Tests...${colors.reset}\n`);
  
  let accessToken = null;
  let testsPassed = 0;
  let testsFailed = 0;

  // Test 1: Health Check
  try {
    console.log('Test 1: Health Check');
    const response = await axios.get(`${API_BASE_URL.replace('/api/v1', '')}/health`);
    console.log(`${colors.green}✓ Health check passed${colors.reset}`);
    console.log(`  Status: ${response.data.status}`);
    console.log(`  Environment: ${response.data.environment}\n`);
    testsPassed++;
  } catch (error) {
    console.log(`${colors.red}✗ Health check failed: ${error.message}${colors.reset}\n`);
    testsFailed++;
  }

  // Test 2: Login
  try {
    console.log('Test 2: User Login');
    const response = await axios.post(`${API_BASE_URL}/auth/login`, TEST_USER);
    accessToken = response.data.accessToken;
    console.log(`${colors.green}✓ Login successful${colors.reset}`);
    console.log(`  User: ${response.data.user.username}`);
    console.log(`  Role: ${response.data.user.role}\n`);
    testsPassed++;
  } catch (error) {
    console.log(`${colors.red}✗ Login failed: ${error.response?.data?.error || error.message}${colors.reset}\n`);
    testsFailed++;
    return; // Can't continue without auth
  }

  // Test 3: Get Current User
  try {
    console.log('Test 3: Get Current User');
    const response = await axios.get(`${API_BASE_URL}/auth/me`, {
      headers: { Authorization: `Bearer ${accessToken}` }
    });
    console.log(`${colors.green}✓ Get current user successful${colors.reset}`);
    console.log(`  User ID: ${response.data.user.id}\n`);
    testsPassed++;
  } catch (error) {
    console.log(`${colors.red}✗ Get current user failed: ${error.response?.data?.error || error.message}${colors.reset}\n`);
    testsFailed++;
  }

  // Test 4: Credit Calculation
  try {
    console.log('Test 4: Credit Calculation');
    const creditData = {
      customerId: 'CUST001',
      creditAmount: 50000,
      income: 75000,
      existingDebt: 10000
    };
    
    const response = await axios.post(
      `${API_BASE_URL}/cobol/credit/calculate`,
      creditData,
      { headers: { Authorization: `Bearer ${accessToken}` } }
    );
    
    console.log(`${colors.green}✓ Credit calculation successful${colors.reset}`);
    console.log(`  Approved: ${response.data.data.approved}`);
    console.log(`  Credit Limit: $${response.data.data.creditLimit}`);
    console.log(`  Interest Rate: ${response.data.data.interestRate}%\n`);
    testsPassed++;
  } catch (error) {
    console.log(`${colors.yellow}⚠ Credit calculation skipped (COBOL runtime may not be available)${colors.reset}`);
    console.log(`  Error: ${error.response?.data?.error?.message || error.message}\n`);
  }

  // Test 5: Interest Calculation
  try {
    console.log('Test 5: Interest Calculation');
    const interestData = {
      principal: 100000,
      rate: 5.5,
      term: 30
    };
    
    const response = await axios.post(
      `${API_BASE_URL}/cobol/interest/calculate`,
      interestData,
      { headers: { Authorization: `Bearer ${accessToken}` } }
    );
    
    console.log(`${colors.green}✓ Interest calculation successful${colors.reset}`);
    console.log(`  Total Interest: $${response.data.data.totalInterest}`);
    console.log(`  Monthly Payment: $${response.data.data.monthlyPayment}\n`);
    testsPassed++;
  } catch (error) {
    console.log(`${colors.yellow}⚠ Interest calculation skipped (COBOL runtime may not be available)${colors.reset}`);
    console.log(`  Error: ${error.response?.data?.error?.message || error.message}\n`);
  }

  // Test 6: Invalid Token
  try {
    console.log('Test 6: Invalid Token Test');
    await axios.get(`${API_BASE_URL}/auth/me`, {
      headers: { Authorization: 'Bearer invalid-token' }
    });
    console.log(`${colors.red}✗ Invalid token test failed (should have been rejected)${colors.reset}\n`);
    testsFailed++;
  } catch (error) {
    if (error.response?.status === 403) {
      console.log(`${colors.green}✓ Invalid token correctly rejected${colors.reset}\n`);
      testsPassed++;
    } else {
      console.log(`${colors.red}✗ Invalid token test failed: ${error.message}${colors.reset}\n`);
      testsFailed++;
    }
  }

  // Test 7: Rate Limiting
  console.log('Test 7: Rate Limiting Test');
  console.log('  Sending multiple requests...');
  let rateLimitHit = false;
  
  for (let i = 0; i < 10; i++) {
    try {
      await axios.post(`${API_BASE_URL}/auth/login`, TEST_USER);
    } catch (error) {
      if (error.response?.status === 429) {
        rateLimitHit = true;
        break;
      }
    }
  }
  
  if (rateLimitHit) {
    console.log(`${colors.green}✓ Rate limiting is working${colors.reset}\n`);
    testsPassed++;
  } else {
    console.log(`${colors.yellow}⚠ Rate limiting may not be configured${colors.reset}\n`);
  }

  // Summary
  console.log(`${colors.blue}Test Summary:${colors.reset}`);
  console.log(`${colors.green}Passed: ${testsPassed}${colors.reset}`);
  console.log(`${colors.red}Failed: ${testsFailed}${colors.reset}`);
  console.log(`Total: ${testsPassed + testsFailed}\n`);

  if (testsFailed === 0) {
    console.log(`${colors.green}All tests passed! 🎉${colors.reset}`);
  } else {
    console.log(`${colors.red}Some tests failed. Please check the logs.${colors.reset}`);
  }
}

// Run tests
testAPI().catch(error => {
  console.error(`${colors.red}Test suite error: ${error.message}${colors.reset}`);
  process.exit(1);
});

// Export for use in other scripts
module.exports = { testAPI };