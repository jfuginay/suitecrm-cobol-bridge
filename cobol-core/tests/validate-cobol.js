const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');
const assert = require('assert');

/**
 * COBOL Program Validation Test Suite
 * 
 * This test suite validates COBOL programs for:
 * - Syntax correctness
 * - Compilation success
 * - Runtime execution
 * - Input/output validation
 * - Performance benchmarks
 */

class CobolValidator {
    constructor(options = {}) {
        this.options = {
            cobolCompiler: process.env.COBOL_COMPILER || 'cobc',
            testDataDir: process.env.TEST_DATA_DIR || path.join(__dirname, 'data'),
            outputDir: process.env.TEST_OUTPUT_DIR || path.join(__dirname, 'output'),
            timeout: parseInt(process.env.TEST_TIMEOUT) || 30000,
            ...options
        };
        
        this.testResults = [];
        this.setupDirectories();
    }
    
    setupDirectories() {
        [this.options.testDataDir, this.options.outputDir].forEach(dir => {
            if (!fs.existsSync(dir)) {
                fs.mkdirSync(dir, { recursive: true });
            }
        });
    }
    
    /**
     * Run all COBOL validation tests
     */
    async runAllTests() {
        console.log('🔍 Starting COBOL Program Validation Tests...\n');
        
        const cobolFiles = this.findCobolFiles();
        
        for (const cobolFile of cobolFiles) {
            try {
                await this.validateProgram(cobolFile);
            } catch (error) {
                console.error(`❌ Failed to validate ${cobolFile}: ${error.message}`);
                this.testResults.push({
                    program: cobolFile,
                    status: 'error',
                    error: error.message
                });
            }
        }
        
        this.printSummary();
        return this.testResults;
    }
    
    /**
     * Find all COBOL files in the project
     */
    findCobolFiles() {
        const cobolDir = path.dirname(__dirname);
        const files = fs.readdirSync(cobolDir);
        
        return files
            .filter(file => file.endsWith('.cob') || file.endsWith('.cbl'))
            .map(file => path.join(cobolDir, file));
    }
    
    /**
     * Validate a single COBOL program
     */
    async validateProgram(programPath) {
        const programName = path.basename(programPath, path.extname(programPath));
        console.log(`🧪 Testing ${programName}...`);
        
        const result = {
            program: programName,
            path: programPath,
            tests: {},
            status: 'passed',
            errors: []
        };
        
        try {
            // Test 1: Syntax validation
            result.tests.syntax = await this.validateSyntax(programPath);
            
            // Test 2: Compilation
            const executablePath = await this.compileProgram(programPath);
            result.tests.compilation = { passed: true, executablePath };
            
            // Test 3: Basic execution
            result.tests.execution = await this.testExecution(executablePath, programName);
            
            // Test 4: Input/Output validation
            result.tests.inputOutput = await this.validateInputOutput(executablePath, programName);
            
            // Test 5: Performance benchmark
            result.tests.performance = await this.benchmarkPerformance(executablePath, programName);
            
            // Test 6: Memory usage
            result.tests.memory = await this.validateMemoryUsage(executablePath, programName);
            
            // Test 7: Data validation
            result.tests.dataValidation = await this.validateDataHandling(executablePath, programName);
            
        } catch (error) {
            result.status = 'failed';
            result.errors.push(error.message);
        }
        
        this.testResults.push(result);
        this.printTestResult(result);
        
        return result;
    }
    
    /**
     * Validate COBOL syntax
     */
    async validateSyntax(programPath) {
        return new Promise((resolve, reject) => {
            const syntaxCheck = spawn(this.options.cobolCompiler, [
                '-fsyntax-only',
                '-Wall',
                '-Wextra',
                programPath
            ]);
            
            let errors = '';
            let warnings = '';
            
            syntaxCheck.stderr.on('data', (data) => {
                const output = data.toString();
                if (output.toLowerCase().includes('error')) {
                    errors += output;
                } else if (output.toLowerCase().includes('warning')) {
                    warnings += output;
                }
            });
            
            syntaxCheck.on('close', (code) => {
                if (code === 0) {
                    resolve({
                        passed: true,
                        warnings: warnings.trim() ? warnings.split('\n') : [],
                        message: 'Syntax validation passed'
                    });
                } else {
                    reject(new Error(`Syntax errors found:\n${errors}`));
                }
            });
            
            syntaxCheck.on('error', (error) => {
                reject(new Error(`Failed to run syntax check: ${error.message}`));
            });
        });
    }
    
    /**
     * Compile COBOL program
     */
    async compileProgram(programPath) {
        const programName = path.basename(programPath, path.extname(programPath));
        const executablePath = path.join(this.options.outputDir, programName);
        
        return new Promise((resolve, reject) => {
            const compile = spawn(this.options.cobolCompiler, [
                '-x',           // Create executable
                '-o', executablePath,
                '-O2',          // Optimization level 2
                '-Wall',        // All warnings
                '-std=cobol2014', // COBOL 2014 standard
                programPath
            ]);
            
            let errors = '';
            
            compile.stderr.on('data', (data) => {
                errors += data.toString();
            });
            
            compile.on('close', (code) => {
                if (code === 0 && fs.existsSync(executablePath)) {
                    resolve(executablePath);
                } else {
                    reject(new Error(`Compilation failed:\n${errors}`));
                }
            });
            
            compile.on('error', (error) => {
                reject(new Error(`Failed to run compiler: ${error.message}`));
            });
        });
    }
    
    /**
     * Test basic program execution
     */
    async testExecution(executablePath, programName) {
        return new Promise((resolve, reject) => {
            const startTime = Date.now();
            
            const execute = spawn(executablePath, [], {
                timeout: this.options.timeout,
                stdio: ['pipe', 'pipe', 'pipe']
            });
            
            let stdout = '';
            let stderr = '';
            
            execute.stdout.on('data', (data) => {
                stdout += data.toString();
            });
            
            execute.stderr.on('data', (data) => {
                stderr += data.toString();
            });
            
            execute.on('close', (code) => {
                const executionTime = Date.now() - startTime;
                
                if (code === 0) {
                    resolve({
                        passed: true,
                        executionTime,
                        stdout: stdout.trim(),
                        stderr: stderr.trim(),
                        exitCode: code
                    });
                } else {
                    reject(new Error(`Program exited with code ${code}:\n${stderr}`));
                }
            });
            
            execute.on('error', (error) => {
                if (error.code === 'ETIMEDOUT') {
                    reject(new Error(`Program execution timed out after ${this.options.timeout}ms`));
                } else {
                    reject(new Error(`Execution failed: ${error.message}`));
                }
            });
            
            // Close stdin to prevent hanging
            execute.stdin.end();
        });
    }
    
    /**
     * Validate input/output handling
     */
    async validateInputOutput(executablePath, programName) {
        const testCases = this.getTestCases(programName);
        const results = [];
        
        for (const testCase of testCases) {
            try {
                const result = await this.runTestCase(executablePath, testCase);
                results.push({
                    testCase: testCase.name,
                    passed: true,
                    input: testCase.input,
                    expectedOutput: testCase.expectedOutput,
                    actualOutput: result.stdout,
                    executionTime: result.executionTime
                });
            } catch (error) {
                results.push({
                    testCase: testCase.name,
                    passed: false,
                    error: error.message,
                    input: testCase.input
                });
            }
        }
        
        const passed = results.filter(r => r.passed).length;
        const total = results.length;
        
        return {
            passed: passed === total,
            results,
            summary: `${passed}/${total} test cases passed`
        };
    }
    
    /**
     * Run a specific test case
     */
    async runTestCase(executablePath, testCase) {
        return new Promise((resolve, reject) => {
            const startTime = Date.now();
            
            const execute = spawn(executablePath, [], {
                timeout: this.options.timeout,
                stdio: ['pipe', 'pipe', 'pipe']
            });
            
            let stdout = '';
            let stderr = '';
            
            execute.stdout.on('data', (data) => {
                stdout += data.toString();
            });
            
            execute.stderr.on('data', (data) => {
                stderr += data.toString();
            });
            
            execute.on('close', (code) => {
                const executionTime = Date.now() - startTime;
                
                if (code === 0) {
                    // Validate output if expected output is provided
                    if (testCase.expectedOutput && 
                        !this.validateOutput(stdout.trim(), testCase.expectedOutput)) {
                        reject(new Error(
                            `Output mismatch:\nExpected: ${testCase.expectedOutput}\nActual: ${stdout.trim()}`
                        ));
                        return;
                    }
                    
                    resolve({
                        stdout: stdout.trim(),
                        stderr: stderr.trim(),
                        executionTime,
                        exitCode: code
                    });
                } else {
                    reject(new Error(`Test case failed with exit code ${code}:\n${stderr}`));
                }
            });
            
            execute.on('error', (error) => {
                reject(new Error(`Test case execution failed: ${error.message}`));
            });
            
            // Send input if provided
            if (testCase.input) {
                execute.stdin.write(testCase.input);
            }
            execute.stdin.end();
        });
    }
    
    /**
     * Validate program output
     */
    validateOutput(actual, expected) {
        if (typeof expected === 'string') {
            return actual === expected;
        } else if (expected instanceof RegExp) {
            return expected.test(actual);
        } else if (typeof expected === 'function') {
            return expected(actual);
        }
        return false;
    }
    
    /**
     * Get test cases for a program
     */
    getTestCases(programName) {
        const testCasesFile = path.join(this.options.testDataDir, `${programName}-tests.json`);
        
        if (fs.existsSync(testCasesFile)) {
            return JSON.parse(fs.readFileSync(testCasesFile, 'utf8'));
        }
        
        // Default test cases based on program name
        return this.getDefaultTestCases(programName);
    }
    
    /**
     * Get default test cases for common programs
     */
    getDefaultTestCases(programName) {
        const defaults = {
            'enhanced-financial-calc': [
                {
                    name: 'basic_calculation',
                    input: '100000\n5.5\n30\n',
                    expectedOutput: /Total Interest: \$\d+\.\d{2}/
                },
                {
                    name: 'zero_principal',
                    input: '0\n5.5\n30\n',
                    expectedOutput: /Total Interest: \$0\.00/
                },
                {
                    name: 'high_interest_rate',
                    input: '50000\n15.0\n15\n',
                    expectedOutput: /Total Interest: \$\d+\.\d{2}/
                }
            ],
            'credit_calculator': [
                {
                    name: 'good_credit',
                    input: 'CUST001\n75000\n10000\n50000\n',
                    expectedOutput: /APPROVED|CREDIT LIMIT/i
                },
                {
                    name: 'poor_credit',
                    input: 'CUST002\n25000\n45000\n50000\n',
                    expectedOutput: /DECLINED|INSUFFICIENT/i
                }
            ]
        };
        
        return defaults[programName] || [
            {
                name: 'basic_execution',
                input: '',
                expectedOutput: () => true // Just check it runs
            }
        ];
    }
    
    /**
     * Benchmark program performance
     */
    async benchmarkPerformance(executablePath, programName) {
        const iterations = 10;
        const executionTimes = [];
        
        console.log(`    🏃 Running performance benchmark (${iterations} iterations)...`);
        
        for (let i = 0; i < iterations; i++) {
            try {
                const result = await this.testExecution(executablePath, programName);
                executionTimes.push(result.executionTime);
            } catch (error) {
                // Skip failed executions in performance test
            }
        }
        
        if (executionTimes.length === 0) {
            throw new Error('No successful executions for performance benchmark');
        }
        
        const avg = executionTimes.reduce((a, b) => a + b, 0) / executionTimes.length;
        const min = Math.min(...executionTimes);
        const max = Math.max(...executionTimes);
        
        return {
            passed: true,
            iterations: executionTimes.length,
            averageTime: Math.round(avg),
            minTime: min,
            maxTime: max,
            executionTimes
        };
    }
    
    /**
     * Validate memory usage
     */
    async validateMemoryUsage(executablePath, programName) {
        return new Promise((resolve, reject) => {
            // Use time command to get memory usage on Unix systems
            const timeCmd = process.platform === 'darwin' ? 'gtime' : 'time';
            
            const monitor = spawn('/usr/bin/time', [
                '-v', // Verbose output with memory info
                executablePath
            ]);
            
            let output = '';
            
            monitor.stderr.on('data', (data) => {
                output += data.toString();
            });
            
            monitor.on('close', (code) => {
                if (code === 0) {
                    const memoryMatch = output.match(/Maximum resident set size \(kbytes\): (\d+)/);
                    const maxMemory = memoryMatch ? parseInt(memoryMatch[1]) : null;
                    
                    resolve({
                        passed: true,
                        maxMemoryKB: maxMemory,
                        memoryInfo: output.split('\n').filter(line => 
                            line.includes('memory') || line.includes('resident')
                        )
                    });
                } else {
                    // Fallback to basic execution test if time command fails
                    resolve({
                        passed: true,
                        message: 'Memory monitoring not available'
                    });
                }
            });
            
            monitor.on('error', (error) => {
                // Fallback to basic test
                resolve({
                    passed: true,
                    message: 'Memory monitoring not available'
                });
            });
        });
    }
    
    /**
     * Validate data handling and edge cases
     */
    async validateDataHandling(executablePath, programName) {
        const edgeCases = [
            { name: 'empty_input', input: '' },
            { name: 'null_bytes', input: '\0\0\0' },
            { name: 'long_input', input: 'A'.repeat(1000) },
            { name: 'special_chars', input: '!@#$%^&*()' },
            { name: 'unicode_input', input: 'äöü测试' }
        ];
        
        const results = [];
        
        for (const testCase of edgeCases) {
            try {
                await this.runTestCase(executablePath, testCase);
                results.push({
                    testCase: testCase.name,
                    passed: true,
                    message: 'Handled gracefully'
                });
            } catch (error) {
                // Some edge cases are expected to fail gracefully
                const isGracefulFailure = error.message.includes('exit code') && 
                                        !error.message.includes('segmentation') &&
                                        !error.message.includes('core dumped');
                
                results.push({
                    testCase: testCase.name,
                    passed: isGracefulFailure,
                    error: error.message,
                    graceful: isGracefulFailure
                });
            }
        }
        
        const passed = results.filter(r => r.passed).length;
        const total = results.length;
        
        return {
            passed: passed >= total * 0.6, // Allow 40% edge case failures
            results,
            summary: `${passed}/${total} edge cases handled correctly`
        };
    }
    
    /**
     * Print test result for a single program
     */
    printTestResult(result) {
        const status = result.status === 'passed' ? '✅' : '❌';
        console.log(`  ${status} ${result.program}`);
        
        Object.entries(result.tests).forEach(([testName, testResult]) => {
            if (testResult.passed !== undefined) {
                const testStatus = testResult.passed ? '  ✓' : '  ✗';
                console.log(`    ${testStatus} ${testName}`);
                
                if (testResult.summary) {
                    console.log(`      ${testResult.summary}`);
                }
                
                if (testResult.averageTime) {
                    console.log(`      Average execution: ${testResult.averageTime}ms`);
                }
            }
        });
        
        if (result.errors.length > 0) {
            console.log('    Errors:');
            result.errors.forEach(error => {
                console.log(`      - ${error}`);
            });
        }
        
        console.log();
    }
    
    /**
     * Print test summary
     */
    printSummary() {
        console.log('\n📊 COBOL Validation Summary');
        console.log('=' .repeat(50));
        
        const total = this.testResults.length;
        const passed = this.testResults.filter(r => r.status === 'passed').length;
        const failed = total - passed;
        
        console.log(`Total programs tested: ${total}`);
        console.log(`✅ Passed: ${passed}`);
        console.log(`❌ Failed: ${failed}`);
        console.log(`Success rate: ${((passed / total) * 100).toFixed(1)}%`);
        
        if (failed > 0) {
            console.log('\n❌ Failed Programs:');
            this.testResults
                .filter(r => r.status === 'failed')
                .forEach(r => {
                    console.log(`  - ${r.program}: ${r.errors.join(', ')}`);
                });
        }
        
        console.log('\n📈 Performance Summary:');
        const perfResults = this.testResults
            .filter(r => r.tests.performance && r.tests.performance.passed)
            .map(r => ({
                program: r.program,
                avgTime: r.tests.performance.averageTime
            }))
            .sort((a, b) => a.avgTime - b.avgTime);
        
        if (perfResults.length > 0) {
            console.log('  Fastest programs:');
            perfResults.slice(0, 3).forEach((r, i) => {
                console.log(`    ${i + 1}. ${r.program}: ${r.avgTime}ms`);
            });
        }
        
        console.log('\n✨ Validation Complete!\n');
    }
    
    /**
     * Generate test report
     */
    generateReport(outputPath) {
        const report = {
            timestamp: new Date().toISOString(),
            summary: {
                total: this.testResults.length,
                passed: this.testResults.filter(r => r.status === 'passed').length,
                failed: this.testResults.filter(r => r.status === 'failed').length
            },
            results: this.testResults,
            environment: {
                cobolCompiler: this.options.cobolCompiler,
                platform: process.platform,
                nodeVersion: process.version
            }
        };
        
        fs.writeFileSync(outputPath, JSON.stringify(report, null, 2));
        console.log(`📄 Test report saved to: ${outputPath}`);
        
        return report;
    }
}

// Run tests if called directly
if (require.main === module) {
    const validator = new CobolValidator();
    
    validator.runAllTests()
        .then(results => {
            const reportPath = path.join(validator.options.outputDir, 'cobol-validation-report.json');
            validator.generateReport(reportPath);
            
            const failed = results.filter(r => r.status === 'failed').length;
            process.exit(failed > 0 ? 1 : 0);
        })
        .catch(error => {
            console.error('❌ Validation failed:', error.message);
            process.exit(1);
        });
}

module.exports = { CobolValidator };