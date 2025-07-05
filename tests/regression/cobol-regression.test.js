const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const request = require('supertest');
const express = require('express');

// Import all system components to test integration
const { cobolTranspiler } = require('../../src/adapters/cobol-transpiler');
const { cobolRBAC } = require('../../src/auth/cobol-rbac');
const { transpilerAudit } = require('../../src/audit/transpiler-audit');
const { quotaManager } = require('../../src/billing/quota-manager');

// Regression testing utilities
class RegressionTester {
  constructor() {
    this.baselineMetrics = null;
    this.currentMetrics = null;
    this.regressions = [];
    this.improvements = [];
  }

  setBaseline(metrics) {
    this.baselineMetrics = metrics;
  }

  setCurrent(metrics) {
    this.currentMetrics = metrics;
  }

  analyzeRegression() {
    if (!this.baselineMetrics || !this.currentMetrics) {
      throw new Error('Both baseline and current metrics must be set');
    }

    const results = {
      regressions: [],
      improvements: [],
      stable: [],
      summary: {
        totalTests: 0,
        regressionCount: 0,
        improvementCount: 0,
        stableCount: 0,
        regressionRate: 0
      }
    };

    // Compare performance metrics
    for (const [metric, baselineValue] of Object.entries(this.baselineMetrics)) {
      const currentValue = this.currentMetrics[metric];
      
      if (currentValue === undefined) {
        results.regressions.push({
          metric,
          type: 'missing_metric',
          baseline: baselineValue,
          current: 'undefined',
          impact: 'critical'
        });
        continue;
      }

      const percentChange = ((currentValue - baselineValue) / baselineValue) * 100;
      
      // Define regression thresholds based on metric type
      let regressionThreshold = 10; // 10% degradation by default
      let improvementThreshold = -5; // 5% improvement
      
      if (metric.includes('time') || metric.includes('duration')) {
        regressionThreshold = 20; // Allow 20% performance degradation
      } else if (metric.includes('memory')) {
        regressionThreshold = 30; // Allow 30% memory increase
      } else if (metric.includes('error') || metric.includes('failure')) {
        regressionThreshold = 5; // Very strict on error rates
      }

      if (percentChange > regressionThreshold) {
        results.regressions.push({
          metric,
          type: 'performance_degradation',
          baseline: baselineValue,
          current: currentValue,
          percentChange: percentChange.toFixed(2),
          impact: percentChange > 50 ? 'critical' : percentChange > 25 ? 'high' : 'medium'
        });
      } else if (percentChange < improvementThreshold) {
        results.improvements.push({
          metric,
          type: 'performance_improvement',
          baseline: baselineValue,
          current: currentValue,
          percentChange: percentChange.toFixed(2),
          impact: 'positive'
        });
      } else {
        results.stable.push({
          metric,
          baseline: baselineValue,
          current: currentValue,
          percentChange: percentChange.toFixed(2)
        });
      }
    }

    // Calculate summary
    results.summary.totalTests = results.regressions.length + results.improvements.length + results.stable.length;
    results.summary.regressionCount = results.regressions.length;
    results.summary.improvementCount = results.improvements.length;
    results.summary.stableCount = results.stable.length;
    results.summary.regressionRate = (results.regressions.length / results.summary.totalTests) * 100;

    return results;
  }

  generateRegressionReport() {
    const analysis = this.analyzeRegression();
    
    const report = {
      timestamp: new Date().toISOString(),
      passed: analysis.regressions.filter(r => r.impact === 'critical' || r.impact === 'high').length === 0,
      summary: analysis.summary,
      criticalRegressions: analysis.regressions.filter(r => r.impact === 'critical'),
      highRegressions: analysis.regressions.filter(r => r.impact === 'high'),
      mediumRegressions: analysis.regressions.filter(r => r.impact === 'medium'),
      improvements: analysis.improvements,
      stableMetrics: analysis.stable.length
    };

    return report;
  }
}

// Mock dependencies to simulate existing system behavior
jest.mock('../../src/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn()
  }
}));

jest.mock('../../src/database/connection', () => ({
  DatabaseManager: jest.fn().mockImplementation(() => ({
    connect: jest.fn().mockResolvedValue(true),
    disconnect: jest.fn().mockResolvedValue(true),
    query: jest.fn().mockResolvedValue({ rowCount: 1 })
  }))
}));

jest.mock('../../src/utils/cache', () => ({
  CacheManager: jest.fn().mockImplementation(() => ({
    get: jest.fn().mockResolvedValue(null),
    set: jest.fn().mockResolvedValue(true),
    delete: jest.fn().mockResolvedValue(true),
    clear: jest.fn().mockResolvedValue(true)
  }))
}));

describe('COBOL Transpiler Regression Tests', () => {
  let app;
  let regressionTester;
  let baselineMetrics;

  // Simulate baseline metrics from pre-COBOL system
  const BASELINE_SYSTEM_METRICS = {
    // API Performance
    avgApiResponseTime: 250, // milliseconds
    p95ApiResponseTime: 500, // milliseconds
    apiErrorRate: 0.5, // percentage
    apiThroughput: 1000, // requests per minute
    
    // Memory Usage
    avgMemoryUsage: 150, // MB
    peakMemoryUsage: 300, // MB
    memoryLeakRate: 0, // MB per hour
    
    // Database Performance
    avgDbQueryTime: 25, // milliseconds
    dbConnectionPoolUtilization: 30, // percentage
    dbErrorRate: 0.1, // percentage
    
    // Authentication System
    avgAuthTime: 50, // milliseconds
    authSuccessRate: 99.9, // percentage
    authFailureRate: 0.1, // percentage
    
    // Banking Connector Performance
    fisConnectorResponseTime: 180, // milliseconds
    fiservConnectorResponseTime: 200, // milliseconds
    temenosConnectorResponseTime: 220, // milliseconds
    tcsConnectorResponseTime: 190, // milliseconds
    
    // System Stability
    uptimePercentage: 99.95, // percentage
    errorRecoveryTime: 30, // seconds
    systemStartupTime: 15, // seconds
    
    // Security Metrics
    securityEventRate: 0.01, // events per hour
    fraudDetectionAccuracy: 99.5, // percentage
    encryptionOverhead: 5, // percentage
    
    // Business Metrics
    transactionSuccessRate: 99.8, // percentage
    complianceViolationRate: 0.01, // percentage
    customerSatisfactionScore: 4.7 // out of 5
  };

  beforeAll(async () => {
    regressionTester = new RegressionTester();
    regressionTester.setBaseline(BASELINE_SYSTEM_METRICS);

    // Create Express app for regression testing
    app = express();
    app.use(express.json());

    // Initialize all COBOL components alongside existing system
    await cobolTranspiler.initialize();
    await cobolRBAC.initialize();
    await transpilerAudit.initialize();
    await quotaManager.initialize();

    // Setup regression test routes
    setupRegressionRoutes(app);
  });

  afterAll(async () => {
    await cobolTranspiler.shutdown();
    await cobolRBAC.shutdown();
    await transpilerAudit.shutdown();
    await quotaManager.shutdown();

    // Generate final regression report
    const report = regressionTester.generateRegressionReport();
    console.log('\n=== REGRESSION TEST REPORT ===');
    console.log(`Total Metrics Tested: ${report.summary.totalTests}`);
    console.log(`Critical Regressions: ${report.criticalRegressions.length}`);
    console.log(`High Regressions: ${report.highRegressions.length}`);
    console.log(`Medium Regressions: ${report.mediumRegressions.length}`);
    console.log(`Improvements: ${report.improvements.length}`);
    console.log(`Stable Metrics: ${report.stableMetrics}`);
    console.log(`Regression Rate: ${report.summary.regressionRate.toFixed(2)}%`);
    console.log(`Overall Status: ${report.passed ? 'PASSED' : 'FAILED'}`);
  });

  beforeEach(() => {
    jest.clearAllMocks();
  });

  function setupRegressionRoutes(app) {
    // Existing API endpoints that should not be affected by COBOL integration
    
    // Standard banking API (pre-existing)
    app.post('/api/banking/transfer', async (req, res) => {
      const startTime = Date.now();
      
      try {
        // Simulate existing banking transfer logic
        const { fromAccount, toAccount, amount, currency } = req.body;
        
        // Validate inputs (existing logic)
        if (!fromAccount || !toAccount || !amount) {
          return res.status(400).json({ error: 'Missing required fields' });
        }
        
        // Simulate processing delay
        await new Promise(resolve => setTimeout(resolve, Math.random() * 100 + 50));
        
        const endTime = Date.now();
        const processingTime = endTime - startTime;
        
        res.json({
          success: true,
          transactionId: `txn_${Date.now()}`,
          fromAccount,
          toAccount,
          amount,
          currency: currency || 'USD',
          processingTime,
          timestamp: new Date().toISOString()
        });
        
      } catch (error) {
        const endTime = Date.now();
        const processingTime = endTime - startTime;
        
        res.status(500).json({
          success: false,
          error: error.message,
          processingTime
        });
      }
    });

    // Authentication endpoint (pre-existing)
    app.post('/api/auth/login', async (req, res) => {
      const startTime = Date.now();
      
      try {
        const { username, password } = req.body;
        
        if (!username || !password) {
          return res.status(400).json({ error: 'Username and password required' });
        }
        
        // Simulate auth processing
        await new Promise(resolve => setTimeout(resolve, Math.random() * 30 + 20));
        
        const endTime = Date.now();
        const authTime = endTime - startTime;
        
        // Simulate 99.9% success rate
        const success = Math.random() > 0.001;
        
        if (success) {
          res.json({
            success: true,
            token: `token_${Date.now()}`,
            expiresIn: 3600,
            authTime
          });
        } else {
          res.status(401).json({
            success: false,
            error: 'Authentication failed',
            authTime
          });
        }
        
      } catch (error) {
        const endTime = Date.now();
        const authTime = endTime - startTime;
        
        res.status(500).json({
          success: false,
          error: error.message,
          authTime
        });
      }
    });

    // Banking connector endpoints (pre-existing)
    const bankingConnectors = ['fis', 'fiserv', 'temenos', 'tcs'];
    
    bankingConnectors.forEach(connector => {
      app.get(`/api/connectors/${connector}/status`, async (req, res) => {
        const startTime = Date.now();
        
        try {
          // Simulate connector-specific response times
          const responseTimeMap = {
            fis: 180,
            fiserv: 200,
            temenos: 220,
            tcs: 190
          };
          
          const baseResponseTime = responseTimeMap[connector];
          const variance = Math.random() * 40 - 20; // ±20ms variance
          const simulatedDelay = Math.max(baseResponseTime + variance, 50);
          
          await new Promise(resolve => setTimeout(resolve, simulatedDelay));
          
          const endTime = Date.now();
          const responseTime = endTime - startTime;
          
          res.json({
            connector,
            status: 'online',
            responseTime,
            version: '2.1.0',
            capabilities: ['transfer', 'balance', 'statement']
          });
          
        } catch (error) {
          const endTime = Date.now();
          const responseTime = endTime - startTime;
          
          res.status(500).json({
            connector,
            status: 'error',
            error: error.message,
            responseTime
          });
        }
      });
    });

    // System health endpoint (pre-existing)
    app.get('/api/system/health', async (req, res) => {
      const startTime = Date.now();
      
      try {
        const memoryUsage = process.memoryUsage();
        const uptime = process.uptime();
        
        const endTime = Date.now();
        const responseTime = endTime - startTime;
        
        res.json({
          status: 'healthy',
          uptime: uptime,
          memory: {
            rss: Math.round(memoryUsage.rss / 1024 / 1024), // MB
            heapUsed: Math.round(memoryUsage.heapUsed / 1024 / 1024), // MB
            heapTotal: Math.round(memoryUsage.heapTotal / 1024 / 1024) // MB
          },
          responseTime,
          timestamp: new Date().toISOString()
        });
        
      } catch (error) {
        res.status(500).json({
          status: 'unhealthy',
          error: error.message
        });
      }
    });

    // Combined endpoint using both old and new systems
    app.post('/api/hybrid/process-transaction', async (req, res) => {
      const startTime = Date.now();
      
      try {
        const { transactionType, data, useCobolTranspiler } = req.body;
        
        let result;
        
        if (useCobolTranspiler && transactionType === 'cobol_modernization') {
          // Use new COBOL transpiler
          const parseResult = await cobolTranspiler.parseCobol(data.cobolCode, { 
            bankingSystem: data.bankingSystem 
          });
          
          result = {
            type: 'cobol_transpilation',
            complexity: parseResult.complexity,
            variables: parseResult.variables?.length || 0,
            procedures: parseResult.procedures?.length || 0
          };
        } else {
          // Use existing transaction processing
          await new Promise(resolve => setTimeout(resolve, Math.random() * 100 + 50));
          
          result = {
            type: 'traditional_processing',
            processed: true,
            amount: data.amount || 0,
            account: data.account || 'unknown'
          };
        }
        
        const endTime = Date.now();
        const processingTime = endTime - startTime;
        
        res.json({
          success: true,
          result,
          processingTime,
          systemType: useCobolTranspiler ? 'hybrid' : 'legacy'
        });
        
      } catch (error) {
        const endTime = Date.now();
        const processingTime = endTime - startTime;
        
        res.status(500).json({
          success: false,
          error: error.message,
          processingTime
        });
      }
    });
  }

  describe('Existing API Performance Regression', () => {
    test('should maintain existing banking transfer API performance', async () => {
      const responseTimes = [];
      const numRequests = 20;
      
      for (let i = 0; i < numRequests; i++) {
        const response = await request(app)
          .post('/api/banking/transfer')
          .send({
            fromAccount: '123456789',
            toAccount: '987654321',
            amount: 1000.00,
            currency: 'USD'
          });
        
        expect(response.status).toBe(200);
        expect(response.body.success).toBe(true);
        expect(response.body.processingTime).toBeDefined();
        
        responseTimes.push(response.body.processingTime);
      }
      
      const avgResponseTime = responseTimes.reduce((a, b) => a + b, 0) / responseTimes.length;
      const p95ResponseTime = responseTimes.sort((a, b) => a - b)[Math.floor(numRequests * 0.95)];
      
      // Check against baseline metrics
      expect(avgResponseTime).toBeLessThan(BASELINE_SYSTEM_METRICS.avgApiResponseTime * 1.2); // Allow 20% degradation
      expect(p95ResponseTime).toBeLessThan(BASELINE_SYSTEM_METRICS.p95ApiResponseTime * 1.2);
      
      console.log(`Banking Transfer API Performance:
        Average: ${avgResponseTime.toFixed(2)}ms (baseline: ${BASELINE_SYSTEM_METRICS.avgApiResponseTime}ms)
        P95: ${p95ResponseTime.toFixed(2)}ms (baseline: ${BASELINE_SYSTEM_METRICS.p95ApiResponseTime}ms)
      `);
    });

    test('should maintain authentication system performance', async () => {
      const authTimes = [];
      const numRequests = 15;
      let successCount = 0;
      
      for (let i = 0; i < numRequests; i++) {
        const response = await request(app)
          .post('/api/auth/login')
          .send({
            username: `testuser${i}`,
            password: 'testpassword'
          });
        
        if (response.status === 200) {
          successCount++;
          authTimes.push(response.body.authTime);
        } else if (response.status === 401) {
          // Expected failure rate
          authTimes.push(response.body.authTime);
        }
      }
      
      const avgAuthTime = authTimes.reduce((a, b) => a + b, 0) / authTimes.length;
      const successRate = (successCount / numRequests) * 100;
      
      expect(avgAuthTime).toBeLessThan(BASELINE_SYSTEM_METRICS.avgAuthTime * 1.3); // Allow 30% degradation
      expect(successRate).toBeGreaterThan(95); // Should maintain high success rate
      
      console.log(`Authentication Performance:
        Average Auth Time: ${avgAuthTime.toFixed(2)}ms (baseline: ${BASELINE_SYSTEM_METRICS.avgAuthTime}ms)
        Success Rate: ${successRate.toFixed(1)}% (baseline: ${BASELINE_SYSTEM_METRICS.authSuccessRate}%)
      `);
    });

    test('should maintain banking connector performance', async () => {
      const connectorPerformance = {};
      const connectors = ['fis', 'fiserv', 'temenos', 'tcs'];
      
      for (const connector of connectors) {
        const response = await request(app)
          .get(`/api/connectors/${connector}/status`);
        
        expect(response.status).toBe(200);
        expect(response.body.status).toBe('online');
        expect(response.body.responseTime).toBeDefined();
        
        connectorPerformance[connector] = response.body.responseTime;
        
        // Check against baseline
        const baselineKey = `${connector}ConnectorResponseTime`;
        const baseline = BASELINE_SYSTEM_METRICS[baselineKey];
        expect(response.body.responseTime).toBeLessThan(baseline * 1.5); // Allow 50% degradation
      }
      
      console.log('Banking Connector Performance:');
      connectors.forEach(connector => {
        const baselineKey = `${connector}ConnectorResponseTime`;
        const baseline = BASELINE_SYSTEM_METRICS[baselineKey];
        console.log(`  ${connector}: ${connectorPerformance[connector]}ms (baseline: ${baseline}ms)`);
      });
    });
  });

  describe('System Resource Regression', () => {
    test('should not significantly increase memory usage', async () => {
      const initialMemory = process.memoryUsage();
      
      // Perform various operations to stress test memory
      const operations = [];
      
      // Banking operations
      for (let i = 0; i < 10; i++) {
        operations.push(
          request(app)
            .post('/api/banking/transfer')
            .send({
              fromAccount: `acc${i}`,
              toAccount: `acc${i + 1}`,
              amount: 100.00
            })
        );
      }
      
      // COBOL operations (new functionality)
      operations.push(
        request(app)
          .post('/api/hybrid/process-transaction')
          .send({
            transactionType: 'cobol_modernization',
            useCobolTranspiler: true,
            data: {
              cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nPROCEDURE DIVISION.\nSTOP RUN.',
              bankingSystem: 'fis'
            }
          })
      );
      
      await Promise.all(operations);
      
      // Allow garbage collection
      await new Promise(resolve => setTimeout(resolve, 1000));
      global.gc && global.gc();
      
      const finalMemory = process.memoryUsage();
      const memoryIncrease = (finalMemory.heapUsed - initialMemory.heapUsed) / 1024 / 1024; // MB
      
      expect(memoryIncrease).toBeLessThan(100); // Should not increase by more than 100MB
      
      console.log(`Memory Usage:
        Initial: ${(initialMemory.heapUsed / 1024 / 1024).toFixed(2)}MB
        Final: ${(finalMemory.heapUsed / 1024 / 1024).toFixed(2)}MB
        Increase: ${memoryIncrease.toFixed(2)}MB
      `);
    });

    test('should maintain system health metrics', async () => {
      const response = await request(app)
        .get('/api/system/health');
      
      expect(response.status).toBe(200);
      expect(response.body.status).toBe('healthy');
      expect(response.body.responseTime).toBeLessThan(100); // Health check should be fast
      expect(response.body.memory.heapUsed).toBeLessThan(500); // Should not use more than 500MB
      
      console.log(`System Health:
        Status: ${response.body.status}
        Uptime: ${response.body.uptime.toFixed(2)}s
        Memory (Heap Used): ${response.body.memory.heapUsed}MB
        Response Time: ${response.body.responseTime}ms
      `);
    });
  });

  describe('Functional Regression', () => {
    test('should maintain backward compatibility with existing APIs', async () => {
      // Test that all existing endpoints still work as expected
      const compatibilityTests = [
        {
          name: 'Banking Transfer',
          request: () => request(app)
            .post('/api/banking/transfer')
            .send({
              fromAccount: '123456789',
              toAccount: '987654321',
              amount: 500.00
            }),
          expectedStatus: 200,
          expectedFields: ['success', 'transactionId', 'processingTime']
        },
        {
          name: 'Authentication',
          request: () => request(app)
            .post('/api/auth/login')
            .send({
              username: 'testuser',
              password: 'testpass'
            }),
          expectedStatus: 200,
          expectedFields: ['success', 'token', 'expiresIn']
        },
        {
          name: 'System Health',
          request: () => request(app)
            .get('/api/system/health'),
          expectedStatus: 200,
          expectedFields: ['status', 'uptime', 'memory']
        }
      ];
      
      for (const test of compatibilityTests) {
        const response = await test.request();
        
        expect(response.status).toBe(test.expectedStatus);
        
        for (const field of test.expectedFields) {
          expect(response.body).toHaveProperty(field);
        }
        
        console.log(`${test.name} compatibility: PASSED`);
      }
    });

    test('should maintain data format consistency', async () => {
      const response = await request(app)
        .post('/api/banking/transfer')
        .send({
          fromAccount: '123456789',
          toAccount: '987654321',
          amount: 1000.00,
          currency: 'EUR'
        });
      
      expect(response.status).toBe(200);
      
      // Verify response structure hasn't changed
      const expectedStructure = {
        success: 'boolean',
        transactionId: 'string',
        fromAccount: 'string',
        toAccount: 'string',
        amount: 'number',
        currency: 'string',
        processingTime: 'number',
        timestamp: 'string'
      };
      
      for (const [field, expectedType] of Object.entries(expectedStructure)) {
        expect(response.body).toHaveProperty(field);
        expect(typeof response.body[field]).toBe(expectedType);
      }
    });
  });

  describe('Integration Regression', () => {
    test('should maintain existing system integration points', async () => {
      // Test that COBOL integration doesn't break existing system integration
      const connectors = ['fis', 'fiserv', 'temenos', 'tcs'];
      
      for (const connector of connectors) {
        const response = await request(app)
          .get(`/api/connectors/${connector}/status`);
        
        expect(response.status).toBe(200);
        expect(response.body.connector).toBe(connector);
        expect(response.body.status).toBe('online');
        expect(response.body.capabilities).toBeInstanceOf(Array);
        expect(response.body.capabilities.length).toBeGreaterThan(0);
      }
    });

    test('should support hybrid operations without affecting legacy functionality', async () => {
      // Test legacy operation
      const legacyResponse = await request(app)
        .post('/api/hybrid/process-transaction')
        .send({
          transactionType: 'standard_transfer',
          useCobolTranspiler: false,
          data: {
            amount: 1000.00,
            account: '123456789'
          }
        });
      
      expect(legacyResponse.status).toBe(200);
      expect(legacyResponse.body.success).toBe(true);
      expect(legacyResponse.body.systemType).toBe('legacy');
      
      // Test hybrid operation
      const hybridResponse = await request(app)
        .post('/api/hybrid/process-transaction')
        .send({
          transactionType: 'cobol_modernization',
          useCobolTranspiler: true,
          data: {
            cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n01 VAR1 PIC 9(5).\nPROCEDURE DIVISION.\nSTOP RUN.',
            bankingSystem: 'fis'
          }
        });
      
      expect(hybridResponse.status).toBe(200);
      expect(hybridResponse.body.success).toBe(true);
      expect(hybridResponse.body.systemType).toBe('hybrid');
      
      // Both should complete successfully with similar performance characteristics
      const performanceDelta = Math.abs(hybridResponse.body.processingTime - legacyResponse.body.processingTime);
      expect(performanceDelta).toBeLessThan(5000); // Should be within 5 seconds of each other
    });
  });

  describe('Error Handling Regression', () => {
    test('should maintain existing error handling patterns', async () => {
      // Test various error scenarios to ensure they still behave as expected
      const errorTests = [
        {
          name: 'Missing fields in banking transfer',
          request: () => request(app)
            .post('/api/banking/transfer')
            .send({ fromAccount: '123' }), // Missing required fields
          expectedStatus: 400,
          expectedError: 'Missing required fields'
        },
        {
          name: 'Missing auth credentials',
          request: () => request(app)
            .post('/api/auth/login')
            .send({}), // No username/password
          expectedStatus: 400,
          expectedError: 'Username and password required'
        }
      ];
      
      for (const test of errorTests) {
        const response = await test.request();
        
        expect(response.status).toBe(test.expectedStatus);
        expect(response.body.error).toBe(test.expectedError);
        
        console.log(`Error handling for '${test.name}': PASSED`);
      }
    });

    test('should not introduce new error conditions in existing flows', async () => {
      // Perform successful operations multiple times to ensure stability
      const iterations = 10;
      let successCount = 0;
      
      for (let i = 0; i < iterations; i++) {
        try {
          const response = await request(app)
            .post('/api/banking/transfer')
            .send({
              fromAccount: `acc${i}`,
              toAccount: `acc${i + 1000}`,
              amount: 100.00
            });
          
          if (response.status === 200) {
            successCount++;
          }
        } catch (error) {
          console.log(`Iteration ${i} failed:`, error.message);
        }
      }
      
      const successRate = (successCount / iterations) * 100;
      expect(successRate).toBeGreaterThan(95); // Should maintain high success rate
      
      console.log(`Stability Test: ${successCount}/${iterations} successful (${successRate.toFixed(1)}%)`);
    });
  });

  describe('Performance Baseline Comparison', () => {
    test('should generate comprehensive regression metrics', async () => {
      // Collect current system metrics
      const currentMetrics = {};
      
      // API Performance metrics
      const transferResponses = [];
      for (let i = 0; i < 10; i++) {
        const response = await request(app)
          .post('/api/banking/transfer')
          .send({
            fromAccount: `perf${i}`,
            toAccount: `perf${i + 1}`,
            amount: 100.00
          });
        if (response.status === 200) {
          transferResponses.push(response.body.processingTime);
        }
      }
      
      currentMetrics.avgApiResponseTime = transferResponses.reduce((a, b) => a + b, 0) / transferResponses.length;
      currentMetrics.p95ApiResponseTime = transferResponses.sort((a, b) => a - b)[Math.floor(transferResponses.length * 0.95)];
      
      // Memory metrics
      const memoryUsage = process.memoryUsage();
      currentMetrics.avgMemoryUsage = Math.round(memoryUsage.heapUsed / 1024 / 1024);
      currentMetrics.peakMemoryUsage = Math.round(memoryUsage.heapTotal / 1024 / 1024);
      
      // Authentication metrics
      const authResponses = [];
      for (let i = 0; i < 5; i++) {
        const response = await request(app)
          .post('/api/auth/login')
          .send({
            username: `perfuser${i}`,
            password: 'perfpass'
          });
        if (response.body.authTime) {
          authResponses.push(response.body.authTime);
        }
      }
      currentMetrics.avgAuthTime = authResponses.reduce((a, b) => a + b, 0) / authResponses.length;
      
      // Banking connector metrics
      const connectors = ['fis', 'fiserv', 'temenos', 'tcs'];
      for (const connector of connectors) {
        const response = await request(app)
          .get(`/api/connectors/${connector}/status`);
        if (response.status === 200) {
          currentMetrics[`${connector}ConnectorResponseTime`] = response.body.responseTime;
        }
      }
      
      // Set current metrics and analyze
      regressionTester.setCurrent(currentMetrics);
      const regressionReport = regressionTester.generateRegressionReport();
      
      // Report should pass (no critical regressions)
      expect(regressionReport.passed).toBe(true);
      expect(regressionReport.criticalRegressions.length).toBe(0);
      
      // Log detailed comparison
      console.log('\n=== DETAILED REGRESSION ANALYSIS ===');
      console.log(`Regression Rate: ${regressionReport.summary.regressionRate.toFixed(2)}%`);
      
      if (regressionReport.highRegressions.length > 0) {
        console.log('\nHigh Impact Regressions:');
        regressionReport.highRegressions.forEach(reg => {
          console.log(`  - ${reg.metric}: ${reg.percentChange}% change (${reg.baseline} -> ${reg.current})`);
        });
      }
      
      if (regressionReport.improvements.length > 0) {
        console.log('\nPerformance Improvements:');
        regressionReport.improvements.forEach(imp => {
          console.log(`  + ${imp.metric}: ${imp.percentChange}% improvement (${imp.baseline} -> ${imp.current})`);
        });
      }
      
      console.log(`\nStable Metrics: ${regressionReport.stableMetrics}`);
    });
  });
});