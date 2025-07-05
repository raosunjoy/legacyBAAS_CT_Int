const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const request = require('supertest');
const express = require('express');
const cluster = require('cluster');
const os = require('os');

// Import system components
const { cobolTranspiler } = require('../../src/adapters/cobol-transpiler');
const { cobolRBAC } = require('../../src/auth/cobol-rbac');
const { quotaManager } = require('../../src/billing/quota-manager');
const { transpilerAudit } = require('../../src/audit/transpiler-audit');

// Performance testing utilities
class PerformanceProfiler {
  constructor() {
    this.metrics = {
      memory: [],
      cpu: [],
      responseTime: [],
      throughput: [],
      errors: []
    };
    this.startTime = null;
    this.endTime = null;
  }

  start() {
    this.startTime = process.hrtime.bigint();
    this.initialMemory = process.memoryUsage();
  }

  stop() {
    this.endTime = process.hrtime.bigint();
    this.finalMemory = process.memoryUsage();
  }

  getDuration() {
    return Number(this.endTime - this.startTime) / 1000000; // Convert to milliseconds
  }

  getMemoryUsage() {
    return {
      initial: this.initialMemory,
      final: this.finalMemory,
      delta: {
        rss: this.finalMemory.rss - this.initialMemory.rss,
        heapUsed: this.finalMemory.heapUsed - this.initialMemory.heapUsed,
        heapTotal: this.finalMemory.heapTotal - this.initialMemory.heapTotal,
        external: this.finalMemory.external - this.initialMemory.external
      }
    };
  }

  recordMetric(type, value) {
    this.metrics[type].push({
      timestamp: Date.now(),
      value: value
    });
  }

  getPercentile(values, percentile) {
    const sorted = values.sort((a, b) => a - b);
    const index = Math.ceil((percentile / 100) * sorted.length) - 1;
    return sorted[index];
  }

  generateReport() {
    const responseTimes = this.metrics.responseTime.map(m => m.value);
    const memoryUsage = this.getMemoryUsage();

    return {
      duration: this.getDuration(),
      responseTime: {
        min: Math.min(...responseTimes),
        max: Math.max(...responseTimes),
        avg: responseTimes.reduce((a, b) => a + b, 0) / responseTimes.length,
        p50: this.getPercentile(responseTimes, 50),
        p95: this.getPercentile(responseTimes, 95),
        p99: this.getPercentile(responseTimes, 99)
      },
      memory: memoryUsage,
      throughput: {
        requestsPerSecond: (responseTimes.length / this.getDuration()) * 1000,
        totalRequests: responseTimes.length
      },
      errors: this.metrics.errors.length,
      errorRate: (this.metrics.errors.length / responseTimes.length) * 100
    };
  }
}

// Mock dependencies
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
    query: jest.fn().mockImplementation(() => {
      // Simulate database latency
      return new Promise(resolve => {
        setTimeout(() => resolve({ rowCount: 1 }), Math.random() * 10);
      });
    })
  }))
}));

jest.mock('../../src/utils/cache', () => ({
  CacheManager: jest.fn().mockImplementation(() => ({
    get: jest.fn().mockImplementation((key) => {
      // Simulate cache hit/miss ratio (80% hit rate)
      return Math.random() < 0.8 ? `cached_${key}` : null;
    }),
    set: jest.fn(),
    delete: jest.fn(),
    clear: jest.fn()
  }))
}));

describe('COBOL Transpiler Performance Tests', () => {
  let app;
  let profiler;
  let mockDb;

  // Sample COBOL programs of different complexities
  const cobolPrograms = {
    simple: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. SIMPLE-CALC.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 NUM1 PIC 9(5).
      01 NUM2 PIC 9(5).
      01 RESULT PIC 9(6).
      PROCEDURE DIVISION.
      MAIN.
          MOVE 100 TO NUM1
          MOVE 200 TO NUM2
          ADD NUM1 TO NUM2 GIVING RESULT
          DISPLAY RESULT
          STOP RUN.
    `,
    medium: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. PAYMENT-PROCESSOR.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 PAYMENT-RECORD.
         05 ACCOUNT-NUMBER    PIC 9(12).
         05 ROUTING-NUMBER    PIC 9(9).
         05 AMOUNT           PIC 9(10)V99.
         05 TRANSACTION-TYPE PIC X(10).
         05 STATUS-CODE      PIC X(2).
      01 VALIDATION-FLAGS.
         05 ACCOUNT-VALID    PIC X VALUE 'N'.
         05 AMOUNT-VALID     PIC X VALUE 'N'.
         05 ROUTING-VALID    PIC X VALUE 'N'.
      PROCEDURE DIVISION.
      MAIN-PROCESS.
          PERFORM INITIALIZE-PAYMENT
          PERFORM VALIDATE-PAYMENT
          PERFORM PROCESS-TRANSACTION
          PERFORM GENERATE-RESPONSE
          STOP RUN.
      INITIALIZE-PAYMENT.
          MOVE SPACES TO PAYMENT-RECORD
          MOVE 'PENDING' TO STATUS-CODE.
      VALIDATE-PAYMENT.
          PERFORM VALIDATE-ACCOUNT
          PERFORM VALIDATE-AMOUNT
          PERFORM VALIDATE-ROUTING.
      VALIDATE-ACCOUNT.
          IF ACCOUNT-NUMBER NUMERIC AND ACCOUNT-NUMBER > 0
             MOVE 'Y' TO ACCOUNT-VALID
          END-IF.
      VALIDATE-AMOUNT.
          IF AMOUNT NUMERIC AND AMOUNT > 0
             MOVE 'Y' TO AMOUNT-VALID
          END-IF.
      VALIDATE-ROUTING.
          IF ROUTING-NUMBER NUMERIC AND ROUTING-NUMBER > 0
             MOVE 'Y' TO ROUTING-VALID
          END-IF.
      PROCESS-TRANSACTION.
          IF ACCOUNT-VALID = 'Y' AND AMOUNT-VALID = 'Y' AND ROUTING-VALID = 'Y'
             MOVE 'APPROVED' TO STATUS-CODE
             PERFORM EXECUTE-TRANSFER
          ELSE
             MOVE 'REJECTED' TO STATUS-CODE
          END-IF.
      EXECUTE-TRANSFER.
          DISPLAY "Executing transfer for amount: " AMOUNT.
      GENERATE-RESPONSE.
          DISPLAY "Transaction Status: " STATUS-CODE.
    `,
    complex: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. TRADE-FINANCE-PROCESSOR.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 TRADE-DETAILS.
         05 TRADE-ID         PIC X(20).
         05 IMPORTER-INFO.
            10 IMPORTER-ID   PIC 9(12).
            10 IMPORTER-NAME PIC X(50).
            10 IMPORTER-COUNTRY PIC X(3).
         05 EXPORTER-INFO.
            10 EXPORTER-ID   PIC 9(12).
            10 EXPORTER-NAME PIC X(50).
            10 EXPORTER-COUNTRY PIC X(3).
         05 FINANCIAL-DETAILS.
            10 TRADE-VALUE   PIC 9(15)V99.
            10 CURRENCY-CODE PIC X(3).
            10 PAYMENT-TERMS PIC X(20).
            10 LETTER-OF-CREDIT-NUM PIC X(30).
      01 COMPLIANCE-CHECKS.
         05 SANCTIONS-STATUS PIC X(10).
         05 AML-STATUS      PIC X(10).
         05 FATF-STATUS     PIC X(10).
         05 COUNTRY-RISK    PIC 9(2).
      01 PROCESSING-VARIABLES.
         05 CURRENT-STEP    PIC 9(2).
         05 ERROR-COUNT     PIC 9(3).
         05 WARNING-COUNT   PIC 9(3).
         05 PROCESSING-TIME PIC 9(8).
      PROCEDURE DIVISION.
      MAIN-WORKFLOW.
          PERFORM INITIALIZE-TRADE
          PERFORM VALIDATE-TRADE-DATA
          PERFORM COMPLIANCE-SCREENING
          PERFORM RISK-ASSESSMENT
          PERFORM DOCUMENTATION-CHECK
          PERFORM APPROVAL-WORKFLOW
          PERFORM GENERATE-REPORTS
          STOP RUN.
      INITIALIZE-TRADE.
          MOVE SPACES TO TRADE-DETAILS
          MOVE ZEROS TO COMPLIANCE-CHECKS
          MOVE 1 TO CURRENT-STEP.
      VALIDATE-TRADE-DATA.
          ADD 1 TO CURRENT-STEP
          PERFORM VALIDATE-PARTIES
          PERFORM VALIDATE-FINANCIAL-TERMS
          PERFORM VALIDATE-DOCUMENTATION.
      VALIDATE-PARTIES.
          IF IMPORTER-ID NOT NUMERIC OR EXPORTER-ID NOT NUMERIC
             ADD 1 TO ERROR-COUNT
          END-IF.
      VALIDATE-FINANCIAL-TERMS.
          IF TRADE-VALUE NOT NUMERIC OR TRADE-VALUE <= 0
             ADD 1 TO ERROR-COUNT
          END-IF
          IF CURRENCY-CODE = SPACES
             ADD 1 TO ERROR-COUNT
          END-IF.
      VALIDATE-DOCUMENTATION.
          IF LETTER-OF-CREDIT-NUM = SPACES
             ADD 1 TO WARNING-COUNT
          END-IF.
      COMPLIANCE-SCREENING.
          ADD 1 TO CURRENT-STEP
          PERFORM SANCTIONS-CHECK
          PERFORM AML-CHECK
          PERFORM FATF-CHECK.
      SANCTIONS-CHECK.
          MOVE 'CLEAR' TO SANCTIONS-STATUS.
      AML-CHECK.
          MOVE 'CLEAR' TO AML-STATUS.
      FATF-CHECK.
          MOVE 'CLEAR' TO FATF-STATUS.
      RISK-ASSESSMENT.
          ADD 1 TO CURRENT-STEP
          COMPUTE COUNTRY-RISK = 5.
      DOCUMENTATION-CHECK.
          ADD 1 TO CURRENT-STEP.
      APPROVAL-WORKFLOW.
          ADD 1 TO CURRENT-STEP
          IF ERROR-COUNT = 0
             DISPLAY "Trade approved for processing"
          ELSE
             DISPLAY "Trade requires manual review"
          END-IF.
      GENERATE-REPORTS.
          ADD 1 TO CURRENT-STEP
          DISPLAY "Processing completed in step: " CURRENT-STEP.
    `
  };

  beforeAll(async () => {
    // Initialize performance profiler
    profiler = new PerformanceProfiler();

    // Create Express app
    app = express();
    app.use(express.json({ limit: '10mb' }));

    // Initialize system components
    await cobolTranspiler.initialize();
    await cobolRBAC.initialize();
    await quotaManager.initialize();

    // Setup performance testing routes
    setupPerformanceRoutes(app);

    mockDb = cobolTranspiler.db;
  });

  afterAll(async () => {
    await cobolTranspiler.shutdown();
    await cobolRBAC.shutdown();
    await quotaManager.shutdown();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    profiler = new PerformanceProfiler();

    // Setup test user with high quota
    const userId = 'perf-test-user';
    const customerId = 'perf-test-customer';
    
    cobolRBAC.permissions.set(userId, new Set([
      'transpiler.execute',
      'banking.fis.access',
      'banking.fiserv.access',
      'banking.temenos.access',
      'banking.tcs.access'
    ]));

    quotaManager.quotaCache.set(`quota:${customerId}`, {
      customerId,
      tier: 'enterprise',
      monthlyQuota: 10000,
      currentUsage: 0,
      overageUsage: 0,
      maxOveragePercent: 200,
      status: 'active',
      periodEnd: new Date(Date.now() + 86400000)
    });
  });

  function setupPerformanceRoutes(app) {
    // High-performance transpiler endpoint
    app.post('/api/perf/transpile',
      // Minimal middleware for performance testing
      async (req, res, next) => {
        req.user = {
          userId: req.headers['x-user-id'] || 'perf-test-user',
          customerId: req.headers['x-customer-id'] || 'perf-test-customer'
        };
        req.startTime = process.hrtime.bigint();
        next();
      },
      async (req, res) => {
        try {
          const { cobolCode, bankingSystem, targetBlockchain } = req.body;

          // Parse COBOL
          const parseResult = await cobolTranspiler.parseCobol(cobolCode, { bankingSystem });
          
          // Generate contract
          const contractResult = await cobolTranspiler.generateSmartContract(
            parseResult.ast,
            targetBlockchain || 'ethereum',
            { bankingSystem }
          );

          const endTime = process.hrtime.bigint();
          const duration = Number(endTime - req.startTime) / 1000000;

          res.json({
            success: true,
            duration,
            parseResult: {
              complexity: parseResult.complexity,
              variableCount: parseResult.variables?.length || 0,
              procedureCount: parseResult.procedures?.length || 0
            },
            contractResult: {
              codeSize: contractResult.code?.length || 0,
              language: contractResult.language
            },
            memory: process.memoryUsage()
          });

        } catch (error) {
          const endTime = process.hrtime.bigint();
          const duration = Number(endTime - req.startTime) / 1000000;

          res.status(500).json({
            success: false,
            error: error.message,
            duration
          });
        }
      }
    );

    // Batch processing endpoint
    app.post('/api/perf/batch-transpile', async (req, res) => {
      try {
        const { programs } = req.body;
        const startTime = process.hrtime.bigint();
        const results = [];

        for (const program of programs) {
          const programStartTime = process.hrtime.bigint();
          
          const parseResult = await cobolTranspiler.parseCobol(program.cobolCode, { 
            bankingSystem: program.bankingSystem 
          });
          
          const contractResult = await cobolTranspiler.generateSmartContract(
            parseResult.ast,
            program.targetBlockchain || 'ethereum',
            { bankingSystem: program.bankingSystem }
          );

          const programEndTime = process.hrtime.bigint();
          const programDuration = Number(programEndTime - programStartTime) / 1000000;

          results.push({
            id: program.id,
            success: true,
            duration: programDuration,
            codeSize: contractResult.code?.length || 0
          });
        }

        const endTime = process.hrtime.bigint();
        const totalDuration = Number(endTime - startTime) / 1000000;

        res.json({
          success: true,
          totalDuration,
          averageDuration: totalDuration / programs.length,
          results,
          memory: process.memoryUsage()
        });

      } catch (error) {
        res.status(500).json({
          success: false,
          error: error.message
        });
      }
    });
  }

  describe('Single Request Performance', () => {
    test('should process simple COBOL program within performance targets', async () => {
      profiler.start();

      const response = await request(app)
        .post('/api/perf/transpile')
        .send({
          cobolCode: cobolPrograms.simple,
          bankingSystem: 'fis',
          targetBlockchain: 'ethereum'
        })
        .set('x-user-id', 'perf-test-user')
        .set('x-customer-id', 'perf-test-customer');

      profiler.stop();
      profiler.recordMetric('responseTime', response.body.duration);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.duration).toBeLessThan(5000); // < 5 seconds
      expect(response.body.parseResult.complexity).toBeDefined();
      expect(response.body.contractResult.codeSize).toBeGreaterThan(0);

      const report = profiler.generateReport();
      expect(report.responseTime.avg).toBeLessThan(5000);
    });

    test('should process medium complexity COBOL within performance targets', async () => {
      profiler.start();

      const response = await request(app)
        .post('/api/perf/transpile')
        .send({
          cobolCode: cobolPrograms.medium,
          bankingSystem: 'fiserv',
          targetBlockchain: 'ethereum'
        });

      profiler.stop();
      profiler.recordMetric('responseTime', response.body.duration);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.duration).toBeLessThan(15000); // < 15 seconds
      expect(response.body.parseResult.variableCount).toBeGreaterThan(5);
      expect(response.body.parseResult.procedureCount).toBeGreaterThan(3);

      const report = profiler.generateReport();
      expect(report.responseTime.avg).toBeLessThan(15000);
    });

    test('should process complex COBOL within performance targets', async () => {
      profiler.start();

      const response = await request(app)
        .post('/api/perf/transpile')
        .send({
          cobolCode: cobolPrograms.complex,
          bankingSystem: 'tcs',
          targetBlockchain: 'corda'
        });

      profiler.stop();
      profiler.recordMetric('responseTime', response.body.duration);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.duration).toBeLessThan(30000); // < 30 seconds
      expect(response.body.parseResult.variableCount).toBeGreaterThan(10);
      expect(response.body.contractResult.codeSize).toBeGreaterThan(1000);

      const report = profiler.generateReport();
      expect(report.responseTime.avg).toBeLessThan(30000);
    });
  });

  describe('Concurrent Load Testing', () => {
    test('should handle 10 concurrent simple transpilations', async () => {
      profiler.start();

      const concurrentRequests = Array.from({ length: 10 }, (_, i) =>
        request(app)
          .post('/api/perf/transpile')
          .send({
            cobolCode: cobolPrograms.simple,
            bankingSystem: 'fis',
            targetBlockchain: 'ethereum'
          })
          .set('x-user-id', `perf-user-${i}`)
          .set('x-customer-id', 'perf-test-customer')
      );

      const responses = await Promise.all(concurrentRequests);
      
      profiler.stop();

      // Record metrics for all responses
      responses.forEach(response => {
        if (response.status === 200) {
          profiler.recordMetric('responseTime', response.body.duration);
        } else {
          profiler.recordMetric('errors', response.status);
        }
      });

      const report = profiler.generateReport();

      // All requests should succeed
      expect(responses.every(r => r.status === 200)).toBe(true);
      
      // Performance targets
      expect(report.responseTime.p95).toBeLessThan(10000); // P95 < 10 seconds
      expect(report.errorRate).toBe(0); // No errors
      expect(report.throughput.requestsPerSecond).toBeGreaterThan(1); // > 1 RPS
    });

    test('should handle 25 concurrent medium complexity transpilations', async () => {
      profiler.start();

      const concurrentRequests = Array.from({ length: 25 }, (_, i) =>
        request(app)
          .post('/api/perf/transpile')
          .send({
            cobolCode: cobolPrograms.medium,
            bankingSystem: ['fis', 'fiserv', 'temenos', 'tcs'][i % 4],
            targetBlockchain: ['ethereum', 'xrp', 'algorand', 'corda'][i % 4]
          })
          .set('x-user-id', `perf-user-${i}`)
          .set('x-customer-id', 'perf-test-customer')
      );

      const responses = await Promise.all(concurrentRequests);
      
      profiler.stop();

      responses.forEach(response => {
        if (response.status === 200) {
          profiler.recordMetric('responseTime', response.body.duration);
        } else {
          profiler.recordMetric('errors', response.status);
        }
      });

      const report = profiler.generateReport();

      // Performance expectations for higher load
      expect(responses.filter(r => r.status === 200).length).toBeGreaterThan(20); // > 80% success rate
      expect(report.responseTime.p95).toBeLessThan(25000); // P95 < 25 seconds
      expect(report.errorRate).toBeLessThan(20); // < 20% error rate
    });

    test('should handle sustained load over time', async () => {
      const testDuration = 30000; // 30 seconds
      const requestInterval = 1000; // 1 request per second
      const startTime = Date.now();
      const responses = [];
      
      profiler.start();

      while (Date.now() - startTime < testDuration) {
        const requestStartTime = Date.now();
        
        try {
          const response = await request(app)
            .post('/api/perf/transpile')
            .send({
              cobolCode: cobolPrograms.simple,
              bankingSystem: 'fis',
              targetBlockchain: 'ethereum'
            })
            .timeout(10000); // 10 second timeout

          responses.push(response);
          profiler.recordMetric('responseTime', response.body?.duration || 0);

        } catch (error) {
          profiler.recordMetric('errors', error.status || 500);
        }

        // Wait for next interval
        const elapsed = Date.now() - requestStartTime;
        if (elapsed < requestInterval) {
          await new Promise(resolve => setTimeout(resolve, requestInterval - elapsed));
        }
      }

      profiler.stop();
      const report = profiler.generateReport();

      // Sustained load expectations
      expect(responses.length).toBeGreaterThan(25); // Should process multiple requests
      expect(report.responseTime.avg).toBeLessThan(8000); // Average < 8 seconds
      expect(report.throughput.requestsPerSecond).toBeGreaterThan(0.8); // > 0.8 RPS sustained
    });
  });

  describe('Batch Processing Performance', () => {
    test('should efficiently process batch of 5 programs', async () => {
      const batchPrograms = [
        { id: 1, cobolCode: cobolPrograms.simple, bankingSystem: 'fis', targetBlockchain: 'ethereum' },
        { id: 2, cobolCode: cobolPrograms.medium, bankingSystem: 'fiserv', targetBlockchain: 'xrp' },
        { id: 3, cobolCode: cobolPrograms.simple, bankingSystem: 'temenos', targetBlockchain: 'algorand' },
        { id: 4, cobolCode: cobolPrograms.medium, bankingSystem: 'tcs', targetBlockchain: 'corda' },
        { id: 5, cobolCode: cobolPrograms.simple, bankingSystem: 'fis', targetBlockchain: 'ethereum' }
      ];

      profiler.start();

      const response = await request(app)
        .post('/api/perf/batch-transpile')
        .send({ programs: batchPrograms })
        .timeout(60000); // 1 minute timeout

      profiler.stop();

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.results).toHaveLength(5);
      expect(response.body.totalDuration).toBeLessThan(45000); // < 45 seconds total
      expect(response.body.averageDuration).toBeLessThan(10000); // < 10 seconds average

      // All programs should process successfully
      expect(response.body.results.every(r => r.success)).toBe(true);
    });

    test('should handle mixed complexity batch efficiently', async () => {
      const mixedBatch = [
        { id: 1, cobolCode: cobolPrograms.simple, bankingSystem: 'fis' },
        { id: 2, cobolCode: cobolPrograms.complex, bankingSystem: 'tcs' },
        { id: 3, cobolCode: cobolPrograms.medium, bankingSystem: 'fiserv' },
        { id: 4, cobolCode: cobolPrograms.simple, bankingSystem: 'temenos' },
        { id: 5, cobolCode: cobolPrograms.medium, bankingSystem: 'fis' },
        { id: 6, cobolCode: cobolPrograms.simple, bankingSystem: 'tcs' }
      ];

      const response = await request(app)
        .post('/api/perf/batch-transpile')
        .send({ programs: mixedBatch })
        .timeout(120000); // 2 minute timeout

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.results).toHaveLength(6);
      
      // Complex programs should take longer but still complete
      const complexResult = response.body.results.find(r => r.id === 2);
      const simpleResult = response.body.results.find(r => r.id === 1);
      
      expect(complexResult.duration).toBeGreaterThan(simpleResult.duration);
      expect(complexResult.codeSize).toBeGreaterThan(simpleResult.codeSize);
    });
  });

  describe('Memory Usage and Resource Management', () => {
    test('should maintain stable memory usage under load', async () => {
      const initialMemory = process.memoryUsage();
      const memoryMeasurements = [];

      // Process 20 requests while monitoring memory
      for (let i = 0; i < 20; i++) {
        await request(app)
          .post('/api/perf/transpile')
          .send({
            cobolCode: cobolPrograms.medium,
            bankingSystem: 'fis',
            targetBlockchain: 'ethereum'
          });

        const currentMemory = process.memoryUsage();
        memoryMeasurements.push(currentMemory);

        // Small delay to allow garbage collection
        await new Promise(resolve => setTimeout(resolve, 100));
      }

      const finalMemory = process.memoryUsage();
      const maxHeapUsed = Math.max(...memoryMeasurements.map(m => m.heapUsed));
      const memoryGrowth = finalMemory.heapUsed - initialMemory.heapUsed;

      // Memory should not grow excessively
      expect(memoryGrowth).toBeLessThan(100 * 1024 * 1024); // < 100MB growth
      expect(maxHeapUsed).toBeLessThan(500 * 1024 * 1024); // < 500MB max heap
    });

    test('should handle large COBOL programs without memory issues', async () => {
      // Create a large COBOL program
      const largeProgram = cobolPrograms.complex + '\n' + cobolPrograms.medium + '\n' + cobolPrograms.simple;
      
      const initialMemory = process.memoryUsage();

      const response = await request(app)
        .post('/api/perf/transpile')
        .send({
          cobolCode: largeProgram,
          bankingSystem: 'tcs',
          targetBlockchain: 'corda'
        })
        .timeout(60000);

      const finalMemory = process.memoryUsage();
      const memoryIncrease = finalMemory.heapUsed - initialMemory.heapUsed;

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(memoryIncrease).toBeLessThan(200 * 1024 * 1024); // < 200MB for large program
    });
  });

  describe('API Response Time Benchmarks', () => {
    test('should meet P95 response time targets for standard API calls', async () => {
      const responseTimes = [];
      const numRequests = 50;

      // Execute multiple requests to get statistical data
      for (let i = 0; i < numRequests; i++) {
        const startTime = Date.now();
        
        const response = await request(app)
          .post('/api/perf/transpile')
          .send({
            cobolCode: cobolPrograms.simple,
            bankingSystem: 'fis',
            targetBlockchain: 'ethereum'
          });

        const endTime = Date.now();
        const responseTime = endTime - startTime;
        
        responseTimes.push(responseTime);
        
        expect(response.status).toBe(200);
      }

      // Calculate percentiles
      const sortedTimes = responseTimes.sort((a, b) => a - b);
      const p50 = sortedTimes[Math.floor(numRequests * 0.5)];
      const p95 = sortedTimes[Math.floor(numRequests * 0.95)];
      const p99 = sortedTimes[Math.floor(numRequests * 0.99)];
      const average = responseTimes.reduce((a, b) => a + b, 0) / numRequests;

      // Performance targets
      expect(p50).toBeLessThan(3000); // P50 < 3 seconds
      expect(p95).toBeLessThan(8000); // P95 < 8 seconds
      expect(p99).toBeLessThan(15000); // P99 < 15 seconds
      expect(average).toBeLessThan(5000); // Average < 5 seconds

      console.log(`Performance Metrics:
        P50: ${p50}ms
        P95: ${p95}ms
        P99: ${p99}ms
        Average: ${average}ms
        Min: ${Math.min(...responseTimes)}ms
        Max: ${Math.max(...responseTimes)}ms
      `);
    });

    test('should maintain performance under varying load patterns', async () => {
      const loadPatterns = [
        { name: 'Low Load', concurrency: 2, duration: 10000 },
        { name: 'Medium Load', concurrency: 5, duration: 10000 },
        { name: 'High Load', concurrency: 10, duration: 10000 }
      ];

      for (const pattern of loadPatterns) {
        const results = [];
        const startTime = Date.now();

        // Create concurrent load
        const promises = Array.from({ length: pattern.concurrency }, async () => {
          const patternResults = [];
          
          while (Date.now() - startTime < pattern.duration) {
            const requestStart = Date.now();
            
            try {
              const response = await request(app)
                .post('/api/perf/transpile')
                .send({
                  cobolCode: cobolPrograms.simple,
                  bankingSystem: 'fis',
                  targetBlockchain: 'ethereum'
                })
                .timeout(20000);

              const requestEnd = Date.now();
              patternResults.push({
                success: response.status === 200,
                duration: requestEnd - requestStart,
                responseTime: response.body?.duration || 0
              });

            } catch (error) {
              patternResults.push({
                success: false,
                duration: Date.now() - requestStart,
                error: error.message
              });
            }

            // Brief pause between requests
            await new Promise(resolve => setTimeout(resolve, 500));
          }

          return patternResults;
        });

        const concurrentResults = await Promise.all(promises);
        const allResults = concurrentResults.flat();

        const successRate = (allResults.filter(r => r.success).length / allResults.length) * 100;
        const avgResponseTime = allResults
          .filter(r => r.success)
          .reduce((sum, r) => sum + r.duration, 0) / allResults.filter(r => r.success).length;

        // Performance expectations vary by load
        if (pattern.name === 'Low Load') {
          expect(successRate).toBeGreaterThan(95);
          expect(avgResponseTime).toBeLessThan(5000);
        } else if (pattern.name === 'Medium Load') {
          expect(successRate).toBeGreaterThan(90);
          expect(avgResponseTime).toBeLessThan(8000);
        } else { // High Load
          expect(successRate).toBeGreaterThan(80);
          expect(avgResponseTime).toBeLessThan(12000);
        }

        console.log(`${pattern.name} Results:
          Success Rate: ${successRate.toFixed(1)}%
          Average Response Time: ${avgResponseTime.toFixed(0)}ms
          Total Requests: ${allResults.length}
        `);
      }
    });
  });

  describe('Stress Testing and Breaking Points', () => {
    test('should gracefully handle resource exhaustion', async () => {
      // Gradually increase load until we find breaking point
      const maxConcurrency = 50;
      let successfulConcurrency = 0;

      for (let concurrency = 5; concurrency <= maxConcurrency; concurrency += 5) {
        const promises = Array.from({ length: concurrency }, () =>
          request(app)
            .post('/api/perf/transpile')
            .send({
              cobolCode: cobolPrograms.medium,
              bankingSystem: 'fis',
              targetBlockchain: 'ethereum'
            })
            .timeout(30000)
        );

        try {
          const responses = await Promise.all(promises);
          const successCount = responses.filter(r => r.status === 200).length;
          const successRate = (successCount / concurrency) * 100;

          if (successRate >= 80) {
            successfulConcurrency = concurrency;
          } else {
            console.log(`Breaking point reached at concurrency: ${concurrency}, success rate: ${successRate}%`);
            break;
          }

        } catch (error) {
          console.log(`System overloaded at concurrency: ${concurrency}`);
          break;
        }

        // Brief recovery time between tests
        await new Promise(resolve => setTimeout(resolve, 2000));
      }

      // System should handle at least 15 concurrent requests
      expect(successfulConcurrency).toBeGreaterThanOrEqual(15);
      console.log(`Maximum successful concurrency: ${successfulConcurrency}`);
    });
  });
});