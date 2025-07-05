const { describe, test, expect, beforeEach, afterEach, jest } = require('@jest/globals');
const { PerformanceOptimizer } = require('../../src/utils/performance-optimizer');

// Mock external dependencies
jest.mock('node-cache', () => {
  return jest.fn().mockImplementation(() => ({
    get: jest.fn(),
    set: jest.fn(),
    flushAll: jest.fn(),
    options: { stdTTL: 3600, maxKeys: 500 }
  }));
});

jest.mock('../adapters/cobol-transpiler', () => ({
  parseCobol: jest.fn((data) => ({ ast: 'parsed', data }))
}));

jest.mock('../adapters/templates/template-engine', () => ({
  generateTemplate: jest.fn((data) => ({ template: 'generated', data }))
}));

describe('PerformanceOptimizer', () => {
  let optimizer;
  let originalSetInterval;
  let originalGc;

  beforeEach(() => {
    optimizer = new PerformanceOptimizer();
    originalSetInterval = global.setInterval;
    originalGc = global.gc;
    global.setInterval = jest.fn();
    global.gc = jest.fn();
  });

  afterEach(() => {
    global.setInterval = originalSetInterval;
    global.gc = originalGc;
    if (optimizer) {
      optimizer.cleanup();
    }
  });

  describe('Constructor and Initialization', () => {
    test('should initialize with default configurations', () => {
      expect(optimizer.cache).toBeDefined();
      expect(optimizer.memoryCache).toBeInstanceOf(Map);
      expect(optimizer.performanceMetrics).toBeDefined();
      expect(optimizer.optimizationStrategies).toBeDefined();
    });

    test('should calculate memory limit correctly', () => {
      expect(optimizer.memoryLimit).toBeGreaterThan(0);
      expect(typeof optimizer.memoryLimit).toBe('number');
    });

    test('should set up performance monitoring', () => {
      expect(global.setInterval).toHaveBeenCalledWith(
        expect.any(Function),
        30000
      );
    });

    test('should initialize connection pool', () => {
      expect(optimizer.connectionPool).toEqual({
        maxConnections: 50,
        currentConnections: 0,
        availableConnections: [],
        waitingRequests: []
      });
    });

    test('should initialize batch processor', () => {
      expect(optimizer.batchProcessor).toEqual({
        batchSize: 10,
        maxWaitTime: 1000,
        currentBatch: [],
        batchTimeout: null
      });
    });
  });

  describe('Performance Metrics Collection', () => {
    test('should collect memory usage metrics', () => {
      optimizer.collectPerformanceMetrics();
      
      expect(optimizer.performanceMetrics.memoryUsage).toHaveLength(1);
      expect(optimizer.performanceMetrics.memoryUsage[0]).toMatchObject({
        timestamp: expect.any(Number),
        heapUsed: expect.any(Number),
        heapTotal: expect.any(Number),
        external: expect.any(Number),
        rss: expect.any(Number)
      });
    });

    test('should collect CPU usage metrics', () => {
      optimizer.collectPerformanceMetrics();
      
      expect(optimizer.performanceMetrics.cpuUsage).toHaveLength(1);
      expect(optimizer.performanceMetrics.cpuUsage[0]).toMatchObject({
        timestamp: expect.any(Number),
        user: expect.any(Number),
        system: expect.any(Number)
      });
    });

    test('should limit metrics history size', () => {
      // Add more than 100 metrics
      for (let i = 0; i < 120; i++) {
        optimizer.collectPerformanceMetrics();
      }
      
      expect(optimizer.performanceMetrics.memoryUsage.length).toBeLessThanOrEqual(100);
      expect(optimizer.performanceMetrics.cpuUsage.length).toBeLessThanOrEqual(100);
    });
  });

  describe('Performance Analysis', () => {
    beforeEach(() => {
      // Add some sample metrics
      for (let i = 0; i < 10; i++) {
        optimizer.performanceMetrics.memoryUsage.push({
          timestamp: Date.now() - (10 - i) * 1000,
          heapUsed: 1000000 + (i * 100000), // Increasing memory usage
          heapTotal: 2000000,
          external: 100000,
          rss: 1500000
        });
      }
    });

    test('should analyze memory trends', () => {
      const trend = optimizer.analyzeMemoryTrend();
      
      expect(trend).toMatchObject({
        increasing: expect.any(Boolean),
        critical: expect.any(Boolean),
        avgMemory: expect.any(Number)
      });
    });

    test('should detect increasing memory trend', () => {
      const trend = optimizer.analyzeMemoryTrend();
      expect(trend.increasing).toBe(true);
    });

    test('should analyze CPU trends', () => {
      // Add CPU metrics
      for (let i = 0; i < 10; i++) {
        optimizer.performanceMetrics.cpuUsage.push({
          timestamp: Date.now() - (10 - i) * 1000,
          user: 90000000, // High CPU usage
          system: 10000000
        });
      }
      
      const trend = optimizer.analyzeCpuTrend();
      
      expect(trend).toMatchObject({
        high: expect.any(Boolean),
        avg: expect.any(Number)
      });
      expect(trend.high).toBe(true);
    });

    test('should analyze cache efficiency', () => {
      optimizer.performanceMetrics.cacheHitRate = { hits: 70, misses: 30 };
      
      const efficiency = optimizer.analyzeCacheEfficiency();
      
      expect(efficiency).toMatchObject({
        low: expect.any(Boolean),
        rate: expect.any(Number)
      });
      expect(efficiency.rate).toBe(0.7);
      expect(efficiency.low).toBe(false);
    });

    test('should detect low cache efficiency', () => {
      optimizer.performanceMetrics.cacheHitRate = { hits: 30, misses: 70 };
      
      const efficiency = optimizer.analyzeCacheEfficiency();
      
      expect(efficiency.low).toBe(true);
      expect(efficiency.rate).toBe(0.3);
    });

    test('should generate performance analysis with recommendations', () => {
      const analysis = optimizer.analyzePerformance();
      
      expect(analysis).toMatchObject({
        memoryTrend: expect.any(Object),
        cpuTrend: expect.any(Object),
        cacheEfficiency: expect.any(Object),
        recommendations: expect.any(Array)
      });
    });
  });

  describe('Optimization Strategies', () => {
    test('should optimize COBOL parsing', () => {
      const result = optimizer.optimizeCobolParsing();
      
      expect(result).toMatchObject({
        name: 'COBOL_PARSING',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(result.strategies).toContain('Enable AST caching for repeated COBOL structures');
    });

    test('should optimize template generation', () => {
      const result = optimizer.optimizeTemplateGeneration();
      
      expect(result).toMatchObject({
        name: 'TEMPLATE_GENERATION',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(result.strategies).toContain('Cache compiled templates in memory');
    });

    test('should optimize blockchain deployment', () => {
      const result = optimizer.optimizeBlockchainDeployment();
      
      expect(result).toMatchObject({
        name: 'BLOCKCHAIN_DEPLOYMENT',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(result.strategies).toContain('Batch multiple deployments together');
    });

    test('should optimize database queries', () => {
      const result = optimizer.optimizeDatabaseQueries();
      
      expect(result).toMatchObject({
        name: 'DATABASE_QUERIES',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(result.strategies).toContain('Add database query result caching');
    });

    test('should optimize memory management', () => {
      const result = optimizer.optimizeMemoryManagement();
      
      expect(result).toMatchObject({
        name: 'MEMORY_MANAGEMENT',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(result.strategies).toContain('Forced garbage collection');
      expect(global.gc).toHaveBeenCalled();
    });

    test('should optimize caching strategy', () => {
      const result = optimizer.optimizeCaching();
      
      expect(result).toMatchObject({
        name: 'CACHING',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(optimizer.cache.options.stdTTL).toBe(7200);
    });

    test('should optimize concurrency', () => {
      const result = optimizer.optimizeConcurrency();
      
      expect(result).toMatchObject({
        name: 'CONCURRENCY',
        timestamp: expect.any(Number),
        strategies: expect.any(Array)
      });
      expect(result.strategies).toContain('Implement worker thread pool');
    });
  });

  describe('Caching System', () => {
    test('should cache and retrieve data from memory cache', () => {
      const key = 'test-key';
      const data = { test: 'data' };
      
      optimizer.setCached(key, data);
      const retrieved = optimizer.getCached(key);
      
      expect(retrieved).toEqual(data);
      expect(optimizer.performanceMetrics.cacheHitRate.hits).toBe(1);
    });

    test('should fall back to node cache when memory cache misses', () => {
      const key = 'test-key';
      const data = { test: 'data' };
      
      optimizer.cache.get.mockReturnValue(data);
      const retrieved = optimizer.getCached(key);
      
      expect(retrieved).toEqual(data);
      expect(optimizer.performanceMetrics.cacheHitRate.hits).toBe(1);
    });

    test('should record cache misses', () => {
      optimizer.cache.get.mockReturnValue(undefined);
      
      const retrieved = optimizer.getCached('non-existent-key');
      
      expect(retrieved).toBeNull();
      expect(optimizer.performanceMetrics.cacheHitRate.misses).toBe(1);
    });

    test('should set data in both cache layers', () => {
      const key = 'test-key';
      const data = { test: 'data' };
      
      optimizer.setCached(key, data, 1800);
      
      expect(optimizer.cache.set).toHaveBeenCalledWith(key, data, 1800);
      expect(optimizer.memoryCache.has(key)).toBe(true);
    });
  });

  describe('Batch Processing', () => {
    test('should add operations to batch', () => {
      const operation = 'COBOL_PARSE';
      const data = { cobol: 'test code' };
      
      optimizer.addToBatch(operation, data);
      
      expect(optimizer.batchProcessor.currentBatch).toHaveLength(1);
      expect(optimizer.batchProcessor.currentBatch[0]).toMatchObject({
        operation,
        data,
        timestamp: expect.any(Number)
      });
    });

    test('should process batch when size limit reached', () => {
      const spy = jest.spyOn(optimizer, 'processBatch');
      
      // Add operations to fill the batch
      for (let i = 0; i < 10; i++) {
        optimizer.addToBatch('TEST_OP', { id: i });
      }
      
      expect(spy).toHaveBeenCalled();
    });

    test('should process batch after timeout', (done) => {
      const spy = jest.spyOn(optimizer, 'processBatch');
      
      optimizer.addToBatch('TEST_OP', { id: 1 });
      
      setTimeout(() => {
        expect(spy).toHaveBeenCalled();
        done();
      }, 1100);
    });

    test('should group operations by type', () => {
      const batch = [
        { operation: 'COBOL_PARSE', data: { id: 1 } },
        { operation: 'TEMPLATE_GENERATE', data: { id: 2 } },
        { operation: 'COBOL_PARSE', data: { id: 3 } }
      ];
      
      const grouped = optimizer.groupOperationsByType(batch);
      
      expect(grouped.COBOL_PARSE).toHaveLength(2);
      expect(grouped.TEMPLATE_GENERATE).toHaveLength(1);
    });

    test('should execute batch COBOL parsing', () => {
      const operations = [
        { data: { cobol: 'test1' } },
        { data: { cobol: 'test2' } }
      ];
      
      const results = optimizer.batchCobolParse(operations);
      
      expect(results).toHaveLength(2);
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(true);
    });

    test('should execute batch template generation', () => {
      const operations = [
        { data: { template: 'test1' } },
        { data: { template: 'test2' } }
      ];
      
      const results = optimizer.batchTemplateGenerate(operations);
      
      expect(results).toHaveLength(2);
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(true);
    });

    test('should execute batch blockchain deployment', () => {
      const operations = [
        { data: { contract: 'test1' } },
        { data: { contract: 'test2' } }
      ];
      
      const results = optimizer.batchBlockchainDeploy(operations);
      
      expect(results).toHaveLength(2);
      expect(results[0].success).toBe(true);
      expect(results[1].success).toBe(true);
    });
  });

  describe('Connection Pool Management', () => {
    test('should acquire connection from available pool', async () => {
      optimizer.connectionPool.availableConnections.push({ id: 'test-conn' });
      
      const connection = await optimizer.acquireConnection();
      
      expect(connection.id).toBe('test-conn');
      expect(optimizer.connectionPool.currentConnections).toBe(1);
      expect(optimizer.connectionPool.availableConnections).toHaveLength(0);
    });

    test('should create new connection when pool is empty', async () => {
      const connection = await optimizer.acquireConnection();
      
      expect(connection).toMatchObject({
        id: expect.any(Number),
        created: expect.any(Number),
        used: 0
      });
      expect(optimizer.connectionPool.currentConnections).toBe(1);
    });

    test('should queue requests when pool is at capacity', (done) => {
      // Fill the pool to capacity
      optimizer.connectionPool.currentConnections = optimizer.connectionPool.maxConnections;
      
      optimizer.acquireConnection().then(connection => {
        expect(connection).toBeDefined();
        done();
      });
      
      expect(optimizer.connectionPool.waitingRequests).toHaveLength(1);
      
      // Release a connection to fulfill the waiting request
      optimizer.releaseConnection({ id: 'test' });
    });

    test('should release connection properly', () => {
      const connection = { id: 'test-conn' };
      optimizer.connectionPool.currentConnections = 1;
      
      optimizer.releaseConnection(connection);
      
      expect(optimizer.connectionPool.currentConnections).toBe(0);
      expect(optimizer.connectionPool.availableConnections).toContain(connection);
    });

    test('should fulfill waiting requests when releasing connection', () => {
      const connection = { id: 'test-conn' };
      optimizer.connectionPool.currentConnections = 1;
      
      let resolvedConnection;
      optimizer.connectionPool.waitingRequests.push({
        resolve: (conn) => { resolvedConnection = conn; },
        reject: () => {}
      });
      
      optimizer.releaseConnection(connection);
      
      expect(resolvedConnection).toBe(connection);
      expect(optimizer.connectionPool.currentConnections).toBe(1);
    });
  });

  describe('Performance Reporting', () => {
    beforeEach(() => {
      // Add sample response time data
      optimizer.performanceMetrics.responseTime.push(
        { operation: 'TEST', duration: 100, batchSize: 1, timestamp: Date.now() },
        { operation: 'TEST', duration: 200, batchSize: 1, timestamp: Date.now() },
        { operation: 'TEST', duration: 150, batchSize: 1, timestamp: Date.now() }
      );
      
      optimizer.performanceMetrics.cacheHitRate = { hits: 80, misses: 20 };
    });

    test('should calculate average response time', () => {
      const avgTime = optimizer.calculateAverageResponseTime();
      expect(avgTime).toBe(150); // (100 + 200 + 150) / 3
    });

    test('should get current memory usage', () => {
      const memUsage = optimizer.getCurrentMemoryUsage();
      
      expect(memUsage).toMatchObject({
        heapUsed: expect.any(Number),
        heapTotal: expect.any(Number),
        external: expect.any(Number),
        rss: expect.any(Number),
        percentage: expect.any(Number)
      });
    });

    test('should get current CPU usage', () => {
      const cpuUsage = optimizer.getCurrentCpuUsage();
      
      expect(cpuUsage).toMatchObject({
        user: expect.any(Number),
        system: expect.any(Number),
        total: expect.any(Number)
      });
    });

    test('should calculate cache hit rate', () => {
      const hitRate = optimizer.calculateCacheHitRate();
      expect(hitRate).toBe(80); // 80 hits out of 100 total
    });

    test('should get connection pool status', () => {
      const status = optimizer.getConnectionPoolStatus();
      
      expect(status).toMatchObject({
        maxConnections: 50,
        currentConnections: expect.any(Number),
        availableConnections: expect.any(Number),
        waitingRequests: expect.any(Number)
      });
    });

    test('should generate performance report', () => {
      const report = optimizer.getPerformanceReport();
      
      expect(report).toMatchObject({
        timestamp: expect.any(Number),
        metrics: {
          averageResponseTime: expect.any(Number),
          memoryUsage: expect.any(Object),
          cpuUsage: expect.any(Object),
          cacheHitRate: expect.any(Number),
          connectionPoolStatus: expect.any(Object)
        },
        optimizations: expect.any(Array),
        recommendations: expect.any(Array)
      });
    });

    test('should generate recommendations based on metrics', () => {
      // Mock high memory usage
      jest.spyOn(optimizer, 'getCurrentMemoryUsage').mockReturnValue({
        percentage: 85
      });
      
      const recommendations = optimizer.generateRecommendations();
      
      expect(recommendations).toContain('High memory usage detected - consider memory optimization');
    });
  });

  describe('Worker Thread Management', () => {
    test('should create worker pool', () => {
      const workers = optimizer.createWorkerPool(4);
      
      expect(workers).toHaveLength(4);
      workers.forEach(worker => {
        expect(worker).toMatchObject({
          id: expect.any(Number),
          busy: false,
          tasks: 0,
          created: expect.any(Number)
        });
      });
    });

    test('should create individual worker', () => {
      const worker = optimizer.createWorker();
      
      expect(worker).toMatchObject({
        id: expect.any(Number),
        busy: false,
        tasks: 0,
        created: expect.any(Number)
      });
    });
  });

  describe('Cleanup and Resource Management', () => {
    test('should cleanup all resources', () => {
      // Set up some state to cleanup
      optimizer.memoryCache.set('test', 'data');
      optimizer.batchProcessor.currentBatch = [{ test: 'data' }];
      optimizer.batchProcessor.batchTimeout = setTimeout(() => {}, 1000);
      optimizer.connectionPool.availableConnections = [{ id: 'test' }];
      optimizer.connectionPool.waitingRequests = [{ resolve: () => {} }];
      
      optimizer.cleanup();
      
      expect(optimizer.cache.flushAll).toHaveBeenCalled();
      expect(optimizer.memoryCache.size).toBe(0);
      expect(optimizer.batchProcessor.currentBatch).toHaveLength(0);
      expect(optimizer.connectionPool.availableConnections).toHaveLength(0);
      expect(optimizer.connectionPool.waitingRequests).toHaveLength(0);
    });

    test('should handle cleanup with null timeout', () => {
      optimizer.batchProcessor.batchTimeout = null;
      
      expect(() => optimizer.cleanup()).not.toThrow();
    });
  });

  describe('Edge Cases and Error Handling', () => {
    test('should handle empty performance metrics gracefully', () => {
      optimizer.performanceMetrics.responseTime = [];
      
      const avgTime = optimizer.calculateAverageResponseTime();
      expect(avgTime).toBe(0);
    });

    test('should handle zero cache hits/misses', () => {
      optimizer.performanceMetrics.cacheHitRate = { hits: 0, misses: 0 };
      
      const efficiency = optimizer.analyzeCacheEfficiency();
      expect(efficiency.rate).toBe(0);
      expect(efficiency.low).toBe(false);
    });

    test('should handle batch processing errors gracefully', () => {
      const operations = [{ data: { invalid: 'data' } }];
      
      expect(() => optimizer.batchCobolParse(operations)).not.toThrow();
    });

    test('should handle memory management when gc is not available', () => {
      global.gc = undefined;
      
      expect(() => optimizer.optimizeMemoryManagement()).not.toThrow();
    });
  });
});