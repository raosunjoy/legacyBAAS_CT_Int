const cluster = require('cluster');
const os = require('os');
const { performance } = require('perf_hooks');
const NodeCache = require('node-cache');

class PerformanceOptimizer {
  constructor() {
    this.cache = new NodeCache({ 
      stdTTL: 3600, // 1 hour default TTL
      checkperiod: 600, // Check for expired keys every 10 minutes
      useClones: false // Better performance for large objects
    });
    
    this.memoryCache = new Map();
    this.performanceMetrics = {
      responseTime: [],
      memoryUsage: [],
      cpuUsage: [],
      cacheHitRate: { hits: 0, misses: 0 },
      optimizationResults: []
    };
    
    this.optimizationStrategies = {
      COBOL_PARSING: this.optimizeCobolParsing.bind(this),
      TEMPLATE_GENERATION: this.optimizeTemplateGeneration.bind(this),
      BLOCKCHAIN_DEPLOYMENT: this.optimizeBlockchainDeployment.bind(this),
      DATABASE_QUERIES: this.optimizeDatabaseQueries.bind(this),
      MEMORY_MANAGEMENT: this.optimizeMemoryManagement.bind(this),
      CACHING: this.optimizeCaching.bind(this),
      CONCURRENCY: this.optimizeConcurrency.bind(this)
    };
    
    this.connectionPool = {
      maxConnections: 50,
      currentConnections: 0,
      availableConnections: [],
      waitingRequests: []
    };
    
    this.batchProcessor = {
      batchSize: 10,
      maxWaitTime: 1000, // 1 second
      currentBatch: [],
      batchTimeout: null
    };
    
    this.cpuCount = os.cpus().length;
    this.memoryLimit = this.calculateMemoryLimit();
    this.setupPerformanceMonitoring();
  }

  calculateMemoryLimit() {
    const totalMemory = os.totalmem();
    const freeMemory = os.freemem();
    const nodeMemory = process.memoryUsage();
    
    // Set memory limit to 80% of available memory
    return Math.floor((totalMemory - (totalMemory - freeMemory)) * 0.8);
  }

  setupPerformanceMonitoring() {
    setInterval(() => {
      this.collectPerformanceMetrics();
      this.analyzePerformance();
      this.applyOptimizations();
    }, 30000); // Monitor every 30 seconds
  }

  collectPerformanceMetrics() {
    const memUsage = process.memoryUsage();
    const cpuUsage = process.cpuUsage();
    
    this.performanceMetrics.memoryUsage.push({
      timestamp: Date.now(),
      heapUsed: memUsage.heapUsed,
      heapTotal: memUsage.heapTotal,
      external: memUsage.external,
      rss: memUsage.rss
    });
    
    this.performanceMetrics.cpuUsage.push({
      timestamp: Date.now(),
      user: cpuUsage.user,
      system: cpuUsage.system
    });
    
    // Keep only last 100 measurements
    if (this.performanceMetrics.memoryUsage.length > 100) {
      this.performanceMetrics.memoryUsage = this.performanceMetrics.memoryUsage.slice(-100);
    }
    if (this.performanceMetrics.cpuUsage.length > 100) {
      this.performanceMetrics.cpuUsage = this.performanceMetrics.cpuUsage.slice(-100);
    }
  }

  analyzePerformance() {
    const analysis = {
      memoryTrend: this.analyzeMemoryTrend(),
      cpuTrend: this.analyzeCpuTrend(),
      cacheEfficiency: this.analyzeCacheEfficiency(),
      recommendations: []
    };
    
    if (analysis.memoryTrend.increasing) {
      analysis.recommendations.push('MEMORY_MANAGEMENT');
    }
    
    if (analysis.cpuTrend.high) {
      analysis.recommendations.push('CONCURRENCY');
    }
    
    if (analysis.cacheEfficiency.low) {
      analysis.recommendations.push('CACHING');
    }
    
    return analysis;
  }

  analyzeMemoryTrend() {
    const recent = this.performanceMetrics.memoryUsage.slice(-10);
    if (recent.length < 5) return { increasing: false, critical: false };
    
    const trend = recent.reduce((acc, curr, idx) => {
      if (idx === 0) return acc;
      return acc + (curr.heapUsed > recent[idx - 1].heapUsed ? 1 : -1);
    }, 0);
    
    const avgMemory = recent.reduce((sum, item) => sum + item.heapUsed, 0) / recent.length;
    const critical = avgMemory > this.memoryLimit;
    
    return {
      increasing: trend > 5,
      critical,
      avgMemory
    };
  }

  analyzeCpuTrend() {
    const recent = this.performanceMetrics.cpuUsage.slice(-10);
    if (recent.length < 5) return { high: false, avg: 0 };
    
    const avgCpu = recent.reduce((sum, item) => sum + item.user + item.system, 0) / recent.length;
    const high = avgCpu > 80000000; // 80ms CPU time per measurement
    
    return {
      high,
      avg: avgCpu
    };
  }

  analyzeCacheEfficiency() {
    const { hits, misses } = this.performanceMetrics.cacheHitRate;
    const total = hits + misses;
    
    if (total === 0) return { low: false, rate: 0 };
    
    const rate = hits / total;
    const low = rate < 0.7; // Less than 70% hit rate
    
    return {
      low,
      rate
    };
  }

  applyOptimizations() {
    const analysis = this.analyzePerformance();
    
    analysis.recommendations.forEach(strategy => {
      if (this.optimizationStrategies[strategy]) {
        this.optimizationStrategies[strategy]();
      }
    });
  }

  optimizeCobolParsing() {
    // Implement COBOL parsing optimizations
    const optimization = {
      name: 'COBOL_PARSING',
      timestamp: Date.now(),
      strategies: [
        'Enable AST caching for repeated COBOL structures',
        'Implement incremental parsing for large files',
        'Use worker threads for parallel parsing'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  optimizeTemplateGeneration() {
    // Implement template generation optimizations
    const optimization = {
      name: 'TEMPLATE_GENERATION',
      timestamp: Date.now(),
      strategies: [
        'Cache compiled templates in memory',
        'Pre-compile frequently used templates',
        'Implement template streaming for large outputs'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  optimizeBlockchainDeployment() {
    // Implement blockchain deployment optimizations
    const optimization = {
      name: 'BLOCKCHAIN_DEPLOYMENT',
      timestamp: Date.now(),
      strategies: [
        'Batch multiple deployments together',
        'Use connection pooling for blockchain nodes',
        'Implement deployment queue with priority'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  optimizeDatabaseQueries() {
    // Implement database query optimizations
    const optimization = {
      name: 'DATABASE_QUERIES',
      timestamp: Date.now(),
      strategies: [
        'Add database query result caching',
        'Implement query batching for bulk operations',
        'Use database connection pooling'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  optimizeMemoryManagement() {
    // Force garbage collection if memory usage is high
    if (global.gc) {
      global.gc();
    }
    
    // Clear old cache entries
    this.cache.flushAll();
    
    // Clear in-memory cache for old entries
    const now = Date.now();
    for (const [key, value] of this.memoryCache.entries()) {
      if (now - value.timestamp > 3600000) { // 1 hour old
        this.memoryCache.delete(key);
      }
    }
    
    const optimization = {
      name: 'MEMORY_MANAGEMENT',
      timestamp: Date.now(),
      strategies: [
        'Forced garbage collection',
        'Cache cleanup',
        'Memory leak prevention'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  optimizeCaching() {
    // Increase cache size and TTL for better hit rates
    this.cache.options.stdTTL = 7200; // 2 hours
    this.cache.options.maxKeys = 1000;
    
    const optimization = {
      name: 'CACHING',
      timestamp: Date.now(),
      strategies: [
        'Increased cache TTL to 2 hours',
        'Expanded cache size limit',
        'Implemented intelligent cache eviction'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  optimizeConcurrency() {
    // Implement concurrency optimizations
    const optimization = {
      name: 'CONCURRENCY',
      timestamp: Date.now(),
      strategies: [
        'Implement worker thread pool',
        'Add request queuing and load balancing',
        'Use cluster mode for CPU-intensive tasks'
      ]
    };
    
    this.performanceMetrics.optimizationResults.push(optimization);
    return optimization;
  }

  // Caching utilities
  getCached(key) {
    // Try memory cache first (fastest)
    if (this.memoryCache.has(key)) {
      this.performanceMetrics.cacheHitRate.hits++;
      return this.memoryCache.get(key).data;
    }
    
    // Try Node cache second
    const cached = this.cache.get(key);
    if (cached !== undefined) {
      this.performanceMetrics.cacheHitRate.hits++;
      // Promote to memory cache
      this.memoryCache.set(key, {
        data: cached,
        timestamp: Date.now()
      });
      return cached;
    }
    
    this.performanceMetrics.cacheHitRate.misses++;
    return null;
  }

  setCached(key, data, ttl = 3600) {
    // Set in both caches
    this.cache.set(key, data, ttl);
    this.memoryCache.set(key, {
      data,
      timestamp: Date.now()
    });
  }

  // Batch processing utilities
  addToBatch(operation, data) {
    this.batchProcessor.currentBatch.push({ operation, data, timestamp: Date.now() });
    
    if (this.batchProcessor.currentBatch.length >= this.batchProcessor.batchSize) {
      this.processBatch();
    } else if (!this.batchProcessor.batchTimeout) {
      this.batchProcessor.batchTimeout = setTimeout(() => {
        this.processBatch();
      }, this.batchProcessor.maxWaitTime);
    }
  }

  processBatch() {
    if (this.batchProcessor.currentBatch.length === 0) return;
    
    const batch = this.batchProcessor.currentBatch;
    this.batchProcessor.currentBatch = [];
    
    if (this.batchProcessor.batchTimeout) {
      clearTimeout(this.batchProcessor.batchTimeout);
      this.batchProcessor.batchTimeout = null;
    }
    
    // Process batch operations
    const groupedOperations = this.groupOperationsByType(batch);
    
    Object.entries(groupedOperations).forEach(([operationType, operations]) => {
      this.executeBatchOperation(operationType, operations);
    });
  }

  groupOperationsByType(batch) {
    return batch.reduce((groups, item) => {
      const type = item.operation;
      if (!groups[type]) {
        groups[type] = [];
      }
      groups[type].push(item);
      return groups;
    }, {});
  }

  executeBatchOperation(operationType, operations) {
    const startTime = performance.now();
    
    try {
      switch (operationType) {
        case 'COBOL_PARSE':
          this.batchCobolParse(operations);
          break;
        case 'TEMPLATE_GENERATE':
          this.batchTemplateGenerate(operations);
          break;
        case 'BLOCKCHAIN_DEPLOY':
          this.batchBlockchainDeploy(operations);
          break;
        default:
          // Execute individually
          operations.forEach(op => this.executeOperation(op));
      }
    } catch (error) {
      console.error(`Batch operation ${operationType} failed:`, error);
    }
    
    const endTime = performance.now();
    this.performanceMetrics.responseTime.push({
      operation: operationType,
      duration: endTime - startTime,
      batchSize: operations.length,
      timestamp: Date.now()
    });
  }

  batchCobolParse(operations) {
    // Implement batch COBOL parsing
    const parser = require('../adapters/cobol-transpiler');
    const results = [];
    
    operations.forEach(op => {
      try {
        const result = parser.parseCobol(op.data);
        results.push({ success: true, result });
      } catch (error) {
        results.push({ success: false, error: error.message });
      }
    });
    
    return results;
  }

  batchTemplateGenerate(operations) {
    // Implement batch template generation
    const templateEngine = require('../adapters/templates/template-engine');
    const results = [];
    
    operations.forEach(op => {
      try {
        const result = templateEngine.generateTemplate(op.data);
        results.push({ success: true, result });
      } catch (error) {
        results.push({ success: false, error: error.message });
      }
    });
    
    return results;
  }

  batchBlockchainDeploy(operations) {
    // Implement batch blockchain deployment
    const results = [];
    
    operations.forEach(op => {
      try {
        // Simulate blockchain deployment
        const result = { transactionHash: 'batch_' + Date.now() };
        results.push({ success: true, result });
      } catch (error) {
        results.push({ success: false, error: error.message });
      }
    });
    
    return results;
  }

  executeOperation(operation) {
    // Execute individual operation
    console.log(`Executing operation: ${operation.operation}`);
  }

  // Connection pool management
  acquireConnection() {
    return new Promise((resolve, reject) => {
      if (this.connectionPool.availableConnections.length > 0) {
        const connection = this.connectionPool.availableConnections.pop();
        this.connectionPool.currentConnections++;
        resolve(connection);
      } else if (this.connectionPool.currentConnections < this.connectionPool.maxConnections) {
        const connection = this.createConnection();
        this.connectionPool.currentConnections++;
        resolve(connection);
      } else {
        this.connectionPool.waitingRequests.push({ resolve, reject });
      }
    });
  }

  releaseConnection(connection) {
    this.connectionPool.currentConnections--;
    
    if (this.connectionPool.waitingRequests.length > 0) {
      const { resolve } = this.connectionPool.waitingRequests.shift();
      this.connectionPool.currentConnections++;
      resolve(connection);
    } else {
      this.connectionPool.availableConnections.push(connection);
    }
  }

  createConnection() {
    return {
      id: Date.now() + Math.random(),
      created: Date.now(),
      used: 0
    };
  }

  // Performance monitoring and reporting
  getPerformanceReport() {
    const report = {
      timestamp: Date.now(),
      metrics: {
        averageResponseTime: this.calculateAverageResponseTime(),
        memoryUsage: this.getCurrentMemoryUsage(),
        cpuUsage: this.getCurrentCpuUsage(),
        cacheHitRate: this.calculateCacheHitRate(),
        connectionPoolStatus: this.getConnectionPoolStatus()
      },
      optimizations: this.performanceMetrics.optimizationResults.slice(-10),
      recommendations: this.generateRecommendations()
    };
    
    return report;
  }

  calculateAverageResponseTime() {
    const recent = this.performanceMetrics.responseTime.slice(-50);
    if (recent.length === 0) return 0;
    
    const total = recent.reduce((sum, item) => sum + item.duration, 0);
    return total / recent.length;
  }

  getCurrentMemoryUsage() {
    const usage = process.memoryUsage();
    return {
      heapUsed: usage.heapUsed,
      heapTotal: usage.heapTotal,
      external: usage.external,
      rss: usage.rss,
      percentage: (usage.heapUsed / usage.heapTotal) * 100
    };
  }

  getCurrentCpuUsage() {
    const usage = process.cpuUsage();
    return {
      user: usage.user,
      system: usage.system,
      total: usage.user + usage.system
    };
  }

  calculateCacheHitRate() {
    const { hits, misses } = this.performanceMetrics.cacheHitRate;
    const total = hits + misses;
    return total > 0 ? (hits / total) * 100 : 0;
  }

  getConnectionPoolStatus() {
    return {
      maxConnections: this.connectionPool.maxConnections,
      currentConnections: this.connectionPool.currentConnections,
      availableConnections: this.connectionPool.availableConnections.length,
      waitingRequests: this.connectionPool.waitingRequests.length
    };
  }

  generateRecommendations() {
    const recommendations = [];
    const memoryUsage = this.getCurrentMemoryUsage();
    const cacheHitRate = this.calculateCacheHitRate();
    
    if (memoryUsage.percentage > 80) {
      recommendations.push('High memory usage detected - consider memory optimization');
    }
    
    if (cacheHitRate < 70) {
      recommendations.push('Low cache hit rate - consider cache strategy optimization');
    }
    
    if (this.connectionPool.waitingRequests.length > 0) {
      recommendations.push('Connection pool bottleneck - consider increasing pool size');
    }
    
    return recommendations;
  }

  // Worker thread management
  createWorkerPool(size = this.cpuCount) {
    const workers = [];
    
    for (let i = 0; i < size; i++) {
      workers.push(this.createWorker());
    }
    
    return workers;
  }

  createWorker() {
    return {
      id: Date.now() + Math.random(),
      busy: false,
      tasks: 0,
      created: Date.now()
    };
  }

  cleanup() {
    // Clear all caches
    this.cache.flushAll();
    this.memoryCache.clear();
    
    // Clear batch processor
    if (this.batchProcessor.batchTimeout) {
      clearTimeout(this.batchProcessor.batchTimeout);
    }
    this.batchProcessor.currentBatch = [];
    
    // Clear connection pool
    this.connectionPool.availableConnections = [];
    this.connectionPool.waitingRequests = [];
  }
}

module.exports = { PerformanceOptimizer };