/**
 * Advanced Transaction Analytics Engine
 * Real-time monitoring and analytics for banking-to-blockchain transactions
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 3 - Advanced Enterprise Features
 */

const EventEmitter = require('events');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'transaction-analytics' }
});

/**
 * Transaction status constants
 */
const ANALYTICS_METRICS = {
  TRANSACTION_COUNT: 'transaction_count',
  PROCESSING_TIME: 'processing_time',
  SUCCESS_RATE: 'success_rate',
  ERROR_RATE: 'error_rate',
  NETWORK_PERFORMANCE: 'network_performance',
  COMPLIANCE_RATE: 'compliance_rate',
  CBDC_VOLUME: 'cbdc_volume',
  CROSS_BORDER_VOLUME: 'cross_border_volume'
};

/**
 * Time window constants for analytics
 */
const TIME_WINDOWS = {
  REAL_TIME: 'real_time',
  LAST_HOUR: 'last_hour',
  LAST_24_HOURS: 'last_24_hours',
  LAST_7_DAYS: 'last_7_days',
  LAST_30_DAYS: 'last_30_days'
};

/**
 * Analytics aggregation types
 */
const AGGREGATION_TYPES = {
  SUM: 'sum',
  AVERAGE: 'average',
  COUNT: 'count',
  MIN: 'min',
  MAX: 'max',
  PERCENTILE: 'percentile'
};

/**
 * Transaction Analytics Engine Class
 * Provides comprehensive analytics for banking transactions
 */
class TransactionAnalytics extends EventEmitter {
  constructor(config = {}) {
    super();
    
    this.config = {
      // Analytics configuration
      retentionPeriod: config.retentionPeriod || 30 * 24 * 60 * 60 * 1000, // 30 days
      aggregationInterval: config.aggregationInterval || 60000, // 1 minute
      enableRealTimeMetrics: config.enableRealTimeMetrics !== false,
      enableComplianceTracking: config.enableComplianceTracking !== false,
      enablePerformanceTracking: config.enablePerformanceTracking !== false,
      
      // Dashboard configuration
      maxDataPoints: config.maxDataPoints || 1000,
      refreshInterval: config.refreshInterval || 30000, // 30 seconds
      alertThresholds: {
        errorRate: config.errorRateThreshold || 0.05, // 5%
        processingTime: config.processingTimeThreshold || 30000, // 30 seconds
        complianceRate: config.complianceRateThreshold || 0.95, // 95%
        ...config.alertThresholds
      },
      
      ...config
    };

    // Analytics data storage
    this.transactionData = new Map();
    this.aggregatedMetrics = new Map();
    this.realTimeMetrics = {
      transactionCount: 0,
      processingTimes: [],
      errorCount: 0,
      complianceChecks: { passed: 0, failed: 0 },
      networkMetrics: new Map(),
      cbdcMetrics: {
        totalMinted: 0,
        totalBurned: 0,
        totalTransferred: 0,
        activeAccounts: new Set()
      }
    };
    
    // Performance tracking
    this.performanceMetrics = {
      swiftParsing: [],
      bancsIntegration: [],
      blockchainSubmission: [],
      endToEndLatency: []
    };
    
    // Compliance tracking
    this.complianceMetrics = {
      amlChecks: { passed: 0, failed: 0, pending: 0 },
      sanctionsChecks: { passed: 0, failed: 0 },
      fatfReporting: { completed: 0, pending: 0 },
      manualReviews: { required: 0, completed: 0 }
    };
    
    // Network-specific metrics
    this.networkMetrics = new Map([
      ['XRP', { transactions: 0, volume: 0, avgLatency: 0, errors: 0 }],
      ['CORDA', { transactions: 0, volume: 0, avgLatency: 0, errors: 0 }],
      ['ETHEREUM_L2', { transactions: 0, volume: 0, avgLatency: 0, errors: 0 }],
      ['ALGORAND', { transactions: 0, volume: 0, avgLatency: 0, errors: 0 }]
    ]);
    
    // Start aggregation timer
    this.aggregationTimer = setInterval(() => {
      this.performAggregation();
    }, this.config.aggregationInterval);
    
    // Start cleanup timer
    this.cleanupTimer = setInterval(() => {
      this.cleanupOldData();
    }, 24 * 60 * 60 * 1000); // Daily cleanup
    
    logger.info('Transaction Analytics Engine initialized', {
      retentionPeriod: this.config.retentionPeriod,
      aggregationInterval: this.config.aggregationInterval,
      enableRealTimeMetrics: this.config.enableRealTimeMetrics
    });
  }

  /**
   * Record a new transaction for analytics
   */
  recordTransaction(transactionData) {
    const timestamp = Date.now();
    const transactionId = transactionData.id || uuidv4();
    
    const analyticsRecord = {
      id: transactionId,
      timestamp,
      type: transactionData.type || 'unknown',
      amount: transactionData.amount || 0,
      currency: transactionData.currency || 'USD',
      sourceNetwork: transactionData.sourceNetwork || 'SWIFT',
      targetNetwork: transactionData.targetNetwork,
      status: transactionData.status || 'initiated',
      processingTime: transactionData.processingTime || 0,
      
      // Banking context
      swiftMessageType: transactionData.swiftMessageType,
      sendingBIC: transactionData.sendingBIC,
      receivingBIC: transactionData.receivingBIC,
      
      // Compliance context
      complianceStatus: transactionData.complianceStatus || 'pending',
      riskScore: transactionData.riskScore || 0,
      amlStatus: transactionData.amlStatus,
      sanctionsCheck: transactionData.sanctionsCheck,
      
      // Performance metrics
      swiftParsingTime: transactionData.swiftParsingTime || 0,
      bancsProcessingTime: transactionData.bancsProcessingTime || 0,
      blockchainSubmissionTime: transactionData.blockchainSubmissionTime || 0,
      
      // Additional metadata
      metadata: transactionData.metadata || {}
    };
    
    // Store transaction data
    this.transactionData.set(transactionId, analyticsRecord);
    
    // Update real-time metrics
    this.updateRealTimeMetrics(analyticsRecord);
    
    // Update network-specific metrics
    this.updateNetworkMetrics(analyticsRecord);
    
    // Update compliance metrics
    this.updateComplianceMetrics(analyticsRecord);
    
    // Update performance metrics
    this.updatePerformanceMetrics(analyticsRecord);
    
    // Emit analytics event
    this.emit('transaction_recorded', {
      transactionId,
      timestamp,
      metrics: this.getRealTimeMetrics()
    });
    
    logger.debug('Transaction recorded for analytics', {
      transactionId,
      type: analyticsRecord.type,
      amount: analyticsRecord.amount,
      targetNetwork: analyticsRecord.targetNetwork
    });
    
    return transactionId;
  }

  /**
   * Update transaction status and metrics
   */
  updateTransactionStatus(transactionId, statusUpdate) {
    const transaction = this.transactionData.get(transactionId);
    if (!transaction) {
      logger.warn('Transaction not found for status update', { transactionId });
      return;
    }
    
    const previousStatus = transaction.status;
    
    // Update transaction record
    Object.assign(transaction, {
      status: statusUpdate.status || transaction.status,
      processingTime: statusUpdate.processingTime || transaction.processingTime,
      complianceStatus: statusUpdate.complianceStatus || transaction.complianceStatus,
      riskScore: statusUpdate.riskScore || transaction.riskScore,
      updatedAt: Date.now(),
      ...statusUpdate
    });
    
    // Update metrics based on status change
    this.handleStatusChange(transaction, previousStatus, statusUpdate.status);
    
    // Emit status update event
    this.emit('transaction_status_updated', {
      transactionId,
      previousStatus,
      newStatus: statusUpdate.status,
      metrics: this.getRealTimeMetrics()
    });
    
    logger.debug('Transaction status updated', {
      transactionId,
      previousStatus,
      newStatus: statusUpdate.status
    });
  }

  /**
   * Get comprehensive analytics dashboard data
   */
  getDashboardData(timeWindow = TIME_WINDOWS.LAST_24_HOURS) {
    const now = Date.now();
    const timeRange = this.getTimeRange(timeWindow, now);
    
    return {
      timestamp: now,
      timeWindow,
      timeRange,
      
      // Transaction overview
      transactionOverview: this.getTransactionOverview(timeRange),
      
      // Performance metrics
      performanceMetrics: this.getPerformanceMetrics(timeRange),
      
      // Network distribution
      networkDistribution: this.getNetworkDistribution(timeRange),
      
      // Compliance metrics
      complianceMetrics: this.getComplianceMetrics(timeRange),
      
      // CBDC metrics (if applicable)
      cbdcMetrics: this.getCBDCMetrics(timeRange),
      
      // Real-time metrics
      realTimeMetrics: this.getRealTimeMetrics(),
      
      // Error analysis
      errorAnalysis: this.getErrorAnalysis(timeRange),
      
      // Volume trends
      volumeTrends: this.getVolumeTrends(timeRange),
      
      // Geographic distribution
      geographicDistribution: this.getGeographicDistribution(timeRange)
    };
  }

  /**
   * Get transaction overview metrics
   */
  getTransactionOverview(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    
    const totalTransactions = transactions.length;
    const successfulTransactions = transactions.filter(t => t.status === 'confirmed').length;
    const failedTransactions = transactions.filter(t => t.status === 'failed').length;
    const pendingTransactions = transactions.filter(t => t.status === 'pending' || t.status === 'processing').length;
    
    const totalVolume = transactions.reduce((sum, t) => sum + (t.amount || 0), 0);
    const avgProcessingTime = transactions.length > 0 
      ? transactions.reduce((sum, t) => sum + (t.processingTime || 0), 0) / transactions.length 
      : 0;
    
    return {
      totalTransactions,
      successfulTransactions,
      failedTransactions,
      pendingTransactions,
      successRate: totalTransactions > 0 ? successfulTransactions / totalTransactions : 0,
      errorRate: totalTransactions > 0 ? failedTransactions / totalTransactions : 0,
      totalVolume,
      avgProcessingTime,
      transactionsPerHour: this.calculateTransactionsPerHour(transactions, timeRange)
    };
  }

  /**
   * Get performance metrics
   */
  getPerformanceMetrics(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    
    return {
      swiftParsing: this.calculatePerformanceStats(transactions, 'swiftParsingTime'),
      bancsIntegration: this.calculatePerformanceStats(transactions, 'bancsProcessingTime'),
      blockchainSubmission: this.calculatePerformanceStats(transactions, 'blockchainSubmissionTime'),
      endToEndLatency: this.calculatePerformanceStats(transactions, 'processingTime'),
      
      // Latency percentiles
      latencyPercentiles: this.calculateLatencyPercentiles(transactions),
      
      // Throughput metrics
      throughputMetrics: this.calculateThroughputMetrics(transactions, timeRange)
    };
  }

  /**
   * Get network distribution metrics
   */
  getNetworkDistribution(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    const networkStats = new Map();
    
    transactions.forEach(transaction => {
      const network = transaction.targetNetwork || 'Unknown';
      if (!networkStats.has(network)) {
        networkStats.set(network, {
          count: 0,
          volume: 0,
          successCount: 0,
          errorCount: 0,
          avgLatency: 0,
          latencies: []
        });
      }
      
      const stats = networkStats.get(network);
      stats.count++;
      stats.volume += transaction.amount || 0;
      stats.latencies.push(transaction.processingTime || 0);
      
      if (transaction.status === 'confirmed') stats.successCount++;
      if (transaction.status === 'failed') stats.errorCount++;
    });
    
    // Calculate averages
    networkStats.forEach((stats, network) => {
      stats.avgLatency = stats.latencies.length > 0 
        ? stats.latencies.reduce((a, b) => a + b, 0) / stats.latencies.length 
        : 0;
      stats.successRate = stats.count > 0 ? stats.successCount / stats.count : 0;
      stats.errorRate = stats.count > 0 ? stats.errorCount / stats.count : 0;
    });
    
    return Object.fromEntries(networkStats);
  }

  /**
   * Get compliance metrics
   */
  getComplianceMetrics(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    
    const amlChecks = { passed: 0, failed: 0, pending: 0 };
    const sanctionsChecks = { passed: 0, failed: 0 };
    const riskScoreDistribution = { low: 0, medium: 0, high: 0 };
    const manualReviews = { required: 0, completed: 0 };
    
    transactions.forEach(transaction => {
      // AML status
      if (transaction.amlStatus === 'passed') amlChecks.passed++;
      else if (transaction.amlStatus === 'failed') amlChecks.failed++;
      else amlChecks.pending++;
      
      // Sanctions check
      if (transaction.sanctionsCheck === 'passed') sanctionsChecks.passed++;
      else if (transaction.sanctionsCheck === 'failed') sanctionsChecks.failed++;
      
      // Risk score distribution
      const riskScore = transaction.riskScore || 0;
      if (riskScore < 30) riskScoreDistribution.low++;
      else if (riskScore < 70) riskScoreDistribution.medium++;
      else riskScoreDistribution.high++;
      
      // Manual reviews
      if (transaction.metadata?.manualReviewRequired) {
        manualReviews.required++;
        if (transaction.metadata?.manualReviewCompleted) {
          manualReviews.completed++;
        }
      }
    });
    
    const totalChecks = amlChecks.passed + amlChecks.failed + amlChecks.pending;
    
    return {
      amlChecks,
      sanctionsChecks,
      riskScoreDistribution,
      manualReviews,
      complianceRate: totalChecks > 0 ? (amlChecks.passed + sanctionsChecks.passed) / (totalChecks * 2) : 0,
      avgRiskScore: transactions.length > 0 
        ? transactions.reduce((sum, t) => sum + (t.riskScore || 0), 0) / transactions.length 
        : 0
    };
  }

  /**
   * Get CBDC-specific metrics
   */
  getCBDCMetrics(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    const cbdcTransactions = transactions.filter(t => 
      t.targetNetwork === 'ALGORAND' || t.type?.includes('cbdc')
    );
    
    const metrics = {
      totalTransactions: cbdcTransactions.length,
      mintOperations: 0,
      burnOperations: 0,
      transferOperations: 0,
      freezeOperations: 0,
      totalVolume: 0,
      activeAccounts: new Set(),
      avgTransactionSize: 0
    };
    
    cbdcTransactions.forEach(transaction => {
      metrics.totalVolume += transaction.amount || 0;
      
      if (transaction.metadata?.cbdcType) {
        switch (transaction.metadata.cbdcType) {
          case 'mint': metrics.mintOperations++; break;
          case 'burn': metrics.burnOperations++; break;
          case 'transfer': metrics.transferOperations++; break;
          case 'compliance_freeze': metrics.freezeOperations++; break;
        }
      }
      
      if (transaction.metadata?.senderAddress) {
        metrics.activeAccounts.add(transaction.metadata.senderAddress);
      }
      if (transaction.metadata?.receiverAddress) {
        metrics.activeAccounts.add(transaction.metadata.receiverAddress);
      }
    });
    
    metrics.avgTransactionSize = metrics.totalTransactions > 0 
      ? metrics.totalVolume / metrics.totalTransactions 
      : 0;
    metrics.activeAccountsCount = metrics.activeAccounts.size;
    
    return metrics;
  }

  /**
   * Helper method to update real-time metrics
   */
  updateRealTimeMetrics(transaction) {
    this.realTimeMetrics.transactionCount++;
    
    if (transaction.processingTime > 0) {
      this.realTimeMetrics.processingTimes.push(transaction.processingTime);
      
      // Keep only last 1000 processing times for memory efficiency
      if (this.realTimeMetrics.processingTimes.length > 1000) {
        this.realTimeMetrics.processingTimes = this.realTimeMetrics.processingTimes.slice(-1000);
      }
    }
    
    if (transaction.status === 'failed') {
      this.realTimeMetrics.errorCount++;
    }
    
    // Update compliance metrics
    if (transaction.complianceStatus === 'passed') {
      this.realTimeMetrics.complianceChecks.passed++;
    } else if (transaction.complianceStatus === 'failed') {
      this.realTimeMetrics.complianceChecks.failed++;
    }
  }

  /**
   * Helper method to update network metrics
   */
  updateNetworkMetrics(transaction) {
    const network = transaction.targetNetwork;
    if (!network || !this.networkMetrics.has(network)) return;
    
    const metrics = this.networkMetrics.get(network);
    metrics.transactions++;
    metrics.volume += transaction.amount || 0;
    
    if (transaction.processingTime > 0) {
      metrics.avgLatency = (metrics.avgLatency * (metrics.transactions - 1) + transaction.processingTime) / metrics.transactions;
    }
    
    if (transaction.status === 'failed') {
      metrics.errors++;
    }
  }

  /**
   * Helper method to update compliance metrics
   */
  updateComplianceMetrics(transaction) {
    if (transaction.amlStatus === 'passed') this.complianceMetrics.amlChecks.passed++;
    else if (transaction.amlStatus === 'failed') this.complianceMetrics.amlChecks.failed++;
    else this.complianceMetrics.amlChecks.pending++;
    
    if (transaction.sanctionsCheck === 'passed') this.complianceMetrics.sanctionsChecks.passed++;
    else if (transaction.sanctionsCheck === 'failed') this.complianceMetrics.sanctionsChecks.failed++;
    
    if (transaction.metadata?.manualReviewRequired) {
      this.complianceMetrics.manualReviews.required++;
    }
  }

  /**
   * Helper method to update performance metrics
   */
  updatePerformanceMetrics(transaction) {
    if (transaction.swiftParsingTime > 0) {
      this.performanceMetrics.swiftParsing.push(transaction.swiftParsingTime);
    }
    if (transaction.bancsProcessingTime > 0) {
      this.performanceMetrics.bancsIntegration.push(transaction.bancsProcessingTime);
    }
    if (transaction.blockchainSubmissionTime > 0) {
      this.performanceMetrics.blockchainSubmission.push(transaction.blockchainSubmissionTime);
    }
    if (transaction.processingTime > 0) {
      this.performanceMetrics.endToEndLatency.push(transaction.processingTime);
    }
    
    // Keep only last 1000 entries for each metric
    Object.keys(this.performanceMetrics).forEach(key => {
      if (this.performanceMetrics[key].length > 1000) {
        this.performanceMetrics[key] = this.performanceMetrics[key].slice(-1000);
      }
    });
  }

  /**
   * Get transactions within time range
   */
  getTransactionsInRange(timeRange) {
    return Array.from(this.transactionData.values()).filter(
      transaction => transaction.timestamp >= timeRange.start && transaction.timestamp <= timeRange.end
    );
  }

  /**
   * Get time range for analysis
   */
  getTimeRange(timeWindow, now) {
    const ranges = {
      [TIME_WINDOWS.REAL_TIME]: { start: now - 5 * 60 * 1000, end: now }, // Last 5 minutes
      [TIME_WINDOWS.LAST_HOUR]: { start: now - 60 * 60 * 1000, end: now },
      [TIME_WINDOWS.LAST_24_HOURS]: { start: now - 24 * 60 * 60 * 1000, end: now },
      [TIME_WINDOWS.LAST_7_DAYS]: { start: now - 7 * 24 * 60 * 60 * 1000, end: now },
      [TIME_WINDOWS.LAST_30_DAYS]: { start: now - 30 * 24 * 60 * 60 * 1000, end: now }
    };
    
    return ranges[timeWindow] || ranges[TIME_WINDOWS.LAST_24_HOURS];
  }

  /**
   * Calculate performance statistics
   */
  calculatePerformanceStats(transactions, field) {
    const values = transactions
      .map(t => t[field])
      .filter(v => v && v > 0)
      .sort((a, b) => a - b);
    
    if (values.length === 0) {
      return { count: 0, min: 0, max: 0, avg: 0, median: 0, p95: 0, p99: 0 };
    }
    
    const sum = values.reduce((a, b) => a + b, 0);
    const count = values.length;
    
    return {
      count,
      min: values[0],
      max: values[values.length - 1],
      avg: sum / count,
      median: values[Math.floor(count / 2)],
      p95: values[Math.floor(count * 0.95)],
      p99: values[Math.floor(count * 0.99)]
    };
  }

  /**
   * Calculate latency percentiles
   */
  calculateLatencyPercentiles(transactions) {
    const latencies = transactions
      .map(t => t.processingTime)
      .filter(l => l && l > 0)
      .sort((a, b) => a - b);
    
    if (latencies.length === 0) return {};
    
    return {
      p50: latencies[Math.floor(latencies.length * 0.5)],
      p75: latencies[Math.floor(latencies.length * 0.75)],
      p90: latencies[Math.floor(latencies.length * 0.9)],
      p95: latencies[Math.floor(latencies.length * 0.95)],
      p99: latencies[Math.floor(latencies.length * 0.99)]
    };
  }

  /**
   * Calculate throughput metrics
   */
  calculateThroughputMetrics(transactions, timeRange) {
    const durationHours = (timeRange.end - timeRange.start) / (60 * 60 * 1000);
    
    return {
      transactionsPerHour: durationHours > 0 ? transactions.length / durationHours : 0,
      volumePerHour: durationHours > 0 
        ? transactions.reduce((sum, t) => sum + (t.amount || 0), 0) / durationHours 
        : 0
    };
  }

  /**
   * Calculate transactions per hour
   */
  calculateTransactionsPerHour(transactions, timeRange) {
    const hours = (timeRange.end - timeRange.start) / (60 * 60 * 1000);
    return hours > 0 ? transactions.length / hours : 0;
  }

  /**
   * Get error analysis
   */
  getErrorAnalysis(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    const errors = transactions.filter(t => t.status === 'failed');
    
    const errorsByNetwork = {};
    const errorsByType = {};
    const errorsByReason = {};
    
    errors.forEach(error => {
      const network = error.targetNetwork || 'Unknown';
      const type = error.type || 'Unknown';
      const reason = error.metadata?.errorReason || 'Unknown';
      
      errorsByNetwork[network] = (errorsByNetwork[network] || 0) + 1;
      errorsByType[type] = (errorsByType[type] || 0) + 1;
      errorsByReason[reason] = (errorsByReason[reason] || 0) + 1;
    });
    
    return {
      totalErrors: errors.length,
      errorRate: transactions.length > 0 ? errors.length / transactions.length : 0,
      errorsByNetwork,
      errorsByType,
      errorsByReason,
      recentErrors: errors.slice(-10).map(e => ({
        id: e.id,
        timestamp: e.timestamp,
        network: e.targetNetwork,
        type: e.type,
        reason: e.metadata?.errorReason
      }))
    };
  }

  /**
   * Get volume trends
   */
  getVolumeTrends(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    const hourlyData = new Map();
    
    transactions.forEach(transaction => {
      const hour = Math.floor(transaction.timestamp / (60 * 60 * 1000)) * (60 * 60 * 1000);
      
      if (!hourlyData.has(hour)) {
        hourlyData.set(hour, { count: 0, volume: 0 });
      }
      
      const data = hourlyData.get(hour);
      data.count++;
      data.volume += transaction.amount || 0;
    });
    
    return Array.from(hourlyData.entries())
      .map(([timestamp, data]) => ({
        timestamp,
        count: data.count,
        volume: data.volume
      }))
      .sort((a, b) => a.timestamp - b.timestamp);
  }

  /**
   * Get geographic distribution
   */
  getGeographicDistribution(timeRange) {
    const transactions = this.getTransactionsInRange(timeRange);
    const countries = new Map();
    
    transactions.forEach(transaction => {
      const sendingCountry = this.getBICCountry(transaction.sendingBIC);
      const receivingCountry = this.getBICCountry(transaction.receivingBIC);
      
      if (sendingCountry) {
        const data = countries.get(sendingCountry) || { outgoing: 0, incoming: 0, volume: 0 };
        data.outgoing++;
        data.volume += transaction.amount || 0;
        countries.set(sendingCountry, data);
      }
      
      if (receivingCountry && receivingCountry !== sendingCountry) {
        const data = countries.get(receivingCountry) || { outgoing: 0, incoming: 0, volume: 0 };
        data.incoming++;
        countries.set(receivingCountry, data);
      }
    });
    
    return Object.fromEntries(countries);
  }

  /**
   * Extract country from BIC code
   */
  getBICCountry(bic) {
    if (!bic || bic.length < 6) return null;
    return bic.substring(4, 6);
  }

  /**
   * Get real-time metrics
   */
  getRealTimeMetrics() {
    const now = Date.now();
    const recentTransactions = Array.from(this.transactionData.values())
      .filter(t => now - t.timestamp < 5 * 60 * 1000); // Last 5 minutes
    
    const avgProcessingTime = this.realTimeMetrics.processingTimes.length > 0
      ? this.realTimeMetrics.processingTimes.reduce((a, b) => a + b, 0) / this.realTimeMetrics.processingTimes.length
      : 0;
    
    return {
      timestamp: now,
      totalTransactions: this.realTimeMetrics.transactionCount,
      recentTransactions: recentTransactions.length,
      errorCount: this.realTimeMetrics.errorCount,
      avgProcessingTime,
      errorRate: this.realTimeMetrics.transactionCount > 0 
        ? this.realTimeMetrics.errorCount / this.realTimeMetrics.transactionCount 
        : 0,
      complianceRate: this.realTimeMetrics.complianceChecks.passed + this.realTimeMetrics.complianceChecks.failed > 0
        ? this.realTimeMetrics.complianceChecks.passed / (this.realTimeMetrics.complianceChecks.passed + this.realTimeMetrics.complianceChecks.failed)
        : 0,
      networkMetrics: Object.fromEntries(this.networkMetrics)
    };
  }

  /**
   * Handle status changes for analytics
   */
  handleStatusChange(transaction, previousStatus, newStatus) {
    // Update success/error counts based on status change
    if (newStatus === 'failed' && previousStatus !== 'failed') {
      this.realTimeMetrics.errorCount++;
    }
    
    // Update network metrics
    if (newStatus === 'failed' && transaction.targetNetwork) {
      const networkMetric = this.networkMetrics.get(transaction.targetNetwork);
      if (networkMetric) {
        networkMetric.errors++;
      }
    }
  }

  /**
   * Perform periodic aggregation of metrics
   */
  performAggregation() {
    const now = Date.now();
    const aggregationKey = Math.floor(now / this.config.aggregationInterval);
    
    // Get transactions in the current aggregation window
    const windowStart = aggregationKey * this.config.aggregationInterval;
    const windowEnd = windowStart + this.config.aggregationInterval;
    
    const windowTransactions = Array.from(this.transactionData.values())
      .filter(t => t.timestamp >= windowStart && t.timestamp < windowEnd);
    
    // Create aggregated metrics
    const aggregatedData = {
      timestamp: windowStart,
      transactionCount: windowTransactions.length,
      totalVolume: windowTransactions.reduce((sum, t) => sum + (t.amount || 0), 0),
      successCount: windowTransactions.filter(t => t.status === 'confirmed').length,
      errorCount: windowTransactions.filter(t => t.status === 'failed').length,
      avgProcessingTime: windowTransactions.length > 0
        ? windowTransactions.reduce((sum, t) => sum + (t.processingTime || 0), 0) / windowTransactions.length
        : 0,
      networkDistribution: this.aggregateNetworkDistribution(windowTransactions),
      complianceMetrics: this.aggregateComplianceMetrics(windowTransactions)
    };
    
    this.aggregatedMetrics.set(aggregationKey, aggregatedData);
    
    // Emit aggregation event
    this.emit('metrics_aggregated', {
      timestamp: now,
      aggregationWindow: aggregationKey,
      metrics: aggregatedData
    });
    
    logger.debug('Metrics aggregated', {
      window: aggregationKey,
      transactionCount: aggregatedData.transactionCount,
      totalVolume: aggregatedData.totalVolume
    });
  }

  /**
   * Aggregate network distribution for time window
   */
  aggregateNetworkDistribution(transactions) {
    const distribution = {};
    
    transactions.forEach(transaction => {
      const network = transaction.targetNetwork || 'Unknown';
      if (!distribution[network]) {
        distribution[network] = { count: 0, volume: 0, errors: 0 };
      }
      
      distribution[network].count++;
      distribution[network].volume += transaction.amount || 0;
      if (transaction.status === 'failed') {
        distribution[network].errors++;
      }
    });
    
    return distribution;
  }

  /**
   * Aggregate compliance metrics for time window
   */
  aggregateComplianceMetrics(transactions) {
    return {
      amlPassed: transactions.filter(t => t.amlStatus === 'passed').length,
      amlFailed: transactions.filter(t => t.amlStatus === 'failed').length,
      sanctionsPassed: transactions.filter(t => t.sanctionsCheck === 'passed').length,
      sanctionsFailed: transactions.filter(t => t.sanctionsCheck === 'failed').length,
      avgRiskScore: transactions.length > 0
        ? transactions.reduce((sum, t) => sum + (t.riskScore || 0), 0) / transactions.length
        : 0
    };
  }

  /**
   * Clean up old data based on retention period
   */
  cleanupOldData() {
    const cutoffTime = Date.now() - this.config.retentionPeriod;
    let removedCount = 0;
    
    // Clean up transaction data
    for (const [id, transaction] of this.transactionData.entries()) {
      if (transaction.timestamp < cutoffTime) {
        this.transactionData.delete(id);
        removedCount++;
      }
    }
    
    // Clean up aggregated metrics
    for (const [key, data] of this.aggregatedMetrics.entries()) {
      if (data.timestamp < cutoffTime) {
        this.aggregatedMetrics.delete(key);
      }
    }
    
    logger.info('Analytics data cleanup completed', {
      removedTransactions: removedCount,
      remainingTransactions: this.transactionData.size,
      retentionPeriod: this.config.retentionPeriod
    });
  }

  /**
   * Get system health metrics
   */
  getSystemHealth() {
    return {
      timestamp: Date.now(),
      dataPoints: this.transactionData.size,
      aggregatedMetrics: this.aggregatedMetrics.size,
      memoryUsage: process.memoryUsage(),
      uptime: process.uptime(),
      isHealthy: this.transactionData.size < this.config.maxDataPoints * 2, // Health check
      alertStatus: this.checkAlertThresholds()
    };
  }

  /**
   * Check alert thresholds
   */
  checkAlertThresholds() {
    const realTimeMetrics = this.getRealTimeMetrics();
    const alerts = [];
    
    if (realTimeMetrics.errorRate > this.config.alertThresholds.errorRate) {
      alerts.push({
        type: 'error_rate',
        threshold: this.config.alertThresholds.errorRate,
        current: realTimeMetrics.errorRate,
        severity: 'high'
      });
    }
    
    if (realTimeMetrics.avgProcessingTime > this.config.alertThresholds.processingTime) {
      alerts.push({
        type: 'processing_time',
        threshold: this.config.alertThresholds.processingTime,
        current: realTimeMetrics.avgProcessingTime,
        severity: 'medium'
      });
    }
    
    if (realTimeMetrics.complianceRate < this.config.alertThresholds.complianceRate) {
      alerts.push({
        type: 'compliance_rate',
        threshold: this.config.alertThresholds.complianceRate,
        current: realTimeMetrics.complianceRate,
        severity: 'high'
      });
    }
    
    return alerts;
  }

  /**
   * Shutdown analytics engine
   */
  shutdown() {
    if (this.aggregationTimer) {
      clearInterval(this.aggregationTimer);
    }
    
    if (this.cleanupTimer) {
      clearInterval(this.cleanupTimer);
    }
    
    logger.info('Transaction Analytics Engine shut down', {
      totalTransactions: this.transactionData.size,
      aggregatedMetrics: this.aggregatedMetrics.size
    });
  }
}

module.exports = {
  TransactionAnalytics,
  ANALYTICS_METRICS,
  TIME_WINDOWS,
  AGGREGATION_TYPES
};