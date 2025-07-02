/**
 * Production Performance Monitoring System
 * Real-time monitoring for Banking Legacy-to-Blockchain B2BaaS Platform
 * 
 * Features:
 * - Real-time metrics collection
 * - Performance alerting
 * - Health monitoring
 * - Custom dashboards
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
  defaultMeta: { service: 'performance-monitor' }
});

/**
 * Metric Types
 */
const METRIC_TYPES = {
  COUNTER: 'counter',
  GAUGE: 'gauge',
  HISTOGRAM: 'histogram',
  TIMER: 'timer'
};

/**
 * Alert Severity Levels
 */
const ALERT_SEVERITY = {
  CRITICAL: 'critical',
  HIGH: 'high',
  MEDIUM: 'medium',
  LOW: 'low',
  INFO: 'info'
};

/**
 * Performance Monitoring Class
 * Comprehensive monitoring solution for production environments
 */
class PerformanceMonitor extends EventEmitter {
  constructor(config = {}) {
    super();

    this.config = {
      // Monitoring configuration
      enableRealTimeMonitoring: config.enableRealTimeMonitoring !== false,
      enableAlerting: config.enableAlerting !== false,
      enableDashboard: config.enableDashboard !== false,
      
      // Collection intervals
      metricsCollectionInterval: config.metricsCollectionInterval || 5000, // 5 seconds
      healthCheckInterval: config.healthCheckInterval || 30000, // 30 seconds
      alertingInterval: config.alertingInterval || 10000, // 10 seconds
      
      // Retention policies
      metricsRetentionDays: config.metricsRetentionDays || 30,
      alertRetentionDays: config.alertRetentionDays || 7,
      
      // Storage configuration
      storageAdapter: config.storageAdapter || 'memory', // memory, redis, mongodb
      exportFormat: config.exportFormat || 'prometheus', // prometheus, json, csv
      
      // Alert configuration
      alertWebhookUrl: config.alertWebhookUrl,
      alertEmailRecipients: config.alertEmailRecipients || [],
      alertSlackWebhook: config.alertSlackWebhook,
      
      ...config
    };

    // Monitoring state
    this.isMonitoring = false;
    this.monitoringStartTime = null;
    this.intervals = new Map();
    
    // Metrics storage
    this.metrics = new Map();
    this.metricHistory = new Map();
    this.activeAlerts = new Map();
    this.alertHistory = [];
    
    // Performance counters
    this.systemMetrics = {
      // Platform metrics
      totalTransactions: 0,
      successfulTransactions: 0,
      failedTransactions: 0,
      averageResponseTime: 0,
      throughputTPS: 0,
      
      // System metrics
      cpuUsage: 0,
      memoryUsage: 0,
      diskUsage: 0,
      networkIO: { in: 0, out: 0 },
      
      // Banking connector metrics
      connectorHealth: new Map(),
      connectorLatency: new Map(),
      connectorErrors: new Map(),
      
      // Blockchain gateway metrics
      blockchainHealth: new Map(),
      blockchainLatency: new Map(),
      blockchainCosts: new Map(),
      
      // Business metrics
      dailyVolume: 0,
      monthlyVolume: 0,
      averageTransactionValue: 0,
      complianceAlerts: 0,
      
      // Error tracking
      errorsByType: new Map(),
      errorsByConnector: new Map(),
      criticalErrors: 0
    };

    // Alert thresholds
    this.alertThresholds = {
      transactionFailureRate: config.alertThresholds?.transactionFailureRate || 0.05, // 5%
      averageResponseTime: config.alertThresholds?.averageResponseTime || 5000, // 5 seconds
      throughputDrop: config.alertThresholds?.throughputDrop || 0.3, // 30% drop
      cpuUsage: config.alertThresholds?.cpuUsage || 80, // 80%
      memoryUsage: config.alertThresholds?.memoryUsage || 85, // 85%
      diskUsage: config.alertThresholds?.diskUsage || 90, // 90%
      connectorFailureRate: config.alertThresholds?.connectorFailureRate || 0.1, // 10%
      blockchainLatency: config.alertThresholds?.blockchainLatency || 30000, // 30 seconds
      complianceFailureRate: config.alertThresholds?.complianceFailureRate || 0.02 // 2%
    };

    logger.info('Performance Monitor initialized', {
      enableRealTimeMonitoring: this.config.enableRealTimeMonitoring,
      enableAlerting: this.config.enableAlerting,
      metricsCollectionInterval: this.config.metricsCollectionInterval
    });
  }

  /**
   * Start monitoring
   */
  async startMonitoring() {
    if (this.isMonitoring) {
      logger.warn('Monitoring already active');
      return;
    }

    try {
      this.isMonitoring = true;
      this.monitoringStartTime = Date.now();

      // Start metrics collection
      if (this.config.enableRealTimeMonitoring) {
        this.startMetricsCollection();
      }

      // Start health monitoring
      this.startHealthMonitoring();

      // Start alerting
      if (this.config.enableAlerting) {
        this.startAlertMonitoring();
      }

      this.emit('monitoring:started', {
        timestamp: new Date().toISOString(),
        config: this.config
      });

      logger.info('Performance monitoring started');

    } catch (error) {
      logger.error('Failed to start monitoring', { error: error.message });
      throw error;
    }
  }

  /**
   * Stop monitoring
   */
  async stopMonitoring() {
    if (!this.isMonitoring) {
      return;
    }

    try {
      this.isMonitoring = false;

      // Clear all intervals
      for (const [name, interval] of this.intervals.entries()) {
        clearInterval(interval);
        this.intervals.delete(name);
      }

      this.emit('monitoring:stopped', {
        timestamp: new Date().toISOString(),
        uptime: Date.now() - this.monitoringStartTime
      });

      logger.info('Performance monitoring stopped');

    } catch (error) {
      logger.error('Failed to stop monitoring', { error: error.message });
    }
  }

  /**
   * Start metrics collection
   */
  startMetricsCollection() {
    const collectMetrics = async () => {
      try {
        await this.collectSystemMetrics();
        await this.collectPlatformMetrics();
        await this.collectBusinessMetrics();
        
        this.emit('metrics:collected', {
          timestamp: new Date().toISOString(),
          metrics: this.getMetricsSummary()
        });

      } catch (error) {
        logger.error('Metrics collection failed', { error: error.message });
      }
    };

    const interval = setInterval(collectMetrics, this.config.metricsCollectionInterval);
    this.intervals.set('metrics', interval);

    logger.info('Metrics collection started', {
      interval: this.config.metricsCollectionInterval
    });
  }

  /**
   * Start health monitoring
   */
  startHealthMonitoring() {
    const checkHealth = async () => {
      try {
        const healthStatus = await this.performHealthChecks();
        
        this.emit('health:checked', {
          timestamp: new Date().toISOString(),
          status: healthStatus
        });

        // Check for health-related alerts
        this.checkHealthAlerts(healthStatus);

      } catch (error) {
        logger.error('Health check failed', { error: error.message });
      }
    };

    const interval = setInterval(checkHealth, this.config.healthCheckInterval);
    this.intervals.set('health', interval);

    logger.info('Health monitoring started', {
      interval: this.config.healthCheckInterval
    });
  }

  /**
   * Start alert monitoring
   */
  startAlertMonitoring() {
    const checkAlerts = async () => {
      try {
        await this.evaluateAlertConditions();
        await this.processActiveAlerts();
        
      } catch (error) {
        logger.error('Alert monitoring failed', { error: error.message });
      }
    };

    const interval = setInterval(checkAlerts, this.config.alertingInterval);
    this.intervals.set('alerts', interval);

    logger.info('Alert monitoring started', {
      interval: this.config.alertingInterval
    });
  }

  /**
   * Collect system metrics (CPU, Memory, Disk, Network)
   */
  async collectSystemMetrics() {
    try {
      // CPU Usage
      const cpuUsage = await this.getCPUUsage();
      this.systemMetrics.cpuUsage = cpuUsage;
      this.recordMetric('system.cpu.usage', cpuUsage, METRIC_TYPES.GAUGE);

      // Memory Usage
      const memoryUsage = await this.getMemoryUsage();
      this.systemMetrics.memoryUsage = memoryUsage;
      this.recordMetric('system.memory.usage', memoryUsage, METRIC_TYPES.GAUGE);

      // Disk Usage
      const diskUsage = await this.getDiskUsage();
      this.systemMetrics.diskUsage = diskUsage;
      this.recordMetric('system.disk.usage', diskUsage, METRIC_TYPES.GAUGE);

      // Network I/O
      const networkIO = await this.getNetworkIO();
      this.systemMetrics.networkIO = networkIO;
      this.recordMetric('system.network.in', networkIO.in, METRIC_TYPES.GAUGE);
      this.recordMetric('system.network.out', networkIO.out, METRIC_TYPES.GAUGE);

    } catch (error) {
      logger.error('System metrics collection failed', { error: error.message });
    }
  }

  /**
   * Collect platform-specific metrics
   */
  async collectPlatformMetrics() {
    try {
      // Transaction metrics
      const transactionMetrics = this.getTransactionMetrics();
      Object.entries(transactionMetrics).forEach(([key, value]) => {
        this.recordMetric(`platform.transactions.${key}`, value, METRIC_TYPES.COUNTER);
      });

      // Connector metrics
      const connectorMetrics = await this.getConnectorMetrics();
      connectorMetrics.forEach((metrics, connectorId) => {
        Object.entries(metrics).forEach(([key, value]) => {
          this.recordMetric(`platform.connector.${connectorId}.${key}`, value, METRIC_TYPES.GAUGE);
        });
      });

      // Blockchain metrics
      const blockchainMetrics = await this.getBlockchainMetrics();
      blockchainMetrics.forEach((metrics, network) => {
        Object.entries(metrics).forEach(([key, value]) => {
          this.recordMetric(`platform.blockchain.${network}.${key}`, value, METRIC_TYPES.GAUGE);
        });
      });

    } catch (error) {
      logger.error('Platform metrics collection failed', { error: error.message });
    }
  }

  /**
   * Collect business metrics
   */
  async collectBusinessMetrics() {
    try {
      // Calculate business KPIs
      const dailyVolume = this.calculateDailyVolume();
      const monthlyVolume = this.calculateMonthlyVolume();
      const averageTransactionValue = this.calculateAverageTransactionValue();

      this.systemMetrics.dailyVolume = dailyVolume;
      this.systemMetrics.monthlyVolume = monthlyVolume;
      this.systemMetrics.averageTransactionValue = averageTransactionValue;

      this.recordMetric('business.daily_volume', dailyVolume, METRIC_TYPES.GAUGE);
      this.recordMetric('business.monthly_volume', monthlyVolume, METRIC_TYPES.GAUGE);
      this.recordMetric('business.average_transaction_value', averageTransactionValue, METRIC_TYPES.GAUGE);

    } catch (error) {
      logger.error('Business metrics collection failed', { error: error.message });
    }
  }

  /**
   * Record a metric
   */
  recordMetric(name, value, type = METRIC_TYPES.GAUGE, labels = {}) {
    const timestamp = Date.now();
    const metric = {
      name,
      value,
      type,
      labels,
      timestamp,
      id: uuidv4()
    };

    // Store current metric
    this.metrics.set(name, metric);

    // Store in history
    if (!this.metricHistory.has(name)) {
      this.metricHistory.set(name, []);
    }
    const history = this.metricHistory.get(name);
    history.push(metric);

    // Limit history size (keep last 1000 entries)
    if (history.length > 1000) {
      history.splice(0, history.length - 1000);
    }

    this.emit('metric:recorded', metric);
  }

  /**
   * Record a transaction event
   */
  recordTransaction(transactionData) {
    const { success, responseTime, amount, currency, connectorId, blockchainNetwork } = transactionData;

    // Update counters
    this.systemMetrics.totalTransactions++;
    if (success) {
      this.systemMetrics.successfulTransactions++;
    } else {
      this.systemMetrics.failedTransactions++;
    }

    // Update response time
    const totalTransactions = this.systemMetrics.totalTransactions;
    this.systemMetrics.averageResponseTime = 
      (this.systemMetrics.averageResponseTime * (totalTransactions - 1) + responseTime) / totalTransactions;

    // Record metrics
    this.recordMetric('transactions.total', this.systemMetrics.totalTransactions, METRIC_TYPES.COUNTER);
    this.recordMetric('transactions.successful', this.systemMetrics.successfulTransactions, METRIC_TYPES.COUNTER);
    this.recordMetric('transactions.failed', this.systemMetrics.failedTransactions, METRIC_TYPES.COUNTER);
    this.recordMetric('transactions.response_time', responseTime, METRIC_TYPES.HISTOGRAM);
    this.recordMetric('transactions.average_response_time', this.systemMetrics.averageResponseTime, METRIC_TYPES.GAUGE);

    // Business metrics
    if (amount && currency) {
      this.recordMetric('business.transaction_amount', amount, METRIC_TYPES.HISTOGRAM, { currency });
    }

    // Connector-specific metrics
    if (connectorId) {
      this.recordMetric(`connectors.${connectorId}.transactions`, 1, METRIC_TYPES.COUNTER);
      if (!success) {
        this.recordMetric(`connectors.${connectorId}.errors`, 1, METRIC_TYPES.COUNTER);
      }
    }

    // Blockchain-specific metrics
    if (blockchainNetwork) {
      this.recordMetric(`blockchain.${blockchainNetwork}.transactions`, 1, METRIC_TYPES.COUNTER);
      this.recordMetric(`blockchain.${blockchainNetwork}.response_time`, responseTime, METRIC_TYPES.HISTOGRAM);
    }
  }

  /**
   * Evaluate alert conditions
   */
  async evaluateAlertConditions() {
    const conditions = [
      this.checkTransactionFailureRate(),
      this.checkResponseTimeThreshold(),
      this.checkSystemResourceUsage(),
      this.checkConnectorHealth(),
      this.checkBlockchainHealth(),
      this.checkComplianceMetrics()
    ];

    for (const condition of conditions) {
      try {
        await condition;
      } catch (error) {
        logger.error('Alert condition evaluation failed', { error: error.message });
      }
    }
  }

  /**
   * Check transaction failure rate
   */
  checkTransactionFailureRate() {
    const totalTransactions = this.systemMetrics.totalTransactions;
    const failedTransactions = this.systemMetrics.failedTransactions;
    
    if (totalTransactions === 0) return;
    
    const failureRate = failedTransactions / totalTransactions;
    
    if (failureRate > this.alertThresholds.transactionFailureRate) {
      this.createAlert({
        id: 'transaction_failure_rate',
        title: 'High Transaction Failure Rate',
        description: `Transaction failure rate (${(failureRate * 100).toFixed(2)}%) exceeds threshold (${(this.alertThresholds.transactionFailureRate * 100).toFixed(2)}%)`,
        severity: ALERT_SEVERITY.HIGH,
        metric: 'transaction_failure_rate',
        currentValue: failureRate,
        threshold: this.alertThresholds.transactionFailureRate,
        category: 'performance'
      });
    }
  }

  /**
   * Check response time threshold
   */
  checkResponseTimeThreshold() {
    const averageResponseTime = this.systemMetrics.averageResponseTime;
    
    if (averageResponseTime > this.alertThresholds.averageResponseTime) {
      this.createAlert({
        id: 'high_response_time',
        title: 'High Average Response Time',
        description: `Average response time (${averageResponseTime}ms) exceeds threshold (${this.alertThresholds.averageResponseTime}ms)`,
        severity: ALERT_SEVERITY.MEDIUM,
        metric: 'average_response_time',
        currentValue: averageResponseTime,
        threshold: this.alertThresholds.averageResponseTime,
        category: 'performance'
      });
    }
  }

  /**
   * Check system resource usage
   */
  checkSystemResourceUsage() {
    // CPU usage
    if (this.systemMetrics.cpuUsage > this.alertThresholds.cpuUsage) {
      this.createAlert({
        id: 'high_cpu_usage',
        title: 'High CPU Usage',
        description: `CPU usage (${this.systemMetrics.cpuUsage}%) exceeds threshold (${this.alertThresholds.cpuUsage}%)`,
        severity: ALERT_SEVERITY.HIGH,
        metric: 'cpu_usage',
        currentValue: this.systemMetrics.cpuUsage,
        threshold: this.alertThresholds.cpuUsage,
        category: 'infrastructure'
      });
    }

    // Memory usage
    if (this.systemMetrics.memoryUsage > this.alertThresholds.memoryUsage) {
      this.createAlert({
        id: 'high_memory_usage',
        title: 'High Memory Usage',
        description: `Memory usage (${this.systemMetrics.memoryUsage}%) exceeds threshold (${this.alertThresholds.memoryUsage}%)`,
        severity: ALERT_SEVERITY.HIGH,
        metric: 'memory_usage',
        currentValue: this.systemMetrics.memoryUsage,
        threshold: this.alertThresholds.memoryUsage,
        category: 'infrastructure'
      });
    }

    // Disk usage
    if (this.systemMetrics.diskUsage > this.alertThresholds.diskUsage) {
      this.createAlert({
        id: 'high_disk_usage',
        title: 'High Disk Usage',
        description: `Disk usage (${this.systemMetrics.diskUsage}%) exceeds threshold (${this.alertThresholds.diskUsage}%)`,
        severity: ALERT_SEVERITY.CRITICAL,
        metric: 'disk_usage',
        currentValue: this.systemMetrics.diskUsage,
        threshold: this.alertThresholds.diskUsage,
        category: 'infrastructure'
      });
    }
  }

  /**
   * Create and manage an alert
   */
  createAlert(alertData) {
    const existingAlert = this.activeAlerts.get(alertData.id);
    
    if (existingAlert) {
      // Update existing alert
      existingAlert.occurrences++;
      existingAlert.lastOccurrence = new Date().toISOString();
      existingAlert.currentValue = alertData.currentValue;
    } else {
      // Create new alert
      const alert = {
        ...alertData,
        alertId: uuidv4(),
        createdAt: new Date().toISOString(),
        lastOccurrence: new Date().toISOString(),
        occurrences: 1,
        status: 'active',
        acknowledged: false
      };

      this.activeAlerts.set(alertData.id, alert);
      this.alertHistory.push({ ...alert, action: 'created' });

      // Emit alert event
      this.emit('alert:created', alert);

      // Send notifications
      this.sendAlertNotifications(alert);

      logger.warn('Alert created', {
        alertId: alert.alertId,
        title: alert.title,
        severity: alert.severity
      });
    }
  }

  /**
   * Send alert notifications
   */
  async sendAlertNotifications(alert) {
    try {
      // Webhook notification
      if (this.config.alertWebhookUrl) {
        await this.sendWebhookNotification(alert);
      }

      // Email notification
      if (this.config.alertEmailRecipients.length > 0) {
        await this.sendEmailNotification(alert);
      }

      // Slack notification
      if (this.config.alertSlackWebhook) {
        await this.sendSlackNotification(alert);
      }

    } catch (error) {
      logger.error('Failed to send alert notifications', { 
        alertId: alert.alertId,
        error: error.message 
      });
    }
  }

  /**
   * Get current monitoring status
   */
  getMonitoringStatus() {
    return {
      isMonitoring: this.isMonitoring,
      uptime: this.monitoringStartTime ? Date.now() - this.monitoringStartTime : 0,
      config: this.config,
      metricsCount: this.metrics.size,
      activeAlertsCount: this.activeAlerts.size,
      systemMetrics: this.systemMetrics,
      lastHealthCheck: this.lastHealthCheck
    };
  }

  /**
   * Get metrics summary
   */
  getMetricsSummary() {
    const summary = {};
    
    for (const [name, metric] of this.metrics.entries()) {
      summary[name] = {
        value: metric.value,
        type: metric.type,
        timestamp: metric.timestamp
      };
    }
    
    return summary;
  }

  /**
   * Export metrics in specified format
   */
  exportMetrics(format = 'json') {
    switch (format) {
      case 'prometheus':
        return this.exportPrometheusFormat();
      case 'json':
        return this.exportJSONFormat();
      case 'csv':
        return this.exportCSVFormat();
      default:
        throw new Error(`Unsupported export format: ${format}`);
    }
  }

  /**
   * Cleanup and shutdown
   */
  async cleanup() {
    await this.stopMonitoring();
    
    this.metrics.clear();
    this.metricHistory.clear();
    this.activeAlerts.clear();
    this.alertHistory = [];
    
    logger.info('Performance monitor cleaned up');
  }

  // ===== UTILITY METHODS =====

  async getCPUUsage() {
    // Mock implementation - replace with actual system monitoring
    return Math.random() * 100;
  }

  async getMemoryUsage() {
    // Mock implementation - replace with actual system monitoring
    const used = process.memoryUsage();
    return (used.heapUsed / used.heapTotal) * 100;
  }

  async getDiskUsage() {
    // Mock implementation - replace with actual disk monitoring
    return Math.random() * 100;
  }

  async getNetworkIO() {
    // Mock implementation - replace with actual network monitoring
    return {
      in: Math.random() * 1000,
      out: Math.random() * 1000
    };
  }

  getTransactionMetrics() {
    return {
      total: this.systemMetrics.totalTransactions,
      successful: this.systemMetrics.successfulTransactions,
      failed: this.systemMetrics.failedTransactions,
      average_response_time: this.systemMetrics.averageResponseTime
    };
  }

  async getConnectorMetrics() {
    // Mock implementation - integrate with actual connectors
    return new Map([
      ['tcs_bancs', { health: 'healthy', latency: 150, errors: 0 }],
      ['swift_gateway', { health: 'healthy', latency: 200, errors: 1 }]
    ]);
  }

  async getBlockchainMetrics() {
    // Mock implementation - integrate with actual blockchain gateways
    return new Map([
      ['xrp', { health: 'healthy', latency: 3000, cost: 0.001 }],
      ['ethereum_l2', { health: 'healthy', latency: 2500, cost: 0.05 }],
      ['corda', { health: 'degraded', latency: 5000, cost: 0.02 }]
    ]);
  }

  calculateDailyVolume() {
    // Mock implementation - calculate from actual transaction data
    return this.systemMetrics.totalTransactions * this.systemMetrics.averageTransactionValue;
  }

  calculateMonthlyVolume() {
    // Mock implementation - calculate from actual transaction data
    return this.calculateDailyVolume() * 30;
  }

  calculateAverageTransactionValue() {
    // Mock implementation - calculate from actual transaction data
    return 1000 + Math.random() * 9000;
  }
}

module.exports = {
  PerformanceMonitor,
  METRIC_TYPES,
  ALERT_SEVERITY
};