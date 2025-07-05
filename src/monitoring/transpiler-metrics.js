const EventEmitter = require('events');
const os = require('os');
const { performance } = require('perf_hooks');

class TranspilerMetrics extends EventEmitter {
  constructor() {
    super();
    this.metrics = {
      transpilations: {
        total: 0,
        successful: 0,
        failed: 0,
        inProgress: 0
      },
      performance: {
        averageProcessingTime: 0,
        p95ProcessingTime: 0,
        p99ProcessingTime: 0,
        responseTimes: []
      },
      system: {
        cpuUsage: 0,
        memoryUsage: 0,
        heapUsage: 0,
        loadAverage: []
      },
      bankingSystems: {
        fis: { usage: 0, errors: 0, avgTime: 0 },
        fiserv: { usage: 0, errors: 0, avgTime: 0 },
        temenos: { usage: 0, errors: 0, avgTime: 0 },
        tcs: { usage: 0, errors: 0, avgTime: 0 }
      },
      blockchains: {
        ethereum: { deployments: 0, failures: 0, avgGasCost: 0 },
        xrp: { deployments: 0, failures: 0, avgCost: 0 },
        algorand: { deployments: 0, failures: 0, avgCost: 0 },
        corda: { deployments: 0, failures: 0, avgCost: 0 }
      },
      errors: {
        parseErrors: 0,
        generationErrors: 0,
        deploymentErrors: 0,
        validationErrors: 0,
        timeoutErrors: 0
      },
      quotas: {
        totalCustomers: 0,
        activeQuotas: 0,
        quotaExceeded: 0,
        overageUsage: 0
      }
    };

    this.startTime = Date.now();
    this.collectors = new Map();
    this.intervals = new Map();
    this.initialized = false;
  }

  async initialize() {
    if (this.initialized) return;

    // Start system metrics collection
    this.startSystemMetricsCollection();
    
    // Start periodic cleanup
    this.startPeriodicCleanup();
    
    this.initialized = true;
    this.emit('initialized');
  }

  async shutdown() {
    // Clear all intervals
    for (const interval of this.intervals.values()) {
      clearInterval(interval);
    }
    this.intervals.clear();
    
    this.initialized = false;
    this.emit('shutdown');
  }

  // Transpilation Metrics
  startTranspilation(workflowId, metadata = {}) {
    const startTime = performance.now();
    
    this.collectors.set(workflowId, {
      startTime,
      metadata,
      status: 'in_progress'
    });

    this.metrics.transpilations.inProgress++;
    this.metrics.transpilations.total++;

    this.emit('transpilation_started', { workflowId, metadata });
    
    return {
      workflowId,
      startTime,
      recordStep: (step, duration) => this.recordTranspilationStep(workflowId, step, duration),
      recordError: (error) => this.recordTranspilationError(workflowId, error),
      complete: (result) => this.completeTranspilation(workflowId, result)
    };
  }

  recordTranspilationStep(workflowId, step, duration) {
    const collector = this.collectors.get(workflowId);
    if (!collector) return;

    if (!collector.steps) collector.steps = [];
    collector.steps.push({ step, duration, timestamp: Date.now() });

    this.emit('transpilation_step', { workflowId, step, duration });
  }

  recordTranspilationError(workflowId, error) {
    const collector = this.collectors.get(workflowId);
    if (!collector) return;

    collector.error = error;
    collector.status = 'failed';

    // Categorize error
    if (error.type === 'PARSE_ERROR') {
      this.metrics.errors.parseErrors++;
    } else if (error.type === 'GENERATION_ERROR') {
      this.metrics.errors.generationErrors++;
    } else if (error.type === 'DEPLOYMENT_ERROR') {
      this.metrics.errors.deploymentErrors++;
    } else if (error.type === 'VALIDATION_ERROR') {
      this.metrics.errors.validationErrors++;
    } else if (error.type === 'TIMEOUT_ERROR') {
      this.metrics.errors.timeoutErrors++;
    }

    this.emit('transpilation_error', { workflowId, error });
  }

  completeTranspilation(workflowId, result = {}) {
    const collector = this.collectors.get(workflowId);
    if (!collector) return;

    const endTime = performance.now();
    const duration = endTime - collector.startTime;

    this.metrics.transpilations.inProgress--;
    
    if (collector.status === 'failed') {
      this.metrics.transpilations.failed++;
    } else {
      this.metrics.transpilations.successful++;
      collector.status = 'completed';
    }

    // Update performance metrics
    this.updatePerformanceMetrics(duration);

    // Update banking system metrics
    if (collector.metadata.bankingSystem) {
      const system = this.metrics.bankingSystems[collector.metadata.bankingSystem];
      if (system) {
        system.usage++;
        if (collector.status === 'failed') {
          system.errors++;
        }
        system.avgTime = (system.avgTime * (system.usage - 1) + duration) / system.usage;
      }
    }

    // Update blockchain metrics
    if (result.contractAddress && collector.metadata.targetBlockchain) {
      const blockchain = this.metrics.blockchains[collector.metadata.targetBlockchain];
      if (blockchain) {
        blockchain.deployments++;
        if (result.gasCost) {
          blockchain.avgGasCost = (blockchain.avgGasCost * (blockchain.deployments - 1) + result.gasCost) / blockchain.deployments;
        }
      }
    }

    this.collectors.delete(workflowId);
    this.emit('transpilation_completed', { workflowId, duration, result });
  }

  updatePerformanceMetrics(duration) {
    // Add to response times array (keep last 1000)
    this.metrics.performance.responseTimes.push(duration);
    if (this.metrics.performance.responseTimes.length > 1000) {
      this.metrics.performance.responseTimes.shift();
    }

    // Update average
    const responseTimes = this.metrics.performance.responseTimes;
    this.metrics.performance.averageProcessingTime = 
      responseTimes.reduce((sum, time) => sum + time, 0) / responseTimes.length;

    // Update percentiles
    const sorted = [...responseTimes].sort((a, b) => a - b);
    this.metrics.performance.p95ProcessingTime = sorted[Math.floor(sorted.length * 0.95)];
    this.metrics.performance.p99ProcessingTime = sorted[Math.floor(sorted.length * 0.99)];
  }

  // System Metrics Collection
  startSystemMetricsCollection() {
    const interval = setInterval(() => {
      this.collectSystemMetrics();
    }, 10000); // Every 10 seconds

    this.intervals.set('system_metrics', interval);
  }

  collectSystemMetrics() {
    // CPU Usage
    const cpus = os.cpus();
    let totalIdle = 0;
    let totalTick = 0;

    cpus.forEach(cpu => {
      for (const type in cpu.times) {
        totalTick += cpu.times[type];
      }
      totalIdle += cpu.times.idle;
    });

    this.metrics.system.cpuUsage = 100 - (totalIdle / totalTick * 100);

    // Memory Usage
    const memoryUsage = process.memoryUsage();
    this.metrics.system.memoryUsage = memoryUsage.rss / 1024 / 1024; // MB
    this.metrics.system.heapUsage = memoryUsage.heapUsed / 1024 / 1024; // MB

    // Load Average
    this.metrics.system.loadAverage = os.loadavg();

    this.emit('system_metrics_updated', this.metrics.system);
  }

  // Quota Metrics
  updateQuotaMetrics(quotaData) {
    this.metrics.quotas.totalCustomers = quotaData.totalCustomers || 0;
    this.metrics.quotas.activeQuotas = quotaData.activeQuotas || 0;
    this.metrics.quotas.quotaExceeded = quotaData.quotaExceeded || 0;
    this.metrics.quotas.overageUsage = quotaData.overageUsage || 0;

    this.emit('quota_metrics_updated', this.metrics.quotas);
  }

  // Health Check Metrics
  getHealthStatus() {
    const uptime = Date.now() - this.startTime;
    const errorRate = (this.metrics.transpilations.failed / this.metrics.transpilations.total) * 100;
    
    let status = 'healthy';
    const issues = [];

    // Check error rate
    if (errorRate > 5) {
      status = 'unhealthy';
      issues.push(`High error rate: ${errorRate.toFixed(2)}%`);
    }

    // Check system resources
    if (this.metrics.system.cpuUsage > 80) {
      status = 'unhealthy';
      issues.push(`High CPU usage: ${this.metrics.system.cpuUsage.toFixed(2)}%`);
    }

    if (this.metrics.system.memoryUsage > 1000) { // > 1GB
      status = 'unhealthy';
      issues.push(`High memory usage: ${this.metrics.system.memoryUsage.toFixed(2)}MB`);
    }

    // Check performance
    if (this.metrics.performance.p95ProcessingTime > 30000) { // > 30 seconds
      status = 'degraded';
      issues.push(`Slow P95 response time: ${this.metrics.performance.p95ProcessingTime.toFixed(0)}ms`);
    }

    return {
      status,
      uptime,
      issues,
      timestamp: new Date().toISOString()
    };
  }

  // Metrics Export
  getMetrics() {
    return {
      ...this.metrics,
      health: this.getHealthStatus(),
      timestamp: new Date().toISOString()
    };
  }

  getPrometheusMetrics() {
    const metrics = [];
    
    // Transpilation metrics
    metrics.push(`# HELP transpiler_total_transpilations Total number of transpilations`);
    metrics.push(`# TYPE transpiler_total_transpilations counter`);
    metrics.push(`transpiler_total_transpilations ${this.metrics.transpilations.total}`);

    metrics.push(`# HELP transpiler_successful_transpilations Number of successful transpilations`);
    metrics.push(`# TYPE transpiler_successful_transpilations counter`);
    metrics.push(`transpiler_successful_transpilations ${this.metrics.transpilations.successful}`);

    metrics.push(`# HELP transpiler_failed_transpilations Number of failed transpilations`);
    metrics.push(`# TYPE transpiler_failed_transpilations counter`);
    metrics.push(`transpiler_failed_transpilations ${this.metrics.transpilations.failed}`);

    metrics.push(`# HELP transpiler_in_progress_transpilations Number of transpilations in progress`);
    metrics.push(`# TYPE transpiler_in_progress_transpilations gauge`);
    metrics.push(`transpiler_in_progress_transpilations ${this.metrics.transpilations.inProgress}`);

    // Performance metrics
    metrics.push(`# HELP transpiler_avg_processing_time_ms Average processing time in milliseconds`);
    metrics.push(`# TYPE transpiler_avg_processing_time_ms gauge`);
    metrics.push(`transpiler_avg_processing_time_ms ${this.metrics.performance.averageProcessingTime}`);

    metrics.push(`# HELP transpiler_p95_processing_time_ms P95 processing time in milliseconds`);
    metrics.push(`# TYPE transpiler_p95_processing_time_ms gauge`);
    metrics.push(`transpiler_p95_processing_time_ms ${this.metrics.performance.p95ProcessingTime}`);

    // System metrics
    metrics.push(`# HELP system_cpu_usage_percent CPU usage percentage`);
    metrics.push(`# TYPE system_cpu_usage_percent gauge`);
    metrics.push(`system_cpu_usage_percent ${this.metrics.system.cpuUsage}`);

    metrics.push(`# HELP system_memory_usage_mb Memory usage in megabytes`);
    metrics.push(`# TYPE system_memory_usage_mb gauge`);
    metrics.push(`system_memory_usage_mb ${this.metrics.system.memoryUsage}`);

    // Banking system metrics
    for (const [system, data] of Object.entries(this.metrics.bankingSystems)) {
      metrics.push(`# HELP banking_system_usage_total Total usage for banking system ${system}`);
      metrics.push(`# TYPE banking_system_usage_total counter`);
      metrics.push(`banking_system_usage_total{system="${system}"} ${data.usage}`);

      metrics.push(`# HELP banking_system_errors_total Total errors for banking system ${system}`);
      metrics.push(`# TYPE banking_system_errors_total counter`);
      metrics.push(`banking_system_errors_total{system="${system}"} ${data.errors}`);
    }

    // Error metrics
    for (const [errorType, count] of Object.entries(this.metrics.errors)) {
      metrics.push(`# HELP transpiler_errors_total Total errors by type`);
      metrics.push(`# TYPE transpiler_errors_total counter`);
      metrics.push(`transpiler_errors_total{type="${errorType}"} ${count}`);
    }

    return metrics.join('\n');
  }

  // Alerting
  checkAlerts() {
    const alerts = [];
    const health = this.getHealthStatus();

    if (health.status === 'unhealthy') {
      alerts.push({
        severity: 'critical',
        message: 'System unhealthy',
        details: health.issues
      });
    }

    const errorRate = (this.metrics.transpilations.failed / this.metrics.transpilations.total) * 100;
    if (errorRate > 10) {
      alerts.push({
        severity: 'warning',
        message: `High error rate: ${errorRate.toFixed(2)}%`,
        details: this.metrics.errors
      });
    }

    if (this.metrics.transpilations.inProgress > 50) {
      alerts.push({
        severity: 'warning',
        message: `High number of in-progress transpilations: ${this.metrics.transpilations.inProgress}`,
        details: { inProgress: this.metrics.transpilations.inProgress }
      });
    }

    return alerts;
  }

  // Cleanup old data
  startPeriodicCleanup() {
    const interval = setInterval(() => {
      this.cleanupOldData();
    }, 300000); // Every 5 minutes

    this.intervals.set('cleanup', interval);
  }

  cleanupOldData() {
    // Clean up old response times (keep last 1000)
    if (this.metrics.performance.responseTimes.length > 1000) {
      this.metrics.performance.responseTimes = this.metrics.performance.responseTimes.slice(-1000);
    }

    // Clean up stale collectors (older than 1 hour)
    const oneHourAgo = performance.now() - (60 * 60 * 1000);
    for (const [workflowId, collector] of this.collectors.entries()) {
      if (collector.startTime < oneHourAgo) {
        this.collectors.delete(workflowId);
        this.metrics.transpilations.inProgress--;
      }
    }

    this.emit('cleanup_completed');
  }

  // Custom metric recording
  recordCustomMetric(name, value, labels = {}) {
    if (!this.metrics.custom) {
      this.metrics.custom = {};
    }

    const key = `${name}_${JSON.stringify(labels)}`;
    this.metrics.custom[key] = {
      name,
      value,
      labels,
      timestamp: Date.now()
    };

    this.emit('custom_metric_recorded', { name, value, labels });
  }
}

// Global instance
const transpilerMetrics = new TranspilerMetrics();

module.exports = {
  TranspilerMetrics,
  transpilerMetrics
};