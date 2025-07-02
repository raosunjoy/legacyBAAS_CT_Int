/**
 * Real-time Monitoring Dashboard
 * Web-based dashboard for transaction analytics and system monitoring
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 3 - Advanced Enterprise Features
 */

const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const path = require('path');
const winston = require('winston');
const { TransactionAnalytics, TIME_WINDOWS } = require('./transaction-analytics');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'monitoring-dashboard' }
});

/**
 * Dashboard themes and layouts
 */
const DASHBOARD_THEMES = {
  DARK: 'dark',
  LIGHT: 'light',
  CORPORATE: 'corporate'
};

const DASHBOARD_LAYOUTS = {
  EXECUTIVE: 'executive',     // High-level overview
  OPERATIONS: 'operations',   // Detailed operational metrics
  COMPLIANCE: 'compliance',   // Compliance and regulatory focus
  TECHNICAL: 'technical',     // Technical performance metrics
  CBDC: 'cbdc'               // CBDC-specific dashboard
};

/**
 * Real-time Monitoring Dashboard Class
 * Provides web-based analytics dashboard with real-time updates
 */
class MonitoringDashboard {
  constructor(config = {}) {
    this.config = {
      // Server configuration
      port: config.port || process.env.DASHBOARD_PORT || 3002,
      host: config.host || 'localhost',
      
      // Dashboard configuration
      title: config.title || 'LegacyBaaS Banking Platform - Analytics Dashboard',
      theme: config.theme || DASHBOARD_THEMES.CORPORATE,
      defaultLayout: config.defaultLayout || DASHBOARD_LAYOUTS.EXECUTIVE,
      
      // Update intervals
      realTimeUpdateInterval: config.realTimeUpdateInterval || 5000,  // 5 seconds
      dashboardRefreshInterval: config.dashboardRefreshInterval || 30000, // 30 seconds
      
      // Security configuration
      enableAuth: config.enableAuth !== false,
      apiKey: config.apiKey || process.env.DASHBOARD_API_KEY,
      corsOrigins: config.corsOrigins || ['http://localhost:3000'],
      
      // Analytics configuration
      retentionPeriod: config.retentionPeriod || 7 * 24 * 60 * 60 * 1000, // 7 days
      maxConcurrentUsers: config.maxConcurrentUsers || 50,
      
      ...config
    };

    // Initialize Express app
    this.app = express();
    this.server = http.createServer(this.app);
    this.io = socketIo(this.server, {
      cors: {
        origin: this.config.corsOrigins,
        methods: ['GET', 'POST']
      }
    });

    // Initialize analytics engine
    this.analytics = new TransactionAnalytics({
      retentionPeriod: this.config.retentionPeriod,
      enableRealTimeMetrics: true,
      enableComplianceTracking: true,
      enablePerformanceTracking: true
    });

    // Dashboard state
    this.connectedUsers = new Map();
    this.activeLayouts = new Map();
    this.alertSubscriptions = new Set();
    
    // Real-time data cache
    this.dashboardCache = new Map();
    this.lastUpdateTime = Date.now();
    
    // Setup Express middleware and routes
    this.setupMiddleware();
    this.setupRoutes();
    this.setupSocketHandlers();
    this.setupAnalyticsIntegration();
    
    // Start real-time updates
    this.startRealTimeUpdates();
    
    logger.info('Monitoring Dashboard initialized', {
      port: this.config.port,
      theme: this.config.theme,
      defaultLayout: this.config.defaultLayout
    });
  }

  /**
   * Setup Express middleware
   */
  setupMiddleware() {
    // Basic middleware
    this.app.use(express.json({ limit: '10mb' }));
    this.app.use(express.urlencoded({ extended: true }));
    
    // Static files for dashboard assets
    this.app.use('/assets', express.static(path.join(__dirname, 'dashboard-assets')));
    
    // CORS middleware
    this.app.use((req, res, next) => {
      res.header('Access-Control-Allow-Origin', this.config.corsOrigins.join(','));
      res.header('Access-Control-Allow-Headers', 'Origin, X-Requested-With, Content-Type, Accept, Authorization');
      res.header('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
      next();
    });
    
    // Authentication middleware
    if (this.config.enableAuth) {
      this.app.use('/api', this.authMiddleware.bind(this));
    }
    
    // Request logging
    this.app.use((req, res, next) => {
      logger.debug('Dashboard request', {
        method: req.method,
        url: req.url,
        userAgent: req.get('User-Agent'),
        ip: req.ip
      });
      next();
    });
  }

  /**
   * Authentication middleware
   */
  authMiddleware(req, res, next) {
    const apiKey = req.header('X-API-Key') || req.query.apiKey;
    
    if (!apiKey || apiKey !== this.config.apiKey) {
      return res.status(401).json({
        error: 'Unauthorized',
        message: 'Valid API key required'
      });
    }
    
    next();
  }

  /**
   * Setup Express routes
   */
  setupRoutes() {
    // Main dashboard page
    this.app.get('/', (req, res) => {
      res.send(this.generateDashboardHTML());
    });
    
    // Dashboard with specific layout
    this.app.get('/dashboard/:layout', (req, res) => {
      const layout = req.params.layout;
      if (!Object.values(DASHBOARD_LAYOUTS).includes(layout)) {
        return res.status(400).json({ error: 'Invalid dashboard layout' });
      }
      res.send(this.generateDashboardHTML(layout));
    });
    
    // API Routes
    
    // Get current dashboard data
    this.app.get('/api/dashboard/:timeWindow?', async (req, res) => {
      try {
        const timeWindow = req.params.timeWindow || TIME_WINDOWS.LAST_24_HOURS;
        const layout = req.query.layout || this.config.defaultLayout;
        
        const dashboardData = this.analytics.getDashboardData(timeWindow);
        const layoutData = this.formatDataForLayout(dashboardData, layout);
        
        res.json({
          success: true,
          data: layoutData,
          timestamp: Date.now(),
          timeWindow,
          layout
        });
      } catch (error) {
        logger.error('Dashboard data request failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to retrieve dashboard data'
        });
      }
    });
    
    // Get real-time metrics
    this.app.get('/api/realtime', (req, res) => {
      try {
        const metrics = this.analytics.getRealTimeMetrics();
        res.json({
          success: true,
          data: metrics,
          timestamp: Date.now()
        });
      } catch (error) {
        logger.error('Real-time metrics request failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to retrieve real-time metrics'
        });
      }
    });
    
    // Get system health
    this.app.get('/api/health', (req, res) => {
      try {
        const health = this.analytics.getSystemHealth();
        res.json({
          success: true,
          data: health,
          timestamp: Date.now()
        });
      } catch (error) {
        logger.error('System health check failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to retrieve system health'
        });
      }
    });
    
    // Submit transaction data
    this.app.post('/api/transactions', (req, res) => {
      try {
        const transactionData = req.body;
        const transactionId = this.analytics.recordTransaction(transactionData);
        
        res.json({
          success: true,
          transactionId,
          timestamp: Date.now()
        });
      } catch (error) {
        logger.error('Transaction recording failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to record transaction'
        });
      }
    });
    
    // Update transaction status
    this.app.put('/api/transactions/:id', (req, res) => {
      try {
        const transactionId = req.params.id;
        const statusUpdate = req.body;
        
        this.analytics.updateTransactionStatus(transactionId, statusUpdate);
        
        res.json({
          success: true,
          transactionId,
          timestamp: Date.now()
        });
      } catch (error) {
        logger.error('Transaction update failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to update transaction'
        });
      }
    });
    
    // Get alerts
    this.app.get('/api/alerts', (req, res) => {
      try {
        const alerts = this.analytics.checkAlertThresholds();
        res.json({
          success: true,
          data: alerts,
          timestamp: Date.now()
        });
      } catch (error) {
        logger.error('Alerts request failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to retrieve alerts'
        });
      }
    });
    
    // Export data
    this.app.get('/api/export/:format', async (req, res) => {
      try {
        const format = req.params.format;
        const timeWindow = req.query.timeWindow || TIME_WINDOWS.LAST_24_HOURS;
        
        const exportData = await this.exportDashboardData(format, timeWindow);
        
        res.setHeader('Content-Type', this.getExportContentType(format));
        res.setHeader('Content-Disposition', `attachment; filename="analytics-${Date.now()}.${format}"`);
        res.send(exportData);
      } catch (error) {
        logger.error('Data export failed', { error: error.message });
        res.status(500).json({
          success: false,
          error: 'Failed to export data'
        });
      }
    });
    
    // Error handling
    this.app.use((error, req, res, next) => {
      logger.error('Dashboard error', {
        error: error.message,
        stack: error.stack,
        url: req.url
      });
      
      res.status(500).json({
        success: false,
        error: 'Internal server error'
      });
    });
  }

  /**
   * Setup Socket.IO handlers for real-time updates
   */
  setupSocketHandlers() {
    this.io.on('connection', (socket) => {
      const userId = socket.id;
      
      logger.info('Dashboard user connected', { userId });
      
      // Store user connection
      this.connectedUsers.set(userId, {
        socket,
        connectedAt: Date.now(),
        layout: this.config.defaultLayout,
        subscriptions: new Set()
      });
      
      // Handle layout changes
      socket.on('change_layout', (layout) => {
        if (Object.values(DASHBOARD_LAYOUTS).includes(layout)) {
          const user = this.connectedUsers.get(userId);
          if (user) {
            user.layout = layout;
            this.activeLayouts.set(userId, layout);
            logger.debug('User layout changed', { userId, layout });
          }
        }
      });
      
      // Handle time window changes
      socket.on('change_time_window', (timeWindow) => {
        if (Object.values(TIME_WINDOWS).includes(timeWindow)) {
          this.sendDashboardUpdate(socket, timeWindow);
        }
      });
      
      // Handle alert subscriptions
      socket.on('subscribe_alerts', () => {
        this.alertSubscriptions.add(userId);
        logger.debug('User subscribed to alerts', { userId });
      });
      
      socket.on('unsubscribe_alerts', () => {
        this.alertSubscriptions.delete(userId);
        logger.debug('User unsubscribed from alerts', { userId });
      });
      
      // Handle disconnection
      socket.on('disconnect', () => {
        this.connectedUsers.delete(userId);
        this.activeLayouts.delete(userId);
        this.alertSubscriptions.delete(userId);
        logger.info('Dashboard user disconnected', { userId });
      });
      
      // Send initial dashboard data
      this.sendDashboardUpdate(socket);
    });
  }

  /**
   * Setup analytics integration
   */
  setupAnalyticsIntegration() {
    // Listen for analytics events
    this.analytics.on('transaction_recorded', (data) => {
      this.broadcastUpdate('transaction_recorded', data);
    });
    
    this.analytics.on('transaction_status_updated', (data) => {
      this.broadcastUpdate('transaction_status_updated', data);
    });
    
    this.analytics.on('metrics_aggregated', (data) => {
      this.broadcastUpdate('metrics_aggregated', data);
    });
  }

  /**
   * Start real-time updates
   */
  startRealTimeUpdates() {
    // Real-time metrics updates
    setInterval(() => {
      const realTimeMetrics = this.analytics.getRealTimeMetrics();
      this.broadcastUpdate('realtime_metrics', realTimeMetrics);
    }, this.config.realTimeUpdateInterval);
    
    // Dashboard refresh updates
    setInterval(() => {
      this.broadcastDashboardUpdates();
    }, this.config.dashboardRefreshInterval);
    
    // Alert checking
    setInterval(() => {
      const alerts = this.analytics.checkAlertThresholds();
      if (alerts.length > 0) {
        this.broadcastAlerts(alerts);
      }
    }, 60000); // Check alerts every minute
  }

  /**
   * Broadcast update to all connected users
   */
  broadcastUpdate(eventType, data) {
    this.io.emit(eventType, {
      type: eventType,
      data,
      timestamp: Date.now()
    });
  }

  /**
   * Broadcast dashboard updates to all users
   */
  broadcastDashboardUpdates() {
    for (const [userId, user] of this.connectedUsers.entries()) {
      this.sendDashboardUpdate(user.socket, TIME_WINDOWS.LAST_24_HOURS, user.layout);
    }
  }

  /**
   * Send dashboard update to specific socket
   */
  sendDashboardUpdate(socket, timeWindow = TIME_WINDOWS.LAST_24_HOURS, layout = null) {
    try {
      const dashboardData = this.analytics.getDashboardData(timeWindow);
      const userLayout = layout || this.config.defaultLayout;
      const layoutData = this.formatDataForLayout(dashboardData, userLayout);
      
      socket.emit('dashboard_update', {
        data: layoutData,
        timestamp: Date.now(),
        timeWindow,
        layout: userLayout
      });
    } catch (error) {
      logger.error('Failed to send dashboard update', { error: error.message });
    }
  }

  /**
   * Broadcast alerts to subscribed users
   */
  broadcastAlerts(alerts) {
    for (const userId of this.alertSubscriptions) {
      const user = this.connectedUsers.get(userId);
      if (user) {
        user.socket.emit('alerts', {
          alerts,
          timestamp: Date.now()
        });
      }
    }
    
    logger.warn('Alerts broadcasted', { alertCount: alerts.length });
  }

  /**
   * Format data for specific dashboard layout
   */
  formatDataForLayout(dashboardData, layout) {
    const baseData = {
      timestamp: dashboardData.timestamp,
      timeWindow: dashboardData.timeWindow,
      timeRange: dashboardData.timeRange
    };
    
    switch (layout) {
      case DASHBOARD_LAYOUTS.EXECUTIVE:
        return {
          ...baseData,
          overview: dashboardData.transactionOverview,
          networkSummary: this.summarizeNetworkData(dashboardData.networkDistribution),
          complianceSummary: this.summarizeComplianceData(dashboardData.complianceMetrics),
          keyMetrics: this.extractKeyMetrics(dashboardData),
          alerts: dashboardData.realTimeMetrics.alertStatus || []
        };
        
      case DASHBOARD_LAYOUTS.OPERATIONS:
        return {
          ...baseData,
          overview: dashboardData.transactionOverview,
          performance: dashboardData.performanceMetrics,
          networkDistribution: dashboardData.networkDistribution,
          errorAnalysis: dashboardData.errorAnalysis,
          volumeTrends: dashboardData.volumeTrends,
          realTimeMetrics: dashboardData.realTimeMetrics
        };
        
      case DASHBOARD_LAYOUTS.COMPLIANCE:
        return {
          ...baseData,
          complianceMetrics: dashboardData.complianceMetrics,
          riskAnalysis: this.analyzeRiskData(dashboardData),
          geographicDistribution: dashboardData.geographicDistribution,
          regulatoryReporting: this.generateRegulatorySummary(dashboardData),
          alerts: dashboardData.realTimeMetrics.alertStatus || []
        };
        
      case DASHBOARD_LAYOUTS.TECHNICAL:
        return {
          ...baseData,
          performanceMetrics: dashboardData.performanceMetrics,
          networkMetrics: dashboardData.networkDistribution,
          errorAnalysis: dashboardData.errorAnalysis,
          systemHealth: this.analytics.getSystemHealth(),
          throughputAnalysis: this.analyzeThroughput(dashboardData)
        };
        
      case DASHBOARD_LAYOUTS.CBDC:
        return {
          ...baseData,
          cbdcMetrics: dashboardData.cbdcMetrics,
          cbdcOperations: this.analyzeCBDCOperations(dashboardData),
          algorandMetrics: this.extractAlgorandMetrics(dashboardData),
          centralBankActivity: this.analyzeCentralBankActivity(dashboardData)
        };
        
      default:
        return dashboardData;
    }
  }

  /**
   * Summarize network data for executive view
   */
  summarizeNetworkData(networkDistribution) {
    const networks = Object.entries(networkDistribution);
    const totalTransactions = networks.reduce((sum, [, data]) => sum + data.count, 0);
    const totalVolume = networks.reduce((sum, [, data]) => sum + data.volume, 0);
    
    return {
      totalNetworks: networks.length,
      totalTransactions,
      totalVolume,
      topNetwork: networks.sort((a, b) => b[1].count - a[1].count)[0]?.[0],
      avgSuccessRate: networks.length > 0 
        ? networks.reduce((sum, [, data]) => sum + (data.successRate || 0), 0) / networks.length 
        : 0
    };
  }

  /**
   * Summarize compliance data for executive view
   */
  summarizeComplianceData(complianceMetrics) {
    return {
      overallComplianceRate: complianceMetrics.complianceRate,
      amlCompliance: complianceMetrics.amlChecks.passed / 
        (complianceMetrics.amlChecks.passed + complianceMetrics.amlChecks.failed),
      sanctionsCompliance: complianceMetrics.sanctionsChecks.passed / 
        (complianceMetrics.sanctionsChecks.passed + complianceMetrics.sanctionsChecks.failed),
      avgRiskScore: complianceMetrics.avgRiskScore,
      pendingReviews: complianceMetrics.manualReviews.required - complianceMetrics.manualReviews.completed
    };
  }

  /**
   * Extract key metrics for executive dashboard
   */
  extractKeyMetrics(dashboardData) {
    return {
      transactionVolume: dashboardData.transactionOverview.totalVolume,
      successRate: dashboardData.transactionOverview.successRate,
      avgProcessingTime: dashboardData.transactionOverview.avgProcessingTime,
      complianceRate: dashboardData.complianceMetrics.complianceRate,
      cbdcVolume: dashboardData.cbdcMetrics?.totalVolume || 0,
      crossBorderVolume: this.calculateCrossBorderVolume(dashboardData)
    };
  }

  /**
   * Analyze risk data for compliance dashboard
   */
  analyzeRiskData(dashboardData) {
    const riskDistribution = dashboardData.complianceMetrics.riskScoreDistribution;
    const total = riskDistribution.low + riskDistribution.medium + riskDistribution.high;
    
    return {
      distribution: riskDistribution,
      percentages: {
        low: total > 0 ? (riskDistribution.low / total) * 100 : 0,
        medium: total > 0 ? (riskDistribution.medium / total) * 100 : 0,
        high: total > 0 ? (riskDistribution.high / total) * 100 : 0
      },
      totalAssessed: total,
      avgRiskScore: dashboardData.complianceMetrics.avgRiskScore
    };
  }

  /**
   * Generate regulatory summary for compliance dashboard
   */
  generateRegulatorySummary(dashboardData) {
    return {
      amlCompliance: {
        passed: dashboardData.complianceMetrics.amlChecks.passed,
        failed: dashboardData.complianceMetrics.amlChecks.failed,
        pending: dashboardData.complianceMetrics.amlChecks.pending
      },
      sanctionsScreening: {
        passed: dashboardData.complianceMetrics.sanctionsChecks.passed,
        failed: dashboardData.complianceMetrics.sanctionsChecks.failed
      },
      manualReviews: dashboardData.complianceMetrics.manualReviews,
      geographicRisk: this.assessGeographicRisk(dashboardData.geographicDistribution)
    };
  }

  /**
   * Analyze throughput for technical dashboard
   */
  analyzeThroughput(dashboardData) {
    const overview = dashboardData.transactionOverview;
    const performance = dashboardData.performanceMetrics;
    
    return {
      currentThroughput: overview.transactionsPerHour,
      maxThroughput: this.calculateMaxThroughput(performance),
      bottlenecks: this.identifyBottlenecks(performance),
      capacity: this.calculateSystemCapacity(performance)
    };
  }

  /**
   * Analyze CBDC operations
   */
  analyzeCBDCOperations(dashboardData) {
    const cbdc = dashboardData.cbdcMetrics;
    
    return {
      operationBreakdown: {
        mint: cbdc.mintOperations,
        burn: cbdc.burnOperations,
        transfer: cbdc.transferOperations,
        freeze: cbdc.freezeOperations
      },
      circulation: cbdc.totalVolume,
      activeAccounts: cbdc.activeAccountsCount,
      avgTransactionSize: cbdc.avgTransactionSize,
      operationalEfficiency: this.calculateCBDCEfficiency(cbdc)
    };
  }

  /**
   * Extract Algorand-specific metrics
   */
  extractAlgorandMetrics(dashboardData) {
    const algorandData = dashboardData.networkDistribution.ALGORAND || {};
    
    return {
      transactions: algorandData.count || 0,
      volume: algorandData.volume || 0,
      avgLatency: algorandData.avgLatency || 0,
      successRate: algorandData.successRate || 0,
      errors: algorandData.errors || 0
    };
  }

  /**
   * Analyze central bank activity
   */
  analyzeCentralBankActivity(dashboardData) {
    const cbdc = dashboardData.cbdcMetrics;
    
    return {
      monetaryPolicyActions: {
        totalMinted: cbdc.mintOperations,
        totalBurned: cbdc.burnOperations,
        netIssuance: cbdc.mintOperations - cbdc.burnOperations
      },
      regulatoryActions: {
        accountsFrozen: cbdc.freezeOperations,
        complianceInterventions: dashboardData.complianceMetrics.manualReviews.required
      },
      systemHealth: {
        operationalUptime: this.calculateCBDCUptime(),
        transactionCapacity: this.calculateCBDCCapacity(cbdc)
      }
    };
  }

  /**
   * Calculate cross-border volume
   */
  calculateCrossBorderVolume(dashboardData) {
    // This would analyze BIC codes to determine cross-border transactions
    const geo = dashboardData.geographicDistribution;
    return Object.values(geo).reduce((sum, country) => sum + (country.volume || 0), 0);
  }

  /**
   * Assess geographic risk
   */
  assessGeographicRisk(geographicDistribution) {
    // Simplified risk assessment based on country codes
    const highRiskCountries = ['XX', 'YY']; // Example high-risk countries
    let highRiskVolume = 0;
    let totalVolume = 0;
    
    Object.entries(geographicDistribution).forEach(([country, data]) => {
      totalVolume += data.volume || 0;
      if (highRiskCountries.includes(country)) {
        highRiskVolume += data.volume || 0;
      }
    });
    
    return {
      highRiskPercentage: totalVolume > 0 ? (highRiskVolume / totalVolume) * 100 : 0,
      countriesInvolved: Object.keys(geographicDistribution).length,
      highRiskCountries: highRiskCountries.filter(c => geographicDistribution[c])
    };
  }

  /**
   * Calculate max throughput
   */
  calculateMaxThroughput(performanceMetrics) {
    // Based on minimum processing times
    const minLatency = performanceMetrics.endToEndLatency.min || 1000;
    return Math.floor(1000 / minLatency) * 60; // Transactions per minute
  }

  /**
   * Identify performance bottlenecks
   */
  identifyBottlenecks(performanceMetrics) {
    const bottlenecks = [];
    
    if (performanceMetrics.swiftParsing.avg > 5000) {
      bottlenecks.push({ component: 'SWIFT Parser', avgTime: performanceMetrics.swiftParsing.avg });
    }
    
    if (performanceMetrics.bancsIntegration.avg > 10000) {
      bottlenecks.push({ component: 'BaNCS Integration', avgTime: performanceMetrics.bancsIntegration.avg });
    }
    
    if (performanceMetrics.blockchainSubmission.avg > 15000) {
      bottlenecks.push({ component: 'Blockchain Submission', avgTime: performanceMetrics.blockchainSubmission.avg });
    }
    
    return bottlenecks;
  }

  /**
   * Calculate system capacity
   */
  calculateSystemCapacity(performanceMetrics) {
    const avgProcessingTime = performanceMetrics.endToEndLatency.avg || 30000;
    const estimatedCapacity = Math.floor(3600000 / avgProcessingTime); // Per hour
    
    return {
      estimatedTransactionsPerHour: estimatedCapacity,
      currentUtilization: 0.75, // Example utilization
      recommendedMaxLoad: estimatedCapacity * 0.8
    };
  }

  /**
   * Calculate CBDC efficiency
   */
  calculateCBDCEfficiency(cbdcMetrics) {
    const totalOperations = cbdcMetrics.mintOperations + cbdcMetrics.burnOperations + 
                           cbdcMetrics.transferOperations + cbdcMetrics.freezeOperations;
    
    return {
      operationsPerAccount: cbdcMetrics.activeAccountsCount > 0 
        ? totalOperations / cbdcMetrics.activeAccountsCount 
        : 0,
      utilizationRate: cbdcMetrics.totalTransactions > 0 
        ? totalOperations / cbdcMetrics.totalTransactions 
        : 0
    };
  }

  /**
   * Calculate CBDC uptime
   */
  calculateCBDCUptime() {
    // This would integrate with actual system monitoring
    return 99.95; // Example uptime percentage
  }

  /**
   * Calculate CBDC capacity
   */
  calculateCBDCCapacity(cbdcMetrics) {
    return {
      currentTransactions: cbdcMetrics.totalTransactions,
      estimatedCapacity: 10000, // Example capacity
      utilizationPercentage: (cbdcMetrics.totalTransactions / 10000) * 100
    };
  }

  /**
   * Export dashboard data in various formats
   */
  async exportDashboardData(format, timeWindow) {
    const dashboardData = this.analytics.getDashboardData(timeWindow);
    
    switch (format.toLowerCase()) {
      case 'json':
        return JSON.stringify(dashboardData, null, 2);
        
      case 'csv':
        return this.convertToCSV(dashboardData);
        
      case 'xlsx':
        return this.convertToExcel(dashboardData);
        
      default:
        throw new Error(`Unsupported export format: ${format}`);
    }
  }

  /**
   * Convert data to CSV format
   */
  convertToCSV(data) {
    // Simple CSV conversion for transaction overview
    const overview = data.transactionOverview;
    const csv = [
      'Metric,Value',
      `Total Transactions,${overview.totalTransactions}`,
      `Successful Transactions,${overview.successfulTransactions}`,
      `Failed Transactions,${overview.failedTransactions}`,
      `Success Rate,${(overview.successRate * 100).toFixed(2)}%`,
      `Total Volume,${overview.totalVolume}`,
      `Average Processing Time,${overview.avgProcessingTime}ms`
    ].join('\n');
    
    return csv;
  }

  /**
   * Convert data to Excel format
   */
  convertToExcel(data) {
    // This would use a library like xlsx to create proper Excel files
    // For now, return CSV format
    return this.convertToCSV(data);
  }

  /**
   * Get content type for export format
   */
  getExportContentType(format) {
    const types = {
      json: 'application/json',
      csv: 'text/csv',
      xlsx: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
    };
    
    return types[format.toLowerCase()] || 'text/plain';
  }

  /**
   * Generate dashboard HTML
   */
  generateDashboardHTML(layout = null) {
    const dashboardLayout = layout || this.config.defaultLayout;
    
    return `
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>${this.config.title}</title>
    <script src="https://cdn.socket.io/4.7.4/socket.io.min.js"></script>
    <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
    <style>
        .dashboard-container { min-height: 100vh; background: #f8f9fa; }
        .metric-card { background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .metric-value { font-size: 2rem; font-weight: bold; color: #007bff; }
        .metric-label { color: #6c757d; font-size: 0.9rem; }
        .alert-badge { position: relative; }
        .chart-container { height: 300px; }
        .realtime-indicator { 
            display: inline-block; 
            width: 10px; 
            height: 10px; 
            background: #28a745; 
            border-radius: 50%; 
            animation: pulse 2s infinite;
        }
        @keyframes pulse {
            0% { opacity: 1; }
            50% { opacity: 0.5; }
            100% { opacity: 1; }
        }
    </style>
</head>
<body>
    <div class="dashboard-container">
        <nav class="navbar navbar-dark bg-primary">
            <div class="container-fluid">
                <span class="navbar-brand mb-0 h1">
                    <span class="realtime-indicator"></span>
                    ${this.config.title}
                </span>
                <div class="d-flex">
                    <select id="layoutSelect" class="form-select me-3">
                        <option value="executive">Executive</option>
                        <option value="operations">Operations</option>
                        <option value="compliance">Compliance</option>
                        <option value="technical">Technical</option>
                        <option value="cbdc">CBDC</option>
                    </select>
                    <select id="timeWindowSelect" class="form-select">
                        <option value="real_time">Real Time</option>
                        <option value="last_hour">Last Hour</option>
                        <option value="last_24_hours" selected>Last 24 Hours</option>
                        <option value="last_7_days">Last 7 Days</option>
                        <option value="last_30_days">Last 30 Days</option>
                    </select>
                </div>
            </div>
        </nav>
        
        <div class="container-fluid p-4">
            <div id="alertsContainer"></div>
            <div id="dashboardContent">
                <div class="text-center p-5">
                    <div class="spinner-border text-primary" role="status">
                        <span class="visually-hidden">Loading...</span>
                    </div>
                    <p class="mt-3">Loading dashboard data...</p>
                </div>
            </div>
        </div>
    </div>

    <script>
        // Initialize Socket.IO connection
        const socket = io();
        let currentLayout = '${dashboardLayout}';
        let currentTimeWindow = 'last_24_hours';

        // Set initial layout
        document.getElementById('layoutSelect').value = currentLayout;

        // Handle layout changes
        document.getElementById('layoutSelect').addEventListener('change', (e) => {
            currentLayout = e.target.value;
            socket.emit('change_layout', currentLayout);
            loadDashboardData();
        });

        // Handle time window changes
        document.getElementById('timeWindowSelect').addEventListener('change', (e) => {
            currentTimeWindow = e.target.value;
            socket.emit('change_time_window', currentTimeWindow);
            loadDashboardData();
        });

        // Socket event handlers
        socket.on('dashboard_update', (data) => {
            updateDashboard(data.data, data.layout);
        });

        socket.on('realtime_metrics', (data) => {
            updateRealTimeMetrics(data.data);
        });

        socket.on('alerts', (data) => {
            displayAlerts(data.alerts);
        });

        // Load initial dashboard data
        function loadDashboardData() {
            fetch(\`/api/dashboard/\${currentTimeWindow}?layout=\${currentLayout}\`)
                .then(response => response.json())
                .then(data => {
                    if (data.success) {
                        updateDashboard(data.data, currentLayout);
                    }
                })
                .catch(error => console.error('Failed to load dashboard data:', error));
        }

        // Update dashboard content
        function updateDashboard(data, layout) {
            const content = document.getElementById('dashboardContent');
            content.innerHTML = generateDashboardHTML(data, layout);
        }

        // Generate dashboard HTML based on layout
        function generateDashboardHTML(data, layout) {
            switch (layout) {
                case 'executive':
                    return generateExecutiveDashboard(data);
                case 'operations':
                    return generateOperationsDashboard(data);
                case 'compliance':
                    return generateComplianceDashboard(data);
                case 'technical':
                    return generateTechnicalDashboard(data);
                case 'cbdc':
                    return generateCBDCDashboard(data);
                default:
                    return '<div class="alert alert-warning">Unknown dashboard layout</div>';
            }
        }

        // Generate executive dashboard
        function generateExecutiveDashboard(data) {
            return \`
                <div class="row g-4">
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${data.overview?.totalTransactions || 0}</div>
                            <div class="metric-label">Total Transactions</div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${((data.overview?.successRate || 0) * 100).toFixed(1)}%</div>
                            <div class="metric-label">Success Rate</div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">$\${(data.overview?.totalVolume || 0).toLocaleString()}</div>
                            <div class="metric-label">Total Volume</div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${data.overview?.avgProcessingTime || 0}ms</div>
                            <div class="metric-label">Avg Processing Time</div>
                        </div>
                    </div>
                </div>
                <div class="row g-4 mt-4">
                    <div class="col-md-6">
                        <div class="metric-card p-4">
                            <h5>Network Summary</h5>
                            <canvas id="networkChart" class="chart-container"></canvas>
                        </div>
                    </div>
                    <div class="col-md-6">
                        <div class="metric-card p-4">
                            <h5>Compliance Summary</h5>
                            <div class="metric-value">\${((data.complianceSummary?.overallComplianceRate || 0) * 100).toFixed(1)}%</div>
                            <div class="metric-label">Overall Compliance Rate</div>
                        </div>
                    </div>
                </div>
            \`;
        }

        // Generate operations dashboard
        function generateOperationsDashboard(data) {
            return \`
                <div class="row g-4">
                    <div class="col-md-12">
                        <div class="metric-card p-4">
                            <h5>Transaction Overview</h5>
                            <div class="row">
                                <div class="col-md-2">
                                    <div class="metric-value">\${data.overview?.totalTransactions || 0}</div>
                                    <div class="metric-label">Total</div>
                                </div>
                                <div class="col-md-2">
                                    <div class="metric-value text-success">\${data.overview?.successfulTransactions || 0}</div>
                                    <div class="metric-label">Successful</div>
                                </div>
                                <div class="col-md-2">
                                    <div class="metric-value text-danger">\${data.overview?.failedTransactions || 0}</div>
                                    <div class="metric-label">Failed</div>
                                </div>
                                <div class="col-md-2">
                                    <div class="metric-value text-warning">\${data.overview?.pendingTransactions || 0}</div>
                                    <div class="metric-label">Pending</div>
                                </div>
                                <div class="col-md-2">
                                    <div class="metric-value">\${data.overview?.transactionsPerHour?.toFixed(1) || 0}</div>
                                    <div class="metric-label">Per Hour</div>
                                </div>
                                <div class="col-md-2">
                                    <div class="metric-value">\${data.overview?.avgProcessingTime || 0}ms</div>
                                    <div class="metric-label">Avg Time</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            \`;
        }

        // Generate compliance dashboard
        function generateComplianceDashboard(data) {
            return \`
                <div class="row g-4">
                    <div class="col-md-4">
                        <div class="metric-card p-4">
                            <h6>AML Compliance</h6>
                            <div class="metric-value text-success">\${data.complianceMetrics?.amlChecks?.passed || 0}</div>
                            <div class="metric-label">Passed</div>
                        </div>
                    </div>
                    <div class="col-md-4">
                        <div class="metric-card p-4">
                            <h6>Sanctions Screening</h6>
                            <div class="metric-value text-success">\${data.complianceMetrics?.sanctionsChecks?.passed || 0}</div>
                            <div class="metric-label">Passed</div>
                        </div>
                    </div>
                    <div class="col-md-4">
                        <div class="metric-card p-4">
                            <h6>Manual Reviews</h6>
                            <div class="metric-value text-warning">\${data.complianceMetrics?.manualReviews?.required || 0}</div>
                            <div class="metric-label">Required</div>
                        </div>
                    </div>
                </div>
            \`;
        }

        // Generate technical dashboard
        function generateTechnicalDashboard(data) {
            return \`
                <div class="row g-4">
                    <div class="col-md-12">
                        <div class="metric-card p-4">
                            <h5>Performance Metrics</h5>
                            <div class="row">
                                <div class="col-md-3">
                                    <div class="metric-value">\${data.performanceMetrics?.endToEndLatency?.avg?.toFixed(0) || 0}ms</div>
                                    <div class="metric-label">Avg Latency</div>
                                </div>
                                <div class="col-md-3">
                                    <div class="metric-value">\${data.performanceMetrics?.endToEndLatency?.p95?.toFixed(0) || 0}ms</div>
                                    <div class="metric-label">P95 Latency</div>
                                </div>
                                <div class="col-md-3">
                                    <div class="metric-value">\${data.systemHealth?.isHealthy ? 'Healthy' : 'Warning'}</div>
                                    <div class="metric-label">System Status</div>
                                </div>
                                <div class="col-md-3">
                                    <div class="metric-value">\${(data.systemHealth?.uptime / 3600)?.toFixed(1) || 0}h</div>
                                    <div class="metric-label">Uptime</div>
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
            \`;
        }

        // Generate CBDC dashboard
        function generateCBDCDashboard(data) {
            return \`
                <div class="row g-4">
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${data.cbdcMetrics?.totalTransactions || 0}</div>
                            <div class="metric-label">CBDC Transactions</div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${data.cbdcMetrics?.mintOperations || 0}</div>
                            <div class="metric-label">Mint Operations</div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${data.cbdcMetrics?.burnOperations || 0}</div>
                            <div class="metric-label">Burn Operations</div>
                        </div>
                    </div>
                    <div class="col-md-3">
                        <div class="metric-card p-4">
                            <div class="metric-value">\${data.cbdcMetrics?.activeAccountsCount || 0}</div>
                            <div class="metric-label">Active Accounts</div>
                        </div>
                    </div>
                </div>
            \`;
        }

        // Update real-time metrics
        function updateRealTimeMetrics(metrics) {
            // Update any real-time indicators
            console.log('Real-time metrics updated:', metrics);
        }

        // Display alerts
        function displayAlerts(alerts) {
            const container = document.getElementById('alertsContainer');
            if (alerts.length === 0) {
                container.innerHTML = '';
                return;
            }

            const alertsHTML = alerts.map(alert => \`
                <div class="alert alert-\${alert.severity === 'high' ? 'danger' : alert.severity === 'medium' ? 'warning' : 'info'} alert-dismissible fade show" role="alert">
                    <strong>\${alert.type.toUpperCase()}:</strong> 
                    Current: \${alert.current} (Threshold: \${alert.threshold})
                    <button type="button" class="btn-close" data-bs-dismiss="alert"></button>
                </div>
            \`).join('');

            container.innerHTML = alertsHTML;
        }

        // Subscribe to alerts
        socket.emit('subscribe_alerts');

        // Load initial data
        loadDashboardData();
    </script>
</body>
</html>
    `;
  }

  /**
   * Start the dashboard server
   */
  async start() {
    return new Promise((resolve, reject) => {
      this.server.listen(this.config.port, this.config.host, (error) => {
        if (error) {
          logger.error('Failed to start dashboard server', { error: error.message });
          reject(error);
        } else {
          logger.info('Monitoring Dashboard started', {
            port: this.config.port,
            host: this.config.host,
            url: `http://${this.config.host}:${this.config.port}`
          });
          resolve();
        }
      });
    });
  }

  /**
   * Stop the dashboard server
   */
  async stop() {
    return new Promise((resolve) => {
      this.server.close(() => {
        this.analytics.shutdown();
        logger.info('Monitoring Dashboard stopped');
        resolve();
      });
    });
  }

  /**
   * Get connected users info
   */
  getConnectedUsers() {
    return Array.from(this.connectedUsers.entries()).map(([userId, user]) => ({
      userId,
      connectedAt: user.connectedAt,
      layout: user.layout,
      subscriptions: Array.from(user.subscriptions)
    }));
  }
}

module.exports = {
  MonitoringDashboard,
  DASHBOARD_THEMES,
  DASHBOARD_LAYOUTS
};