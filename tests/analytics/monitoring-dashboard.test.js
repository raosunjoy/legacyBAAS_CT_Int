/**
 * Monitoring Dashboard Test Suite
 * Tests real-time dashboard functionality and API endpoints
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 3 - Advanced Enterprise Features
 */

const request = require('supertest');
const { Server } = require('socket.io');
const Client = require('socket.io-client');
const { MonitoringDashboard, DASHBOARD_THEMES, DASHBOARD_LAYOUTS } = require('../../src/analytics/monitoring-dashboard');

// Mock winston logger
jest.mock('winston', () => ({
  createLogger: jest.fn(() => ({
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn()
  })),
  format: {
    combine: jest.fn(),
    timestamp: jest.fn(),
    json: jest.fn()
  }
}));

// Mock socket.io for testing
jest.mock('socket.io', () => {
  const mockIo = {
    on: jest.fn(),
    emit: jest.fn(),
    use: jest.fn()
  };
  return jest.fn(() => mockIo);
});

describe('Monitoring Dashboard', () => {
  let dashboard;
  let testConfig;

  beforeEach(() => {
    testConfig = {
      port: 0, // Use random available port for testing
      title: 'Test Banking Dashboard',
      theme: DASHBOARD_THEMES.CORPORATE,
      defaultLayout: DASHBOARD_LAYOUTS.EXECUTIVE,
      enableAuth: false, // Disable auth for testing
      realTimeUpdateInterval: 100000, // Long interval to prevent timer issues in tests
      dashboardRefreshInterval: 200000,
      corsOrigins: ['http://localhost:3000'],
      retentionPeriod: 24 * 60 * 60 * 1000
    };

    dashboard = new MonitoringDashboard(testConfig);
  });

  afterEach(async () => {
    if (dashboard) {
      await dashboard.stop();
    }
  });

  describe('Initialization', () => {
    test('should initialize with default configuration', () => {
      const defaultDashboard = new MonitoringDashboard();
      
      expect(defaultDashboard.config.port).toBe(3002);
      expect(defaultDashboard.config.title).toBe('LegacyBaaS Banking Platform - Analytics Dashboard');
      expect(defaultDashboard.config.theme).toBe(DASHBOARD_THEMES.CORPORATE);
      expect(defaultDashboard.config.defaultLayout).toBe(DASHBOARD_LAYOUTS.EXECUTIVE);
      expect(defaultDashboard.config.enableAuth).toBe(true);
      
      defaultDashboard.stop();
    });

    test('should initialize with custom configuration', () => {
      expect(dashboard.config.title).toBe('Test Banking Dashboard');
      expect(dashboard.config.theme).toBe(DASHBOARD_THEMES.CORPORATE);
      expect(dashboard.config.defaultLayout).toBe(DASHBOARD_LAYOUTS.EXECUTIVE);
      expect(dashboard.config.enableAuth).toBe(false);
      expect(dashboard.config.realTimeUpdateInterval).toBe(100000);
    });

    test('should initialize Express app and analytics', () => {
      expect(dashboard.app).toBeDefined();
      expect(dashboard.server).toBeDefined();
      expect(dashboard.io).toBeDefined();
      expect(dashboard.analytics).toBeDefined();
    });

    test('should initialize empty user tracking', () => {
      expect(dashboard.connectedUsers.size).toBe(0);
      expect(dashboard.activeLayouts.size).toBe(0);
      expect(dashboard.alertSubscriptions.size).toBe(0);
    });
  });

  describe('Express Routes', () => {
    test('should serve main dashboard page', async () => {
      const response = await request(dashboard.app)
        .get('/')
        .expect(200);

      expect(response.text).toContain('Test Banking Dashboard');
      expect(response.text).toContain('<!DOCTYPE html>');
      expect(response.text).toContain('socket.io');
    });

    test('should serve dashboard with specific layout', async () => {
      const response = await request(dashboard.app)
        .get('/dashboard/operations')
        .expect(200);

      expect(response.text).toContain('Test Banking Dashboard');
    });

    test('should reject invalid dashboard layout', async () => {
      await request(dashboard.app)
        .get('/dashboard/invalid')
        .expect(400);
    });

    test('should get real-time metrics', async () => {
      const response = await request(dashboard.app)
        .get('/api/realtime')
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.data).toBeDefined();
      expect(response.body.timestamp).toBeDefined();
    });

    test('should get system health', async () => {
      const response = await request(dashboard.app)
        .get('/api/health')
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.data.timestamp).toBeDefined();
      expect(response.body.data.isHealthy).toBeDefined();
    });

    test('should get dashboard data', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours')
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.data).toBeDefined();
      expect(response.body.timeWindow).toBe('last_24_hours');
    });

    test('should get dashboard data with layout parameter', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours?layout=operations')
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.layout).toBe('operations');
    });

    test('should submit transaction data', async () => {
      const transactionData = {
        id: 'TEST-TXN-001',
        type: 'payment',
        amount: 1000,
        currency: 'USD',
        targetNetwork: 'XRP',
        status: 'initiated'
      };

      const response = await request(dashboard.app)
        .post('/api/transactions')
        .send(transactionData)
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.transactionId).toBe('TEST-TXN-001');
    });

    test('should update transaction status', async () => {
      // First submit a transaction
      await request(dashboard.app)
        .post('/api/transactions')
        .send({
          id: 'TEST-UPDATE-001',
          type: 'payment',
          amount: 1000,
          targetNetwork: 'XRP'
        });

      // Then update its status
      const response = await request(dashboard.app)
        .put('/api/transactions/TEST-UPDATE-001')
        .send({
          status: 'confirmed',
          processingTime: 5000
        })
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.transactionId).toBe('TEST-UPDATE-001');
    });

    test('should get alerts', async () => {
      const response = await request(dashboard.app)
        .get('/api/alerts')
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(Array.isArray(response.body.data)).toBe(true);
    });
  });

  describe('Data Export', () => {
    test('should export data in JSON format', async () => {
      const response = await request(dashboard.app)
        .get('/api/export/json?timeWindow=last_24_hours')
        .expect(200);

      expect(response.headers['content-type']).toContain('application/json');
      expect(response.headers['content-disposition']).toContain('attachment');
      
      const exportedData = JSON.parse(response.text);
      expect(exportedData.timestamp).toBeDefined();
      expect(exportedData.timeWindow).toBe('last_24_hours');
    });

    test('should export data in CSV format', async () => {
      const response = await request(dashboard.app)
        .get('/api/export/csv?timeWindow=last_24_hours')
        .expect(200);

      expect(response.headers['content-type']).toContain('text/csv');
      expect(response.text).toContain('Metric,Value');
      expect(response.text).toContain('Total Transactions');
    });

    test('should handle unsupported export format', async () => {
      await request(dashboard.app)
        .get('/api/export/xml')
        .expect(500);
    });
  });

  describe('Layout Data Formatting', () => {
    beforeEach(() => {
      // Add sample data to analytics
      dashboard.analytics.recordTransaction({
        id: 'LAYOUT-001',
        type: 'swift_mt103',
        amount: 10000,
        currency: 'USD',
        targetNetwork: 'XRP',
        status: 'confirmed',
        complianceStatus: 'passed',
        amlStatus: 'passed',
        sanctionsCheck: 'passed',
        riskScore: 25
      });

      dashboard.analytics.recordTransaction({
        id: 'LAYOUT-002',
        type: 'cbdc_mint',
        amount: 1000000,
        targetNetwork: 'ALGORAND',
        status: 'confirmed',
        metadata: {
          cbdcType: 'mint',
          senderAddress: 'CENTRAL_BANK'
        }
      });
    });

    test('should format data for executive layout', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours?layout=executive')
        .expect(200);

      const data = response.body.data;
      expect(data.overview).toBeDefined();
      expect(data.networkSummary).toBeDefined();
      expect(data.complianceSummary).toBeDefined();
      expect(data.keyMetrics).toBeDefined();
      expect(data.alerts).toBeDefined();
    });

    test('should format data for operations layout', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours?layout=operations')
        .expect(200);

      const data = response.body.data;
      expect(data.overview).toBeDefined();
      expect(data.performance).toBeDefined();
      expect(data.networkDistribution).toBeDefined();
      expect(data.errorAnalysis).toBeDefined();
      expect(data.volumeTrends).toBeDefined();
      expect(data.realTimeMetrics).toBeDefined();
    });

    test('should format data for compliance layout', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours?layout=compliance')
        .expect(200);

      const data = response.body.data;
      expect(data.complianceMetrics).toBeDefined();
      expect(data.riskAnalysis).toBeDefined();
      expect(data.geographicDistribution).toBeDefined();
      expect(data.regulatoryReporting).toBeDefined();
    });

    test('should format data for technical layout', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours?layout=technical')
        .expect(200);

      const data = response.body.data;
      expect(data.performanceMetrics).toBeDefined();
      expect(data.networkMetrics).toBeDefined();
      expect(data.errorAnalysis).toBeDefined();
      expect(data.systemHealth).toBeDefined();
      expect(data.throughputAnalysis).toBeDefined();
    });

    test('should format data for CBDC layout', async () => {
      const response = await request(dashboard.app)
        .get('/api/dashboard/last_24_hours?layout=cbdc')
        .expect(200);

      const data = response.body.data;
      expect(data.cbdcMetrics).toBeDefined();
      expect(data.cbdcOperations).toBeDefined();
      expect(data.algorandMetrics).toBeDefined();
      expect(data.centralBankActivity).toBeDefined();
    });
  });

  describe('Layout Data Processing', () => {
    test('should summarize network data correctly', () => {
      const networkDistribution = {
        XRP: { count: 10, volume: 50000, successRate: 0.9 },
        CORDA: { count: 5, volume: 100000, successRate: 0.95 },
        ALGORAND: { count: 3, volume: 2000000, successRate: 1.0 }
      };

      const summary = dashboard.summarizeNetworkData(networkDistribution);

      expect(summary.totalNetworks).toBe(3);
      expect(summary.totalTransactions).toBe(18);
      expect(summary.totalVolume).toBe(2150000);
      expect(summary.topNetwork).toBe('XRP');
      expect(summary.avgSuccessRate).toBeCloseTo(0.95, 2);
    });

    test('should summarize compliance data correctly', () => {
      const complianceMetrics = {
        complianceRate: 0.85,
        amlChecks: { passed: 8, failed: 2 },
        sanctionsChecks: { passed: 9, failed: 1 },
        avgRiskScore: 35,
        manualReviews: { required: 5, completed: 3 }
      };

      const summary = dashboard.summarizeComplianceData(complianceMetrics);

      expect(summary.overallComplianceRate).toBe(0.85);
      expect(summary.amlCompliance).toBe(0.8);
      expect(summary.sanctionsCompliance).toBe(0.9);
      expect(summary.avgRiskScore).toBe(35);
      expect(summary.pendingReviews).toBe(2);
    });

    test('should extract key metrics correctly', () => {
      const dashboardData = {
        transactionOverview: {
          totalVolume: 1000000,
          successRate: 0.95,
          avgProcessingTime: 2500
        },
        complianceMetrics: {
          complianceRate: 0.9
        },
        cbdcMetrics: {
          totalVolume: 500000
        }
      };

      // Add missing geographicDistribution
      dashboardData.geographicDistribution = {
        'US': { volume: 100000 },
        'DE': { volume: 200000 }
      };

      const keyMetrics = dashboard.extractKeyMetrics(dashboardData);

      expect(keyMetrics.transactionVolume).toBe(1000000);
      expect(keyMetrics.successRate).toBe(0.95);
      expect(keyMetrics.avgProcessingTime).toBe(2500);
      expect(keyMetrics.complianceRate).toBe(0.9);
      expect(keyMetrics.cbdcVolume).toBe(500000);
      expect(keyMetrics.crossBorderVolume).toBe(300000);
    });

    test('should analyze risk data correctly', () => {
      const dashboardData = {
        complianceMetrics: {
          riskScoreDistribution: { low: 60, medium: 30, high: 10 },
          avgRiskScore: 35
        }
      };

      const riskAnalysis = dashboard.analyzeRiskData(dashboardData);

      expect(riskAnalysis.distribution).toEqual({ low: 60, medium: 30, high: 10 });
      expect(riskAnalysis.percentages.low).toBe(60);
      expect(riskAnalysis.percentages.medium).toBe(30);
      expect(riskAnalysis.percentages.high).toBe(10);
      expect(riskAnalysis.totalAssessed).toBe(100);
      expect(riskAnalysis.avgRiskScore).toBe(35);
    });

    test('should analyze CBDC operations correctly', () => {
      const cbdcMetrics = {
        mintOperations: 5,
        burnOperations: 2,
        transferOperations: 50,
        freezeOperations: 1,
        totalVolume: 1000000,
        activeAccountsCount: 25,
        avgTransactionSize: 17857,
        totalTransactions: 58
      };

      const operations = dashboard.analyzeCBDCOperations({ cbdcMetrics });

      expect(operations.operationBreakdown.mint).toBe(5);
      expect(operations.operationBreakdown.burn).toBe(2);
      expect(operations.operationBreakdown.transfer).toBe(50);
      expect(operations.operationBreakdown.freeze).toBe(1);
      expect(operations.circulation).toBe(1000000);
      expect(operations.activeAccounts).toBe(25);
      expect(operations.avgTransactionSize).toBe(17857);
    });
  });

  describe('Content Type Detection', () => {
    test('should return correct content types for export formats', () => {
      expect(dashboard.getExportContentType('json')).toBe('application/json');
      expect(dashboard.getExportContentType('csv')).toBe('text/csv');
      expect(dashboard.getExportContentType('xlsx')).toBe('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet');
      expect(dashboard.getExportContentType('unknown')).toBe('text/plain');
    });
  });

  describe('CSV Conversion', () => {
    test('should convert transaction overview to CSV', () => {
      const data = {
        transactionOverview: {
          totalTransactions: 100,
          successfulTransactions: 95,
          failedTransactions: 5,
          successRate: 0.95,
          totalVolume: 1000000,
          avgProcessingTime: 2500
        }
      };

      const csv = dashboard.convertToCSV(data);

      expect(csv).toContain('Metric,Value');
      expect(csv).toContain('Total Transactions,100');
      expect(csv).toContain('Successful Transactions,95');
      expect(csv).toContain('Failed Transactions,5');
      expect(csv).toContain('Success Rate,95.00%');
      expect(csv).toContain('Total Volume,1000000');
      expect(csv).toContain('Average Processing Time,2500ms');
    });
  });

  describe('HTML Dashboard Generation', () => {
    test('should generate basic dashboard HTML', () => {
      const html = dashboard.generateDashboardHTML();

      expect(html).toContain('<!DOCTYPE html>');
      expect(html).toContain('Test Banking Dashboard');
      expect(html).toContain('socket.io');
      expect(html).toContain('chart.js');
      expect(html).toContain('bootstrap');
    });

    test('should generate dashboard HTML with specific layout', () => {
      const html = dashboard.generateDashboardHTML('operations');

      expect(html).toContain('Test Banking Dashboard');
      expect(html).toContain('operations');
    });

    test('should include CSS styling', () => {
      const html = dashboard.generateDashboardHTML();

      expect(html).toContain('.dashboard-container');
      expect(html).toContain('.metric-card');
      expect(html).toContain('.realtime-indicator');
      expect(html).toContain('@keyframes pulse');
    });

    test('should include JavaScript functionality', () => {
      const html = dashboard.generateDashboardHTML();

      expect(html).toContain('const socket = io()');
      expect(html).toContain('socket.on(\'dashboard_update\'');
      expect(html).toContain('loadDashboardData()');
      expect(html).toContain('generateExecutiveDashboard');
    });
  });

  describe('Constants and Exports', () => {
    test('should export dashboard themes correctly', () => {
      expect(DASHBOARD_THEMES.DARK).toBe('dark');
      expect(DASHBOARD_THEMES.LIGHT).toBe('light');
      expect(DASHBOARD_THEMES.CORPORATE).toBe('corporate');
    });

    test('should export dashboard layouts correctly', () => {
      expect(DASHBOARD_LAYOUTS.EXECUTIVE).toBe('executive');
      expect(DASHBOARD_LAYOUTS.OPERATIONS).toBe('operations');
      expect(DASHBOARD_LAYOUTS.COMPLIANCE).toBe('compliance');
      expect(DASHBOARD_LAYOUTS.TECHNICAL).toBe('technical');
      expect(DASHBOARD_LAYOUTS.CBDC).toBe('cbdc');
    });
  });

  describe('Error Handling', () => {
    test('should handle dashboard data retrieval errors', async () => {
      // Mock analytics to throw error
      dashboard.analytics.getDashboardData = jest.fn(() => {
        throw new Error('Analytics error');
      });

      await request(dashboard.app)
        .get('/api/dashboard/last_24_hours')
        .expect(500);
    });

    test('should handle real-time metrics errors', async () => {
      // Mock analytics to throw error
      dashboard.analytics.getRealTimeMetrics = jest.fn(() => {
        throw new Error('Metrics error');
      });

      await request(dashboard.app)
        .get('/api/realtime')
        .expect(500);
    });

    test('should handle system health errors', async () => {
      // Mock analytics to throw error
      dashboard.analytics.getSystemHealth = jest.fn(() => {
        throw new Error('Health check error');
      });

      await request(dashboard.app)
        .get('/api/health')
        .expect(500);
    });

    test('should handle transaction recording errors', async () => {
      // Mock analytics to throw error
      dashboard.analytics.recordTransaction = jest.fn(() => {
        throw new Error('Recording error');
      });

      await request(dashboard.app)
        .post('/api/transactions')
        .send({
          id: 'ERROR-TXN',
          type: 'payment',
          amount: 1000
        })
        .expect(500);
    });
  });

  describe('Geographic Risk Assessment', () => {
    test('should assess geographic risk correctly', () => {
      const geographicDistribution = {
        'US': { volume: 1000000 },
        'DE': { volume: 500000 },
        'XX': { volume: 100000 }, // High-risk country
        'YY': { volume: 50000 }   // High-risk country
      };

      const riskAssessment = dashboard.assessGeographicRisk(geographicDistribution);

      expect(riskAssessment.countriesInvolved).toBe(4);
      expect(riskAssessment.highRiskPercentage).toBeCloseTo(9.09, 2); // (150000 / 1650000) * 100
      expect(riskAssessment.highRiskCountries).toEqual(['XX', 'YY']);
    });

    test('should handle empty geographic distribution', () => {
      const riskAssessment = dashboard.assessGeographicRisk({});

      expect(riskAssessment.countriesInvolved).toBe(0);
      expect(riskAssessment.highRiskPercentage).toBe(0);
      expect(riskAssessment.highRiskCountries).toEqual([]);
    });
  });

  describe('Performance Analysis', () => {
    test('should calculate max throughput correctly', () => {
      const performanceMetrics = {
        endToEndLatency: { min: 1000, avg: 2500, max: 5000 }
      };

      const maxThroughput = dashboard.calculateMaxThroughput(performanceMetrics);

      expect(maxThroughput).toBe(60); // 1000ms / 1000 * 60 = 60 per minute
    });

    test('should identify bottlenecks correctly', () => {
      const performanceMetrics = {
        swiftParsing: { avg: 6000 },        // Exceeds 5000ms threshold
        bancsIntegration: { avg: 12000 },   // Exceeds 10000ms threshold
        blockchainSubmission: { avg: 8000 } // Below 15000ms threshold
      };

      const bottlenecks = dashboard.identifyBottlenecks(performanceMetrics);

      expect(bottlenecks).toHaveLength(2);
      expect(bottlenecks[0].component).toBe('SWIFT Parser');
      expect(bottlenecks[0].avgTime).toBe(6000);
      expect(bottlenecks[1].component).toBe('BaNCS Integration');
      expect(bottlenecks[1].avgTime).toBe(12000);
    });

    test('should calculate system capacity correctly', () => {
      const performanceMetrics = {
        endToEndLatency: { avg: 3000 }
      };

      const capacity = dashboard.calculateSystemCapacity(performanceMetrics);

      expect(capacity.estimatedTransactionsPerHour).toBe(1200); // 3600000 / 3000
      expect(capacity.currentUtilization).toBe(0.75);
      expect(capacity.recommendedMaxLoad).toBe(960); // 1200 * 0.8
    });
  });

  describe('CBDC Analytics', () => {
    test('should calculate CBDC efficiency correctly', () => {
      const cbdcMetrics = {
        mintOperations: 10,
        burnOperations: 5,
        transferOperations: 100,
        freezeOperations: 2,
        activeAccountsCount: 50,
        totalTransactions: 150
      };

      const efficiency = dashboard.calculateCBDCEfficiency(cbdcMetrics);

      expect(efficiency.operationsPerAccount).toBeCloseTo(2.34, 2); // 117 / 50
      expect(efficiency.utilizationRate).toBeCloseTo(0.78, 2); // 117 / 150
    });

    test('should handle zero values in CBDC efficiency', () => {
      const cbdcMetrics = {
        mintOperations: 0,
        burnOperations: 0,
        transferOperations: 0,
        freezeOperations: 0,
        activeAccountsCount: 0,
        totalTransactions: 0
      };

      const efficiency = dashboard.calculateCBDCEfficiency(cbdcMetrics);

      expect(efficiency.operationsPerAccount).toBe(0);
      expect(efficiency.utilizationRate).toBe(0);
    });

    test('should calculate CBDC capacity correctly', () => {
      const cbdcMetrics = {
        totalTransactions: 2500
      };

      const capacity = dashboard.calculateCBDCCapacity(cbdcMetrics);

      expect(capacity.currentTransactions).toBe(2500);
      expect(capacity.estimatedCapacity).toBe(10000);
      expect(capacity.utilizationPercentage).toBe(25);
    });
  });
});