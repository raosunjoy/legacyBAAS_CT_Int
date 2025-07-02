/**
 * Transaction Analytics Test Suite
 * Tests advanced analytics engine and monitoring capabilities
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 3 - Advanced Enterprise Features
 */

const { 
  TransactionAnalytics, 
  ANALYTICS_METRICS, 
  TIME_WINDOWS, 
  AGGREGATION_TYPES 
} = require('../../src/analytics/transaction-analytics');

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

describe('Transaction Analytics Engine', () => {
  let analytics;
  let testConfig;

  beforeEach(() => {
    testConfig = {
      retentionPeriod: 24 * 60 * 60 * 1000, // 24 hours for testing
      aggregationInterval: 1000, // 1 second for testing
      enableRealTimeMetrics: true,
      enableComplianceTracking: true,
      enablePerformanceTracking: true,
      maxDataPoints: 100,
      alertThresholds: {
        errorRate: 0.1,
        processingTime: 10000,
        complianceRate: 0.8
      }
    };

    analytics = new TransactionAnalytics(testConfig);
  });

  afterEach(() => {
    if (analytics) {
      analytics.shutdown();
    }
  });

  describe('Initialization', () => {
    test('should initialize with default configuration', () => {
      const defaultAnalytics = new TransactionAnalytics();
      
      expect(defaultAnalytics.config.retentionPeriod).toBe(30 * 24 * 60 * 60 * 1000);
      expect(defaultAnalytics.config.aggregationInterval).toBe(60000);
      expect(defaultAnalytics.config.enableRealTimeMetrics).toBe(true);
      expect(defaultAnalytics.config.maxDataPoints).toBe(1000);
      
      defaultAnalytics.shutdown();
    });

    test('should initialize with custom configuration', () => {
      expect(analytics.config.retentionPeriod).toBe(24 * 60 * 60 * 1000);
      expect(analytics.config.aggregationInterval).toBe(1000);
      expect(analytics.config.enableRealTimeMetrics).toBe(true);
      expect(analytics.config.alertThresholds.errorRate).toBe(0.1);
    });

    test('should initialize empty metrics', () => {
      expect(analytics.realTimeMetrics.transactionCount).toBe(0);
      expect(analytics.realTimeMetrics.errorCount).toBe(0);
      expect(analytics.realTimeMetrics.processingTimes).toEqual([]);
      expect(analytics.transactionData.size).toBe(0);
    });

    test('should initialize network metrics', () => {
      expect(analytics.networkMetrics.has('XRP')).toBe(true);
      expect(analytics.networkMetrics.has('CORDA')).toBe(true);
      expect(analytics.networkMetrics.has('ETHEREUM_L2')).toBe(true);
      expect(analytics.networkMetrics.has('ALGORAND')).toBe(true);
    });
  });

  describe('Transaction Recording', () => {
    test('should record basic transaction', () => {
      const transactionData = {
        id: 'TXN-001',
        type: 'payment',
        amount: 1000,
        currency: 'USD',
        targetNetwork: 'XRP',
        status: 'initiated',
        processingTime: 2500
      };

      const transactionId = analytics.recordTransaction(transactionData);

      expect(transactionId).toBe('TXN-001');
      expect(analytics.transactionData.has('TXN-001')).toBe(true);
      expect(analytics.realTimeMetrics.transactionCount).toBe(1);
      expect(analytics.realTimeMetrics.processingTimes).toContain(2500);
    });

    test('should record transaction with banking context', () => {
      const transactionData = {
        id: 'TXN-002',
        type: 'swift_mt103',
        amount: 50000,
        currency: 'EUR',
        targetNetwork: 'CORDA',
        swiftMessageType: 'MT103',
        sendingBIC: 'BANKDEFF',
        receivingBIC: 'CHASUS33',
        complianceStatus: 'passed',
        riskScore: 25,
        swiftParsingTime: 100,
        bancsProcessingTime: 1500,
        blockchainSubmissionTime: 3000
      };

      const transactionId = analytics.recordTransaction(transactionData);

      const stored = analytics.transactionData.get(transactionId);
      expect(stored.swiftMessageType).toBe('MT103');
      expect(stored.sendingBIC).toBe('BANKDEFF');
      expect(stored.receivingBIC).toBe('CHASUS33');
      expect(stored.complianceStatus).toBe('passed');
      expect(stored.riskScore).toBe(25);
    });

    test('should record CBDC transaction', () => {
      const transactionData = {
        id: 'CBDC-001',
        type: 'cbdc_mint',
        amount: 1000000,
        currency: 'CBDC',
        targetNetwork: 'ALGORAND',
        metadata: {
          cbdcType: 'mint',
          senderAddress: 'ALGO_CENTRAL_BANK',
          centralBankOperation: true
        }
      };

      const transactionId = analytics.recordTransaction(transactionData);

      const stored = analytics.transactionData.get(transactionId);
      expect(stored.type).toBe('cbdc_mint');
      expect(stored.targetNetwork).toBe('ALGORAND');
      expect(stored.metadata.cbdcType).toBe('mint');
    });

    test('should generate transaction ID if not provided', () => {
      const transactionData = {
        type: 'payment',
        amount: 1000,
        targetNetwork: 'XRP'
      };

      const transactionId = analytics.recordTransaction(transactionData);

      expect(transactionId).toBeDefined();
      expect(analytics.transactionData.has(transactionId)).toBe(true);
    });

    test('should emit transaction recorded event', (done) => {
      analytics.on('transaction_recorded', (event) => {
        expect(event.transactionId).toBe('TXN-EVENT-001');
        expect(event.timestamp).toBeDefined();
        expect(event.metrics).toBeDefined();
        done();
      });

      analytics.recordTransaction({
        id: 'TXN-EVENT-001',
        type: 'payment',
        amount: 1000,
        targetNetwork: 'XRP'
      });
    });
  });

  describe('Transaction Status Updates', () => {
    beforeEach(() => {
      analytics.recordTransaction({
        id: 'TXN-UPDATE-001',
        type: 'payment',
        amount: 1000,
        targetNetwork: 'XRP',
        status: 'initiated'
      });
    });

    test('should update transaction status', () => {
      const statusUpdate = {
        status: 'confirmed',
        processingTime: 5000,
        complianceStatus: 'passed'
      };

      analytics.updateTransactionStatus('TXN-UPDATE-001', statusUpdate);

      const transaction = analytics.transactionData.get('TXN-UPDATE-001');
      expect(transaction.status).toBe('confirmed');
      expect(transaction.processingTime).toBe(5000);
      expect(transaction.complianceStatus).toBe('passed');
      expect(transaction.updatedAt).toBeDefined();
    });

    test('should emit status update event', (done) => {
      analytics.on('transaction_status_updated', (event) => {
        expect(event.transactionId).toBe('TXN-UPDATE-001');
        expect(event.previousStatus).toBe('initiated');
        expect(event.newStatus).toBe('confirmed');
        done();
      });

      analytics.updateTransactionStatus('TXN-UPDATE-001', {
        status: 'confirmed'
      });
    });

    test('should handle non-existent transaction update', () => {
      const consoleSpy = jest.spyOn(console, 'warn').mockImplementation();
      
      analytics.updateTransactionStatus('NON-EXISTENT', {
        status: 'confirmed'
      });

      // Should not throw error
      expect(analytics.transactionData.has('NON-EXISTENT')).toBe(false);
      
      consoleSpy.mockRestore();
    });
  });

  describe('Dashboard Data Generation', () => {
    beforeEach(() => {
      // Add sample transactions
      const sampleTransactions = [
        {
          id: 'TXN-DASH-001',
          type: 'swift_mt103',
          amount: 10000,
          currency: 'USD',
          targetNetwork: 'XRP',
          status: 'confirmed',
          processingTime: 2000,
          complianceStatus: 'passed',
          amlStatus: 'passed',
          sanctionsCheck: 'passed',
          riskScore: 15,
          sendingBIC: 'BANKUS33',
          receivingBIC: 'BANKDE88'
        },
        {
          id: 'TXN-DASH-002',
          type: 'payment',
          amount: 5000,
          currency: 'EUR',
          targetNetwork: 'CORDA',
          status: 'confirmed',
          processingTime: 3500,
          complianceStatus: 'passed',
          amlStatus: 'passed',
          sanctionsCheck: 'passed',
          riskScore: 30
        },
        {
          id: 'TXN-DASH-003',
          type: 'payment',
          amount: 1000,
          currency: 'USD',
          targetNetwork: 'ETHEREUM_L2',
          status: 'failed',
          processingTime: 1500,
          complianceStatus: 'failed',
          amlStatus: 'failed',
          riskScore: 85
        }
      ];

      sampleTransactions.forEach(txn => analytics.recordTransaction(txn));
    });

    test('should generate dashboard data for 24 hours', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);

      expect(dashboardData.timestamp).toBeDefined();
      expect(dashboardData.timeWindow).toBe(TIME_WINDOWS.LAST_24_HOURS);
      expect(dashboardData.transactionOverview).toBeDefined();
      expect(dashboardData.performanceMetrics).toBeDefined();
      expect(dashboardData.networkDistribution).toBeDefined();
      expect(dashboardData.complianceMetrics).toBeDefined();
    });

    test('should calculate transaction overview correctly', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const overview = dashboardData.transactionOverview;

      expect(overview.totalTransactions).toBe(3);
      expect(overview.successfulTransactions).toBe(2);
      expect(overview.failedTransactions).toBe(1);
      expect(overview.successRate).toBeCloseTo(2/3, 2);
      expect(overview.errorRate).toBeCloseTo(1/3, 2);
      expect(overview.totalVolume).toBe(16000);
      expect(overview.avgProcessingTime).toBeCloseTo(2333.33, 2);
    });

    test('should calculate network distribution', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const networks = dashboardData.networkDistribution;

      expect(networks.XRP).toBeDefined();
      expect(networks.XRP.count).toBe(1);
      expect(networks.XRP.volume).toBe(10000);
      expect(networks.XRP.successRate).toBe(1);

      expect(networks.CORDA).toBeDefined();
      expect(networks.CORDA.count).toBe(1);
      expect(networks.CORDA.volume).toBe(5000);

      expect(networks.ETHEREUM_L2).toBeDefined();
      expect(networks.ETHEREUM_L2.count).toBe(1);
      expect(networks.ETHEREUM_L2.errorCount).toBe(1);
    });

    test('should calculate compliance metrics', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const compliance = dashboardData.complianceMetrics;

      expect(compliance.amlChecks.passed).toBe(2);
      expect(compliance.amlChecks.failed).toBe(1);
      expect(compliance.sanctionsChecks.passed).toBe(2);
      expect(compliance.riskScoreDistribution.low).toBe(1); // riskScore < 30
      expect(compliance.riskScoreDistribution.medium).toBe(1); // 30 <= riskScore < 70
      expect(compliance.riskScoreDistribution.high).toBe(1); // riskScore >= 70
    });

    test('should calculate performance metrics', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const performance = dashboardData.performanceMetrics;

      expect(performance.endToEndLatency).toBeDefined();
      expect(performance.endToEndLatency.count).toBe(3);
      expect(performance.endToEndLatency.min).toBe(1500);
      expect(performance.endToEndLatency.max).toBe(3500);
      expect(performance.endToEndLatency.avg).toBeCloseTo(2333.33, 2);
    });
  });

  describe('CBDC Metrics', () => {
    beforeEach(() => {
      // Add CBDC transactions
      const cbdcTransactions = [
        {
          id: 'CBDC-MINT-001',
          type: 'cbdc_mint',
          amount: 1000000,
          targetNetwork: 'ALGORAND',
          status: 'confirmed',
          metadata: {
            cbdcType: 'mint',
            senderAddress: 'CENTRAL_BANK_ADDR'
          }
        },
        {
          id: 'CBDC-TRANSFER-001',
          type: 'cbdc_transfer',
          amount: 50000,
          targetNetwork: 'ALGORAND',
          status: 'confirmed',
          metadata: {
            cbdcType: 'transfer',
            senderAddress: 'USER_ADDR_1',
            receiverAddress: 'USER_ADDR_2'
          }
        },
        {
          id: 'CBDC-BURN-001',
          type: 'cbdc_burn',
          amount: 100000,
          targetNetwork: 'ALGORAND',
          status: 'confirmed',
          metadata: {
            cbdcType: 'burn',
            senderAddress: 'CENTRAL_BANK_ADDR'
          }
        }
      ];

      cbdcTransactions.forEach(txn => analytics.recordTransaction(txn));
    });

    test('should calculate CBDC metrics', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const cbdc = dashboardData.cbdcMetrics;

      expect(cbdc.totalTransactions).toBe(3);
      expect(cbdc.mintOperations).toBe(1);
      expect(cbdc.burnOperations).toBe(1);
      expect(cbdc.transferOperations).toBe(1);
      expect(cbdc.totalVolume).toBe(1150000);
      expect(cbdc.activeAccountsCount).toBe(3);
      expect(cbdc.avgTransactionSize).toBeCloseTo(383333.33, 2);
    });

    test('should track active CBDC accounts', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const cbdc = dashboardData.cbdcMetrics;

      expect(cbdc.activeAccounts.has('CENTRAL_BANK_ADDR')).toBe(true);
      expect(cbdc.activeAccounts.has('USER_ADDR_1')).toBe(true);
      expect(cbdc.activeAccounts.has('USER_ADDR_2')).toBe(true);
    });
  });

  describe('Real-time Metrics', () => {
    test('should provide real-time metrics', () => {
      // Add some transactions
      analytics.recordTransaction({
        id: 'RT-001',
        amount: 1000,
        targetNetwork: 'XRP',
        status: 'confirmed',
        processingTime: 2000
      });

      analytics.recordTransaction({
        id: 'RT-002',
        amount: 2000,
        targetNetwork: 'CORDA',
        status: 'failed',
        processingTime: 3000
      });

      const realTimeMetrics = analytics.getRealTimeMetrics();

      expect(realTimeMetrics.totalTransactions).toBe(2);
      expect(realTimeMetrics.errorCount).toBe(1);
      expect(realTimeMetrics.errorRate).toBe(0.5);
      expect(realTimeMetrics.avgProcessingTime).toBe(2500);
      expect(realTimeMetrics.networkMetrics.XRP.transactions).toBe(1);
      expect(realTimeMetrics.networkMetrics.CORDA.transactions).toBe(1);
    });

    test('should limit processing times array size', () => {
      // Add more than 1000 transactions to test array limiting
      for (let i = 0; i < 1100; i++) {
        analytics.recordTransaction({
          id: `LIMIT-${i}`,
          amount: 1000,
          targetNetwork: 'XRP',
          processingTime: 1000 + i
        });
      }

      expect(analytics.realTimeMetrics.processingTimes.length).toBe(1000);
    });
  });

  describe('Error Analysis', () => {
    beforeEach(() => {
      // Add transactions with errors
      const transactions = [
        {
          id: 'ERR-001',
          type: 'payment',
          amount: 1000,
          targetNetwork: 'XRP',
          status: 'failed',
          metadata: { errorReason: 'Network timeout' }
        },
        {
          id: 'ERR-002',
          type: 'payment',
          amount: 2000,
          targetNetwork: 'XRP',
          status: 'failed',
          metadata: { errorReason: 'Insufficient funds' }
        },
        {
          id: 'ERR-003',
          type: 'swift_mt103',
          amount: 5000,
          targetNetwork: 'CORDA',
          status: 'failed',
          metadata: { errorReason: 'Network timeout' }
        },
        {
          id: 'SUCCESS-001',
          type: 'payment',
          amount: 3000,
          targetNetwork: 'XRP',
          status: 'confirmed'
        }
      ];

      transactions.forEach(txn => analytics.recordTransaction(txn));
    });

    test('should analyze errors by network', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const errorAnalysis = dashboardData.errorAnalysis;

      expect(errorAnalysis.totalErrors).toBe(3);
      expect(errorAnalysis.errorRate).toBe(0.75);
      expect(errorAnalysis.errorsByNetwork.XRP).toBe(2);
      expect(errorAnalysis.errorsByNetwork.CORDA).toBe(1);
    });

    test('should analyze errors by type', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const errorAnalysis = dashboardData.errorAnalysis;

      expect(errorAnalysis.errorsByType.payment).toBe(2);
      expect(errorAnalysis.errorsByType.swift_mt103).toBe(1);
    });

    test('should analyze errors by reason', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const errorAnalysis = dashboardData.errorAnalysis;

      expect(errorAnalysis.errorsByReason['Network timeout']).toBe(2);
      expect(errorAnalysis.errorsByReason['Insufficient funds']).toBe(1);
    });

    test('should provide recent errors', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const errorAnalysis = dashboardData.errorAnalysis;

      expect(errorAnalysis.recentErrors).toHaveLength(3);
      expect(errorAnalysis.recentErrors[0].id).toBe('ERR-001');
    });
  });

  describe('Geographic Distribution', () => {
    beforeEach(() => {
      // Add transactions with different BIC codes
      const transactions = [
        {
          id: 'GEO-001',
          amount: 10000,
          sendingBIC: 'BANKUS33XXX',
          receivingBIC: 'BANKDE88XXX'
        },
        {
          id: 'GEO-002',
          amount: 5000,
          sendingBIC: 'BANKGB22XXX',
          receivingBIC: 'BANKFR41XXX'
        },
        {
          id: 'GEO-003',
          amount: 3000,
          sendingBIC: 'BANKUS33XXX',
          receivingBIC: 'BANKJP62XXX'
        }
      ];

      transactions.forEach(txn => analytics.recordTransaction(txn));
    });

    test('should extract country codes from BIC', () => {
      expect(analytics.getBICCountry('BANKUS33XXX')).toBe('US');
      expect(analytics.getBICCountry('BANKDE88XXX')).toBe('DE');
      expect(analytics.getBICCountry('BANKGB22XXX')).toBe('GB');
      expect(analytics.getBICCountry('BANKFR41XXX')).toBe('FR');
      expect(analytics.getBICCountry('BANKJP62XXX')).toBe('JP');
    });

    test('should calculate geographic distribution', () => {
      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const geo = dashboardData.geographicDistribution;

      expect(geo.US.outgoing).toBe(2);
      expect(geo.US.volume).toBe(13000);
      expect(geo.DE.incoming).toBe(1);
      expect(geo.GB.outgoing).toBe(1);
      expect(geo.FR.incoming).toBe(1);
      expect(geo.JP.incoming).toBe(1);
    });

    test('should handle invalid BIC codes', () => {
      expect(analytics.getBICCountry('')).toBeNull();
      expect(analytics.getBICCountry('BANK')).toBeNull();
      expect(analytics.getBICCountry(null)).toBeNull();
    });
  });

  describe('Alert Thresholds', () => {
    test('should detect high error rate alert', () => {
      // Add transactions to exceed error rate threshold
      for (let i = 0; i < 8; i++) {
        analytics.recordTransaction({
          id: `FAIL-${i}`,
          amount: 1000,
          targetNetwork: 'XRP',
          status: 'failed'
        });
      }

      for (let i = 0; i < 2; i++) {
        analytics.recordTransaction({
          id: `SUCCESS-${i}`,
          amount: 1000,
          targetNetwork: 'XRP',
          status: 'confirmed'
        });
      }

      const alerts = analytics.checkAlertThresholds();
      const errorRateAlert = alerts.find(alert => alert.type === 'error_rate');

      expect(errorRateAlert).toBeDefined();
      expect(errorRateAlert.current).toBe(0.8);
      expect(errorRateAlert.threshold).toBe(0.1);
      expect(errorRateAlert.severity).toBe('high');
    });

    test('should detect high processing time alert', () => {
      analytics.recordTransaction({
        id: 'SLOW-001',
        amount: 1000,
        targetNetwork: 'XRP',
        processingTime: 15000 // Exceeds 10000ms threshold
      });

      const alerts = analytics.checkAlertThresholds();
      const processingTimeAlert = alerts.find(alert => alert.type === 'processing_time');

      expect(processingTimeAlert).toBeDefined();
      expect(processingTimeAlert.current).toBe(15000);
      expect(processingTimeAlert.threshold).toBe(10000);
      expect(processingTimeAlert.severity).toBe('medium');
    });

    test('should detect low compliance rate alert', () => {
      // Add transactions with low compliance rate
      for (let i = 0; i < 6; i++) {
        analytics.recordTransaction({
          id: `COMPLIANCE-FAIL-${i}`,
          amount: 1000,
          complianceStatus: 'failed'
        });
      }

      for (let i = 0; i < 4; i++) {
        analytics.recordTransaction({
          id: `COMPLIANCE-PASS-${i}`,
          amount: 1000,
          complianceStatus: 'passed'
        });
      }

      const alerts = analytics.checkAlertThresholds();
      const complianceAlert = alerts.find(alert => alert.type === 'compliance_rate');

      expect(complianceAlert).toBeDefined();
      expect(complianceAlert.current).toBe(0.4);
      expect(complianceAlert.threshold).toBe(0.8);
      expect(complianceAlert.severity).toBe('high');
    });
  });

  describe('System Health', () => {
    test('should provide system health metrics', () => {
      const health = analytics.getSystemHealth();

      expect(health.timestamp).toBeDefined();
      expect(health.dataPoints).toBe(0);
      expect(health.aggregatedMetrics).toBe(0);
      expect(health.memoryUsage).toBeDefined();
      expect(health.uptime).toBeDefined();
      expect(health.isHealthy).toBe(true);
      expect(Array.isArray(health.alertStatus)).toBe(true);
    });

    test('should indicate unhealthy when too many data points', () => {
      // Override maxDataPoints for testing
      analytics.config.maxDataPoints = 5;

      // Add more transactions than limit
      for (let i = 0; i < 12; i++) {
        analytics.recordTransaction({
          id: `OVERLOAD-${i}`,
          amount: 1000,
          targetNetwork: 'XRP'
        });
      }

      const health = analytics.getSystemHealth();
      expect(health.isHealthy).toBe(false);
    });
  });

  describe('Data Cleanup', () => {
    test('should clean up old data', () => {
      // Override retention period for testing
      analytics.config.retentionPeriod = 1000; // 1 second

      // Add transaction
      analytics.recordTransaction({
        id: 'OLD-001',
        amount: 1000,
        targetNetwork: 'XRP'
      });

      expect(analytics.transactionData.size).toBe(1);

      // Wait and cleanup
      setTimeout(() => {
        analytics.cleanupOldData();
        expect(analytics.transactionData.size).toBe(0);
      }, 1100);
    });

    test('should not clean up recent data', () => {
      analytics.recordTransaction({
        id: 'RECENT-001',
        amount: 1000,
        targetNetwork: 'XRP'
      });

      expect(analytics.transactionData.size).toBe(1);

      analytics.cleanupOldData();
      expect(analytics.transactionData.size).toBe(1);
    });
  });

  describe('Volume Trends', () => {
    test('should calculate hourly volume trends', () => {
      const now = Date.now();
      const oneHourAgo = now - (60 * 60 * 1000);
      const twoHoursAgo = now - (2 * 60 * 60 * 1000);

      // Manually set transaction timestamps
      analytics.recordTransaction({
        id: 'TREND-001',
        amount: 1000,
        targetNetwork: 'XRP'
      });
      
      // Modify timestamp to simulate different hours
      const transaction1 = analytics.transactionData.get('TREND-001');
      transaction1.timestamp = twoHoursAgo;

      analytics.recordTransaction({
        id: 'TREND-002',
        amount: 2000,
        targetNetwork: 'XRP'
      });
      
      const transaction2 = analytics.transactionData.get('TREND-002');
      transaction2.timestamp = oneHourAgo;

      analytics.recordTransaction({
        id: 'TREND-003',
        amount: 3000,
        targetNetwork: 'XRP'
      });

      const dashboardData = analytics.getDashboardData(TIME_WINDOWS.LAST_24_HOURS);
      const trends = dashboardData.volumeTrends;

      expect(trends).toHaveLength(3);
      expect(trends[0].count).toBe(1);
      expect(trends[0].volume).toBe(1000);
      expect(trends[1].count).toBe(1);
      expect(trends[1].volume).toBe(2000);
      expect(trends[2].count).toBe(1);
      expect(trends[2].volume).toBe(3000);
    });
  });

  describe('Constants and Exports', () => {
    test('should export analytics metrics constants', () => {
      expect(ANALYTICS_METRICS.TRANSACTION_COUNT).toBe('transaction_count');
      expect(ANALYTICS_METRICS.PROCESSING_TIME).toBe('processing_time');
      expect(ANALYTICS_METRICS.SUCCESS_RATE).toBe('success_rate');
      expect(ANALYTICS_METRICS.CBDC_VOLUME).toBe('cbdc_volume');
    });

    test('should export time windows constants', () => {
      expect(TIME_WINDOWS.REAL_TIME).toBe('real_time');
      expect(TIME_WINDOWS.LAST_HOUR).toBe('last_hour');
      expect(TIME_WINDOWS.LAST_24_HOURS).toBe('last_24_hours');
      expect(TIME_WINDOWS.LAST_7_DAYS).toBe('last_7_days');
      expect(TIME_WINDOWS.LAST_30_DAYS).toBe('last_30_days');
    });

    test('should export aggregation types constants', () => {
      expect(AGGREGATION_TYPES.SUM).toBe('sum');
      expect(AGGREGATION_TYPES.AVERAGE).toBe('average');
      expect(AGGREGATION_TYPES.COUNT).toBe('count');
      expect(AGGREGATION_TYPES.MIN).toBe('min');
      expect(AGGREGATION_TYPES.MAX).toBe('max');
      expect(AGGREGATION_TYPES.PERCENTILE).toBe('percentile');
    });
  });

  describe('Performance Metrics Calculation', () => {
    test('should calculate performance statistics correctly', () => {
      const transactions = [
        { processingTime: 1000 },
        { processingTime: 2000 },
        { processingTime: 3000 },
        { processingTime: 4000 },
        { processingTime: 5000 }
      ];

      const stats = analytics.calculatePerformanceStats(transactions, 'processingTime');

      expect(stats.count).toBe(5);
      expect(stats.min).toBe(1000);
      expect(stats.max).toBe(5000);
      expect(stats.avg).toBe(3000);
      expect(stats.median).toBe(3000);
      expect(stats.p95).toBe(5000);
      expect(stats.p99).toBe(5000);
    });

    test('should handle empty performance data', () => {
      const transactions = [];
      const stats = analytics.calculatePerformanceStats(transactions, 'processingTime');

      expect(stats.count).toBe(0);
      expect(stats.min).toBe(0);
      expect(stats.max).toBe(0);
      expect(stats.avg).toBe(0);
    });

    test('should calculate latency percentiles', () => {
      const transactions = Array.from({ length: 100 }, (_, i) => ({
        processingTime: (i + 1) * 100 // 100ms to 10000ms
      }));

      const percentiles = analytics.calculateLatencyPercentiles(transactions);

      expect(percentiles.p50).toBe(5100);
      expect(percentiles.p75).toBe(7600);
      expect(percentiles.p90).toBe(9100);
      expect(percentiles.p95).toBe(9600);
      expect(percentiles.p99).toBe(10000);
    });
  });
});