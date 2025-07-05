const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const { 
  QuotaManager,
  SUBSCRIPTION_TIERS,
  VIOLATION_SEVERITY,
  NOTIFICATION_CHANNELS 
} = require('../../src/billing/quota-manager');
const { AUDIT_EVENT_TYPES } = require('../../src/audit/transpiler-audit');

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
    query: jest.fn()
  }))
}));

jest.mock('../../src/utils/cache', () => ({
  CacheManager: jest.fn().mockImplementation(() => ({
    get: jest.fn(),
    set: jest.fn(),
    delete: jest.fn(),
    clear: jest.fn()
  }))
}));

jest.mock('../../src/audit/transpiler-audit', () => ({
  transpilerAudit: {
    logEvent: jest.fn().mockResolvedValue(true)
  },
  AUDIT_EVENT_TYPES: {
    QUOTA_CHECK: 'quota.check',
    QUOTA_USAGE: 'quota.usage',
    QUOTA_CHANGE: 'quota.change',
    QUOTA_RESET: 'quota.reset',
    QUOTA_ALERT: 'quota.alert'
  }
}));

describe('Quota Manager', () => {
  let quotaManager;
  let mockDb;
  let mockCache;

  beforeAll(() => {
    // Mock timers for testing intervals
    jest.useFakeTimers();
  });

  afterAll(() => {
    jest.useRealTimers();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    quotaManager = new QuotaManager();
    mockDb = quotaManager.db;
    mockCache = quotaManager.cache;
    
    // Reset quota manager state
    quotaManager.quotaCache.clear();
    quotaManager.usageCounters.clear();
    quotaManager.alertRules.clear();
    quotaManager.lastQuotaCheck.clear();
    quotaManager.isInitialized = false;
  });

  describe('Initialization', () => {
    test('should initialize quota manager successfully', async () => {
      mockDb.connect.mockResolvedValue(true);
      mockDb.query.mockResolvedValue([]);

      await quotaManager.initialize();

      expect(mockDb.connect).toHaveBeenCalled();
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS customer_quotas'));
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS quota_usage_history'));
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS quota_alerts'));
      expect(quotaManager.isInitialized).toBe(true);
    });

    test('should handle initialization errors gracefully', async () => {
      const error = new Error('Database connection failed');
      mockDb.connect.mockRejectedValue(error);

      await expect(quotaManager.initialize()).rejects.toThrow('Database connection failed');
    });

    test('should create quota management tables', async () => {
      await quotaManager.createQuotaTables();

      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS customer_quotas'));
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS quota_usage_history'));
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS quota_alerts'));
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE INDEX IF NOT EXISTS'));
    });

    test('should load quota cache from database', async () => {
      const mockQuotaData = [
        {
          customer_id: 'customer-1',
          subscription_tier: 'professional',
          monthly_quota: 250,
          current_usage: 150,
          overage_usage: 0,
          quota_period_start: '2025-07-01',
          quota_period_end: '2025-07-31',
          base_price: 999.00,
          overage_rate: 6.00,
          max_overage_percent: 100,
          status: 'active',
          alert_thresholds: [80, 90, 95, 100]
        }
      ];

      mockDb.query.mockResolvedValue(mockQuotaData);

      await quotaManager.loadQuotaCache();

      expect(quotaManager.quotaCache.size).toBe(1);
      expect(quotaManager.quotaCache.get('quota:customer-1')).toEqual(
        expect.objectContaining({
          customerId: 'customer-1',
          tier: 'professional',
          monthlyQuota: 250,
          currentUsage: 150
        })
      );
      expect(quotaManager.usageCounters.get('customer-1')).toBe(150);
    });

    test('should setup default alert rules', async () => {
      await quotaManager.setupAlertRules();

      expect(quotaManager.alertRules.size).toBe(3);
      expect(quotaManager.alertRules.get('quota_warning')).toEqual(
        expect.objectContaining({
          type: 'quota_warning',
          thresholds: [75, 85, 90, 95],
          severity: VIOLATION_SEVERITY.WARNING
        })
      );
    });
  });

  describe('Subscription Tiers', () => {
    test('should have correct subscription tier configurations', () => {
      expect(SUBSCRIPTION_TIERS.STARTER).toEqual({
        name: 'starter',
        monthlyQuota: 50,
        basePrice: 299,
        overageRate: 8.00,
        features: ['basic_templates', 'email_support', 'standard_sla'],
        maxOveragePercent: 50,
        alertThresholds: [75, 90, 100]
      });

      expect(SUBSCRIPTION_TIERS.PROFESSIONAL).toEqual({
        name: 'professional',
        monthlyQuota: 250,
        basePrice: 999,
        overageRate: 6.00,
        features: ['premium_templates', 'priority_support', 'enhanced_sla', 'analytics'],
        maxOveragePercent: 100,
        alertThresholds: [80, 90, 95, 100]
      });

      expect(SUBSCRIPTION_TIERS.ENTERPRISE).toEqual({
        name: 'enterprise',
        monthlyQuota: 1000,
        basePrice: 2999,
        overageRate: 4.00,
        features: ['custom_templates', '24_7_support', 'premium_sla', 'advanced_analytics', 'dedicated_manager'],
        maxOveragePercent: 200,
        alertThresholds: [85, 95, 100, 110, 125]
      });

      expect(SUBSCRIPTION_TIERS.CUSTOM).toEqual({
        name: 'custom',
        monthlyQuota: -1,
        basePrice: 0,
        overageRate: 0,
        features: ['unlimited', 'white_glove_support', 'custom_sla', 'on_premise'],
        maxOveragePercent: 0,
        alertThresholds: []
      });
    });
  });

  describe('Quota Status Evaluation', () => {
    test('should allow unlimited quota for custom tier', () => {
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: -1,
        currentUsage: 5000,
        overageUsage: 0,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000) // Tomorrow
      };

      const result = quotaManager.evaluateQuotaStatus(quota);

      expect(result.allowed).toBe(true);
      expect(result.reason).toBe('unlimited_quota');
      expect(result.remaining).toBe(-1);
    });

    test('should allow usage within base quota', () => {
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      const result = quotaManager.evaluateQuotaStatus(quota);

      expect(result.allowed).toBe(true);
      expect(result.reason).toBe('within_quota');
      expect(result.remaining).toBe(100);
      expect(result.overageAllowed).toBe(250);
    });

    test('should allow overage usage within limits', () => {
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 250,
        overageUsage: 50,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      const result = quotaManager.evaluateQuotaStatus(quota);

      expect(result.allowed).toBe(true);
      expect(result.reason).toBe('overage_allowed');
      expect(result.remaining).toBe(0);
      expect(result.overageUsed).toBe(50);
      expect(result.overageRemaining).toBe(200);
    });

    test('should block when maximum overage is exceeded', () => {
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 250,
        overageUsage: 250,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      const result = quotaManager.evaluateQuotaStatus(quota);

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('max_overage_exceeded');
      expect(result.currentUsage).toBe(500);
      expect(result.maxAllowed).toBe(500);
    });

    test('should block when quota is inactive', () => {
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 100,
        overageUsage: 0,
        status: 'inactive',
        periodEnd: new Date(Date.now() + 86400000)
      };

      const result = quotaManager.evaluateQuotaStatus(quota);

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('quota_inactive');
    });

    test('should block when quota period has expired', () => {
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 100,
        overageUsage: 0,
        status: 'active',
        periodEnd: new Date(Date.now() - 86400000) // Yesterday
      };

      const result = quotaManager.evaluateQuotaStatus(quota);

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('quota_period_expired');
    });
  });

  describe('Quota Operations', () => {
    beforeEach(() => {
      quotaManager.isInitialized = true;
    });

    test('should check if customer can perform transpilation', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);

      const result = await quotaManager.canPerformTranspilation('customer-1', 'req-123');

      expect(result.allowed).toBe(true);
      expect(result.reason).toBe('within_quota');
    });

    test('should implement rate limiting for quota checks', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      quotaManager.lastQuotaCheck.set('customer-1', Date.now());

      const result = await quotaManager.canPerformTranspilation('customer-1');

      expect(result.allowed).toBe(true);
      // Should use cached result without database query
      expect(mockDb.query).not.toHaveBeenCalled();
    });

    test('should record usage correctly for base quota', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const result = await quotaManager.recordUsage('customer-1', 'req-123', { type: 'transpilation' });

      expect(result.success).toBe(true);
      expect(result.currentUsage).toBe(151);
      expect(result.overageUsage).toBe(0);
      expect(result.totalUsage).toBe(151);

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('UPDATE customer_quotas'),
        [151, 0, 'customer-1']
      );
    });

    test('should record usage correctly for overage', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 250,
        overageUsage: 25,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const result = await quotaManager.recordUsage('customer-1', 'req-123');

      expect(result.success).toBe(true);
      expect(result.currentUsage).toBe(250);
      expect(result.overageUsage).toBe(26);
      expect(result.totalUsage).toBe(276);
    });

    test('should set customer quota correctly', async () => {
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const result = await quotaManager.setCustomerQuota('customer-1', 'professional', 500);

      expect(result.success).toBe(true);
      expect(result.tier).toBe('professional');
      expect(result.monthlyQuota).toBe(500);
      expect(result.basePrice).toBe(999);
      expect(result.overageRate).toBe(6.00);

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('INSERT INTO customer_quotas'),
        expect.arrayContaining(['customer-1', 'professional', 500])
      );
    });

    test('should reset monthly quota', async () => {
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const result = await quotaManager.resetMonthlyQuota('customer-1');

      expect(result.success).toBe(true);
      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('UPDATE customer_quotas'),
        ['customer-1']
      );
    });

    test('should handle quota retrieval with cache refresh', async () => {
      const mockQuotaData = {
        customer_id: 'customer-1',
        subscription_tier: 'professional',
        monthly_quota: 250,
        current_usage: 150,
        overage_usage: 0,
        quota_period_start: '2025-07-01',
        quota_period_end: '2025-07-31',
        base_price: 999.00,
        overage_rate: 6.00,
        max_overage_percent: 100,
        status: 'active',
        alert_thresholds: [80, 90, 95, 100]
      };

      mockDb.query.mockResolvedValue([mockQuotaData]);

      const quota = await quotaManager.getCustomerQuota('customer-1');

      expect(quota).toEqual(expect.objectContaining({
        customerId: 'customer-1',
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 150
      }));
    });
  });

  describe('Alert Management', () => {
    beforeEach(() => {
      quotaManager.isInitialized = true;
    });

    test('should trigger quota threshold alerts', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        alertThresholds: [75, 90, 95, 100]
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      mockDb.query.mockResolvedValue([]); // No existing alerts

      const sendAlertSpy = jest.spyOn(quotaManager, 'sendQuotaAlert').mockResolvedValue();

      await quotaManager.checkAlertTriggers('customer-1', 240, 0, 250);

      expect(sendAlertSpy).toHaveBeenCalledWith('customer-1', expect.objectContaining({
        type: 'quota_threshold',
        threshold: 95,
        currentUsage: 240,
        quotaLimit: 250,
        usagePercent: '96.0'
      }));

      sendAlertSpy.mockRestore();
    });

    test('should trigger overage alerts', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        alertThresholds: [75, 90, 95, 100]
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      mockDb.query.mockResolvedValue([]); // No existing alerts

      const sendAlertSpy = jest.spyOn(quotaManager, 'sendQuotaAlert').mockResolvedValue();

      await quotaManager.checkAlertTriggers('customer-1', 250, 75, 250);

      expect(sendAlertSpy).toHaveBeenCalledWith('customer-1', expect.objectContaining({
        type: 'overage_warning',
        overageAmount: 75,
        overagePercent: '30.0',
        quotaLimit: 250
      }));

      sendAlertSpy.mockRestore();
    });

    test('should send quota alert with correct severity', async () => {
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const alertData = {
        type: 'quota_threshold',
        threshold: 95,
        currentUsage: 240,
        quotaLimit: 250,
        usagePercent: '96.0'
      };

      await quotaManager.sendQuotaAlert('customer-1', alertData);

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('INSERT INTO quota_alerts'),
        expect.arrayContaining(['customer-1', 'quota_threshold'])
      );
    });

    test('should calculate alert severity correctly', () => {
      // Test overage warning
      let severity = quotaManager.calculateAlertSeverity({ type: 'overage_warning' });
      expect(severity).toBe(VIOLATION_SEVERITY.CRITICAL);

      // Test high threshold
      severity = quotaManager.calculateAlertSeverity({ type: 'quota_threshold', threshold: 100 });
      expect(severity).toBe(VIOLATION_SEVERITY.CRITICAL);

      // Test medium threshold
      severity = quotaManager.calculateAlertSeverity({ type: 'quota_threshold', threshold: 90 });
      expect(severity).toBe(VIOLATION_SEVERITY.WARNING);

      // Test low threshold
      severity = quotaManager.calculateAlertSeverity({ type: 'quota_threshold', threshold: 75 });
      expect(severity).toBe(VIOLATION_SEVERITY.WARNING);
    });

    test('should generate appropriate alert messages', () => {
      // Test quota threshold message
      let message = quotaManager.generateAlertMessage({
        type: 'quota_threshold',
        usagePercent: '96.0',
        currentUsage: 240,
        quotaLimit: 250
      });
      expect(message).toBe('Quota usage at 96.0% (240/250 transpilations used)');

      // Test overage warning message
      message = quotaManager.generateAlertMessage({
        type: 'overage_warning',
        overageAmount: 75,
        overagePercent: '30.0'
      });
      expect(message).toBe('Overage usage detected: 75 transpilations (30.0% of base quota)');

      // Test quota exceeded message
      message = quotaManager.generateAlertMessage({
        type: 'quota_exceeded'
      });
      expect(message).toBe('Monthly quota exceeded. Additional usage will incur overage charges.');
    });

    test('should get correct notification channels by severity', () => {
      // Test critical severity
      let channels = quotaManager.getAlertChannels(VIOLATION_SEVERITY.CRITICAL);
      expect(channels).toContain(NOTIFICATION_CHANNELS.EMAIL);
      expect(channels).toContain(NOTIFICATION_CHANNELS.WEBHOOK);
      expect(channels).toContain(NOTIFICATION_CHANNELS.IN_APP);

      // Test blocked severity
      channels = quotaManager.getAlertChannels(VIOLATION_SEVERITY.BLOCKED);
      expect(channels).toContain(NOTIFICATION_CHANNELS.EMAIL);
      expect(channels).toContain(NOTIFICATION_CHANNELS.SMS);
      expect(channels).toContain(NOTIFICATION_CHANNELS.WEBHOOK);

      // Test warning severity
      channels = quotaManager.getAlertChannels(VIOLATION_SEVERITY.WARNING);
      expect(channels).toContain(NOTIFICATION_CHANNELS.EMAIL);
      expect(channels).toContain(NOTIFICATION_CHANNELS.IN_APP);
    });
  });

  describe('Usage Analytics', () => {
    test('should generate usage analytics report', async () => {
      const mockUsageData = [
        {
          usage_date: '2025-07-01',
          daily_usage: 45,
          cumulative_usage: 200,
          quota_remaining: 50,
          overage_amount: 0,
          cost_incurred: 999.00
        },
        {
          usage_date: '2025-07-02',
          daily_usage: 35,
          cumulative_usage: 235,
          quota_remaining: 15,
          overage_amount: 0,
          cost_incurred: 999.00
        }
      ];

      mockDb.query.mockResolvedValue(mockUsageData);

      const analytics = await quotaManager.getUsageAnalytics('customer-1', '2025-07-01', '2025-07-31');

      expect(analytics.summary).toEqual({
        totalUsage: 80,
        averageDailyUsage: 40,
        totalOverage: 0,
        totalCost: 1998.00,
        peakDailyUsage: 45,
        daysWithOverage: 0
      });

      expect(analytics.dailyData).toEqual(mockUsageData);
      expect(analytics.period).toEqual({
        startDate: '2025-07-01',
        endDate: '2025-07-31'
      });
    });

    test('should generate daily usage report', async () => {
      const mockReportData = [
        {
          customer_id: 'customer-1',
          subscription_tier: 'professional',
          monthly_quota: 250,
          current_usage: 150,
          overage_usage: 0,
          daily_usage: 25,
          daily_cost: 0
        },
        {
          customer_id: 'customer-2',
          subscription_tier: 'enterprise',
          monthly_quota: 1000,
          current_usage: 800,
          overage_usage: 0,
          daily_usage: 50,
          daily_cost: 0
        }
      ];

      mockDb.query.mockResolvedValue(mockReportData);

      const report = await quotaManager.generateDailyUsageReport();

      expect(report.totalCustomers).toBe(2);
      expect(report.activeCustomers).toBe(2);
      expect(report.totalDailyUsage).toBe(75);
      expect(report.totalDailyCost).toBe(0);
      expect(report.customerDetails).toHaveLength(2);
      expect(report.tierBreakdown).toHaveProperty('professional');
      expect(report.tierBreakdown).toHaveProperty('enterprise');
    });

    test('should calculate tier breakdown correctly', () => {
      const mockCustomers = [
        { subscription_tier: 'professional', daily_usage: 25, daily_cost: 0 },
        { subscription_tier: 'professional', daily_usage: 30, daily_cost: 0 },
        { subscription_tier: 'enterprise', daily_usage: 50, daily_cost: 0 }
      ];

      const breakdown = quotaManager.calculateTierBreakdown(mockCustomers);

      expect(breakdown.professional).toEqual({
        count: 2,
        totalUsage: 55,
        totalCost: 0,
        averageUsage: 27.5
      });

      expect(breakdown.enterprise).toEqual({
        count: 1,
        totalUsage: 50,
        totalCost: 0,
        averageUsage: 50
      });
    });
  });

  describe('Quota Resets and Maintenance', () => {
    test('should check for quota resets', async () => {
      const mockExpiredQuotas = [
        { customer_id: 'customer-1', quota_period_end: '2025-06-30' },
        { customer_id: 'customer-2', quota_period_end: '2025-06-30' }
      ];

      mockDb.query.mockResolvedValue(mockExpiredQuotas);
      const resetSpy = jest.spyOn(quotaManager, 'resetMonthlyQuota').mockResolvedValue({ success: true });

      await quotaManager.checkQuotaResets();

      expect(resetSpy).toHaveBeenCalledWith('customer-1');
      expect(resetSpy).toHaveBeenCalledWith('customer-2');
      expect(resetSpy).toHaveBeenCalledTimes(2);

      resetSpy.mockRestore();
    });

    test('should sync usage counters to database', async () => {
      quotaManager.usageCounters.set('customer-1', 275);
      quotaManager.usageCounters.set('customer-2', 150);

      quotaManager.quotaCache.set('quota:customer-1', { monthlyQuota: 250 });
      quotaManager.quotaCache.set('quota:customer-2', { monthlyQuota: 250 });

      mockDb.query.mockResolvedValue({ rowCount: 2 });

      await quotaManager.syncUsageCounters();

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('UPDATE customer_quotas'),
        expect.arrayContaining(['customer-1', 250, 25])
      );
    });

    test('should record daily usage history', async () => {
      const mockQuota = {
        monthlyQuota: 250,
        currentUsage: 200,
        basePrice: 999.00,
        overageRate: 6.00
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      await quotaManager.recordDailyUsage('customer-1', 225);

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('INSERT INTO quota_usage_history'),
        expect.arrayContaining(['customer-1', expect.any(String), 225])
      );
    });
  });

  describe('Event Handling', () => {
    test('should emit quota alert events', async () => {
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const eventSpy = jest.fn();
      quotaManager.on('quota-alert', eventSpy);

      const alertData = {
        type: 'quota_threshold',
        threshold: 95,
        currentUsage: 240,
        quotaLimit: 250
      };

      await quotaManager.sendQuotaAlert('customer-1', alertData);

      expect(eventSpy).toHaveBeenCalledWith(expect.objectContaining({
        customerId: 'customer-1',
        severity: VIOLATION_SEVERITY.WARNING,
        alertData
      }));
    });

    test('should emit daily report events', async () => {
      mockDb.query.mockResolvedValue([]);

      const eventSpy = jest.fn();
      quotaManager.on('daily-report', eventSpy);

      const report = await quotaManager.generateDailyUsageReport();

      expect(eventSpy).toHaveBeenCalledWith(expect.objectContaining({
        totalCustomers: 0,
        activeCustomers: 0,
        totalDailyUsage: 0
      }));
    });
  });

  describe('Error Handling', () => {
    test('should handle quota check failures gracefully', async () => {
      const error = new Error('Database error');
      mockDb.query.mockRejectedValue(error);

      const result = await quotaManager.canPerformTranspilation('customer-1');

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('quota_check_failed');
      expect(result.details).toBe('Database error');
    });

    test('should handle usage recording failures', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);

      const error = new Error('Database update failed');
      mockDb.query.mockRejectedValue(error);

      await expect(quotaManager.recordUsage('customer-1')).rejects.toThrow('Database update failed');
    });

    test('should handle missing quota gracefully', async () => {
      const result = await quotaManager.canPerformTranspilation('nonexistent-customer');

      expect(result.allowed).toBe(false);
      expect(result.reason).toBe('quota_check_failed');
      expect(result.details).toContain('No quota found');
    });
  });

  describe('Audit Integration', () => {
    test('should log quota check events', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);

      const { transpilerAudit } = require('../../src/audit/transpiler-audit');

      await quotaManager.canPerformTranspilation('customer-1', 'req-123');

      expect(transpilerAudit.logEvent).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.QUOTA_CHECK,
        expect.objectContaining({
          customerId: 'customer-1',
          currentUsage: 150,
          monthlyQuota: 250,
          allowed: true
        }),
        expect.objectContaining({
          customerId: 'customer-1',
          requestId: 'req-123',
          action: 'quota_check'
        })
      );
    });

    test('should log quota usage events', async () => {
      const mockQuota = {
        customerId: 'customer-1',
        monthlyQuota: 250,
        currentUsage: 150,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', mockQuota);
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const { transpilerAudit } = require('../../src/audit/transpiler-audit');

      await quotaManager.recordUsage('customer-1', 'req-123', { type: 'transpilation' });

      expect(transpilerAudit.logEvent).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.QUOTA_USAGE,
        expect.objectContaining({
          customerId: 'customer-1',
          usageType: 'base',
          newUsage: 151,
          totalUsage: 151,
          monthlyQuota: 250
        }),
        expect.objectContaining({
          customerId: 'customer-1',
          requestId: 'req-123',
          action: 'record_usage'
        })
      );
    });
  });

  describe('Shutdown', () => {
    test('should shutdown gracefully', async () => {
      quotaManager.auditQueue = [{ test: 'event' }];
      quotaManager.quotaCache.set('test', 'value');
      quotaManager.usageCounters.set('test', 100);

      const syncSpy = jest.spyOn(quotaManager, 'syncUsageCounters').mockResolvedValue();

      await quotaManager.shutdown();

      expect(syncSpy).toHaveBeenCalled();
      expect(quotaManager.quotaCache.size).toBe(0);
      expect(quotaManager.usageCounters.size).toBe(0);
      expect(quotaManager.alertRules.size).toBe(0);
      expect(mockDb.disconnect).toHaveBeenCalled();

      syncSpy.mockRestore();
    });
  });

  describe('Integration Tests', () => {
    test('should handle complete quota lifecycle', async () => {
      quotaManager.isInitialized = true;
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      // Set initial quota
      await quotaManager.setCustomerQuota('customer-1', 'starter', 50);

      // Check if transpilation is allowed
      const quota = {
        customerId: 'customer-1',
        monthlyQuota: 50,
        currentUsage: 0,
        overageUsage: 0,
        maxOveragePercent: 50,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };

      quotaManager.quotaCache.set('quota:customer-1', quota);

      let canTranspile = await quotaManager.canPerformTranspilation('customer-1');
      expect(canTranspile.allowed).toBe(true);

      // Record usage up to quota limit
      for (let i = 0; i < 50; i++) {
        await quotaManager.recordUsage('customer-1');
        quota.currentUsage = i + 1;
      }

      // Check overage allowance
      canTranspile = await quotaManager.canPerformTranspilation('customer-1');
      expect(canTranspile.allowed).toBe(true);
      expect(canTranspile.reason).toBe('overage_allowed');

      // Use up overage allowance
      for (let i = 0; i < 25; i++) {
        await quotaManager.recordUsage('customer-1');
        quota.overageUsage = i + 1;
      }

      // Should now be blocked
      canTranspile = await quotaManager.canPerformTranspilation('customer-1');
      expect(canTranspile.allowed).toBe(false);
      expect(canTranspile.reason).toBe('max_overage_exceeded');

      // Reset quota
      await quotaManager.resetMonthlyQuota('customer-1');

      // Should be allowed again after reset
      quota.currentUsage = 0;
      quota.overageUsage = 0;
      canTranspile = await quotaManager.canPerformTranspilation('customer-1');
      expect(canTranspile.allowed).toBe(true);
    });
  });
});