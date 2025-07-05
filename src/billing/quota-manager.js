/**
 * Automated Quota Management System for COBOL Transpiler
 * 
 * This module implements enterprise-grade quota enforcement and management
 * with real-time tracking, automatic overage calculations, intelligent alerting,
 * and flexible quota adjustments for different subscription tiers.
 * 
 * Features:
 * - Real-time quota tracking and enforcement
 * - Automated overage calculations with tiered pricing
 * - Intelligent alerting and notifications
 * - Flexible quota adjustments and upgrades
 * - Historical usage analytics
 * - Predictive capacity planning
 * - Integration with billing and audit systems
 */

const { logger } = require('../utils/logger');
const { DatabaseManager } = require('../database/connection');
const { CacheManager } = require('../utils/cache');
const { transpilerAudit, AUDIT_EVENT_TYPES } = require('../audit/transpiler-audit');
const { EventEmitter } = require('events');

// Subscription tiers and their limits
const SUBSCRIPTION_TIERS = {
  STARTER: {
    name: 'starter',
    monthlyQuota: 50,
    basePrice: 299,
    overageRate: 8.00,
    features: ['basic_templates', 'email_support', 'standard_sla'],
    maxOveragePercent: 50, // Can't exceed 150% of base quota
    alertThresholds: [75, 90, 100]
  },
  PROFESSIONAL: {
    name: 'professional',
    monthlyQuota: 250,
    basePrice: 999,
    overageRate: 6.00,
    features: ['premium_templates', 'priority_support', 'enhanced_sla', 'analytics'],
    maxOveragePercent: 100, // Can't exceed 200% of base quota
    alertThresholds: [80, 90, 95, 100]
  },
  ENTERPRISE: {
    name: 'enterprise',
    monthlyQuota: 1000,
    basePrice: 2999,
    overageRate: 4.00,
    features: ['custom_templates', '24_7_support', 'premium_sla', 'advanced_analytics', 'dedicated_manager'],
    maxOveragePercent: 200, // Can't exceed 300% of base quota
    alertThresholds: [85, 95, 100, 110, 125]
  },
  CUSTOM: {
    name: 'custom',
    monthlyQuota: -1, // Unlimited
    basePrice: 0, // Custom pricing
    overageRate: 0,
    features: ['unlimited', 'white_glove_support', 'custom_sla', 'on_premise'],
    maxOveragePercent: 0,
    alertThresholds: []
  }
};

// Quota violation severity levels
const VIOLATION_SEVERITY = {
  WARNING: 'warning',
  CRITICAL: 'critical',
  BLOCKED: 'blocked'
};

// Notification channels
const NOTIFICATION_CHANNELS = {
  EMAIL: 'email',
  WEBHOOK: 'webhook',
  SLACK: 'slack',
  SMS: 'sms',
  IN_APP: 'in_app'
};

class QuotaManager extends EventEmitter {
  constructor() {
    super();
    this.db = new DatabaseManager();
    this.cache = new CacheManager();
    this.quotaCache = new Map();
    this.usageCounters = new Map();
    this.alertRules = new Map();
    this.isInitialized = false;
    
    // Rate limiting for quota checks
    this.lastQuotaCheck = new Map();
    this.quotaCheckCooldown = 1000; // 1 second cooldown
  }

  /**
   * Initialize quota management system
   */
  async initialize() {
    try {
      await this.db.connect();
      await this.createQuotaTables();
      await this.loadQuotaCache();
      await this.setupAlertRules();
      this.startUsageTracking();
      this.isInitialized = true;
      
      logger.info('Quota Manager initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize Quota Manager:', error);
      throw error;
    }
  }

  /**
   * Create quota management tables
   */
  async createQuotaTables() {
    const quotaTable = `
      CREATE TABLE IF NOT EXISTS customer_quotas (
        customer_id VARCHAR(100) PRIMARY KEY,
        subscription_tier VARCHAR(50) NOT NULL,
        monthly_quota INTEGER NOT NULL,
        current_usage INTEGER NOT NULL DEFAULT 0,
        overage_usage INTEGER NOT NULL DEFAULT 0,
        quota_period_start DATE NOT NULL,
        quota_period_end DATE NOT NULL,
        base_price DECIMAL(10,2) NOT NULL,
        overage_rate DECIMAL(10,2) NOT NULL,
        max_overage_percent INTEGER NOT NULL DEFAULT 100,
        status VARCHAR(20) NOT NULL DEFAULT 'active',
        alert_thresholds INTEGER[] NOT NULL DEFAULT '{}',
        last_reset_date DATE,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );
    `;

    const usageHistoryTable = `
      CREATE TABLE IF NOT EXISTS quota_usage_history (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        customer_id VARCHAR(100) NOT NULL,
        usage_date DATE NOT NULL,
        daily_usage INTEGER NOT NULL DEFAULT 0,
        cumulative_usage INTEGER NOT NULL DEFAULT 0,
        quota_remaining INTEGER NOT NULL,
        overage_amount INTEGER NOT NULL DEFAULT 0,
        cost_incurred DECIMAL(10,2) NOT NULL DEFAULT 0,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        FOREIGN KEY (customer_id) REFERENCES customer_quotas(customer_id)
      );
    `;

    const quotaAlertsTable = `
      CREATE TABLE IF NOT EXISTS quota_alerts (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        customer_id VARCHAR(100) NOT NULL,
        alert_type VARCHAR(50) NOT NULL,
        threshold_percent INTEGER NOT NULL,
        current_usage INTEGER NOT NULL,
        quota_limit INTEGER NOT NULL,
        severity VARCHAR(20) NOT NULL,
        message TEXT NOT NULL,
        channels VARCHAR(50)[] NOT NULL DEFAULT '{}',
        sent_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        acknowledged_at TIMESTAMPTZ,
        acknowledged_by VARCHAR(100),
        FOREIGN KEY (customer_id) REFERENCES customer_quotas(customer_id)
      );
    `;

    const quotaIndexes = `
      CREATE INDEX IF NOT EXISTS idx_customer_quotas_status ON customer_quotas(status);
      CREATE INDEX IF NOT EXISTS idx_customer_quotas_tier ON customer_quotas(subscription_tier);
      CREATE INDEX IF NOT EXISTS idx_usage_history_customer ON quota_usage_history(customer_id, usage_date);
      CREATE INDEX IF NOT EXISTS idx_quota_alerts_customer ON quota_alerts(customer_id, sent_at);
      CREATE INDEX IF NOT EXISTS idx_quota_alerts_type ON quota_alerts(alert_type, severity);
    `;

    await this.db.query(quotaTable);
    await this.db.query(usageHistoryTable);
    await this.db.query(quotaAlertsTable);
    await this.db.query(quotaIndexes);
  }

  /**
   * Load quota configurations into cache
   */
  async loadQuotaCache() {
    try {
      const query = `
        SELECT 
          customer_id,
          subscription_tier,
          monthly_quota,
          current_usage,
          overage_usage,
          quota_period_start,
          quota_period_end,
          base_price,
          overage_rate,
          max_overage_percent,
          status,
          alert_thresholds
        FROM customer_quotas
        WHERE status = 'active'
      `;
      
      const results = await this.db.query(query);
      
      for (const row of results) {
        const cacheKey = `quota:${row.customer_id}`;
        this.quotaCache.set(cacheKey, {
          customerId: row.customer_id,
          tier: row.subscription_tier,
          monthlyQuota: row.monthly_quota,
          currentUsage: row.current_usage,
          overageUsage: row.overage_usage,
          periodStart: row.quota_period_start,
          periodEnd: row.quota_period_end,
          basePrice: parseFloat(row.base_price),
          overageRate: parseFloat(row.overage_rate),
          maxOveragePercent: row.max_overage_percent,
          status: row.status,
          alertThresholds: row.alert_thresholds || [],
          cachedAt: Date.now()
        });
        
        // Initialize usage counter
        this.usageCounters.set(row.customer_id, row.current_usage);
      }
      
      logger.info(`Loaded quota configurations for ${results.length} customers`);
    } catch (error) {
      logger.error('Failed to load quota cache:', error);
      throw error;
    }
  }

  /**
   * Setup alert rules and notification channels
   */
  async setupAlertRules() {
    // Define default alert rules
    const defaultRules = [
      {
        type: 'quota_warning',
        thresholds: [75, 85, 90, 95],
        severity: VIOLATION_SEVERITY.WARNING,
        channels: [NOTIFICATION_CHANNELS.EMAIL, NOTIFICATION_CHANNELS.IN_APP]
      },
      {
        type: 'quota_exceeded',
        thresholds: [100],
        severity: VIOLATION_SEVERITY.CRITICAL,
        channels: [NOTIFICATION_CHANNELS.EMAIL, NOTIFICATION_CHANNELS.WEBHOOK, NOTIFICATION_CHANNELS.IN_APP]
      },
      {
        type: 'overage_limit',
        thresholds: [150, 200],
        severity: VIOLATION_SEVERITY.BLOCKED,
        channels: [NOTIFICATION_CHANNELS.EMAIL, NOTIFICATION_CHANNELS.SMS, NOTIFICATION_CHANNELS.WEBHOOK]
      }
    ];

    for (const rule of defaultRules) {
      this.alertRules.set(rule.type, rule);
    }
  }

  /**
   * Start real-time usage tracking
   */
  startUsageTracking() {
    // Update usage counters periodically
    setInterval(async () => {
      await this.syncUsageCounters();
    }, 60000); // Every minute

    // Daily quota reset check
    setInterval(async () => {
      await this.checkQuotaResets();
    }, 3600000); // Every hour

    // Generate daily usage reports
    setInterval(async () => {
      await this.generateDailyUsageReport();
    }, 86400000); // Every day at startup time
  }

  /**
   * Check if customer can perform transpilation
   */
  async canPerformTranspilation(customerId, requestId = null) {
    try {
      // Rate limiting for quota checks
      const lastCheck = this.lastQuotaCheck.get(customerId);
      if (lastCheck && (Date.now() - lastCheck) < this.quotaCheckCooldown) {
        // Use cached result for rapid requests
        const cached = this.quotaCache.get(`quota:${customerId}`);
        return this.evaluateQuotaStatus(cached);
      }

      const quota = await this.getCustomerQuota(customerId);
      if (!quota) {
        throw new Error(`No quota found for customer ${customerId}`);
      }

      const result = this.evaluateQuotaStatus(quota);
      
      // Log quota check
      await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.QUOTA_CHECK, {
        customerId,
        currentUsage: quota.currentUsage,
        monthlyQuota: quota.monthlyQuota,
        overageUsage: quota.overageUsage,
        allowed: result.allowed,
        reason: result.reason
      }, {
        customerId,
        requestId,
        action: 'quota_check'
      });

      this.lastQuotaCheck.set(customerId, Date.now());
      return result;

    } catch (error) {
      logger.error(`Quota check failed for customer ${customerId}:`, error);
      return {
        allowed: false,
        reason: 'quota_check_failed',
        details: error.message
      };
    }
  }

  /**
   * Evaluate quota status and determine if action is allowed
   */
  evaluateQuotaStatus(quota) {
    if (!quota || quota.status !== 'active') {
      return {
        allowed: false,
        reason: 'quota_inactive',
        details: 'Customer quota is not active'
      };
    }

    // Check if quota period is current
    const now = new Date();
    const periodEnd = new Date(quota.periodEnd);
    if (now > periodEnd) {
      return {
        allowed: false,
        reason: 'quota_period_expired',
        details: 'Quota period has expired and needs reset'
      };
    }

    // Unlimited quota (custom tier)
    if (quota.monthlyQuota === -1) {
      return {
        allowed: true,
        reason: 'unlimited_quota',
        remaining: -1,
        overageAllowed: 0
      };
    }

    const totalUsage = quota.currentUsage + quota.overageUsage;
    const maxAllowedUsage = quota.monthlyQuota + Math.floor(quota.monthlyQuota * quota.maxOveragePercent / 100);

    // Check if usage would exceed maximum allowed (base + overage limit)
    if (totalUsage >= maxAllowedUsage) {
      return {
        allowed: false,
        reason: 'max_overage_exceeded',
        details: `Usage would exceed maximum allowed (${maxAllowedUsage})`,
        currentUsage: totalUsage,
        maxAllowed: maxAllowedUsage
      };
    }

    // Within base quota
    if (quota.currentUsage < quota.monthlyQuota) {
      return {
        allowed: true,
        reason: 'within_quota',
        remaining: quota.monthlyQuota - quota.currentUsage,
        overageAllowed: Math.floor(quota.monthlyQuota * quota.maxOveragePercent / 100)
      };
    }

    // In overage territory but within limits
    return {
      allowed: true,
      reason: 'overage_allowed',
      remaining: 0,
      overageUsed: quota.overageUsage,
      overageRemaining: maxAllowedUsage - totalUsage,
      overageRate: quota.overageRate
    };
  }

  /**
   * Record transpilation usage
   */
  async recordUsage(customerId, requestId = null, metadata = {}) {
    try {
      const quota = await this.getCustomerQuota(customerId);
      if (!quota) {
        throw new Error(`No quota found for customer ${customerId}`);
      }

      let updatedUsage = quota.currentUsage;
      let updatedOverage = quota.overageUsage;

      // Determine if this usage goes to base quota or overage
      if (quota.currentUsage < quota.monthlyQuota) {
        updatedUsage++;
      } else {
        updatedOverage++;
      }

      // Update database
      const updateQuery = `
        UPDATE customer_quotas 
        SET 
          current_usage = $1,
          overage_usage = $2,
          updated_at = NOW()
        WHERE customer_id = $3
      `;
      
      await this.db.query(updateQuery, [updatedUsage, updatedOverage, customerId]);

      // Update cache
      const cacheKey = `quota:${customerId}`;
      const cachedQuota = this.quotaCache.get(cacheKey);
      if (cachedQuota) {
        cachedQuota.currentUsage = updatedUsage;
        cachedQuota.overageUsage = updatedOverage;
        cachedQuota.cachedAt = Date.now();
      }

      // Update usage counter
      this.usageCounters.set(customerId, updatedUsage + updatedOverage);

      // Record daily usage
      await this.recordDailyUsage(customerId, updatedUsage + updatedOverage);

      // Check for alert triggers
      await this.checkAlertTriggers(customerId, updatedUsage, updatedOverage, quota.monthlyQuota);

      // Log usage event
      await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.QUOTA_USAGE, {
        customerId,
        usageType: updatedUsage > quota.currentUsage ? 'base' : 'overage',
        newUsage: updatedUsage,
        newOverage: updatedOverage,
        totalUsage: updatedUsage + updatedOverage,
        monthlyQuota: quota.monthlyQuota,
        metadata
      }, {
        customerId,
        requestId,
        action: 'record_usage'
      });

      return {
        success: true,
        currentUsage: updatedUsage,
        overageUsage: updatedOverage,
        totalUsage: updatedUsage + updatedOverage,
        monthlyQuota: quota.monthlyQuota
      };

    } catch (error) {
      logger.error(`Failed to record usage for customer ${customerId}:`, error);
      throw error;
    }
  }

  /**
   * Get customer quota information
   */
  async getCustomerQuota(customerId) {
    const cacheKey = `quota:${customerId}`;
    let quota = this.quotaCache.get(cacheKey);

    // Check if cached quota is stale (older than 5 minutes)
    if (!quota || (Date.now() - quota.cachedAt) > 300000) {
      await this.refreshCustomerQuota(customerId);
      quota = this.quotaCache.get(cacheKey);
    }

    return quota;
  }

  /**
   * Refresh customer quota from database
   */
  async refreshCustomerQuota(customerId) {
    try {
      const query = `
        SELECT 
          customer_id,
          subscription_tier,
          monthly_quota,
          current_usage,
          overage_usage,
          quota_period_start,
          quota_period_end,
          base_price,
          overage_rate,
          max_overage_percent,
          status,
          alert_thresholds
        FROM customer_quotas
        WHERE customer_id = $1
      `;
      
      const results = await this.db.query(query, [customerId]);
      
      if (results.length === 0) {
        this.quotaCache.delete(`quota:${customerId}`);
        return null;
      }

      const row = results[0];
      const quota = {
        customerId: row.customer_id,
        tier: row.subscription_tier,
        monthlyQuota: row.monthly_quota,
        currentUsage: row.current_usage,
        overageUsage: row.overage_usage,
        periodStart: row.quota_period_start,
        periodEnd: row.quota_period_end,
        basePrice: parseFloat(row.base_price),
        overageRate: parseFloat(row.overage_rate),
        maxOveragePercent: row.max_overage_percent,
        status: row.status,
        alertThresholds: row.alert_thresholds || [],
        cachedAt: Date.now()
      };

      this.quotaCache.set(`quota:${customerId}`, quota);
      this.usageCounters.set(customerId, quota.currentUsage + quota.overageUsage);

      return quota;

    } catch (error) {
      logger.error(`Failed to refresh quota for customer ${customerId}:`, error);
      throw error;
    }
  }

  /**
   * Create or update customer quota
   */
  async setCustomerQuota(customerId, tierName, customQuota = null) {
    try {
      const tier = SUBSCRIPTION_TIERS[tierName.toUpperCase()];
      if (!tier) {
        throw new Error(`Invalid subscription tier: ${tierName}`);
      }

      const monthlyQuota = customQuota || tier.monthlyQuota;
      const now = new Date();
      const periodStart = new Date(now.getFullYear(), now.getMonth(), 1);
      const periodEnd = new Date(now.getFullYear(), now.getMonth() + 1, 0);

      const upsertQuery = `
        INSERT INTO customer_quotas (
          customer_id, subscription_tier, monthly_quota, current_usage, overage_usage,
          quota_period_start, quota_period_end, base_price, overage_rate, max_overage_percent,
          status, alert_thresholds, last_reset_date, updated_at
        ) VALUES ($1, $2, $3, 0, 0, $4, $5, $6, $7, $8, 'active', $9, $4, NOW())
        ON CONFLICT (customer_id) 
        DO UPDATE SET
          subscription_tier = $2,
          monthly_quota = $3,
          base_price = $6,
          overage_rate = $7,
          max_overage_percent = $8,
          alert_thresholds = $9,
          updated_at = NOW()
      `;

      await this.db.query(upsertQuery, [
        customerId,
        tier.name,
        monthlyQuota,
        periodStart,
        periodEnd,
        tier.basePrice,
        tier.overageRate,
        tier.maxOveragePercent,
        tier.alertThresholds
      ]);

      // Refresh cache
      await this.refreshCustomerQuota(customerId);

      // Log quota change
      await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.QUOTA_CHANGE, {
        customerId,
        newTier: tier.name,
        monthlyQuota,
        basePrice: tier.basePrice,
        overageRate: tier.overageRate
      }, {
        customerId,
        action: 'set_quota'
      });

      logger.info(`Updated quota for customer ${customerId} to ${tier.name} tier`);

      return {
        success: true,
        tier: tier.name,
        monthlyQuota,
        basePrice: tier.basePrice,
        overageRate: tier.overageRate
      };

    } catch (error) {
      logger.error(`Failed to set quota for customer ${customerId}:`, error);
      throw error;
    }
  }

  /**
   * Reset monthly quota for customer
   */
  async resetMonthlyQuota(customerId) {
    try {
      const resetQuery = `
        UPDATE customer_quotas 
        SET 
          current_usage = 0,
          overage_usage = 0,
          quota_period_start = DATE_TRUNC('month', CURRENT_DATE),
          quota_period_end = DATE_TRUNC('month', CURRENT_DATE) + INTERVAL '1 month' - INTERVAL '1 day',
          last_reset_date = CURRENT_DATE,
          updated_at = NOW()
        WHERE customer_id = $1
      `;

      await this.db.query(resetQuery, [customerId]);

      // Refresh cache
      await this.refreshCustomerQuota(customerId);

      // Log quota reset
      await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.QUOTA_RESET, {
        customerId,
        resetDate: new Date().toISOString()
      }, {
        customerId,
        action: 'reset_quota'
      });

      logger.info(`Reset monthly quota for customer ${customerId}`);

      return { success: true };

    } catch (error) {
      logger.error(`Failed to reset quota for customer ${customerId}:`, error);
      throw error;
    }
  }

  /**
   * Check for quota resets (monthly)
   */
  async checkQuotaResets() {
    try {
      const query = `
        SELECT customer_id, quota_period_end
        FROM customer_quotas
        WHERE quota_period_end < CURRENT_DATE AND status = 'active'
      `;

      const results = await this.db.query(query);

      for (const row of results) {
        await this.resetMonthlyQuota(row.customer_id);
        logger.info(`Auto-reset quota for customer ${row.customer_id}`);
      }

      if (results.length > 0) {
        logger.info(`Auto-reset quotas for ${results.length} customers`);
      }

    } catch (error) {
      logger.error('Failed to check quota resets:', error);
    }
  }

  /**
   * Record daily usage statistics
   */
  async recordDailyUsage(customerId, totalUsage) {
    try {
      const today = new Date().toISOString().split('T')[0];
      const quota = await this.getCustomerQuota(customerId);
      
      const quotaRemaining = Math.max(0, quota.monthlyQuota - quota.currentUsage);
      const overageAmount = Math.max(0, totalUsage - quota.monthlyQuota);
      const costIncurred = quota.basePrice + (overageAmount * quota.overageRate);

      const upsertQuery = `
        INSERT INTO quota_usage_history (
          customer_id, usage_date, daily_usage, cumulative_usage,
          quota_remaining, overage_amount, cost_incurred
        ) VALUES ($1, $2, 1, $3, $4, $5, $6)
        ON CONFLICT (customer_id, usage_date)
        DO UPDATE SET
          daily_usage = quota_usage_history.daily_usage + 1,
          cumulative_usage = $3,
          quota_remaining = $4,
          overage_amount = $5,
          cost_incurred = $6
      `;

      await this.db.query(upsertQuery, [
        customerId,
        today,
        totalUsage,
        quotaRemaining,
        overageAmount,
        costIncurred
      ]);

    } catch (error) {
      logger.error(`Failed to record daily usage for customer ${customerId}:`, error);
    }
  }

  /**
   * Check for alert triggers
   */
  async checkAlertTriggers(customerId, currentUsage, overageUsage, monthlyQuota) {
    try {
      const totalUsage = currentUsage + overageUsage;
      const usagePercent = (totalUsage / monthlyQuota) * 100;
      
      const quota = await this.getCustomerQuota(customerId);
      const alertThresholds = quota.alertThresholds || [];

      for (const threshold of alertThresholds) {
        if (usagePercent >= threshold) {
          // Check if we've already sent this alert today
          const today = new Date().toISOString().split('T')[0];
          const existingAlert = await this.db.query(
            'SELECT id FROM quota_alerts WHERE customer_id = $1 AND alert_type = $2 AND DATE(sent_at) = $3',
            [customerId, 'quota_threshold', today]
          );

          if (existingAlert.length === 0) {
            await this.sendQuotaAlert(customerId, {
              type: 'quota_threshold',
              threshold,
              currentUsage: totalUsage,
              quotaLimit: monthlyQuota,
              usagePercent: usagePercent.toFixed(1)
            });
          }
        }
      }

      // Check for overage alerts
      if (overageUsage > 0) {
        const overagePercent = (overageUsage / monthlyQuota) * 100;
        if (overagePercent >= 25) { // Alert when overage is 25% of base quota
          await this.sendQuotaAlert(customerId, {
            type: 'overage_warning',
            overageAmount: overageUsage,
            overagePercent: overagePercent.toFixed(1),
            quotaLimit: monthlyQuota
          });
        }
      }

    } catch (error) {
      logger.error(`Failed to check alert triggers for customer ${customerId}:`, error);
    }
  }

  /**
   * Send quota alert notification
   */
  async sendQuotaAlert(customerId, alertData) {
    try {
      const severity = this.calculateAlertSeverity(alertData);
      const message = this.generateAlertMessage(alertData);
      const channels = this.getAlertChannels(severity);

      // Store alert in database
      const insertQuery = `
        INSERT INTO quota_alerts (
          customer_id, alert_type, threshold_percent, current_usage,
          quota_limit, severity, message, channels
        ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      `;

      await this.db.query(insertQuery, [
        customerId,
        alertData.type,
        alertData.threshold || alertData.overagePercent || 0,
        alertData.currentUsage || 0,
        alertData.quotaLimit || 0,
        severity,
        message,
        channels
      ]);

      // Emit alert event for notification handlers
      this.emit('quota-alert', {
        customerId,
        severity,
        message,
        channels,
        alertData
      });

      // Log alert
      await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.QUOTA_ALERT, {
        customerId,
        alertType: alertData.type,
        severity,
        message,
        ...alertData
      }, {
        customerId,
        action: 'send_alert'
      });

      logger.info(`Sent quota alert for customer ${customerId}: ${message}`);

    } catch (error) {
      logger.error(`Failed to send quota alert for customer ${customerId}:`, error);
    }
  }

  /**
   * Calculate alert severity based on usage
   */
  calculateAlertSeverity(alertData) {
    if (alertData.type === 'overage_warning' || (alertData.threshold && alertData.threshold >= 100)) {
      return VIOLATION_SEVERITY.CRITICAL;
    } else if (alertData.threshold && alertData.threshold >= 90) {
      return VIOLATION_SEVERITY.WARNING;
    }
    return VIOLATION_SEVERITY.WARNING;
  }

  /**
   * Generate human-readable alert message
   */
  generateAlertMessage(alertData) {
    switch (alertData.type) {
      case 'quota_threshold':
        return `Quota usage at ${alertData.usagePercent}% (${alertData.currentUsage}/${alertData.quotaLimit} transpilations used)`;
      case 'overage_warning':
        return `Overage usage detected: ${alertData.overageAmount} transpilations (${alertData.overagePercent}% of base quota)`;
      case 'quota_exceeded':
        return `Monthly quota exceeded. Additional usage will incur overage charges.`;
      case 'overage_limit':
        return `Overage limit reached. Service may be restricted until quota reset or upgrade.`;
      default:
        return `Quota alert: ${alertData.type}`;
    }
  }

  /**
   * Get notification channels based on severity
   */
  getAlertChannels(severity) {
    switch (severity) {
      case VIOLATION_SEVERITY.CRITICAL:
        return [NOTIFICATION_CHANNELS.EMAIL, NOTIFICATION_CHANNELS.WEBHOOK, NOTIFICATION_CHANNELS.IN_APP];
      case VIOLATION_SEVERITY.BLOCKED:
        return [NOTIFICATION_CHANNELS.EMAIL, NOTIFICATION_CHANNELS.SMS, NOTIFICATION_CHANNELS.WEBHOOK];
      default:
        return [NOTIFICATION_CHANNELS.EMAIL, NOTIFICATION_CHANNELS.IN_APP];
    }
  }

  /**
   * Get quota usage analytics
   */
  async getUsageAnalytics(customerId, startDate, endDate) {
    try {
      const query = `
        SELECT 
          usage_date,
          daily_usage,
          cumulative_usage,
          quota_remaining,
          overage_amount,
          cost_incurred
        FROM quota_usage_history
        WHERE customer_id = $1 AND usage_date BETWEEN $2 AND $3
        ORDER BY usage_date ASC
      `;

      const results = await this.db.query(query, [customerId, startDate, endDate]);

      // Calculate summary statistics
      const summary = {
        totalUsage: results.reduce((sum, row) => sum + row.daily_usage, 0),
        averageDailyUsage: results.length > 0 ? results.reduce((sum, row) => sum + row.daily_usage, 0) / results.length : 0,
        totalOverage: results.reduce((sum, row) => sum + row.overage_amount, 0),
        totalCost: results.reduce((sum, row) => sum + parseFloat(row.cost_incurred), 0),
        peakDailyUsage: Math.max(...results.map(row => row.daily_usage), 0),
        daysWithOverage: results.filter(row => row.overage_amount > 0).length
      };

      return {
        summary,
        dailyData: results,
        period: { startDate, endDate }
      };

    } catch (error) {
      logger.error(`Failed to get usage analytics for customer ${customerId}:`, error);
      throw error;
    }
  }

  /**
   * Generate daily usage report
   */
  async generateDailyUsageReport() {
    try {
      const yesterday = new Date();
      yesterday.setDate(yesterday.getDate() - 1);
      const reportDate = yesterday.toISOString().split('T')[0];

      const query = `
        SELECT 
          cq.customer_id,
          cq.subscription_tier,
          cq.monthly_quota,
          cq.current_usage,
          cq.overage_usage,
          COALESCE(quh.daily_usage, 0) as daily_usage,
          COALESCE(quh.cost_incurred, 0) as daily_cost
        FROM customer_quotas cq
        LEFT JOIN quota_usage_history quh ON cq.customer_id = quh.customer_id 
          AND quh.usage_date = $1
        WHERE cq.status = 'active'
        ORDER BY daily_usage DESC
      `;

      const results = await this.db.query(query, [reportDate]);

      const report = {
        reportDate,
        totalCustomers: results.length,
        activeCustomers: results.filter(r => r.daily_usage > 0).length,
        totalDailyUsage: results.reduce((sum, r) => sum + r.daily_usage, 0),
        totalDailyCost: results.reduce((sum, r) => sum + parseFloat(r.daily_cost), 0),
        customerDetails: results.slice(0, 10), // Top 10 customers
        tierBreakdown: this.calculateTierBreakdown(results)
      };

      // Emit report event
      this.emit('daily-report', report);

      logger.info(`Generated daily usage report for ${reportDate}: ${report.activeCustomers}/${report.totalCustomers} customers active`);

      return report;

    } catch (error) {
      logger.error('Failed to generate daily usage report:', error);
      throw error;
    }
  }

  /**
   * Calculate tier breakdown statistics
   */
  calculateTierBreakdown(customers) {
    const breakdown = {};
    
    for (const customer of customers) {
      const tier = customer.subscription_tier;
      if (!breakdown[tier]) {
        breakdown[tier] = {
          count: 0,
          totalUsage: 0,
          totalCost: 0,
          averageUsage: 0
        };
      }
      
      breakdown[tier].count++;
      breakdown[tier].totalUsage += customer.daily_usage;
      breakdown[tier].totalCost += parseFloat(customer.daily_cost);
    }

    // Calculate averages
    for (const tier in breakdown) {
      if (breakdown[tier].count > 0) {
        breakdown[tier].averageUsage = breakdown[tier].totalUsage / breakdown[tier].count;
      }
    }

    return breakdown;
  }

  /**
   * Sync usage counters to database
   */
  async syncUsageCounters() {
    try {
      const updates = [];
      
      for (const [customerId, usage] of this.usageCounters) {
        const quota = this.quotaCache.get(`quota:${customerId}`);
        if (quota) {
          const currentUsage = Math.min(usage, quota.monthlyQuota);
          const overageUsage = Math.max(0, usage - quota.monthlyQuota);
          
          updates.push({
            customerId,
            currentUsage,
            overageUsage
          });
        }
      }

      if (updates.length > 0) {
        // Batch update for performance
        const updateQuery = `
          UPDATE customer_quotas 
          SET current_usage = data.current_usage, overage_usage = data.overage_usage, updated_at = NOW()
          FROM (VALUES ${updates.map((_, i) => `($${i * 3 + 1}, $${i * 3 + 2}, $${i * 3 + 3})`).join(', ')}) 
          AS data(customer_id, current_usage, overage_usage)
          WHERE customer_quotas.customer_id = data.customer_id
        `;
        
        const params = updates.flatMap(u => [u.customerId, u.currentUsage, u.overageUsage]);
        await this.db.query(updateQuery, params);
        
        logger.debug(`Synced usage counters for ${updates.length} customers`);
      }

    } catch (error) {
      logger.error('Failed to sync usage counters:', error);
    }
  }

  /**
   * Shutdown quota manager gracefully
   */
  async shutdown() {
    try {
      await this.syncUsageCounters();
      this.quotaCache.clear();
      this.usageCounters.clear();
      this.alertRules.clear();
      await this.db.disconnect();
      this.removeAllListeners();
      
      logger.info('Quota Manager shut down successfully');
      
    } catch (error) {
      logger.error('Error during quota manager shutdown:', error);
    }
  }
}

// Export singleton instance
const quotaManager = new QuotaManager();

module.exports = {
  QuotaManager,
  quotaManager,
  SUBSCRIPTION_TIERS,
  VIOLATION_SEVERITY,
  NOTIFICATION_CHANNELS
};