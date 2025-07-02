/**
 * TCS BaNCS Webhook Handler
 * Processes real-time notifications from TCS BaNCS core banking system
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 3 - Advanced Enterprise Features
 */

const express = require('express');
const crypto = require('crypto');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const { EventEmitter } = require('events');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'bancs-webhook-handler' }
});

/**
 * BaNCS webhook event types
 */
const WEBHOOK_EVENTS = {
  TRANSACTION_STATUS_CHANGED: 'transaction.status.changed',
  ACCOUNT_BALANCE_UPDATED: 'account.balance.updated',
  COMPLIANCE_STATUS_CHANGED: 'compliance.status.changed',
  CUSTOMER_VERIFICATION_COMPLETED: 'customer.verification.completed',
  FRAUD_ALERT_TRIGGERED: 'fraud.alert.triggered',
  REGULATORY_NOTIFICATION: 'regulatory.notification',
  SYSTEM_MAINTENANCE_SCHEDULED: 'system.maintenance.scheduled',
  API_RATE_LIMIT_EXCEEDED: 'api.rate_limit.exceeded'
};

/**
 * BaNCS transaction status mapping
 */
const BANCS_STATUS_MAPPING = {
  'INITIATED': 'pending',
  'PROCESSING': 'processing',
  'COMPLETED': 'confirmed',
  'FAILED': 'failed',
  'REJECTED': 'rejected',
  'PENDING_APPROVAL': 'pending_approval',
  'AUTHORIZED': 'authorized',
  'SETTLED': 'settled',
  'CANCELLED': 'cancelled',
  'EXPIRED': 'expired'
};

/**
 * BaNCS Webhook Handler Class
 * Processes incoming webhooks from TCS BaNCS system
 */
class BaNCSWebhookHandler extends EventEmitter {
  constructor(config = {}) {
    super();
    
    this.config = {
      // Webhook server configuration
      port: config.port || process.env.BANCS_WEBHOOK_PORT || 3001,
      path: config.path || '/webhooks/bancs',
      
      // Security configuration
      webhookSecret: config.webhookSecret || process.env.BANCS_WEBHOOK_SECRET,
      enableSignatureVerification: config.enableSignatureVerification !== false,
      enableRequestLogging: config.enableRequestLogging !== false,
      
      // Processing configuration
      maxPayloadSize: config.maxPayloadSize || '10mb',
      requestTimeout: config.requestTimeout || 30000,
      enableRetryQueue: config.enableRetryQueue !== false,
      
      // Rate limiting
      enableRateLimit: config.enableRateLimit !== false,
      rateLimitWindow: config.rateLimitWindow || 60000, // 1 minute
      rateLimitMax: config.rateLimitMax || 100,
      
      ...config
    };

    // Initialize Express app
    this.app = express();
    this.server = null;
    
    // Webhook processing state
    this.webhookQueue = [];
    this.processingStats = {
      totalReceived: 0,
      successfullyProcessed: 0,
      failed: 0,
      duplicatesDetected: 0,
      invalidSignatures: 0,
      averageProcessingTime: 0
    };
    
    // Duplicate detection cache
    this.processedWebhooks = new Map();
    this.duplicateCheckExpiry = 300000; // 5 minutes
    
    // Rate limiting storage
    this.rateLimitCounters = new Map();
    
    // Setup middleware and routes
    this.setupMiddleware();
    this.setupRoutes();
    
    logger.info('BaNCS Webhook Handler initialized', {
      port: this.config.port,
      path: this.config.path,
      signatureVerification: this.config.enableSignatureVerification
    });
  }

  /**
   * Setup Express middleware
   */
  setupMiddleware() {
    // Request parsing
    this.app.use(express.json({ 
      limit: this.config.maxPayloadSize,
      verify: (req, res, buf) => {
        req.rawBody = buf;
      }
    }));
    
    this.app.use(express.urlencoded({ extended: true }));
    
    // Request logging middleware
    if (this.config.enableRequestLogging) {
      this.app.use((req, res, next) => {
        const requestId = uuidv4();
        req.requestId = requestId;
        
        logger.info('Webhook request received', {
          requestId,
          method: req.method,
          url: req.url,
          userAgent: req.get('User-Agent'),
          contentLength: req.get('Content-Length'),
          timestamp: new Date().toISOString()
        });
        
        next();
      });
    }
    
    // Rate limiting middleware
    if (this.config.enableRateLimit) {
      this.app.use(this.rateLimitMiddleware.bind(this));
    }
    
    // Security headers
    this.app.use((req, res, next) => {
      res.setHeader('X-Content-Type-Options', 'nosniff');
      res.setHeader('X-Frame-Options', 'DENY');
      res.setHeader('X-XSS-Protection', '1; mode=block');
      next();
    });
  }

  /**
   * Setup webhook routes
   */
  setupRoutes() {
    // Main webhook endpoint
    this.app.post(this.config.path, this.handleWebhook.bind(this));
    
    // Health check endpoint
    this.app.get('/health', (req, res) => {
      res.json({
        status: 'healthy',
        service: 'bancs-webhook-handler',
        uptime: process.uptime(),
        timestamp: new Date().toISOString(),
        stats: this.getProcessingStats()
      });
    });
    
    // Metrics endpoint
    this.app.get('/metrics', (req, res) => {
      res.json({
        webhook_stats: this.processingStats,
        queue_size: this.webhookQueue.length,
        processed_cache_size: this.processedWebhooks.size,
        rate_limit_counters: this.rateLimitCounters.size
      });
    });
    
    // JSON parsing error handling middleware
    this.app.use((error, req, res, next) => {
      if (error instanceof SyntaxError && error.status === 400 && 'body' in error) {
        logger.warn('Malformed JSON payload', {
          requestId: req.requestId,
          error: error.message
        });
        return res.status(400).json({
          error: 'Invalid JSON payload',
          requestId: req.requestId
        });
      }
      
      logger.error('Webhook handler error', {
        requestId: req.requestId,
        error: error.message,
        stack: error.stack
      });
      
      res.status(500).json({
        error: 'Internal server error',
        requestId: req.requestId
      });
    });
  }

  /**
   * Rate limiting middleware
   */
  rateLimitMiddleware(req, res, next) {
    const clientIp = req.ip || req.connection.remoteAddress;
    const now = Date.now();
    const windowStart = now - this.config.rateLimitWindow;
    
    // Clean old entries
    for (const [ip, requests] of this.rateLimitCounters.entries()) {
      const filtered = requests.filter(timestamp => timestamp > windowStart);
      if (filtered.length === 0) {
        this.rateLimitCounters.delete(ip);
      } else {
        this.rateLimitCounters.set(ip, filtered);
      }
    }
    
    // Check current client
    const clientRequests = this.rateLimitCounters.get(clientIp) || [];
    const recentRequests = clientRequests.filter(timestamp => timestamp > windowStart);
    
    if (recentRequests.length >= this.config.rateLimitMax) {
      logger.warn('Rate limit exceeded', {
        clientIp,
        requestCount: recentRequests.length,
        limit: this.config.rateLimitMax
      });
      
      return res.status(429).json({
        error: 'Rate limit exceeded',
        retryAfter: Math.ceil(this.config.rateLimitWindow / 1000)
      });
    }
    
    // Add current request
    recentRequests.push(now);
    this.rateLimitCounters.set(clientIp, recentRequests);
    
    next();
  }

  /**
   * Main webhook handler
   */
  async handleWebhook(req, res) {
    const startTime = Date.now();
    const requestId = req.requestId;
    
    try {
      this.processingStats.totalReceived++;
      
      // Verify webhook signature
      if (this.config.enableSignatureVerification) {
        const isValid = this.verifyWebhookSignature(req);
        if (!isValid) {
          this.processingStats.invalidSignatures++;
          logger.warn('Invalid webhook signature', { requestId });
          return res.status(401).json({ error: 'Invalid signature' });
        }
      }
      
      // Parse webhook payload
      const webhookData = this.parseWebhookPayload(req.body);
      if (!webhookData) {
        logger.error('Invalid webhook payload', { requestId, body: req.body });
        return res.status(400).json({ error: 'Invalid payload format' });
      }
      
      // Check for duplicates
      const isDuplicate = this.checkDuplicate(webhookData);
      if (isDuplicate) {
        this.processingStats.duplicatesDetected++;
        logger.warn('Duplicate webhook detected', { 
          requestId, 
          webhookId: webhookData.id 
        });
        return res.status(200).json({ status: 'duplicate', message: 'Already processed' });
      }
      
      // Process webhook
      await this.processWebhook(webhookData, requestId);
      
      // Update metrics
      const processingTime = Date.now() - startTime;
      this.processingStats.successfullyProcessed++;
      this.processingStats.averageProcessingTime = 
        (this.processingStats.averageProcessingTime * (this.processingStats.successfullyProcessed - 1) + 
         processingTime) / this.processingStats.successfullyProcessed;
      
      logger.info('Webhook processed successfully', {
        requestId,
        webhookId: webhookData.id,
        eventType: webhookData.event_type,
        processingTime
      });
      
      res.status(200).json({
        status: 'success',
        message: 'Webhook processed',
        requestId,
        processingTime
      });
      
    } catch (error) {
      this.processingStats.failed++;
      
      logger.error('Webhook processing failed', {
        requestId,
        error: error.message,
        stack: error.stack
      });
      
      res.status(500).json({
        status: 'error',
        message: 'Processing failed',
        requestId
      });
      
      // Emit error event for monitoring
      this.emit('webhook_error', {
        requestId,
        error: error.message,
        timestamp: new Date().toISOString()
      });
    }
  }

  /**
   * Verify webhook signature
   */
  verifyWebhookSignature(req) {
    if (!this.config.webhookSecret) {
      logger.warn('Webhook secret not configured, skipping signature verification');
      return true;
    }
    
    const signature = req.get('X-BaNCS-Signature') || req.get('X-Signature');
    if (!signature) {
      logger.warn('Missing webhook signature header');
      return false;
    }
    
    const body = req.rawBody || Buffer.from(JSON.stringify(req.body));
    const expectedSignature = crypto
      .createHmac('sha256', this.config.webhookSecret)
      .update(body)
      .digest('hex');
    
    const providedSignature = signature.replace('sha256=', '');
    
    // Ensure both signatures are the same length for timingSafeEqual
    if (expectedSignature.length !== providedSignature.length) {
      return false;
    }
    
    try {
      return crypto.timingSafeEqual(
        Buffer.from(expectedSignature, 'hex'),
        Buffer.from(providedSignature, 'hex')
      );
    } catch (error) {
      logger.warn('Signature comparison failed', { error: error.message });
      return false;
    }
  }

  /**
   * Parse and validate webhook payload
   */
  parseWebhookPayload(body) {
    try {
      // Ensure required fields exist
      if (!body.id || !body.event_type || !body.timestamp) {
        return null;
      }
      
      return {
        id: body.id,
        event_type: body.event_type,
        timestamp: body.timestamp,
        data: body.data || {},
        metadata: body.metadata || {},
        version: body.version || '1.0'
      };
    } catch (error) {
      logger.error('Webhook payload parsing failed', { error: error.message });
      return null;
    }
  }

  /**
   * Check for duplicate webhooks
   */
  checkDuplicate(webhookData) {
    const now = Date.now();
    const webhookId = webhookData.id;
    
    // Clean expired entries
    for (const [id, timestamp] of this.processedWebhooks.entries()) {
      if (now - timestamp > this.duplicateCheckExpiry) {
        this.processedWebhooks.delete(id);
      }
    }
    
    // Check if already processed
    if (this.processedWebhooks.has(webhookId)) {
      return true;
    }
    
    // Mark as processed
    this.processedWebhooks.set(webhookId, now);
    return false;
  }

  /**
   * Process webhook based on event type
   */
  async processWebhook(webhookData, requestId) {
    const { event_type, data, metadata } = webhookData;
    
    switch (event_type) {
      case WEBHOOK_EVENTS.TRANSACTION_STATUS_CHANGED:
        await this.handleTransactionStatusChange(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.ACCOUNT_BALANCE_UPDATED:
        await this.handleAccountBalanceUpdate(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.COMPLIANCE_STATUS_CHANGED:
        await this.handleComplianceStatusChange(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.CUSTOMER_VERIFICATION_COMPLETED:
        await this.handleCustomerVerification(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.FRAUD_ALERT_TRIGGERED:
        await this.handleFraudAlert(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.REGULATORY_NOTIFICATION:
        await this.handleRegulatoryNotification(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.SYSTEM_MAINTENANCE_SCHEDULED:
        await this.handleSystemMaintenance(data, metadata, requestId);
        break;
        
      case WEBHOOK_EVENTS.API_RATE_LIMIT_EXCEEDED:
        await this.handleRateLimitExceeded(data, metadata, requestId);
        break;
        
      default:
        logger.warn('Unknown webhook event type', { 
          event_type, 
          requestId,
          webhookId: webhookData.id 
        });
        
        this.emit('unknown_webhook_event', {
          event_type,
          data,
          requestId,
          timestamp: new Date().toISOString()
        });
    }
  }

  /**
   * Handle transaction status change webhooks
   */
  async handleTransactionStatusChange(data, metadata, requestId) {
    const {
      transaction_id,
      old_status,
      new_status,
      status_reason,
      updated_at
    } = data;
    
    const mappedStatus = BANCS_STATUS_MAPPING[new_status] || new_status.toLowerCase();
    
    logger.info('Transaction status changed', {
      transactionId: transaction_id,
      oldStatus: old_status,
      newStatus: new_status,
      mappedStatus,
      reason: status_reason,
      requestId
    });
    
    // Emit event for blockchain gateway to handle
    this.emit('transaction_status_changed', {
      transactionId: transaction_id,
      oldStatus: BANCS_STATUS_MAPPING[old_status] || old_status.toLowerCase(),
      newStatus: mappedStatus,
      reason: status_reason,
      timestamp: updated_at,
      requestId,
      source: 'bancs_webhook'
    });
  }

  /**
   * Handle account balance update webhooks
   */
  async handleAccountBalanceUpdate(data, metadata, requestId) {
    const {
      account_number,
      old_balance,
      new_balance,
      currency,
      update_type,
      updated_at
    } = data;
    
    logger.info('Account balance updated', {
      accountNumber: account_number,
      oldBalance: old_balance,
      newBalance: new_balance,
      currency,
      updateType: update_type,
      requestId
    });
    
    this.emit('account_balance_updated', {
      accountNumber: account_number,
      oldBalance: old_balance,
      newBalance: new_balance,
      currency,
      updateType: update_type,
      timestamp: updated_at,
      requestId
    });
  }

  /**
   * Handle compliance status change webhooks
   */
  async handleComplianceStatusChange(data, metadata, requestId) {
    const {
      transaction_id,
      compliance_type,
      old_status,
      new_status,
      risk_score,
      manual_review_required,
      updated_at
    } = data;
    
    logger.info('Compliance status changed', {
      transactionId: transaction_id,
      complianceType: compliance_type,
      oldStatus: old_status,
      newStatus: new_status,
      riskScore: risk_score,
      manualReviewRequired: manual_review_required,
      requestId
    });
    
    this.emit('compliance_status_changed', {
      transactionId: transaction_id,
      complianceType: compliance_type,
      oldStatus: old_status,
      newStatus: new_status,
      riskScore: risk_score,
      manualReviewRequired: manual_review_required,
      timestamp: updated_at,
      requestId
    });
  }

  /**
   * Handle customer verification completion webhooks
   */
  async handleCustomerVerification(data, metadata, requestId) {
    const {
      customer_id,
      verification_type,
      status,
      documents_verified,
      verified_at
    } = data;
    
    logger.info('Customer verification completed', {
      customerId: customer_id,
      verificationType: verification_type,
      status,
      documentsVerified: documents_verified,
      requestId
    });
    
    this.emit('customer_verification_completed', {
      customerId: customer_id,
      verificationType: verification_type,
      status,
      documentsVerified: documents_verified,
      timestamp: verified_at,
      requestId
    });
  }

  /**
   * Handle fraud alert webhooks
   */
  async handleFraudAlert(data, metadata, requestId) {
    const {
      alert_id,
      transaction_id,
      alert_type,
      severity,
      risk_indicators,
      recommended_action,
      triggered_at
    } = data;
    
    logger.warn('Fraud alert triggered', {
      alertId: alert_id,
      transactionId: transaction_id,
      alertType: alert_type,
      severity,
      riskIndicators: risk_indicators,
      recommendedAction: recommended_action,
      requestId
    });
    
    this.emit('fraud_alert_triggered', {
      alertId: alert_id,
      transactionId: transaction_id,
      alertType: alert_type,
      severity,
      riskIndicators: risk_indicators,
      recommendedAction: recommended_action,
      timestamp: triggered_at,
      requestId
    });
  }

  /**
   * Handle regulatory notification webhooks
   */
  async handleRegulatoryNotification(data, metadata, requestId) {
    const {
      notification_id,
      regulation_type,
      message,
      action_required,
      deadline,
      received_at
    } = data;
    
    logger.info('Regulatory notification received', {
      notificationId: notification_id,
      regulationType: regulation_type,
      message,
      actionRequired: action_required,
      deadline,
      requestId
    });
    
    this.emit('regulatory_notification', {
      notificationId: notification_id,
      regulationType: regulation_type,
      message,
      actionRequired: action_required,
      deadline,
      timestamp: received_at,
      requestId
    });
  }

  /**
   * Handle system maintenance webhooks
   */
  async handleSystemMaintenance(data, metadata, requestId) {
    const {
      maintenance_id,
      maintenance_type,
      scheduled_start,
      scheduled_end,
      affected_services,
      maintenance_reason
    } = data;
    
    logger.info('System maintenance scheduled', {
      maintenanceId: maintenance_id,
      maintenanceType: maintenance_type,
      scheduledStart: scheduled_start,
      scheduledEnd: scheduled_end,
      affectedServices: affected_services,
      reason: maintenance_reason,
      requestId
    });
    
    this.emit('system_maintenance_scheduled', {
      maintenanceId: maintenance_id,
      maintenanceType: maintenance_type,
      scheduledStart: scheduled_start,
      scheduledEnd: scheduled_end,
      affectedServices: affected_services,
      reason: maintenance_reason,
      requestId
    });
  }

  /**
   * Handle API rate limit exceeded webhooks
   */
  async handleRateLimitExceeded(data, metadata, requestId) {
    const {
      client_id,
      endpoint,
      current_usage,
      limit,
      reset_time,
      exceeded_at
    } = data;
    
    logger.warn('API rate limit exceeded', {
      clientId: client_id,
      endpoint,
      currentUsage: current_usage,
      limit,
      resetTime: reset_time,
      requestId
    });
    
    this.emit('api_rate_limit_exceeded', {
      clientId: client_id,
      endpoint,
      currentUsage: current_usage,
      limit,
      resetTime: reset_time,
      timestamp: exceeded_at,
      requestId
    });
  }

  /**
   * Start the webhook server
   */
  async start() {
    return new Promise((resolve, reject) => {
      this.server = this.app.listen(this.config.port, (error) => {
        if (error) {
          logger.error('Failed to start webhook server', { error: error.message });
          reject(error);
        } else {
          logger.info('BaNCS webhook server started', {
            port: this.config.port,
            path: this.config.path
          });
          resolve();
        }
      });
    });
  }

  /**
   * Stop the webhook server
   */
  async stop() {
    if (this.server) {
      return new Promise((resolve) => {
        this.server.close(() => {
          logger.info('BaNCS webhook server stopped');
          resolve();
        });
      });
    }
  }

  /**
   * Get processing statistics
   */
  getProcessingStats() {
    return {
      ...this.processingStats,
      successRate: this.processingStats.totalReceived > 0 
        ? this.processingStats.successfullyProcessed / this.processingStats.totalReceived 
        : 0,
      queueSize: this.webhookQueue.length,
      processedCacheSize: this.processedWebhooks.size
    };
  }

  /**
   * Clear processing statistics
   */
  clearStats() {
    this.processingStats = {
      totalReceived: 0,
      successfullyProcessed: 0,
      failed: 0,
      duplicatesDetected: 0,
      invalidSignatures: 0,
      averageProcessingTime: 0
    };
    
    this.processedWebhooks.clear();
    this.rateLimitCounters.clear();
    
    logger.info('Webhook processing statistics cleared');
  }
}

module.exports = {
  BaNCSWebhookHandler,
  WEBHOOK_EVENTS,
  BANCS_STATUS_MAPPING
};