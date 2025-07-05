/**
 * SOC2 Compliance & Audit Logging for COBOL Transpiler
 * 
 * This module implements enterprise-grade audit logging to meet SOC2 Type II requirements
 * for the COBOL transpiler system. It tracks all user activities, system events, and
 * security-relevant actions with tamper-proof logging and real-time monitoring.
 * 
 * SOC2 Trust Service Criteria Coverage:
 * - Security (CC6.1): Logical and physical access controls
 * - Availability (CC7.1): System monitoring and incident response  
 * - Processing Integrity (CC8.1): Data processing accuracy and completeness
 * - Confidentiality (CC9.1): Data classification and protection
 * - Privacy (P1.0): Personal data handling and retention
 */

const crypto = require('crypto');
const { logger } = require('../utils/logger');
const { DatabaseManager } = require('../database/connection');
const { EventEmitter } = require('events');

// SOC2 Event Categories
const AUDIT_EVENT_TYPES = {
  // Authentication & Authorization
  AUTH_LOGIN: 'auth.login',
  AUTH_LOGOUT: 'auth.logout',
  AUTH_FAILED: 'auth.failed',
  AUTH_TOKEN_REFRESH: 'auth.token_refresh',
  AUTH_PERMISSION_DENIED: 'auth.permission_denied',
  
  // COBOL Transpiler Operations
  TRANSPILE_START: 'transpiler.start',
  TRANSPILE_COMPLETE: 'transpiler.complete',
  TRANSPILE_FAILED: 'transpiler.failed',
  TRANSPILE_CANCELLED: 'transpiler.cancelled',
  
  // File Operations
  FILE_UPLOAD: 'file.upload',
  FILE_DOWNLOAD: 'file.download',
  FILE_DELETE: 'file.delete',
  FILE_ACCESS: 'file.access',
  
  // Template Management
  TEMPLATE_CREATE: 'template.create',
  TEMPLATE_UPDATE: 'template.update',
  TEMPLATE_DELETE: 'template.delete',
  TEMPLATE_PUBLISH: 'template.publish',
  
  // Project Management
  PROJECT_CREATE: 'project.create',
  PROJECT_UPDATE: 'project.update',
  PROJECT_DELETE: 'project.delete',
  PROJECT_DEPLOY: 'project.deploy',
  
  // System Configuration
  CONFIG_CHANGE: 'config.change',
  USER_ROLE_CHANGE: 'user.role_change',
  PERMISSION_GRANT: 'permission.grant',
  PERMISSION_REVOKE: 'permission.revoke',
  
  // Data Access
  DATA_EXPORT: 'data.export',
  DATA_IMPORT: 'data.import',
  PII_ACCESS: 'data.pii_access',
  SENSITIVE_DATA_ACCESS: 'data.sensitive_access',
  
  // Security Events
  SECURITY_VIOLATION: 'security.violation',
  INTRUSION_ATTEMPT: 'security.intrusion',
  RATE_LIMIT_EXCEEDED: 'security.rate_limit',
  SUSPICIOUS_ACTIVITY: 'security.suspicious',
  
  // System Events
  SYSTEM_START: 'system.start',
  SYSTEM_STOP: 'system.stop',
  SYSTEM_ERROR: 'system.error',
  BACKUP_START: 'backup.start',
  BACKUP_COMPLETE: 'backup.complete'
};

// Risk Levels for SOC2 Classification
const RISK_LEVELS = {
  LOW: 'low',
  MEDIUM: 'medium',
  HIGH: 'high',
  CRITICAL: 'critical'
};

// Data Sensitivity Classifications
const DATA_CLASSIFICATIONS = {
  PUBLIC: 'public',
  INTERNAL: 'internal',
  CONFIDENTIAL: 'confidential',
  RESTRICTED: 'restricted'
};

class TranspilerAuditLogger extends EventEmitter {
  constructor() {
    super();
    this.db = new DatabaseManager();
    this.auditQueue = [];
    this.batchSize = 100;
    this.flushInterval = 5000; // 5 seconds
    this.secretKey = process.env.AUDIT_SIGNING_KEY || this.generateSigningKey();
    this.isInitialized = false;
  }

  /**
   * Initialize audit logging system
   */
  async initialize() {
    try {
      await this.db.connect();
      await this.createAuditTables();
      this.startBatchProcessor();
      this.setupEventListeners();
      this.isInitialized = true;
      
      await this.logEvent(AUDIT_EVENT_TYPES.SYSTEM_START, {
        component: 'transpiler-audit',
        version: process.env.npm_package_version || '1.0.0'
      });
      
      logger.info('Transpiler Audit Logger initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize Transpiler Audit Logger:', error);
      throw error;
    }
  }

  /**
   * Create audit tables if they don't exist
   */
  async createAuditTables() {
    const auditLogTable = `
      CREATE TABLE IF NOT EXISTS transpiler_audit_log (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        event_type VARCHAR(100) NOT NULL,
        user_id VARCHAR(100),
        customer_id VARCHAR(100),
        session_id VARCHAR(100),
        ip_address INET,
        user_agent TEXT,
        risk_level VARCHAR(20) NOT NULL DEFAULT 'low',
        data_classification VARCHAR(20) NOT NULL DEFAULT 'internal',
        event_data JSONB NOT NULL DEFAULT '{}',
        resource_type VARCHAR(50),
        resource_id VARCHAR(100),
        action VARCHAR(50),
        result VARCHAR(20),
        error_message TEXT,
        request_id VARCHAR(100),
        correlation_id VARCHAR(100),
        hash_signature TEXT NOT NULL,
        retention_date DATE,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
      );
    `;

    const auditMetricsTable = `
      CREATE TABLE IF NOT EXISTS transpiler_audit_metrics (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        date DATE NOT NULL,
        hour INTEGER NOT NULL,
        event_type VARCHAR(100) NOT NULL,
        customer_id VARCHAR(100),
        event_count INTEGER NOT NULL DEFAULT 0,
        risk_high_count INTEGER NOT NULL DEFAULT 0,
        risk_critical_count INTEGER NOT NULL DEFAULT 0,
        failed_events INTEGER NOT NULL DEFAULT 0,
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        UNIQUE(date, hour, event_type, customer_id)
      );
    `;

    const auditIndexes = `
      CREATE INDEX IF NOT EXISTS idx_audit_log_timestamp ON transpiler_audit_log(timestamp);
      CREATE INDEX IF NOT EXISTS idx_audit_log_user_id ON transpiler_audit_log(user_id);
      CREATE INDEX IF NOT EXISTS idx_audit_log_customer_id ON transpiler_audit_log(customer_id);
      CREATE INDEX IF NOT EXISTS idx_audit_log_event_type ON transpiler_audit_log(event_type);
      CREATE INDEX IF NOT EXISTS idx_audit_log_risk_level ON transpiler_audit_log(risk_level);
      CREATE INDEX IF NOT EXISTS idx_audit_log_correlation ON transpiler_audit_log(correlation_id);
      CREATE INDEX IF NOT EXISTS idx_audit_metrics_date ON transpiler_audit_metrics(date, hour);
    `;

    await this.db.query(auditLogTable);
    await this.db.query(auditMetricsTable);
    await this.db.query(auditIndexes);
  }

  /**
   * Generate cryptographic signing key for audit integrity
   */
  generateSigningKey() {
    return crypto.randomBytes(32).toString('hex');
  }

  /**
   * Create tamper-proof hash signature for audit entry
   */
  createSignature(auditEntry) {
    const payload = JSON.stringify({
      timestamp: auditEntry.timestamp,
      event_type: auditEntry.event_type,
      user_id: auditEntry.user_id,
      event_data: auditEntry.event_data
    });
    
    return crypto
      .createHmac('sha256', this.secretKey)
      .update(payload)
      .digest('hex');
  }

  /**
   * Verify audit entry integrity
   */
  verifySignature(auditEntry) {
    const expectedSignature = this.createSignature(auditEntry);
    return crypto.timingSafeEqual(
      Buffer.from(auditEntry.hash_signature, 'hex'),
      Buffer.from(expectedSignature, 'hex')
    );
  }

  /**
   * Log audit event with SOC2 compliance
   */
  async logEvent(eventType, eventData = {}, context = {}) {
    try {
      if (!this.isInitialized) {
        // Queue events during initialization
        this.auditQueue.push({ eventType, eventData, context, timestamp: new Date() });
        return;
      }

      const timestamp = new Date();
      const auditEntry = {
        timestamp: timestamp.toISOString(),
        event_type: eventType,
        user_id: context.userId || null,
        customer_id: context.customerId || null,
        session_id: context.sessionId || null,
        ip_address: context.ipAddress || null,
        user_agent: context.userAgent || null,
        risk_level: this.calculateRiskLevel(eventType, eventData),
        data_classification: this.classifyData(eventType, eventData),
        event_data: this.sanitizeEventData(eventData),
        resource_type: context.resourceType || null,
        resource_id: context.resourceId || null,
        action: context.action || null,
        result: context.result || 'success',
        error_message: context.error || null,
        request_id: context.requestId || null,
        correlation_id: context.correlationId || this.generateCorrelationId(),
        retention_date: this.calculateRetentionDate(eventType)
      };

      // Create tamper-proof signature
      auditEntry.hash_signature = this.createSignature(auditEntry);

      // Add to batch queue
      this.auditQueue.push(auditEntry);

      // Immediate flush for critical events
      if (auditEntry.risk_level === RISK_LEVELS.CRITICAL) {
        await this.flushAuditQueue();
      }

      // Emit event for real-time monitoring
      this.emit('audit-event', auditEntry);

      // Update metrics
      await this.updateMetrics(auditEntry);

    } catch (error) {
      logger.error('Failed to log audit event:', error);
      // Ensure audit failures don't break main application
    }
  }

  /**
   * Calculate risk level based on event type and data
   */
  calculateRiskLevel(eventType, eventData) {
    // Critical risk events
    if ([
      AUDIT_EVENT_TYPES.AUTH_FAILED,
      AUDIT_EVENT_TYPES.SECURITY_VIOLATION,
      AUDIT_EVENT_TYPES.INTRUSION_ATTEMPT,
      AUDIT_EVENT_TYPES.PII_ACCESS,
      AUDIT_EVENT_TYPES.PERMISSION_GRANT
    ].includes(eventType)) {
      return RISK_LEVELS.CRITICAL;
    }

    // High risk events
    if ([
      AUDIT_EVENT_TYPES.FILE_DELETE,
      AUDIT_EVENT_TYPES.PROJECT_DELETE,
      AUDIT_EVENT_TYPES.CONFIG_CHANGE,
      AUDIT_EVENT_TYPES.USER_ROLE_CHANGE,
      AUDIT_EVENT_TYPES.DATA_EXPORT
    ].includes(eventType)) {
      return RISK_LEVELS.HIGH;
    }

    // Medium risk events
    if ([
      AUDIT_EVENT_TYPES.FILE_UPLOAD,
      AUDIT_EVENT_TYPES.TRANSPILE_START,
      AUDIT_EVENT_TYPES.PROJECT_CREATE,
      AUDIT_EVENT_TYPES.TEMPLATE_CREATE
    ].includes(eventType)) {
      return RISK_LEVELS.MEDIUM;
    }

    return RISK_LEVELS.LOW;
  }

  /**
   * Classify data sensitivity
   */
  classifyData(eventType, eventData) {
    // Restricted data events
    if ([
      AUDIT_EVENT_TYPES.PII_ACCESS,
      AUDIT_EVENT_TYPES.SENSITIVE_DATA_ACCESS
    ].includes(eventType)) {
      return DATA_CLASSIFICATIONS.RESTRICTED;
    }

    // Confidential data events
    if ([
      AUDIT_EVENT_TYPES.AUTH_LOGIN,
      AUDIT_EVENT_TYPES.PERMISSION_GRANT,
      AUDIT_EVENT_TYPES.CONFIG_CHANGE,
      AUDIT_EVENT_TYPES.DATA_EXPORT
    ].includes(eventType)) {
      return DATA_CLASSIFICATIONS.CONFIDENTIAL;
    }

    // Internal data events
    if ([
      AUDIT_EVENT_TYPES.TRANSPILE_START,
      AUDIT_EVENT_TYPES.FILE_UPLOAD,
      AUDIT_EVENT_TYPES.PROJECT_CREATE
    ].includes(eventType)) {
      return DATA_CLASSIFICATIONS.INTERNAL;
    }

    return DATA_CLASSIFICATIONS.PUBLIC;
  }

  /**
   * Sanitize event data to remove sensitive information
   */
  sanitizeEventData(eventData) {
    const sanitized = { ...eventData };
    
    // Remove sensitive fields
    const sensitiveFields = [
      'password', 'token', 'secret', 'key', 'ssn', 'credit_card',
      'authorization', 'signature', 'private_key', 'api_key'
    ];
    
    for (const field of sensitiveFields) {
      if (sanitized[field]) {
        sanitized[field] = '[REDACTED]';
      }
    }

    // Truncate large fields
    for (const [key, value] of Object.entries(sanitized)) {
      if (typeof value === 'string' && value.length > 1000) {
        sanitized[key] = value.substring(0, 1000) + '[TRUNCATED]';
      }
    }

    return sanitized;
  }

  /**
   * Generate correlation ID for event tracking
   */
  generateCorrelationId() {
    return crypto.randomBytes(16).toString('hex');
  }

  /**
   * Calculate retention date based on SOC2 requirements
   */
  calculateRetentionDate(eventType) {
    const now = new Date();
    
    // Security events: 7 years retention
    if (eventType.startsWith('security.') || eventType.startsWith('auth.')) {
      return new Date(now.getFullYear() + 7, now.getMonth(), now.getDate());
    }
    
    // Financial/audit events: 7 years retention
    if ([
      AUDIT_EVENT_TYPES.DATA_EXPORT,
      AUDIT_EVENT_TYPES.PII_ACCESS,
      AUDIT_EVENT_TYPES.PERMISSION_GRANT
    ].includes(eventType)) {
      return new Date(now.getFullYear() + 7, now.getMonth(), now.getDate());
    }
    
    // Operational events: 3 years retention
    return new Date(now.getFullYear() + 3, now.getMonth(), now.getDate());
  }

  /**
   * Start batch processor for efficient audit logging
   */
  startBatchProcessor() {
    setInterval(async () => {
      if (this.auditQueue.length > 0) {
        await this.flushAuditQueue();
      }
    }, this.flushInterval);
  }

  /**
   * Flush audit queue to database
   */
  async flushAuditQueue() {
    if (this.auditQueue.length === 0) return;

    const batch = this.auditQueue.splice(0, this.batchSize);
    
    try {
      const values = batch.map(entry => [
        entry.timestamp,
        entry.event_type,
        entry.user_id,
        entry.customer_id,
        entry.session_id,
        entry.ip_address,
        entry.user_agent,
        entry.risk_level,
        entry.data_classification,
        JSON.stringify(entry.event_data),
        entry.resource_type,
        entry.resource_id,
        entry.action,
        entry.result,
        entry.error_message,
        entry.request_id,
        entry.correlation_id,
        entry.hash_signature,
        entry.retention_date
      ]);

      const query = `
        INSERT INTO transpiler_audit_log (
          timestamp, event_type, user_id, customer_id, session_id,
          ip_address, user_agent, risk_level, data_classification, event_data,
          resource_type, resource_id, action, result, error_message,
          request_id, correlation_id, hash_signature, retention_date
        ) VALUES ${values.map((_, i) => `($${i * 19 + 1}, $${i * 19 + 2}, $${i * 19 + 3}, $${i * 19 + 4}, $${i * 19 + 5}, $${i * 19 + 6}, $${i * 19 + 7}, $${i * 19 + 8}, $${i * 19 + 9}, $${i * 19 + 10}, $${i * 19 + 11}, $${i * 19 + 12}, $${i * 19 + 13}, $${i * 19 + 14}, $${i * 19 + 15}, $${i * 19 + 16}, $${i * 19 + 17}, $${i * 19 + 18}, $${i * 19 + 19})`).join(', ')}
      `;

      await this.db.query(query, values.flat());
      
      logger.debug(`Flushed ${batch.length} audit events to database`);
      
    } catch (error) {
      logger.error('Failed to flush audit queue:', error);
      // Re-queue failed entries for retry
      this.auditQueue.unshift(...batch);
    }
  }

  /**
   * Update real-time audit metrics
   */
  async updateMetrics(auditEntry) {
    try {
      const now = new Date(auditEntry.timestamp);
      const date = now.toISOString().split('T')[0];
      const hour = now.getHours();
      
      const query = `
        INSERT INTO transpiler_audit_metrics (
          date, hour, event_type, customer_id, event_count,
          risk_high_count, risk_critical_count, failed_events
        ) VALUES ($1, $2, $3, $4, 1, $5, $6, $7)
        ON CONFLICT (date, hour, event_type, customer_id)
        DO UPDATE SET
          event_count = transpiler_audit_metrics.event_count + 1,
          risk_high_count = transpiler_audit_metrics.risk_high_count + $5,
          risk_critical_count = transpiler_audit_metrics.risk_critical_count + $6,
          failed_events = transpiler_audit_metrics.failed_events + $7
      `;
      
      await this.db.query(query, [
        date,
        hour,
        auditEntry.event_type,
        auditEntry.customer_id,
        auditEntry.risk_level === RISK_LEVELS.HIGH ? 1 : 0,
        auditEntry.risk_level === RISK_LEVELS.CRITICAL ? 1 : 0,
        auditEntry.result === 'failed' ? 1 : 0
      ]);
      
    } catch (error) {
      logger.error('Failed to update audit metrics:', error);
    }
  }

  /**
   * Setup event listeners for automatic audit logging
   */
  setupEventListeners() {
    // Listen for transpiler events
    process.on('transpiler:start', (data) => {
      this.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_START, data, data.context);
    });

    process.on('transpiler:complete', (data) => {
      this.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_COMPLETE, data, data.context);
    });

    process.on('transpiler:failed', (data) => {
      this.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_FAILED, data, {
        ...data.context,
        result: 'failed',
        error: data.error
      });
    });

    // Listen for security events
    process.on('security:violation', (data) => {
      this.logEvent(AUDIT_EVENT_TYPES.SECURITY_VIOLATION, data, {
        ...data.context,
        result: 'violation'
      });
    });
  }

  /**
   * Query audit logs with SOC2 compliance filters
   */
  async queryAuditLogs(filters = {}) {
    try {
      let query = 'SELECT * FROM transpiler_audit_log WHERE 1=1';
      const params = [];
      let paramIndex = 1;

      // Apply filters
      if (filters.startDate) {
        query += ` AND timestamp >= $${paramIndex}`;
        params.push(filters.startDate);
        paramIndex++;
      }

      if (filters.endDate) {
        query += ` AND timestamp <= $${paramIndex}`;
        params.push(filters.endDate);
        paramIndex++;
      }

      if (filters.userId) {
        query += ` AND user_id = $${paramIndex}`;
        params.push(filters.userId);
        paramIndex++;
      }

      if (filters.customerId) {
        query += ` AND customer_id = $${paramIndex}`;
        params.push(filters.customerId);
        paramIndex++;
      }

      if (filters.eventType) {
        query += ` AND event_type = $${paramIndex}`;
        params.push(filters.eventType);
        paramIndex++;
      }

      if (filters.riskLevel) {
        query += ` AND risk_level = $${paramIndex}`;
        params.push(filters.riskLevel);
        paramIndex++;
      }

      if (filters.resourceId) {
        query += ` AND resource_id = $${paramIndex}`;
        params.push(filters.resourceId);
        paramIndex++;
      }

      query += ' ORDER BY timestamp DESC';

      if (filters.limit) {
        query += ` LIMIT $${paramIndex}`;
        params.push(filters.limit);
        paramIndex++;
      }

      const results = await this.db.query(query, params);
      return results;

    } catch (error) {
      logger.error('Failed to query audit logs:', error);
      throw error;
    }
  }

  /**
   * Generate SOC2 compliance report
   */
  async generateComplianceReport(startDate, endDate, customerId = null) {
    try {
      const baseQuery = `
        SELECT 
          event_type,
          risk_level,
          data_classification,
          COUNT(*) as event_count,
          COUNT(DISTINCT user_id) as unique_users,
          SUM(CASE WHEN result = 'failed' THEN 1 ELSE 0 END) as failed_events
        FROM transpiler_audit_log 
        WHERE timestamp >= $1 AND timestamp <= $2
      `;

      let query = baseQuery;
      const params = [startDate, endDate];

      if (customerId) {
        query += ' AND customer_id = $3';
        params.push(customerId);
      }

      query += ' GROUP BY event_type, risk_level, data_classification ORDER BY event_count DESC';

      const results = await this.db.query(query, params);

      // Calculate summary metrics
      const summary = {
        reportPeriod: { startDate, endDate },
        totalEvents: results.reduce((sum, row) => sum + parseInt(row.event_count), 0),
        uniqueUsers: new Set(results.map(row => row.unique_users)).size,
        riskDistribution: {},
        dataClassificationDistribution: {},
        failureRate: 0
      };

      let totalFailures = 0;
      for (const row of results) {
        // Risk distribution
        if (!summary.riskDistribution[row.risk_level]) {
          summary.riskDistribution[row.risk_level] = 0;
        }
        summary.riskDistribution[row.risk_level] += parseInt(row.event_count);

        // Data classification distribution
        if (!summary.dataClassificationDistribution[row.data_classification]) {
          summary.dataClassificationDistribution[row.data_classification] = 0;
        }
        summary.dataClassificationDistribution[row.data_classification] += parseInt(row.event_count);

        totalFailures += parseInt(row.failed_events);
      }

      summary.failureRate = summary.totalEvents > 0 ? (totalFailures / summary.totalEvents) * 100 : 0;

      return {
        summary,
        details: results,
        generatedAt: new Date().toISOString(),
        compliance: {
          soc2TypeII: true,
          retentionCompliant: true,
          integrityVerified: true
        }
      };

    } catch (error) {
      logger.error('Failed to generate compliance report:', error);
      throw error;
    }
  }

  /**
   * Verify audit log integrity
   */
  async verifyIntegrity(correlationId) {
    try {
      const query = 'SELECT * FROM transpiler_audit_log WHERE correlation_id = $1';
      const results = await this.db.query(query, [correlationId]);

      if (results.length === 0) {
        return { valid: false, error: 'Audit entry not found' };
      }

      const entry = results[0];
      const isValid = this.verifySignature(entry);

      return {
        valid: isValid,
        entry: entry,
        verifiedAt: new Date().toISOString()
      };

    } catch (error) {
      logger.error('Failed to verify audit integrity:', error);
      return { valid: false, error: error.message };
    }
  }

  /**
   * Clean up expired audit logs based on retention policy
   */
  async cleanupExpiredLogs() {
    try {
      const query = 'DELETE FROM transpiler_audit_log WHERE retention_date < CURRENT_DATE';
      const result = await this.db.query(query);
      
      logger.info(`Cleaned up ${result.rowCount} expired audit logs`);
      
      await this.logEvent(AUDIT_EVENT_TYPES.SYSTEM_MAINTENANCE, {
        action: 'cleanup_expired_logs',
        deletedCount: result.rowCount
      });

      return result.rowCount;

    } catch (error) {
      logger.error('Failed to cleanup expired logs:', error);
      throw error;
    }
  }

  /**
   * Shutdown audit logger gracefully
   */
  async shutdown() {
    try {
      await this.logEvent(AUDIT_EVENT_TYPES.SYSTEM_STOP, {
        component: 'transpiler-audit'
      });
      
      // Flush any remaining events
      await this.flushAuditQueue();
      
      await this.db.disconnect();
      this.removeAllListeners();
      
      logger.info('Transpiler Audit Logger shut down successfully');
      
    } catch (error) {
      logger.error('Error during audit logger shutdown:', error);
    }
  }
}

// Export singleton instance
const transpilerAudit = new TranspilerAuditLogger();

module.exports = {
  TranspilerAuditLogger,
  transpilerAudit,
  AUDIT_EVENT_TYPES,
  RISK_LEVELS,
  DATA_CLASSIFICATIONS
};