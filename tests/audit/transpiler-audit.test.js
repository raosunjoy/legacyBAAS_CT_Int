const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const crypto = require('crypto');
const { 
  TranspilerAuditLogger,
  AUDIT_EVENT_TYPES,
  RISK_LEVELS,
  DATA_CLASSIFICATIONS 
} = require('../../src/audit/transpiler-audit');

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

describe('Transpiler Audit Logger', () => {
  let auditLogger;
  let mockDb;

  beforeAll(async () => {
    auditLogger = new TranspilerAuditLogger();
    mockDb = auditLogger.db;
    
    // Mock environment variable
    process.env.AUDIT_SIGNING_KEY = 'test-signing-key-for-audit-integrity-verification';
  });

  afterAll(async () => {
    await auditLogger.shutdown();
    delete process.env.AUDIT_SIGNING_KEY;
  });

  beforeEach(() => {
    jest.clearAllMocks();
    auditLogger.auditQueue = [];
    auditLogger.isInitialized = false;
  });

  describe('Initialization', () => {
    test('should initialize audit logger successfully', async () => {
      mockDb.connect.mockResolvedValue(true);
      mockDb.query.mockResolvedValue({ rowCount: 0 });

      await auditLogger.initialize();

      expect(mockDb.connect).toHaveBeenCalled();
      expect(mockDb.query).toHaveBeenCalledWith(expect.stringContaining('CREATE TABLE IF NOT EXISTS'));
      expect(auditLogger.isInitialized).toBe(true);
    });

    test('should handle initialization errors gracefully', async () => {
      const error = new Error('Database connection failed');
      mockDb.connect.mockRejectedValue(error);

      await expect(auditLogger.initialize()).rejects.toThrow('Database connection failed');
    });

    test('should create required audit tables', async () => {
      mockDb.query.mockResolvedValue({ rowCount: 0 });

      await auditLogger.createAuditTables();

      // Verify main audit log table creation
      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('CREATE TABLE IF NOT EXISTS transpiler_audit_log')
      );

      // Verify metrics table creation
      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('CREATE TABLE IF NOT EXISTS transpiler_audit_metrics')
      );

      // Verify indexes creation
      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('CREATE INDEX IF NOT EXISTS')
      );
    });
  });

  describe('Event Logging', () => {
    beforeEach(async () => {
      auditLogger.isInitialized = true;
      mockDb.query.mockResolvedValue({ rowCount: 1 });
    });

    test('should log basic audit event correctly', async () => {
      const eventData = {
        projectId: 'proj-123',
        fileName: 'test.cbl',
        lineCount: 500
      };

      const context = {
        userId: 'user-123',
        customerId: 'customer-456',
        sessionId: 'session-789',
        ipAddress: '192.168.1.100',
        requestId: 'req-abc123'
      };

      await auditLogger.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_START, eventData, context);

      expect(auditLogger.auditQueue).toHaveLength(1);
      
      const loggedEvent = auditLogger.auditQueue[0];
      expect(loggedEvent.event_type).toBe(AUDIT_EVENT_TYPES.TRANSPILE_START);
      expect(loggedEvent.user_id).toBe('user-123');
      expect(loggedEvent.customer_id).toBe('customer-456');
      expect(loggedEvent.event_data).toEqual(eventData);
      expect(loggedEvent.hash_signature).toBeDefined();
    });

    test('should calculate correct risk levels for different event types', async () => {
      // Test critical risk events
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.SECURITY_VIOLATION, {});
      expect(auditLogger.auditQueue[0].risk_level).toBe(RISK_LEVELS.CRITICAL);

      auditLogger.auditQueue = [];

      // Test high risk events
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.FILE_DELETE, {});
      expect(auditLogger.auditQueue[0].risk_level).toBe(RISK_LEVELS.HIGH);

      auditLogger.auditQueue = [];

      // Test medium risk events
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.FILE_UPLOAD, {});
      expect(auditLogger.auditQueue[0].risk_level).toBe(RISK_LEVELS.MEDIUM);

      auditLogger.auditQueue = [];

      // Test low risk events
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.COBOL_VIEW, {});
      expect(auditLogger.auditQueue[0].risk_level).toBe(RISK_LEVELS.LOW);
    });

    test('should classify data sensitivity correctly', async () => {
      // Test restricted data classification
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.PII_ACCESS, {});
      expect(auditLogger.auditQueue[0].data_classification).toBe(DATA_CLASSIFICATIONS.RESTRICTED);

      auditLogger.auditQueue = [];

      // Test confidential data classification
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.AUTH_LOGIN, {});
      expect(auditLogger.auditQueue[0].data_classification).toBe(DATA_CLASSIFICATIONS.CONFIDENTIAL);

      auditLogger.auditQueue = [];

      // Test internal data classification
      await auditLogger.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_START, {});
      expect(auditLogger.auditQueue[0].data_classification).toBe(DATA_CLASSIFICATIONS.INTERNAL);
    });

    test('should sanitize sensitive data in event logs', async () => {
      const sensitiveData = {
        projectName: 'Test Project',
        password: 'secret123',
        token: 'bearer-token-xyz',
        api_key: 'api-key-sensitive',
        normalField: 'normal-value'
      };

      await auditLogger.logEvent(AUDIT_EVENT_TYPES.PROJECT_CREATE, sensitiveData);

      const loggedEvent = auditLogger.auditQueue[0];
      expect(loggedEvent.event_data.password).toBe('[REDACTED]');
      expect(loggedEvent.event_data.token).toBe('[REDACTED]');
      expect(loggedEvent.event_data.api_key).toBe('[REDACTED]');
      expect(loggedEvent.event_data.projectName).toBe('Test Project');
      expect(loggedEvent.event_data.normalField).toBe('normal-value');
    });

    test('should truncate large data fields', async () => {
      const largeData = {
        description: 'x'.repeat(1500),
        normalField: 'normal'
      };

      await auditLogger.logEvent(AUDIT_EVENT_TYPES.PROJECT_CREATE, largeData);

      const loggedEvent = auditLogger.auditQueue[0];
      expect(loggedEvent.event_data.description).toMatch(/\[TRUNCATED\]$/);
      expect(loggedEvent.event_data.description.length).toBeLessThan(1020);
      expect(loggedEvent.event_data.normalField).toBe('normal');
    });

    test('should immediately flush critical events', async () => {
      const flushSpy = jest.spyOn(auditLogger, 'flushAuditQueue').mockResolvedValue();

      await auditLogger.logEvent(AUDIT_EVENT_TYPES.SECURITY_VIOLATION, {
        violationType: 'unauthorized_access',
        resource: 'sensitive-data'
      });

      expect(flushSpy).toHaveBeenCalled();
      flushSpy.mockRestore();
    });
  });

  describe('Cryptographic Integrity', () => {
    test('should create valid hash signatures for audit entries', () => {
      const auditEntry = {
        timestamp: '2025-07-05T10:00:00.000Z',
        event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
        user_id: 'user-123',
        event_data: { projectId: 'proj-123' }
      };

      const signature = auditLogger.createSignature(auditEntry);

      expect(signature).toBeDefined();
      expect(typeof signature).toBe('string');
      expect(signature.length).toBe(64); // SHA-256 hex string length
    });

    test('should verify audit entry signatures correctly', () => {
      const auditEntry = {
        timestamp: '2025-07-05T10:00:00.000Z',
        event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
        user_id: 'user-123',
        event_data: { projectId: 'proj-123' }
      };

      const signature = auditLogger.createSignature(auditEntry);
      auditEntry.hash_signature = signature;

      const isValid = auditLogger.verifySignature(auditEntry);
      expect(isValid).toBe(true);
    });

    test('should detect tampered audit entries', () => {
      const auditEntry = {
        timestamp: '2025-07-05T10:00:00.000Z',
        event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
        user_id: 'user-123',
        event_data: { projectId: 'proj-123' }
      };

      const signature = auditLogger.createSignature(auditEntry);
      
      // Tamper with the entry
      auditEntry.user_id = 'user-456';
      auditEntry.hash_signature = signature;

      const isValid = auditLogger.verifySignature(auditEntry);
      expect(isValid).toBe(false);
    });

    test('should generate unique correlation IDs', () => {
      const id1 = auditLogger.generateCorrelationId();
      const id2 = auditLogger.generateCorrelationId();

      expect(id1).toBeDefined();
      expect(id2).toBeDefined();
      expect(id1).not.toBe(id2);
      expect(id1.length).toBe(32); // 16 bytes = 32 hex chars
    });
  });

  describe('Batch Processing', () => {
    beforeEach(() => {
      auditLogger.isInitialized = true;
    });

    test('should flush audit queue to database', async () => {
      // Add test events to queue
      auditLogger.auditQueue = [
        {
          timestamp: '2025-07-05T10:00:00.000Z',
          event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
          user_id: 'user-123',
          customer_id: 'customer-456',
          event_data: { projectId: 'proj-1' },
          hash_signature: 'test-signature-1'
        },
        {
          timestamp: '2025-07-05T10:01:00.000Z',
          event_type: AUDIT_EVENT_TYPES.TRANSPILE_COMPLETE,
          user_id: 'user-123',
          customer_id: 'customer-456',
          event_data: { projectId: 'proj-1', success: true },
          hash_signature: 'test-signature-2'
        }
      ];

      mockDb.query.mockResolvedValue({ rowCount: 2 });

      await auditLogger.flushAuditQueue();

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('INSERT INTO transpiler_audit_log'),
        expect.any(Array)
      );
      expect(auditLogger.auditQueue).toHaveLength(0);
    });

    test('should handle database errors during flush', async () => {
      auditLogger.auditQueue = [{
        timestamp: '2025-07-05T10:00:00.000Z',
        event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
        user_id: 'user-123',
        hash_signature: 'test-signature'
      }];

      const error = new Error('Database insert failed');
      mockDb.query.mockRejectedValue(error);

      await auditLogger.flushAuditQueue();

      // Events should be re-queued for retry
      expect(auditLogger.auditQueue).toHaveLength(1);
    });
  });

  describe('Query and Reporting', () => {
    beforeEach(() => {
      mockDb.query.mockResolvedValue([
        {
          id: '1',
          timestamp: '2025-07-05T10:00:00.000Z',
          event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
          user_id: 'user-123',
          risk_level: RISK_LEVELS.MEDIUM
        }
      ]);
    });

    test('should query audit logs with filters', async () => {
      const filters = {
        startDate: '2025-07-01',
        endDate: '2025-07-31',
        userId: 'user-123',
        eventType: AUDIT_EVENT_TYPES.TRANSPILE_START,
        riskLevel: RISK_LEVELS.HIGH,
        limit: 100
      };

      await auditLogger.queryAuditLogs(filters);

      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('WHERE 1=1'),
        expect.arrayContaining(['2025-07-01', '2025-07-31', 'user-123'])
      );
    });

    test('should generate SOC2 compliance report', async () => {
      mockDb.query.mockResolvedValue([
        {
          event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
          risk_level: RISK_LEVELS.MEDIUM,
          data_classification: DATA_CLASSIFICATIONS.INTERNAL,
          event_count: '50',
          unique_users: '10',
          failed_events: '2'
        },
        {
          event_type: AUDIT_EVENT_TYPES.AUTH_LOGIN,
          risk_level: RISK_LEVELS.LOW,
          data_classification: DATA_CLASSIFICATIONS.CONFIDENTIAL,
          event_count: '100',
          unique_users: '25',
          failed_events: '5'
        }
      ]);

      const report = await auditLogger.generateComplianceReport(
        '2025-07-01',
        '2025-07-31',
        'customer-123'
      );

      expect(report).toHaveProperty('summary');
      expect(report).toHaveProperty('details');
      expect(report).toHaveProperty('generatedAt');
      expect(report).toHaveProperty('compliance');

      expect(report.summary.totalEvents).toBe(150);
      expect(report.summary.failureRate).toBe((7/150) * 100);
      expect(report.compliance.soc2TypeII).toBe(true);
    });

    test('should verify audit log integrity', async () => {
      const testEntry = {
        id: '1',
        correlation_id: 'corr-123',
        timestamp: '2025-07-05T10:00:00.000Z',
        event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
        user_id: 'user-123',
        event_data: { projectId: 'proj-123' },
        hash_signature: auditLogger.createSignature({
          timestamp: '2025-07-05T10:00:00.000Z',
          event_type: AUDIT_EVENT_TYPES.TRANSPILE_START,
          user_id: 'user-123',
          event_data: { projectId: 'proj-123' }
        })
      };

      mockDb.query.mockResolvedValue([testEntry]);

      const result = await auditLogger.verifyIntegrity('corr-123');

      expect(result.valid).toBe(true);
      expect(result.entry).toBeDefined();
      expect(result.verifiedAt).toBeDefined();
    });
  });

  describe('Retention Management', () => {
    test('should calculate correct retention dates', () => {
      // Security events should have 7-year retention
      const securityRetention = auditLogger.calculateRetentionDate(AUDIT_EVENT_TYPES.SECURITY_VIOLATION);
      const expectedSecurityYear = new Date().getFullYear() + 7;
      expect(securityRetention.getFullYear()).toBe(expectedSecurityYear);

      // Auth events should have 7-year retention
      const authRetention = auditLogger.calculateRetentionDate(AUDIT_EVENT_TYPES.AUTH_LOGIN);
      const expectedAuthYear = new Date().getFullYear() + 7;
      expect(authRetention.getFullYear()).toBe(expectedAuthYear);

      // Operational events should have 3-year retention
      const operationalRetention = auditLogger.calculateRetentionDate(AUDIT_EVENT_TYPES.TRANSPILE_START);
      const expectedOperationalYear = new Date().getFullYear() + 3;
      expect(operationalRetention.getFullYear()).toBe(expectedOperationalYear);
    });

    test('should clean up expired audit logs', async () => {
      mockDb.query.mockResolvedValue({ rowCount: 25 });

      const deletedCount = await auditLogger.cleanupExpiredLogs();

      expect(mockDb.query).toHaveBeenCalledWith(
        'DELETE FROM transpiler_audit_log WHERE retention_date < CURRENT_DATE'
      );
      expect(deletedCount).toBe(25);
    });
  });

  describe('Event Listeners', () => {
    test('should automatically log transpiler events', (done) => {
      auditLogger.isInitialized = true;
      
      const logEventSpy = jest.spyOn(auditLogger, 'logEvent').mockResolvedValue();

      const eventData = {
        projectId: 'proj-123',
        context: {
          userId: 'user-123',
          customerId: 'customer-456'
        }
      };

      // Simulate transpiler event
      process.emit('transpiler:start', eventData);

      // Give event loop time to process
      setTimeout(() => {
        expect(logEventSpy).toHaveBeenCalledWith(
          AUDIT_EVENT_TYPES.TRANSPILE_START,
          eventData,
          eventData.context
        );
        
        logEventSpy.mockRestore();
        done();
      }, 10);
    });

    test('should automatically log security events', (done) => {
      auditLogger.isInitialized = true;
      
      const logEventSpy = jest.spyOn(auditLogger, 'logEvent').mockResolvedValue();

      const securityData = {
        violationType: 'unauthorized_access',
        context: {
          userId: 'user-123',
          ipAddress: '192.168.1.100'
        }
      };

      // Simulate security event
      process.emit('security:violation', securityData);

      setTimeout(() => {
        expect(logEventSpy).toHaveBeenCalledWith(
          AUDIT_EVENT_TYPES.SECURITY_VIOLATION,
          securityData,
          expect.objectContaining({
            ...securityData.context,
            result: 'violation'
          })
        );
        
        logEventSpy.mockRestore();
        done();
      }, 10);
    });
  });
});