/**
 * TCS BaNCS Webhook Handler Test Suite
 * Tests real-time webhook processing and event handling
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 3 - Advanced Enterprise Features
 */

const request = require('supertest');
const crypto = require('crypto');
const { BaNCSWebhookHandler, WEBHOOK_EVENTS, BANCS_STATUS_MAPPING } = require('../../../src/connectors/tcs-bancs/bancs-webhook-handler');

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

describe('BaNCS Webhook Handler', () => {
  let webhookHandler;
  let testConfig;

  beforeEach(() => {
    testConfig = {
      port: 0, // Use random available port for testing
      path: '/test-webhooks/bancs',
      webhookSecret: 'test-secret-key',
      enableSignatureVerification: true,
      enableRequestLogging: true,
      enableRateLimit: false, // Disable for testing
      maxPayloadSize: '1mb'
    };

    webhookHandler = new BaNCSWebhookHandler(testConfig);
  });

  afterEach(async () => {
    if (webhookHandler.server) {
      await webhookHandler.stop();
    }
  });

  describe('Initialization', () => {
    test('should initialize with default configuration', () => {
      const defaultHandler = new BaNCSWebhookHandler();
      
      expect(defaultHandler.config.port).toBe(3001);
      expect(defaultHandler.config.path).toBe('/webhooks/bancs');
      expect(defaultHandler.config.enableSignatureVerification).toBe(true);
      expect(defaultHandler.config.maxPayloadSize).toBe('10mb');
    });

    test('should initialize with custom configuration', () => {
      expect(webhookHandler.config.port).toBe(0);
      expect(webhookHandler.config.path).toBe('/test-webhooks/bancs');
      expect(webhookHandler.config.webhookSecret).toBe('test-secret-key');
    });

    test('should setup Express app with middleware', () => {
      expect(webhookHandler.app).toBeDefined();
      expect(webhookHandler.processingStats).toMatchObject({
        totalReceived: 0,
        successfullyProcessed: 0,
        failed: 0,
        duplicatesDetected: 0,
        invalidSignatures: 0,
        averageProcessingTime: 0
      });
    });
  });

  describe('Webhook Signature Verification', () => {
    test('should verify valid webhook signature', () => {
      const payload = JSON.stringify({ test: 'data' });
      const signature = crypto
        .createHmac('sha256', testConfig.webhookSecret)
        .update(payload)
        .digest('hex');

      const mockReq = {
        get: jest.fn((header) => {
          if (header === 'X-BaNCS-Signature') return `sha256=${signature}`;
          return null;
        }),
        rawBody: Buffer.from(payload)
      };

      const isValid = webhookHandler.verifyWebhookSignature(mockReq);
      expect(isValid).toBe(true);
    });

    test('should reject invalid webhook signature', () => {
      const payload = JSON.stringify({ test: 'data' });
      const invalidSignature = 'invalid-signature';

      const mockReq = {
        get: jest.fn((header) => {
          if (header === 'X-BaNCS-Signature') return `sha256=${invalidSignature}`;
          return null;
        }),
        rawBody: Buffer.from(payload)
      };

      const isValid = webhookHandler.verifyWebhookSignature(mockReq);
      expect(isValid).toBe(false);
    });

    test('should handle missing signature header', () => {
      const payload = JSON.stringify({ test: 'data' });

      const mockReq = {
        get: jest.fn(() => null),
        rawBody: Buffer.from(payload)
      };

      const isValid = webhookHandler.verifyWebhookSignature(mockReq);
      expect(isValid).toBe(false);
    });

    test('should skip verification when secret not configured', () => {
      const handlerWithoutSecret = new BaNCSWebhookHandler({
        webhookSecret: null,
        enableSignatureVerification: true
      });

      const mockReq = {
        get: jest.fn(() => null),
        rawBody: Buffer.from('test')
      };

      const isValid = handlerWithoutSecret.verifyWebhookSignature(mockReq);
      expect(isValid).toBe(true);
    });
  });

  describe('Webhook Payload Processing', () => {
    test('should parse valid webhook payload', () => {
      const validPayload = {
        id: 'webhook-123',
        event_type: 'transaction.status.changed',
        timestamp: '2023-12-01T10:00:00Z',
        data: { transaction_id: 'TXN001' },
        metadata: { source: 'bancs' }
      };

      const parsed = webhookHandler.parseWebhookPayload(validPayload);
      
      expect(parsed).toEqual({
        id: 'webhook-123',
        event_type: 'transaction.status.changed',
        timestamp: '2023-12-01T10:00:00Z',
        data: { transaction_id: 'TXN001' },
        metadata: { source: 'bancs' },
        version: '1.0'
      });
    });

    test('should reject invalid webhook payload', () => {
      const invalidPayload = {
        // Missing required fields
        data: { test: 'data' }
      };

      const parsed = webhookHandler.parseWebhookPayload(invalidPayload);
      expect(parsed).toBeNull();
    });

    test('should handle missing optional fields', () => {
      const minimalPayload = {
        id: 'webhook-123',
        event_type: 'transaction.status.changed',
        timestamp: '2023-12-01T10:00:00Z'
      };

      const parsed = webhookHandler.parseWebhookPayload(minimalPayload);
      
      expect(parsed).toEqual({
        id: 'webhook-123',
        event_type: 'transaction.status.changed',
        timestamp: '2023-12-01T10:00:00Z',
        data: {},
        metadata: {},
        version: '1.0'
      });
    });
  });

  describe('Duplicate Detection', () => {
    test('should detect duplicate webhooks', () => {
      const webhookData = {
        id: 'webhook-duplicate-test',
        event_type: 'test.event'
      };

      const firstCheck = webhookHandler.checkDuplicate(webhookData);
      expect(firstCheck).toBe(false);

      const secondCheck = webhookHandler.checkDuplicate(webhookData);
      expect(secondCheck).toBe(true);
    });

    test('should clean expired duplicate entries', () => {
      const webhookData = {
        id: 'webhook-expire-test',
        event_type: 'test.event'
      };

      // Set a very short expiry for testing
      webhookHandler.duplicateCheckExpiry = 100; // 100ms

      const firstCheck = webhookHandler.checkDuplicate(webhookData);
      expect(firstCheck).toBe(false);

      // Wait for expiry
      return new Promise(resolve => {
        setTimeout(() => {
          const secondCheck = webhookHandler.checkDuplicate(webhookData);
          expect(secondCheck).toBe(false); // Should not be duplicate after expiry
          resolve();
        }, 150);
      });
    });
  });

  describe('Rate Limiting', () => {
    test('should enforce rate limits when enabled', async () => {
      const rateLimitedHandler = new BaNCSWebhookHandler({
        ...testConfig,
        enableRateLimit: true,
        rateLimitMax: 2,
        rateLimitWindow: 1000
      });

      await rateLimitedHandler.start();
      const app = rateLimitedHandler.app;

      // First request should succeed
      await request(app)
        .post(testConfig.path)
        .send({ id: 'test1', event_type: 'test', timestamp: '2023-12-01T10:00:00Z' })
        .expect(401); // Will fail signature but should pass rate limit

      // Second request should succeed
      await request(app)
        .post(testConfig.path)
        .send({ id: 'test2', event_type: 'test', timestamp: '2023-12-01T10:00:00Z' })
        .expect(401);

      // Third request should be rate limited
      await request(app)
        .post(testConfig.path)
        .send({ id: 'test3', event_type: 'test', timestamp: '2023-12-01T10:00:00Z' })
        .expect(429);

      await rateLimitedHandler.stop();
    });
  });

  describe('Webhook Event Processing', () => {
    let eventSpy;

    beforeEach(() => {
      eventSpy = jest.fn();
      webhookHandler.on('transaction_status_changed', eventSpy);
    });

    test('should process transaction status change webhook', async () => {
      const webhookData = {
        id: 'webhook-txn-status',
        event_type: WEBHOOK_EVENTS.TRANSACTION_STATUS_CHANGED,
        data: {
          transaction_id: 'TXN001',
          old_status: 'PROCESSING',
          new_status: 'COMPLETED',
          status_reason: 'Transaction successful',
          updated_at: '2023-12-01T10:00:00Z'
        }
      };

      await webhookHandler.processWebhook(webhookData, 'test-request-id');

      expect(eventSpy).toHaveBeenCalledWith({
        transactionId: 'TXN001',
        oldStatus: 'processing',
        newStatus: 'confirmed',
        reason: 'Transaction successful',
        timestamp: '2023-12-01T10:00:00Z',
        requestId: 'test-request-id',
        source: 'bancs_webhook'
      });
    });

    test('should process account balance update webhook', async () => {
      const balanceEventSpy = jest.fn();
      webhookHandler.on('account_balance_updated', balanceEventSpy);

      const webhookData = {
        id: 'webhook-balance',
        event_type: WEBHOOK_EVENTS.ACCOUNT_BALANCE_UPDATED,
        data: {
          account_number: '1234567890',
          old_balance: 10000,
          new_balance: 15000,
          currency: 'USD',
          update_type: 'CREDIT',
          updated_at: '2023-12-01T10:00:00Z'
        }
      };

      await webhookHandler.processWebhook(webhookData, 'test-request-id');

      expect(balanceEventSpy).toHaveBeenCalledWith({
        accountNumber: '1234567890',
        oldBalance: 10000,
        newBalance: 15000,
        currency: 'USD',
        updateType: 'CREDIT',
        timestamp: '2023-12-01T10:00:00Z',
        requestId: 'test-request-id'
      });
    });

    test('should process compliance status change webhook', async () => {
      const complianceEventSpy = jest.fn();
      webhookHandler.on('compliance_status_changed', complianceEventSpy);

      const webhookData = {
        id: 'webhook-compliance',
        event_type: WEBHOOK_EVENTS.COMPLIANCE_STATUS_CHANGED,
        data: {
          transaction_id: 'TXN002',
          compliance_type: 'AML',
          old_status: 'PENDING',
          new_status: 'APPROVED',
          risk_score: 25,
          manual_review_required: false,
          updated_at: '2023-12-01T10:00:00Z'
        }
      };

      await webhookHandler.processWebhook(webhookData, 'test-request-id');

      expect(complianceEventSpy).toHaveBeenCalledWith({
        transactionId: 'TXN002',
        complianceType: 'AML',
        oldStatus: 'PENDING',
        newStatus: 'APPROVED',
        riskScore: 25,
        manualReviewRequired: false,
        timestamp: '2023-12-01T10:00:00Z',
        requestId: 'test-request-id'
      });
    });

    test('should process fraud alert webhook', async () => {
      const fraudEventSpy = jest.fn();
      webhookHandler.on('fraud_alert_triggered', fraudEventSpy);

      const webhookData = {
        id: 'webhook-fraud',
        event_type: WEBHOOK_EVENTS.FRAUD_ALERT_TRIGGERED,
        data: {
          alert_id: 'ALERT001',
          transaction_id: 'TXN003',
          alert_type: 'SUSPICIOUS_PATTERN',
          severity: 'HIGH',
          risk_indicators: ['VELOCITY', 'LOCATION'],
          recommended_action: 'BLOCK_TRANSACTION',
          triggered_at: '2023-12-01T10:00:00Z'
        }
      };

      await webhookHandler.processWebhook(webhookData, 'test-request-id');

      expect(fraudEventSpy).toHaveBeenCalledWith({
        alertId: 'ALERT001',
        transactionId: 'TXN003',
        alertType: 'SUSPICIOUS_PATTERN',
        severity: 'HIGH',
        riskIndicators: ['VELOCITY', 'LOCATION'],
        recommendedAction: 'BLOCK_TRANSACTION',
        timestamp: '2023-12-01T10:00:00Z',
        requestId: 'test-request-id'
      });
    });

    test('should handle unknown event types', async () => {
      const unknownEventSpy = jest.fn();
      webhookHandler.on('unknown_webhook_event', unknownEventSpy);

      const webhookData = {
        id: 'webhook-unknown',
        event_type: 'unknown.event.type',
        data: { test: 'data' }
      };

      await webhookHandler.processWebhook(webhookData, 'test-request-id');

      expect(unknownEventSpy).toHaveBeenCalledWith({
        event_type: 'unknown.event.type',
        data: { test: 'data' },
        requestId: 'test-request-id',
        timestamp: expect.any(String)
      });
    });
  });

  describe('HTTP Endpoints', () => {
    beforeEach(async () => {
      await webhookHandler.start();
    });

    test('should handle valid webhook with correct signature', async () => {
      const payload = {
        id: 'webhook-valid',
        event_type: WEBHOOK_EVENTS.TRANSACTION_STATUS_CHANGED,
        timestamp: '2023-12-01T10:00:00Z',
        data: {
          transaction_id: 'TXN001',
          old_status: 'PROCESSING',
          new_status: 'COMPLETED'
        }
      };

      const payloadString = JSON.stringify(payload);
      const signature = crypto
        .createHmac('sha256', testConfig.webhookSecret)
        .update(payloadString)
        .digest('hex');

      const response = await request(webhookHandler.app)
        .post(testConfig.path)
        .set('X-BaNCS-Signature', `sha256=${signature}`)
        .send(payload)
        .expect(200);

      expect(response.body).toMatchObject({
        status: 'success',
        message: 'Webhook processed'
      });

      expect(webhookHandler.processingStats.successfullyProcessed).toBe(1);
    });

    test('should reject webhook with invalid signature', async () => {
      const payload = {
        id: 'webhook-invalid-sig',
        event_type: WEBHOOK_EVENTS.TRANSACTION_STATUS_CHANGED,
        timestamp: '2023-12-01T10:00:00Z',
        data: { transaction_id: 'TXN001' }
      };

      await request(webhookHandler.app)
        .post(testConfig.path)
        .set('X-BaNCS-Signature', 'sha256=invalid-signature')
        .send(payload)
        .expect(401);

      expect(webhookHandler.processingStats.invalidSignatures).toBe(1);
    });

    test('should handle duplicate webhooks gracefully', async () => {
      const payload = {
        id: 'webhook-duplicate',
        event_type: WEBHOOK_EVENTS.TRANSACTION_STATUS_CHANGED,
        timestamp: '2023-12-01T10:00:00Z',
        data: { transaction_id: 'TXN001' }
      };

      const payloadString = JSON.stringify(payload);
      const signature = crypto
        .createHmac('sha256', testConfig.webhookSecret)
        .update(payloadString)
        .digest('hex');

      // First request
      await request(webhookHandler.app)
        .post(testConfig.path)
        .set('X-BaNCS-Signature', `sha256=${signature}`)
        .send(payload)
        .expect(200);

      // Duplicate request
      const response = await request(webhookHandler.app)
        .post(testConfig.path)
        .set('X-BaNCS-Signature', `sha256=${signature}`)
        .send(payload)
        .expect(200);

      expect(response.body).toMatchObject({
        status: 'duplicate',
        message: 'Already processed'
      });

      expect(webhookHandler.processingStats.duplicatesDetected).toBe(1);
    });

    test('should provide health check endpoint', async () => {
      const response = await request(webhookHandler.app)
        .get('/health')
        .expect(200);

      expect(response.body).toMatchObject({
        status: 'healthy',
        service: 'bancs-webhook-handler',
        uptime: expect.any(Number),
        timestamp: expect.any(String),
        stats: expect.objectContaining({
          totalReceived: expect.any(Number),
          successfullyProcessed: expect.any(Number)
        })
      });
    });

    test('should provide metrics endpoint', async () => {
      const response = await request(webhookHandler.app)
        .get('/metrics')
        .expect(200);

      expect(response.body).toMatchObject({
        webhook_stats: expect.objectContaining({
          totalReceived: expect.any(Number),
          successfullyProcessed: expect.any(Number),
          failed: expect.any(Number)
        }),
        queue_size: expect.any(Number),
        processed_cache_size: expect.any(Number),
        rate_limit_counters: expect.any(Number)
      });
    });

    test('should handle malformed JSON payload', async () => {
      await request(webhookHandler.app)
        .post(testConfig.path)
        .set('Content-Type', 'application/json')
        .send('{ invalid json }')
        .expect(400);
    });
  });

  describe('Server Management', () => {
    test('should start and stop server successfully', async () => {
      const testHandler = new BaNCSWebhookHandler({
        port: 0, // Random port
        enableRateLimit: false
      });

      await testHandler.start();
      expect(testHandler.server).toBeDefined();

      await testHandler.stop();
    });

    test('should handle server start errors', async () => {
      // Try to start on an invalid port
      const badHandler = new BaNCSWebhookHandler({
        port: -1
      });

      await expect(badHandler.start()).rejects.toThrow();
    });
  });

  describe('Statistics and Monitoring', () => {
    test('should track processing statistics correctly', () => {
      webhookHandler.processingStats.totalReceived = 10;
      webhookHandler.processingStats.successfullyProcessed = 8;
      webhookHandler.processingStats.failed = 2;

      const stats = webhookHandler.getProcessingStats();

      expect(stats).toMatchObject({
        totalReceived: 10,
        successfullyProcessed: 8,
        failed: 2,
        successRate: 0.8,
        queueSize: 0,
        processedCacheSize: 0
      });
    });

    test('should clear statistics', () => {
      webhookHandler.processingStats.totalReceived = 10;
      webhookHandler.processedWebhooks.set('test', Date.now());

      webhookHandler.clearStats();

      expect(webhookHandler.processingStats.totalReceived).toBe(0);
      expect(webhookHandler.processedWebhooks.size).toBe(0);
    });
  });

  describe('Constants and Exports', () => {
    test('should export webhook events correctly', () => {
      expect(WEBHOOK_EVENTS).toMatchObject({
        TRANSACTION_STATUS_CHANGED: 'transaction.status.changed',
        ACCOUNT_BALANCE_UPDATED: 'account.balance.updated',
        COMPLIANCE_STATUS_CHANGED: 'compliance.status.changed',
        FRAUD_ALERT_TRIGGERED: 'fraud.alert.triggered'
      });
    });

    test('should export status mapping correctly', () => {
      expect(BANCS_STATUS_MAPPING).toMatchObject({
        'INITIATED': 'pending',
        'PROCESSING': 'processing',
        'COMPLETED': 'confirmed',
        'FAILED': 'failed',
        'REJECTED': 'rejected'
      });
    });
  });
});