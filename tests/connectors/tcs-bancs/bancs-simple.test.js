/**
 * TCS BaNCS Simple Test Suite
 * Basic functionality tests for TCS BaNCS integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * TCS Partnership Module
 */

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

// Mock axios
const mockAxios = {
  create: jest.fn(() => ({
    defaults: {
      baseURL: 'https://test-bancs-api.tcs.com',
      timeout: 5000,
      headers: {}
    },
    interceptors: {
      request: { use: jest.fn() },
      response: { use: jest.fn() }
    },
    get: jest.fn(),
    post: jest.fn()
  })),
  post: jest.fn()
};

jest.mock('axios', () => mockAxios);

const { TCSBaNCSConnector, BANCS_ENDPOINTS, STATUS_MAPPING } = require('../../../src/connectors/tcs-bancs/bancs-connector');
const { TCSBaNCSIntegrationService, PREPROCESSING_STAGES, INTEGRATION_EVENTS } = require('../../../src/connectors/tcs-bancs/bancs-integration-service');

describe('TCS BaNCS Simple Tests', () => {
  
  describe('TCS BaNCS Connector', () => {
    test('should initialize connector with config', () => {
      const config = {
        baseUrl: 'https://test-api.com',
        clientId: 'test-client',
        bankCode: 'TESTBANK'
      };
      
      const connector = new TCSBaNCSConnector(config);
      
      expect(connector.config.baseUrl).toBe('https://test-api.com');
      expect(connector.config.clientId).toBe('test-client');
      expect(connector.config.bankCode).toBe('TESTBANK');
    });

    test('should validate required transaction fields', () => {
      const connector = new TCSBaNCSConnector();
      
      const validTransaction = {
        id: 'TXN001',
        amount: 1000,
        currency: 'USD',
        sender: { account: '123456' },
        receiver: { account: '789012' }
      };
      
      expect(() => connector.validateTransactionInput(validTransaction)).not.toThrow();
    });

    test('should reject invalid transaction', () => {
      const connector = new TCSBaNCSConnector();
      
      const invalidTransaction = {
        id: 'TXN001'
        // Missing required fields
      };
      
      expect(() => connector.validateTransactionInput(invalidTransaction)).toThrow();
    });

    test('should transform balance response correctly', () => {
      const connector = new TCSBaNCSConnector();
      
      const mockResponse = {
        account_number: '123456',
        currency: 'USD',
        current_balance: 1000,
        available_balance: 900
      };
      
      const transformed = connector.transformBalanceResponse(mockResponse);
      
      expect(transformed.accountNumber).toBe('123456');
      expect(transformed.currency).toBe('USD');
      expect(transformed.currentBalance).toBe(1000);
      expect(transformed.availableBalance).toBe(900);
    });

    test('should export correct constants', () => {
      expect(BANCS_ENDPOINTS.ACCOUNT_INQUIRY).toBe('/api/v1/accounts/inquiry');
      expect(BANCS_ENDPOINTS.BALANCE_CHECK).toBe('/api/v1/accounts/balance');
      expect(STATUS_MAPPING.COMPLETED).toBe('confirmed');
    });
  });

  describe('TCS BaNCS Integration Service', () => {
    test('should initialize integration service', () => {
      const config = {
        bankCode: 'TESTBANK',
        enablePreprocessing: true
      };
      
      const service = new TCSBaNCSIntegrationService(config);
      
      expect(service.config.bankCode).toBe('TESTBANK');
      expect(service.config.enablePreprocessing).toBe(true);
    });

    test('should validate transaction input', async () => {
      const service = new TCSBaNCSIntegrationService();
      
      const validTransaction = {
        id: 'TXN001',
        messageType: 'MT103',
        amount: 1000,
        currency: 'USD',
        sender: { account: '123456', name: 'John Doe' },
        receiver: { account: '789012', name: 'Jane Smith' }
      };
      
      const result = await service.performInputValidation(validTransaction, {});
      
      expect(result.passed).toBe(true);
      expect(result.errors).toHaveLength(0);
      expect(result.validatedFields.transactionId).toBe(true);
    });

    test('should detect validation errors', async () => {
      const service = new TCSBaNCSIntegrationService();
      
      const invalidTransaction = {
        // Missing required fields
        currency: 'USD'
      };
      
      const result = await service.performInputValidation(invalidTransaction, {});
      
      expect(result.passed).toBe(false);
      expect(result.errors.length).toBeGreaterThan(0);
    });

    test('should calculate risk profile', () => {
      const service = new TCSBaNCSIntegrationService();
      
      const highValueTransaction = {
        amount: 150000, // > 100K threshold
        receiver: { bic: 'DEUTDEFF' } // Cross-border
      };
      
      const enrichments = {
        customer: { riskRating: 'HIGH' }
      };
      
      const riskProfile = service.calculateRiskProfile(highValueTransaction, enrichments);
      
      expect(riskProfile.level).toBe('high');
      expect(riskProfile.factors).toContain('high_value');
      expect(riskProfile.factors).toContain('cross_border');
      expect(riskProfile.score).toBeGreaterThan(80);
    });

    test('should identify internal accounts', () => {
      const service = new TCSBaNCSIntegrationService({ bankCode: 'TESTBANK' });
      
      expect(service.isInternalAccount('TESTBANK1234567890')).toBe(true);
      expect(service.isInternalAccount('123456789012')).toBe(true); // 12 digits
      expect(service.isInternalAccount('OTHERBANK1234567890')).toBe(false);
    });

    test('should calculate urgency levels', () => {
      const service = new TCSBaNCSIntegrationService();
      
      const highValueMT103 = { messageType: 'MT103', amount: 150000 };
      const institutionalMT202 = { messageType: 'MT202', amount: 50000 };
      const normalTransaction = { messageType: 'MT103', amount: 25000 };
      
      expect(service.calculateUrgency(highValueMT103, {})).toBe('high');
      expect(service.calculateUrgency(institutionalMT202, {})).toBe('institutional');
      expect(service.calculateUrgency(normalTransaction, {})).toBe('normal');
    });

    test('should provide metrics', () => {
      const service = new TCSBaNCSIntegrationService();
      
      const metrics = service.getMetrics();
      
      expect(metrics).toHaveProperty('processing');
      expect(metrics).toHaveProperty('cache');
      expect(metrics).toHaveProperty('queues');
      expect(metrics).toHaveProperty('active');
      expect(metrics.processing.totalProcessed).toBe(0);
    });

    test('should export correct constants', () => {
      expect(PREPROCESSING_STAGES.VALIDATION).toBe('validation');
      expect(PREPROCESSING_STAGES.COMPLIANCE).toBe('compliance');
      expect(INTEGRATION_EVENTS.TRANSACTION_VALIDATED).toBe('transaction_validated');
    });
  });

  describe('Integration Workflow', () => {
    test('should prepare routing data structure', async () => {
      const service = new TCSBaNCSIntegrationService({ 
        bankCode: 'TESTBANK',
        branchCode: 'MAIN',
        baseURL: 'https://test-bancs-api.tcs.com',
        apiKey: 'test-key',
        enableCaching: false
      });
      
      const transaction = {
        id: 'TXN001',
        amount: 75000,
        currency: 'USD',
        messageType: 'MT103',
        receiver: {
          bic: 'BANKGB22XXX'
        }
      };
      
      const preprocessingResult = {
        processingId: 'PROC001',
        status: 'validated',
        stages: {
          accountVerification: {
            passed: true,
            verifications: {
              senderBalance: { availableBalance: 100000 }
            }
          },
          compliance: { passed: true, requiresManualReview: false }
        }
      };
      
      const result = await service.prepareForRouting(transaction, preprocessingResult);
      
      expect(result.success).toBe(true);
      expect(result.routingData).toHaveProperty('enhancedTransaction');
      expect(result.routingData).toHaveProperty('routingHints');
      expect(result.routingData).toHaveProperty('riskAssessment');
      expect(result.routingData.enhancedTransaction.preprocessingId).toBe('PROC001');
    });

    test('should generate routing recommendations', () => {
      const service = new TCSBaNCSIntegrationService();
      
      const routingData = {
        enhancedTransaction: { amount: 75000 },
        routingHints: { costSensitivity: 'high' },
        riskAssessment: { 
          level: 'low',
          requiresManualReview: false 
        }
      };
      
      const recommendations = service.generateRoutingRecommendations(routingData);
      
      expect(recommendations).toBeInstanceOf(Array);
      expect(recommendations.length).toBeGreaterThan(0);
      
      // Should recommend Corda for high-value, low-risk transactions
      const cordaRec = recommendations.find(r => r.message.includes('Corda'));
      expect(cordaRec).toBeDefined();
      expect(cordaRec.priority).toBe('high');
    });

    test('should extract compliance flags', () => {
      const service = new TCSBaNCSIntegrationService();
      
      const complianceResult = {
        aml: { required: true },
        fatf: { required: false },
        sanctions: { passed: true },
        requiresManualReview: true,
        riskScore: 75
      };
      
      const flags = service.extractComplianceFlags(complianceResult);
      
      expect(flags).toMatchObject({
        amlRequired: true,
        fatfReporting: false,
        sanctionsCheck: true,
        manualReview: true,
        riskScore: 75
      });
    });
  });
});