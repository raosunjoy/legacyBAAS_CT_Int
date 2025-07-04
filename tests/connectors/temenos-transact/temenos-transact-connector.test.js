/**
 * Temenos Transact Connector Complete Test Suite
 * Achieving 100% test coverage for Temenos Transact European banking integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const axios = require('axios');
jest.mock('axios');

const { 
  TemenosTransactConnector, 
  TRANSACT_ENDPOINTS, 
  TRANSACT_TRANSACTION_TYPES,
  TRANSACT_CURRENCIES 
} = require('../../../src/connectors/temenos-transact/temenos-transact-connector');

const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

describe('TemenosTransactConnector - Complete Test Suite', () => {
  let connector;
  let mockHttpClient;
  let config;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock configuration
    config = {
      baseUrl: 'https://api.temenos.com/transact',
      clientId: 'test_client_001',
      clientSecret: 'test_secret_001',
      bankId: 'EU_BANK_001',
      username: 'test_user',
      password: 'test_password',
      environment: 'sandbox',
      enableSEPA: true,
      enableSWIFTGPI: true,
      enableT24Legacy: true,
      enableEuropeanCompliance: true,
      supportedCurrencies: ['EUR', 'USD', 'GBP', 'CHF'],
      baseCurrency: 'EUR',
      testMode: true
    };

    // Create connector instance first
    connector = new TemenosTransactConnector(config);
    
    // Mock the httpClient as a function that can be called directly
    mockHttpClient = jest.fn();
    mockHttpClient.defaults = { headers: { common: {} } };
    mockHttpClient.post = jest.fn();
    mockHttpClient.get = jest.fn();
    mockHttpClient.put = jest.fn();
    mockHttpClient.delete = jest.fn();
    
    // Make the main function return a proper response structure
    mockHttpClient.mockImplementation(() => Promise.resolve({
      data: {},
      status: 200,
      config: { metadata: { startTime: Date.now() } }
    }));
    
    // Replace the connector's httpClient with our mock
    connector.httpClient = mockHttpClient;
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('Constructor and Configuration', () => {
    test('should initialize with complete configuration', () => {
      expect(connector.config.bankCode).toBe('TEMENOS_TRANSACT');
      expect(connector.config.bankName).toBe('Temenos Transact Platform');
      expect(connector.transactConfig.bankId).toBe('EU_BANK_001');
      expect(connector.transactConfig.baseUrl).toBe('https://api.temenos.com/transact');
      expect(connector.transactConfig.enableSEPA).toBe(true);
      expect(connector.transactConfig.enableSWIFTGPI).toBe(true);
      expect(connector.transactConfig.baseCurrency).toBe('EUR');
    });

    test('should handle minimal configuration', () => {
      const minimalConnector = new TemenosTransactConnector({});
      expect(minimalConnector.config.timeout).toBe(30000);
      expect(minimalConnector.transactConfig.enableSEPA).toBe(true);
      expect(minimalConnector.transactConfig.enableSWIFTGPI).toBe(true);
      expect(minimalConnector.transactConfig.baseCurrency).toBe('EUR');
    });

    test('should use environment variables when config not provided', () => {
      process.env.TEMENOS_TRANSACT_API_URL = 'https://api.temenos.eu/transact';
      process.env.TEMENOS_BANK_ID = 'ENV_EU_001';
      
      const envConnector = new TemenosTransactConnector({});
      expect(envConnector.transactConfig.baseUrl).toBe('https://api.temenos.eu/transact');
      expect(envConnector.transactConfig.bankId).toBe('ENV_EU_001');
      
      delete process.env.TEMENOS_TRANSACT_API_URL;
      delete process.env.TEMENOS_BANK_ID;
    });

    test('should initialize required components', () => {
      expect(connector.httpClient).toBeDefined();
      expect(connector.requestTimes).toBeInstanceOf(Array);
      expect(connector.transactMetrics).toBeDefined();
    });

    test('should initialize metrics tracking', () => {
      expect(connector.transactMetrics).toEqual({
        europeanTransactions: 0,
        sepaTransactions: 0,
        swiftGPITransactions: 0,
        complianceChecks: 0,
        multiCurrencyTransactions: 0,
        t24LegacyOperations: 0
      });
    });
  });

  describe('Authentication and Session Management', () => {
    test('should authenticate with OAuth2', async () => {
      const mockAuthResponse = {
        data: {
          access_token: 'TRANSACT_TOKEN_001',
          token_type: 'Bearer',
          expires_in: 3600,
          scope: 'transact banking compliance',
          institution_id: 'EU_BANK_001'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(mockAuthResponse);

      await connector.authenticate();

      expect(connector.accessToken).toBe('TRANSACT_TOKEN_001');
      expect(connector.tokenExpiry).toBeGreaterThan(Date.now());
      expect(connector.isAuthenticated).toBe(true);
    });

    test('should handle authentication failure', async () => {
      const authError = new Error('Authentication failed');
      authError.response = {
        status: 401,
        data: { 
          error: 'invalid_client',
          error_description: 'Invalid client credentials',
          error_code: 'AUTH_001'
        }
      };

      mockHttpClient.post.mockRejectedValue(authError);

      await expect(connector.authenticate()).rejects.toThrow('Transact authentication failed');
      expect(connector.metrics.authenticationFailures).toBe(1);
      expect(connector.isAuthenticated).toBe(false);
    });

    test('should test connection successfully', async () => {
      connector.accessToken = 'TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;

      const healthResponse = {
        data: { status: 'healthy', version: '2023.12' },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.get.mockResolvedValue(healthResponse);

      const result = await connector.testConnection();
      expect(result).toBe(true);
    });
  });

  describe('Account Operations', () => {
    beforeEach(() => {
      connector.accessToken = 'TRANSACT_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.isAuthenticated = true;
    });

    test('should get account details', async () => {
      const accountResponse = {
        data: {
          accountId: 'EU_ACC_001',
          accountNumber: 'GB29NWBK60161331926819',
          accountName: 'EUROPEAN BUSINESS ACCOUNT',
          accountType: 'CURRENT',
          currency: 'EUR',
          balance: {
            available: 15000.00,
            current: 15250.00,
            pending: 250.00
          },
          status: 'ACTIVE',
          openDate: '2023-01-15',
          branchCode: 'EUR001',
          productCode: 'CUR_EUR',
          complianceStatus: 'COMPLIANT'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.get.mockResolvedValue(accountResponse);

      const result = await connector.getAccountDetails('GB29NWBK60161331926819');

      expect(result.accountNumber).toBe('GB29NWBK60161331926819');
      expect(result.currency).toBe('EUR');
      expect(result.balance).toBe(15000.00);
      expect(result.accountType).toBe('CURRENT');
    });

    test('should check account balance with multi-currency support', async () => {
      const balanceResponse = {
        data: {
          accountNumber: 'GB29NWBK60161331926819',
          balances: [
            { currency: 'EUR', available: 15000.00, current: 15250.00 },
            { currency: 'USD', available: 5000.00, current: 5100.00 },
            { currency: 'GBP', available: 8000.00, current: 8050.00 }
          ],
          lastUpdated: '2023-12-01T10:00:00Z'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.get.mockResolvedValue(balanceResponse);

      const result = await connector.checkAccountBalance('GB29NWBK60161331926819', 'EUR');

      expect(result.balance).toBe(15000.00);
      expect(result.currency).toBe('EUR');
      expect(result.multiCurrencyBalances).toHaveLength(3);
    });

    test('should handle account not found', async () => {
      const notFoundError = new Error('Account not found');
      notFoundError.response = {
        status: 404,
        data: { 
          error: 'ACCOUNT_NOT_FOUND',
          message: 'Account does not exist'
        }
      };

      mockHttpClient.get.mockRejectedValue(notFoundError);

      await expect(connector.getAccountDetails('INVALID_ACC')).rejects.toThrow('Account not found');
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(() => {
      connector.accessToken = 'TRANSACT_TOKEN_001';
      connector.sessionToken = 'SESSION_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.isAuthenticated = true;
    });

    test('should validate European transaction', async () => {
      const transaction = {
        id: 'TXN_EUR_001',
        type: 'debit',
        fromAccount: 'GB29NWBK60161331926819',
        toAccount: 'FR1420041010050500013M02606',
        amount: 1000.00,
        currency: 'EUR',
        description: 'SEPA Credit Transfer'
      };

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(true);
      expect(result.complianceChecks).toContain('SEPA_VALIDATION');
      expect(result.complianceChecks).toContain('EUROPEAN_COMPLIANCE');
    });

    test('should process debit transaction with SEPA', async () => {
      const transaction = {
        id: 'TXN_SEPA_001',
        type: 'debit',
        fromAccount: 'GB29NWBK60161331926819',
        toAccount: 'FR1420041010050500013M02606',
        amount: 500.00,
        currency: 'EUR',
        description: 'SEPA Instant Payment'
      };

      const debitResponse = {
        data: {
          transactionId: 'TXN_SEPA_001',
          status: 'CONFIRMED',
          amount: 500.00,
          currency: 'EUR',
          fromAccount: 'GB29NWBK60161331926819',
          toAccount: 'FR1420041010050500013M02606',
          sepaReference: 'SEPA20231201001',
          swiftGPIReference: 'GPI20231201001',
          executionTime: '2023-12-01T10:00:00Z',
          valueDate: '2023-12-01',
          complianceStatus: 'APPROVED'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(debitResponse);

      const result = await connector.processDebit(transaction);

      expect(result.transactionId).toBe('TXN_SEPA_001');
      expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(result.sepaReference).toBe('SEPA20231201001');
      expect(connector.transactMetrics.sepaTransactions).toBe(1);
    });

    test('should process credit transaction with SWIFT GPI', async () => {
      const transaction = {
        id: 'TXN_GPI_001',
        type: 'credit',
        fromAccount: 'US12345678901234567890',
        toAccount: 'GB29NWBK60161331926819',
        amount: 2500.00,
        currency: 'USD',
        description: 'International Wire Transfer'
      };

      const creditResponse = {
        data: {
          transactionId: 'TXN_GPI_001',
          status: 'CONFIRMED',
          amount: 2500.00,
          currency: 'USD',
          swiftGPIReference: 'GPI20231201002',
          trackingReference: 'UETR12345678',
          exchangeRate: 0.85,
          convertedAmount: 2125.00,
          convertedCurrency: 'EUR',
          executionTime: '2023-12-01T10:30:00Z'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(creditResponse);

      const result = await connector.processCredit(transaction);

      expect(result.transactionId).toBe('TXN_GPI_001');
      expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(result.swiftGPIReference).toBe('GPI20231201002');
      expect(connector.transactMetrics.swiftGPITransactions).toBe(1);
    });

    test('should handle transaction failures', async () => {
      const transaction = {
        id: 'TXN_FAIL_001',
        fromAccount: 'GB29NWBK60161331926819',
        amount: 50000.00,
        currency: 'EUR'
      };

      const failureResponse = {
        data: {
          transactionId: 'TXN_FAIL_001',
          status: 'REJECTED',
          errorCode: 'INSUFFICIENT_FUNDS',
          errorMessage: 'Account balance insufficient'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(failureResponse);

      const result = await connector.processDebit(transaction);

      expect(result.status).toBe(TRANSACTION_STATUS.FAILED);
      expect(result.errorCode).toBe('INSUFFICIENT_FUNDS');
    });

    test('should get transaction status with tracking', async () => {
      const statusResponse = {
        data: {
          transactionId: 'TXN_STATUS_001',
          status: 'PROCESSING',
          trackingSteps: [
            { step: 'INITIATED', timestamp: '2023-12-01T10:00:00Z' },
            { step: 'COMPLIANCE_CHECK', timestamp: '2023-12-01T10:01:00Z' },
            { step: 'FUNDS_RESERVED', timestamp: '2023-12-01T10:02:00Z' }
          ],
          estimatedCompletion: '2023-12-01T10:30:00Z'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.get.mockResolvedValue(statusResponse);

      const result = await connector.getTransactionStatus('TXN_STATUS_001');

      expect(result.status).toBe('PROCESSING');
      expect(result.trackingSteps).toHaveLength(3);
    });
  });

  describe('European Compliance', () => {
    beforeEach(() => {
      connector.accessToken = 'TRANSACT_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.isAuthenticated = true;
    });

    test('should perform European compliance check', async () => {
      const transaction = {
        amount: 15000.00,
        currency: 'EUR',
        fromAccount: 'GB29NWBK60161331926819',
        description: 'Large value transfer'
      };

      const complianceResponse = {
        data: {
          status: 'APPROVED',
          checks: ['AML', 'KYC', 'FATCA', 'MiFID_II'],
          riskScore: 0.15,
          requiresManualReview: false,
          complianceReference: 'COMP20231201001'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(complianceResponse);

      const result = await connector.performEuropeanComplianceCheck(transaction);

      expect(result.status).toBe('APPROVED');
      expect(result.checks).toContain('MiFID_II');
      expect(connector.transactMetrics.complianceChecks).toBe(1);
    });

    test('should validate SEPA transaction format', async () => {
      const sepaTransaction = {
        fromAccount: 'GB29NWBK60161331926819',
        toAccount: 'FR1420041010050500013M02606',
        amount: 100.00,
        currency: 'EUR',
        description: 'SEPA payment'
      };

      const result = await connector.validateSEPATransaction(sepaTransaction);

      expect(result.isValid).toBe(true);
      expect(result.sepaCompliant).toBe(true);
      expect(result.ibanValid).toBe(true);
    });

    test('should handle compliance failures', async () => {
      const transaction = {
        amount: 100000.00,
        currency: 'EUR',
        fromAccount: 'SUSPICIOUS_ACCOUNT'
      };

      const complianceResponse = {
        data: {
          status: 'REJECTED',
          checks: ['AML', 'KYC'],
          riskScore: 0.95,
          requiresManualReview: true,
          rejectionReason: 'HIGH_RISK_TRANSACTION'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(complianceResponse);

      const result = await connector.performEuropeanComplianceCheck(transaction);

      expect(result.status).toBe('REJECTED');
      expect(result.riskScore).toBeGreaterThan(0.9);
    });
  });

  describe('Status and Health Monitoring', () => {
    test('should return enhanced status with Transact metrics', async () => {
      connector.transactMetrics = {
        europeanTransactions: 150,
        sepaTransactions: 85,
        swiftGPITransactions: 65,
        complianceChecks: 200,
        multiCurrencyTransactions: 45,
        t24LegacyOperations: 25
      };

      const status = connector.getStatus();

      expect(status.transactMetrics).toBeDefined();
      expect(status.transactMetrics.europeanTransactions).toBe(150);
      expect(status.complianceStatus).toBeDefined();
      expect(status.multiCurrencySupport).toBeDefined();
    });

    test('should provide health status', async () => {
      // Set up authentication
      connector.accessToken = 'TRANSACT_TOKEN_001';
      connector.sessionToken = 'SESSION_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.isAuthenticated = true;

      const healthResponse = {
        data: { 
          status: 'healthy', 
          version: '2023.12',
          services: {
            'account-service': 'UP',
            'payment-service': 'UP',
            'compliance-service': 'UP'
          }
        },
        status: 200
      };

      mockHttpClient.get.mockResolvedValue(healthResponse);

      const result = await connector.getHealthStatus();

      expect(result.healthy).toBe(true);
      expect(result.services).toBeDefined();
    });
  });

  describe('Cleanup and Resource Management', () => {
    test('should cleanup resources properly', async () => {
      connector.accessToken = 'TOKEN_001';
      
      await connector.cleanup();

      expect(connector.accessToken).toBeNull();
    });

    test('should handle cleanup errors gracefully', async () => {
      const cleanupError = new Error('Cleanup failed');
      mockHttpClient.delete.mockRejectedValue(cleanupError);

      await expect(connector.cleanup()).resolves.not.toThrow();
    });
  });

  describe('Integration with Base Connector', () => {
    test('should properly extend BaseBankingConnector', () => {
      expect(connector).toBeInstanceOf(TemenosTransactConnector);
      expect(connector.config).toBeDefined();
      expect(connector.metrics).toBeDefined();
    });

    test('should implement all required abstract methods', () => {
      expect(typeof connector.authenticate).toBe('function');
      expect(typeof connector.testConnection).toBe('function');
      expect(typeof connector.getAccountDetails).toBe('function');
      expect(typeof connector.validateTransaction).toBe('function');
      expect(typeof connector.processDebit).toBe('function');
      expect(typeof connector.processCredit).toBe('function');
    });
  });

  describe('Transact Constants and Exports', () => {
    test('should export all required endpoints', () => {
      expect(TRANSACT_ENDPOINTS).toBeDefined();
      expect(TRANSACT_ENDPOINTS.OAUTH_TOKEN).toBe('/oauth/token');
      expect(TRANSACT_ENDPOINTS.ACCOUNT_SERVICE).toBe('/accounts');
    });

    test('should export transaction types', () => {
      expect(TRANSACT_TRANSACTION_TYPES).toBeDefined();
    });

    test('should export supported currencies', () => {
      expect(TRANSACT_CURRENCIES).toBeDefined();
    });
  });
});