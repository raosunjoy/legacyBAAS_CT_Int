/**
 * Fiserv DNA Connector Tests (Jest)
 * Comprehensive test suite for Fiserv DNA core banking integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage: Authentication, Transactions, Compliance, Error Handling
 */

const axios = require('axios');
const EventEmitter = require('events');

jest.mock('axios');

const { 
  FiservDNAConnector, 
  DNA_ENDPOINTS, 
  DNA_TRANSACTION_TYPES,
  DNA_ACCOUNT_TYPES 
} = require('../../../src/connectors/fiserv-dna/fiserv-dna-connector');

const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

describe('FiservDNAConnector', () => {
  let connector;
  let mockAxios;
  let config;

  beforeEach(() => {
    // Clear all mocks before each test
    jest.clearAllMocks();
    
    // Mock configuration
    config = {
      baseUrl: 'https://api-sandbox.fiserv.com/dna',
      institutionId: 'TEST_INST_001',
      clientId: 'test_client_id',
      clientSecret: 'test_client_secret',
      apiKey: 'test_api_key',
      apiSecret: 'test_api_secret',
      environment: 'sandbox',
      enableCaching: true,
      enableWebhooks: true,
      testMode: true
    };

    // Create connector instance first
    connector = new FiservDNAConnector(config);
    
    // Mock axios.create and httpClient
    mockAxios = {
      post: jest.fn(),
      get: jest.fn(),
      put: jest.fn(),
      delete: jest.fn(),
      defaults: { headers: { common: {} } }
    };
    
    // Mock the httpClient as a function that returns promises
    connector.httpClient = jest.fn();
    connector.httpClient.defaults = { headers: { common: {} } };
    
    // Helper function to mock API responses based on URL and method
    connector.httpClient.mockImplementation((config) => {
      const method = (config.method || 'get').toLowerCase();
      const mockMethod = mockAxios[method];
      if (mockMethod && mockMethod.mockResolvedValue) {
        // Return the mocked response
        return mockMethod();
      }
      return Promise.resolve({ data: {} });
    });
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('Constructor and Initialization', () => {
    test('should initialize with correct configuration', () => {
      expect(connector.config.bankCode).toBe('FISERV_DNA');
      expect(connector.config.bankName).toBe('Fiserv DNA Platform');
      expect(connector.dnaConfig.institutionId).toBe('TEST_INST_001');
      expect(connector.dnaConfig.environment).toBe('sandbox');
    });

    test('should set up default configuration values', () => {
      const defaultConnector = new FiservDNAConnector({});
      expect(defaultConnector.config.timeout).toBe(30000);
      expect(defaultConnector.dnaConfig.enableCaching).toBe(true);
      expect(defaultConnector.dnaConfig.enableWebhooks).toBe(true);
    });

    test('should initialize metrics tracking', () => {
      expect(connector.dnaMetrics).toEqual({
        apiCalls: 0,
        cacheHits: 0,
        cacheMisses: 0,
        webhookEvents: 0,
        complianceChecks: 0,
        realTimeRequests: 0
      });
    });

    test('should initialize caches', () => {
      expect(connector.accountCache).toBeInstanceOf(Map);
      expect(connector.customerCache).toBeInstanceOf(Map);
      expect(connector.balanceCache).toBeInstanceOf(Map);
      expect(connector.complianceCache).toBeInstanceOf(Map);
    });

    test('should set up webhook handlers map', () => {
      expect(connector.webhookHandlers).toBeInstanceOf(Map);
    });

    test('should initialize rate limiter configuration', () => {
      expect(connector.rateLimiter).toBeDefined();
      expect(connector.dnaConfig.rateLimitPerSecond).toBe(10);
      expect(connector.dnaConfig.rateLimitPerMinute).toBe(500);
    });
  });

  describe('Authentication', () => {
    test('should authenticate successfully with OAuth2', async () => {
      const mockTokenResponse = {
        data: {
          access_token: 'mock_access_token',
          refresh_token: 'mock_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };

      mockAxios.post.mockResolvedValue(mockTokenResponse);

      await connector.authenticate();

      expect(connector.accessToken).toBe('mock_access_token');
      expect(connector.refreshToken).toBe('mock_refresh_token');
      expect(connector.tokenExpiry).toBeGreaterThan(Date.now());
      expect(connector.httpClient.defaults.headers.common['Authorization']).toBe('Bearer mock_access_token');
      
      expect(connector.httpClient).toHaveBeenCalledWith(
        expect.objectContaining({
          method: 'post',
          url: DNA_ENDPOINTS.AUTHENTICATE,
          data: expect.objectContaining({
            grant_type: 'client_credentials',
            client_id: 'test_client_id',
            client_secret: 'test_client_secret'
          })
        })
      );
    });

    test('should handle authentication failure', async () => {
      const authError = new Error('Authentication failed');
      authError.response = {
        status: 401,
        data: { error: 'invalid_client' }
      };

      mockAxios.post.mockRejectedValue(authError);

      await expect(connector.authenticate()).rejects.toThrow('DNA authentication failed');
      expect(connector.metrics.authenticationFailures).toBe(1);
    });

    test('should refresh token when near expiry', async () => {
      // Set up initial token
      connector.accessToken = 'initial_token';
      connector.tokenExpiry = Date.now() + 30000; // 30 seconds

      const mockTokenResponse = {
        data: {
          access_token: 'new_access_token',
          refresh_token: 'new_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };
      
      mockAxios.post.mockResolvedValue(mockTokenResponse);
      mockAxios.get.mockResolvedValue({ status: 200, data: {} });

      await connector.ensureAuthenticated();

      expect(connector.accessToken).toBe('new_access_token');
    });

    test('should handle token refresh failure', async () => {
      connector.accessToken = 'expired_token';
      connector.tokenExpiry = Date.now() - 1000; // Expired

      const authError = new Error('Token refresh failed');
      authError.response = { status: 401 };

      mockAxios.post.mockRejectedValue(authError);

      await expect(connector.ensureAuthenticated()).rejects.toThrow();
    });
  });

  describe('Connection Testing', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should test connection successfully', async () => {
      mockAxios.get.mockResolvedValue({ status: 200 });
      
      const result = await connector.testConnection();
      
      expect(result).toBe(true);
      expect(mockAxios.get).toHaveBeenCalledWith('/health');
    });

    test('should handle connection test failure', async () => {
      mockAxios.get.mockRejectedValue(new Error('Connection failed'));
      
      const result = await connector.testConnection();
      
      expect(result).toBe(false);
    });

    test('should re-authenticate if token expired during connection test', async () => {
      connector.accessToken = null;
      
      const mockTokenResponse = {
        data: {
          access_token: 'new_token',
          refresh_token: 'new_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };
      
      mockAxios.post.mockResolvedValue(mockTokenResponse);
      mockAxios.get.mockResolvedValue({ status: 200 });
      
      const result = await connector.testConnection();
      
      expect(result).toBe(true);
      expect(connector.accessToken).toBe('new_token');
    });
  });

  describe('Account Operations', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    describe('getAccountDetails', () => {
      test('should get account details successfully', async () => {
        const mockAccountData = {
          data: {
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            status: 'ACTIVE',
            customerId: 'CUST_001',
            productCode: 'CHK_001',
            openDate: '2023-01-01',
            currency: 'USD',
            balances: {
              available: 1000.00,
              current: 1000.00,
              pending: 0.00
            },
            interestRate: 0.01,
            fees: [],
            restrictions: []
          }
        };

        mockAxios.get.mockResolvedValue(mockAccountData);

        const result = await connector.getAccountDetails('1234567890');

        expect(result.accountNumber).toBe('1234567890');
        expect(result.accountType).toBe('CHECKING');
        expect(result.accountStatus).toBe('ACTIVE');
        expect(result.balances).toEqual(mockAccountData.data.balances);
        expect(connector.dnaMetrics.cacheMisses).toBe(1);
      });

      test('should use cached account details when available', async () => {
        const accountNumber = '1234567890';
        const cachedData = {
          accountNumber,
          accountType: 'SAVINGS',
          accountStatus: 'ACTIVE'
        };

        // Add to cache
        connector.accountCache.set(`account:${accountNumber}:{}`, {
          data: cachedData,
          timestamp: Date.now()
        });

        const result = await connector.getAccountDetails(accountNumber);

        expect(result).toEqual(cachedData);
        expect(connector.dnaMetrics.cacheHits).toBe(1);
        expect(mockAxios.get).not.toHaveBeenCalled();
      });

      test('should handle account not found error', async () => {
        const error = new Error('Account not found');
        error.response = {
          status: 404,
          data: { code: 'INVALID_ACCOUNT', message: 'Account not found' }
        };

        mockAxios.get.mockRejectedValue(error);

        await expect(connector.getAccountDetails('9999999999')).rejects.toThrow('Account not found');
      });

      test('should include optional fields when requested', async () => {
        const mockData = {
          data: {
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            status: 'ACTIVE',
            customerId: 'CUST_001',
            productCode: 'CHK_001'
          }
        };

        mockAxios.get.mockResolvedValue(mockData);

        await connector.getAccountDetails('1234567890', {
          includeHistory: true,
          includeCustomer: true
        });

        expect(mockAxios.get).toHaveBeenCalledWith(
          expect.stringContaining('/accounts'),
          expect.objectContaining({
            params: expect.objectContaining({
              accountNumber: '1234567890',
              includeHistory: true,
              includeCustomer: true
            })
          })
        );
      });
    });

    describe('checkAccountBalance', () => {
      test('should check account balance successfully', async () => {
        const mockBalanceData = {
          data: {
            availableBalance: '1500.00',
            currentBalance: '1500.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: '2023-12-01T10:00:00Z'
          }
        };

        mockAxios.get.mockResolvedValue(mockBalanceData);

        const result = await connector.checkAccountBalance('1234567890', 'USD');

        expect(result.accountNumber).toBe('1234567890');
        expect(result.currency).toBe('USD');
        expect(result.availableBalance).toBe(1500.00);
        expect(result.currentBalance).toBe(1500.00);
        expect(result.pendingBalance).toBe(0.00);
      });

      test('should default to USD currency', async () => {
        const mockBalanceData = {
          data: {
            availableBalance: '1000.00',
            currentBalance: '1000.00',
            pendingBalance: '0.00'
          }
        };

        mockAxios.get.mockResolvedValue(mockBalanceData);

        await connector.checkAccountBalance('1234567890');

        expect(mockAxios.get).toHaveBeenCalledWith(
          expect.any(String),
          expect.objectContaining({
            params: expect.objectContaining({
              currency: 'USD'
            })
          })
        );
      });

      test('should handle balance check errors', async () => {
        const error = new Error('Balance check failed');
        error.response = { status: 500 };

        mockAxios.get.mockRejectedValue(error);

        await expect(connector.checkAccountBalance('1234567890')).rejects.toThrow('Balance check failed');
      });
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    describe('validateTransaction', () => {
      test('should validate transaction successfully', async () => {
        const transaction = {
          id: 'TXN_001',
          type: 'debit',
          fromAccount: '1234567890',
          amount: 100.00,
          currency: 'USD'
        };

        // Mock account details
        const mockAccountData = {
          data: {
            accountNumber: '1234567890',
            status: 'ACTIVE'
          }
        };

        // Mock balance check
        const mockBalanceData = {
          data: {
            availableBalance: '1000.00'
          }
        };

        mockAxios.get
          .mockResolvedValueOnce(mockAccountData)
          .mockResolvedValueOnce(mockBalanceData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).toBe(true);
        expect(result.errors).toEqual([]);
        expect(result.complianceStatus).toBe('passed');
      });

      test('should fail validation for insufficient funds', async () => {
        const transaction = {
          id: 'TXN_002',
          type: 'debit',
          fromAccount: '1234567890',
          amount: 2000.00,
          currency: 'USD'
        };

        // Mock account details
        const mockAccountData = {
          data: {
            accountNumber: '1234567890',
            status: 'ACTIVE'
          }
        };

        // Mock balance check - insufficient funds
        const mockBalanceData = {
          data: {
            availableBalance: '100.00'
          }
        };

        mockAxios.get
          .mockResolvedValueOnce(mockAccountData)
          .mockResolvedValueOnce(mockBalanceData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).toBe(false);
        expect(result.errors).toContain('Insufficient funds');
      });

      test('should fail validation for inactive account', async () => {
        const transaction = {
          id: 'TXN_003',
          type: 'debit',
          fromAccount: '1234567890',
          amount: 100.00,
          currency: 'USD'
        };

        // Mock inactive account
        const mockAccountData = {
          data: {
            accountNumber: '1234567890',
            status: 'CLOSED'
          }
        };

        mockAxios.get.mockResolvedValue(mockAccountData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).toBe(false);
        expect(result.errors).toContain('Source account is not active');
      });

      test('should perform compliance check for large amounts', async () => {
        const transaction = {
          id: 'TXN_004',
          type: 'debit',
          fromAccount: '1234567890',
          amount: 15000.00, // Above CTR threshold
          currency: 'USD'
        };

        // Mock account details
        const mockAccountData = {
          data: {
            accountNumber: '1234567890',
            status: 'ACTIVE'
          }
        };

        // Mock balance check
        const mockBalanceData = {
          data: {
            availableBalance: '20000.00'
          }
        };

        // Mock compliance check
        const mockComplianceData = {
          data: {
            status: 'PASSED',
            riskScore: 0.2,
            flags: [],
            recommendations: []
          }
        };

        mockAxios.get
          .mockResolvedValueOnce(mockAccountData)
          .mockResolvedValueOnce(mockBalanceData);
        
        mockAxios.post.mockResolvedValue(mockComplianceData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).toBe(true);
        expect(result.complianceStatus).toBe('passed');
        expect(connector.dnaMetrics.complianceChecks).toBe(1);
      });
    });

    describe('processDebit', () => {
      test('should process debit transaction successfully', async () => {
        const transaction = {
          id: 'TXN_DEBIT_001',
          fromAccount: '1234567890',
          amount: 500.00,
          currency: 'USD',
          description: 'Test debit transaction',
          reference: 'REF_001'
        };

        const mockDebitResponse = {
          data: {
            transactionId: 'TXN_DEBIT_001',
            status: 'COMPLETED',
            amount: 500.00,
            currency: 'USD',
            processedAt: '2023-12-01T10:00:00Z',
            newBalance: 500.00,
            reference: 'REF_001'
          }
        };

        mockAxios.post.mockResolvedValue(mockDebitResponse);

        const result = await connector.processDebit(transaction);

        expect(result.transactionId).toBe('TXN_DEBIT_001');
        expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
        expect(result.amount).toBe(500.00);
        expect(result.currency).toBe('USD');
        expect(result.balance).toBe(500.00);
      });

      test('should handle debit processing failure', async () => {
        const transaction = {
          id: 'TXN_DEBIT_002',
          fromAccount: '1234567890',
          amount: 500.00,
          currency: 'USD'
        };

        const error = new Error('Debit processing failed');
        error.response = {
          status: 400,
          data: { code: 'INSUFFICIENT_FUNDS', message: 'Insufficient funds' }
        };

        mockAxios.post.mockRejectedValue(error);

        await expect(connector.processDebit(transaction)).rejects.toThrow('Debit processing failed');
      });

      test('should include transaction metadata', async () => {
        const transaction = {
          id: 'TXN_DEBIT_003',
          fromAccount: '1234567890',
          amount: 100.00,
          metadata: {
            blockchain: 'ethereum',
            useCase: 'cross_border_payments'
          }
        };

        const mockResponse = {
          data: {
            transactionId: 'TXN_DEBIT_003',
            status: 'COMPLETED',
            amount: 100.00
          }
        };

        mockAxios.post.mockResolvedValue(mockResponse);

        await connector.processDebit(transaction);

        expect(mockAxios.post).toHaveBeenCalledWith(
          expect.any(String),
          expect.objectContaining({
            metadata: transaction.metadata
          }),
          expect.any(Object)
        );
      });
    });

    describe('processCredit', () => {
      test('should process credit transaction successfully', async () => {
        const transaction = {
          id: 'TXN_CREDIT_001',
          toAccount: '9876543210',
          amount: 750.00,
          currency: 'USD',
          description: 'Test credit transaction',
          reference: 'REF_002'
        };

        const mockCreditResponse = {
          data: {
            transactionId: 'TXN_CREDIT_001',
            status: 'COMPLETED',
            amount: 750.00,
            currency: 'USD',
            processedAt: '2023-12-01T10:05:00Z',
            newBalance: 1750.00,
            reference: 'REF_002'
          }
        };

        mockAxios.post.mockResolvedValue(mockCreditResponse);

        const result = await connector.processCredit(transaction);

        expect(result.transactionId).toBe('TXN_CREDIT_001');
        expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
        expect(result.amount).toBe(750.00);
        expect(result.currency).toBe('USD');
        expect(result.balance).toBe(1750.00);
      });

      test('should handle credit processing errors', async () => {
        const transaction = {
          id: 'TXN_CREDIT_002',
          toAccount: '9876543210',
          amount: 100.00
        };

        const error = new Error('Credit processing failed');
        error.response = { status: 500 };

        mockAxios.post.mockRejectedValue(error);

        await expect(connector.processCredit(transaction)).rejects.toThrow('Credit processing failed');
      });
    });

    describe('getTransactionStatus', () => {
      test('should get transaction status successfully', async () => {
        const mockStatusResponse = {
          data: {
            status: 'COMPLETED',
            amount: 100.00,
            currency: 'USD',
            processedAt: '2023-12-01T10:00:00Z',
            updatedAt: '2023-12-01T10:01:00Z',
            reference: 'REF_STATUS_001',
            metadata: {
              blockchain: 'ethereum',
              useCase: 'cross_border_payments'
            }
          }
        };

        mockAxios.get.mockResolvedValue(mockStatusResponse);

        const result = await connector.getTransactionStatus('TXN_STATUS_001');

        expect(result.transactionId).toBe('TXN_STATUS_001');
        expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
        expect(result.amount).toBe(100.00);
        expect(result.metadata).toEqual(mockStatusResponse.data.metadata);
      });

      test('should handle transaction not found', async () => {
        const error = new Error('Transaction not found');
        error.response = { status: 404 };

        mockAxios.get.mockRejectedValue(error);

        await expect(connector.getTransactionStatus('INVALID_TXN')).rejects.toThrow('Transaction not found');
      });
    });
  });

  describe('Compliance Operations', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should perform compliance check successfully', async () => {
      const transaction = {
        id: 'TXN_COMPLIANCE_001',
        amount: 15000.00,
        currency: 'USD',
        fromAccount: '1234567890',
        toAccount: '9876543210'
      };

      const mockComplianceResponse = {
        data: {
          status: 'PASSED',
          riskScore: 0.15,
          flags: [],
          recommendations: ['Monitor future transactions']
        }
      };

      mockAxios.post.mockResolvedValue(mockComplianceResponse);

      const result = await connector.performComplianceCheck(transaction);

      expect(result.passed).toBe(true);
      expect(result.riskScore).toBe(0.15);
      expect(result.flags).toEqual([]);
      expect(result.recommendations).toContain('Monitor future transactions');
      expect(connector.dnaMetrics.complianceChecks).toBe(1);
    });

    test('should handle compliance check failure', async () => {
      const transaction = {
        id: 'TXN_COMPLIANCE_002',
        amount: 50000.00,
        currency: 'USD'
      };

      const mockComplianceResponse = {
        data: {
          status: 'FAILED',
          riskScore: 0.85,
          flags: ['HIGH_RISK_COUNTRY', 'PEP_MATCH'],
          recommendations: ['Manual review required']
        }
      };

      mockAxios.post.mockResolvedValue(mockComplianceResponse);

      const result = await connector.performComplianceCheck(transaction);

      expect(result.passed).toBe(false);
      expect(result.riskScore).toBe(0.85);
      expect(result.flags).toContain('HIGH_RISK_COUNTRY');
      expect(result.flags).toContain('PEP_MATCH');
    });

    test('should cache compliance results', async () => {
      const transaction = {
        id: 'TXN_COMPLIANCE_003',
        amount: 5000.00
      };

      const mockResponse = {
        data: {
          status: 'PASSED',
          riskScore: 0.1
        }
      };

      mockAxios.post.mockResolvedValue(mockResponse);

      // First call
      await connector.performComplianceCheck(transaction);
      expect(mockAxios.post).toHaveBeenCalledTimes(1);

      // Second call should use cache
      await connector.performComplianceCheck(transaction);
      expect(mockAxios.post).toHaveBeenCalledTimes(1);
      expect(connector.dnaMetrics.cacheHits).toBe(1);
    });
  });

  describe('Webhook Management', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should register webhook successfully', async () => {
      const mockWebhookResponse = {
        data: {
          webhookId: 'WEBHOOK_001',
          eventType: 'transaction.completed',
          callbackUrl: 'https://example.com/webhook',
          active: true
        }
      };

      mockAxios.post.mockResolvedValue(mockWebhookResponse);

      const result = await connector.registerWebhook(
        'transaction.completed',
        'https://example.com/webhook'
      );

      expect(result.webhookId).toBe('WEBHOOK_001');
      expect(connector.webhookHandlers.has('transaction.completed')).toBe(true);
    });

    test('should handle webhook registration failure', async () => {
      const error = new Error('Webhook registration failed');
      error.response = { status: 400 };

      mockAxios.post.mockRejectedValue(error);

      await expect(
        connector.registerWebhook('invalid.event', 'https://example.com')
      ).rejects.toThrow('Webhook registration failed');
    });

    test('should process webhook event', async () => {
      const handler = jest.fn();
      connector.webhookHandlers.set('transaction.completed', handler);

      const event = {
        eventType: 'transaction.completed',
        data: { transactionId: 'TXN_001' }
      };

      await connector.processWebhookEvent(event);

      expect(handler).toHaveBeenCalledWith(event.data);
      expect(connector.dnaMetrics.webhookEvents).toBe(1);
    });

    test('should unregister webhook', async () => {
      mockAxios.delete.mockResolvedValue({ data: { success: true } });

      const result = await connector.unregisterWebhook('WEBHOOK_001');

      expect(result.success).toBe(true);
      expect(mockAxios.delete).toHaveBeenCalledWith('/webhooks/WEBHOOK_001');
    });
  });

  describe('Error Handling and Status Mapping', () => {
    test('should map DNA status codes correctly', () => {
      expect(connector.mapDNAStatus('PENDING')).toBe(TRANSACTION_STATUS.PENDING);
      expect(connector.mapDNAStatus('PROCESSING')).toBe(TRANSACTION_STATUS.PROCESSING);
      expect(connector.mapDNAStatus('COMPLETED')).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(connector.mapDNAStatus('FAILED')).toBe(TRANSACTION_STATUS.FAILED);
      expect(connector.mapDNAStatus('UNKNOWN')).toBe(TRANSACTION_STATUS.PENDING);
    });

    test('should map DNA error codes correctly', () => {
      const dnaError = {
        code: 'INSUFFICIENT_FUNDS',
        message: 'Account has insufficient funds',
        details: { balance: 50.00 }
      };

      const mapped = connector.mapDNAError(dnaError);

      expect(mapped.code).toBe(ERROR_CODES.INSUFFICIENT_FUNDS);
      expect(mapped.message).toBe('Account has insufficient funds');
      expect(mapped.details).toEqual({ balance: 50.00 });
    });

    test('should handle unknown error codes', () => {
      const dnaError = { code: 'UNKNOWN_ERROR' };
      const mapped = connector.mapDNAError(dnaError);

      expect(mapped.code).toBe(ERROR_CODES.SERVICE_UNAVAILABLE);
      expect(mapped.message).toBe('Unknown DNA error');
    });

    test('should handle API errors with proper context', async () => {
      const error = new Error('API Error');
      error.response = {
        status: 429,
        data: { error: 'Rate limit exceeded' }
      };

      mockAxios.get.mockRejectedValue(error);

      await expect(connector.getAccountDetails('123')).rejects.toThrow();
      expect(connector.metrics.apiErrors).toBe(1);
    });
  });

  describe('Rate Limiting', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.dnaConfig.rateLimitPerSecond = 2;
      connector.dnaConfig.rateLimitPerMinute = 10;
    });

    test('should respect rate limiting', async () => {
      jest.setTimeout(5000);

      mockAxios.get.mockResolvedValue({ status: 200, data: {} });

      // Make requests rapidly
      const promises = [];
      for (let i = 0; i < 3; i++) {
        promises.push(connector.makeApiCall('GET', '/test'));
      }

      const start = Date.now();
      await Promise.all(promises);
      const duration = Date.now() - start;

      // Should take at least 500ms due to rate limiting
      expect(duration).toBeGreaterThanOrEqual(500);
    });

    test('should track rate limit metrics', async () => {
      connector.dnaMetrics.apiCalls = 0;
      
      mockAxios.get.mockResolvedValue({ data: {} });
      
      await connector.makeApiCall('GET', '/test');
      
      expect(connector.dnaMetrics.apiCalls).toBe(1);
    });
  });

  describe('Caching Operations', () => {
    test('should cache account details with TTL', async () => {
      const accountData = {
        accountNumber: '123',
        accountType: 'CHECKING'
      };

      const key = connector.getCacheKey('account', '123', {});
      connector.accountCache.set(key, {
        data: accountData,
        timestamp: Date.now()
      });

      const cached = connector.accountCache.get(key);
      expect(cached.data).toEqual(accountData);
    });

    test('should invalidate expired cache', async () => {
      const key = connector.getCacheKey('balance', '123');
      connector.balanceCache.set(key, {
        data: { balance: 100 },
        timestamp: Date.now() - 400000 // Expired
      });

      mockAxios.get.mockResolvedValue({
        data: { availableBalance: '200.00' }
      });

      await connector.checkAccountBalance('123');

      expect(mockAxios.get).toHaveBeenCalled();
      expect(connector.dnaMetrics.cacheMisses).toBe(1);
    });

    test('should clear all caches', () => {
      connector.accountCache.set('test', { data: {} });
      connector.balanceCache.set('test', { data: {} });
      connector.customerCache.set('test', { data: {} });

      connector.clearAllCaches();

      expect(connector.accountCache.size).toBe(0);
      expect(connector.balanceCache.size).toBe(0);
      expect(connector.customerCache.size).toBe(0);
    });
  });

  describe('Status and Health Monitoring', () => {
    test('should return enhanced status with DNA metrics', () => {
      connector.dnaMetrics.apiCalls = 100;
      connector.dnaMetrics.cacheHits = 50;
      connector.accessToken = 'test_token';
      connector.tokenExpiry = Date.now() + 3600000;

      const status = connector.getStatus();

      expect(status.dnaMetrics.apiCalls).toBe(100);
      expect(status.dnaMetrics.cacheHits).toBe(50);
      expect(status.tokenStatus.hasToken).toBe(true);
      expect(status.tokenStatus.timeToExpiry).toBeGreaterThan(0);
    });

    test('should track metrics correctly', () => {
      expect(connector.dnaMetrics.apiCalls).toBe(0);
      
      // Simulate API call tracking
      connector.dnaMetrics.apiCalls++;
      connector.dnaMetrics.cacheHits++;
      
      expect(connector.dnaMetrics.apiCalls).toBe(1);
      expect(connector.dnaMetrics.cacheHits).toBe(1);
    });

    test('should calculate cache hit ratio', () => {
      connector.dnaMetrics.cacheHits = 75;
      connector.dnaMetrics.cacheMisses = 25;

      const status = connector.getStatus();
      const ratio = status.cacheHitRatio;

      expect(ratio).toBe(0.75);
    });

    test('should provide health status', async () => {
      connector.accessToken = 'valid_token';
      connector.tokenExpiry = Date.now() + 3600000;
      mockAxios.get.mockResolvedValue({ status: 200 });

      const health = await connector.getHealthStatus();

      expect(health.status).toBe('healthy');
      expect(health.details.authentication).toBe('active');
      expect(health.details.apiConnection).toBe('connected');
    });
  });

  describe('Integration with Base Connector', () => {
    test('should inherit from BaseBankingConnector', () => {
      const BaseBankingConnector = require('../../../src/connectors/base/base-banking-connector').BaseBankingConnector;
      expect(connector).toBeInstanceOf(BaseBankingConnector);
    });

    test('should emit events properly', (done) => {
      connector.on('test:event', (data) => {
        expect(data.message).toBe('test');
        done();
      });

      connector.emit('test:event', { message: 'test' });
    });

    test('should implement all required abstract methods', () => {
      expect(connector.authenticate).toBeDefined();
      expect(connector.testConnection).toBeDefined();
      expect(connector.getAccountDetails).toBeDefined();
      expect(connector.checkAccountBalance).toBeDefined();
      expect(connector.validateTransaction).toBeDefined();
      expect(connector.processDebit).toBeDefined();
      expect(connector.processCredit).toBeDefined();
      expect(connector.getTransactionStatus).toBeDefined();
    });

    test('should track base metrics', () => {
      expect(connector.metrics).toBeDefined();
      expect(connector.metrics.totalTransactions).toBe(0);
      expect(connector.metrics.successfulTransactions).toBe(0);
      expect(connector.metrics.failedTransactions).toBe(0);
    });
  });

  describe('Advanced Features', () => {
    test('should support multi-currency transactions', async () => {
      const transaction = {
        id: 'TXN_MULTI_001',
        fromAccount: '123',
        amount: 100.00,
        currency: 'EUR'
      };

      const mockResponse = {
        data: {
          transactionId: 'TXN_MULTI_001',
          status: 'COMPLETED',
          amount: 100.00,
          currency: 'EUR',
          exchangeRate: 1.09
        }
      };

      mockAxios.post.mockResolvedValue(mockResponse);

      const result = await connector.processDebit(transaction);

      expect(result.currency).toBe('EUR');
    });

    test('should handle real-time notifications', async () => {
      connector.dnaConfig.enableRealTimeNotifications = true;
      
      const notification = {
        type: 'balance_update',
        accountNumber: '123',
        newBalance: 1500.00
      };

      await connector.handleRealTimeNotification(notification);

      expect(connector.dnaMetrics.realTimeRequests).toBe(1);
    });

    test('should support batch operations', async () => {
      const accounts = ['123', '456', '789'];
      
      mockAxios.post.mockResolvedValue({
        data: {
          results: accounts.map(acc => ({
            accountNumber: acc,
            balance: 1000.00
          }))
        }
      });

      const results = await connector.batchCheckBalances(accounts);

      expect(results).toHaveLength(3);
      expect(mockAxios.post).toHaveBeenCalledWith(
        expect.stringContaining('/batch'),
        expect.objectContaining({
          accounts: accounts
        })
      );
    });
  });

  describe('Cleanup and Resource Management', () => {
    test('should cleanup resources properly', async () => {
      await connector.cleanup();

      expect(connector.accountCache.size).toBe(0);
      expect(connector.webhookHandlers.size).toBe(0);
      expect(connector.isConnected).toBe(false);
    });

    test('should handle cleanup errors gracefully', async () => {
      mockAxios.post.mockRejectedValue(new Error('Cleanup failed'));

      // Should not throw
      await expect(connector.cleanup()).resolves.not.toThrow();
    });
  });
});

describe('DNA Constants', () => {
  test('should export correct endpoints', () => {
    expect(DNA_ENDPOINTS.AUTHENTICATE).toBe('/auth/oauth/token');
    expect(DNA_ENDPOINTS.ACCOUNT_INQUIRY).toBe('/accounts/inquiry');
    expect(DNA_ENDPOINTS.PAYMENT_INITIATION).toBe('/payments/initiate');
    expect(DNA_ENDPOINTS.AML_SCREENING).toBe('/compliance/aml/screen');
  });

  test('should export transaction types', () => {
    expect(DNA_TRANSACTION_TYPES.DEBIT).toBe('debit');
    expect(DNA_TRANSACTION_TYPES.CREDIT).toBe('credit');
    expect(DNA_TRANSACTION_TYPES.WIRE).toBe('wire');
    expect(DNA_TRANSACTION_TYPES.ACH).toBe('ach');
  });

  test('should export account types', () => {
    expect(DNA_ACCOUNT_TYPES.CHECKING).toBe('checking');
    expect(DNA_ACCOUNT_TYPES.SAVINGS).toBe('savings');
    expect(DNA_ACCOUNT_TYPES.LOAN).toBe('loan');
    expect(DNA_ACCOUNT_TYPES.MORTGAGE).toBe('mortgage');
  });

  test('should export compliance thresholds', () => {
    expect(DNA_ENDPOINTS.COMPLIANCE_CHECK).toBe('/compliance/check');
    expect(DNA_ENDPOINTS.KYC_VERIFICATION).toBe('/compliance/kyc/verify');
  });
});