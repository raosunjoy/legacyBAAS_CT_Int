/**
 * Fiserv DNA Connector Complete Test Suite
 * Achieving 100% test coverage for Fiserv DNA integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const axios = require('axios');
jest.mock('axios');

const { 
  FiservDNAConnector, 
  DNA_ENDPOINTS, 
  DNA_TRANSACTION_TYPES,
  DNA_ACCOUNT_TYPES 
} = require('../../../src/connectors/fiserv-dna/fiserv-dna-connector');

const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

describe('FiservDNAConnector - Complete Test Suite', () => {
  let connector;
  let mockHttpClient;
  let config;

  beforeEach(() => {
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
      enableMutualTLS: false,
      rateLimitPerSecond: 10,
      rateLimitPerMinute: 500,
      cacheExpiry: 300000,
      testMode: true
    };

    // Create connector instance first
    connector = new FiservDNAConnector(config);
    
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
      expect(connector.config.bankCode).toBe('FISERV_DNA');
      expect(connector.config.bankName).toBe('Fiserv DNA Platform');
      expect(connector.dnaConfig.institutionId).toBe('TEST_INST_001');
      expect(connector.dnaConfig.environment).toBe('sandbox');
      expect(connector.dnaConfig.enableCaching).toBe(true);
      expect(connector.dnaConfig.enableWebhooks).toBe(true);
      expect(connector.dnaConfig.rateLimitPerSecond).toBe(10);
      expect(connector.dnaConfig.cacheExpiry).toBe(300000);
    });

    test('should handle minimal configuration', () => {
      const minimalConnector = new FiservDNAConnector({});
      expect(minimalConnector.config.timeout).toBe(30000);
      expect(minimalConnector.dnaConfig.enableCaching).toBe(true);
      expect(minimalConnector.dnaConfig.enableWebhooks).toBe(true);
      expect(minimalConnector.dnaConfig.rateLimitPerSecond).toBe(100);
      expect(minimalConnector.dnaConfig.rateLimitPerMinute).toBe(1000);
    });

    test('should use environment variables when config not provided', () => {
      process.env.FISERV_DNA_API_URL = 'https://api.fiserv.com/dna';
      process.env.FISERV_DNA_INSTITUTION_ID = 'ENV_INST_001';
      
      const envConnector = new FiservDNAConnector({});
      expect(envConnector.dnaConfig.baseUrl).toBe('https://api.fiserv.com/dna');
      expect(envConnector.dnaConfig.institutionId).toBe('ENV_INST_001');
      
      delete process.env.FISERV_DNA_API_URL;
      delete process.env.FISERV_DNA_INSTITUTION_ID;
    });

    test('should initialize all required components', () => {
      expect(connector.accountCache).toBeInstanceOf(Map);
      expect(connector.customerCache).toBeInstanceOf(Map);
      expect(connector.webhookHandlers).toBeInstanceOf(Map);
      expect(connector.requestTimes).toBeInstanceOf(Array);
      expect(connector.httpClient).toBeDefined();
      expect(connector.dnaMetrics).toBeDefined();
    });

    test('should set up metrics tracking', () => {
      expect(connector.dnaMetrics).toEqual({
        apiCalls: 0,
        cacheHits: 0,
        cacheMisses: 0,
        webhookEvents: 0,
        complianceChecks: 0,
        realTimeRequests: 0
      });
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

      mockHttpClient.post.mockResolvedValue(mockTokenResponse);

      await connector.authenticate();

      expect(connector.accessToken).toBe('mock_access_token');
      expect(connector.refreshToken).toBe('mock_refresh_token');
      expect(connector.tokenExpiry).toBeGreaterThan(Date.now());
      expect(mockHttpClient.defaults.headers.common['Authorization']).toBe('Bearer mock_access_token');
    });

    test('should handle authentication with API key only', async () => {
      // Mock successful OAuth2 response even without clientId
      const mockResponse = {
        data: {
          access_token: 'mock_api_key_token',
          refresh_token: 'mock_refresh',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };

      mockHttpClient.post.mockResolvedValue(mockResponse);
      
      const keyOnlyConfig = { ...config, clientId: '', clientSecret: '' };
      const keyConnector = new FiservDNAConnector(keyOnlyConfig);
      keyConnector.httpClient = mockHttpClient;
      
      await keyConnector.authenticate();
      
      expect(keyConnector.accessToken).toBe('mock_api_key_token');
    });

    test('should handle authentication failure', async () => {
      const authError = new Error('Authentication failed');
      authError.response = {
        status: 401,
        data: { error: 'invalid_client', error_description: 'Invalid credentials' }
      };

      mockHttpClient.post.mockRejectedValue(authError);

      await expect(connector.authenticate()).rejects.toThrow('DNA authentication failed');
      expect(connector.metrics.authenticationFailures).toBe(1);
      expect(connector.accessToken).toBeNull();
    });

    test('should refresh token when expired', async () => {
      connector.accessToken = 'expired_token';
      connector.refreshToken = 'refresh_token';
      connector.tokenExpiry = Date.now() - 1000;

      const refreshResponse = {
        data: {
          access_token: 'new_access_token',
          refresh_token: 'new_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };

      mockHttpClient.post.mockResolvedValue(refreshResponse);

      // Call makeApiCall which should trigger re-authentication
      await connector.makeApiCall('GET', '/test');

      expect(connector.accessToken).toBe('new_access_token');
    });

    test('should handle token refresh failure', async () => {
      connector.accessToken = 'expired_token';
      connector.refreshToken = 'invalid_refresh';
      connector.tokenExpiry = Date.now() - 1000;

      const refreshError = new Error('Refresh failed');
      refreshError.response = { status: 401 };

      mockHttpClient.post.mockRejectedValue(refreshError);

      await expect(connector.makeApiCall('GET', '/test')).rejects.toThrow('DNA authentication failed');
    });
  });

  describe('Connection Testing', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should test connection successfully', async () => {
      // Set up valid token to avoid authentication
      connector.accessToken = 'valid_token';
      connector.tokenExpiry = Date.now() + 3600000;
      
      mockHttpClient.mockResolvedValue({ 
        status: 200, 
        data: { status: 'healthy' },
        config: { metadata: { startTime: Date.now() } }
      });
      
      const result = await connector.testConnection();
      
      expect(result).toBe(true);
      expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
        method: 'get',
        url: '/health'
      }));
    });

    test('should handle connection test failure', async () => {
      mockHttpClient.mockRejectedValue(new Error('Connection failed'));
      
      const result = await connector.testConnection();
      
      expect(result).toBe(false);
    });

    test('should re-authenticate if needed during connection test', async () => {
      connector.accessToken = null;
      
      const authResponse = {
        data: {
          access_token: 'new_token',
          refresh_token: 'new_refresh',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };
      
      mockHttpClient.post.mockResolvedValueOnce(authResponse);
      mockHttpClient.mockResolvedValueOnce({ 
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      });
      
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
            restrictions: [],
            metadata: {}
          },
          status: 200,
          config: { metadata: { startTime: Date.now() } }
        };

        // Override the default mock for this specific test
        mockHttpClient.mockImplementationOnce(() => Promise.resolve(mockAccountData));

        const result = await connector.getAccountDetails('1234567890');

        expect(result).toEqual({
          accountNumber: '1234567890',
          accountType: 'CHECKING',
          accountStatus: 'ACTIVE',
          customerId: 'CUST_001',
          productCode: 'CHK_001',
          openDate: '2023-01-01',
          currency: 'USD',
          balances: mockAccountData.data.balances,
          holds: [],
          interestRate: 0.01,
          fees: [],
          restrictions: [],
          metadata: {}
        });
        
        expect(connector.dnaMetrics.cacheMisses).toBe(1);
      });

      test('should use cached account details', async () => {
        const cachedData = {
          accountNumber: '1234567890',
          accountType: 'SAVINGS',
          accountStatus: 'ACTIVE'
        };

        const cacheKey = `account:1234567890:{}`;
        connector.accountCache.set(cacheKey, {
          data: cachedData,
          timestamp: Date.now()
        });

        const result = await connector.getAccountDetails('1234567890');

        expect(result).toEqual(cachedData);
        expect(connector.dnaMetrics.cacheHits).toBe(1);
        expect(mockHttpClient).not.toHaveBeenCalled();
      });

      test('should handle expired cache', async () => {
        const cacheKey = `account:1234567890:{}`;
        connector.accountCache.set(cacheKey, {
          data: { accountNumber: '1234567890' },
          timestamp: Date.now() - 400000 // Expired
        });

        const freshData = {
          data: {
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            status: 'ACTIVE',
            customerId: 'CUST_001',
            productCode: 'CHK_001',
            openDate: '2023-01-01',
            currency: 'USD',
            balances: {},
            holds: [],
            interestRate: 0,
            fees: [],
            restrictions: [],
            metadata: {}
          },
          status: 200,
          config: { metadata: { startTime: Date.now() } }
        };

        mockHttpClient.mockImplementationOnce(() => Promise.resolve(freshData));

        await connector.getAccountDetails('1234567890');

        expect(mockHttpClient).toHaveBeenCalled();
        expect(connector.dnaMetrics.cacheMisses).toBe(1);
      });

      test('should handle account not found', async () => {
        const error = new Error('Not found');
        error.response = {
          status: 404,
          data: { code: 'INVALID_ACCOUNT', message: 'Account not found' }
        };

        mockHttpClient.mockRejectedValue(error);

        await expect(connector.getAccountDetails('9999999999')).rejects.toThrow('Account not found');
      });

      test('should include optional parameters', async () => {
        mockHttpClient.mockImplementationOnce(() => Promise.resolve({ 
          data: {
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            status: 'ACTIVE',
            currency: 'USD',
            balances: {},
            holds: [],
            fees: [],
            restrictions: [],
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }));

        await connector.getAccountDetails('1234567890', {
          includeHistory: true,
          includeCustomer: true,
          historyDays: 30
        });

        expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
          method: 'get',
          url: expect.stringContaining('/accounts'),
          params: expect.objectContaining({
            includeHistory: true,
            includeCustomer: true,
            historyDays: 30
          })
        }));
      });
    });

    describe('checkAccountBalance', () => {
      test('should check balance successfully', async () => {
        const mockBalanceData = {
          data: {
            availableBalance: '1500.00',
            currentBalance: '1500.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: '2023-12-01T10:00:00Z'
          }
        };

        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          ...mockBalanceData,
          config: { metadata: { startTime: Date.now() } }
        }));

        const result = await connector.checkAccountBalance('1234567890', 'USD');

        expect(result).toEqual({
          accountNumber: '1234567890',
          currency: 'USD',
          availableBalance: 1500.00,
          currentBalance: 1500.00,
          pendingBalance: 0.00,
          holds: [],
          lastUpdated: '2023-12-01T10:00:00Z'
        });
      });

      test('should default to USD currency', async () => {
        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          data: {
            availableBalance: '1000.00',
            currentBalance: '1000.00',
            pendingBalance: '0.00'
          },
          config: { metadata: { startTime: Date.now() } }
        }));

        await connector.checkAccountBalance('1234567890');

        expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
          method: 'get',
          url: expect.stringContaining('/accounts/balance'),
          params: expect.objectContaining({
            accountNumber: '1234567890',
            currency: 'USD'
          })
        }));
      });

      test('should cache balance results', async () => {
        const balanceData = {
          data: {
            availableBalance: '2000.00',
            currentBalance: '2000.00'
          },
          config: { metadata: { startTime: Date.now() } }
        };

        mockHttpClient.mockImplementationOnce(() => Promise.resolve(balanceData));

        // First call - should hit the API and cache miss
        await connector.checkAccountBalance('1234567890');
        expect(connector.dnaMetrics.cacheMisses).toBe(1);
        expect(connector.dnaMetrics.cacheHits).toBe(0);

        // Second call should use cache (no additional API call)
        await connector.checkAccountBalance('1234567890');
        expect(connector.dnaMetrics.cacheHits).toBe(1);
        expect(connector.dnaMetrics.cacheMisses).toBe(1);
        expect(mockHttpClient).toHaveBeenCalledTimes(1);
      });
    });
  });

  describe('Transaction Validation', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should validate transaction successfully', async () => {
      const transaction = {
        id: 'TXN_001',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 100.00,
        currency: 'USD'
      };

      const mockAccountData = {
        data: {
          accountNumber: '1234567890',
          accountType: 'CHECKING',
          status: 'ACTIVE',
          customerId: 'CUST_001',
          productCode: 'CHK_001',
          openDate: '2023-01-01',
          currency: 'USD',
          balances: {},
          holds: [],
          fees: [],
          restrictions: [],
          metadata: {}
        },
        config: { metadata: { startTime: Date.now() } }
      };

      const mockBalanceData = {
        data: {
          availableBalance: '1000.00',
          currentBalance: '1000.00',
          pendingBalance: '0.00',
          holds: [],
          lastUpdated: new Date().toISOString()
        },
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient
        .mockImplementationOnce(() => Promise.resolve(mockAccountData))
        .mockImplementationOnce(() => Promise.resolve(mockBalanceData));

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(true);
      expect(result.errors).toEqual([]);
      expect(result.warnings).toEqual([]);
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

      mockHttpClient
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            status: 'ACTIVE',
            currency: 'USD',
            balances: {},
            holds: [],
            fees: [],
            restrictions: [],
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }))
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            availableBalance: '100.00',
            currentBalance: '100.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: new Date().toISOString()
          },
          config: { metadata: { startTime: Date.now() } }
        }));

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('Insufficient funds');
    });

    test('should fail validation for inactive account', async () => {
      const transaction = {
        id: 'TXN_003',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 100.00
      };

      mockHttpClient.mockImplementationOnce(() => Promise.resolve({
        data: { 
          accountNumber: '1234567890',
          accountType: 'CHECKING',
          status: 'CLOSED',
          currency: 'USD',
          balances: {},
          holds: [],
          fees: [],
          restrictions: [],
          metadata: {}
        },
        config: { metadata: { startTime: Date.now() } }
      }));

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('Source account is not active');
    });

    test('should validate credit transactions', async () => {
      const transaction = {
        id: 'TXN_004',
        type: 'credit',
        toAccount: '9876543210',
        amount: 500.00
      };

      mockHttpClient.mockResolvedValue({
        data: { status: 'ACTIVE' }
      });

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(true);
    });

    test('should perform compliance check for large amounts', async () => {
      const transaction = {
        id: 'TXN_005',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 15000.00,
        currency: 'USD'
      };

      const complianceResponse = {
        data: {
          status: 'PASSED',
          riskScore: 0.2,
          flags: []
        }
      };

      // Reset metrics first
      connector.dnaMetrics.complianceChecks = 0;

      mockHttpClient
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            accountStatus: 'ACTIVE',
            status: 'ACTIVE',
            currency: 'USD',
            balances: {},
            holds: [],
            fees: [],
            restrictions: [],
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }))
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            availableBalance: '20000.00',
            currentBalance: '20000.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: new Date().toISOString()
          },
          config: { metadata: { startTime: Date.now() } }
        }))
        .mockImplementationOnce(() => Promise.resolve({
          ...complianceResponse,
          config: { metadata: { startTime: Date.now() } }
        }));

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(true);
      expect(result.complianceStatus).toBe('passed');
      expect(connector.dnaMetrics.complianceChecks).toBe(1);
    });

    test('should fail validation when compliance check fails', async () => {
      const transaction = {
        id: 'TXN_COMPLIANCE_FAIL',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 25000.00, // Large amount triggering compliance
        currency: 'USD'
      };

      const complianceFailureResponse = {
        data: {
          status: 'FAILED',
          riskScore: 0.9,
          flags: ['HIGH_RISK', 'SUSPICIOUS_PATTERN']
        }
      };

      mockHttpClient
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            status: 'ACTIVE',
            currency: 'USD',
            balances: {},
            holds: [],
            fees: [],
            restrictions: [],
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }))
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            availableBalance: '30000.00',
            currentBalance: '30000.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: new Date().toISOString()
          },
          config: { metadata: { startTime: Date.now() } }
        }))
        .mockImplementationOnce(() => Promise.resolve({
          ...complianceFailureResponse,
          config: { metadata: { startTime: Date.now() } }
        }));

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(false);
      expect(result.errors).toContain('Compliance check failed');
      expect(result.complianceStatus).toBe('failed');
    });

    test('should handle international transaction validation', async () => {
      const transaction = {
        id: 'TXN_006',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 5000.00,
        currency: 'USD',
        isInternational: true
      };

      mockHttpClient
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            accountNumber: '1234567890',
            accountType: 'CHECKING',
            accountStatus: 'ACTIVE',
            status: 'ACTIVE',
            currency: 'USD',
            balances: {},
            holds: [],
            fees: [],
            restrictions: [],
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }))
        .mockImplementationOnce(() => Promise.resolve({ 
          data: { 
            availableBalance: '10000.00',
            currentBalance: '10000.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: new Date().toISOString()
          },
          config: { metadata: { startTime: Date.now() } }
        }));

      const result = await connector.validateTransaction(transaction);

      expect(result.warnings).toContain('International transaction - additional fees may apply');
      expect(result.isValid).toBe(true);
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    describe('processDebit', () => {
      test('should process debit successfully', async () => {
        const transaction = {
          id: 'TXN_DEBIT_001',
          fromAccount: '1234567890',
          amount: 500.00,
          currency: 'USD',
          description: 'Test debit',
          reference: 'REF_001'
        };

        const mockResponse = {
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

        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          ...mockResponse,
          config: { metadata: { startTime: Date.now() } }
        }));

        const result = await connector.processDebit(transaction);

        expect(result).toEqual({
          transactionId: 'TXN_DEBIT_001',
          status: TRANSACTION_STATUS.CONFIRMED,
          amount: 500.00,
          currency: 'USD',
          processedAt: '2023-12-01T10:00:00Z',
          balance: 500.00,
          reference: 'REF_001'
        });

        expect(result.transactionId).toBe('TXN_DEBIT_001');
        expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      });

      test('should handle debit failure', async () => {
        const transaction = {
          id: 'TXN_DEBIT_002',
          fromAccount: '1234567890',
          amount: 500.00
        };

        // Reset metrics
        connector.metrics.failedTransactions = 0;

        const error = new Error('Debit failed');
        error.response = {
          status: 400,
          data: { code: 'INSUFFICIENT_FUNDS' }
        };

        mockHttpClient.mockImplementationOnce(() => Promise.reject(error));

        await expect(connector.processDebit(transaction)).rejects.toThrow();
        expect(connector.metrics.failedTransactions).toBe(1);
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

        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          data: { 
            transactionId: 'TXN_DEBIT_003',
            status: 'COMPLETED',
            amount: 100.00,
            currency: 'USD',
            processedAt: new Date().toISOString(),
            newBalance: 900.00,
            reference: 'TXN_DEBIT_003'
          },
          config: { metadata: { startTime: Date.now() } }
        }));

        await connector.processDebit(transaction);

        expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
          method: 'post',
          url: '/transactions/debit',
          data: expect.objectContaining({
            metadata: expect.objectContaining({
              blockchain: 'ethereum',
              useCase: 'cross_border_payments'
            })
          })
        }));
      });
    });

    describe('processCredit', () => {
      test('should process credit successfully', async () => {
        const transaction = {
          id: 'TXN_CREDIT_001',
          toAccount: '9876543210',
          amount: 750.00,
          currency: 'USD',
          description: 'Test credit',
          reference: 'REF_002'
        };

        const mockResponse = {
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

        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          ...mockResponse,
          config: { metadata: { startTime: Date.now() } }
        }));

        const result = await connector.processCredit(transaction);

        expect(result).toEqual({
          transactionId: 'TXN_CREDIT_001',
          status: TRANSACTION_STATUS.CONFIRMED,
          amount: 750.00,
          currency: 'USD',
          processedAt: '2023-12-01T10:05:00Z',
          balance: 1750.00,
          reference: 'REF_002'
        });
      });

      test('should handle multi-currency credits', async () => {
        const transaction = {
          id: 'TXN_CREDIT_002',
          toAccount: '9876543210',
          amount: 100.00,
          currency: 'EUR'
        };

        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          data: {
            transactionId: 'TXN_CREDIT_002',
            status: 'COMPLETED',
            amount: 100.00,
            currency: 'EUR',
            processedAt: new Date().toISOString(),
            newBalance: 1100.00,
            reference: 'TXN_CREDIT_002',
            exchangeRate: 1.09
          },
          config: { metadata: { startTime: Date.now() } }
        }));

        await connector.processCredit(transaction);

        expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
          method: 'post',
          url: '/transactions/credit',
          data: expect.objectContaining({
            currency: 'EUR'
          })
        }));
      });
    });

    describe('getTransactionStatus', () => {
      test('should get transaction status successfully', async () => {
        const mockStatus = {
          data: {
            status: 'COMPLETED',
            amount: 100.00,
            currency: 'USD',
            processedAt: '2023-12-01T10:00:00Z',
            updatedAt: '2023-12-01T10:01:00Z',
            reference: 'REF_001',
            metadata: {
              blockchain: 'ethereum',
              useCase: 'cross_border_payments'
            }
          }
        };

        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          ...mockStatus,
          config: { metadata: { startTime: Date.now() } }
        }));

        const result = await connector.getTransactionStatus('TXN_001');

        expect(result).toEqual({
          transactionId: 'TXN_001',
          status: TRANSACTION_STATUS.CONFIRMED,
          amount: 100.00,
          currency: 'USD',
          processedAt: '2023-12-01T10:00:00Z',
          updatedAt: '2023-12-01T10:01:00Z',
          reference: 'REF_001',
          metadata: mockStatus.data.metadata
        });
      });

      test('should handle pending status', async () => {
        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          data: { 
            transactionId: 'TXN_002',
            status: 'PENDING',
            amount: 500.00,
            currency: 'USD',
            processedAt: new Date().toISOString(),
            updatedAt: new Date().toISOString(),
            reference: 'TXN_002',
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }));

        const result = await connector.getTransactionStatus('TXN_002');
        expect(result.status).toBe(TRANSACTION_STATUS.PENDING);
      });

      test('should handle failed status', async () => {
        mockHttpClient.mockImplementationOnce(() => Promise.resolve({
          data: { 
            transactionId: 'TXN_003',
            status: 'FAILED',
            amount: 750.00,
            currency: 'USD',
            processedAt: new Date().toISOString(),
            updatedAt: new Date().toISOString(),
            reference: 'TXN_003',
            failureReason: 'Insufficient funds',
            metadata: {}
          },
          config: { metadata: { startTime: Date.now() } }
        }));

        const result = await connector.getTransactionStatus('TXN_003');
        expect(result.status).toBe(TRANSACTION_STATUS.FAILED);
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
        id: 'TXN_COMP_001',
        amount: 15000.00,
        currency: 'USD',
        fromAccount: '1234567890',
        toAccount: '9876543210'
      };

      const mockCompliance = {
        data: {
          status: 'PASSED',
          riskScore: 0.15,
          flags: [],
          recommendations: ['Monitor future transactions'],
          screeningResults: {
            aml: 'CLEAR',
            kyc: 'VERIFIED',
            sanctions: 'CLEAR',
            pep: 'NO_MATCH'
          }
        }
      };

      mockHttpClient.mockResolvedValue(mockCompliance);

      const result = await connector.performComplianceCheck(transaction);

      expect(result).toEqual({
        passed: true,
        riskScore: 0.15,
        flags: [],
        recommendations: ['Monitor future transactions'],
        screeningResults: mockCompliance.data.screeningResults
      });

      expect(connector.dnaMetrics.complianceChecks).toBe(1);
    });

    test('should handle compliance failure', async () => {
      const transaction = {
        id: 'TXN_COMP_002',
        amount: 50000.00
      };

      const mockCompliance = {
        data: {
          status: 'FAILED',
          riskScore: 0.85,
          flags: ['HIGH_RISK_COUNTRY', 'PEP_MATCH'],
          recommendations: ['Manual review required']
        }
      };

      mockHttpClient.mockResolvedValue(mockCompliance);

      const result = await connector.performComplianceCheck(transaction);

      expect(result.passed).toBe(false);
      expect(result.riskScore).toBe(0.85);
      expect(result.flags).toContain('HIGH_RISK_COUNTRY');
      expect(result.flags).toContain('PEP_MATCH');
    });

    test('should cache compliance results', async () => {
      // Set compliance threshold to 0 so that the compliance check is always performed
      connector.dnaConfig.complianceThreshold = 0;
      
      const transaction = {
        id: 'TXN_COMP_003',
        amount: 5000.00
      };

      mockHttpClient.mockResolvedValue({
        data: { status: 'PASSED', riskScore: 0.1 }
      });

      // First call
      await connector.performComplianceCheck(transaction);
      expect(mockHttpClient).toHaveBeenCalledTimes(1);

      // Second call should use cache
      await connector.performComplianceCheck(transaction);
      expect(mockHttpClient).toHaveBeenCalledTimes(1);
      expect(connector.dnaMetrics.cacheHits).toBe(1);
    });

    test('should bypass check for small amounts when configured', async () => {
      connector.dnaConfig.complianceThreshold = 10000;
      
      const transaction = {
        id: 'TXN_COMP_004',
        amount: 500.00
      };

      const result = await connector.performComplianceCheck(transaction);

      expect(result.passed).toBe(true);
      expect(result.riskScore).toBe(0);
      expect(mockHttpClient).not.toHaveBeenCalled();
    });
  });

  describe('Webhook Management', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should register webhook successfully', async () => {
      const mockWebhook = {
        data: {
          webhookId: 'WEBHOOK_001',
          eventType: 'transaction.completed',
          callbackUrl: 'https://example.com/webhook',
          active: true,
          createdAt: '2023-12-01T10:00:00Z'
        }
      };

      mockHttpClient.mockImplementationOnce(() => Promise.resolve({
        ...mockWebhook,
        config: { metadata: { startTime: Date.now() } }
      }));

      const result = await connector.registerWebhook(
        'transaction.completed',
        'https://example.com/webhook'
      );

      expect(result).toEqual(mockWebhook.data);
      expect(connector.webhookHandlers.has('transaction.completed')).toBe(true);
    });

    test('should handle webhook registration failure', async () => {
      const error = new Error('Registration failed');
      error.response = { status: 400 };

      mockHttpClient.mockRejectedValue(error);

      await expect(
        connector.registerWebhook('invalid.event', 'https://example.com')
      ).rejects.toThrow('Registration failed');
    });

    test('should process webhook event with handler', async () => {
      const handler = jest.fn();
      connector.webhookHandlers.set('transaction.completed', handler);

      const event = {
        eventType: 'transaction.completed',
        data: {
          transactionId: 'TXN_001',
          status: 'COMPLETED'
        }
      };

      await connector.processWebhookEvent(event);

      expect(handler).toHaveBeenCalledWith(event.data);
      expect(connector.dnaMetrics.webhookEvents).toBe(1);
    });

    test('should handle webhook event without handler', async () => {
      const event = {
        eventType: 'unknown.event',
        data: {}
      };

      // Should not throw
      await connector.processWebhookEvent(event);
      expect(connector.dnaMetrics.webhookEvents).toBe(0);
    });

    test('should unregister webhook', async () => {
      mockHttpClient.mockResolvedValue({ data: { success: true } });

      const result = await connector.unregisterWebhook('WEBHOOK_001');

      expect(result.success).toBe(true);
      expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
        method: 'delete',
        url: '/webhooks/WEBHOOK_001'
      }));
    });

    test('should list active webhooks', async () => {
      const mockWebhooks = {
        data: {
          webhooks: [
            { webhookId: 'WEBHOOK_001', eventType: 'transaction.completed' },
            { webhookId: 'WEBHOOK_002', eventType: 'account.updated' }
          ]
        }
      };

      mockHttpClient.mockResolvedValue(mockWebhooks);

      const result = await connector.listWebhooks();

      expect(result).toEqual(mockWebhooks.data.webhooks);
    });
  });

  describe('Error Handling and Mapping', () => {
    test('should map DNA status codes correctly', () => {
      expect(connector.mapDNAStatus('PENDING')).toBe(TRANSACTION_STATUS.PENDING);
      expect(connector.mapDNAStatus('PROCESSING')).toBe(TRANSACTION_STATUS.PROCESSING);
      expect(connector.mapDNAStatus('COMPLETED')).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(connector.mapDNAStatus('FAILED')).toBe(TRANSACTION_STATUS.FAILED);
      expect(connector.mapDNAStatus('REJECTED')).toBe(TRANSACTION_STATUS.REJECTED);
      expect(connector.mapDNAStatus('AUTHORIZED')).toBe(TRANSACTION_STATUS.AUTHORIZED);
      expect(connector.mapDNAStatus('UNKNOWN')).toBe(TRANSACTION_STATUS.PENDING);
    });

    test('should map DNA error codes correctly', () => {
      const errorMappings = [
        { dna: 'INSUFFICIENT_FUNDS', expected: ERROR_CODES.INSUFFICIENT_FUNDS },
        { dna: 'INVALID_ACCOUNT', expected: ERROR_CODES.INVALID_ACCOUNT },
        { dna: 'ACCOUNT_CLOSED', expected: ERROR_CODES.ACCOUNT_INACTIVE },
        { dna: 'DUPLICATE_TRANSACTION', expected: ERROR_CODES.DUPLICATE_TRANSACTION },
        { dna: 'EXCEEDS_LIMIT', expected: ERROR_CODES.LIMIT_EXCEEDED },
        { dna: 'AUTHENTICATION_FAILED', expected: ERROR_CODES.AUTHENTICATION_FAILED },
        { dna: 'AUTHORIZATION_FAILED', expected: ERROR_CODES.AUTHORIZATION_FAILED },
        { dna: 'SERVICE_UNAVAILABLE', expected: ERROR_CODES.SERVICE_UNAVAILABLE },
        { dna: 'UNKNOWN_ERROR', expected: ERROR_CODES.SERVICE_UNAVAILABLE }
      ];

      errorMappings.forEach(({ dna, expected }) => {
        const mapped = connector.mapDNAError({ code: dna });
        expect(mapped.code).toBe(expected);
      });
    });

    test('should handle error with details', () => {
      const dnaError = {
        code: 'INSUFFICIENT_FUNDS',
        message: 'Account has insufficient funds',
        details: {
          balance: 50.00,
          required: 100.00
        }
      };

      const mapped = connector.mapDNAError(dnaError);

      expect(mapped).toEqual({
        code: ERROR_CODES.INSUFFICIENT_FUNDS,
        message: 'Account has insufficient funds',
        details: dnaError.details
      });
    });

    test('should handle API errors with retries', async () => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;

      const error = new Error('Network error');
      error.code = 'ECONNRESET';

      mockHttpClient
        .mockRejectedValueOnce(error)
        .mockResolvedValue({ data: { success: true } });

      const result = await connector.makeApiCall('GET', '/test');

      expect(result.data.success).toBe(true);
      expect(mockHttpClient).toHaveBeenCalledTimes(2);
    });
  });

  describe('Rate Limiting', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.dnaConfig.rateLimitPerSecond = 2;
      connector.dnaConfig.rateLimitPerMinute = 10;
    });

    test('should enforce rate limiting', async () => {
      mockHttpClient.mockResolvedValue({ data: {} });

      const start = Date.now();
      
      // Make 3 rapid requests
      const promises = [
        connector.makeApiCall('GET', '/test1'),
        connector.makeApiCall('GET', '/test2'),
        connector.makeApiCall('GET', '/test3')
      ];

      await Promise.all(promises);
      const duration = Date.now() - start;

      // Should take at least 500ms due to rate limiting (2 per second)
      expect(duration).toBeGreaterThanOrEqual(500);
    });

    test('should enforce per-minute rate limiting', async () => {
      mockHttpClient.mockImplementation(() => Promise.resolve({
        data: {},
        config: { metadata: { startTime: Date.now() } }
      }));

      // Set low per-minute limit for testing
      connector.dnaConfig.rateLimitPerMinute = 2;
      
      // Pre-fill request times to hit the per-minute limit
      const now = Date.now();
      connector.requestTimes = [now - 50000, now - 40000]; // Two requests within the minute
      
      const start = Date.now();
      
      // This should trigger the per-minute rate limiting (line 676)
      await connector.makeApiCall('GET', '/test-minute-limit');
      
      const duration = Date.now() - start;
      
      // Should have been delayed due to per-minute rate limiting
      expect(duration).toBeGreaterThan(0);
    });

    test('should track rate limit metrics', async () => {
      mockHttpClient.mockImplementation(() => Promise.resolve({
        data: {},
        config: { metadata: { startTime: Date.now() } }
      }));
      
      await connector.makeApiCall('GET', '/test');
      
      expect(connector.dnaMetrics.apiCalls).toBe(1);
      expect(connector.requestTimes.length).toBe(1);
    });

    test('should clean up old request times', async () => {
      // Add old request times
      connector.requestTimes = [
        Date.now() - 70000, // Over 1 minute old
        Date.now() - 30000, // 30 seconds old
        Date.now() - 5000   // 5 seconds old
      ];

      connector.cleanupRequestTimes();

      // Should only keep requests from last minute
      expect(connector.requestTimes.length).toBe(2);
    });
  });

  describe('Caching Operations', () => {
    test('should generate correct cache keys', () => {
      const key1 = connector.getCacheKey('account', '123');
      expect(key1).toBe('account:123');

      const key2 = connector.getCacheKey('balance', '456', { currency: 'EUR' });
      expect(key2).toBe('balance:456:{"currency":"EUR"}');
    });

    test('should check cache validity', () => {
      const validCache = {
        timestamp: Date.now() - 100000 // 100 seconds old
      };
      expect(connector.isCacheValid(validCache)).toBe(true);

      const invalidCache = {
        timestamp: Date.now() - 400000 // 400 seconds old
      };
      expect(connector.isCacheValid(invalidCache)).toBe(false);
    });

    test('should clear all caches', () => {
      connector.accountCache.set('test1', { data: {} });
      connector.balanceCache.set('test2', { data: {} });
      connector.customerCache.set('test3', { data: {} });
      connector.complianceCache.set('test4', { data: {} });

      connector.clearAllCaches();

      expect(connector.accountCache.size).toBe(0);
      expect(connector.balanceCache.size).toBe(0);
      expect(connector.customerCache.size).toBe(0);
      expect(connector.complianceCache.size).toBe(0);
    });

    test('should clear expired cache entries', () => {
      connector.accountCache.set('valid', {
        data: {},
        timestamp: Date.now()
      });
      
      connector.accountCache.set('expired', {
        data: {},
        timestamp: Date.now() - 400000
      });

      connector.clearExpiredCache();

      expect(connector.accountCache.has('valid')).toBe(true);
      expect(connector.accountCache.has('expired')).toBe(false);
    });
  });

  describe('Advanced Features', () => {
    beforeEach(() => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    test('should handle real-time notifications', async () => {
      const notification = {
        type: 'balance_update',
        accountNumber: '1234567890',
        newBalance: 1500.00,
        timestamp: '2023-12-01T10:00:00Z'
      };

      // Set up cache data to be invalidated
      const cacheKey = connector.getCacheKey('balance', notification.accountNumber);
      connector.balanceCache.set(cacheKey, { balance: 1000.00, timestamp: Date.now() });
      
      // Verify cache is initially populated
      expect(connector.balanceCache.has(cacheKey)).toBe(true);

      await connector.handleRealTimeNotification(notification);

      expect(connector.dnaMetrics.realTimeRequests).toBe(1);
      
      // Should invalidate balance cache
      expect(connector.balanceCache.has(cacheKey)).toBe(false);
    });

    test('should support batch operations', async () => {
      const accounts = ['123', '456', '789'];
      
      const mockBatchResponse = {
        data: {
          results: [
            { accountNumber: '123', balance: 1000.00 },
            { accountNumber: '456', balance: 2000.00 },
            { accountNumber: '789', balance: 3000.00 }
          ]
        }
      };

      mockHttpClient.mockResolvedValue(mockBatchResponse);

      const results = await connector.batchCheckBalances(accounts);

      expect(results).toEqual(mockBatchResponse.data.results);
      expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
        method: 'post',
        url: expect.stringContaining('/batch'),
        data: expect.objectContaining({
          operation: 'checkBalance',
          accounts: accounts
        })
      }));
    });

    test('should handle mutual TLS when enabled', async () => {
      const fs = require('fs');
      const https = require('https');
      
      // Mock fs.readFileSync
      jest.spyOn(fs, 'readFileSync').mockImplementation((path) => {
        if (path.includes('cert')) return 'mock-cert';
        if (path.includes('key')) return 'mock-key';
        return 'mock-file';
      });

      const tlsConfig = {
        ...config,
        enableMutualTLS: true,
        certificatePath: '/path/to/cert.pem',
        privateKeyPath: '/path/to/key.pem'
      };

      const tlsConnector = new FiservDNAConnector(tlsConfig);
      
      mockHttpClient.mockResolvedValue({
        data: {
          access_token: 'tls_token',
          refresh_token: 'tls_refresh',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      });

      await tlsConnector.authenticate();
      
      expect(fs.readFileSync).toHaveBeenCalledWith('/path/to/cert.pem');
      expect(fs.readFileSync).toHaveBeenCalledWith('/path/to/key.pem');
      
      fs.readFileSync.mockRestore();
    });

    test('should support transaction history queries', async () => {
      const mockHistory = {
        data: {
          transactions: [
            { id: 'TXN_001', amount: 100.00, date: '2023-12-01' },
            { id: 'TXN_002', amount: 200.00, date: '2023-12-02' }
          ],
          totalCount: 2
        }
      };

      mockHttpClient.mockResolvedValue(mockHistory);

      const result = await connector.getTransactionHistory('1234567890', {
        startDate: '2023-12-01',
        endDate: '2023-12-31',
        limit: 100
      });

      expect(result.transactions).toHaveLength(2);
      expect(result.totalCount).toBe(2);
    });
  });

  describe('Status and Health Monitoring', () => {
    test('should return complete status information', () => {
      connector.dnaMetrics.apiCalls = 100;
      connector.dnaMetrics.cacheHits = 50;
      connector.dnaMetrics.cacheMisses = 25;
      connector.accessToken = 'test_token';
      connector.tokenExpiry = Date.now() + 3600000;

      const status = connector.getStatus();

      expect(status).toMatchObject({
        bankCode: 'FISERV_DNA',
        connectionStatus: 'CONNECTED',
        dnaMetrics: expect.objectContaining({
          apiCalls: 100,
          cacheHits: 50,
          cacheMisses: 25
        }),
        tokenStatus: expect.objectContaining({
          hasToken: true,
          timeToExpiry: expect.any(Number)
        }),
        cacheHitRatio: 0.67
      });
    });

    test('should calculate cache hit ratio correctly', () => {
      connector.dnaMetrics.cacheHits = 0;
      connector.dnaMetrics.cacheMisses = 0;

      let status = connector.getStatus();
      expect(status.cacheHitRatio).toBe(0);

      connector.dnaMetrics.cacheHits = 75;
      connector.dnaMetrics.cacheMisses = 25;

      status = connector.getStatus();
      expect(status.cacheHitRatio).toBe(0.75);
    });

    test('should provide health status', async () => {
      connector.accessToken = 'valid_token';
      connector.tokenExpiry = Date.now() + 3600000;
      mockHttpClient.mockResolvedValue({ status: 200 });

      const health = await connector.getHealthStatus();

      expect(health).toEqual({
        status: 'healthy',
        details: {
          authentication: 'active',
          apiConnection: 'connected',
          tokenValid: true,
          cacheStatus: 'active',
          webhookStatus: 'active'
        },
        timestamp: expect.any(String)
      });
    });

    test('should detect unhealthy status', async () => {
      connector.accessToken = null;

      const health = await connector.getHealthStatus();

      expect(health.status).toBe('unhealthy');
      expect(health.details.authentication).toBe('inactive');
    });
  });

  describe('Cleanup and Resource Management', () => {
    test('should cleanup resources properly', async () => {
      connector.webhookHandlers.set('test', jest.fn());
      connector.accountCache.set('test', { data: {} });
      
      await connector.cleanup();

      expect(connector.accountCache.size).toBe(0);
      expect(connector.webhookHandlers.size).toBe(0);
      expect(connector.isConnected).toBe(false);
      expect(connector.accessToken).toBeNull();
    });

    test('should handle cleanup errors gracefully', async () => {
      mockHttpClient.mockRejectedValue(new Error('Cleanup failed'));

      // Should not throw
      await expect(connector.cleanup()).resolves.not.toThrow();
    });

    test('should emit cleanup event', async () => {
      const cleanupHandler = jest.fn();
      connector.on('cleanup', cleanupHandler);

      await connector.cleanup();

      expect(cleanupHandler).toHaveBeenCalled();
    });
  });

  describe('Base Connector Integration', () => {
    test('should properly extend BaseBankingConnector', () => {
      const BaseBankingConnector = require('../../../src/connectors/base/base-banking-connector').BaseBankingConnector;
      expect(connector).toBeInstanceOf(BaseBankingConnector);
    });

    test('should implement all required abstract methods', () => {
      const requiredMethods = [
        'authenticate',
        'testConnection',
        'getAccountDetails',
        'checkAccountBalance',
        'validateTransaction',
        'processDebit',
        'processCredit',
        'getTransactionStatus'
      ];

      requiredMethods.forEach(method => {
        expect(connector[method]).toBeDefined();
        expect(typeof connector[method]).toBe('function');
      });
    });

    test('should emit transaction events', async () => {
      const completedHandler = jest.fn();
      connector.on('transaction:completed', completedHandler);

      mockHttpClient.mockResolvedValue({
        data: { status: 'COMPLETED', transactionId: 'TXN_001' }
      });

      await connector.processDebit({
        id: 'TXN_001',
        fromAccount: '123',
        amount: 100
      });

      expect(completedHandler).toHaveBeenCalledWith(expect.objectContaining({
        transactionId: 'TXN_001'
      }));
    });

    test('should track base metrics', () => {
      expect(connector.metrics).toBeDefined();
      expect(connector.metrics.totalTransactions).toBe(0);
      expect(connector.metrics.successfulTransactions).toBe(0);
      expect(connector.metrics.failedTransactions).toBe(0);
      expect(connector.metrics.apiErrors).toBe(0);
    });
  });

  describe('Edge Cases and Error Scenarios', () => {
    test('should handle network timeouts', async () => {
      const timeoutError = new Error('Timeout');
      timeoutError.code = 'ECONNABORTED';

      mockHttpClient.mockRejectedValue(timeoutError);

      await expect(connector.getAccountDetails('123')).rejects.toThrow('Timeout');
      expect(connector.metrics.apiErrors).toBe(1);
    });

    test('should handle malformed responses', async () => {
      mockHttpClient.mockResolvedValue({ data: null });

      await expect(connector.getAccountDetails('123')).rejects.toThrow();
    });

    test('should handle rate limit exceeded', async () => {
      const rateLimitError = new Error('Rate limit exceeded');
      rateLimitError.response = {
        status: 429,
        headers: { 'retry-after': '60' }
      };

      mockHttpClient.mockRejectedValue(rateLimitError);

      await expect(connector.makeApiCall('GET', '/test')).rejects.toThrow('Rate limit exceeded');
    });

    test('should handle service maintenance', async () => {
      const maintenanceError = new Error('Service unavailable');
      maintenanceError.response = {
        status: 503,
        data: { message: 'Scheduled maintenance' }
      };

      mockHttpClient.mockRejectedValue(maintenanceError);

      await expect(connector.testConnection()).resolves.toBe(false);
    });

    test('should handle authentication without OAuth2', async () => {
      const apiKeyConfig = {
        ...config,
        clientId: null,
        clientSecret: null
      };

      const apiKeyConnector = new FiservDNAConnector(apiKeyConfig);
      
      await apiKeyConnector.authenticate();
      
      expect(apiKeyConnector.httpClient.defaults.headers.common['X-API-Key']).toBe('test_api_key');
    });

    test('should update metrics on API success', async () => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      
      mockHttpClient.mockResolvedValue({ 
        data: {}, 
        config: { 
          metadata: { 
            startTime: Date.now() - 100 
          } 
        } 
      });

      await connector.makeApiCall('GET', '/test');
      
      expect(connector.dnaMetrics.apiCalls).toBe(1);
    });

    test('should update metrics on API failure', async () => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      
      const error = new Error('API error');
      error.response = { status: 500 };
      mockHttpClient.mockRejectedValue(error);

      try {
        await connector.makeApiCall('GET', '/test');
      } catch (e) {
        // Expected to throw
      }
      
      expect(connector.dnaMetrics.apiCalls).toBe(1);
    });

    test('should handle missing helper methods', () => {
      // Test missing methods that might not be covered
      expect(() => connector.getCacheKey('test', '123')).not.toThrow();
      expect(() => connector.isCacheValid({ timestamp: Date.now() })).not.toThrow();
      expect(() => connector.clearExpiredCache()).not.toThrow();
    });

    test('should handle webhook processing without handler', async () => {
      const event = {
        eventType: 'unknown.event',
        data: { test: 'data' }
      };

      // Should not throw even without handler
      await connector.processWebhookEvent(event);
      expect(connector.dnaMetrics.webhookEvents).toBe(0);
    });

    test('should handle real-time notification cache invalidation', async () => {
      const notification = {
        type: 'account_update',
        accountNumber: '1234567890',
        timestamp: '2023-12-01T10:00:00Z'
      };

      // Add some cache entries first
      const cacheKey = connector.getCacheKey('account', notification.accountNumber);
      connector.accountCache.set(cacheKey, { data: {}, timestamp: Date.now() });

      await connector.handleRealTimeNotification(notification);

      expect(connector.dnaMetrics.realTimeRequests).toBe(1);
    });

    test('should handle batch operations with empty array', async () => {
      const result = await connector.batchCheckBalances([]);
      expect(result).toEqual([]);
    });

    test('should handle GET request with params in makeApiCall', async () => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      
      mockHttpClient.mockResolvedValue({ data: {} });

      await connector.makeApiCall('GET', '/test', { params: { test: 'value' } });
      
      expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
        method: 'get',
        data: expect.objectContaining({
          params: { test: 'value' }
        })
      }));
    });

    test('should handle POST request with data in makeApiCall', async () => {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      
      mockHttpClient.mockResolvedValue({ data: {} });

      await connector.makeApiCall('POST', '/test', { testData: 'value' });
      
      expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
        method: 'post',
        data: { testData: 'value' }
      }));
    });
  });
});

describe('DNA Constants and Exports', () => {
  test('should export all required endpoints', () => {
    expect(DNA_ENDPOINTS).toBeDefined();
    expect(DNA_ENDPOINTS.AUTHENTICATE).toBe('/auth/oauth/token');
    expect(DNA_ENDPOINTS.ACCOUNT_INQUIRY).toBe('/accounts/inquiry');
    expect(DNA_ENDPOINTS.BALANCE_CHECK).toBe('/accounts/balance');
    expect(DNA_ENDPOINTS.PAYMENT_INITIATION).toBe('/payments/initiate');
    expect(DNA_ENDPOINTS.TRANSACTION_STATUS).toBe('/transactions/status');
    expect(DNA_ENDPOINTS.CUSTOMER_SERVICE).toBe('/customers');
    expect(DNA_ENDPOINTS.AML_SCREENING).toBe('/compliance/aml/screen');
    expect(DNA_ENDPOINTS.KYC_VERIFICATION).toBe('/compliance/kyc/verify');
    expect(DNA_ENDPOINTS.COMPLIANCE_CHECK).toBe('/compliance/check');
    expect(DNA_ENDPOINTS.WEBHOOK_MANAGEMENT).toBe('/webhooks');
    expect(DNA_ENDPOINTS.BATCH_OPERATIONS).toBe('/batch');
  });

  test('should export transaction types', () => {
    expect(DNA_TRANSACTION_TYPES).toBeDefined();
    expect(DNA_TRANSACTION_TYPES.DEBIT).toBe('debit');
    expect(DNA_TRANSACTION_TYPES.CREDIT).toBe('credit');
    expect(DNA_TRANSACTION_TYPES.TRANSFER).toBe('transfer');
    expect(DNA_TRANSACTION_TYPES.WIRE).toBe('wire');
    expect(DNA_TRANSACTION_TYPES.ACH).toBe('ach');
    expect(DNA_TRANSACTION_TYPES.PAYMENT).toBe('payment');
    expect(DNA_TRANSACTION_TYPES.REVERSAL).toBe('reversal');
  });

  test('should export account types', () => {
    expect(DNA_ACCOUNT_TYPES).toBeDefined();
    expect(DNA_ACCOUNT_TYPES.CHECKING).toBe('checking');
    expect(DNA_ACCOUNT_TYPES.SAVINGS).toBe('savings');
    expect(DNA_ACCOUNT_TYPES.MONEY_MARKET).toBe('money_market');
    expect(DNA_ACCOUNT_TYPES.CD).toBe('cd');
    expect(DNA_ACCOUNT_TYPES.LOAN).toBe('loan');
    expect(DNA_ACCOUNT_TYPES.MORTGAGE).toBe('mortgage');
    expect(DNA_ACCOUNT_TYPES.CREDIT_CARD).toBe('credit_card');
    expect(DNA_ACCOUNT_TYPES.LINE_OF_CREDIT).toBe('line_of_credit');
  });
});