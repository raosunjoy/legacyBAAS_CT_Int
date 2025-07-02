/**
 * Enhanced TCS BaNCS Connector Test Suite
 * Comprehensive test coverage for multi-bank architecture connector
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Production-Ready Connector Testing - 100% Coverage Target
 */

const { EnhancedTCSBaNCSConnector } = require('../../../src/connectors/tcs-bancs/enhanced-bancs-connector');
const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

// Mock axios
jest.mock('axios');
const axios = require('axios');

// Mock crypto
jest.mock('crypto', () => ({
  createHmac: jest.fn().mockReturnValue({
    update: jest.fn().mockReturnThis(),
    digest: jest.fn().mockReturnValue('mocked-signature')
  }),
  randomBytes: jest.fn().mockReturnValue(Buffer.from('mocked-random-bytes'))
}));

// Mock uuid
jest.mock('uuid', () => ({
  v4: jest.fn().mockReturnValue('mocked-uuid-12345')
}));

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

describe('Enhanced TCS BaNCS Connector', () => {
  let connector;
  let mockHttpClient;
  let mockConfig;

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();

    mockConfig = {
      bankCode: 'TEST_BANCS',
      bankName: 'Test TCS BaNCS',
      clientId: 'test_client_id',
      clientSecret: 'test_client_secret',
      baseUrl: 'https://test-bancs.example.com',
      institutionId: 'INST001',
      branchCode: 'BRANCH001',
      encryptionKey: 'test_encryption_key',
      signatureKey: 'test_signature_key',
      webhookSecret: 'test_webhook_secret',
      timeout: 30000,
      enableEncryption: true,
      enableSignatures: true,
      testMode: true
    };

    // Create mock HTTP client
    mockHttpClient = {
      interceptors: {
        request: { use: jest.fn() },
        response: { use: jest.fn() }
      },
      post: jest.fn(),
      get: jest.fn(),
      put: jest.fn(),
      delete: jest.fn()
    };

    axios.create.mockReturnValue(mockHttpClient);

    connector = new EnhancedTCSBaNCSConnector(mockConfig);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('Initialization', () => {
    test('should initialize with correct configuration', () => {
      expect(connector.config.bankCode).toBe('TEST_BANCS');
      expect(connector.config.bankName).toBe('Test TCS BaNCS');
      expect(connector.bancsConfig.baseUrl).toBe('https://test-bancs.example.com');
      expect(connector.bancsConfig.institutionId).toBe('INST001');
      expect(connector.bancsConfig.branchCode).toBe('BRANCH001');
    });

    test('should use default configuration values', () => {
      const defaultConnector = new EnhancedTCSBaNCSConnector();
      
      expect(defaultConnector.config.bankCode).toBe('TCS_BANCS');
      expect(defaultConnector.config.bankName).toBe('TCS BaNCS');
    });

    test('should create HTTP client with correct configuration', () => {
      expect(axios.create).toHaveBeenCalledWith({
        baseURL: 'https://test-bancs.example.com',
        timeout: 30000,
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'User-Agent': 'LegacyBaaS-Enhanced-TCS-Connector/2.0'
        }
      });
    });

    test('should setup HTTP interceptors', () => {
      expect(mockHttpClient.interceptors.request.use).toHaveBeenCalled();
      expect(mockHttpClient.interceptors.response.use).toHaveBeenCalled();
    });

    test('should initialize authentication state', () => {
      expect(connector.accessToken).toBeNull();
      expect(connector.tokenExpiry).toBeNull();
    });
  });

  describe('HTTP Interceptors', () => {
    test('should add required headers in request interceptor', async () => {
      const requestConfig = {
        url: '/test',
        method: 'GET',
        headers: {}
      };

      // Mock authentication
      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
      connector.accessToken = 'test_token';
      connector.generateSignature = jest.fn().mockReturnValue('test_signature');

      const requestInterceptor = mockHttpClient.interceptors.request.use.mock.calls[0][0];
      const result = await requestInterceptor(requestConfig);

      expect(result.headers.Authorization).toBe('Bearer test_token');
      expect(result.headers['X-Bank-Code']).toBe('TEST_BANCS');
      expect(result.headers['X-Institution-ID']).toBe('INST001');
      expect(result.headers['X-Request-ID']).toBe('mocked-uuid-12345');
      expect(result.headers['X-Signature']).toBe('test_signature');
      expect(result.metadata).toBeDefined();
    });

    test('should handle request interceptor without authentication', async () => {
      const requestConfig = {
        url: '/test',
        method: 'GET',
        headers: {}
      };

      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
      connector.accessToken = null;

      const requestInterceptor = mockHttpClient.interceptors.request.use.mock.calls[0][0];
      const result = await requestInterceptor(requestConfig);

      expect(result.headers.Authorization).toBeUndefined();
    });

    test('should handle response interceptor success', () => {
      const response = {
        config: {
          metadata: { requestId: 'test-123', startTime: Date.now() - 1000 }
        },
        data: { success: true }
      };

      connector.updateMetrics = jest.fn();

      const responseInterceptor = mockHttpClient.interceptors.response.use.mock.calls[0][0];
      const result = responseInterceptor(response);

      expect(connector.updateMetrics).toHaveBeenCalledWith('success', expect.any(Number));
      expect(result).toBe(response);
    });

    test('should handle response interceptor error', async () => {
      const error = {
        config: {
          metadata: { requestId: 'test-123', startTime: Date.now() - 1000 }
        },
        response: {
          status: 500,
          data: { error: 'Server error' }
        }
      };

      connector.updateMetrics = jest.fn();
      connector.mapErrorCode = jest.fn().mockReturnValue(ERROR_CODES.NETWORK_ERROR);

      const responseErrorInterceptor = mockHttpClient.interceptors.response.use.mock.calls[0][1];
      
      await expect(responseErrorInterceptor(error)).rejects.toEqual(error);
      expect(connector.updateMetrics).toHaveBeenCalledWith('failure', expect.any(Number));
    });
  });

  describe('Authentication', () => {
    test('should authenticate successfully', async () => {
      const tokenResponse = {
        data: {
          access_token: 'new_access_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };

      mockHttpClient.post.mockResolvedValue(tokenResponse);

      await connector.authenticate();

      expect(mockHttpClient.post).toHaveBeenCalledWith('/oauth/token', {
        grant_type: 'client_credentials',
        client_id: 'test_client_id',
        client_secret: 'test_client_secret',
        scope: 'bancs:read bancs:write bancs:accounts bancs:payments'
      }, {
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
      });

      expect(connector.accessToken).toBe('new_access_token');
      expect(connector.tokenExpiry).toBeGreaterThan(Date.now());
    });

    test('should handle authentication failure', async () => {
      const error = new Error('Authentication failed');
      mockHttpClient.post.mockRejectedValue(error);

      await expect(connector.authenticate()).rejects.toThrow('Authentication failed');
    });

    test('should ensure authentication when token is valid', async () => {
      connector.accessToken = 'valid_token';
      connector.tokenExpiry = Date.now() + 3600000; // 1 hour from now

      const authenticateSpy = jest.spyOn(connector, 'authenticate');

      await connector.ensureAuthenticated();

      expect(authenticateSpy).not.toHaveBeenCalled();
    });

    test('should authenticate when token is expired', async () => {
      connector.accessToken = 'expired_token';
      connector.tokenExpiry = Date.now() - 1000; // 1 second ago

      const authenticateSpy = jest.spyOn(connector, 'authenticate').mockResolvedValue();

      await connector.ensureAuthenticated();

      expect(authenticateSpy).toHaveBeenCalled();
    });

    test('should authenticate when no token exists', async () => {
      connector.accessToken = null;
      connector.tokenExpiry = null;

      const authenticateSpy = jest.spyOn(connector, 'authenticate').mockResolvedValue();

      await connector.ensureAuthenticated();

      expect(authenticateSpy).toHaveBeenCalled();
    });
  });

  describe('Signature Generation', () => {
    test('should generate signature correctly', () => {
      const config = {
        method: 'POST',
        url: '/api/test',
        data: { test: 'data' },
        headers: {
          'X-Timestamp': '2023-01-01T00:00:00.000Z'
        }
      };

      const signature = connector.generateSignature(config);

      expect(signature).toBe('mocked-signature');
    });

    test('should handle signature generation without data', () => {
      const config = {
        method: 'GET',
        url: '/api/test',
        headers: {
          'X-Timestamp': '2023-01-01T00:00:00.000Z'
        }
      };

      const signature = connector.generateSignature(config);

      expect(signature).toBe('mocked-signature');
    });
  });

  describe('Connection Testing', () => {
    test('should test connection successfully', async () => {
      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
      mockHttpClient.get.mockResolvedValue({
        status: 200,
        data: { status: 'healthy', timestamp: new Date().toISOString() }
      });

      const result = await connector.testConnection();

      expect(result).toBe(true);
      expect(mockHttpClient.get).toHaveBeenCalledWith('/api/v1/health');
    });

    test('should handle connection test failure', async () => {
      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
      mockHttpClient.get.mockRejectedValue(new Error('Connection failed'));

      const result = await connector.testConnection();

      expect(result).toBe(false);
    });
  });

  describe('Account Operations', () => {
    beforeEach(() => {
      // Mock authentication for all account operations
      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
    });

    test('should get account details successfully', async () => {
      const accountData = {
        accountNumber: '1234567890',
        accountType: 'SAVINGS',
        accountStatus: 'ACTIVE',
        currency: 'USD',
        openDate: '2023-01-01',
        customer: {
          id: 'CUST001',
          name: 'John Doe',
          type: 'INDIVIDUAL'
        }
      };

      mockHttpClient.get.mockResolvedValue({
        data: accountData
      });

      const result = await connector.getAccountDetails('1234567890');

      expect(result).toEqual(expect.objectContaining(accountData));
      expect(mockHttpClient.get).toHaveBeenCalledWith('/api/v1/accounts/inquiry', {
        params: {
          accountNumber: '1234567890',
          includeBalance: true,
          includeCustomer: true,
          includeProducts: false
        }
      });
    });

    test('should get account details with options', async () => {
      const options = {
        includeCustomer: true,
        includeProducts: true
      };

      mockHttpClient.get.mockResolvedValue({
        data: { accountNumber: '1234567890' }
      });

      await connector.getAccountDetails('1234567890', options);

      expect(mockHttpClient.get).toHaveBeenCalledWith('/api/v1/accounts/inquiry', {
        params: {
          accountNumber: '1234567890',
          includeBalance: true,
          includeCustomer: true,
          includeProducts: true
        }
      });
    });

    test('should handle account details error', async () => {
      mockHttpClient.get.mockRejectedValue(new Error('Account not found'));

      await expect(connector.getAccountDetails('1234567890')).rejects.toThrow('Account details retrieval failed');
    });

    test('should check account balance successfully', async () => {
      const balanceData = {
        accountNumber: '1234567890',
        currency: 'USD',
        availableBalance: 50000.00,
        currentBalance: 52000.00,
        pendingTransactions: 2000.00,
        lastUpdated: '2023-01-01T12:00:00Z'
      };

      mockHttpClient.get.mockResolvedValue({
        data: balanceData
      });

      const result = await connector.checkAccountBalance('1234567890', 'USD');

      expect(result).toEqual(expect.objectContaining(balanceData));
      expect(mockHttpClient.get).toHaveBeenCalledWith('/api/v1/accounts/balance', {
        params: {
          accountNumber: '1234567890',
          currency: 'USD'
        }
      });
    });

    test('should check account balance without currency', async () => {
      mockHttpClient.get.mockResolvedValue({
        data: { availableBalance: 50000.00 }
      });

      await connector.checkAccountBalance('1234567890');

      expect(mockHttpClient.get).toHaveBeenCalledWith('/api/v1/accounts/balance', {
        params: {
          accountNumber: '1234567890'
        }
      });
    });
  });

  describe('Transaction Operations', () => {
    beforeEach(() => {
      // Mock authentication for all transaction operations
      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
    });

    test('should validate transaction successfully', async () => {
      const transaction = {
        id: 'TXN001',
        amount: 1000,
        currency: 'USD',
        sender: { account: '1234567890' },
        receiver: { account: '0987654321' }
      };

      const validationResponse = {
        isValid: true,
        validationId: 'VAL001',
        checks: {
          accountValidation: true,
          balanceCheck: true,
          complianceCheck: true
        }
      };

      mockHttpClient.post.mockResolvedValue({
        data: {
          status: 'success',
          data: validationResponse
        }
      });

      const result = await connector.validateTransaction(transaction);

      expect(result).toEqual(validationResponse);
      expect(mockHttpClient.post).toHaveBeenCalledWith('/api/v1/payments/validate', transaction);
    });

    test('should handle transaction validation failure', async () => {
      const transaction = {
        id: 'TXN001',
        amount: 1000,
        currency: 'USD'
      };

      mockHttpClient.post.mockRejectedValue(new Error('Validation failed'));

      await expect(connector.validateTransaction(transaction)).rejects.toThrow('Validation failed');
    });

    test('should process debit transaction successfully', async () => {
      const transaction = {
        id: 'TXN001',
        amount: 1000,
        currency: 'USD',
        sender: { account: '1234567890' }
      };

      const debitResponse = {
        debitId: 'DEB001',
        status: TRANSACTION_STATUS.CONFIRMED,
        transactionId: 'TXN001',
        processedAt: new Date().toISOString()
      };

      mockHttpClient.post.mockResolvedValue({
        data: {
          status: 'success',
          data: debitResponse
        }
      });

      const result = await connector.processDebit(transaction);

      expect(result).toEqual(debitResponse);
      expect(mockHttpClient.post).toHaveBeenCalledWith('/api/v1/payments/debit', transaction);
    });

    test('should process credit transaction successfully', async () => {
      const transaction = {
        id: 'TXN001',
        amount: 1000,
        currency: 'USD',
        receiver: { account: '0987654321' }
      };

      const creditResponse = {
        creditId: 'CRED001',
        status: TRANSACTION_STATUS.CONFIRMED,
        transactionId: 'TXN001',
        processedAt: new Date().toISOString()
      };

      mockHttpClient.post.mockResolvedValue({
        data: {
          status: 'success',
          data: creditResponse
        }
      });

      const result = await connector.processCredit(transaction);

      expect(result).toEqual(creditResponse);
      expect(mockHttpClient.post).toHaveBeenCalledWith('/api/v1/payments/credit', transaction);
    });

    test('should get transaction status successfully', async () => {
      const statusResponse = {
        transactionId: 'TXN001',
        status: TRANSACTION_STATUS.CONFIRMED,
        statusDetails: {
          debitStatus: 'COMPLETED',
          creditStatus: 'COMPLETED',
          settlementStatus: 'SETTLED'
        },
        lastUpdated: new Date().toISOString()
      };

      mockHttpClient.get.mockResolvedValue({
        data: {
          status: 'success',
          data: statusResponse
        }
      });

      const result = await connector.getTransactionStatus('TXN001');

      expect(result).toEqual(statusResponse);
      expect(mockHttpClient.get).toHaveBeenCalledWith('/api/v1/payments/status/TXN001');
    });
  });

  describe('Compliance Operations', () => {
    beforeEach(() => {
      // Mock authentication for all compliance operations
      connector.ensureAuthenticated = jest.fn().mockResolvedValue();
    });

    test('should perform compliance check successfully', async () => {
      const transaction = {
        id: 'TXN001',
        amount: 10000,
        currency: 'USD',
        sender: { account: '1234567890' },
        receiver: { account: '0987654321' }
      };

      const complianceResponse = {
        passed: true,
        riskScore: 25,
        checks: {
          aml: { passed: true, score: 20 },
          kyc: { passed: true, verified: true },
          sanctions: { passed: true, matches: [] },
          pep: { passed: true, matches: [] }
        },
        requiresManualReview: false
      };

      mockHttpClient.post.mockResolvedValue({
        data: {
          status: 'success',
          data: complianceResponse
        }
      });

      const result = await connector.performComplianceCheck(transaction);

      expect(result).toEqual(complianceResponse);
      expect(mockHttpClient.post).toHaveBeenCalledWith('/api/v1/compliance/check', transaction);
    });

    test('should handle compliance check failure', async () => {
      const transaction = { id: 'TXN001' };

      mockHttpClient.post.mockRejectedValue(new Error('Compliance service unavailable'));

      await expect(connector.performComplianceCheck(transaction)).rejects.toThrow('Compliance service unavailable');
    });
  });

  describe('Data Encryption', () => {
    test('should encrypt data when encryption is enabled', () => {
      connector.config.enableEncryption = true;
      
      const data = { sensitive: 'information' };
      const encrypted = connector.encryptData(data);

      expect(encrypted).toBeDefined();
      expect(encrypted).not.toEqual(data);
    });

    test('should decrypt data correctly', () => {
      connector.config.enableEncryption = true;
      
      const originalData = { sensitive: 'information' };
      const encrypted = connector.encryptData(originalData);
      const decrypted = connector.decryptData(encrypted);

      expect(decrypted).toEqual(originalData);
    });

    test('should return original data when encryption is disabled', () => {
      connector.config.enableEncryption = false;
      
      const data = { sensitive: 'information' };
      const result = connector.encryptData(data);

      expect(result).toEqual(data);
    });
  });

  describe('Webhook Handling', () => {
    test('should verify webhook signature correctly', () => {
      const payload = JSON.stringify({ event: 'payment.completed' });
      const signature = 'valid_signature';

      connector.generateWebhookSignature = jest.fn().mockReturnValue(signature);

      const isValid = connector.verifyWebhookSignature(payload, signature);

      expect(isValid).toBe(true);
      expect(connector.generateWebhookSignature).toHaveBeenCalledWith(payload);
    });

    test('should reject invalid webhook signature', () => {
      const payload = JSON.stringify({ event: 'payment.completed' });
      const signature = 'invalid_signature';

      connector.generateWebhookSignature = jest.fn().mockReturnValue('valid_signature');

      const isValid = connector.verifyWebhookSignature(payload, signature);

      expect(isValid).toBe(false);
    });

    test('should process webhook notification successfully', () => {
      const notification = {
        event: 'payment.completed',
        data: {
          transactionId: 'TXN001',
          status: 'COMPLETED'
        },
        timestamp: new Date().toISOString()
      };

      const emitSpy = jest.spyOn(connector, 'emit');

      connector.processWebhookNotification(notification);

      expect(emitSpy).toHaveBeenCalledWith('webhook:notification', notification);
      expect(emitSpy).toHaveBeenCalledWith('payment:completed', notification.data);
    });
  });

  describe('Error Handling', () => {
    test('should map authentication errors correctly', () => {
      const error = new Error('Invalid credentials provided');
      
      const mappedCode = connector.mapErrorCode(error);
      
      expect(mappedCode).toBe(ERROR_CODES.AUTHENTICATION_FAILED);
    });

    test('should map network errors correctly', () => {
      const error = new Error('Network timeout occurred');
      
      const mappedCode = connector.mapErrorCode(error);
      
      expect(mappedCode).toBe(ERROR_CODES.TIMEOUT_ERROR);
    });

    test('should map balance errors correctly', () => {
      const error = new Error('Insufficient funds available');
      
      const mappedCode = connector.mapErrorCode(error);
      
      expect(mappedCode).toBe(ERROR_CODES.INSUFFICIENT_FUNDS);
    });

    test('should return general error for unmapped errors', () => {
      const error = new Error('Unknown error occurred');
      
      const mappedCode = connector.mapErrorCode(error);
      
      expect(mappedCode).toBe(ERROR_CODES.GENERAL_ERROR);
    });
  });

  describe('Health and Status', () => {
    test('should return health status successfully', async () => {
      connector.testConnection = jest.fn().mockResolvedValue(true);

      const healthStatus = await connector.getHealthStatus();

      expect(healthStatus).toMatchObject({
        status: 'healthy',
        bankCode: 'TEST_BANCS',
        isConnected: true,
        timestamp: expect.any(String)
      });
    });

    test('should return unhealthy status when connection fails', async () => {
      connector.testConnection = jest.fn().mockResolvedValue(false);

      const healthStatus = await connector.getHealthStatus();

      expect(healthStatus.status).toBe('unhealthy');
      expect(healthStatus.isConnected).toBe(false);
    });

    test('should return comprehensive connector status', () => {
      connector.metrics = {
        totalRequests: 100,
        successfulRequests: 95,
        failedRequests: 5,
        averageResponseTime: 250
      };

      const status = connector.getStatus();

      expect(status).toMatchObject({
        bankCode: 'TEST_BANCS',
        bankName: 'Test TCS BaNCS',
        isConnected: expect.any(Boolean),
        metrics: expect.any(Object),
        activeTransactions: expect.any(Number),
        completedTransactions: expect.any(Number)
      });
    });
  });

  describe('Cleanup', () => {
    test('should cleanup resources properly', async () => {
      connector.httpClient = mockHttpClient;
      
      await connector.cleanup();

      expect(connector.accessToken).toBeNull();
      expect(connector.tokenExpiry).toBeNull();
    });
  });
});