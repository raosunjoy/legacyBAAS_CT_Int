/**
 * Multi-Bank Architecture Tests
 * Comprehensive test suite for the multi-bank connector framework
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage: Multi-Bank Connector Factory & Base Classes
 */

const { BankingConnectorFactory, SUPPORTED_BANKS, TRANSACTION_STATUS, ERROR_CODES } = require('../base/connector-factory');
const { BaseBankingConnector } = require('../base/base-banking-connector');
const { EnhancedTCSBaNCSConnector } = require('../tcs-bancs/enhanced-bancs-connector');
const { createMultiBankConfig, validateBankConfig } = require('../config/multi-bank-config');

// Mock HTTP client for testing
jest.mock('axios');
const axios = require('axios');

describe('Multi-Bank Architecture', () => {
  let factory;
  let mockConfig;

  beforeEach(() => {
    // Reset mocks
    jest.clearAllMocks();
    
    // Create test configuration
    mockConfig = createMultiBankConfig([
      {
        type: SUPPORTED_BANKS.TCS_BANCS,
        bankCode: 'TEST_BANCS',
        config: {
          clientId: 'test_client_id',
          clientSecret: 'test_client_secret',
          baseUrl: 'https://test-bancs.example.com',
          testMode: true
        }
      }
    ], 'development');

    // Initialize factory
    factory = new BankingConnectorFactory(mockConfig.factory);
  });

  afterEach(async () => {
    if (factory) {
      await factory.cleanup();
    }
  });

  describe('BankingConnectorFactory', () => {
    describe('Factory Initialization', () => {
      test('should initialize with default configuration', () => {
        const testFactory = new BankingConnectorFactory();
        
        expect(testFactory.config.maxConnectorsPerBank).toBe(5);
        expect(testFactory.config.loadBalancingStrategy).toBe('round-robin');
        expect(testFactory.config.enableFailover).toBe(true);
        expect(testFactory.connectors).toBeInstanceOf(Map);
        expect(testFactory.factoryMetrics.totalConnectors).toBe(0);
      });

      test('should initialize with custom configuration', () => {
        const customConfig = {
          maxConnectorsPerBank: 10,
          loadBalancingStrategy: 'least-connections',
          enableFailover: false
        };
        
        const testFactory = new BankingConnectorFactory(customConfig);
        
        expect(testFactory.config.maxConnectorsPerBank).toBe(10);
        expect(testFactory.config.loadBalancingStrategy).toBe('least-connections');
        expect(testFactory.config.enableFailover).toBe(false);
      });
    });

    describe('Bank Registration', () => {
      test('should register a bank successfully', async () => {
        const bankConfig = mockConfig.banks[0];
        
        // Mock successful authentication
        axios.create.mockReturnValue({
          post: jest.fn().mockResolvedValue({
            data: {
              access_token: 'test_token',
              expires_in: 3600,
              token_type: 'Bearer'
            }
          }),
          get: jest.fn().mockResolvedValue({ status: 200 }),
          interceptors: {
            request: { use: jest.fn() },
            response: { use: jest.fn() }
          }
        });

        await factory.registerBank(bankConfig.bankCode, bankConfig);
        
        expect(factory.connectorConfigs.has(bankConfig.bankCode)).toBe(true);
        expect(factory.connectors.has(bankConfig.bankCode)).toBe(true);
        expect(factory.factoryMetrics.totalConnectors).toBeGreaterThan(0);
      });

      test('should validate bank configuration during registration', async () => {
        const invalidConfig = {
          type: SUPPORTED_BANKS.TCS_BANCS,
          // Missing required fields
        };

        await expect(
          factory.registerBank('INVALID_BANK', invalidConfig)
        ).rejects.toThrow();
      });

      test('should emit registration events', async () => {
        const bankConfig = mockConfig.banks[0];
        const eventSpy = jest.fn();
        
        factory.on('bank:registered', eventSpy);

        // Mock successful registration
        axios.create.mockReturnValue({
          post: jest.fn().mockResolvedValue({
            data: { access_token: 'test_token', expires_in: 3600 }
          }),
          get: jest.fn().mockResolvedValue({ status: 200 }),
          interceptors: {
            request: { use: jest.fn() },
            response: { use: jest.fn() }
          }
        });

        await factory.registerBank(bankConfig.bankCode, bankConfig);
        
        expect(eventSpy).toHaveBeenCalledWith(
          expect.objectContaining({
            bankCode: bankConfig.bankCode,
            connectorsCreated: expect.any(Number),
            timestamp: expect.any(String)
          })
        );
      });
    });

    describe('Load Balancing', () => {
      let testConnectors;

      beforeEach(() => {
        testConnectors = [
          { isConnected: true, connectionId: 'conn1', activeTransactions: new Map() },
          { isConnected: true, connectionId: 'conn2', activeTransactions: new Map() },
          { isConnected: true, connectionId: 'conn3', activeTransactions: new Map() }
        ];
      });

      test('should use round-robin load balancing', () => {
        factory.config.loadBalancingStrategy = 'round-robin';
        factory.connectors.set('TEST_BANK', testConnectors);
        factory.roundRobinCounters.set('TEST_BANK', 0);

        const connector1 = factory.getConnector('TEST_BANK');
        const connector2 = factory.getConnector('TEST_BANK');
        const connector3 = factory.getConnector('TEST_BANK');
        const connector4 = factory.getConnector('TEST_BANK'); // Should wrap around

        expect(connector1).toBe(testConnectors[0]);
        expect(connector2).toBe(testConnectors[1]);
        expect(connector3).toBe(testConnectors[2]);
        expect(connector4).toBe(testConnectors[0]);
      });

      test('should use least-connections load balancing', () => {
        factory.config.loadBalancingStrategy = 'least-connections';
        factory.connectors.set('TEST_BANK', testConnectors);

        // Add some active transactions to simulate load
        testConnectors[0].activeTransactions.set('tx1', {});
        testConnectors[0].activeTransactions.set('tx2', {});
        testConnectors[1].activeTransactions.set('tx3', {});
        // testConnectors[2] has no active transactions

        const connector = factory.getConnector('TEST_BANK');
        expect(connector).toBe(testConnectors[2]); // Least connections
      });

      test('should use random load balancing', () => {
        factory.config.loadBalancingStrategy = 'random';
        factory.connectors.set('TEST_BANK', testConnectors);

        const connector = factory.getConnector('TEST_BANK');
        expect(testConnectors).toContain(connector);
      });

      test('should filter out unhealthy connectors', () => {
        testConnectors[0].isConnected = false; // Unhealthy
        testConnectors[1].isConnected = false; // Unhealthy
        // testConnectors[2] is healthy

        factory.connectors.set('TEST_BANK', testConnectors);

        const connector = factory.getConnector('TEST_BANK');
        expect(connector).toBe(testConnectors[2]);
      });

      test('should throw error when no healthy connectors available', () => {
        testConnectors.forEach(conn => { conn.isConnected = false; });
        factory.connectors.set('TEST_BANK', testConnectors);
        factory.config.enableFailover = false;

        expect(() => factory.getConnector('TEST_BANK'))
          .toThrow('No healthy connectors available for bank TEST_BANK');
      });
    });

    describe('Transaction Processing', () => {
      test('should process transaction successfully', async () => {
        const mockConnector = {
          isConnected: true,
          connectionId: 'test_conn',
          activeTransactions: new Map(),
          processTransaction: jest.fn().mockResolvedValue({
            transactionId: 'tx123',
            status: TRANSACTION_STATUS.CONFIRMED,
            timestamp: new Date().toISOString()
          })
        };

        factory.connectors.set('TEST_BANK', [mockConnector]);

        const transaction = {
          id: 'tx123',
          amount: 1000,
          currency: 'USD',
          sender: { account: '123456' },
          receiver: { account: '789012' }
        };

        const result = await factory.processTransaction('TEST_BANK', transaction);

        expect(mockConnector.processTransaction).toHaveBeenCalledWith(transaction);
        expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
        expect(factory.factoryMetrics.totalTransactions).toBe(1);
      });

      test('should retry failed transactions', async () => {
        const mockConnector = {
          isConnected: true,
          connectionId: 'test_conn',
          activeTransactions: new Map(),
          processTransaction: jest.fn()
            .mockRejectedValueOnce(new Error('Network error'))
            .mockRejectedValueOnce(new Error('Timeout'))
            .mockResolvedValue({
              transactionId: 'tx123',
              status: TRANSACTION_STATUS.CONFIRMED
            })
        };

        factory.connectors.set('TEST_BANK', [mockConnector]);
        factory.config.retryAttempts = 3;

        const transaction = { id: 'tx123', amount: 100, currency: 'USD' };

        const result = await factory.processTransaction('TEST_BANK', transaction);

        expect(mockConnector.processTransaction).toHaveBeenCalledTimes(3);
        expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      });

      test('should emit transaction events', async () => {
        const mockConnector = {
          isConnected: true,
          connectionId: 'test_conn',
          activeTransactions: new Map(),
          processTransaction: jest.fn().mockResolvedValue({
            transactionId: 'tx123',
            status: TRANSACTION_STATUS.CONFIRMED
          })
        };

        factory.connectors.set('TEST_BANK', [mockConnector]);

        const completedSpy = jest.fn();
        factory.on('transaction:completed', completedSpy);

        const transaction = { id: 'tx123' };
        await factory.processTransaction('TEST_BANK', transaction);

        expect(completedSpy).toHaveBeenCalledWith(
          expect.objectContaining({
            bankCode: 'TEST_BANK',
            transactionId: 'tx123'
          })
        );
      });
    });

    describe('Failover Handling', () => {
      test('should handle failover when no healthy connectors', async () => {
        const unhealthyConnector = {
          isConnected: false,
          connectionId: 'unhealthy_conn',
          activeTransactions: new Map()
        };

        factory.connectors.set('TEST_BANK', [unhealthyConnector]);
        factory.config.enableFailover = true;

        // Mock successful connector creation during failover
        const mockNewConnector = {
          isConnected: true,
          connectionId: 'new_conn',
          activeTransactions: new Map(),
          initialize: jest.fn().mockResolvedValue()
        };

        // Mock factory.createConnector to return the new connector
        jest.spyOn(factory, 'createConnector').mockResolvedValue(mockNewConnector);

        const failoverSpy = jest.fn();
        factory.on('failover:success', failoverSpy);

        // This should trigger failover
        expect(() => factory.getConnector('TEST_BANK')).toThrow();

        // Wait for failover attempt
        await new Promise(resolve => setTimeout(resolve, 100));

        expect(factory.createConnector).toHaveBeenCalledWith('TEST_BANK');
        expect(factory.factoryMetrics.failoverEvents).toBeGreaterThan(0);
      });
    });

    describe('Factory Status and Metrics', () => {
      test('should return comprehensive factory status', () => {
        const status = factory.getFactoryStatus();

        expect(status).toHaveProperty('factoryMetrics');
        expect(status).toHaveProperty('connectorStatus');
        expect(status).toHaveProperty('registeredBanks');
        expect(status).toHaveProperty('config');
        
        expect(status.factoryMetrics).toHaveProperty('totalConnectors');
        expect(status.factoryMetrics).toHaveProperty('activeConnectors');
        expect(status.factoryMetrics).toHaveProperty('uptime');
      });

      test('should return health status for all connectors', async () => {
        const mockConnector = {
          getHealthStatus: jest.fn().mockResolvedValue({
            status: 'healthy',
            connectionId: 'test_conn',
            timestamp: new Date().toISOString()
          })
        };

        factory.connectors.set('TEST_BANK', [mockConnector]);

        const healthStatus = await factory.getHealthStatus();

        expect(healthStatus).toHaveProperty('factoryStatus', 'healthy');
        expect(healthStatus).toHaveProperty('banks');
        expect(healthStatus.banks.TEST_BANK).toHaveProperty('overall');
        expect(healthStatus.banks.TEST_BANK).toHaveProperty('connectors');
      });
    });
  });

  describe('BaseBankingConnector', () => {
    let baseConnector;

    beforeEach(() => {
      // Create a test implementation of BaseBankingConnector
      class TestBankingConnector extends BaseBankingConnector {
        async authenticate() {
          this.accessToken = 'test_token';
        }

        async testConnection() {
          return true;
        }

        async getAccountDetails(accountNumber) {
          return { accountNumber, status: 'ACTIVE' };
        }

        async checkAccountBalance(accountNumber) {
          return { accountNumber, availableBalance: 1000 };
        }

        async validateTransaction(transaction) {
          return { isValid: true, validationId: 'val123' };
        }

        async processDebit(transaction) {
          return { debitId: 'deb123', status: TRANSACTION_STATUS.CONFIRMED };
        }

        async processCredit(transaction) {
          return { creditId: 'cred123', status: TRANSACTION_STATUS.CONFIRMED };
        }

        async getTransactionStatus(transactionId) {
          return { transactionId, status: TRANSACTION_STATUS.CONFIRMED };
        }
      }

      baseConnector = new TestBankingConnector({
        bankCode: 'TEST_BANK',
        bankName: 'Test Bank',
        clientId: 'test_client',
        clientSecret: 'test_secret'
      });
    });

    afterEach(async () => {
      await baseConnector.cleanup();
    });

    describe('Initialization', () => {
      test('should initialize with proper configuration', async () => {
        await baseConnector.initialize();

        expect(baseConnector.isConnected).toBe(true);
        expect(baseConnector.config.bankCode).toBe('TEST_BANK');
        expect(baseConnector.config.bankName).toBe('Test Bank');
        expect(baseConnector.connectionId).toBeDefined();
      });

      test('should emit connected event on successful initialization', async () => {
        const eventSpy = jest.fn();
        baseConnector.on('connected', eventSpy);

        await baseConnector.initialize();

        expect(eventSpy).toHaveBeenCalledWith(
          expect.objectContaining({
            bankCode: 'TEST_BANK',
            connectionId: baseConnector.connectionId
          })
        );
      });

      test('should emit error event on initialization failure', async () => {
        // Override authenticate to fail
        baseConnector.authenticate = jest.fn().mockRejectedValue(new Error('Auth failed'));

        const errorSpy = jest.fn();
        baseConnector.on('error', errorSpy);

        await expect(baseConnector.initialize()).rejects.toThrow('Auth failed');
        expect(errorSpy).toHaveBeenCalled();
      });
    });

    describe('Transaction Processing', () => {
      beforeEach(async () => {
        await baseConnector.initialize();
      });

      test('should process complete transaction successfully', async () => {
        const transaction = {
          id: 'tx123',
          type: 'transfer',
          amount: 500,
          currency: 'USD',
          sender: { account: '123456', name: 'John Doe' },
          receiver: { account: '789012', name: 'Jane Smith' }
        };

        const result = await baseConnector.processTransaction(transaction);

        expect(result).toHaveProperty('transactionId', 'tx123');
        expect(result).toHaveProperty('status', TRANSACTION_STATUS.CONFIRMED);
        expect(result).toHaveProperty('debitResult');
        expect(result).toHaveProperty('creditResult');
        expect(result).toHaveProperty('responseTime');
        expect(baseConnector.metrics.transactionsProcessed).toBe(1);
      });

      test('should handle transaction validation failure', async () => {
        baseConnector.validateTransaction = jest.fn().mockResolvedValue({
          isValid: false,
          errorMessage: 'Insufficient funds'
        });

        const transaction = {
          id: 'tx456',
          amount: 10000,
          currency: 'USD',
          sender: { account: '123456' },
          receiver: { account: '789012' }
        };

        await expect(baseConnector.processTransaction(transaction))
          .rejects.toThrow('Transaction validation failed: Insufficient funds');
      });

      test('should emit transaction events', async () => {
        const completedSpy = jest.fn();
        const failedSpy = jest.fn();
        
        baseConnector.on('transaction:completed', completedSpy);
        baseConnector.on('transaction:failed', failedSpy);

        const transaction = {
          id: 'tx789',
          type: 'debit',
          amount: 100,
          currency: 'USD',
          sender: { account: '123456' },
          receiver: { account: '789012' }
        };

        await baseConnector.processTransaction(transaction);

        expect(completedSpy).toHaveBeenCalledWith(
          expect.objectContaining({
            transactionId: 'tx789',
            status: TRANSACTION_STATUS.CONFIRMED
          })
        );
        expect(failedSpy).not.toHaveBeenCalled();
      });
    });

    describe('Error Handling and Mapping', () => {
      test('should map errors to standard error codes', () => {
        const authError = new Error('Authentication failed');
        const balanceError = new Error('Insufficient funds available');
        const accountError = new Error('Invalid account number');
        const complianceError = new Error('Compliance check failed');
        const timeoutError = new Error('Request timeout occurred');

        expect(baseConnector.mapErrorCode(authError)).toBe(ERROR_CODES.AUTHENTICATION_FAILED);
        expect(baseConnector.mapErrorCode(balanceError)).toBe(ERROR_CODES.INSUFFICIENT_FUNDS);
        expect(baseConnector.mapErrorCode(accountError)).toBe(ERROR_CODES.INVALID_ACCOUNT);
        expect(baseConnector.mapErrorCode(complianceError)).toBe(ERROR_CODES.COMPLIANCE_FAILURE);
        expect(baseConnector.mapErrorCode(timeoutError)).toBe(ERROR_CODES.TIMEOUT_ERROR);
      });
    });

    describe('Metrics and Status', () => {
      beforeEach(async () => {
        await baseConnector.initialize();
      });

      test('should track performance metrics', () => {
        baseConnector.updateMetrics('success', 150);
        baseConnector.updateMetrics('success', 200);
        baseConnector.updateMetrics('failure', 300);

        const metrics = baseConnector.metrics;
        expect(metrics.totalRequests).toBe(3);
        expect(metrics.successfulRequests).toBe(2);
        expect(metrics.failedRequests).toBe(1);
        expect(metrics.averageResponseTime).toBeCloseTo(216.67, 1);
      });

      test('should return comprehensive status', () => {
        const status = baseConnector.getStatus();

        expect(status).toHaveProperty('bankCode', 'TEST_BANK');
        expect(status).toHaveProperty('bankName', 'Test Bank');
        expect(status).toHaveProperty('isConnected', true);
        expect(status).toHaveProperty('metrics');
        expect(status.metrics).toHaveProperty('successRate');
        expect(status).toHaveProperty('activeTransactions');
        expect(status).toHaveProperty('completedTransactions');
      });

      test('should return health status', async () => {
        const healthStatus = await baseConnector.getHealthStatus();

        expect(healthStatus).toHaveProperty('status', 'healthy');
        expect(healthStatus).toHaveProperty('bankCode', 'TEST_BANK');
        expect(healthStatus).toHaveProperty('isConnected', true);
        expect(healthStatus).toHaveProperty('timestamp');
      });
    });
  });

  describe('Configuration Management', () => {
    describe('Bank Configuration Validation', () => {
      test('should validate valid configuration', () => {
        const validConfig = {
          type: SUPPORTED_BANKS.TCS_BANCS,
          bankCode: 'TEST_BANCS',
          config: {
            clientId: 'test_client',
            clientSecret: 'test_secret',
            baseUrl: 'https://test.example.com',
            enableEncryption: true,
            enableHealthChecks: true
          }
        };

        const result = validateBankConfig(validConfig);
        expect(result.isValid).toBe(true);
        expect(result.errors).toHaveLength(0);
      });

      test('should identify missing required fields', () => {
        const invalidConfig = {
          type: SUPPORTED_BANKS.TCS_BANCS,
          config: {
            // Missing clientId, clientSecret, baseUrl
            enableEncryption: true
          }
        };

        const result = validateBankConfig(invalidConfig);
        expect(result.isValid).toBe(false);
        expect(result.errors).toContain('Client ID is required');
        expect(result.errors).toContain('Client secret is required');
        expect(result.errors).toContain('Base URL is required');
      });

      test('should provide warnings for recommended fields', () => {
        const configWithWarnings = {
          type: SUPPORTED_BANKS.TCS_BANCS,
          config: {
            clientId: 'test_client',
            clientSecret: 'test_secret',
            baseUrl: 'https://test.example.com',
            enableEncryption: false,
            enableHealthChecks: false
          }
        };

        const result = validateBankConfig(configWithWarnings);
        expect(result.isValid).toBe(true);
        expect(result.warnings).toContain('Encryption is disabled, consider enabling for production');
        expect(result.warnings).toContain('Health checks are disabled');
      });
    });

    describe('Multi-Bank Configuration Creation', () => {
      test('should create configuration for multiple banks', () => {
        const banks = [
          SUPPORTED_BANKS.TCS_BANCS,
          {
            type: SUPPORTED_BANKS.FINACLE,
            bankCode: 'CUSTOM_FINACLE',
            config: { timeout: 25000 }
          }
        ];

        const config = createMultiBankConfig(banks, 'production');

        expect(config.banks).toHaveLength(2);
        expect(config.banks[0].type).toBe(SUPPORTED_BANKS.TCS_BANCS);
        expect(config.banks[1].type).toBe(SUPPORTED_BANKS.FINACLE);
        expect(config.banks[1].bankCode).toBe('CUSTOM_FINACLE');
        expect(config.environment).toBe('production');
        expect(config.factory.testMode).toBe(false);
      });
    });
  });
});

// Additional integration tests
describe('Multi-Bank Integration Tests', () => {
  test('should handle multiple banks simultaneously', async () => {
    const factory = new BankingConnectorFactory({
      maxConnectorsPerBank: 2,
      loadBalancingStrategy: 'round-robin'
    });

    // Mock multiple bank configurations
    const mockConfigs = [
      {
        bankCode: 'BANK_A',
        type: SUPPORTED_BANKS.TCS_BANCS,
        config: {
          clientId: 'bank_a_client',
          clientSecret: 'bank_a_secret',
          baseUrl: 'https://bank-a.example.com',
          testMode: true
        }
      },
      {
        bankCode: 'BANK_B', 
        type: SUPPORTED_BANKS.TCS_BANCS,
        config: {
          clientId: 'bank_b_client',
          clientSecret: 'bank_b_secret',
          baseUrl: 'https://bank-b.example.com',
          testMode: true
        }
      }
    ];

    // Mock HTTP clients for both banks
    axios.create.mockReturnValue({
      post: jest.fn().mockResolvedValue({
        data: { access_token: 'test_token', expires_in: 3600 }
      }),
      get: jest.fn().mockResolvedValue({ status: 200 }),
      interceptors: {
        request: { use: jest.fn() },
        response: { use: jest.fn() }
      }
    });

    // Register both banks
    for (const config of mockConfigs) {
      await factory.registerBank(config.bankCode, config);
    }

    // Verify both banks are registered
    expect(factory.connectorConfigs.size).toBe(2);
    expect(factory.connectors.size).toBe(2);

    // Test getting connectors for both banks
    const connectorA = factory.getConnector('BANK_A');
    const connectorB = factory.getConnector('BANK_B');

    expect(connectorA).toBeDefined();
    expect(connectorB).toBeDefined();
    expect(connectorA.config.bankCode).toBe('BANK_A');
    expect(connectorB.config.bankCode).toBe('BANK_B');

    await factory.cleanup();
  });
});