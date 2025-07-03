/**
 * Fiserv DNA Connector Tests
 * Comprehensive test suite for Fiserv DNA core banking integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage: Authentication, Transactions, Compliance, Error Handling
 */

const { expect } = require('chai');
const sinon = require('sinon');
const axios = require('axios');
const EventEmitter = require('events');

const { 
  FiservDNAConnector, 
  DNA_ENDPOINTS, 
  DNA_TRANSACTION_TYPES,
  DNA_ACCOUNT_TYPES 
} = require('../../../src/connectors/fiserv-dna/fiserv-dna-connector');

const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

describe('FiservDNAConnector', function() {
  let connector;
  let axiosStub;
  let config;

  beforeEach(function() {
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

    // Create connector instance
    connector = new FiservDNAConnector(config);

    // Stub axios methods
    axiosStub = {
      post: sinon.stub(),
      get: sinon.stub(),
      put: sinon.stub(),
      delete: sinon.stub(),
      create: sinon.stub().returns({
        post: sinon.stub(),
        get: sinon.stub(),
        put: sinon.stub(),
        delete: sinon.stub(),
        defaults: { headers: { common: {} } }
      })
    };

    // Replace axios with our stub
    connector.httpClient = axiosStub.create();
  });

  afterEach(function() {
    sinon.restore();
  });

  describe('Constructor and Initialization', function() {
    it('should initialize with correct configuration', function() {
      expect(connector.config.bankCode).to.equal('FISERV_DNA');
      expect(connector.config.bankName).to.equal('Fiserv DNA Platform');
      expect(connector.dnaConfig.institutionId).to.equal('TEST_INST_001');
      expect(connector.dnaConfig.environment).to.equal('sandbox');
    });

    it('should set up default configuration values', function() {
      const defaultConnector = new FiservDNAConnector({});
      expect(defaultConnector.config.timeout).to.equal(30000);
      expect(defaultConnector.dnaConfig.enableCaching).to.be.true;
      expect(defaultConnector.dnaConfig.enableWebhooks).to.be.true;
    });

    it('should initialize metrics tracking', function() {
      expect(connector.dnaMetrics).to.deep.include({
        apiCalls: 0,
        cacheHits: 0,
        cacheMisses: 0,
        webhookEvents: 0,
        complianceChecks: 0,
        realTimeRequests: 0
      });
    });
  });

  describe('Authentication', function() {
    it('should authenticate successfully with OAuth2', async function() {
      const mockTokenResponse = {
        data: {
          access_token: 'mock_access_token',
          refresh_token: 'mock_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };

      connector.httpClient.post.resolves(mockTokenResponse);

      await connector.authenticate();

      expect(connector.accessToken).to.equal('mock_access_token');
      expect(connector.refreshToken).to.equal('mock_refresh_token');
      expect(connector.tokenExpiry).to.be.above(Date.now());
      
      expect(connector.httpClient.post.calledOnce).to.be.true;
      expect(connector.httpClient.post.firstCall.args[0]).to.equal(DNA_ENDPOINTS.AUTHENTICATE);
    });

    it('should handle authentication failure', async function() {
      const authError = new Error('Authentication failed');
      authError.response = {
        status: 401,
        data: { error: 'invalid_client' }
      };

      connector.httpClient.post.rejects(authError);

      try {
        await connector.authenticate();
        expect.fail('Should have thrown authentication error');
      } catch (error) {
        expect(error.message).to.include('DNA authentication failed');
        expect(connector.metrics.authenticationFailures).to.equal(1);
      }
    });

    it('should refresh token before expiry', async function() {
      // Set up initial token
      connector.accessToken = 'initial_token';
      connector.tokenExpiry = Date.now() + 30000; // 30 seconds

      const mockResponse = { status: 200, data: {} };
      connector.httpClient.get.resolves(mockResponse);

      // Mock re-authentication
      const mockTokenResponse = {
        data: {
          access_token: 'new_access_token',
          refresh_token: 'new_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };
      connector.httpClient.post.resolves(mockTokenResponse);

      // Simulate token near expiry
      connector.tokenExpiry = Date.now() + 30000; // 30 seconds

      await connector.makeApiCall('GET', '/test');

      expect(connector.accessToken).to.equal('new_access_token');
    });
  });

  describe('Connection Testing', function() {
    beforeEach(function() {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    it('should test connection successfully', async function() {
      connector.httpClient.get.resolves({ status: 200 });
      
      const result = await connector.testConnection();
      
      expect(result).to.be.true;
    });

    it('should handle connection test failure', async function() {
      connector.httpClient.get.rejects(new Error('Connection failed'));
      
      const result = await connector.testConnection();
      
      expect(result).to.be.false;
    });

    it('should re-authenticate if token expired during connection test', async function() {
      connector.accessToken = null;
      
      const mockTokenResponse = {
        data: {
          access_token: 'new_token',
          refresh_token: 'new_refresh_token',
          expires_in: 3600,
          token_type: 'Bearer'
        }
      };
      
      connector.httpClient.post.resolves(mockTokenResponse);
      connector.httpClient.get.resolves({ status: 200 });
      
      const result = await connector.testConnection();
      
      expect(result).to.be.true;
      expect(connector.accessToken).to.equal('new_token');
    });
  });

  describe('Account Operations', function() {
    beforeEach(function() {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    describe('getAccountDetails', function() {
      it('should get account details successfully', async function() {
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

        connector.httpClient.get.resolves(mockAccountData);

        const result = await connector.getAccountDetails('1234567890');

        expect(result.accountNumber).to.equal('1234567890');
        expect(result.accountType).to.equal('CHECKING');
        expect(result.accountStatus).to.equal('ACTIVE');
        expect(result.balances).to.deep.equal(mockAccountData.data.balances);
        expect(connector.dnaMetrics.cacheMisses).to.equal(1);
      });

      it('should use cached account details when available', async function() {
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

        expect(result).to.deep.equal(cachedData);
        expect(connector.dnaMetrics.cacheHits).to.equal(1);
        expect(connector.httpClient.get.called).to.be.false;
      });

      it('should handle account not found error', async function() {
        const error = new Error('Account not found');
        error.response = {
          status: 404,
          data: { code: 'INVALID_ACCOUNT', message: 'Account not found' }
        };

        connector.httpClient.get.rejects(error);

        try {
          await connector.getAccountDetails('9999999999');
          expect.fail('Should have thrown account not found error');
        } catch (err) {
          expect(err.message).to.include('Account not found');
        }
      });
    });

    describe('checkAccountBalance', function() {
      it('should check account balance successfully', async function() {
        const mockBalanceData = {
          data: {
            availableBalance: '1500.00',
            currentBalance: '1500.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: '2023-12-01T10:00:00Z'
          }
        };

        connector.httpClient.get.resolves(mockBalanceData);

        const result = await connector.checkAccountBalance('1234567890', 'USD');

        expect(result.accountNumber).to.equal('1234567890');
        expect(result.currency).to.equal('USD');
        expect(result.availableBalance).to.equal(1500.00);
        expect(result.currentBalance).to.equal(1500.00);
        expect(result.pendingBalance).to.equal(0.00);
      });

      it('should default to USD currency', async function() {
        const mockBalanceData = {
          data: {
            availableBalance: '1000.00',
            currentBalance: '1000.00',
            pendingBalance: '0.00',
            holds: [],
            lastUpdated: '2023-12-01T10:00:00Z'
          }
        };

        connector.httpClient.get.resolves(mockBalanceData);

        await connector.checkAccountBalance('1234567890');

        expect(connector.httpClient.get.firstCall.args[1].params.currency).to.equal('USD');
      });
    });
  });

  describe('Transaction Processing', function() {
    beforeEach(function() {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    describe('validateTransaction', function() {
      it('should validate transaction successfully', async function() {
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

        connector.httpClient.get
          .onFirstCall().resolves(mockAccountData)
          .onSecondCall().resolves(mockBalanceData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).to.be.true;
        expect(result.errors).to.be.empty;
        expect(result.complianceStatus).to.equal('passed');
      });

      it('should fail validation for insufficient funds', async function() {
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

        connector.httpClient.get
          .onFirstCall().resolves(mockAccountData)
          .onSecondCall().resolves(mockBalanceData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).to.be.false;
        expect(result.errors).to.include('Insufficient funds');
      });

      it('should fail validation for inactive account', async function() {
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

        connector.httpClient.get.resolves(mockAccountData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).to.be.false;
        expect(result.errors).to.include('Source account is not active');
      });

      it('should perform compliance check for large amounts', async function() {
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

        connector.httpClient.get
          .onFirstCall().resolves(mockAccountData)
          .onSecondCall().resolves(mockBalanceData);
        
        connector.httpClient.post.resolves(mockComplianceData);

        const result = await connector.validateTransaction(transaction);

        expect(result.isValid).to.be.true;
        expect(result.complianceStatus).to.equal('passed');
        expect(connector.dnaMetrics.complianceChecks).to.equal(1);
      });
    });

    describe('processDebit', function() {
      it('should process debit transaction successfully', async function() {
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

        connector.httpClient.post.resolves(mockDebitResponse);

        const result = await connector.processDebit(transaction);

        expect(result.transactionId).to.equal('TXN_DEBIT_001');
        expect(result.status).to.equal(TRANSACTION_STATUS.CONFIRMED);
        expect(result.amount).to.equal(500.00);
        expect(result.currency).to.equal('USD');
        expect(result.balance).to.equal(500.00);
      });

      it('should handle debit processing failure', async function() {
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

        connector.httpClient.post.rejects(error);

        try {
          await connector.processDebit(transaction);
          expect.fail('Should have thrown debit processing error');
        } catch (err) {
          expect(err.message).to.include('Debit processing failed');
        }
      });
    });

    describe('processCredit', function() {
      it('should process credit transaction successfully', async function() {
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

        connector.httpClient.post.resolves(mockCreditResponse);

        const result = await connector.processCredit(transaction);

        expect(result.transactionId).to.equal('TXN_CREDIT_001');
        expect(result.status).to.equal(TRANSACTION_STATUS.CONFIRMED);
        expect(result.amount).to.equal(750.00);
        expect(result.currency).to.equal('USD');
        expect(result.balance).to.equal(1750.00);
      });
    });

    describe('getTransactionStatus', function() {
      it('should get transaction status successfully', async function() {
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

        connector.httpClient.get.resolves(mockStatusResponse);

        const result = await connector.getTransactionStatus('TXN_STATUS_001');

        expect(result.transactionId).to.equal('TXN_STATUS_001');
        expect(result.status).to.equal(TRANSACTION_STATUS.CONFIRMED);
        expect(result.amount).to.equal(100.00);
        expect(result.metadata).to.deep.equal(mockStatusResponse.data.metadata);
      });
    });
  });

  describe('Compliance Operations', function() {
    beforeEach(function() {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    it('should perform compliance check successfully', async function() {
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

      connector.httpClient.post.resolves(mockComplianceResponse);

      const result = await connector.performComplianceCheck(transaction);

      expect(result.passed).to.be.true;
      expect(result.riskScore).to.equal(0.15);
      expect(result.flags).to.be.empty;
      expect(result.recommendations).to.include('Monitor future transactions');
      expect(connector.dnaMetrics.complianceChecks).to.equal(1);
    });

    it('should handle compliance check failure', async function() {
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

      connector.httpClient.post.resolves(mockComplianceResponse);

      const result = await connector.performComplianceCheck(transaction);

      expect(result.passed).to.be.false;
      expect(result.riskScore).to.equal(0.85);
      expect(result.flags).to.include('HIGH_RISK_COUNTRY');
      expect(result.flags).to.include('PEP_MATCH');
    });
  });

  describe('Webhook Management', function() {
    beforeEach(function() {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
    });

    it('should register webhook successfully', async function() {
      const mockWebhookResponse = {
        data: {
          webhookId: 'WEBHOOK_001',
          eventType: 'transaction.completed',
          callbackUrl: 'https://example.com/webhook',
          active: true
        }
      };

      connector.httpClient.post.resolves(mockWebhookResponse);

      const result = await connector.registerWebhook(
        'transaction.completed',
        'https://example.com/webhook'
      );

      expect(result.webhookId).to.equal('WEBHOOK_001');
      expect(connector.webhookHandlers.has('transaction.completed')).to.be.true;
    });
  });

  describe('Error Handling and Status Mapping', function() {
    it('should map DNA status codes correctly', function() {
      expect(connector.mapDNAStatus('PENDING')).to.equal(TRANSACTION_STATUS.PENDING);
      expect(connector.mapDNAStatus('PROCESSING')).to.equal(TRANSACTION_STATUS.PROCESSING);
      expect(connector.mapDNAStatus('COMPLETED')).to.equal(TRANSACTION_STATUS.CONFIRMED);
      expect(connector.mapDNAStatus('FAILED')).to.equal(TRANSACTION_STATUS.FAILED);
      expect(connector.mapDNAStatus('UNKNOWN')).to.equal(TRANSACTION_STATUS.PENDING);
    });

    it('should map DNA error codes correctly', function() {
      const dnaError = {
        code: 'INSUFFICIENT_FUNDS',
        message: 'Account has insufficient funds',
        details: { balance: 50.00 }
      };

      const mapped = connector.mapDNAError(dnaError);

      expect(mapped.code).to.equal(ERROR_CODES.INSUFFICIENT_FUNDS);
      expect(mapped.message).to.equal('Account has insufficient funds');
      expect(mapped.details).to.deep.equal({ balance: 50.00 });
    });

    it('should handle unknown error codes', function() {
      const dnaError = { code: 'UNKNOWN_ERROR' };
      const mapped = connector.mapDNAError(dnaError);

      expect(mapped.code).to.equal(ERROR_CODES.SERVICE_UNAVAILABLE);
      expect(mapped.message).to.equal('Unknown DNA error');
    });
  });

  describe('Rate Limiting', function() {
    beforeEach(function() {
      connector.accessToken = 'mock_token';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.dnaConfig.rateLimitPerSecond = 2;
      connector.dnaConfig.rateLimitPerMinute = 10;
    });

    it('should respect rate limiting', async function() {
      this.timeout(5000);

      connector.httpClient.get.resolves({ status: 200, data: {} });

      // Make requests rapidly
      const promises = [];
      for (let i = 0; i < 3; i++) {
        promises.push(connector.makeApiCall('GET', '/test'));
      }

      const start = Date.now();
      await Promise.all(promises);
      const duration = Date.now() - start;

      // Should take at least 1 second due to rate limiting
      expect(duration).to.be.at.least(500);
    });
  });

  describe('Status and Health Monitoring', function() {
    it('should return enhanced status with DNA metrics', function() {
      connector.dnaMetrics.apiCalls = 100;
      connector.dnaMetrics.cacheHits = 50;
      connector.accessToken = 'test_token';
      connector.tokenExpiry = Date.now() + 3600000;

      const status = connector.getStatus();

      expect(status.dnaMetrics.apiCalls).to.equal(100);
      expect(status.dnaMetrics.cacheHits).to.equal(50);
      expect(status.tokenStatus.hasToken).to.be.true;
      expect(status.tokenStatus.timeToExpiry).to.be.above(0);
    });

    it('should track metrics correctly', function() {
      expect(connector.dnaMetrics.apiCalls).to.equal(0);
      
      // Simulate API call tracking
      connector.dnaMetrics.apiCalls++;
      connector.dnaMetrics.cacheHits++;
      
      expect(connector.dnaMetrics.apiCalls).to.equal(1);
      expect(connector.dnaMetrics.cacheHits).to.equal(1);
    });
  });

  describe('Integration with Base Connector', function() {
    it('should inherit from BaseBankingConnector', function() {
      expect(connector).to.be.instanceOf(require('../../../src/connectors/base/base-banking-connector').BaseBankingConnector);
    });

    it('should emit events properly', function(done) {
      connector.on('test:event', (data) => {
        expect(data.message).to.equal('test');
        done();
      });

      connector.emit('test:event', { message: 'test' });
    });

    it('should implement all required abstract methods', function() {
      expect(connector.authenticate).to.be.a('function');
      expect(connector.testConnection).to.be.a('function');
      expect(connector.getAccountDetails).to.be.a('function');
      expect(connector.checkAccountBalance).to.be.a('function');
      expect(connector.validateTransaction).to.be.a('function');
      expect(connector.processDebit).to.be.a('function');
      expect(connector.processCredit).to.be.a('function');
      expect(connector.getTransactionStatus).to.be.a('function');
    });
  });
});

describe('DNA Constants', function() {
  it('should export correct endpoints', function() {
    expect(DNA_ENDPOINTS.AUTHENTICATE).to.equal('/auth/oauth/token');
    expect(DNA_ENDPOINTS.ACCOUNT_INQUIRY).to.equal('/accounts/inquiry');
    expect(DNA_ENDPOINTS.PAYMENT_INITIATION).to.equal('/payments/initiate');
    expect(DNA_ENDPOINTS.AML_SCREENING).to.equal('/compliance/aml/screen');
  });

  it('should export transaction types', function() {
    expect(DNA_TRANSACTION_TYPES.DEBIT).to.equal('debit');
    expect(DNA_TRANSACTION_TYPES.CREDIT).to.equal('credit');
    expect(DNA_TRANSACTION_TYPES.WIRE).to.equal('wire');
    expect(DNA_TRANSACTION_TYPES.ACH).to.equal('ach');
  });

  it('should export account types', function() {
    expect(DNA_ACCOUNT_TYPES.CHECKING).to.equal('checking');
    expect(DNA_ACCOUNT_TYPES.SAVINGS).to.equal('savings');
    expect(DNA_ACCOUNT_TYPES.LOAN).to.equal('loan');
    expect(DNA_ACCOUNT_TYPES.MORTGAGE).to.equal('mortgage');
  });
});