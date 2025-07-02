/**
 * TCS BaNCS to Blockchain Integration Tests
 * Complete end-to-end flow: SWIFT → BaNCS Validation → Smart Router → Blockchain
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 2 Integration Testing - TCS BaNCS Flow
 */

const SWIFTParser = require('../../src/adapters/swift-parser');
const { SmartRouter } = require('../../src/router/smart-router');
const { TCSBaNCSIntegrationService } = require('../../src/connectors/tcs-bancs/bancs-integration-service');
const { BaseBlockchainGateway, TRANSACTION_STATUS, NETWORK_TYPES } = require('../../src/blockchain/base-gateway');

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

// Mock TCS BaNCS Connector
jest.mock('../../src/connectors/tcs-bancs/bancs-connector', () => {
  const EventEmitter = require('events');
  
  class MockTCSBaNCSConnector extends EventEmitter {
    constructor(config) {
      super();
      this.config = config;
      this.bankCode = config.bankCode || 'TCS_BANCS';
      this.isConnected = false;
      this.metrics = {
        totalRequests: 0,
        successfulRequests: 0,
        failedRequests: 0,
        averageResponseTime: 0
      };
      
      // Mock methods with default implementations
      this.initialize = jest.fn().mockResolvedValue();
      this.testConnection = jest.fn().mockResolvedValue(true);
      this.getAccountDetails = jest.fn().mockResolvedValue({
        accountNumber: 'TEST123',
        accountStatus: 'ACTIVE',
        currency: 'USD'
      });
      this.checkAccountBalance = jest.fn().mockResolvedValue({
        availableBalance: 100000,
        accountStatus: 'ACTIVE'
      });
      this.performComplianceCheck = jest.fn().mockResolvedValue({
        passed: true,
        riskScore: 25,
        reason: 'Low risk transaction',
        requiresManualReview: false,
        aml: { passed: true, required: true },
        sanctions: { passed: true, matches: [] }
      });
      this.validateTransaction = jest.fn().mockResolvedValue({ isValid: true });
      this.getMetrics = jest.fn().mockReturnValue(this.metrics);
      this.cleanup = jest.fn().mockResolvedValue();
    }
  }
  
  return { TCSBaNCSConnector: MockTCSBaNCSConnector };
});

// Mock axios for BaNCS connector
jest.mock('axios', () => ({
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
}));

// Create a mock blockchain gateway for testing
class MockBaNCSBlockchainGateway extends BaseBlockchainGateway {
  constructor(networkType = NETWORK_TYPES.XRP) {
    super(networkType, { testMode: true });
    this.mockConnected = false;
    this.mockTransactions = new Map();
  }

  async connect() {
    await new Promise(resolve => setTimeout(resolve, 10));
    this.mockConnected = true;
    this.isConnected = true;
    return true;
  }

  async disconnect() {
    this.mockConnected = false;
    this.isConnected = false;
    return true;
  }

  async submitTransaction(transaction) {
    if (!this.mockConnected) {
      throw new Error('Not connected to network');
    }

    await new Promise(resolve => setTimeout(resolve, 50));

    const result = {
      id: `bancs-${this.networkType}-${Date.now()}`,
      status: TRANSACTION_STATUS.SUBMITTED,
      networkType: this.networkType,
      gasUsed: '21000',
      timestamp: new Date().toISOString(),
      bancsValidated: true, // Flag indicating BaNCS validation
      preprocessingId: transaction.preprocessingId
    };

    this.trackTransaction(transaction.id, result);
    this.mockTransactions.set(transaction.id, result);
    return result;
  }

  async getTransactionStatus(transactionId) {
    const tracked = this.transactionHistory.get(transactionId);
    if (!tracked) {
      throw new Error(`Transaction ${transactionId} not found`);
    }

    const mockTx = this.mockTransactions.get(transactionId);
    if (mockTx) {
      mockTx.status = TRANSACTION_STATUS.CONFIRMED;
      mockTx.blockNumber = 1000000 + Math.floor(Math.random() * 1000);
    }

    return {
      transactionId,
      networkTransactionId: tracked.networkTransactionId,
      status: TRANSACTION_STATUS.CONFIRMED,
      blockNumber: mockTx?.blockNumber,
      timestamp: tracked.timestamp,
      bancsValidated: true
    };
  }

  async getNetworkHealth() {
    return {
      networkType: this.networkType,
      isHealthy: true,
      latency: 50 + Math.random() * 100,
      blockNumber: 1000000,
      timestamp: new Date().toISOString()
    };
  }
}

describe('BaNCS to Blockchain Integration', () => {
  let swiftParser;
  let smartRouter;
  let bancsIntegrationService;
  let mockXrpGateway;
  let mockCordaGateway;

  beforeAll(async () => {
    // Initialize components
    swiftParser = new SWIFTParser();
    smartRouter = new SmartRouter({
      testMode: true,
      enableRouting: true,
      enableCompliance: true
    });

    bancsIntegrationService = new TCSBaNCSIntegrationService({
      testMode: true,
      bankCode: 'TESTBANK',
      branchCode: 'BRANCH01',
      enablePreprocessing: true,
      enableRealTimeValidation: true
    });

    // Create mock gateways
    mockXrpGateway = new MockBaNCSBlockchainGateway(NETWORK_TYPES.XRP);
    mockCordaGateway = new MockBaNCSBlockchainGateway(NETWORK_TYPES.CORDA);

    // Register gateways with router
    smartRouter.registerGateway('xrp-ledger', mockXrpGateway);
    smartRouter.registerGateway('r3-corda', mockCordaGateway);

    // Connect gateways
    await mockXrpGateway.connect();
    await mockCordaGateway.connect();
  });

  afterAll(async () => {
    await mockXrpGateway.disconnect();
    await mockCordaGateway.disconnect();
    await bancsIntegrationService.cleanup();
  });

  describe('Complete BaNCS Integration Flow', () => {
    test('should process high-value MT103 with BaNCS validation and Corda routing', async () => {
      // Real MT103 SWIFT message for high-value transaction
      const mt103Message = `{1:F01HSBCGB2LAXXX0000000000}{2:I103CORDGB21XXXXN}{3:{113:UUUU}{108:MT103 HIGH VALUE}}{4:
:20:HV21040567890
:23B:CRED
:32A:211201USD250000,00
:50K:/1234567890
HSBC PRIVATE BANK
1 MAIN STREET
LONDON EC1A 1AA
:59:/9876543210
CORDA ENTERPRISE CLIENT
1 FINTECH SQUARE
LONDON E14 5HP
:70:HIGH VALUE TRANSACTION FOR TRADE FINANCE
REGULATORY COMPLIANCE REQUIRED
:71A:SHA
-}`;

      // Step 1: Parse SWIFT message
      const parsedMessage = swiftParser.parse(mt103Message);
      
      expect(parsedMessage).toMatchObject({
        messageType: 'MT103',
        transactionReference: 'HV21040567890',
        amount: 25000000, // Parser returns amount in minor units (cents)
        currency: 'USD'
      });

      // Step 2: Create transaction for BaNCS preprocessing
      const transaction = {
        id: parsedMessage.transactionReference,
        messageType: parsedMessage.messageType,
        amount: parsedMessage.amount / 100, // Convert to major units
        currency: parsedMessage.currency,
        sender: {
          name: parsedMessage.sender.name,
          account: parsedMessage.sender.account
        },
        receiver: {
          name: parsedMessage.receiver.name,
          account: parsedMessage.receiver.account
        },
        reference: parsedMessage.transactionReference,
        remittanceInfo: parsedMessage.remittanceInfo
      };

      // Mock BaNCS responses for successful validation
      const mockConnector = bancsIntegrationService.bancsConnector;
      
      // Mock account verification
      mockConnector.getAccountDetails = jest.fn()
        .mockResolvedValueOnce({
          accountNumber: '1234567890',
          accountStatus: 'ACTIVE',
          currency: 'USD',
          customer: {
            id: 'CUST001',
            name: 'HSBC PRIVATE BANK',
            riskRating: 'LOW'
          }
        })
        .mockResolvedValueOnce({
          accountNumber: '9876543210',
          accountStatus: 'ACTIVE',
          currency: 'USD'
        });

      // Mock balance check
      mockConnector.checkAccountBalance = jest.fn().mockResolvedValue({
        availableBalance: 500000, // Sufficient funds
        accountStatus: 'ACTIVE'
      });

      // Mock compliance check for high-value transaction
      mockConnector.performComplianceCheck = jest.fn().mockResolvedValue({
        passed: true,
        riskScore: 45, // Medium risk for high value
        reason: 'High value transaction - additional monitoring required',
        requiresManualReview: false,
        aml: { passed: true, required: true },
        fatf: { required: true, passed: true },
        sanctions: { passed: true }
      });

      // Step 3: BaNCS Preprocessing
      const preprocessingResult = await bancsIntegrationService.preprocessTransaction(transaction);
      
      expect(preprocessingResult.status).toBe('validated');
      expect(preprocessingResult.stages.validation.passed).toBe(true);
      expect(preprocessingResult.stages.accountVerification.passed).toBe(true);
      expect(preprocessingResult.stages.compliance.passed).toBe(true);
      expect(preprocessingResult.stages.routingPrep.success).toBe(true);

      // Verify BaNCS-specific enhancements
      const enhancedTransaction = preprocessingResult.stages.routingPrep.routingData.enhancedTransaction;
      expect(enhancedTransaction.preprocessingId).toBeDefined();
      expect(enhancedTransaction.bankingContext.senderBank).toBe('TESTBANK');
      expect(enhancedTransaction.bankingContext.accountVerified).toBe(true);
      expect(enhancedTransaction.bankingContext.complianceCleared).toBe(true);

      // Step 4: Smart Routing with BaNCS context
      const routingContext = {
        bancsPreprocessed: true,
        preprocessingResult: preprocessingResult,
        institutionalGrade: true,
        complianceCleared: true
      };

      const routingDecision = await smartRouter.route(enhancedTransaction, routingContext);

      expect(routingDecision.status).toBe('routed');
      expect(routingDecision.targetNetwork).toBe('r3-corda'); // High-value should route to Corda

      // Step 5: Execute on Corda with BaNCS validation
      enhancedTransaction.preprocessingId = preprocessingResult.processingId;
      const submissionResult = await mockCordaGateway.submitTransaction(enhancedTransaction);

      expect(submissionResult).toMatchObject({
        status: TRANSACTION_STATUS.SUBMITTED,
        networkType: 'r3-corda',
        bancsValidated: true,
        preprocessingId: preprocessingResult.processingId
      });

      // Step 6: Verify transaction status and BaNCS integration
      const statusResult = await mockCordaGateway.getTransactionStatus(transaction.id);
      expect(statusResult.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(statusResult.bancsValidated).toBe(true);

      console.log(`✅ High-value ${transaction.currency} ${transaction.amount} processed:`);
      console.log(`   📋 BaNCS Preprocessing: ${preprocessingResult.processingTime}ms`);
      console.log(`   🔗 Blockchain Network: ${routingDecision.targetNetwork}`);
      console.log(`   ✅ Compliance Score: ${preprocessingResult.stages.compliance.riskScore}/100`);
      console.log(`   💰 Available Balance: $${preprocessingResult.stages.accountVerification.verifications.senderBalance.availableBalance}`);
    });

    test('should handle small-value transaction with cost optimization', async () => {
      const smallValueMT103 = `{1:F01NWBKGB22XXXX0000000000}{2:I103TESTGB21XXXXN}{3:{113:UUUU}}{4:
:20:SV21040567891
:23B:CRED
:32A:211201USD2500,00
:50K:/1111111111
DIGITAL WALLET USER
MOBILE APP PAYMENT
:59:/2222222222
E-COMMERCE MERCHANT
ONLINE STORE
:70:PAYMENT FOR ORDER ORD-2023-12345
DIGITAL GOODS PURCHASE
:71A:SHA
-}`;

      const parsedMessage = swiftParser.parse(smallValueMT103);
      const transaction = {
        id: parsedMessage.transactionReference,
        messageType: parsedMessage.messageType,
        amount: parsedMessage.amount / 100,
        currency: parsedMessage.currency,
        sender: {
          name: parsedMessage.sender.name,
          account: parsedMessage.sender.account
        },
        receiver: {
          name: parsedMessage.receiver.name,
          account: parsedMessage.receiver.account
        }
      };

      // Mock BaNCS responses for small transaction
      const mockConnector = bancsIntegrationService.bancsConnector;
      
      mockConnector.getAccountDetails = jest.fn().mockResolvedValue({
        accountNumber: '1111111111',
        accountStatus: 'ACTIVE',
        currency: 'USD'
      });

      mockConnector.checkAccountBalance = jest.fn().mockResolvedValue({
        availableBalance: 10000,
        accountStatus: 'ACTIVE'
      });

      mockConnector.performComplianceCheck = jest.fn().mockResolvedValue({
        passed: true,
        riskScore: 15, // Low risk
        reason: 'Low value transaction - standard processing',
        requiresManualReview: false
      });

      // Preprocess through BaNCS
      const preprocessingResult = await bancsIntegrationService.preprocessTransaction(transaction);
      expect(preprocessingResult.status).toBe('validated');

      // Route with cost optimization context
      const enhancedTransaction = preprocessingResult.stages.routingPrep.routingData.enhancedTransaction;
      const routingDecision = await smartRouter.route(enhancedTransaction, {
        optimizeForCost: true,
        bancsPreprocessed: true
      });

      expect(routingDecision.status).toBe('routed');
      // Small amounts should prefer cost-effective networks
      expect(['ethereum-polygon', 'xrp-ledger']).toContain(routingDecision.targetNetwork);

      console.log(`✅ Small-value ${transaction.currency} ${transaction.amount} optimized for cost`);
      console.log(`   🎯 Target Network: ${routingDecision.targetNetwork}`);
      console.log(`   📊 Risk Score: ${preprocessingResult.stages.compliance.riskScore}/100`);
    });

    test('should handle compliance failure with manual review', async () => {
      const suspiciousMT103 = `{1:F01RISKGB22XXXX0000000000}{2:I103COMPGB21XXXXN}{3:{113:UUUU}}{4:
:20:RF21040567892
:23B:CRED
:32A:211201USD75000,00
:50K:/9999999999
HIGH RISK ENTITY
SANCTIONS LIST ADDRESS
:59:/8888888888
SUSPICIOUS COUNTERPARTY
OFFSHORE JURISDICTION
:70:LARGE CASH TRANSACTION
POSSIBLE MONEY LAUNDERING
:71A:SHA
-}`;

      const parsedMessage = swiftParser.parse(suspiciousMT103);
      const transaction = {
        id: parsedMessage.transactionReference,
        messageType: parsedMessage.messageType,
        amount: parsedMessage.amount / 100,
        currency: parsedMessage.currency,
        sender: {
          name: parsedMessage.sender.name,
          account: parsedMessage.sender.account
        },
        receiver: {
          name: parsedMessage.receiver.name,
          account: parsedMessage.receiver.account
        }
      };

      // Create a fresh integration service instance to avoid mock conflicts
      const testIntegrationService = new TCSBaNCSIntegrationService({
        testMode: true,
        bankCode: 'TESTBANK',
        branchCode: 'BRANCH01'
      });

      // Mock the connector methods directly
      testIntegrationService.bancsConnector.getAccountDetails = jest.fn().mockResolvedValue({
        accountNumber: '9999999999',
        accountStatus: 'ACTIVE',
        currency: 'USD'
      });

      testIntegrationService.bancsConnector.checkAccountBalance = jest.fn().mockResolvedValue({
        availableBalance: 100000,
        accountStatus: 'ACTIVE'
      });

      testIntegrationService.bancsConnector.performComplianceCheck = jest.fn().mockResolvedValue({
        passed: false,
        riskScore: 95, // Very high risk
        reason: 'High risk transaction - sanctions match detected',
        requiresManualReview: true,
        aml: { passed: false, required: true },
        sanctions: { passed: false, matches: ['ENTITY_123'] }
      });

      // Preprocess through BaNCS
      const preprocessingResult = await testIntegrationService.preprocessTransaction(transaction);
      
      expect(preprocessingResult.status).toBe('compliance_failed');
      expect(preprocessingResult.stages.compliance.passed).toBe(false);
      expect(preprocessingResult.stages.compliance.requiresManualReview).toBe(true);
      expect(preprocessingResult.rejectionReason).toContain('sanctions match detected');

      console.log(`🛑 Compliance failure detected:`);
      console.log(`   ⚠️  Risk Score: ${preprocessingResult.stages.compliance.riskScore}/100`);
      console.log(`   📋 Manual Review Required: ${preprocessingResult.stages.compliance.requiresManualReview}`);
      console.log(`   🚫 Rejection Reason: ${preprocessingResult.rejectionReason}`);
    });
  });

  describe('BaNCS Integration Performance', () => {
    test('should demonstrate end-to-end processing metrics', async () => {
      const startTime = Date.now();

      // Process multiple transactions to test performance
      const transactions = [
        { id: 'PERF-1', amount: 5000, currency: 'USD', type: 'retail' },
        { id: 'PERF-2', amount: 75000, currency: 'EUR', type: 'corporate' },
        { id: 'PERF-3', amount: 150000, currency: 'GBP', type: 'institutional' }
      ];

      const results = [];

      for (const tx of transactions) {
        // Mock BaNCS responses
        const mockConnector = bancsIntegrationService.bancsConnector;
        
        mockConnector.getAccountDetails = jest.fn().mockResolvedValue({
          accountStatus: 'ACTIVE',
          currency: tx.currency
        });

        mockConnector.checkAccountBalance = jest.fn().mockResolvedValue({
          availableBalance: tx.amount * 2,
          accountStatus: 'ACTIVE'
        });

        mockConnector.performComplianceCheck = jest.fn().mockResolvedValue({
          passed: true,
          riskScore: 25 + (tx.amount / 10000), // Risk increases with amount
          requiresManualReview: false
        });

        // Process through BaNCS
        const preprocessingResult = await bancsIntegrationService.preprocessTransaction(tx);
        
        // Route to blockchain
        const enhancedTx = preprocessingResult.stages.routingPrep.routingData.enhancedTransaction;
        const routingDecision = await smartRouter.route(enhancedTx);
        
        results.push({
          transaction: tx,
          preprocessing: preprocessingResult,
          routing: routingDecision,
          processingTime: preprocessingResult.processingTime
        });
      }

      const totalTime = Date.now() - startTime;

      // Verify all transactions processed successfully
      expect(results.length).toBe(3);
      results.forEach(result => {
        expect(result.preprocessing.status).toBe('validated');
        expect(result.routing.status).toBe('routed');
        expect(result.processingTime).toBeGreaterThan(0);
      });

      // Get service metrics
      const serviceMetrics = bancsIntegrationService.getMetrics();
      expect(serviceMetrics.processing.totalProcessed).toBeGreaterThanOrEqual(3);

      // Get router metrics
      const routerMetrics = smartRouter.getRoutingStats();
      expect(routerMetrics.totalTransactions).toBeGreaterThanOrEqual(3);

      console.log(`📊 BaNCS Integration Performance:`);
      console.log(`   🚀 Total Processing Time: ${totalTime}ms`);
      console.log(`   📈 Average BaNCS Preprocessing: ${results.reduce((sum, r) => sum + r.processingTime, 0) / results.length}ms`);
      console.log(`   🎯 BaNCS Success Rate: ${serviceMetrics.processing.successfulValidations / serviceMetrics.processing.totalProcessed * 100}%`);
      console.log(`   🔀 Router Success Rate: ${routerMetrics.successRate * 100}%`);
      console.log(`   💰 Transaction Volume: $${results.reduce((sum, r) => sum + r.transaction.amount, 0)}`);
    });

    test('should demonstrate network preference algorithms', async () => {
      const bancsService = new TCSBaNCSIntegrationService({ bankCode: 'TESTBANK' });

      // Test different transaction profiles
      const testCases = [
        {
          name: 'High-value private banking',
          transaction: { amount: 250000, receiver: { bic: null } }, // Internal
          expectedNetworks: ['r3-corda']
        },
        {
          name: 'Cross-border remittance',
          transaction: { amount: 5000, receiver: { bic: 'DEUTDEFF' } }, // External
          expectedNetworks: ['xrp-ledger']
        },
        {
          name: 'Micro-payment',
          transaction: { amount: 100, receiver: { bic: null } },
          expectedNetworks: ['ethereum-polygon']
        },
        {
          name: 'Trade finance',
          transaction: { amount: 500000, receiver: { bic: 'HSBCGB2L' } },
          expectedNetworks: ['r3-corda']
        }
      ];

      const preferences = testCases.map(testCase => {
        const networks = bancsService.getPreferredNetworks(testCase.transaction, {});
        return {
          name: testCase.name,
          networks,
          matches: testCase.expectedNetworks.some(expected => networks.includes(expected))
        };
      });

      // Verify routing preferences work correctly
      preferences.forEach(pref => {
        expect(pref.matches).toBe(true);
      });

      console.log(`🎯 Network Preference Algorithms:`);
      preferences.forEach(pref => {
        console.log(`   ${pref.name}: ${pref.networks.join(', ')}`);
      });
    });
  });

  describe('Error Recovery and Resilience', () => {
    test('should handle BaNCS service failures gracefully', async () => {
      const transaction = {
        id: 'RESILIENCE-001',
        messageType: 'MT103',
        amount: 10000,
        currency: 'USD',
        sender: { account: '1234567890' },
        receiver: { account: '0987654321' }
      };

      // Create a fresh integration service instance to avoid mock conflicts
      const testIntegrationService = new TCSBaNCSIntegrationService({
        testMode: true,
        bankCode: 'TESTBANK',
        branchCode: 'BRANCH01'
      });

      // Mock BaNCS service failure at the getAccountDetails level
      testIntegrationService.bancsConnector.getAccountDetails = jest.fn().mockRejectedValue(new Error('BaNCS service unavailable'));

      const preprocessingResult = await testIntegrationService.preprocessTransaction(transaction);

      expect(preprocessingResult.status).toBe('error');
      expect(preprocessingResult.error).toContain('BaNCS service unavailable');

      // Verify metrics tracked the failure
      const metrics = testIntegrationService.getMetrics();
      expect(metrics.processing.failedValidations).toBeGreaterThan(0);

      console.log(`🔄 BaNCS Resilience Test:`);
      console.log(`   ❌ Service Failure Handled Gracefully`);
      console.log(`   📊 Failure Rate: ${metrics.processing.failedValidations / metrics.processing.totalProcessed * 100}%`);
    });
  });
});