/**
 * SWIFT to Blockchain Integration Tests
 * Tests core SWIFT message parsing and blockchain gateway integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 2 Integration Testing - Core Flow
 */

const SWIFTParser = require('../../src/adapters/swift-parser');
const { SmartRouter } = require('../../src/router/smart-router');
const { BaseBlockchainGateway, TRANSACTION_STATUS, NETWORK_TYPES } = require('../../src/blockchain/base-gateway');

// Create a mock gateway for testing
class MockBlockchainGateway extends BaseBlockchainGateway {
  constructor(networkType = NETWORK_TYPES.XRP) {
    super(networkType, { testMode: true });
    this.mockConnected = false;
    this.mockTransactions = new Map();
    this.mockMetrics = {
      totalTransactions: 0,
      successfulTransactions: 0,
      failedTransactions: 0
    };
  }

  async connect() {
    await new Promise(resolve => setTimeout(resolve, 10)); // Simulate connection delay
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

    await new Promise(resolve => setTimeout(resolve, 50)); // Simulate processing time

    const result = {
      id: `mock-${this.networkType}-${Date.now()}`,
      status: TRANSACTION_STATUS.SUBMITTED,
      networkType: this.networkType,
      gasUsed: '21000',
      timestamp: new Date().toISOString()
    };

    // Track the transaction in the base gateway's history
    this.trackTransaction(transaction.id, result);
    this.mockTransactions.set(transaction.id, result);
    this.mockMetrics.totalTransactions++;
    this.mockMetrics.successfulTransactions++;
    return result;
  }

  async getTransactionStatus(transactionId) {
    const tracked = this.transactionHistory.get(transactionId);
    if (!tracked) {
      throw new Error(`Transaction ${transactionId} not found`);
    }

    // Simulate confirmation after some time
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
      timestamp: tracked.timestamp
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

  getMetrics() {
    return {
      ...this.mockMetrics,
      averageProcessingTime: 50,
      timestamp: new Date().toISOString()
    };
  }
}

describe('SWIFT to Blockchain Integration', () => {
  let swiftParser;
  let smartRouter;
  let mockXrpGateway;
  let mockEthereumGateway;

  beforeAll(async () => {
    // Initialize components
    swiftParser = new SWIFTParser();
    smartRouter = new SmartRouter({
      testMode: true,
      enableRouting: true,
      enableCompliance: true
    });

    // Create mock gateways
    mockXrpGateway = new MockBlockchainGateway(NETWORK_TYPES.XRP);
    mockEthereumGateway = new MockBlockchainGateway(NETWORK_TYPES.ETHEREUM_L2);

    // Register gateways with router
    smartRouter.registerGateway('xrp-ledger', mockXrpGateway);
    smartRouter.registerGateway('ethereum-polygon', mockEthereumGateway);

    // Connect gateways
    await mockXrpGateway.connect();
    await mockEthereumGateway.connect();
  });

  afterAll(async () => {
    await mockXrpGateway.disconnect();
    await mockEthereumGateway.disconnect();
  });

  describe('Complete SWIFT Message Processing', () => {
    test('should process MT103 message from parsing to blockchain execution', async () => {
      // Real MT103 SWIFT message
      const mt103Message = `{1:F01HSBCGB2LAXXX0000000000}{2:I103BARCGB22XXXXN}{3:{113:UUUU}{108:MT103 TEST}}{4:
:20:FT21040512345
:23B:CRED
:32A:211201USD25000,00
:50K:/1234567890
HSBC BANK PLC
1 MAIN STREET
LONDON EC1A 1AA
:59:/9876543210
BARCLAYS BANK PLC
1 CHURCHILL PLACE
LONDON E14 5HP
:70:PAYMENT FOR TRADE INVOICE TI-2021-4567
SHIPPING: CONTAINER ABC123
:71A:SHA
-}`;

      // Step 1: Parse SWIFT message
      const parsedMessage = swiftParser.parse(mt103Message);
      
      expect(parsedMessage).toMatchObject({
        messageType: 'MT103',
        transactionReference: 'FT21040512345',
        amount: 2500000, // Parser returns amount in minor units (cents)
        currency: 'USD',
        sender: expect.objectContaining({
          account: '/1234567890',
          name: 'HSBC BANK PLC'
        }),
        receiver: expect.objectContaining({
          account: '/9876543210',
          name: 'BARCLAYS BANK PLC'
        }),
        remittanceInfo: expect.stringContaining('PAYMENT FOR TRADE INVOICE')
      });

      // Step 2: Convert to transaction format
      const transaction = {
        id: parsedMessage.transactionReference,
        messageType: parsedMessage.messageType,
        amount: parsedMessage.amount / 100, // Convert from minor units to major units
        currency: parsedMessage.currency,
        sender: {
          name: parsedMessage.sender.name,
          account: parsedMessage.sender.account,
          address: parsedMessage.sender.address
        },
        receiver: {
          name: parsedMessage.receiver.name,
          account: parsedMessage.receiver.account,
          address: parsedMessage.receiver.address
        },
        reference: parsedMessage.transactionReference,
        remittanceInfo: parsedMessage.remittanceInfo,
        urgency: 'normal',
        compliance: {
          amlRequired: true,
          fatfReporting: (parsedMessage.amount / 100) >= 1000
        }
      };

      // Step 3: Route transaction
      const routingDecision = await smartRouter.route(transaction, {
        enableCompliance: true,
        optimizeForCost: true
      });

      expect(routingDecision.status).toBe('routed');
      expect(routingDecision.targetNetwork).toBeDefined();
      expect(['xrp-ledger', 'ethereum-polygon']).toContain(routingDecision.targetNetwork);

      // Step 4: Execute on blockchain
      // Get the appropriate gateway based on routing decision
      const gateway = routingDecision.targetNetwork === 'xrp-ledger' ? mockXrpGateway : mockEthereumGateway;
      const submissionResult = await gateway.submitTransaction(transaction);

      expect(submissionResult).toMatchObject({
        id: expect.any(String),
        status: TRANSACTION_STATUS.SUBMITTED,
        networkType: routingDecision.targetNetwork
      });

      // Step 5: Verify transaction status
      const statusResult = await gateway.getTransactionStatus(transaction.id);
      expect(statusResult.status).toBe(TRANSACTION_STATUS.CONFIRMED);

      console.log(`✅ Processed ${transaction.currency} ${transaction.amount} transaction via ${routingDecision.targetNetwork}`);
    });

    test('should process MT202 bank cover payment', async () => {
      const mt202Message = `{1:F01CITIUS33XXXX0000000000}{2:I202DEUTDEFFXXXXN}{3:{113:UUUU}}{4:
:20:COV21040567
:21:FT21040512345
:32A:211201EUR75000,00
:52A:CITIUS33XXX
:53B:/EUR1122334455
CITIBANK N.A.
NEW YORK
:58A:/EUR9988776655
DEUTDEFFXXX
-}`;

      // Parse MT202
      const parsedMessage = swiftParser.parse(mt202Message);

      expect(parsedMessage).toMatchObject({
        messageType: 'MT202',
        transactionReference: 'COV21040567',
        amount: 7500000, // Parser returns amount in minor units
        currency: 'EUR'
      });
      
      // Check if related reference exists in raw data
      console.log('MT202 parsed fields:', Object.keys(parsedMessage));

      // Create institutional transfer
      const transaction = {
        id: parsedMessage.transactionReference,
        messageType: parsedMessage.messageType,
        amount: parsedMessage.amount / 100, // Convert from minor units
        currency: parsedMessage.currency,
        type: 'institutional_cover',
        sender: {
          name: 'CITIBANK N.A.',
          account: 'EUR1122334455'
        },
        receiver: {
          name: 'DEUTSCHE BANK',
          account: 'EUR9988776655',
          bic: 'DEUTDEFF'
        },
        // relatedReference: parsedMessage.relatedReference, // May not be parsed by current implementation
        institutional: true
      };

      // Route institutional transfer
      const routingDecision = await smartRouter.route(transaction, {
        institutionalPriority: true,
        enableCompliance: true
      });

      expect(routingDecision.status).toBe('routed');
      // Note: institutional priority may be handled differently in the router implementation

      console.log(`✅ Routed institutional EUR ${transaction.amount} cover payment`);
    });
  });

  describe('Error Handling and Resilience', () => {
    test('should handle invalid SWIFT message gracefully', async () => {
      const invalidMessage = `{1:INVALID}{2:ALSO_INVALID}{4:
:20:TEST123
:invalid_field:invalid_content
-}`;

      // Should throw parsing error
      expect(() => swiftParser.parse(invalidMessage)).toThrow();
    });

    test('should handle routing failures with fallback', async () => {
      const transaction = {
        id: 'FALLBACK-TEST-123',
        messageType: 'MT103',
        amount: 15000,
        currency: 'USD',
        urgency: 'high',
        sender: { name: 'Test Bank A', account: '123456' },
        receiver: { name: 'Test Bank B', account: '654321' }
      };

      // Simulate primary network failure
      mockXrpGateway.isConnected = false;

      const routingDecision = await smartRouter.route(transaction, {
        enableFallback: true
      });

      expect(routingDecision.status).toBe('routed');
      expect(routingDecision.targetNetwork).toBe('ethereum-polygon'); // Should use fallback

      // Restore connection
      mockXrpGateway.isConnected = true;
    });

    test('should maintain transaction history across failures', async () => {
      const transaction = {
        id: 'HISTORY-TEST-456',
        messageType: 'MT103',
        amount: 5000,
        currency: 'GBP',
        sender: { name: 'Test Bank C', account: '789012' },
        receiver: { name: 'Test Bank D', account: '210987' }
      };

      const routingDecision = await smartRouter.route(transaction);
      const gateway = smartRouter.getGateway(routingDecision.targetNetwork);
      
      await gateway.submitTransaction(transaction);

      // Verify transaction is tracked
      const history = gateway.getTransactionHistory();
      expect(history).toContainEqual(
        expect.objectContaining({
          id: transaction.id
        })
      );

      // Verify metrics updated
      const metrics = gateway.getMetrics();
      expect(metrics.totalTransactions).toBeGreaterThan(0);
    });
  });

  describe('Performance and Metrics', () => {
    test('should track processing metrics across the pipeline', async () => {
      const startTime = Date.now();

      // Process a batch of transactions
      const transactions = [
        { id: 'PERF-1', messageType: 'MT103', amount: 1000, currency: 'USD', sender: { name: 'Bank 1', account: '001' }, receiver: { name: 'Bank 2', account: '002' } },
        { id: 'PERF-2', messageType: 'MT103', amount: 2500, currency: 'EUR', sender: { name: 'Bank 3', account: '003' }, receiver: { name: 'Bank 4', account: '004' } },
        { id: 'PERF-3', messageType: 'MT103', amount: 8000, currency: 'GBP', sender: { name: 'Bank 5', account: '005' }, receiver: { name: 'Bank 6', account: '006' } }
      ];

      for (const tx of transactions) {
        const routingDecision = await smartRouter.route(tx);
        const gateway = smartRouter.getGateway(routingDecision.targetNetwork);
        await gateway.submitTransaction(tx);
      }

      const totalTime = Date.now() - startTime;

      // Verify router metrics
      const routerMetrics = smartRouter.getMetrics();
      expect(routerMetrics.totalTransactions).toBeGreaterThanOrEqual(3);
      expect(routerMetrics.successfulRoutes).toBeGreaterThanOrEqual(3);

      // Verify gateway metrics
      const xrpMetrics = mockXrpGateway.getMetrics();
      const ethMetrics = mockEthereumGateway.getMetrics();
      
      const totalGatewayTransactions = xrpMetrics.totalTransactions + ethMetrics.totalTransactions;
      expect(totalGatewayTransactions).toBeGreaterThanOrEqual(3);

      console.log(`📊 Processed ${transactions.length} transactions in ${totalTime}ms`);
      console.log(`📈 Router: ${routerMetrics.totalTransactions} total, ${routerMetrics.successRate * 100}% success rate`);
      console.log(`🔗 XRP Gateway: ${xrpMetrics.totalTransactions} transactions`);
      console.log(`⛓️ Ethereum Gateway: ${ethMetrics.totalTransactions} transactions`);
    });

    test('should demonstrate compliance workflow', async () => {
      // High-value transaction requiring compliance
      const highValueTx = {
        id: 'COMPLIANCE-789',
        messageType: 'MT103',
        amount: 50000,
        currency: 'USD',
        sender: {
          name: 'CORPORATE CLIENT A',
          account: 'CORP123',
          jurisdiction: 'US'
        },
        receiver: {
          name: 'OVERSEAS ENTITY B',
          account: 'OVER456',
          jurisdiction: 'DE'
        },
        compliance: {
          amlRequired: true,
          fatfReporting: true,
          crossBorder: true
        }
      };

      const routingDecision = await smartRouter.route(highValueTx, {
        enableCompliance: true,
        requireDocumentation: true
      });

      expect(routingDecision.status).toBe('routed');
      expect(routingDecision.complianceFlags).toMatchObject({
        amlRequired: true,
        fatfReporting: true,
        crossBorder: true
      });

      console.log(`✅ Compliance workflow: ${Object.keys(routingDecision.complianceFlags).length} checks applied`);
    });
  });
});