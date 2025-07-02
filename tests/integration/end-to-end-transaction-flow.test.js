/**
 * End-to-End Transaction Flow Integration Tests
 * Tests complete transaction processing from SWIFT message to blockchain execution
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Sprint 2 Integration Testing
 */

const { SwiftParser } = require('../../src/adapters/swift-parser');
const { SmartRouter } = require('../../src/router/smart-router');
const { XRPGateway } = require('../../src/blockchain/xrp-gateway');
const { EthereumL2Gateway } = require('../../src/blockchain/ethereum-l2-gateway');
const { TRANSACTION_STATUS } = require('../../src/blockchain/base-gateway');

// Mock blockchain SDKs for integration testing
jest.mock('xrpl', () => ({
  Client: jest.fn(),
  Wallet: {
    fromMnemonic: jest.fn()
  },
  dropsToXrp: jest.fn(drops => (parseInt(drops) / 1000000).toString()),
  xrpToDrops: jest.fn(xrp => (parseFloat(xrp) * 1000000).toString()),
  isValidAddress: jest.fn(() => true)
}));

jest.mock('ethers', () => ({
  JsonRpcProvider: jest.fn(),
  Wallet: jest.fn(),
  Contract: jest.fn(),
  isAddress: jest.fn(() => true),
  parseUnits: jest.fn((value, decimals) => BigInt(value) * BigInt(10 ** decimals)),
  formatUnits: jest.fn((value, decimals) => {
    const num = typeof value === 'bigint' ? Number(value) : Number(value);
    if (decimals === 'gwei') decimals = 9;
    return (num / Math.pow(10, decimals)).toString();
  })
}));

describe('End-to-End Transaction Flow Integration', () => {
  let swiftParser;
  let smartRouter;
  let xrpGateway;
  let ethereumGateway;

  beforeAll(async () => {
    // Initialize all components
    swiftParser = new SwiftParser();
    smartRouter = new SmartRouter({
      testMode: true,
      enableRouting: true
    });
    
    xrpGateway = new XRPGateway({
      testMode: true,
      serverUrl: 'wss://s.altnet.rippletest.net:51233',
      walletSeed: 'test-seed-1234567890'
    });
    
    ethereumGateway = new EthereumL2Gateway({
      testMode: true,
      rpcUrl: 'https://rpc-mumbai.maticvigil.com',
      privateKey: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef'
    });

    // Setup router with gateways
    smartRouter.registerGateway('xrp-ledger', xrpGateway);
    smartRouter.registerGateway('ethereum-polygon', ethereumGateway);
  });

  afterAll(async () => {
    // Cleanup
    await smartRouter.cleanup?.();
    await xrpGateway.cleanup?.();
    await ethereumGateway.cleanup?.();
  });

  describe('Complete Transaction Processing Flow', () => {
    test('should process MT103 cross-border payment end-to-end', async () => {
      // 1. SWIFT Message Input
      const mt103Message = `{1:F01HSBCGB2LAXXX0000000000}{2:I103BARCGB22XXXXN}{3:{113:UUUU}{108:MT103 TEST}}{4:
:20:FT21123456789
:23B:CRED
:32A:211201USD50000,00
:50K:/123456789
HSBC BANK PLC
LONDON
:59:/987654321
BARCLAYS BANK PLC
LONDON
:70:PAYMENT FOR INVOICE 12345
:71A:SHA
-}`;

      // 2. Parse SWIFT Message
      const parsedMessage = swiftParser.parseMessage(mt103Message);
      
      expect(parsedMessage.messageType).toBe('MT103');
      expect(parsedMessage.amount).toBe(50000);
      expect(parsedMessage.currency).toBe('USD');
      expect(parsedMessage.transactionReference).toBe('FT21123456789');

      // 3. Convert to standardized transaction format
      const transaction = {
        id: parsedMessage.transactionReference,
        amount: parsedMessage.amount,
        currency: parsedMessage.currency,
        sender: {
          name: 'HSBC BANK PLC',
          account: '123456789',
          bic: 'HSBCGB2L'
        },
        receiver: {
          name: 'BARCLAYS BANK PLC',
          account: '987654321',
          bic: 'BARCGB22'
        },
        reference: parsedMessage.transactionReference,
        urgency: 'high',
        compliance: {
          amlRequired: true,
          fatfReporting: true
        }
      };

      // 4. Route Transaction
      const routingDecision = await smartRouter.route(transaction, {
        prioritizeSpeed: true,
        enableCompliance: true
      });

      expect(routingDecision.success).toBe(true);
      expect(routingDecision.targetNetwork).toBeDefined();
      expect(['xrp-ledger', 'ethereum-polygon']).toContain(routingDecision.targetNetwork);
      
      // 5. Verify routing decision reasoning
      expect(routingDecision.factors).toMatchObject({
        amount: 50000,
        currency: 'USD',
        urgency: 'high',
        complianceRequired: true
      });

      // 6. Execute on chosen blockchain
      const gateway = smartRouter.getGateway(routingDecision.targetNetwork);
      expect(gateway).toBeDefined();

      // Mock successful blockchain execution
      const mockResult = {
        id: 'blockchain-tx-hash-123',
        status: TRANSACTION_STATUS.SUBMITTED,
        networkType: routingDecision.targetNetwork,
        gasUsed: '21000',
        timestamp: new Date().toISOString()
      };

      // Mock the gateway methods
      jest.spyOn(gateway, 'connect').mockResolvedValue(true);
      jest.spyOn(gateway, 'submitTransaction').mockResolvedValue(mockResult);
      jest.spyOn(gateway, 'getTransactionStatus').mockResolvedValue({
        ...mockResult,
        status: TRANSACTION_STATUS.CONFIRMED
      });

      // Connect and submit
      await gateway.connect();
      const submissionResult = await gateway.submitTransaction(transaction);
      
      expect(submissionResult.id).toBe('blockchain-tx-hash-123');
      expect(submissionResult.status).toBe(TRANSACTION_STATUS.SUBMITTED);

      // 7. Verify transaction status
      const finalStatus = await gateway.getTransactionStatus(transaction.id);
      expect(finalStatus.status).toBe(TRANSACTION_STATUS.CONFIRMED);

      // 8. Verify compliance tracking
      expect(smartRouter.getMetrics().totalTransactions).toBe(1);
      expect(smartRouter.getMetrics().successfulRoutes).toBe(1);
    }, 30000);

    test('should handle multi-network routing for high-value trade finance', async () => {
      // High-value trade finance transaction requiring Corda privacy
      const tradeFinanceTransaction = {
        id: 'LC-2021-456789',
        amount: 1000000, // $1M
        currency: 'USD',
        type: 'trade_finance',
        sender: {
          name: 'IMPORT CORP',
          account: 'TRADE123',
          bic: 'CITIUS33'
        },
        receiver: {
          name: 'EXPORT LTD',
          account: 'EXP456',
          bic: 'DEUTDEFF'
        },
        tradeDetails: {
          letterOfCredit: 'LC123456',
          shipmentDetails: 'Electronics shipment',
          documentRequired: true
        },
        compliance: {
          amlRequired: true,
          fatfReporting: true,
          tradeCompliance: true
        }
      };

      // Route high-value trade finance
      const routingDecision = await smartRouter.route(tradeFinanceTransaction, {
        prioritizePrivacy: true,
        requireDocumentation: true,
        enableCompliance: true
      });

      expect(routingDecision.success).toBe(true);
      
      // High-value trade finance should prefer privacy-focused networks
      expect(routingDecision.reasoning).toContain('high-value');
      expect(routingDecision.reasoning).toContain('privacy');
      
      // Verify compliance flags
      expect(routingDecision.complianceFlags).toMatchObject({
        amlRequired: true,
        fatfReporting: true,
        tradeCompliance: true
      });
    });

    test('should handle low-value tokenized deposits efficiently', async () => {
      // Small tokenized deposit for DeFi yield farming
      const tokenizedDeposit = {
        id: 'DEP-2021-789',
        amount: 1000, // $1K
        currency: 'USDC',
        type: 'tokenized_deposit',
        sender: {
          name: 'RETAIL CUSTOMER',
          account: 'RET789',
          ethereumAddress: '0x1234567890123456789012345678901234567890'
        },
        receiver: {
          name: 'YIELD PROTOCOL',
          account: 'YIELD123',
          ethereumAddress: '0x0987654321098765432109876543210987654321'
        },
        defiOptions: {
          enableYieldFarming: true,
          targetApy: 8.5,
          riskTolerance: 'medium'
        }
      };

      // Route tokenized deposit
      const routingDecision = await smartRouter.route(tokenizedDeposit, {
        optimizeForCost: true,
        enableDeFi: true
      });

      expect(routingDecision.success).toBe(true);
      
      // Small amounts should prefer cost-efficient networks
      expect(routingDecision.reasoning).toContain('cost-efficient');
      expect(routingDecision.targetNetwork).toBe('ethereum-polygon');
      
      // Verify DeFi optimization
      expect(routingDecision.optimization).toMatchObject({
        costEfficient: true,
        defiEnabled: true
      });
    });

    test('should handle routing failures gracefully', async () => {
      // Invalid transaction that should fail routing
      const invalidTransaction = {
        id: 'INVALID-123',
        amount: -1000, // Invalid negative amount
        currency: 'INVALID',
        sender: null,
        receiver: null
      };

      // Should handle validation errors
      await expect(smartRouter.route(invalidTransaction)).rejects.toThrow();
      
      // Router should maintain metrics even on failures
      const metrics = smartRouter.getMetrics();
      expect(metrics.totalRoutingAttempts).toBeGreaterThan(0);
    });

    test('should support MT202 bank-to-bank transfers', async () => {
      // MT202 bank-to-bank cover payment
      const mt202Message = `{1:F01HSBCGB2LAXXX0000000000}{2:I202BARCGB22XXXXN}{3:{113:UUUU}}{4:
:20:COV21123456
:21:FT21123456789
:32A:211201USD50000,00
:53B:/USD123456789
CORRESPONDENT BANK
:58A:/USD987654321
BARCGB22XXX
-}`;

      // Parse MT202
      const parsedMessage = swiftParser.parseMessage(mt202Message);
      
      expect(parsedMessage.messageType).toBe('MT202');
      expect(parsedMessage.amount).toBe(50000);
      expect(parsedMessage.currency).toBe('USD');

      // MT202 should be routed as institutional transfer
      const transaction = {
        id: parsedMessage.transactionReference,
        amount: parsedMessage.amount,
        currency: parsedMessage.currency,
        type: 'institutional_transfer',
        sender: {
          name: 'CORRESPONDENT BANK',
          account: 'USD123456789'
        },
        receiver: {
          name: 'BARCLAYS',
          account: 'USD987654321',
          bic: 'BARCGB22'
        },
        institutional: true
      };

      const routingDecision = await smartRouter.route(transaction, {
        institutionalPriority: true
      });

      expect(routingDecision.success).toBe(true);
      expect(routingDecision.priority).toBe('institutional');
    });
  });

  describe('Multi-Gateway Coordination', () => {
    test('should coordinate multiple gateways for complex transactions', async () => {
      // Transaction requiring both XRP for speed and Ethereum for tokenization
      const complexTransaction = {
        id: 'COMPLEX-789',
        amount: 100000,
        currency: 'USD',
        type: 'hybrid_transfer',
        steps: [
          { network: 'xrp-ledger', purpose: 'fast_settlement' },
          { network: 'ethereum-polygon', purpose: 'tokenization' }
        ]
      };

      // Router should handle multi-step transactions
      const routingDecision = await smartRouter.route(complexTransaction, {
        allowMultiNetwork: true
      });

      expect(routingDecision.success).toBe(true);
      expect(routingDecision.multiNetwork).toBe(true);
      expect(routingDecision.steps).toHaveLength(2);
    });

    test('should fall back to alternative networks on failure', async () => {
      // Simulate primary network failure
      const transaction = {
        id: 'FALLBACK-123',
        amount: 25000,
        currency: 'USD',
        urgency: 'high'
      };

      // Mock primary gateway failure
      jest.spyOn(xrpGateway, 'getNetworkHealth').mockResolvedValue({
        isHealthy: false,
        error: 'Network congestion'
      });

      const routingDecision = await smartRouter.route(transaction, {
        enableFallback: true
      });

      expect(routingDecision.success).toBe(true);
      expect(routingDecision.fallbackUsed).toBe(true);
      expect(routingDecision.targetNetwork).toBe('ethereum-polygon'); // Should fallback
    });
  });

  describe('Performance and Monitoring', () => {
    test('should track comprehensive metrics across all components', async () => {
      // Process multiple transactions to generate metrics
      const transactions = [
        { id: 'PERF-1', amount: 1000, currency: 'USD' },
        { id: 'PERF-2', amount: 5000, currency: 'EUR' },
        { id: 'PERF-3', amount: 25000, currency: 'GBP' }
      ];

      for (const tx of transactions) {
        await smartRouter.route(tx);
      }

      // Verify router metrics
      const routerMetrics = smartRouter.getMetrics();
      expect(routerMetrics.totalTransactions).toBeGreaterThanOrEqual(3);
      expect(routerMetrics.averageLatency).toBeGreaterThan(0);

      // Verify gateway metrics
      const xrpMetrics = xrpGateway.getMetrics();
      expect(xrpMetrics.networkType).toBe('xrp-ledger');
      
      const ethMetrics = ethereumGateway.getMetrics();
      expect(ethMetrics.networkType).toBe('ethereum-polygon');
    });

    test('should handle high-volume transaction processing', async () => {
      // Simulate high-volume processing
      const batchSize = 10;
      const transactions = Array.from({ length: batchSize }, (_, i) => ({
        id: `BATCH-${i}`,
        amount: 1000 + i * 100,
        currency: 'USD'
      }));

      const startTime = Date.now();
      
      // Process all transactions
      const results = await Promise.all(
        transactions.map(tx => smartRouter.route(tx))
      );

      const processingTime = Date.now() - startTime;

      // Verify all processed successfully
      expect(results).toHaveLength(batchSize);
      results.forEach(result => {
        expect(result.success).toBe(true);
      });

      // Performance should be reasonable (< 5 seconds for 10 transactions)
      expect(processingTime).toBeLessThan(5000);
      
      console.log(`Processed ${batchSize} transactions in ${processingTime}ms`);
    });
  });
});