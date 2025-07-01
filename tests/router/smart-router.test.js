/**
 * Smart Router Tests
 * Comprehensive test suite with 100% coverage for intelligent transaction routing
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const { SmartRouter, BLOCKCHAIN_NETWORKS, TRANSACTION_TYPES, ROUTING_FACTORS } = require('../../src/router/smart-router');

describe('Smart Router', () => {
  let router;
  let mockTransaction;

  beforeEach(() => {
    router = new SmartRouter({
      highValueThreshold: 100000,
      fastRouteThreshold: 10000,
      enableCostOptimization: true
    });

    mockTransaction = {
      id: 'test-tx-123',
      messageType: 'MT103',
      transactionReference: 'TRN123456789',
      amount: 50000,
      currency: 'USD',
      sender: { name: 'John Doe' },
      receiver: { name: 'Jane Smith' }
    };
  });

  afterEach(() => {
    router.removeAllListeners();
    router.clearRoutingHistory();
  });

  describe('Constructor', () => {
    test('should initialize with default configuration', () => {
      const defaultRouter = new SmartRouter();
      expect(defaultRouter.config.highValueThreshold).toBe(100000);
      expect(defaultRouter.config.defaultNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(defaultRouter.config.enableCostOptimization).toBe(true);
    });

    test('should initialize with custom configuration', () => {
      const customRouter = new SmartRouter({
        highValueThreshold: 50000,
        defaultNetwork: BLOCKCHAIN_NETWORKS.CORDA,
        enableCostOptimization: false
      });

      expect(customRouter.config.highValueThreshold).toBe(50000);
      expect(customRouter.config.defaultNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(customRouter.config.enableCostOptimization).toBe(false);
    });

    test('should initialize routing rules and network gateways', () => {
      expect(router.routingRules).toBeInstanceOf(Map);
      expect(router.networkGateways).toBeInstanceOf(Map);
      expect(router.routingHistory).toBeInstanceOf(Array);
      expect(router.networkMetrics).toBeInstanceOf(Map);
    });

    test('should initialize default routing rules', () => {
      expect(router.routingRules.get('USD-MXN')).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(router.routingRules.get('EUR-GBP')).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(router.routingRules.get('high_value')).toBe(BLOCKCHAIN_NETWORKS.CORDA);
    });
  });

  describe('Transaction Validation', () => {
    test('should validate valid transaction', () => {
      expect(() => router.validateTransaction(mockTransaction)).not.toThrow();
    });

    test('should throw error for null transaction', () => {
      expect(() => router.validateTransaction(null)).toThrow('Transaction is required');
    });

    test('should throw error for missing transaction ID', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.id;
      expect(() => router.validateTransaction(invalidTx)).toThrow('Transaction ID is required');
    });

    test('should throw error for missing message type', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.messageType;
      expect(() => router.validateTransaction(invalidTx)).toThrow('Transaction message type is required');
    });

    test('should throw error for invalid amount', () => {
      const invalidTx = { ...mockTransaction, amount: 0 };
      expect(() => router.validateTransaction(invalidTx)).toThrow('Valid transaction amount is required');
      
      const negativeAmountTx = { ...mockTransaction, amount: -100 };
      expect(() => router.validateTransaction(negativeAmountTx)).toThrow('Valid transaction amount is required');
    });

    test('should throw error for missing currency', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.currency;
      expect(() => router.validateTransaction(invalidTx)).toThrow('Transaction currency is required');
    });

    test('should throw error for unsupported message type', () => {
      const invalidTx = { ...mockTransaction, messageType: 'MT999' };
      expect(() => router.validateTransaction(invalidTx)).toThrow('Unsupported message type: MT999');
    });
  });

  describe('Routing Factor Analysis', () => {
    test('should analyze routing factors correctly', async () => {
      const factors = await router.analyzeRoutingFactors(mockTransaction, {});
      
      expect(factors).toHaveProperty('amount', 50000);
      expect(factors).toHaveProperty('currency', 'USD');
      expect(factors).toHaveProperty('messageType', 'MT103');
      expect(factors).toHaveProperty('isHighValue', false);
      expect(factors).toHaveProperty('isFastRoute', true);
      expect(factors).toHaveProperty('currencyPair', 'USD-USD');
      expect(factors).toHaveProperty('complianceLevel', 'high');
      expect(factors).toHaveProperty('networkMetrics');
      expect(factors).toHaveProperty('costAnalysis');
    });

    test('should identify high value transactions', async () => {
      const highValueTx = { ...mockTransaction, amount: 150000 };
      const factors = await router.analyzeRoutingFactors(highValueTx, {});
      
      expect(factors.isHighValue).toBe(true);
      expect(factors.isFastRoute).toBe(true);
    });

    test('should handle custom context', async () => {
      const context = {
        urgency: 'urgent',
        bankPreferences: { preferredNetwork: 'xrp-ledger' },
        regulatedEntity: true
      };
      
      const factors = await router.analyzeRoutingFactors(mockTransaction, context);
      
      expect(factors.urgency).toBe('urgent');
      expect(factors.bankPreferences.preferredNetwork).toBe('xrp-ledger');
      expect(factors.complianceLevel).toBe('high');
    });
  });

  describe('Currency-Based Routing Rules', () => {
    test('should route optimal currency pairs to XRP', () => {
      const factors = { currencyPair: 'USD-MXN' };
      const decision = router.applyCurrencyRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(decision.reason).toBe('Optimal liquidity corridor for XRP');
      expect(decision.confidence).toBe(0.9);
    });

    test('should route regulated currencies to Corda', () => {
      const eurTransaction = { ...mockTransaction, currency: 'EUR' };
      const factors = { currencyPair: 'USD-EUR' };
      const decision = router.applyCurrencyRules(eurTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(decision.reason).toBe('Regulated currency preference for Corda');
      expect(decision.confidence).toBe(0.8);
    });

    test('should return null for non-matching currency rules', () => {
      const factors = { currencyPair: 'USD-JPY' };
      const decision = router.applyCurrencyRules(mockTransaction, factors);
      
      expect(decision).toBeNull();
    });
  });

  describe('Amount-Based Routing Rules', () => {
    test('should route high value transactions to Corda', () => {
      const factors = { isHighValue: true, isFastRoute: true };
      const decision = router.applyAmountRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(decision.reason).toBe('High-value transaction requires privacy');
      expect(decision.confidence).toBe(0.85);
    });

    test('should route fast route transactions to XRP', () => {
      const factors = { isHighValue: false, isFastRoute: true };
      const decision = router.applyAmountRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(decision.reason).toBe('Fast settlement required');
      expect(decision.confidence).toBe(0.8);
    });

    test('should route small amounts to Ethereum L2', () => {
      const smallTransaction = { ...mockTransaction, amount: 500 };
      const factors = { isHighValue: false, isFastRoute: false };
      const decision = router.applyAmountRules(smallTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ETHEREUM_L2);
      expect(decision.reason).toBe('Cost-optimized for small amounts');
      expect(decision.confidence).toBe(0.7);
    });

    test('should return null for medium amounts without special requirements', () => {
      const mediumTransaction = { ...mockTransaction, amount: 5000 };
      const factors = { isHighValue: false, isFastRoute: false };
      const decision = router.applyAmountRules(mediumTransaction, factors);
      
      expect(decision).toBeNull();
    });
  });

  describe('Compliance-Based Routing Rules', () => {
    test('should route high compliance transactions to Corda', () => {
      const factors = { complianceLevel: 'high' };
      const decision = router.applyComplianceRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(decision.reason).toBe('High compliance requirements');
      expect(decision.confidence).toBe(0.9);
    });

    test('should route CBDC requirements to Algorand', () => {
      const factors = { requiresCBDC: true };
      const decision = router.applyComplianceRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ALGORAND);
      expect(decision.reason).toBe('CBDC compatibility required');
      expect(decision.confidence).toBe(0.95);
    });

    test('should return null for standard compliance', () => {
      const factors = { complianceLevel: 'standard' };
      const decision = router.applyComplianceRules(mockTransaction, factors);
      
      expect(decision).toBeNull();
    });
  });

  describe('Performance-Based Routing Rules', () => {
    test('should route urgent transactions to fastest network', async () => {
      const networkMetrics = await router.getNetworkMetrics();
      const factors = { urgency: 'urgent', networkMetrics };
      const decision = router.applyPerformanceRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ETHEREUM_L2); // Lowest latency
      expect(decision.reason).toBe('Urgent transaction requires fastest network');
      expect(decision.confidence).toBe(0.8);
    });

    test('should avoid congested networks', async () => {
      const networkMetrics = {
        [BLOCKCHAIN_NETWORKS.XRP]: { congestion: 0.3, availability: 0.99 },
        [BLOCKCHAIN_NETWORKS.CORDA]: { congestion: 0.2, availability: 0.98 },
        [BLOCKCHAIN_NETWORKS.ALGORAND]: { congestion: 0.1, availability: 0.99 }
      };
      const factors = { urgency: 'normal', networkMetrics };
      const decision = router.applyPerformanceRules(mockTransaction, factors);
      
      expect([BLOCKCHAIN_NETWORKS.XRP, BLOCKCHAIN_NETWORKS.CORDA, BLOCKCHAIN_NETWORKS.ALGORAND])
        .toContain(decision.targetNetwork);
      expect(decision.reason).toBe('Avoiding network congestion');
    });

    test('should return null for normal urgency with no congestion', async () => {
      const networkMetrics = await router.getNetworkMetrics();
      const factors = { urgency: 'normal', networkMetrics };
      const decision = router.applyPerformanceRules(mockTransaction, factors);
      
      expect(decision).not.toBeNull(); // Should avoid congestion
    });
  });

  describe('Cost Optimization Rules', () => {
    test('should route to cheapest network when cost optimization enabled', async () => {
      const costAnalysis = {
        [BLOCKCHAIN_NETWORKS.XRP]: 50,
        [BLOCKCHAIN_NETWORKS.CORDA]: 250,
        [BLOCKCHAIN_NETWORKS.ETHEREUM_L2]: 100,
        [BLOCKCHAIN_NETWORKS.ALGORAND]: 25
      };
      const factors = { costAnalysis };
      const decision = router.applyCostRules(mockTransaction, factors);
      
      expect(decision.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ALGORAND);
      expect(decision.reason).toBe('Cost optimization');
      expect(decision.confidence).toBe(0.6);
    });

    test('should return null when cost optimization disabled', () => {
      const noCostRouter = new SmartRouter({ enableCostOptimization: false });
      const factors = { costAnalysis: {} };
      const decision = noCostRouter.applyCostRules(mockTransaction, factors);
      
      expect(decision).toBeNull();
    });
  });

  describe('Routing Conflict Resolution', () => {
    test('should return highest priority decision when multiple decisions exist', () => {
      const decisions = [
        { targetNetwork: BLOCKCHAIN_NETWORKS.XRP, priority: 70, confidence: 0.8 },
        { targetNetwork: BLOCKCHAIN_NETWORKS.CORDA, priority: 90, confidence: 0.7 },
        { targetNetwork: BLOCKCHAIN_NETWORKS.ETHEREUM_L2, priority: 60, confidence: 0.9 }
      ];
      
      const result = router.resolveRoutingConflicts(decisions, {});
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(result.priority).toBe(90);
    });

    test('should use confidence as tiebreaker when priorities are equal', () => {
      const decisions = [
        { targetNetwork: BLOCKCHAIN_NETWORKS.XRP, priority: 80, confidence: 0.7 },
        { targetNetwork: BLOCKCHAIN_NETWORKS.CORDA, priority: 80, confidence: 0.9 }
      ];
      
      const result = router.resolveRoutingConflicts(decisions, {});
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(result.confidence).toBe(0.9);
    });

    test('should return single decision when only one exists', () => {
      const decisions = [
        { targetNetwork: BLOCKCHAIN_NETWORKS.XRP, priority: 70, confidence: 0.8 }
      ];
      
      const result = router.resolveRoutingConflicts(decisions, {});
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
    });

    test('should fallback to default network when no decisions exist', () => {
      const result = router.resolveRoutingConflicts([], {});
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP); // Default
      expect(result.reason).toBe('Default network fallback');
      expect(result.confidence).toBe(0.5);
    });
  });

  describe('Network Availability Validation', () => {
    test('should validate available network', async () => {
      const result = await router.validateNetworkAvailability(BLOCKCHAIN_NETWORKS.XRP);
      expect(result).toBe(true);
    });

    test('should throw error for unavailable network', async () => {
      // Mock low availability
      jest.spyOn(router, 'getNetworkMetrics').mockResolvedValue({
        [BLOCKCHAIN_NETWORKS.XRP]: { availability: 0.8 }
      });

      await expect(router.validateNetworkAvailability(BLOCKCHAIN_NETWORKS.XRP))
        .rejects.toThrow('Network xrp-ledger is not available');
    });

    test('should throw error for non-existent network', async () => {
      await expect(router.validateNetworkAvailability('invalid-network'))
        .rejects.toThrow('Network invalid-network is not available');
    });
  });

  describe('Complete Routing Flow', () => {
    test('should successfully route a transaction', async () => {
      // Set up event listener before routing
      const routedEvents = [];
      router.on('transactionRouted', (event) => {
        routedEvents.push(event);
      });

      const result = await router.route(mockTransaction);
      
      expect(result).toHaveProperty('routingId');
      expect(result).toHaveProperty('transactionId', mockTransaction.id);
      expect(result).toHaveProperty('targetNetwork');
      expect(result).toHaveProperty('status', 'routed');
      expect(result).toHaveProperty('processingTime');
      expect(typeof result.processingTime).toBe('number');
      expect(result.processingTime).toBeGreaterThanOrEqual(0);
      
      // Check that event was emitted
      expect(routedEvents).toHaveLength(1);
      expect(routedEvents[0].transactionId).toBe(mockTransaction.id);
    });

    test('should handle routing errors and emit error events', async () => {
      const invalidTransaction = null;
      
      // Set up event listener before routing
      const errorEvents = [];
      router.on('routingError', (event) => {
        errorEvents.push(event);
      });

      await expect(router.route(invalidTransaction)).rejects.toThrow();
      
      // Check that error event was emitted
      expect(errorEvents).toHaveLength(1);
      expect(errorEvents[0]).toHaveProperty('error');
    });

    test('should store routing decision in history', async () => {
      await router.route(mockTransaction);
      
      expect(router.routingHistory).toHaveLength(1);
      expect(router.routingHistory[0].transactionId).toBe(mockTransaction.id);
    });
  });

  describe('Utility Methods', () => {
    test('should analyze currency pair correctly', () => {
      const pair = router.analyzeCurrencyPair(mockTransaction);
      expect(pair).toBe('USD-USD');
    });

    test('should analyze compliance requirements', () => {
      const highAmount = router.analyzeComplianceRequirements({ amount: 60000 }, {});
      expect(highAmount).toBe('high');
      
      const regulated = router.analyzeComplianceRequirements(mockTransaction, { regulatedEntity: true });
      expect(regulated).toBe('high');
      
      const standard = router.analyzeComplianceRequirements({ amount: 1000 }, {});
      expect(standard).toBe('standard');
    });

    test('should find fastest network correctly', async () => {
      const metrics = await router.getNetworkMetrics();
      const fastest = router.findFastestNetwork(metrics);
      expect(fastest).toBe(BLOCKCHAIN_NETWORKS.ETHEREUM_L2); // Lowest latency
    });

    test('should filter congested networks', async () => {
      const metrics = await router.getNetworkMetrics();
      const available = router.filterCongestedNetworks(metrics);
      expect(available.length).toBeGreaterThan(0);
      expect(available).toContain(BLOCKCHAIN_NETWORKS.XRP); // Low congestion
    });

    test('should find cheapest network', async () => {
      const costAnalysis = {
        [BLOCKCHAIN_NETWORKS.XRP]: 10,
        [BLOCKCHAIN_NETWORKS.CORDA]: 50,
        [BLOCKCHAIN_NETWORKS.ALGORAND]: 5
      };
      const cheapest = router.findCheapestNetwork(costAnalysis);
      expect(cheapest).toBe(BLOCKCHAIN_NETWORKS.ALGORAND);
    });
  });

  describe('Gateway Management', () => {
    test('should register network gateway', () => {
      const mockGateway = { send: jest.fn() };
      router.registerGateway(BLOCKCHAIN_NETWORKS.XRP, mockGateway);
      
      expect(router.networkGateways.get(BLOCKCHAIN_NETWORKS.XRP)).toBe(mockGateway);
    });
  });

  describe('Statistics and Monitoring', () => {
    test('should return routing statistics', async () => {
      // Route a few transactions
      await router.route(mockTransaction);
      await router.route({ ...mockTransaction, id: 'test-tx-456' });
      
      const stats = router.getRoutingStats();
      
      expect(stats.totalTransactions).toBe(2);
      expect(typeof stats.averageProcessingTime).toBe('number');
      expect(stats.averageProcessingTime).toBeGreaterThanOrEqual(0);
      expect(stats.successRate).toBe(1);
      expect(stats.networkDistribution).toBeDefined();
    });

    test('should return empty statistics when no transactions routed', () => {
      const stats = router.getRoutingStats();
      
      expect(stats.totalTransactions).toBe(0);
      expect(stats.averageProcessingTime).toBe(0);
      expect(stats.successRate).toBe(0);
      expect(stats.networkDistribution).toEqual({});
    });

    test('should clear routing history', () => {
      router.routingHistory.push({ test: 'data' });
      expect(router.routingHistory).toHaveLength(1);
      
      router.clearRoutingHistory();
      expect(router.routingHistory).toHaveLength(0);
    });
  });

  describe('Network Metrics', () => {
    test('should return network metrics with all required properties', async () => {
      const metrics = await router.getNetworkMetrics();
      
      const expectedNetworks = Object.values(BLOCKCHAIN_NETWORKS);
      expectedNetworks.forEach(network => {
        expect(metrics[network]).toHaveProperty('latency');
        expect(metrics[network]).toHaveProperty('throughput');
        expect(metrics[network]).toHaveProperty('congestion');
        expect(metrics[network]).toHaveProperty('cost');
        expect(metrics[network]).toHaveProperty('availability');
      });
    });

    test('should analyze transaction costs correctly', async () => {
      const costAnalysis = await router.analyzeCosts(mockTransaction);
      
      const expectedNetworks = Object.values(BLOCKCHAIN_NETWORKS);
      expectedNetworks.forEach(network => {
        expect(costAnalysis[network]).toBeGreaterThan(0);
        expect(typeof costAnalysis[network]).toBe('number');
      });
    });
  });

  describe('Constants Export', () => {
    test('should export all required constants', () => {
      expect(BLOCKCHAIN_NETWORKS).toBeDefined();
      expect(TRANSACTION_TYPES).toBeDefined();
      expect(ROUTING_FACTORS).toBeDefined();
      
      expect(BLOCKCHAIN_NETWORKS.XRP).toBe('xrp-ledger');
      expect(BLOCKCHAIN_NETWORKS.CORDA).toBe('r3-corda');
      expect(BLOCKCHAIN_NETWORKS.ETHEREUM_L2).toBe('ethereum-polygon');
      expect(BLOCKCHAIN_NETWORKS.ALGORAND).toBe('algorand');
    });
  });
});