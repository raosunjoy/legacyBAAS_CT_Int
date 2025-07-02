/**
 * Smart Router - Core Transaction Routing Engine
 * Routes transactions between legacy banking systems and blockchain networks
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Implements intelligent routing based on amount, currency, compliance, and network conditions
 */

const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const EventEmitter = require('events');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'smart-router' }
});

/**
 * Blockchain Network Types
 */
const BLOCKCHAIN_NETWORKS = {
  XRP: 'xrp-ledger',
  CORDA: 'r3-corda',
  ETHEREUM_L2: 'ethereum-polygon', 
  ALGORAND: 'algorand'
};

/**
 * Transaction Types
 */
const TRANSACTION_TYPES = {
  CROSS_BORDER_PAYMENT: 'cross_border_payment',
  TRADE_FINANCE: 'trade_finance',
  TOKENIZED_DEPOSIT: 'tokenized_deposit',
  CBDC_TRANSFER: 'cbdc_transfer',
  COMPLIANCE_CHECK: 'compliance_check'
};

/**
 * Routing Decision Factors
 */
const ROUTING_FACTORS = {
  AMOUNT_THRESHOLD: 'amount_threshold',
  CURRENCY_PAIR: 'currency_pair',
  COMPLIANCE_LEVEL: 'compliance_level',
  NETWORK_CONGESTION: 'network_congestion',
  COST_OPTIMIZATION: 'cost_optimization',
  SPEED_REQUIREMENT: 'speed_requirement',
  BANK_PREFERENCE: 'bank_preference'
};

/**
 * Smart Router Class
 * Central orchestrator for cross-ledger transaction routing
 */
class SmartRouter extends EventEmitter {
  constructor(config = {}) {
    super();
    
    this.config = {
      // Default routing thresholds
      highValueThreshold: config.highValueThreshold || 100000, // $100K
      fastRouteThreshold: config.fastRouteThreshold || 10000,   // $10K
      
      // Network preferences
      defaultNetwork: config.defaultNetwork || BLOCKCHAIN_NETWORKS.XRP,
      fallbackNetwork: config.fallbackNetwork || BLOCKCHAIN_NETWORKS.CORDA,
      
      // Routing rules
      enableCostOptimization: config.enableCostOptimization !== false,
      enableComplianceCheck: config.enableComplianceCheck !== false,
      enableLoadBalancing: config.enableLoadBalancing !== false,
      
      // Performance settings
      routingTimeout: config.routingTimeout || 30000, // 30 seconds
      maxRetries: config.maxRetries || 3,
      
      ...config
    };

    // Initialize components
    this.routingRules = new Map();
    this.networkGateways = new Map();
    this.routingHistory = [];
    this.networkMetrics = new Map();
    this.startTime = Date.now();
    
    // Initialize default routing rules
    this.initializeDefaultRules();
    
    logger.info('Smart Router initialized', {
      config: this.config,
      supportedNetworks: Object.values(BLOCKCHAIN_NETWORKS)
    });
  }

  /**
   * Route transaction to optimal blockchain network
   * @param {Object} transaction - Parsed transaction object
   * @param {Object} context - Additional routing context
   * @returns {Promise<Object>} Routing decision
   */
  async route(transaction, context = {}) {
    const routingId = uuidv4();
    const startTime = Date.now();

    try {
      logger.info('Starting transaction routing', {
        routingId,
        transactionId: transaction.id,
        messageType: transaction.messageType,
        amount: transaction.amount,
        currency: transaction.currency
      });

      // Validate transaction
      this.validateTransaction(transaction);

      // Analyze routing factors
      const routingFactors = await this.analyzeRoutingFactors(transaction, context);

      // Apply routing rules
      const routingDecision = await this.applyRoutingRules(transaction, routingFactors);

      // Validate network availability
      await this.validateNetworkAvailability(routingDecision.targetNetwork);

      // Record routing decision
      const routingResult = {
        routingId,
        transactionId: transaction.id,
        sourceType: 'swift',
        targetNetwork: routingDecision.targetNetwork,
        routingFactors,
        decision: routingDecision,
        complianceFlags: transaction.compliance || routingFactors.compliance,
        timestamp: new Date().toISOString(),
        processingTime: Date.now() - startTime,
        status: 'routed'
      };

      // Store in routing history
      this.routingHistory.push(routingResult);

      // Emit routing event
      this.emit('transactionRouted', routingResult);

      logger.info('Transaction routing completed', {
        routingId,
        targetNetwork: routingDecision.targetNetwork,
        processingTime: routingResult.processingTime,
        confidence: routingDecision.confidence
      });

      return routingResult;

    } catch (error) {
      logger.error('Transaction routing failed', {
        routingId,
        transactionId: transaction ? transaction.id : null,
        error: error.message,
        processingTime: Date.now() - startTime
      });

      this.emit('routingError', {
        routingId,
        transactionId: transaction ? transaction.id : null,
        error: error.message,
        timestamp: new Date().toISOString()
      });

      throw error;
    }
  }

  /**
   * Validate transaction for routing
   * @param {Object} transaction - Transaction to validate
   */
  validateTransaction(transaction) {
    if (!transaction) {
      throw new Error('Transaction is required');
    }

    if (!transaction.id) {
      throw new Error('Transaction ID is required');
    }

    if (!transaction.messageType) {
      throw new Error('Transaction message type is required');
    }

    if (!transaction.amount || transaction.amount <= 0) {
      throw new Error('Valid transaction amount is required');
    }

    if (!transaction.currency) {
      throw new Error('Transaction currency is required');
    }

    // Validate supported message types
    const supportedTypes = ['MT103', 'MT202'];
    if (!supportedTypes.includes(transaction.messageType)) {
      throw new Error(`Unsupported message type: ${transaction.messageType}`);
    }
  }

  /**
   * Analyze factors that influence routing decisions
   * @param {Object} transaction - Transaction to analyze
   * @param {Object} context - Additional context
   * @returns {Promise<Object>} Routing factors
   */
  async analyzeRoutingFactors(transaction, context) {
    const factors = {
      // Transaction characteristics
      amount: transaction.amount,
      currency: transaction.currency,
      messageType: transaction.messageType,
      urgency: context.urgency || 'normal',
      
      // Amount-based factors
      isHighValue: transaction.amount >= this.config.highValueThreshold,
      isFastRoute: transaction.amount >= this.config.fastRouteThreshold,
      
      // Currency pair analysis
      currencyPair: this.analyzeCurrencyPair(transaction),
      
      // Compliance requirements
      complianceLevel: this.analyzeComplianceRequirements(transaction, context),
      
      // Network conditions
      networkMetrics: await this.getNetworkMetrics(),
      
      // Cost analysis
      costAnalysis: await this.analyzeCosts(transaction),
      
      // Bank preferences
      bankPreferences: context.bankPreferences || {}
    };

    return factors;
  }

  /**
   * Apply routing rules to determine target network
   * @param {Object} transaction - Transaction object
   * @param {Object} factors - Routing factors
   * @returns {Promise<Object>} Routing decision
   */
  async applyRoutingRules(transaction, factors) {
    const decisions = [];

    // Rule 1: Currency-based routing
    const currencyDecision = this.applyCurrencyRules(transaction, factors);
    if (currencyDecision) decisions.push(currencyDecision);

    // Rule 2: Amount-based routing
    const amountDecision = this.applyAmountRules(transaction, factors);
    if (amountDecision) decisions.push(amountDecision);

    // Rule 3: Compliance-based routing
    const complianceDecision = this.applyComplianceRules(transaction, factors);
    if (complianceDecision) decisions.push(complianceDecision);

    // Rule 4: Performance-based routing
    const performanceDecision = this.applyPerformanceRules(transaction, factors);
    if (performanceDecision) decisions.push(performanceDecision);

    // Rule 5: Cost optimization
    const costDecision = this.applyCostRules(transaction, factors);
    if (costDecision) decisions.push(costDecision);

    // Resolve conflicts and select best decision
    const finalDecision = this.resolveRoutingConflicts(decisions, factors);

    return finalDecision;
  }

  /**
   * Apply currency-based routing rules
   * @param {Object} transaction - Transaction object
   * @param {Object} factors - Routing factors
   * @returns {Object} Routing decision
   */
  applyCurrencyRules(transaction, factors) {
    const { currency } = transaction;
    const { currencyPair } = factors;

    // XRP for high-liquidity corridors
    const xrpOptimalPairs = ['USD-MXN', 'USD-PHP', 'EUR-USD', 'GBP-USD'];
    if (xrpOptimalPairs.includes(currencyPair)) {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.XRP,
        reason: 'Optimal liquidity corridor for XRP',
        confidence: 0.9,
        priority: 80
      };
    }

    // Corda for EUR and regulated currencies
    if (['EUR', 'GBP', 'CHF'].includes(currency)) {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.CORDA,
        reason: 'Regulated currency preference for Corda',
        confidence: 0.8,
        priority: 70
      };
    }

    return null;
  }

  /**
   * Apply amount-based routing rules
   * @param {Object} transaction - Transaction object
   * @param {Object} factors - Routing factors
   * @returns {Object} Routing decision
   */
  applyAmountRules(transaction, factors) {
    const { amount } = transaction;
    const { isHighValue, isFastRoute } = factors;

    // High-value transactions to Corda for privacy
    if (isHighValue) {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.CORDA,
        reason: 'High-value transaction requires privacy',
        confidence: 0.85,
        priority: 85
      };
    }

    // Fast route for medium amounts
    if (isFastRoute && amount < this.config.highValueThreshold) {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.XRP,
        reason: 'Fast settlement required',
        confidence: 0.8,
        priority: 75
      };
    }

    // Small amounts to most cost-effective network
    if (amount < 1000) {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.ETHEREUM_L2,
        reason: 'Cost-optimized for small amounts',
        confidence: 0.7,
        priority: 60
      };
    }

    return null;
  }

  /**
   * Apply compliance-based routing rules
   * @param {Object} transaction - Transaction object
   * @param {Object} factors - Routing factors
   * @returns {Object} Routing decision
   */
  applyComplianceRules(transaction, factors) {
    const { complianceLevel } = factors;

    // High compliance requirements
    if (complianceLevel === 'high') {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.CORDA,
        reason: 'High compliance requirements',
        confidence: 0.9,
        priority: 90
      };
    }

    // CBDC-compatible routing
    if (factors.requiresCBDC) {
      return {
        targetNetwork: BLOCKCHAIN_NETWORKS.ALGORAND,
        reason: 'CBDC compatibility required',
        confidence: 0.95,
        priority: 95
      };
    }

    return null;
  }

  /**
   * Apply performance-based routing rules
   * @param {Object} transaction - Transaction object
   * @param {Object} factors - Routing factors
   * @returns {Object} Routing decision
   */
  applyPerformanceRules(transaction, factors) {
    const { networkMetrics, urgency } = factors;

    // Route to fastest available network for urgent transactions
    if (urgency === 'urgent') {
      const fastestNetwork = this.findFastestNetwork(networkMetrics);
      return {
        targetNetwork: fastestNetwork,
        reason: 'Urgent transaction requires fastest network',
        confidence: 0.8,
        priority: 85
      };
    }

    // Avoid congested networks
    const availableNetworks = this.filterCongestedNetworks(networkMetrics);
    if (availableNetworks.length > 0) {
      return {
        targetNetwork: availableNetworks[0],
        reason: 'Avoiding network congestion',
        confidence: 0.7,
        priority: 65
      };
    }

    return null;
  }

  /**
   * Apply cost optimization rules
   * @param {Object} transaction - Transaction object
   * @param {Object} factors - Routing factors
   * @returns {Object} Routing decision
   */
  applyCostRules(transaction, factors) {
    if (!this.config.enableCostOptimization) return null;

    const { costAnalysis } = factors;
    
    // Find most cost-effective network
    const cheapestNetwork = this.findCheapestNetwork(costAnalysis);
    
    return {
      targetNetwork: cheapestNetwork,
      reason: 'Cost optimization',
      confidence: 0.6,
      priority: 50
    };
  }

  /**
   * Resolve conflicts between routing decisions
   * @param {Array} decisions - Array of routing decisions
   * @param {Object} factors - Routing factors
   * @returns {Object} Final routing decision
   */
  resolveRoutingConflicts(decisions, factors) {
    // Filter decisions to only include networks with registered and connected gateways
    const availableDecisions = decisions.filter(decision => {
      const gateway = this.networkGateways.get(decision.targetNetwork);
      return gateway && gateway.isConnected !== false;
    });

    if (availableDecisions.length === 0) {
      // Fallback to first connected registered gateway if available
      const connectedNetworks = Array.from(this.networkGateways.keys()).filter(network => {
        const gateway = this.networkGateways.get(network);
        return gateway && gateway.isConnected !== false;
      });
      
      if (connectedNetworks.length > 0) {
        return {
          targetNetwork: connectedNetworks[0],
          reason: 'Fallback to connected gateway',
          confidence: 0.5,
          priority: 10
        };
      }
      
      // Original fallback if no gateways registered
      return {
        targetNetwork: this.config.defaultNetwork,
        reason: 'Default network fallback',
        confidence: 0.5,
        priority: 10
      };
    }

    if (availableDecisions.length === 1) {
      return availableDecisions[0];
    }

    // Sort by priority (highest first) and confidence
    availableDecisions.sort((a, b) => {
      if (a.priority !== b.priority) {
        return b.priority - a.priority;
      }
      return b.confidence - a.confidence;
    });

    // Return highest priority decision
    const winner = availableDecisions[0];
    
    logger.info('Routing conflict resolved', {
      totalDecisions: availableDecisions.length,
      winningDecision: winner,
      allDecisions: availableDecisions
    });

    return winner;
  }

  /**
   * Initialize default routing rules
   */
  initializeDefaultRules() {
    // Currency pair mappings
    this.routingRules.set('USD-MXN', BLOCKCHAIN_NETWORKS.XRP);
    this.routingRules.set('USD-PHP', BLOCKCHAIN_NETWORKS.XRP);
    this.routingRules.set('EUR-USD', BLOCKCHAIN_NETWORKS.XRP);
    this.routingRules.set('EUR-GBP', BLOCKCHAIN_NETWORKS.CORDA);
    
    // Amount thresholds
    this.routingRules.set('high_value', BLOCKCHAIN_NETWORKS.CORDA);
    this.routingRules.set('fast_settlement', BLOCKCHAIN_NETWORKS.XRP);
    this.routingRules.set('cost_optimized', BLOCKCHAIN_NETWORKS.ETHEREUM_L2);
    
    logger.info('Default routing rules initialized', {
      ruleCount: this.routingRules.size
    });
  }

  /**
   * Analyze currency pair for routing optimization
   * @param {Object} transaction - Transaction object
   * @returns {string} Currency pair string
   */
  analyzeCurrencyPair(transaction) {
    // For now, assume source currency is USD for MT103
    // In real implementation, extract from SWIFT message
    const sourceCurrency = 'USD';
    const targetCurrency = transaction.currency;
    
    return `${sourceCurrency}-${targetCurrency}`;
  }

  /**
   * Analyze compliance requirements
   * @param {Object} transaction - Transaction object
   * @param {Object} context - Additional context
   * @returns {string} Compliance level
   */
  analyzeComplianceRequirements(transaction, context) {
    // High compliance for large amounts or regulated entities
    if (transaction.amount >= 50000) return 'high';
    if (context.regulatedEntity) return 'high';
    if (context.sanctionsCheck) return 'high';
    
    return 'standard';
  }

  /**
   * Get current network metrics
   * @returns {Promise<Object>} Network performance metrics
   */
  async getNetworkMetrics() {
    // Simulated network metrics - in production, fetch from monitoring systems
    return {
      [BLOCKCHAIN_NETWORKS.XRP]: {
        latency: 3.2,
        throughput: 1500,
        congestion: 0.3,
        cost: 0.001,
        availability: 0.99
      },
      [BLOCKCHAIN_NETWORKS.CORDA]: {
        latency: 5.1,
        throughput: 800,
        congestion: 0.2,
        cost: 0.005,
        availability: 0.98
      },
      [BLOCKCHAIN_NETWORKS.ETHEREUM_L2]: {
        latency: 2.8,
        throughput: 2000,
        congestion: 0.4,
        cost: 0.002,
        availability: 0.97
      },
      [BLOCKCHAIN_NETWORKS.ALGORAND]: {
        latency: 4.5,
        throughput: 1000,
        congestion: 0.1,
        cost: 0.001,
        availability: 0.99
      }
    };
  }

  /**
   * Analyze transaction costs across networks
   * @param {Object} transaction - Transaction object
   * @returns {Promise<Object>} Cost analysis
   */
  async analyzeCosts(transaction) {
    const networkMetrics = await this.getNetworkMetrics();
    const costs = {};
    
    for (const [network, metrics] of Object.entries(networkMetrics)) {
      costs[network] = metrics.cost * transaction.amount * 0.001; // Percentage-based fee
    }
    
    return costs;
  }

  /**
   * Find fastest available network
   * @param {Object} networkMetrics - Network performance metrics
   * @returns {string} Fastest network identifier
   */
  findFastestNetwork(networkMetrics) {
    let fastestNetwork = this.config.defaultNetwork;
    let lowestLatency = Infinity;
    
    for (const [network, metrics] of Object.entries(networkMetrics)) {
      if (metrics.latency < lowestLatency && metrics.availability > 0.95) {
        lowestLatency = metrics.latency;
        fastestNetwork = network;
      }
    }
    
    return fastestNetwork;
  }

  /**
   * Filter out congested networks
   * @param {Object} networkMetrics - Network performance metrics
   * @returns {Array} Available networks
   */
  filterCongestedNetworks(networkMetrics) {
    return Object.keys(networkMetrics).filter(network => 
      networkMetrics[network].congestion < 0.7 && 
      networkMetrics[network].availability > 0.95
    );
  }

  /**
   * Find most cost-effective network
   * @param {Object} costAnalysis - Cost analysis results
   * @returns {string} Cheapest network identifier
   */
  findCheapestNetwork(costAnalysis) {
    let cheapestNetwork = this.config.defaultNetwork;
    let lowestCost = Infinity;
    
    for (const [network, cost] of Object.entries(costAnalysis)) {
      if (cost < lowestCost) {
        lowestCost = cost;
        cheapestNetwork = network;
      }
    }
    
    return cheapestNetwork;
  }

  /**
   * Validate network availability
   * @param {string} network - Network identifier
   * @returns {Promise<boolean>} Network availability
   */
  async validateNetworkAvailability(network) {
    const metrics = await this.getNetworkMetrics();
    const networkMetrics = metrics[network];
    
    if (!networkMetrics || networkMetrics.availability < 0.95) {
      throw new Error(`Network ${network} is not available`);
    }
    
    return true;
  }

  /**
   * Register a blockchain network gateway
   * @param {string} network - Network identifier
   * @param {Object} gateway - Gateway instance
   */
  registerGateway(network, gateway) {
    this.networkGateways.set(network, gateway);
    logger.info('Network gateway registered', { network });
  }

  /**
   * Get a registered blockchain network gateway
   * @param {string} network - Network identifier
   * @returns {Object} Gateway instance
   */
  getGateway(network) {
    const gateway = this.networkGateways.get(network);
    if (!gateway) {
      throw new Error(`No gateway registered for network: ${network}`);
    }
    return gateway;
  }

  /**
   * Get router metrics
   * @returns {Object} Router metrics
   */
  getMetrics() {
    const totalTransactions = this.routingHistory.length;
    const successfulRoutes = this.routingHistory.filter(route => route.status === 'routed').length;
    const averageProcessingTime = totalTransactions > 0 
      ? this.routingHistory.reduce((sum, route) => sum + route.processingTime, 0) / totalTransactions 
      : 0;

    return {
      totalTransactions,
      successfulRoutes,
      averageProcessingTime: Math.round(averageProcessingTime),
      networkDistribution: this.getNetworkDistribution(),
      uptime: Date.now() - this.startTime
    };
  }

  /**
   * Get network distribution statistics
   * @returns {Object} Network distribution
   */
  getNetworkDistribution() {
    const distribution = {};
    for (const route of this.routingHistory) {
      const network = route.targetNetwork;
      distribution[network] = (distribution[network] || 0) + 1;
    }
    return distribution;
  }

  /**
   * Get routing statistics
   * @returns {Object} Routing statistics
   */
  getRoutingStats() {
    const stats = {
      totalTransactions: this.routingHistory.length,
      averageProcessingTime: 0,
      networkDistribution: {},
      successRate: 0
    };

    if (this.routingHistory.length > 0) {
      // Calculate average processing time
      const totalTime = this.routingHistory.reduce((sum, route) => sum + route.processingTime, 0);
      stats.averageProcessingTime = totalTime / this.routingHistory.length;

      // Calculate network distribution
      for (const route of this.routingHistory) {
        const network = route.targetNetwork;
        stats.networkDistribution[network] = (stats.networkDistribution[network] || 0) + 1;
      }

      // Calculate success rate
      const successfulRoutes = this.routingHistory.filter(route => route.status === 'routed').length;
      stats.successRate = successfulRoutes / this.routingHistory.length;
    }

    return stats;
  }

  /**
   * Clear routing history (for testing/maintenance)
   */
  clearRoutingHistory() {
    this.routingHistory = [];
    logger.info('Routing history cleared');
  }
}

module.exports = {
  SmartRouter,
  BLOCKCHAIN_NETWORKS,
  TRANSACTION_TYPES,
  ROUTING_FACTORS
};