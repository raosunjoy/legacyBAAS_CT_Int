/**
 * Banking Connector Factory
 * Manages creation and lifecycle of multiple banking system connectors
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Multi-Bank Architecture Framework
 */

const winston = require('winston');
const EventEmitter = require('events');
const { v4: uuidv4 } = require('uuid');

// Import base connector
const { BaseBankingConnector, TRANSACTION_STATUS, ERROR_CODES } = require('./base-banking-connector');

// Import specific bank connectors
const { TCSBaNCSConnector } = require('../tcs-bancs/bancs-connector');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'connector-factory' }
});

/**
 * Supported banking systems
 */
const SUPPORTED_BANKS = {
  TCS_BANCS: 'tcs-bancs',
  FINACLE: 'finacle',
  FLEXCUBE: 'flexcube',
  TEMENOS_T24: 'temenos-t24',
  FINASTRA_FUSION: 'finastra-fusion',
  MAMBU: 'mambu',
  AVALOQ: 'avaloq',
  SAP_BANKING: 'sap-banking',
  FISERV_DNA: 'fiserv-dna',
  JACK_HENRY: 'jack-henry'
};

/**
 * Banking Connector Factory Class
 * Manages multiple banking system connectors with load balancing and failover
 */
class BankingConnectorFactory extends EventEmitter {
  constructor(config = {}) {
    super();

    this.config = {
      // Factory configuration
      maxConnectorsPerBank: config.maxConnectorsPerBank || 5,
      connectionPoolSize: config.connectionPoolSize || 10,
      healthCheckInterval: config.healthCheckInterval || 60000,
      retryAttempts: config.retryAttempts || 3,
      
      // Load balancing
      loadBalancingStrategy: config.loadBalancingStrategy || 'round-robin', // round-robin, least-connections, random
      enableFailover: config.enableFailover !== false,
      
      // Monitoring
      enableMetrics: config.enableMetrics !== false,
      enableDetailedLogging: config.enableDetailedLogging || false,
      
      ...config
    };

    // Connector registry
    this.connectors = new Map(); // bankCode -> connector instances
    this.connectorConfigs = new Map(); // bankCode -> configuration
    this.activeConnections = new Map(); // connectionId -> connector
    
    // Load balancing state
    this.roundRobinCounters = new Map(); // bankCode -> counter
    
    // Factory metrics
    this.factoryMetrics = {
      totalConnectors: 0,
      activeConnectors: 0,
      failedConnectors: 0,
      totalTransactions: 0,
      failoverEvents: 0,
      uptime: Date.now()
    };

    logger.info('Banking Connector Factory initialized', {
      maxConnectorsPerBank: this.config.maxConnectorsPerBank,
      loadBalancingStrategy: this.config.loadBalancingStrategy,
      enableFailover: this.config.enableFailover
    });
  }

  /**
   * Register a bank connector configuration
   * @param {string} bankCode - Unique bank identifier
   * @param {Object} connectorConfig - Connector configuration
   * @returns {Promise<void>}
   */
  async registerBank(bankCode, connectorConfig) {
    try {
      logger.info('Registering bank connector', {
        bankCode,
        bankType: connectorConfig.type || 'unknown'
      });

      // Validate configuration
      this.validateConnectorConfig(bankCode, connectorConfig);
      
      // Store configuration
      this.connectorConfigs.set(bankCode, {
        ...connectorConfig,
        bankCode,
        registeredAt: new Date().toISOString()
      });

      // Initialize connector pool for this bank
      this.connectors.set(bankCode, []);
      this.roundRobinCounters.set(bankCode, 0);

      // Create initial connectors
      const initialConnectors = connectorConfig.initialConnectors || 1;
      for (let i = 0; i < initialConnectors; i++) {
        await this.createConnector(bankCode);
      }

      this.emit('bank:registered', {
        bankCode,
        connectorsCreated: initialConnectors,
        timestamp: new Date().toISOString()
      });

      logger.info('Bank connector registered successfully', {
        bankCode,
        initialConnectors
      });

    } catch (error) {
      logger.error('Bank registration failed', {
        bankCode,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Create a new connector instance for a bank
   * @param {string} bankCode 
   * @returns {Promise<BaseBankingConnector>}
   */
  async createConnector(bankCode) {
    try {
      const config = this.connectorConfigs.get(bankCode);
      if (!config) {
        throw new Error(`Bank ${bankCode} not registered`);
      }

      // Check connector limit
      const existingConnectors = this.connectors.get(bankCode) || [];
      if (existingConnectors.length >= this.config.maxConnectorsPerBank) {
        throw new Error(`Maximum connectors reached for bank ${bankCode}`);
      }

      // Create connector based on bank type
      let connector;
      switch (config.type) {
        case SUPPORTED_BANKS.TCS_BANCS:
          connector = new TCSBaNCSConnector(config);
          break;
        
        case SUPPORTED_BANKS.FINACLE:
          connector = new FinacleConnector(config);
          break;
          
        case SUPPORTED_BANKS.FLEXCUBE:
          connector = new FlexcubeConnector(config);
          break;
          
        // Add more bank types as needed
        default:
          throw new Error(`Unsupported bank type: ${config.type}`);
      }

      // Set up event listeners
      this.setupConnectorEventListeners(connector, bankCode);

      // Initialize the connector
      await connector.initialize();

      // Add to connector pool
      existingConnectors.push(connector);
      this.connectors.set(bankCode, existingConnectors);
      this.activeConnections.set(connector.connectionId, connector);

      // Update metrics
      this.factoryMetrics.totalConnectors++;
      this.factoryMetrics.activeConnectors++;

      logger.info('Connector created successfully', {
        bankCode,
        connectionId: connector.connectionId,
        totalConnectors: existingConnectors.length
      });

      return connector;

    } catch (error) {
      this.factoryMetrics.failedConnectors++;
      
      logger.error('Connector creation failed', {
        bankCode,
        error: error.message
      });
      
      throw error;
    }
  }

  /**
   * Get an available connector for a bank using load balancing
   * @param {string} bankCode 
   * @returns {BaseBankingConnector}
   */
  getConnector(bankCode) {
    const connectors = this.connectors.get(bankCode);
    if (!connectors || connectors.length === 0) {
      throw new Error(`No connectors available for bank ${bankCode}`);
    }

    // Filter healthy connectors
    const healthyConnectors = connectors.filter(c => c.isConnected);
    if (healthyConnectors.length === 0) {
      if (this.config.enableFailover) {
        this.handleFailover(bankCode);
      }
      throw new Error(`No healthy connectors available for bank ${bankCode}`);
    }

    // Apply load balancing strategy
    let selectedConnector;
    switch (this.config.loadBalancingStrategy) {
      case 'round-robin':
        selectedConnector = this.selectRoundRobin(bankCode, healthyConnectors);
        break;
        
      case 'least-connections':
        selectedConnector = this.selectLeastConnections(healthyConnectors);
        break;
        
      case 'random':
        selectedConnector = this.selectRandom(healthyConnectors);
        break;
        
      default:
        selectedConnector = healthyConnectors[0];
    }

    logger.debug('Connector selected', {
      bankCode,
      connectionId: selectedConnector.connectionId,
      strategy: this.config.loadBalancingStrategy,
      availableConnectors: healthyConnectors.length
    });

    return selectedConnector;
  }

  /**
   * Process a transaction using the appropriate bank connector
   * @param {string} bankCode 
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processTransaction(bankCode, transaction) {
    const startTime = Date.now();
    let attempts = 0;
    let lastError;

    while (attempts < this.config.retryAttempts) {
      try {
        const connector = this.getConnector(bankCode);
        const result = await connector.processTransaction(transaction);
        
        this.factoryMetrics.totalTransactions++;
        
        this.emit('transaction:completed', {
          bankCode,
          transactionId: transaction.id,
          responseTime: Date.now() - startTime,
          attempts: attempts + 1
        });

        return result;

      } catch (error) {
        attempts++;
        lastError = error;
        
        logger.warn('Transaction attempt failed', {
          bankCode,
          transactionId: transaction.id,
          attempt: attempts,
          error: error.message
        });

        if (attempts < this.config.retryAttempts) {
          await this.delay(1000 * attempts); // Exponential backoff
        }
      }
    }

    this.emit('transaction:failed', {
      bankCode,
      transactionId: transaction.id,
      attempts,
      error: lastError.message
    });

    throw lastError;
  }

  /**
   * Round-robin load balancing
   */
  selectRoundRobin(bankCode, connectors) {
    const counter = this.roundRobinCounters.get(bankCode) || 0;
    const selectedIndex = counter % connectors.length;
    this.roundRobinCounters.set(bankCode, counter + 1);
    return connectors[selectedIndex];
  }

  /**
   * Least connections load balancing
   */
  selectLeastConnections(connectors) {
    return connectors.reduce((least, current) => {
      return current.activeTransactions.size < least.activeTransactions.size 
        ? current 
        : least;
    });
  }

  /**
   * Random load balancing
   */
  selectRandom(connectors) {
    const randomIndex = Math.floor(Math.random() * connectors.length);
    return connectors[randomIndex];
  }

  /**
   * Handle failover scenario
   */
  async handleFailover(bankCode) {
    logger.warn('Initiating failover for bank', { bankCode });
    
    this.factoryMetrics.failoverEvents++;
    
    try {
      // Try to create a new connector
      await this.createConnector(bankCode);
      
      this.emit('failover:success', {
        bankCode,
        timestamp: new Date().toISOString()
      });
      
    } catch (error) {
      this.emit('failover:failed', {
        bankCode,
        error: error.message,
        timestamp: new Date().toISOString()
      });
    }
  }

  /**
   * Setup event listeners for a connector
   */
  setupConnectorEventListeners(connector, bankCode) {
    connector.on('connected', (event) => {
      this.emit('connector:connected', { ...event, bankCode });
    });

    connector.on('disconnected', (event) => {
      this.factoryMetrics.activeConnectors--;
      this.activeConnections.delete(connector.connectionId);
      this.emit('connector:disconnected', { ...event, bankCode });
    });

    connector.on('error', (error) => {
      this.emit('connector:error', { 
        bankCode, 
        connectionId: connector.connectionId,
        error: error.message 
      });
    });

    connector.on('transaction:completed', (event) => {
      this.emit('transaction:completed', { ...event, bankCode });
    });

    connector.on('transaction:failed', (event) => {
      this.emit('transaction:failed', { ...event, bankCode });
    });
  }

  /**
   * Validate connector configuration
   */
  validateConnectorConfig(bankCode, config) {
    if (!bankCode) {
      throw new Error('Bank code is required');
    }

    if (!config.type) {
      throw new Error('Connector type is required');
    }

    if (!Object.values(SUPPORTED_BANKS).includes(config.type)) {
      throw new Error(`Unsupported bank type: ${config.type}`);
    }

    if (!config.clientId || !config.clientSecret) {
      throw new Error('Client credentials are required');
    }
  }

  /**
   * Get factory status and metrics
   */
  getFactoryStatus() {
    const connectorStatus = {};
    
    for (const [bankCode, connectors] of this.connectors.entries()) {
      connectorStatus[bankCode] = {
        total: connectors.length,
        healthy: connectors.filter(c => c.isConnected).length,
        connectors: connectors.map(c => c.getStatus())
      };
    }

    return {
      factoryMetrics: {
        ...this.factoryMetrics,
        uptime: Date.now() - this.factoryMetrics.uptime
      },
      connectorStatus,
      registeredBanks: Array.from(this.connectorConfigs.keys()),
      config: {
        maxConnectorsPerBank: this.config.maxConnectorsPerBank,
        loadBalancingStrategy: this.config.loadBalancingStrategy,
        enableFailover: this.config.enableFailover
      }
    };
  }

  /**
   * Get health status for all connectors
   */
  async getHealthStatus() {
    const healthStatus = {};
    
    for (const [bankCode, connectors] of this.connectors.entries()) {
      const connectorHealth = await Promise.all(
        connectors.map(async (connector) => {
          try {
            return await connector.getHealthStatus();
          } catch (error) {
            return {
              status: 'error',
              connectionId: connector.connectionId,
              error: error.message
            };
          }
        })
      );
      
      healthStatus[bankCode] = {
        overall: connectorHealth.every(h => h.status === 'healthy') ? 'healthy' : 'degraded',
        connectors: connectorHealth
      };
    }

    return {
      factoryStatus: 'healthy',
      banks: healthStatus,
      timestamp: new Date().toISOString()
    };
  }

  /**
   * Cleanup all connectors
   */
  async cleanup() {
    logger.info('Cleaning up connector factory');
    
    const cleanupPromises = [];
    
    for (const connectors of this.connectors.values()) {
      for (const connector of connectors) {
        if (typeof connector.cleanup === 'function') {
          cleanupPromises.push(connector.cleanup());
        }
      }
    }

    await Promise.all(cleanupPromises);
    
    this.connectors.clear();
    this.connectorConfigs.clear();
    this.activeConnections.clear();
    this.roundRobinCounters.clear();

    logger.info('Connector factory cleaned up');
  }

  /**
   * Utility method for delays
   */
  delay(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }
}

// Placeholder connector classes for other bank types
class FinacleConnector extends BaseBankingConnector {
  async authenticate() {
    // Implementation for Finacle authentication
    throw new Error('Finacle connector not yet implemented');
  }

  async testConnection() {
    return false;
  }

  async getAccountDetails(accountNumber, options = {}) {
    throw new Error('Finacle connector not yet implemented');
  }

  async checkAccountBalance(accountNumber, currency = null) {
    throw new Error('Finacle connector not yet implemented');
  }

  async validateTransaction(transaction) {
    throw new Error('Finacle connector not yet implemented');
  }

  async processDebit(transaction) {
    throw new Error('Finacle connector not yet implemented');
  }

  async processCredit(transaction) {
    throw new Error('Finacle connector not yet implemented');
  }

  async getTransactionStatus(transactionId) {
    throw new Error('Finacle connector not yet implemented');
  }
}

class FlexcubeConnector extends BaseBankingConnector {
  async authenticate() {
    // Implementation for Flexcube authentication
    throw new Error('Flexcube connector not yet implemented');
  }

  async testConnection() {
    return false;
  }

  async getAccountDetails(accountNumber, options = {}) {
    throw new Error('Flexcube connector not yet implemented');
  }

  async checkAccountBalance(accountNumber, currency = null) {
    throw new Error('Flexcube connector not yet implemented');
  }

  async validateTransaction(transaction) {
    throw new Error('Flexcube connector not yet implemented');
  }

  async processDebit(transaction) {
    throw new Error('Flexcube connector not yet implemented');
  }

  async processCredit(transaction) {
    throw new Error('Flexcube connector not yet implemented');
  }

  async getTransactionStatus(transactionId) {
    throw new Error('Flexcube connector not yet implemented');
  }
}

module.exports = {
  BankingConnectorFactory,
  SUPPORTED_BANKS,
  TRANSACTION_STATUS,
  ERROR_CODES
};