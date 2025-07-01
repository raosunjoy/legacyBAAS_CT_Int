/**
 * Base Blockchain Gateway
 * Abstract base class for all blockchain network gateways
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Provides common interface and functionality for multi-ledger operations
 */

const winston = require('winston');
const { EventEmitter } = require('events');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'blockchain-gateway' }
});

/**
 * Transaction Status Enumeration
 */
const TRANSACTION_STATUS = {
  PENDING: 'pending',
  SUBMITTED: 'submitted',
  CONFIRMED: 'confirmed',
  FAILED: 'failed',
  REJECTED: 'rejected'
};

/**
 * Network Types
 */
const NETWORK_TYPES = {
  XRP: 'xrp-ledger',
  CORDA: 'r3-corda',
  ETHEREUM_L2: 'ethereum-polygon',
  ALGORAND: 'algorand'
};

/**
 * Base Blockchain Gateway Class
 * Abstract class that all blockchain gateways must extend
 */
class BaseBlockchainGateway extends EventEmitter {
  constructor(networkType, config = {}) {
    super();
    
    if (this.constructor === BaseBlockchainGateway) {
      throw new Error('BaseBlockchainGateway is abstract and cannot be instantiated directly');
    }
    
    this.networkType = networkType;
    this.config = {
      // Default configuration
      timeout: config.timeout || 30000, // 30 seconds
      retryAttempts: config.retryAttempts || 3,
      retryDelay: config.retryDelay || 1000, // 1 second
      
      // Network-specific configuration
      testMode: config.testMode || false,
      enableLogging: config.enableLogging !== false,
      
      ...config
    };
    
    // Gateway state
    this.isConnected = false;
    this.connectionAttempts = 0;
    this.lastHealthCheck = null;
    this.transactionHistory = new Map();
    this.pendingTransactions = new Map();
    
    // Performance metrics
    this.metrics = {
      totalTransactions: 0,
      successfulTransactions: 0,
      failedTransactions: 0,
      averageLatency: 0,
      lastLatency: 0
    };
    
    logger.info('Blockchain gateway initialized', {
      networkType: this.networkType,
      testMode: this.config.testMode
    });
  }

  /**
   * Connect to the blockchain network
   * Must be implemented by subclasses
   * @returns {Promise<boolean>} Connection success
   */
  async connect() {
    throw new Error('connect() method must be implemented by subclass');
  }

  /**
   * Disconnect from the blockchain network
   * Must be implemented by subclasses
   * @returns {Promise<boolean>} Disconnection success
   */
  async disconnect() {
    throw new Error('disconnect() method must be implemented by subclass');
  }

  /**
   * Submit transaction to blockchain network
   * Must be implemented by subclasses
   * @param {Object} transaction - Transaction to submit
   * @returns {Promise<Object>} Transaction result
   */
  async submitTransaction(transaction) {
    throw new Error('submitTransaction() method must be implemented by subclass');
  }

  /**
   * Get transaction status from blockchain
   * Must be implemented by subclasses
   * @param {string} transactionId - Transaction ID to query
   * @returns {Promise<Object>} Transaction status
   */
  async getTransactionStatus(transactionId) {
    throw new Error('getTransactionStatus() method must be implemented by subclass');
  }

  /**
   * Get network health information
   * Must be implemented by subclasses
   * @returns {Promise<Object>} Network health metrics
   */
  async getNetworkHealth() {
    throw new Error('getNetworkHealth() method must be implemented by subclass');
  }

  /**
   * Validate transaction before submission
   * Can be overridden by subclasses for network-specific validation
   * @param {Object} transaction - Transaction to validate
   * @returns {boolean} Validation result
   */
  validateTransaction(transaction) {
    if (!transaction) {
      throw new Error('Transaction is required');
    }

    if (!transaction.id) {
      throw new Error('Transaction ID is required');
    }

    if (!transaction.amount || transaction.amount <= 0) {
      throw new Error('Valid transaction amount is required');
    }

    if (!transaction.currency) {
      throw new Error('Transaction currency is required');
    }

    if (!transaction.sender) {
      throw new Error('Transaction sender is required');
    }

    if (!transaction.receiver) {
      throw new Error('Transaction receiver is required');
    }

    return true;
  }

  /**
   * Process transaction with retry logic and monitoring
   * @param {Object} transaction - Transaction to process
   * @returns {Promise<Object>} Processing result
   */
  async processTransaction(transaction) {
    const startTime = Date.now();
    const processingId = uuidv4();
    
    try {
      logger.info('Processing blockchain transaction', {
        processingId,
        transactionId: transaction.id,
        networkType: this.networkType,
        amount: transaction.amount,
        currency: transaction.currency
      });

      // Validate transaction
      this.validateTransaction(transaction);

      // Ensure network connection
      await this.ensureConnection();

      // Submit transaction with retry logic
      const result = await this.submitWithRetry(transaction);

      // Track transaction
      this.trackTransaction(transaction.id, result);

      // Update metrics
      const latency = Date.now() - startTime;
      this.updateMetrics(true, latency);

      // Emit success event
      this.emit('transactionSubmitted', {
        processingId,
        transactionId: transaction.id,
        networkType: this.networkType,
        result,
        latency
      });

      logger.info('Transaction processed successfully', {
        processingId,
        transactionId: transaction.id,
        networkType: this.networkType,
        latency,
        resultId: result.id
      });

      return {
        success: true,
        transactionId: transaction.id,
        networkTransactionId: result.id,
        status: result.status,
        networkType: this.networkType,
        latency,
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      const latency = Date.now() - startTime;
      this.updateMetrics(false, latency);

      logger.error('Transaction processing failed', {
        processingId,
        transactionId: transaction.id,
        networkType: this.networkType,
        error: error.message,
        latency
      });

      // Emit error event
      this.emit('transactionFailed', {
        processingId,
        transactionId: transaction.id,
        networkType: this.networkType,
        error: error.message,
        latency
      });

      throw error;
    }
  }

  /**
   * Submit transaction with retry logic
   * @param {Object} transaction - Transaction to submit
   * @returns {Promise<Object>} Submission result
   */
  async submitWithRetry(transaction) {
    let lastError;
    
    for (let attempt = 1; attempt <= this.config.retryAttempts; attempt++) {
      try {
        const result = await this.submitTransaction(transaction);
        
        if (attempt > 1) {
          logger.info('Transaction submitted successfully after retry', {
            transactionId: transaction.id,
            attempt,
            networkType: this.networkType
          });
        }
        
        return result;
        
      } catch (error) {
        lastError = error;
        
        logger.warn('Transaction submission failed', {
          transactionId: transaction.id,
          attempt,
          maxAttempts: this.config.retryAttempts,
          error: error.message,
          networkType: this.networkType
        });
        
        // Don't retry on validation errors
        if (this.isValidationError(error)) {
          throw error;
        }
        
        // Wait before retry (exponential backoff)
        if (attempt < this.config.retryAttempts) {
          const delay = this.config.retryDelay * Math.pow(2, attempt - 1);
          await this.sleep(delay);
        }
      }
    }
    
    throw new Error(`Transaction submission failed after ${this.config.retryAttempts} attempts: ${lastError.message}`);
  }

  /**
   * Ensure network connection is established
   * @returns {Promise<void>}
   */
  async ensureConnection() {
    if (!this.isConnected) {
      await this.connect();
    }
    
    // Perform health check if it's been a while
    const now = Date.now();
    if (!this.lastHealthCheck || (now - this.lastHealthCheck) > 60000) { // 1 minute
      try {
        await this.getNetworkHealth();
        this.lastHealthCheck = now;
      } catch (error) {
        logger.warn('Network health check failed, attempting reconnection', {
          networkType: this.networkType,
          error: error.message
        });
        
        this.isConnected = false;
        await this.connect();
      }
    }
  }

  /**
   * Track transaction in local store
   * @param {string} transactionId - Transaction ID
   * @param {Object} result - Transaction result
   */
  trackTransaction(transactionId, result) {
    this.transactionHistory.set(transactionId, {
      id: transactionId,
      networkTransactionId: result.id,
      status: result.status,
      timestamp: new Date().toISOString(),
      networkType: this.networkType
    });

    // Add to pending if not confirmed
    if (result.status === TRANSACTION_STATUS.PENDING || result.status === TRANSACTION_STATUS.SUBMITTED) {
      this.pendingTransactions.set(transactionId, result);
    }
  }

  /**
   * Update performance metrics
   * @param {boolean} success - Whether transaction was successful
   * @param {number} latency - Transaction latency in milliseconds
   */
  updateMetrics(success, latency) {
    this.metrics.totalTransactions++;
    this.metrics.lastLatency = latency;
    
    if (success) {
      this.metrics.successfulTransactions++;
    } else {
      this.metrics.failedTransactions++;
    }
    
    // Calculate average latency (simple moving average)
    this.metrics.averageLatency = 
      (this.metrics.averageLatency * (this.metrics.totalTransactions - 1) + latency) / 
      this.metrics.totalTransactions;
  }

  /**
   * Check if error is a validation error (non-retryable)
   * @param {Error} error - Error to check
   * @returns {boolean} Whether error is validation-related
   */
  isValidationError(error) {
    const validationKeywords = ['invalid', 'validation', 'format', 'required', 'missing'];
    const message = error.message.toLowerCase();
    return validationKeywords.some(keyword => message.includes(keyword));
  }

  /**
   * Sleep utility for retry delays
   * @param {number} ms - Milliseconds to sleep
   * @returns {Promise<void>}
   */
  sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Get gateway metrics
   * @returns {Object} Performance metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate: this.metrics.totalTransactions > 0 
        ? this.metrics.successfulTransactions / this.metrics.totalTransactions 
        : 0,
      networkType: this.networkType,
      isConnected: this.isConnected,
      pendingTransactions: this.pendingTransactions.size,
      totalTrackedTransactions: this.transactionHistory.size
    };
  }

  /**
   * Get transaction history
   * @param {number} limit - Maximum number of transactions to return
   * @returns {Array} Transaction history
   */
  getTransactionHistory(limit = 100) {
    const transactions = Array.from(this.transactionHistory.values());
    return transactions
      .sort((a, b) => new Date(b.timestamp) - new Date(a.timestamp))
      .slice(0, limit);
  }

  /**
   * Clear transaction history (for testing/maintenance)
   */
  clearTransactionHistory() {
    this.transactionHistory.clear();
    this.pendingTransactions.clear();
    logger.info('Transaction history cleared', { networkType: this.networkType });
  }

  /**
   * Get network information
   * @returns {Object} Network information
   */
  getNetworkInfo() {
    return {
      networkType: this.networkType,
      isConnected: this.isConnected,
      config: {
        testMode: this.config.testMode,
        timeout: this.config.timeout,
        retryAttempts: this.config.retryAttempts
      },
      metrics: this.getMetrics()
    };
  }
}

module.exports = {
  BaseBlockchainGateway,
  TRANSACTION_STATUS,
  NETWORK_TYPES
};