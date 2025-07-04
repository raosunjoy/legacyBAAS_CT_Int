/**
 * Base Banking Connector Abstract Class
 * Provides a standardized interface for all banking system integrations
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Multi-Bank Architecture Framework
 */

const EventEmitter = require('events');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'base-banking-connector' }
});

/**
 * Standard transaction status enumeration
 */
const TRANSACTION_STATUS = {
  PENDING: 'pending',
  PROCESSING: 'processing',
  CONFIRMED: 'confirmed',
  FAILED: 'failed',
  REJECTED: 'rejected',
  PENDING_APPROVAL: 'pending_approval',
  AUTHORIZED: 'authorized',
  SETTLED: 'settled',
  CANCELLED: 'cancelled'
};

/**
 * Standard error codes
 */
const ERROR_CODES = {
  AUTHENTICATION_FAILED: 'AUTHENTICATION_FAILED',
  INSUFFICIENT_FUNDS: 'INSUFFICIENT_FUNDS',
  INVALID_ACCOUNT: 'INVALID_ACCOUNT',
  COMPLIANCE_FAILURE: 'COMPLIANCE_FAILURE',
  NETWORK_ERROR: 'NETWORK_ERROR',
  TIMEOUT_ERROR: 'TIMEOUT_ERROR',
  RATE_LIMIT_EXCEEDED: 'RATE_LIMIT_EXCEEDED',
  SERVICE_UNAVAILABLE: 'SERVICE_UNAVAILABLE',
  INVALID_TRANSACTION: 'INVALID_TRANSACTION',
  ACCOUNT_BLOCKED: 'ACCOUNT_BLOCKED'
};

/**
 * Abstract Base Banking Connector Class
 * All banking connector implementations must extend this class
 */
class BaseBankingConnector extends EventEmitter {
  constructor(config = {}) {
    super();
    
    this.config = {
      // Core configuration
      bankCode: config.bankCode || process.env.BANK_CODE,
      bankName: config.bankName || 'Unknown Bank',
      apiVersion: config.apiVersion || 'v1',
      timeout: config.timeout || 30000,
      
      // Authentication
      authMethod: config.authMethod || 'oauth2',
      clientId: config.clientId,
      clientSecret: config.clientSecret,
      
      // Integration settings
      testMode: config.testMode || false,
      retryAttempts: config.retryAttempts || 3,
      retryDelay: config.retryDelay || 1000,
      
      // Security
      enableEncryption: config.enableEncryption !== false,
      enableSignatures: config.enableSignatures !== false,
      
      // Monitoring
      enableMetrics: config.enableMetrics !== false,
      enableHealthChecks: config.enableHealthChecks !== false,
      
      ...config
    };

    // Connection state
    this.isConnected = false;
    this.connectionId = uuidv4();
    this.lastHeartbeat = null;
    
    // Performance metrics
    this.metrics = {
      totalRequests: 0,
      successfulRequests: 0,
      failedRequests: 0,
      averageResponseTime: 0,
      authenticationFailures: 0,
      timeoutErrors: 0,
      complianceChecks: 0,
      transactionsProcessed: 0,
      failedTransactions: 0,
      uptime: Date.now()
    };

    // Transaction tracking
    this.activeTransactions = new Map();
    this.completedTransactions = new Map();
    
    logger.info('Base banking connector initialized', {
      bankCode: this.config.bankCode,
      bankName: this.config.bankName,
      connectionId: this.connectionId,
      testMode: this.config.testMode
    });
  }

  // ============================
  // ABSTRACT METHODS - Must be implemented by subclasses
  // ============================

  /**
   * Authenticate with the banking system
   * @returns {Promise<void>}
   */
  async authenticate() {
    throw new Error('authenticate() method must be implemented by subclass');
  }

  /**
   * Test connection to banking system
   * @returns {Promise<boolean>}
   */
  async testConnection() {
    throw new Error('testConnection() method must be implemented by subclass');
  }

  /**
   * Get account details
   * @param {string} accountNumber 
   * @param {Object} options 
   * @returns {Promise<Object>}
   */
  async getAccountDetails(accountNumber, options = {}) {
    throw new Error('getAccountDetails() method must be implemented by subclass');
  }

  /**
   * Check account balance
   * @param {string} accountNumber 
   * @param {string} currency 
   * @returns {Promise<Object>}
   */
  async checkAccountBalance(accountNumber, currency = null) {
    throw new Error('checkAccountBalance() method must be implemented by subclass');
  }

  /**
   * Validate transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async validateTransaction(transaction) {
    throw new Error('validateTransaction() method must be implemented by subclass');
  }

  /**
   * Process debit transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processDebit(transaction) {
    throw new Error('processDebit() method must be implemented by subclass');
  }

  /**
   * Process credit transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processCredit(transaction) {
    throw new Error('processCredit() method must be implemented by subclass');
  }

  /**
   * Get transaction status
   * @param {string} transactionId 
   * @returns {Promise<Object>}
   */
  async getTransactionStatus(transactionId) {
    throw new Error('getTransactionStatus() method must be implemented by subclass');
  }

  // ============================
  // CONCRETE METHODS - Implemented in base class
  // ============================

  /**
   * Initialize the connector
   * @returns {Promise<void>}
   */
  async initialize() {
    try {
      logger.info('Initializing banking connector', {
        bankCode: this.config.bankCode,
        connectionId: this.connectionId
      });

      // Authenticate with banking system
      await this.authenticate();
      
      // Test connection
      const connectionTest = await this.testConnection();
      if (!connectionTest) {
        throw new Error('Connection test failed');
      }

      this.isConnected = true;
      this.lastHeartbeat = Date.now();
      
      // Start health monitoring if enabled
      if (this.config.enableHealthChecks) {
        this.startHealthMonitoring();
      }

      this.emit('connected', {
        bankCode: this.config.bankCode,
        connectionId: this.connectionId,
        timestamp: new Date().toISOString()
      });

      logger.info('Banking connector initialized successfully', {
        bankCode: this.config.bankCode,
        connectionId: this.connectionId
      });

    } catch (error) {
      logger.error('Banking connector initialization failed', {
        bankCode: this.config.bankCode,
        error: error.message
      });
      
      this.emit('error', error);
      throw error;
    }
  }

  /**
   * Process a complete transaction (debit + credit)
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processTransaction(transaction) {
    const transactionId = transaction.id || uuidv4();
    const startTime = Date.now();

    try {
      logger.info('Processing transaction', {
        transactionId,
        bankCode: this.config.bankCode,
        amount: transaction.amount,
        currency: transaction.currency
      });

      // Track active transaction
      this.activeTransactions.set(transactionId, {
        ...transaction,
        startTime,
        status: TRANSACTION_STATUS.PROCESSING
      });

      // Validate transaction
      const validation = await this.validateTransaction(transaction);
      if (!validation.isValid) {
        throw new Error(`Transaction validation failed: ${validation.errorMessage}`);
      }

      // Process debit (outgoing)
      let debitResult = null;
      if (transaction.type === 'debit' || transaction.type === 'transfer') {
        debitResult = await this.processDebit(transaction);
      }

      // Process credit (incoming)
      let creditResult = null;
      if (transaction.type === 'credit' || transaction.type === 'transfer') {
        creditResult = await this.processCredit(transaction);
      }

      const responseTime = Date.now() - startTime;
      const result = {
        transactionId,
        status: TRANSACTION_STATUS.CONFIRMED,
        debitResult,
        creditResult,
        responseTime,
        timestamp: new Date().toISOString(),
        bankCode: this.config.bankCode
      };

      // Update metrics
      this.updateMetrics('success', responseTime);
      this.metrics.transactionsProcessed++;

      // Move to completed transactions
      this.activeTransactions.delete(transactionId);
      this.completedTransactions.set(transactionId, result);

      this.emit('transaction:completed', result);
      
      logger.info('Transaction processed successfully', {
        transactionId,
        responseTime,
        bankCode: this.config.bankCode
      });

      return result;

    } catch (error) {
      const responseTime = Date.now() - startTime;
      
      // Update metrics
      this.updateMetrics('failure', responseTime);

      // Update transaction status
      const errorResult = {
        transactionId,
        status: TRANSACTION_STATUS.FAILED,
        error: error.message,
        errorCode: this.mapErrorCode(error),
        responseTime,
        timestamp: new Date().toISOString(),
        bankCode: this.config.bankCode
      };

      this.activeTransactions.set(transactionId, errorResult);
      
      this.emit('transaction:failed', errorResult);
      
      logger.error('Transaction processing failed', {
        transactionId,
        error: error.message,
        responseTime,
        bankCode: this.config.bankCode
      });

      throw error;
    }
  }

  /**
   * Start health monitoring
   */
  startHealthMonitoring() {
    setInterval(async () => {
      try {
        const isHealthy = await this.testConnection();
        
        if (isHealthy) {
          this.lastHeartbeat = Date.now();
          this.emit('heartbeat', {
            status: 'healthy',
            timestamp: new Date().toISOString(),
            bankCode: this.config.bankCode
          });
        } else {
          this.emit('heartbeat', {
            status: 'unhealthy',
            timestamp: new Date().toISOString(),
            bankCode: this.config.bankCode
          });
        }
      } catch (error) {
        logger.warn('Health check failed', {
          bankCode: this.config.bankCode,
          error: error.message
        });
      }
    }, 60000); // Every minute
  }

  /**
   * Update performance metrics
   * @param {string} type - 'success' or 'failure'
   * @param {number} responseTime - Response time in milliseconds
   */
  updateMetrics(type, responseTime) {
    this.metrics.totalRequests++;
    
    if (type === 'success') {
      this.metrics.successfulRequests++;
    } else {
      this.metrics.failedRequests++;
    }

    // Update average response time
    this.metrics.averageResponseTime = 
      (this.metrics.averageResponseTime * (this.metrics.totalRequests - 1) + responseTime) / 
      this.metrics.totalRequests;
  }

  /**
   * Map generic errors to standard error codes
   * @param {Error} error 
   * @returns {string}
   */
  mapErrorCode(error) {
    const message = error.message.toLowerCase();
    
    if (message.includes('authentication') || message.includes('unauthorized')) {
      return ERROR_CODES.AUTHENTICATION_FAILED;
    }
    if (message.includes('insufficient funds') || message.includes('balance')) {
      return ERROR_CODES.INSUFFICIENT_FUNDS;
    }
    if (message.includes('account') && message.includes('invalid')) {
      return ERROR_CODES.INVALID_ACCOUNT;
    }
    if (message.includes('compliance')) {
      return ERROR_CODES.COMPLIANCE_FAILURE;
    }
    if (message.includes('timeout')) {
      return ERROR_CODES.TIMEOUT_ERROR;
    }
    if (message.includes('rate limit')) {
      return ERROR_CODES.RATE_LIMIT_EXCEEDED;
    }
    
    return ERROR_CODES.SERVICE_UNAVAILABLE;
  }

  /**
   * Get connector status and metrics
   * @returns {Object}
   */
  getStatus() {
    return {
      bankCode: this.config.bankCode,
      bankName: this.config.bankName,
      connectionId: this.connectionId,
      isConnected: this.isConnected,
      lastHeartbeat: this.lastHeartbeat,
      uptime: Date.now() - this.metrics.uptime,
      metrics: {
        ...this.metrics,
        successRate: this.metrics.totalRequests > 0 
          ? this.metrics.successfulRequests / this.metrics.totalRequests 
          : 0
      },
      activeTransactions: this.activeTransactions.size,
      completedTransactions: this.completedTransactions.size
    };
  }

  /**
   * Get health status
   * @returns {Object}
   */
  async getHealthStatus() {
    const connectionTest = await this.testConnection();
    
    return {
      status: connectionTest ? 'healthy' : 'unhealthy',
      bankCode: this.config.bankCode,
      isConnected: this.isConnected,
      lastHeartbeat: this.lastHeartbeat,
      uptime: Date.now() - this.metrics.uptime,
      timestamp: new Date().toISOString()
    };
  }

  /**
   * Cleanup resources
   */
  async cleanup() {
    try {
      this.isConnected = false;
      this.activeTransactions.clear();
      this.completedTransactions.clear();
      
      this.emit('disconnected', {
        bankCode: this.config.bankCode,
        connectionId: this.connectionId,
        timestamp: new Date().toISOString()
      });

      logger.info('Banking connector cleaned up', {
        bankCode: this.config.bankCode,
        connectionId: this.connectionId
      });
    } catch (error) {
      logger.error('Cleanup failed', {
        bankCode: this.config.bankCode,
        error: error.message
      });
    }
  }
}

module.exports = {
  BaseBankingConnector,
  TRANSACTION_STATUS,
  ERROR_CODES
};