/**
 * TCS BaNCS Integration Service
 * Orchestrates integration between blockchain platform and TCS BaNCS
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * TCS Partnership Module
 */

const { TCSBaNCSConnector } = require('./bancs-connector');
const { EventEmitter } = require('events');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'bancs-integration-service' }
});

/**
 * Transaction preprocessing stages
 */
const PREPROCESSING_STAGES = {
  VALIDATION: 'validation',
  COMPLIANCE: 'compliance',
  AUTHORIZATION: 'authorization',
  ENRICHMENT: 'enrichment',
  ROUTING_PREP: 'routing_prep'
};

/**
 * Integration event types
 */
const INTEGRATION_EVENTS = {
  TRANSACTION_VALIDATED: 'transaction_validated',
  TRANSACTION_REJECTED: 'transaction_rejected',
  BALANCE_CHECKED: 'balance_checked',
  COMPLIANCE_PASSED: 'compliance_passed',
  COMPLIANCE_FAILED: 'compliance_failed',
  ACCOUNT_VERIFIED: 'account_verified',
  WEBHOOK_RECEIVED: 'webhook_received'
};

/**
 * TCS BaNCS Integration Service Class
 * Provides high-level integration capabilities
 */
class TCSBaNCSIntegrationService extends EventEmitter {
  constructor(config = {}) {
    super();
    
    this.config = {
      // Service configuration
      enablePreprocessing: config.enablePreprocessing !== false,
      enableRealTimeValidation: config.enableRealTimeValidation !== false,
      enableWebhooks: config.enableWebhooks !== false,
      
      // Caching configuration
      enableCaching: config.enableCaching !== false,
      cacheExpiry: config.cacheExpiry || 300000, // 5 minutes
      
      // Performance settings
      batchSize: config.batchSize || 100,
      maxConcurrentRequests: config.maxConcurrentRequests || 10,
      
      // Retry configuration
      retryAttempts: config.retryAttempts || 3,
      retryDelay: config.retryDelay || 1000,
      
      ...config
    };

    // Initialize BaNCS connector
    this.bancsConnector = new TCSBaNCSConnector(config);

    // Processing queues
    this.validationQueue = [];
    this.processingQueue = [];
    this.webhookQueue = [];

    // Cache for frequently accessed data
    this.accountCache = new Map();
    this.balanceCache = new Map();
    this.customerCache = new Map();

    // Active processing tracking
    this.activeTransactions = new Map();
    this.processingStats = {
      totalProcessed: 0,
      successfulValidations: 0,
      failedValidations: 0,
      averageProcessingTime: 0,
      complianceFailures: 0
    };

    logger.info('TCS BaNCS Integration Service initialized', {
      enablePreprocessing: this.config.enablePreprocessing,
      enableRealTimeValidation: this.config.enableRealTimeValidation,
      enableWebhooks: this.config.enableWebhooks
    });
  }

  /**
   * Preprocess transaction before blockchain routing
   * @param {Object} transaction - Parsed SWIFT transaction
   * @param {Object} context - Additional context
   * @returns {Promise<Object>} Preprocessing result
   */
  async preprocessTransaction(transaction, context = {}) {
    const processingId = uuidv4();
    const startTime = Date.now();

    let result = {
      processingId,
      transactionId: transaction.id,
      status: 'processing',
      stages: {},
      validations: {},
      enrichments: {},
      recommendations: {},
      timestamp: new Date().toISOString()
    };

    try {
      logger.info('Starting transaction preprocessing', {
        processingId,
        transactionId: transaction.id,
        messageType: transaction.messageType,
        amount: transaction.amount,
        currency: transaction.currency
      });

      // Track active processing
      this.activeTransactions.set(processingId, {
        transaction,
        context,
        startTime,
        currentStage: null
      });

      // Stage 1: Input Validation
      result.currentStage = PREPROCESSING_STAGES.VALIDATION;
      result.stages.validation = await this.performInputValidation(transaction, context);

      if (!result.stages.validation.passed) {
        result.status = 'rejected';
        result.rejectionReason = result.stages.validation.errors.join(', ');
        
        this.emit(INTEGRATION_EVENTS.TRANSACTION_REJECTED, result);
        return result;
      }

      // Stage 2: Account Verification & Balance Check
      result.currentStage = 'account_verification';
      result.stages.accountVerification = await this.verifyAccounts(transaction);

      if (!result.stages.accountVerification.passed) {
        result.status = 'rejected';
        result.rejectionReason = result.stages.accountVerification.reason;
        
        this.emit(INTEGRATION_EVENTS.TRANSACTION_REJECTED, result);
        return result;
      }

      // Stage 3: Compliance Check
      result.currentStage = PREPROCESSING_STAGES.COMPLIANCE;
      result.stages.compliance = await this.bancsConnector.performComplianceCheck(transaction);

      if (!result.stages.compliance.passed) {
        result.status = 'compliance_failed';
        result.rejectionReason = result.stages.compliance.reason;
        
        this.processingStats.complianceFailures++;
        this.emit(INTEGRATION_EVENTS.COMPLIANCE_FAILED, result);
        
        // Return immediately for any compliance failure
        return result;
      } else {
        this.emit(INTEGRATION_EVENTS.COMPLIANCE_PASSED, result);
      }

      // Stage 4: Transaction Enrichment
      result.currentStage = PREPROCESSING_STAGES.ENRICHMENT;
      result.stages.enrichment = await this.enrichTransaction(transaction);

      // Stage 5: Routing Preparation
      result.currentStage = PREPROCESSING_STAGES.ROUTING_PREP;
      result.stages.routingPrep = await this.prepareForRouting(transaction, result);

      // Final status
      result.status = 'validated';
      result.processingTime = Date.now() - startTime;
      
      // Update statistics
      this.processingStats.totalProcessed++;
      this.processingStats.successfulValidations++;
      this.processingStats.averageProcessingTime = 
        (this.processingStats.averageProcessingTime * (this.processingStats.totalProcessed - 1) + 
         result.processingTime) / this.processingStats.totalProcessed;

      this.emit(INTEGRATION_EVENTS.TRANSACTION_VALIDATED, result);

      logger.info('Transaction preprocessing completed successfully', {
        processingId,
        transactionId: transaction.id,
        processingTime: result.processingTime,
        status: result.status
      });

      return result;

    } catch (error) {
      this.processingStats.failedValidations++;
      
      logger.error('Transaction preprocessing failed', {
        processingId,
        transactionId: transaction.id,
        error: error.message,
        stage: result?.currentStage || 'unknown'
      });

      return {
        processingId,
        transactionId: transaction.id,
        status: 'error',
        error: error.message,
        processingTime: Date.now() - startTime,
        timestamp: new Date().toISOString()
      };
    } finally {
      this.activeTransactions.delete(processingId);
    }
  }

  /**
   * Perform input validation
   * @param {Object} transaction - Transaction to validate
   * @param {Object} context - Context information
   * @returns {Promise<Object>} Validation result
   */
  async performInputValidation(transaction, context) {
    const errors = [];
    const warnings = [];

    try {
      // Basic field validation
      if (!transaction.id) errors.push('Transaction ID is required');
      if (!transaction.messageType) errors.push('Message type is required');
      if (!transaction.amount || transaction.amount <= 0) errors.push('Valid amount is required');
      if (!transaction.currency) errors.push('Currency is required');

      // Sender validation
      if (!transaction.sender) {
        errors.push('Sender information is required');
      } else {
        if (!transaction.sender.account) errors.push('Sender account is required');
        if (!transaction.sender.name) warnings.push('Sender name not provided');
      }

      // Receiver validation
      if (!transaction.receiver) {
        errors.push('Receiver information is required');
      } else {
        if (!transaction.receiver.account) errors.push('Receiver account is required');
        if (!transaction.receiver.name) warnings.push('Receiver name not provided');
      }

      // Business rule validation
      if (transaction.amount > 10000000) { // 10M limit
        warnings.push('Large transaction amount detected, may require additional approval');
      }

      // Currency validation
      const supportedCurrencies = ['USD', 'EUR', 'GBP', 'JPY', 'CHF', 'CAD', 'AUD'];
      if (!supportedCurrencies.includes(transaction.currency)) {
        warnings.push(`Currency ${transaction.currency} may require special handling`);
      }

      return {
        passed: errors.length === 0,
        errors,
        warnings,
        validatedFields: {
          transactionId: !!transaction.id,
          messageType: !!transaction.messageType,
          amount: !!(transaction.amount && transaction.amount > 0),
          currency: !!transaction.currency,
          sender: !!(transaction.sender && transaction.sender.account),
          receiver: !!(transaction.receiver && transaction.receiver.account)
        }
      };

    } catch (error) {
      logger.error('Input validation error', { error: error.message });
      return {
        passed: false,
        errors: ['Validation process failed', error.message],
        warnings
      };
    }
  }

  /**
   * Verify sender and receiver accounts
   * @param {Object} transaction - Transaction to verify
   * @returns {Promise<Object>} Verification result
   */
  async verifyAccounts(transaction) {
    try {
      const verifications = {
        sender: null,
        receiver: null,
        senderBalance: null
      };

      // Verify sender account
      try {
        const senderAccount = await this.getAccountDetails(transaction.sender.account);
        verifications.sender = {
          exists: true,
          status: senderAccount.accountStatus,
          currency: senderAccount.currency,
          isActive: senderAccount.accountStatus === 'ACTIVE'
        };

        if (!verifications.sender.isActive) {
          return {
            passed: false,
            reason: `Sender account is not active: ${senderAccount.accountStatus}`,
            verifications
          };
        }

        // Check balance
        const balance = await this.bancsConnector.checkAccountBalance(
          transaction.sender.account, 
          transaction.currency
        );
        
        verifications.senderBalance = balance;
        this.emit(INTEGRATION_EVENTS.BALANCE_CHECKED, { 
          account: transaction.sender.account, 
          balance 
        });

        if (balance.availableBalance < transaction.amount) {
          return {
            passed: false,
            reason: `Insufficient funds. Available: ${balance.availableBalance}, Required: ${transaction.amount}`,
            verifications
          };
        }

      } catch (error) {
        logger.warn('Sender account verification failed', { 
          account: transaction.sender.account,
          error: error.message 
        });
        
        // Check if this is a service availability error that should bubble up
        if (error.message.includes('service unavailable') || 
            error.message.includes('network error') || 
            error.message.includes('timeout')) {
          // Re-throw service errors to be caught by outer exception handler
          throw error;
        }
        
        verifications.sender = {
          exists: false,
          error: error.message
        };

        return {
          passed: false,
          reason: `Sender account verification failed: ${error.message}`,
          verifications
        };
      }

      // Verify receiver account (for internal transfers)
      if (this.isInternalAccount(transaction.receiver.account)) {
        try {
          const receiverAccount = await this.getAccountDetails(transaction.receiver.account);
          verifications.receiver = {
            exists: true,
            status: receiverAccount.accountStatus,
            currency: receiverAccount.currency,
            isActive: receiverAccount.accountStatus === 'ACTIVE'
          };

          if (!verifications.receiver.isActive) {
            return {
              passed: false,
              reason: `Receiver account is not active: ${receiverAccount.accountStatus}`,
              verifications
            };
          }

        } catch (error) {
          logger.warn('Receiver account verification failed', { 
            account: transaction.receiver.account,
            error: error.message 
          });
          
          verifications.receiver = {
            exists: false,
            error: error.message
          };
        }
      } else {
        // External account - just mark as external
        verifications.receiver = {
          external: true,
          bic: transaction.receiver.bic || null
        };
      }

      this.emit(INTEGRATION_EVENTS.ACCOUNT_VERIFIED, verifications);

      return {
        passed: true,
        verifications
      };

    } catch (error) {
      logger.error('Account verification error', { error: error.message });
      
      // Check if this is a service availability error that should bubble up
      if (error.message.includes('service unavailable') || 
          error.message.includes('network error') || 
          error.message.includes('timeout')) {
        // Re-throw service errors to be caught by outer exception handler
        throw error;
      }
      
      return {
        passed: false,
        reason: `Account verification failed: ${error.message}`
      };
    }
  }

  /**
   * Enrich transaction with additional data
   * @param {Object} transaction - Transaction to enrich
   * @returns {Promise<Object>} Enrichment result
   */
  async enrichTransaction(transaction) {
    try {
      const enrichments = {
        customer: null,
        products: null,
        riskProfile: null,
        historicalData: null,
        fxRates: null
      };

      // Get customer information
      if (transaction.sender.account) {
        try {
          const accountDetails = await this.getAccountDetails(transaction.sender.account, {
            includeCustomer: true,
            includeProducts: true
          });
          
          enrichments.customer = accountDetails.customer;
          enrichments.products = accountDetails.products;
          
        } catch (error) {
          logger.warn('Customer enrichment failed', { error: error.message });
        }
      }

      // Add FX rates for cross-currency transactions
      if (transaction.currency !== 'USD') {
        try {
          enrichments.fxRates = await this.getFXRates(transaction.currency, 'USD');
        } catch (error) {
          logger.warn('FX rate enrichment failed', { error: error.message });
        }
      }

      // Risk profiling
      enrichments.riskProfile = this.calculateRiskProfile(transaction, enrichments);

      return {
        success: true,
        enrichments,
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      logger.error('Transaction enrichment error', { error: error.message });
      return {
        success: false,
        error: error.message
      };
    }
  }

  /**
   * Prepare transaction for blockchain routing
   * @param {Object} transaction - Original transaction
   * @param {Object} preprocessingResult - Preprocessing result
   * @returns {Promise<Object>} Routing preparation result
   */
  async prepareForRouting(transaction, preprocessingResult) {
    try {
      const routingData = {
        // Enhanced transaction data
        enhancedTransaction: {
          ...transaction,
          preprocessingId: preprocessingResult.processingId,
          validationStatus: preprocessingResult.status,
          bankingContext: {
            senderBank: this.config.bankCode,
            senderBranch: this.config.branchCode,
            accountVerified: preprocessingResult.stages.accountVerification.passed,
            complianceCleared: preprocessingResult.stages.compliance.passed,
            availableBalance: preprocessingResult.stages.accountVerification.verifications.senderBalance?.availableBalance
          }
        },

        // Routing hints
        routingHints: {
          preferredNetworks: this.getPreferredNetworks(transaction, preprocessingResult),
          urgency: this.calculateUrgency(transaction, preprocessingResult),
          costSensitivity: this.calculateCostSensitivity(transaction, preprocessingResult),
          complianceFlags: this.extractComplianceFlags(preprocessingResult.stages.compliance)
        },

        // Risk assessment
        riskAssessment: {
          level: preprocessingResult.stages.enrichment?.enrichments?.riskProfile?.level || 'medium',
          factors: preprocessingResult.stages.enrichment?.enrichments?.riskProfile?.factors || [],
          requiresManualReview: preprocessingResult.stages.compliance.requiresManualReview || false
        }
      };

      return {
        success: true,
        routingData,
        recommendations: this.generateRoutingRecommendations(routingData)
      };

    } catch (error) {
      logger.error('Routing preparation error', { error: error.message });
      return {
        success: false,
        error: error.message
      };
    }
  }

  /**
   * Get account details with caching
   * @param {string} accountNumber - Account number
   * @param {Object} options - Query options
   * @returns {Promise<Object>} Account details
   */
  async getAccountDetails(accountNumber, options = {}) {
    const cacheKey = `account:${accountNumber}:${JSON.stringify(options)}`;
    
    if (this.config.enableCaching && this.accountCache.has(cacheKey)) {
      const cached = this.accountCache.get(cacheKey);
      if (Date.now() - cached.timestamp < this.config.cacheExpiry) {
        return cached.data;
      }
    }

    const accountDetails = await this.bancsConnector.getAccountDetails(accountNumber, options);
    
    if (this.config.enableCaching) {
      this.accountCache.set(cacheKey, {
        data: accountDetails,
        timestamp: Date.now()
      });
    }

    return accountDetails;
  }

  /**
   * Check if account is internal to the bank
   * @param {string} accountNumber - Account number to check
   * @returns {boolean} True if internal account
   */
  isInternalAccount(accountNumber) {
    // Simple check - internal accounts typically start with bank code
    return accountNumber.startsWith(this.config.bankCode) || 
           accountNumber.length <= 12; // Domestic account number format
  }

  /**
   * Get FX rates (placeholder implementation)
   * @param {string} fromCurrency - Source currency
   * @param {string} toCurrency - Target currency
   * @returns {Promise<Object>} FX rate data
   */
  async getFXRates(fromCurrency, toCurrency) {
    // In real implementation, this would call BaNCS FX service
    return {
      from: fromCurrency,
      to: toCurrency,
      rate: 1.0, // Placeholder
      timestamp: new Date().toISOString(),
      source: 'BaNCS_FX_SERVICE'
    };
  }

  /**
   * Calculate risk profile for transaction
   * @param {Object} transaction - Transaction data
   * @param {Object} enrichments - Enrichment data
   * @returns {Object} Risk profile
   */
  calculateRiskProfile(transaction, enrichments) {
    const factors = [];
    let score = 0;

    // Amount-based risk
    if (transaction.amount > 100000) {
      factors.push('high_value');
      score += 30;
    } else if (transaction.amount > 10000) {
      factors.push('medium_value');
      score += 10;
    }

    // Cross-border risk
    if (transaction.receiver.bic && transaction.receiver.bic.substring(4, 6) !== 'US') {
      factors.push('cross_border');
      score += 20;
    }

    // Customer risk (if available)
    if (enrichments.customer?.riskRating === 'HIGH') {
      factors.push('high_risk_customer');
      score += 40;
    }

    // Determine level
    let level = 'low';
    if (score >= 50) level = 'high';
    else if (score >= 20) level = 'medium';

    return { level, score, factors };
  }

  /**
   * Get preferred networks for routing
   * @param {Object} transaction - Transaction data
   * @param {Object} preprocessingResult - Preprocessing result
   * @returns {Array} Preferred network list
   */
  getPreferredNetworks(transaction, preprocessingResult) {
    const preferences = [];

    // High-value transactions prefer Corda for privacy
    if (transaction.amount > 50000) {
      preferences.push('r3-corda');
    }

    // Cross-border payments prefer XRP for speed
    if (transaction.receiver && transaction.receiver.bic) {
      preferences.push('xrp-ledger');
    }

    // Small amounts prefer Ethereum L2 for cost efficiency
    if (transaction.amount < 5000) {
      preferences.push('ethereum-polygon');
    }

    return preferences;
  }

  /**
   * Calculate transaction urgency
   * @param {Object} transaction - Transaction data
   * @param {Object} preprocessingResult - Preprocessing result
   * @returns {string} Urgency level
   */
  calculateUrgency(transaction, preprocessingResult) {
    // Business logic for urgency calculation
    if (transaction.messageType === 'MT103' && transaction.amount > 100000) {
      return 'high';
    }
    
    if (transaction.messageType === 'MT202') {
      return 'institutional'; // Special priority for institutional transfers
    }

    return 'normal';
  }

  /**
   * Calculate cost sensitivity
   * @param {Object} transaction - Transaction data
   * @param {Object} preprocessingResult - Preprocessing result
   * @returns {string} Cost sensitivity level
   */
  calculateCostSensitivity(transaction, preprocessingResult) {
    // Small amounts are cost-sensitive
    if (transaction.amount < 1000) return 'high';
    if (transaction.amount < 10000) return 'medium';
    return 'low';
  }

  /**
   * Extract compliance flags
   * @param {Object} complianceResult - Compliance check result
   * @returns {Object} Compliance flags
   */
  extractComplianceFlags(complianceResult) {
    return {
      amlRequired: complianceResult.aml?.required || false,
      fatfReporting: complianceResult.fatf?.required || false,
      sanctionsCheck: complianceResult.sanctions?.passed || false,
      manualReview: complianceResult.requiresManualReview || false,
      riskScore: complianceResult.riskScore || 0
    };
  }

  /**
   * Generate routing recommendations
   * @param {Object} routingData - Routing data
   * @returns {Array} Routing recommendations
   */
  generateRoutingRecommendations(routingData) {
    const recommendations = [];
    const { enhancedTransaction, routingHints, riskAssessment } = routingData;

    // High-value, low-risk transactions
    if (enhancedTransaction.amount > 50000 && riskAssessment.level === 'low') {
      recommendations.push({
        type: 'network_preference',
        message: 'Consider Corda for high-value transaction privacy',
        priority: 'high'
      });
    }

    // Cost optimization
    if (routingHints.costSensitivity === 'high') {
      recommendations.push({
        type: 'cost_optimization',
        message: 'Use Ethereum L2 for cost-effective processing',
        priority: 'medium'
      });
    }

    // Compliance recommendations
    if (riskAssessment.requiresManualReview) {
      recommendations.push({
        type: 'compliance',
        message: 'Manual compliance review required before processing',
        priority: 'critical'
      });
    }

    return recommendations;
  }

  /**
   * Get integration service metrics
   * @returns {Object} Service metrics
   */
  getMetrics() {
    return {
      processing: this.processingStats,
      bancs: this.bancsConnector.getMetrics(),
      cache: {
        accountCacheSize: this.accountCache.size,
        balanceCacheSize: this.balanceCache.size,
        customerCacheSize: this.customerCache.size
      },
      queues: {
        validationQueueSize: this.validationQueue.length,
        processingQueueSize: this.processingQueue.length,
        webhookQueueSize: this.webhookQueue.length
      },
      active: {
        activeTransactions: this.activeTransactions.size
      }
    };
  }

  /**
   * Cleanup service resources
   */
  async cleanup() {
    await this.bancsConnector.cleanup();
    
    this.accountCache.clear();
    this.balanceCache.clear();
    this.customerCache.clear();
    this.activeTransactions.clear();
    
    this.validationQueue.length = 0;
    this.processingQueue.length = 0;
    this.webhookQueue.length = 0;

    logger.info('TCS BaNCS Integration Service cleaned up');
  }
}

module.exports = {
  TCSBaNCSIntegrationService,
  PREPROCESSING_STAGES,
  INTEGRATION_EVENTS
};