/**
 * Fiserv DNA Core Banking Connector
 * Complete integration for Fiserv DNA platform (40% US market share)
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Enterprise-Grade Core Banking Integration
 */

const { BaseBankingConnector, TRANSACTION_STATUS, ERROR_CODES } = require('../base/base-banking-connector');
const axios = require('axios');
const crypto = require('crypto');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const xml2js = require('xml2js');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'fiserv-dna-connector' }
});

/**
 * Fiserv DNA API Endpoints
 */
const DNA_ENDPOINTS = {
  // Core Banking APIs
  AUTHENTICATE: '/auth/oauth/token',
  ACCOUNT_INQUIRY: '/accounts/inquiry',
  ACCOUNT_BALANCE: '/accounts/balance',
  ACCOUNT_HISTORY: '/accounts/history',
  CUSTOMER_INFO: '/customers/details',
  
  // Transaction Processing
  PAYMENT_INITIATION: '/payments/initiate',
  PAYMENT_STATUS: '/payments/status',
  TRANSFER_FUNDS: '/transfers/create',
  WIRE_TRANSFER: '/wires/send',
  
  // Compliance & Screening
  AML_SCREENING: '/compliance/aml/screen',
  KYC_VERIFICATION: '/compliance/kyc/verify',
  SANCTIONS_CHECK: '/compliance/sanctions/check',
  
  // Real-time Services
  REAL_TIME_BALANCE: '/realtime/balance',
  REAL_TIME_HOLDS: '/realtime/holds',
  NOTIFICATION_WEBHOOK: '/webhooks/register',
  
  // Core Banking Functions
  ACCOUNT_OPENING: '/accounts/open',
  ACCOUNT_CLOSURE: '/accounts/close',
  LOAN_ORIGINATION: '/loans/originate',
  DEPOSIT_SERVICES: '/deposits/manage',
  
  // Transaction Management
  TRANSACTIONS: '/transactions',
  
  // Compliance & Risk
  COMPLIANCE_CHECK: '/compliance/check',
  
  // Webhook Management
  WEBHOOKS: '/webhooks',
  
  // Health & Status
  HEALTH: '/health'
};

/**
 * Fiserv DNA Transaction Types
 */
const DNA_TRANSACTION_TYPES = {
  DEBIT: 'debit',
  CREDIT: 'credit',
  TRANSFER: 'transfer',
  WIRE: 'wire',
  ACH: 'ach',
  CHECK: 'check',
  CARD: 'card',
  MOBILE: 'mobile',
  ONLINE: 'online'
};

/**
 * Fiserv DNA Account Types
 */
const DNA_ACCOUNT_TYPES = {
  CHECKING: 'checking',
  SAVINGS: 'savings',
  MONEY_MARKET: 'money_market',
  CD: 'certificate_deposit',
  LOAN: 'loan',
  CREDIT_LINE: 'credit_line',
  MORTGAGE: 'mortgage'
};

/**
 * Fiserv DNA Connector Implementation
 * Provides complete integration with Fiserv DNA core banking platform
 */
class FiservDNAConnector extends BaseBankingConnector {
  constructor(config = {}) {
    super({
      bankCode: 'FISERV_DNA',
      bankName: 'Fiserv DNA Platform',
      apiVersion: 'v2',
      timeout: 30000,
      ...config
    });

    // Fiserv DNA specific configuration
    this.dnaConfig = {
      // API Configuration
      baseUrl: config.baseUrl || process.env.FISERV_DNA_API_URL,
      institutionId: config.institutionId || process.env.FISERV_DNA_INSTITUTION_ID,
      apiKey: config.apiKey || process.env.FISERV_DNA_API_KEY,
      apiSecret: config.apiSecret || process.env.FISERV_DNA_API_SECRET,
      
      // OAuth Configuration
      clientId: config.clientId || process.env.FISERV_DNA_CLIENT_ID,
      clientSecret: config.clientSecret || process.env.FISERV_DNA_CLIENT_SECRET,
      scope: config.scope || 'core_banking payments compliance',
      
      // Environment Settings
      environment: config.environment || process.env.FISERV_DNA_ENVIRONMENT || 'sandbox',
      region: config.region || 'us-east-1',
      
      // Security Settings
      enableMutualTLS: config.enableMutualTLS !== false,
      certificatePath: config.certificatePath,
      privateKeyPath: config.privateKeyPath,
      
      // Integration Options
      enableWebhooks: config.enableWebhooks !== false,
      webhookSecret: config.webhookSecret || process.env.FISERV_DNA_WEBHOOK_SECRET,
      maxRetries: config.maxRetries || 3,
      retryDelay: config.retryDelay || 2000,
      
      // Caching Configuration
      enableCaching: config.enableCaching !== false,
      cacheExpiry: config.cacheExpiry || 300000, // 5 minutes
      
      // Rate Limiting
      rateLimitPerSecond: config.rateLimitPerSecond || 100,
      rateLimitPerMinute: config.rateLimitPerMinute || 1000
    };

    // Authentication tokens
    this.accessToken = null;
    this.refreshToken = null;
    this.tokenExpiry = null;

    // HTTP client with DNA-specific configuration
    this.httpClient = axios.create({
      baseURL: this.dnaConfig.baseUrl,
      timeout: this.config.timeout,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        'User-Agent': 'LegacyBaaS-DNA-Connector/2.0',
        'X-Institution-ID': this.dnaConfig.institutionId,
        'X-API-Version': this.config.apiVersion
      }
    });

    // Account details cache
    this.accountCache = new Map();
    this.customerCache = new Map();
    this.complianceCache = new Map();
    
    // Rate limiting
    this.requestTimes = [];
    
    // Webhook handlers
    this.webhookHandlers = new Map();

    // DNA-specific metrics
    this.dnaMetrics = {
      apiCalls: 0,
      cacheHits: 0,
      cacheMisses: 0,
      webhookEvents: 0,
      complianceChecks: 0,
      realTimeRequests: 0
    };

    logger.info('Fiserv DNA connector initialized', {
      institutionId: this.dnaConfig.institutionId,
      environment: this.dnaConfig.environment,
      baseUrl: this.dnaConfig.baseUrl
    });
  }

  /**
   * Authenticate with Fiserv DNA platform using OAuth2
   * @returns {Promise<void>}
   */
  async authenticate() {
    try {
      logger.info('Authenticating with Fiserv DNA', {
        clientId: this.dnaConfig.clientId,
        environment: this.dnaConfig.environment
      });

      const authData = {
        grant_type: 'client_credentials',
        client_id: this.dnaConfig.clientId,
        client_secret: this.dnaConfig.clientSecret,
        scope: this.dnaConfig.scope
      };

      // Add mutual TLS if enabled
      const requestConfig = {
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        }
      };

      if (this.dnaConfig.enableMutualTLS && this.dnaConfig.certificatePath) {
        requestConfig.httpsAgent = new https.Agent({
          cert: fs.readFileSync(this.dnaConfig.certificatePath),
          key: fs.readFileSync(this.dnaConfig.privateKeyPath),
          rejectUnauthorized: true
        });
      }

      const response = await this.httpClient.post(
        DNA_ENDPOINTS.AUTHENTICATE,
        new URLSearchParams(authData),
        requestConfig
      );

      this.accessToken = response.data.access_token;
      this.refreshToken = response.data.refresh_token;
      this.tokenExpiry = Date.now() + (response.data.expires_in * 1000);

      // Set default authorization header
      this.httpClient.defaults.headers.common['Authorization'] = `Bearer ${this.accessToken}`;

      logger.info('DNA authentication successful', {
        tokenType: response.data.token_type,
        expiresIn: response.data.expires_in
      });

    } catch (error) {
      this.metrics.authenticationFailures++;
      logger.error('DNA authentication failed', {
        error: error.message,
        status: error.response?.status,
        data: error.response?.data
      });
      throw new Error(`DNA authentication failed: ${error.message}`);
    }
  }

  /**
   * Test connection to Fiserv DNA platform
   * @returns {Promise<boolean>}
   */
  async testConnection() {
    try {
      // Check if token is valid
      if (!this.accessToken || Date.now() >= this.tokenExpiry) {
        await this.authenticate();
      }

      // Test with a simple API call
      const response = await this.makeApiCall('GET', '/health');
      return response.status === 200;

    } catch (error) {
      logger.warn('DNA connection test failed', { error: error.message });
      return false;
    }
  }

  /**
   * Get account details from Fiserv DNA
   * @param {string} accountNumber 
   * @param {Object} options 
   * @returns {Promise<Object>}
   */
  async getAccountDetails(accountNumber, options = {}) {
    const cacheKey = `account:${accountNumber}:${JSON.stringify(options)}`;
    
    // Check cache first
    if (this.dnaConfig.enableCaching && this.accountCache.has(cacheKey)) {
      const cached = this.accountCache.get(cacheKey);
      if (Date.now() - cached.timestamp < this.dnaConfig.cacheExpiry) {
        this.dnaMetrics.cacheHits++;
        return cached.data;
      }
    }

    try {
      const response = await this.makeApiCall('GET', DNA_ENDPOINTS.ACCOUNT_INQUIRY, {
        params: {
          accountNumber,
          includeBalances: options.includeBalances || true,
          includeHolds: options.includeHolds || false,
          includeHistory: options.includeHistory || false,
          includeCustomer: options.includeCustomer || false,
          historyDays: options.historyDays || 0
        }
      });

      const accountData = {
        accountNumber: response.data.accountNumber,
        accountType: response.data.accountType,
        accountStatus: response.data.status,
        customerId: response.data.customerId,
        productCode: response.data.productCode,
        openDate: response.data.openDate,
        currency: response.data.currency || 'USD',
        balances: response.data.balances || {},
        holds: response.data.holds || [],
        interestRate: response.data.interestRate,
        fees: response.data.fees || [],
        restrictions: response.data.restrictions || [],
        metadata: response.data.metadata || {}
      };

      // Cache the result
      if (this.dnaConfig.enableCaching) {
        this.accountCache.set(cacheKey, {
          data: accountData,
          timestamp: Date.now()
        });
      }

      this.dnaMetrics.cacheMisses++;
      return accountData;

    } catch (error) {
      logger.error('Failed to get DNA account details', {
        accountNumber,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Check account balance
   * @param {string} accountNumber 
   * @param {string} currency 
   * @returns {Promise<Object>}
   */
  async checkAccountBalance(accountNumber, currency = 'USD') {
    const cacheKey = `balance:${accountNumber}:${currency}`;
    
    // Check cache first
    if (this.dnaConfig.enableCaching && this.accountCache.has(cacheKey)) {
      const cached = this.accountCache.get(cacheKey);
      if (Date.now() - cached.timestamp < this.dnaConfig.cacheExpiry) {
        this.dnaMetrics.cacheHits++;
        return cached.data;
      }
    }

    try {
      const response = await this.makeApiCall('GET', DNA_ENDPOINTS.ACCOUNT_BALANCE, {
        params: { accountNumber, currency }
      });

      const balanceData = {
        accountNumber,
        currency,
        availableBalance: parseFloat(response.data.availableBalance),
        currentBalance: parseFloat(response.data.currentBalance),
        pendingBalance: parseFloat(response.data.pendingBalance),
        holds: response.data.holds || [],
        lastUpdated: response.data.lastUpdated || new Date().toISOString()
      };

      // Cache the result
      if (this.dnaConfig.enableCaching) {
        this.accountCache.set(cacheKey, {
          data: balanceData,
          timestamp: Date.now()
        });
      }

      this.dnaMetrics.cacheMisses++;
      return balanceData;

    } catch (error) {
      logger.error('Failed to check DNA account balance', {
        accountNumber,
        currency,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Validate transaction against DNA business rules
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async validateTransaction(transaction) {
    try {
      // Pre-validation checks
      const validation = {
        isValid: true,
        errors: [],
        warnings: [],
        complianceStatus: 'passed'
      };

      // Account validation
      if (transaction.fromAccount) {
        const account = await this.getAccountDetails(transaction.fromAccount);
        if (account.accountStatus !== 'ACTIVE') {
          validation.isValid = false;
          validation.errors.push('Source account is not active');
        }
      }

      // Balance validation for debit transactions
      if (transaction.type === 'debit' || transaction.type === 'transfer') {
        const balance = await this.checkAccountBalance(transaction.fromAccount, transaction.currency);
        if (balance.availableBalance < transaction.amount) {
          validation.isValid = false;
          validation.errors.push('Insufficient funds');
        }
      }

      // International transaction warnings
      if (transaction.isInternational) {
        validation.warnings.push('International transaction - additional fees may apply');
      }

      // Compliance screening
      if (transaction.amount > 10000) { // CTR threshold
        const complianceResult = await this.performComplianceCheck(transaction);
        if (!complianceResult.passed) {
          validation.isValid = false;
          validation.errors.push('Compliance check failed');
          validation.complianceStatus = 'failed';
        }
      }

      return validation;

    } catch (error) {
      logger.error('DNA transaction validation failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Process debit transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processDebit(transaction) {
    try {
      const debitRequest = {
        transactionId: transaction.id || uuidv4(),
        accountNumber: transaction.fromAccount,
        amount: transaction.amount,
        currency: transaction.currency || 'USD',
        description: transaction.description,
        reference: transaction.reference,
        metadata: {
          blockchain: transaction.blockchainNetwork,
          useCase: transaction.useCase,
          ...transaction.metadata
        }
      };

      const response = await this.makeApiCall('POST', '/transactions/debit', debitRequest);

      return {
        transactionId: response.data.transactionId,
        status: this.mapDNAStatus(response.data.status),
        amount: response.data.amount,
        currency: response.data.currency,
        processedAt: response.data.processedAt,
        balance: response.data.newBalance,
        reference: response.data.reference
      };

    } catch (error) {
      this.metrics.failedTransactions++;
      logger.error('DNA debit processing failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Process credit transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processCredit(transaction) {
    try {
      const creditRequest = {
        transactionId: transaction.id || uuidv4(),
        accountNumber: transaction.toAccount,
        amount: transaction.amount,
        currency: transaction.currency || 'USD',
        description: transaction.description,
        reference: transaction.reference,
        metadata: {
          blockchain: transaction.blockchainNetwork,
          useCase: transaction.useCase,
          ...transaction.metadata
        }
      };

      const response = await this.makeApiCall('POST', '/transactions/credit', creditRequest);

      return {
        transactionId: response.data.transactionId,
        status: this.mapDNAStatus(response.data.status),
        amount: response.data.amount,
        currency: response.data.currency,
        processedAt: response.data.processedAt,
        balance: response.data.newBalance,
        reference: response.data.reference
      };

    } catch (error) {
      logger.error('DNA credit processing failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get transaction status from DNA
   * @param {string} transactionId 
   * @returns {Promise<Object>}
   */
  async getTransactionStatus(transactionId) {
    try {
      const response = await this.makeApiCall('GET', DNA_ENDPOINTS.PAYMENT_STATUS, {
        params: { transactionId }
      });

      return {
        transactionId,
        status: this.mapDNAStatus(response.data.status),
        amount: response.data.amount,
        currency: response.data.currency,
        processedAt: response.data.processedAt,
        updatedAt: response.data.updatedAt,
        reference: response.data.reference,
        metadata: response.data.metadata
      };

    } catch (error) {
      logger.error('Failed to get DNA transaction status', {
        transactionId,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Perform compliance check (AML/KYC/Sanctions)
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async performComplianceCheck(transaction) {
    try {
      // Check if compliance threshold is configured and amount is below it
      const threshold = this.dnaConfig.complianceThreshold !== undefined ? this.dnaConfig.complianceThreshold : 10000;
      if (transaction.amount < threshold) {
        return {
          passed: true,
          riskScore: 0,
          flags: [],
          recommendations: [],
          bypassed: true,
          reason: 'Amount below compliance threshold'
        };
      }

      // Check cache first
      const cacheKey = `compliance:${transaction.id || 'unknown'}:${transaction.amount}`;
      if (this.dnaConfig.enableCaching && this.complianceCache.has(cacheKey)) {
        const cached = this.complianceCache.get(cacheKey);
        if (Date.now() - cached.timestamp < this.dnaConfig.cacheExpiry) {
          this.dnaMetrics.cacheHits = (this.dnaMetrics.cacheHits || 0) + 1;
          return cached.data;
        }
      }

      this.dnaMetrics.complianceChecks++;

      const complianceRequest = {
        transactionId: transaction.id,
        amount: transaction.amount,
        currency: transaction.currency,
        fromAccount: transaction.fromAccount,
        toAccount: transaction.toAccount,
        parties: transaction.parties || [],
        checkTypes: ['AML', 'KYC', 'SANCTIONS', 'PEP']
      };

      const response = await this.makeApiCall('POST', DNA_ENDPOINTS.AML_SCREENING, complianceRequest);

      const result = {
        passed: response.data.status === 'PASSED',
        riskScore: response.data.riskScore,
        flags: response.data.flags || [],
        recommendations: response.data.recommendations || [],
        screeningResults: response.data.screeningResults || {}
      };

      // Cache the result
      if (this.dnaConfig.enableCaching) {
        this.complianceCache.set(cacheKey, {
          data: result,
          timestamp: Date.now()
        });
      }

      this.dnaMetrics.cacheMisses = (this.dnaMetrics.cacheMisses || 0) + 1;
      return result;

    } catch (error) {
      logger.error('DNA compliance check failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Register webhook for real-time notifications
   * @param {string} eventType 
   * @param {string} callbackUrl 
   * @returns {Promise<Object>}
   */
  async registerWebhook(eventType, callbackUrl) {
    try {
      const webhookRequest = {
        eventType,
        callbackUrl,
        secret: this.dnaConfig.webhookSecret,
        active: true
      };

      const response = await this.makeApiCall('POST', DNA_ENDPOINTS.NOTIFICATION_WEBHOOK, webhookRequest);

      this.webhookHandlers.set(eventType, {
        id: response.data.webhookId,
        callbackUrl,
        secret: this.dnaConfig.webhookSecret
      });

      return response.data;

    } catch (error) {
      logger.error('Failed to register DNA webhook', {
        eventType,
        callbackUrl,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Make authenticated API call to DNA
   * @param {string} method 
   * @param {string} endpoint 
   * @param {Object} data 
   * @returns {Promise<Object>}
   */
  async makeApiCall(method, endpoint, data = {}) {
    // Check rate limiting
    await this.checkRateLimit();

    // Refresh token if needed
    if (Date.now() >= this.tokenExpiry - 60000) { // Refresh 1 minute before expiry
      await this.authenticate();
    }

    try {
      this.dnaMetrics.apiCalls++;
      const startTime = Date.now();
      
      const config = {
        method: method.toLowerCase(),
        url: endpoint,
        headers: {
          'X-Request-ID': uuidv4(),
          'X-Timestamp': new Date().toISOString()
        },
        metadata: { startTime }
      };

      if (method.toUpperCase() === 'GET') {
        config.params = data.params;
      } else {
        config.data = data;
      }

      const response = await this.httpClient(config);
      
      this.updateMetrics('success', startTime);
      return response;

    } catch (error) {
      this.updateMetrics('failure');
      
      // Enhanced error handling for DNA-specific errors
      if (error.response && error.response.data) {
        const dnaError = this.mapDNAError(error.response.data);
        throw new Error(dnaError.message);
      }
      
      throw error;
    }
  }

  /**
   * Check rate limiting
   * @returns {Promise<void>}
   */
  async checkRateLimit() {
    const now = Date.now();
    const oneSecondAgo = now - 1000;
    const oneMinuteAgo = now - 60000;

    // Clean old timestamps
    this.requestTimes = this.requestTimes.filter(time => time > oneMinuteAgo);

    // Check per-second limit
    const recentRequests = this.requestTimes.filter(time => time > oneSecondAgo);
    if (recentRequests.length >= this.dnaConfig.rateLimitPerSecond) {
      await new Promise(resolve => setTimeout(resolve, 1000 - (now - recentRequests[0])));
    }

    // Check per-minute limit
    if (this.requestTimes.length >= this.dnaConfig.rateLimitPerMinute) {
      await new Promise(resolve => setTimeout(resolve, 60000 - (now - this.requestTimes[0])));
    }

    this.requestTimes.push(now);
  }

  /**
   * Map DNA status codes to standard status
   * @param {string} dnaStatus 
   * @returns {string}
   */
  mapDNAStatus(dnaStatus) {
    const statusMap = {
      'PENDING': TRANSACTION_STATUS.PENDING,
      'PROCESSING': TRANSACTION_STATUS.PROCESSING,
      'COMPLETED': TRANSACTION_STATUS.CONFIRMED,
      'APPROVED': TRANSACTION_STATUS.AUTHORIZED,
      'SETTLED': TRANSACTION_STATUS.SETTLED,
      'FAILED': TRANSACTION_STATUS.FAILED,
      'REJECTED': TRANSACTION_STATUS.REJECTED,
      'CANCELLED': TRANSACTION_STATUS.CANCELLED
    };

    return statusMap[dnaStatus] || TRANSACTION_STATUS.PENDING;
  }

  /**
   * Map DNA error codes to standard error codes
   * @param {Object} dnaError 
   * @returns {Object}
   */
  mapDNAError(dnaError = {}) {
    const errorMap = {
      'INSUFFICIENT_FUNDS': ERROR_CODES.INSUFFICIENT_FUNDS,
      'INVALID_ACCOUNT': ERROR_CODES.INVALID_ACCOUNT,
      'AUTHENTICATION_ERROR': ERROR_CODES.AUTHENTICATION_FAILED,
      'RATE_LIMIT_EXCEEDED': ERROR_CODES.RATE_LIMIT_EXCEEDED,
      'SERVICE_UNAVAILABLE': ERROR_CODES.SERVICE_UNAVAILABLE
    };

    return {
      code: errorMap[dnaError.code] || ERROR_CODES.SERVICE_UNAVAILABLE,
      message: dnaError.message || 'Unknown DNA error',
      details: dnaError.details || {}
    };
  }

  /**
   * Ensure authentication token is valid
   * @returns {Promise<void>}
   */
  async ensureAuthenticated() {
    if (!this.accessToken || Date.now() >= this.tokenExpiry - 60000) {
      await this.authenticate();
    }
  }


  /**
   * Register webhook
   * @param {string} eventType 
   * @param {string} url 
   * @param {Function} handler 
   * @returns {Promise<Object>}
   */
  async registerWebhook(eventType, url, handler) {
    try {
      const webhookRequest = {
        eventType,
        callbackUrl: url,
        secret: this.dnaConfig.webhookSecret,
        active: true
      };

      const response = await this.makeApiCall('POST', DNA_ENDPOINTS.NOTIFICATION_WEBHOOK, webhookRequest);
      
      const webhookId = response.data.webhookId;
      if (handler) {
        this.webhookHandlers.set(webhookId, handler);
      }

      this.webhookHandlers.set(eventType, {
        id: webhookId,
        callbackUrl: url,
        secret: this.dnaConfig.webhookSecret
      });

      return {
        webhookId,
        eventType,
        callbackUrl: url,
        active: true,
        createdAt: response.data.createdAt
      };

    } catch (error) {
      logger.error('DNA webhook registration failed', {
        eventType,
        url,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Unregister webhook
   * @param {string} webhookId 
   * @returns {Promise<Object>}
   */
  async unregisterWebhook(webhookId) {
    try {
      const response = await this.makeApiCall('DELETE', `${DNA_ENDPOINTS.WEBHOOKS}/${webhookId}`);
      this.webhookHandlers.delete(webhookId);
      return response.data;
    } catch (error) {
      logger.error('DNA webhook unregistration failed', {
        webhookId,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * List active webhooks
   * @returns {Promise<Array>}
   */
  async listWebhooks() {
    try {
      const response = await this.makeApiCall('GET', DNA_ENDPOINTS.WEBHOOKS);
      return response.data.webhooks || [];
    } catch (error) {
      logger.error('DNA webhook listing failed', { error: error.message });
      throw error;
    }
  }

  /**
   * Process webhook event
   * @param {Object} event 
   * @returns {Promise<void>}
   */
  async processWebhookEvent(event) {
    try {
      // Try to find handler by eventType first, then by webhookId
      let handler = this.webhookHandlers.get(event.eventType) || this.webhookHandlers.get(event.webhookId);
      
      if (handler) {
        // If the handler is actually an object (from registerWebhook), get the function
        if (typeof handler === 'object' && typeof handler.handler === 'function') {
          handler = handler.handler;
        }
        
        // Call handler with event data
        await handler(event.data || event);
        
        // Only increment metrics if handler was found and called
        this.dnaMetrics.webhookEvents = (this.dnaMetrics.webhookEvents || 0) + 1;
      } else {
        logger.warn('No handler registered for webhook', { 
          eventType: event.eventType,
          webhookId: event.webhookId 
        });
      }
    } catch (error) {
      logger.error('Webhook event processing failed', {
        eventType: event.eventType,
        webhookId: event.webhookId,
        error: error.message
      });
    }
  }

  /**
   * Generate cache key
   * @param {string} prefix 
   * @param {string} identifier 
   * @returns {string}
   */
  generateCacheKey(prefix, identifier) {
    return `${prefix}:${identifier}`;
  }

  /**
   * Check cache validity
   * @param {Object} cacheEntry 
   * @param {number} maxAge 
   * @returns {boolean}
   */
  isCacheValid(cacheEntry, maxAge = 300000) {
    return cacheEntry && (Date.now() - cacheEntry.timestamp < maxAge);
  }

  /**
   * Clear all caches
   */
  clearAllCaches() {
    this.accountCache.clear();
    this.customerCache.clear();
    this.complianceCache.clear();
  }

  /**
   * Clear expired cache entries
   */
  clearExpiredCaches() {
    const now = Date.now();
    const maxAge = 300000;

    for (const [key, entry] of this.accountCache.entries()) {
      if (now - entry.timestamp > maxAge) {
        this.accountCache.delete(key);
      }
    }

    for (const [key, entry] of this.customerCache.entries()) {
      if (now - entry.timestamp > maxAge) {
        this.customerCache.delete(key);
      }
    }

    for (const [key, entry] of this.complianceCache.entries()) {
      if (now - entry.timestamp > maxAge) {
        this.complianceCache.delete(key);
      }
    }
  }

  /**
   * Clean up old request times for rate limiting
   */
  cleanupRequestTimes() {
    const now = Date.now();
    const oneMinuteAgo = now - 60000;
    this.requestTimes = this.requestTimes.filter(time => time > oneMinuteAgo);
  }

  /**
   * Handle real-time notifications
   * @param {Object} notification 
   */
  handleRealTimeNotification(notification) {
    try {
      this.emit('notification', notification);
      logger.info('Real-time notification received', { type: notification.type });
    } catch (error) {
      logger.error('Real-time notification handling failed', { error: error.message });
    }
  }

  /**
   * Support batch operations
   * @param {Array} operations 
   * @returns {Promise<Array>}
   */
  async processBatchOperations(operations) {
    try {
      const batchRequest = {
        operations: operations.map(op => ({
          type: op.type,
          accountId: op.accountId,
          amount: op.amount,
          reference: op.reference
        }))
      };

      const response = await this.makeApiCall('POST', `${DNA_ENDPOINTS.TRANSACTIONS}/batch`, batchRequest);
      return response.data.results || [];

    } catch (error) {
      logger.error('Batch operations failed', { error: error.message });
      throw error;
    }
  }

  /**
   * Handle mutual TLS configuration
   * @param {Object} tlsConfig 
   */
  configureMutualTLS(tlsConfig) {
    if (this.dnaConfig.enableMutualTLS && tlsConfig) {
      this.httpClient.defaults.httpsAgent = new require('https').Agent({
        cert: tlsConfig.clientCert,
        key: tlsConfig.clientKey,
        ca: tlsConfig.caCert,
        rejectUnauthorized: true
      });
    }
  }

  /**
   * Query transaction history
   * @param {Object} criteria 
   * @returns {Promise<Array>}
   */
  async queryTransactionHistory(criteria) {
    try {
      const queryParams = {
        accountId: criteria.accountId,
        startDate: criteria.startDate,
        endDate: criteria.endDate,
        limit: criteria.limit || 100,
        offset: criteria.offset || 0
      };

      const response = await this.makeApiCall('GET', `${DNA_ENDPOINTS.TRANSACTIONS}/history`, { params: queryParams });
      return response.data.transactions || [];

    } catch (error) {
      logger.error('Transaction history query failed', { error: error.message });
      throw error;
    }
  }

  /**
   * Get health status
   * @returns {Promise<Object>}
   */
  async getHealthStatus() {
    try {
      const response = await this.makeApiCall('GET', DNA_ENDPOINTS.HEALTH);
      return {
        healthy: response.data.status === 'UP',
        services: response.data.services || {},
        timestamp: new Date().toISOString()
      };
    } catch (error) {
      return {
        healthy: false,
        error: error.message,
        timestamp: new Date().toISOString()
      };
    }
  }

  /**
   * Calculate cache hit ratio
   * @returns {Object}
   */
  calculateCacheHitRatio() {
    const totalRequests = this.metrics.successfulRequests + this.metrics.failedRequests;
    const cacheHits = this.dnaMetrics.cacheHits || 0;
    
    return {
      hitRatio: totalRequests > 0 ? cacheHits / totalRequests : 0,
      totalRequests,
      cacheHits
    };
  }

  /**
   * Cleanup resources
   * @returns {Promise<void>}
   */
  async cleanup() {
    try {
      // Clear all caches
      this.clearAllCaches();
      
      // Unregister all webhooks
      const webhookIds = Array.from(this.webhookHandlers.keys());
      await Promise.all(webhookIds.map(id => this.unregisterWebhook(id).catch(() => {})));
      
      // Clear timers and intervals
      this.requestTimes = [];
      
      this.emit('cleanup', { timestamp: new Date().toISOString() });
      
      logger.info('DNA connector cleanup completed');
    } catch (error) {
      logger.error('DNA connector cleanup failed', { error: error.message });
    }
  }

  /**
   * Get enhanced status including DNA-specific metrics
   * @returns {Object}
   */
  getStatus() {
    const baseStatus = super.getStatus();
    
    return {
      ...baseStatus,
      dnaMetrics: this.dnaMetrics,
      tokenStatus: {
        hasToken: !!this.accessToken,
        expiresAt: this.tokenExpiry,
        timeToExpiry: this.tokenExpiry ? this.tokenExpiry - Date.now() : null
      },
      cacheStatus: {
        accountCacheSize: this.accountCache.size,
        customerCacheSize: this.customerCache.size,
        hitRatio: this.calculateCacheHitRatio()
      },
      webhookStatus: {
        registeredWebhooks: Array.from(this.webhookHandlers.keys())
      },
      healthStatus: this.isConnected && this.accessToken ? 'healthy' : 'unhealthy'
    };
  }
}

module.exports = {
  FiservDNAConnector,
  DNA_ENDPOINTS,
  DNA_TRANSACTION_TYPES,
  DNA_ACCOUNT_TYPES
};