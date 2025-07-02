/**
 * TCS BaNCS Core Banking Integration Connector
 * Provides seamless integration with TCS BaNCS core banking platform
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * TCS Partnership Module
 */

const axios = require('axios');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const crypto = require('crypto');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'tcs-bancs-connector' }
});

/**
 * BaNCS API endpoints configuration
 */
const BANCS_ENDPOINTS = {
  ACCOUNT_INQUIRY: '/api/v1/accounts/inquiry',
  BALANCE_CHECK: '/api/v1/accounts/balance',
  TRANSACTION_HISTORY: '/api/v1/accounts/transactions',
  PAYMENT_INITIATION: '/api/v1/payments/initiate',
  PAYMENT_STATUS: '/api/v1/payments/status',
  CUSTOMER_DETAILS: '/api/v1/customers/details',
  COMPLIANCE_CHECK: '/api/v1/compliance/check',
  NOTIFICATION_WEBHOOK: '/api/v1/webhooks/notifications'
};

/**
 * Transaction status mapping between BaNCS and our platform
 */
const STATUS_MAPPING = {
  // BaNCS Status -> Our Platform Status
  'INITIATED': 'pending',
  'PROCESSING': 'processing',
  'COMPLETED': 'confirmed',
  'FAILED': 'failed',
  'REJECTED': 'rejected',
  'PENDING_APPROVAL': 'pending_approval',
  'AUTHORIZED': 'authorized',
  'SETTLED': 'settled'
};

/**
 * TCS BaNCS Connector Class
 * Handles all interactions with TCS BaNCS core banking system
 */
class TCSBaNCSConnector {
  constructor(config = {}) {
    this.config = {
      // BaNCS API configuration
      baseUrl: config.baseUrl || process.env.BANCS_BASE_URL || 'https://bancs-api.tcs.com',
      apiVersion: config.apiVersion || 'v1',
      timeout: config.timeout || 30000,
      
      // Authentication configuration
      clientId: config.clientId || process.env.BANCS_CLIENT_ID,
      clientSecret: config.clientSecret || process.env.BANCS_CLIENT_SECRET,
      authMethod: config.authMethod || 'oauth2', // oauth2, api_key, mutual_tls
      
      // Bank-specific configuration
      bankCode: config.bankCode || process.env.BANK_CODE,
      branchCode: config.branchCode || process.env.BRANCH_CODE,
      institutionId: config.institutionId || process.env.INSTITUTION_ID,
      
      // Security configuration
      encryptionKey: config.encryptionKey || process.env.BANCS_ENCRYPTION_KEY,
      signatureKey: config.signatureKey || process.env.BANCS_SIGNATURE_KEY,
      enableEncryption: config.enableEncryption !== false,
      
      // Integration settings
      testMode: config.testMode || false,
      retryAttempts: config.retryAttempts || 3,
      retryDelay: config.retryDelay || 1000,
      
      // Webhook configuration
      webhookSecret: config.webhookSecret || process.env.BANCS_WEBHOOK_SECRET,
      webhookEndpoint: config.webhookEndpoint || '/webhooks/bancs',
      
      ...config
    };

    // Initialize HTTP client
    this.httpClient = axios.create({
      baseURL: this.config.baseUrl,
      timeout: this.config.timeout,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        'User-Agent': 'LegacyBaaS-TCS-Connector/1.0'
      }
    });

    // Authentication token storage
    this.accessToken = null;
    this.tokenExpiry = null;

    // Request tracking
    this.requestHistory = new Map();
    this.pendingRequests = new Map();

    // Performance metrics
    this.metrics = {
      totalRequests: 0,
      successfulRequests: 0,
      failedRequests: 0,
      averageResponseTime: 0,
      lastResponseTime: 0,
      authenticationFailures: 0,
      timeoutErrors: 0
    };

    // Setup request/response interceptors
    this.setupInterceptors();

    logger.info('TCS BaNCS Connector initialized', {
      baseUrl: this.config.baseUrl,
      bankCode: this.config.bankCode,
      testMode: this.config.testMode,
      authMethod: this.config.authMethod
    });
  }

  /**
   * Setup HTTP client interceptors for logging and error handling
   */
  setupInterceptors() {
    // Request interceptor
    this.httpClient.interceptors.request.use(
      async (config) => {
        const requestId = uuidv4();
        config.metadata = { requestId, startTime: Date.now() };
        
        // Add authentication headers
        await this.ensureAuthenticated();
        if (this.accessToken) {
          config.headers.Authorization = `Bearer ${this.accessToken}`;
        }

        // Add bank-specific headers
        config.headers['X-Bank-Code'] = this.config.bankCode;
        config.headers['X-Institution-ID'] = this.config.institutionId;
        config.headers['X-Request-ID'] = requestId;
        config.headers['X-Timestamp'] = new Date().toISOString();

        // Add signature if enabled
        if (this.config.signatureKey) {
          const signature = this.generateSignature(config);
          config.headers['X-Signature'] = signature;
        }

        // Encrypt sensitive data if enabled
        if (this.config.enableEncryption && config.data) {
          config.data = this.encryptData(config.data);
        }

        logger.debug('BaNCS API request initiated', {
          requestId,
          method: config.method,
          url: config.url,
          headers: { ...config.headers, Authorization: '[REDACTED]' }
        });

        this.metrics.totalRequests++;
        return config;
      },
      (error) => {
        logger.error('Request interceptor error', { error: error.message });
        return Promise.reject(error);
      }
    );

    // Response interceptor
    this.httpClient.interceptors.response.use(
      (response) => {
        const { requestId, startTime } = response.config.metadata;
        const responseTime = Date.now() - startTime;
        
        this.metrics.successfulRequests++;
        this.metrics.lastResponseTime = responseTime;
        this.metrics.averageResponseTime = 
          (this.metrics.averageResponseTime * (this.metrics.totalRequests - 1) + responseTime) / 
          this.metrics.totalRequests;

        // Decrypt response data if needed
        if (this.config.enableEncryption && response.data) {
          response.data = this.decryptData(response.data);
        }

        logger.debug('BaNCS API response received', {
          requestId,
          status: response.status,
          responseTime,
          dataLength: JSON.stringify(response.data).length
        });

        // Store successful request in history
        this.requestHistory.set(requestId, {
          request: {
            method: response.config.method,
            url: response.config.url,
            timestamp: new Date(startTime).toISOString()
          },
          response: {
            status: response.status,
            responseTime,
            timestamp: new Date().toISOString()
          }
        });

        return response;
      },
      (error) => {
        const requestId = error.config?.metadata?.requestId;
        const startTime = error.config?.metadata?.startTime;
        const responseTime = startTime ? Date.now() - startTime : 0;

        this.metrics.failedRequests++;
        this.metrics.lastResponseTime = responseTime;

        // Handle specific error types
        if (error.response?.status === 401) {
          this.metrics.authenticationFailures++;
          this.accessToken = null; // Force re-authentication
          logger.warn('Authentication failure, token invalidated', { requestId });
        } else if (error.code === 'ECONNABORTED') {
          this.metrics.timeoutErrors++;
          logger.warn('Request timeout', { requestId, timeout: this.config.timeout });
        }

        logger.error('BaNCS API error', {
          requestId,
          status: error.response?.status,
          message: error.message,
          responseTime,
          url: error.config?.url
        });

        return Promise.reject(error);
      }
    );
  }

  /**
   * Ensure the connector is authenticated with BaNCS
   */
  async ensureAuthenticated() {
    if (this.accessToken && this.tokenExpiry && Date.now() < this.tokenExpiry) {
      return; // Token is still valid
    }

    try {
      switch (this.config.authMethod) {
        case 'oauth2':
          await this.authenticateOAuth2();
          break;
        case 'api_key':
          this.authenticateApiKey();
          break;
        case 'mutual_tls':
          // TLS authentication is handled at the HTTP client level
          break;
        default:
          throw new Error(`Unsupported authentication method: ${this.config.authMethod}`);
      }
    } catch (error) {
      logger.error('Authentication failed', { 
        method: this.config.authMethod,
        error: error.message 
      });
      throw error;
    }
  }

  /**
   * Authenticate using OAuth2 client credentials flow
   */
  async authenticateOAuth2() {
    try {
      const authPayload = {
        grant_type: 'client_credentials',
        client_id: this.config.clientId,
        client_secret: this.config.clientSecret,
        scope: 'bancs:read bancs:write bancs:accounts bancs:payments'
      };

      const response = await axios.post(`${this.config.baseUrl}/oauth/token`, authPayload, {
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
        timeout: this.config.timeout
      });

      this.accessToken = response.data.access_token;
      this.tokenExpiry = Date.now() + (response.data.expires_in * 1000) - 60000; // 1 minute buffer

      logger.info('OAuth2 authentication successful', {
        expiresIn: response.data.expires_in,
        tokenType: response.data.token_type
      });

    } catch (error) {
      logger.error('OAuth2 authentication failed', { error: error.message });
      throw new Error(`OAuth2 authentication failed: ${error.message}`);
    }
  }

  /**
   * Authenticate using API key
   */
  authenticateApiKey() {
    if (!this.config.clientId) {
      throw new Error('API key not configured');
    }
    
    // API key authentication is handled via headers
    this.accessToken = this.config.clientId;
    this.tokenExpiry = Date.now() + (24 * 60 * 60 * 1000); // 24 hours
    
    logger.info('API key authentication configured');
  }

  /**
   * Generate request signature for security
   */
  generateSignature(config) {
    if (!this.config.signatureKey) return null;

    const timestamp = config.headers['X-Timestamp'];
    const method = config.method.toUpperCase();
    const path = new URL(config.url, this.config.baseUrl).pathname;
    const body = config.data ? JSON.stringify(config.data) : '';
    
    const signatureString = `${method}\n${path}\n${timestamp}\n${body}`;
    
    return crypto
      .createHmac('sha256', this.config.signatureKey)
      .update(signatureString)
      .digest('hex');
  }

  /**
   * Encrypt sensitive data
   */
  encryptData(data) {
    if (!this.config.encryptionKey) return data;

    try {
      const cipher = crypto.createCipher('aes-256-cbc', this.config.encryptionKey);
      let encrypted = cipher.update(JSON.stringify(data), 'utf8', 'hex');
      encrypted += cipher.final('hex');
      return { encrypted: true, data: encrypted };
    } catch (error) {
      logger.warn('Data encryption failed, sending unencrypted', { error: error.message });
      return data;
    }
  }

  /**
   * Decrypt response data
   */
  decryptData(data) {
    if (!data.encrypted || !this.config.encryptionKey) return data;

    try {
      const decipher = crypto.createDecipher('aes-256-cbc', this.config.encryptionKey);
      let decrypted = decipher.update(data.data, 'hex', 'utf8');
      decrypted += decipher.final('utf8');
      return JSON.parse(decrypted);
    } catch (error) {
      logger.error('Data decryption failed', { error: error.message });
      return data;
    }
  }

  /**
   * Get account details from BaNCS
   * @param {string} accountNumber - Account number to query
   * @param {Object} options - Additional query options
   * @returns {Promise<Object>} Account details
   */
  async getAccountDetails(accountNumber, options = {}) {
    try {
      const params = {
        accountNumber,
        includeBalance: options.includeBalance !== false,
        includeCustomer: options.includeCustomer !== false,
        includeProducts: options.includeProducts || false,
        ...options
      };

      const response = await this.httpClient.get(BANCS_ENDPOINTS.ACCOUNT_INQUIRY, { params });
      
      return this.transformAccountResponse(response.data);
    } catch (error) {
      logger.error('Account details retrieval failed', {
        accountNumber,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Check account balance
   * @param {string} accountNumber - Account number
   * @param {string} currency - Currency code (optional)
   * @returns {Promise<Object>} Balance information
   */
  async checkAccountBalance(accountNumber, currency = null) {
    try {
      const params = { accountNumber };
      if (currency) params.currency = currency;

      const response = await this.httpClient.get(BANCS_ENDPOINTS.BALANCE_CHECK, { params });
      
      return this.transformBalanceResponse(response.data);
    } catch (error) {
      logger.error('Balance check failed', {
        accountNumber,
        currency,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Validate transaction before processing
   * @param {Object} transaction - Transaction to validate
   * @returns {Promise<Object>} Validation result
   */
  async validateTransaction(transaction) {
    try {
      // Pre-validation checks
      this.validateTransactionInput(transaction);

      // Check sender account balance
      const balance = await this.checkAccountBalance(
        transaction.sender.account, 
        transaction.currency
      );

      // Verify sufficient funds
      const availableBalance = parseFloat(balance.availableBalance);
      const transactionAmount = parseFloat(transaction.amount);

      if (availableBalance < transactionAmount) {
        return {
          isValid: false,
          errorCode: 'INSUFFICIENT_FUNDS',
          errorMessage: `Insufficient balance. Available: ${availableBalance}, Required: ${transactionAmount}`,
          availableBalance,
          requiredAmount: transactionAmount
        };
      }

      // Check account status
      if (balance.accountStatus !== 'ACTIVE') {
        return {
          isValid: false,
          errorCode: 'ACCOUNT_INACTIVE',
          errorMessage: `Account is not active. Status: ${balance.accountStatus}`,
          accountStatus: balance.accountStatus
        };
      }

      // Compliance checks
      const complianceResult = await this.performComplianceCheck(transaction);
      if (!complianceResult.passed) {
        return {
          isValid: false,
          errorCode: 'COMPLIANCE_FAILURE',
          errorMessage: complianceResult.reason,
          complianceDetails: complianceResult
        };
      }

      return {
        isValid: true,
        validationId: uuidv4(),
        accountBalance: availableBalance,
        complianceResult,
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      logger.error('Transaction validation failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Perform compliance checks
   * @param {Object} transaction - Transaction to check
   * @returns {Promise<Object>} Compliance result
   */
  async performComplianceCheck(transaction) {
    try {
      const compliancePayload = {
        transactionId: transaction.id,
        amount: transaction.amount,
        currency: transaction.currency,
        sender: {
          accountNumber: transaction.sender.account,
          name: transaction.sender.name
        },
        receiver: {
          accountNumber: transaction.receiver.account,
          name: transaction.receiver.name,
          bic: transaction.receiver.bic
        },
        purpose: transaction.remittanceInfo || transaction.purpose,
        transactionType: transaction.messageType || 'MT103'
      };

      const response = await this.httpClient.post(
        BANCS_ENDPOINTS.COMPLIANCE_CHECK, 
        compliancePayload
      );

      return {
        passed: response.data.status === 'APPROVED',
        riskScore: response.data.riskScore,
        reason: response.data.reason,
        requiresManualReview: response.data.requiresManualReview || false,
        sanctions: response.data.sanctionsCheck || {},
        aml: response.data.amlCheck || {},
        fatf: response.data.fatfCheck || {}
      };

    } catch (error) {
      logger.error('Compliance check failed', {
        transactionId: transaction.id,
        error: error.message
      });
      
      // In case of compliance service failure, default to requiring manual review
      return {
        passed: false,
        reason: 'Compliance service unavailable',
        requiresManualReview: true,
        error: error.message
      };
    }
  }

  /**
   * Transform account response to standard format
   */
  transformAccountResponse(data) {
    return {
      accountNumber: data.account_number || data.accountNumber,
      accountName: data.account_name || data.accountName,
      accountType: data.account_type || data.accountType,
      accountStatus: data.account_status || data.accountStatus,
      currency: data.currency || data.base_currency,
      branch: {
        code: data.branch_code || data.branchCode,
        name: data.branch_name || data.branchName
      },
      customer: data.customer ? {
        id: data.customer.customer_id || data.customer.customerId,
        name: data.customer.customer_name || data.customer.customerName,
        type: data.customer.customer_type || data.customer.customerType
      } : null,
      balance: data.balance ? this.transformBalanceResponse(data.balance) : null,
      lastUpdated: data.last_updated || data.lastUpdated || new Date().toISOString()
    };
  }

  /**
   * Transform balance response to standard format
   */
  transformBalanceResponse(data) {
    return {
      accountNumber: data.account_number || data.accountNumber,
      currency: data.currency,
      currentBalance: parseFloat(data.current_balance || data.currentBalance || 0),
      availableBalance: parseFloat(data.available_balance || data.availableBalance || 0),
      holdAmount: parseFloat(data.hold_amount || data.holdAmount || 0),
      overdraftLimit: parseFloat(data.overdraft_limit || data.overdraftLimit || 0),
      accountStatus: data.account_status || data.accountStatus || 'UNKNOWN',
      lastTransactionDate: data.last_transaction_date || data.lastTransactionDate,
      timestamp: data.timestamp || new Date().toISOString()
    };
  }

  /**
   * Validate transaction input
   */
  validateTransactionInput(transaction) {
    const required = ['id', 'amount', 'currency', 'sender', 'receiver'];
    
    for (const field of required) {
      if (!transaction[field]) {
        throw new Error(`Missing required field: ${field}`);
      }
    }

    if (!transaction.sender.account) {
      throw new Error('Sender account number is required');
    }

    if (!transaction.receiver.account) {
      throw new Error('Receiver account number is required');
    }

    if (parseFloat(transaction.amount) <= 0) {
      throw new Error('Transaction amount must be greater than zero');
    }
  }

  /**
   * Get connector metrics
   * @returns {Object} Performance metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate: this.metrics.totalRequests > 0 
        ? this.metrics.successfulRequests / this.metrics.totalRequests 
        : 0,
      isConnected: !!this.accessToken,
      tokenExpiry: this.tokenExpiry,
      requestHistorySize: this.requestHistory.size
    };
  }

  /**
   * Test connection to BaNCS
   * @returns {Promise<boolean>} Connection test result
   */
  async testConnection() {
    try {
      await this.ensureAuthenticated();
      
      // Make a simple health check request
      const response = await this.httpClient.get('/health');
      
      logger.info('BaNCS connection test successful');
      return true;
    } catch (error) {
      logger.error('BaNCS connection test failed', { error: error.message });
      return false;
    }
  }

  /**
   * Cleanup resources
   */
  async cleanup() {
    this.accessToken = null;
    this.tokenExpiry = null;
    this.requestHistory.clear();
    this.pendingRequests.clear();
    
    logger.info('TCS BaNCS Connector cleaned up');
  }
}

module.exports = {
  TCSBaNCSConnector,
  BANCS_ENDPOINTS,
  STATUS_MAPPING
};