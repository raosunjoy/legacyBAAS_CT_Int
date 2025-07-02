/**
 * Enhanced TCS BaNCS Connector
 * Extends BaseBankingConnector for multi-bank architecture compatibility
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Multi-Bank Architecture Implementation
 */

const axios = require('axios');
const crypto = require('crypto');
const { v4: uuidv4 } = require('uuid');
const { BaseBankingConnector, TRANSACTION_STATUS, ERROR_CODES } = require('../base/base-banking-connector');

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
  NOTIFICATION_WEBHOOK: '/api/v1/webhooks/notifications',
  HEALTH_CHECK: '/api/v1/health',
  OAUTH_TOKEN: '/oauth/token'
};

/**
 * Enhanced TCS BaNCS Connector Class
 * Production-ready connector with multi-bank architecture support
 */
class EnhancedTCSBaNCSConnector extends BaseBankingConnector {
  constructor(config = {}) {
    // Call parent constructor with standardized config
    super({
      bankName: 'TCS BaNCS',
      bankCode: config.bankCode || 'TCS_BANCS',
      ...config
    });

    // BaNCS-specific configuration
    this.bancsConfig = {
      baseUrl: config.baseUrl || process.env.BANCS_BASE_URL || 'https://bancs-api.tcs.com',
      institutionId: config.institutionId || process.env.INSTITUTION_ID,
      branchCode: config.branchCode || process.env.BRANCH_CODE,
      encryptionKey: config.encryptionKey || process.env.BANCS_ENCRYPTION_KEY,
      signatureKey: config.signatureKey || process.env.BANCS_SIGNATURE_KEY,
      webhookSecret: config.webhookSecret || process.env.BANCS_WEBHOOK_SECRET,
      webhookEndpoint: config.webhookEndpoint || '/webhooks/bancs'
    };

    // Initialize HTTP client
    this.httpClient = axios.create({
      baseURL: this.bancsConfig.baseUrl,
      timeout: this.config.timeout,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        'User-Agent': 'LegacyBaaS-Enhanced-TCS-Connector/2.0'
      }
    });

    // Authentication state
    this.accessToken = null;
    this.tokenExpiry = null;

    // Setup HTTP interceptors
    this.setupHttpInterceptors();
  }

  /**
   * Setup HTTP request/response interceptors
   */
  setupHttpInterceptors() {
    // Request interceptor
    this.httpClient.interceptors.request.use(
      async (config) => {
        const requestId = uuidv4();
        config.metadata = { requestId, startTime: Date.now() };

        // Ensure authentication
        await this.ensureAuthenticated();
        if (this.accessToken) {
          config.headers.Authorization = `Bearer ${this.accessToken}`;
        }

        // Add BaNCS-specific headers
        config.headers['X-Bank-Code'] = this.config.bankCode;
        config.headers['X-Institution-ID'] = this.bancsConfig.institutionId;
        config.headers['X-Request-ID'] = requestId;
        config.headers['X-Timestamp'] = new Date().toISOString();

        // Add signature for security
        if (this.bancsConfig.signatureKey) {
          config.headers['X-Signature'] = this.generateSignature(config);
        }

        // Encrypt sensitive data
        if (this.config.enableEncryption && config.data) {
          config.data = this.encryptData(config.data);
        }

        return config;
      },
      (error) => Promise.reject(error)
    );

    // Response interceptor
    this.httpClient.interceptors.response.use(
      (response) => {
        const { requestId, startTime } = response.config.metadata;
        const responseTime = Date.now() - startTime;

        // Update metrics
        this.updateMetrics('success', responseTime);

        // Decrypt response if needed
        if (this.config.enableEncryption && response.data?.encrypted) {
          response.data = this.decryptData(response.data);
        }

        return response;
      },
      (error) => {
        const startTime = error.config?.metadata?.startTime;
        const responseTime = startTime ? Date.now() - startTime : 0;

        this.updateMetrics('failure', responseTime);

        // Handle authentication errors
        if (error.response?.status === 401) {
          this.accessToken = null;
          this.tokenExpiry = null;
        }

        return Promise.reject(error);
      }
    );
  }

  /**
   * Authenticate with TCS BaNCS using OAuth2
   * @returns {Promise<void>}
   */
  async authenticate() {
    try {
      const authPayload = {
        grant_type: 'client_credentials',
        client_id: this.config.clientId,
        client_secret: this.config.clientSecret,
        scope: 'bancs:read bancs:write bancs:accounts bancs:payments'
      };

      const response = await this.httpClient.post(BANCS_ENDPOINTS.OAUTH_TOKEN, authPayload, {
        headers: { 'Content-Type': 'application/x-www-form-urlencoded' }
      });

      this.accessToken = response.data.access_token;
      this.tokenExpiry = Date.now() + (response.data.expires_in * 1000) - 60000; // 1 minute buffer

      this.emit('authenticated', {
        bankCode: this.config.bankCode,
        expiresIn: response.data.expires_in,
        timestamp: new Date().toISOString()
      });

    } catch (error) {
      this.metrics.authenticationFailures++;
      throw new Error(`TCS BaNCS authentication failed: ${error.message}`);
    }
  }

  /**
   * Ensure valid authentication
   */
  async ensureAuthenticated() {
    if (!this.accessToken || !this.tokenExpiry || Date.now() >= this.tokenExpiry) {
      await this.authenticate();
    }
  }

  /**
   * Test connection to TCS BaNCS
   * @returns {Promise<boolean>}
   */
  async testConnection() {
    try {
      await this.ensureAuthenticated();
      const response = await this.httpClient.get(BANCS_ENDPOINTS.HEALTH_CHECK);
      return response.status === 200;
    } catch (error) {
      return false;
    }
  }

  /**
   * Get account details from BaNCS
   * @param {string} accountNumber 
   * @param {Object} options 
   * @returns {Promise<Object>}
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
      throw new Error(`Account details retrieval failed: ${error.message}`);
    }
  }

  /**
   * Check account balance
   * @param {string} accountNumber 
   * @param {string} currency 
   * @returns {Promise<Object>}
   */
  async checkAccountBalance(accountNumber, currency = null) {
    try {
      const params = { accountNumber };
      if (currency) params.currency = currency;

      const response = await this.httpClient.get(BANCS_ENDPOINTS.BALANCE_CHECK, { params });
      return this.transformBalanceResponse(response.data);

    } catch (error) {
      throw new Error(`Balance check failed: ${error.message}`);
    }
  }

  /**
   * Validate transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async validateTransaction(transaction) {
    try {
      // Input validation
      this.validateTransactionInput(transaction);

      // Check balance
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
          errorCode: ERROR_CODES.INSUFFICIENT_FUNDS,
          errorMessage: `Insufficient balance. Available: ${availableBalance}, Required: ${transactionAmount}`,
          availableBalance,
          requiredAmount: transactionAmount
        };
      }

      // Check account status
      if (balance.accountStatus !== 'ACTIVE') {
        return {
          isValid: false,
          errorCode: ERROR_CODES.ACCOUNT_BLOCKED,
          errorMessage: `Account is not active. Status: ${balance.accountStatus}`,
          accountStatus: balance.accountStatus
        };
      }

      // Compliance checks
      const complianceResult = await this.performComplianceCheck(transaction);
      if (!complianceResult.passed) {
        return {
          isValid: false,
          errorCode: ERROR_CODES.COMPLIANCE_FAILURE,
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
      throw new Error(`Transaction validation failed: ${error.message}`);
    }
  }

  /**
   * Process debit transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processDebit(transaction) {
    try {
      const debitPayload = {
        transactionId: transaction.id,
        accountNumber: transaction.sender.account,
        amount: transaction.amount,
        currency: transaction.currency,
        description: transaction.description || transaction.remittanceInfo,
        reference: transaction.reference || transaction.id,
        timestamp: new Date().toISOString()
      };

      const response = await this.httpClient.post('/api/v1/payments/debit', debitPayload);

      return {
        debitId: response.data.debitId || response.data.transactionId,
        status: this.mapBaNCSStatus(response.data.status),
        amount: response.data.amount,
        currency: response.data.currency,
        accountNumber: response.data.accountNumber,
        timestamp: response.data.timestamp || new Date().toISOString(),
        bankReference: response.data.bankReference
      };

    } catch (error) {
      throw new Error(`Debit processing failed: ${error.message}`);
    }
  }

  /**
   * Process credit transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async processCredit(transaction) {
    try {
      const creditPayload = {
        transactionId: transaction.id,
        accountNumber: transaction.receiver.account,
        amount: transaction.amount,
        currency: transaction.currency,
        description: transaction.description || transaction.remittanceInfo,
        reference: transaction.reference || transaction.id,
        senderDetails: {
          name: transaction.sender.name,
          account: transaction.sender.account,
          bank: transaction.sender.bic
        },
        timestamp: new Date().toISOString()
      };

      const response = await this.httpClient.post('/api/v1/payments/credit', creditPayload);

      return {
        creditId: response.data.creditId || response.data.transactionId,
        status: this.mapBaNCSStatus(response.data.status),
        amount: response.data.amount,
        currency: response.data.currency,
        accountNumber: response.data.accountNumber,
        timestamp: response.data.timestamp || new Date().toISOString(),
        bankReference: response.data.bankReference
      };

    } catch (error) {
      throw new Error(`Credit processing failed: ${error.message}`);
    }
  }

  /**
   * Get transaction status
   * @param {string} transactionId 
   * @returns {Promise<Object>}
   */
  async getTransactionStatus(transactionId) {
    try {
      const response = await this.httpClient.get(
        `${BANCS_ENDPOINTS.PAYMENT_STATUS}/${transactionId}`
      );

      return {
        transactionId,
        status: this.mapBaNCSStatus(response.data.status),
        amount: response.data.amount,
        currency: response.data.currency,
        timestamp: response.data.timestamp,
        bankReference: response.data.bankReference,
        details: response.data.details
      };

    } catch (error) {
      throw new Error(`Transaction status retrieval failed: ${error.message}`);
    }
  }

  /**
   * Perform compliance checks
   * @param {Object} transaction 
   * @returns {Promise<Object>}
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

      this.metrics.complianceChecks++;

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
      // Default to requiring manual review if compliance service fails
      return {
        passed: false,
        reason: 'Compliance service unavailable',
        requiresManualReview: true,
        error: error.message
      };
    }
  }

  /**
   * Map BaNCS status to standard status
   * @param {string} bancsStatus 
   * @returns {string}
   */
  mapBaNCSStatus(bancsStatus) {
    const mapping = {
      'INITIATED': TRANSACTION_STATUS.PENDING,
      'PROCESSING': TRANSACTION_STATUS.PROCESSING,
      'COMPLETED': TRANSACTION_STATUS.CONFIRMED,
      'FAILED': TRANSACTION_STATUS.FAILED,
      'REJECTED': TRANSACTION_STATUS.REJECTED,
      'PENDING_APPROVAL': TRANSACTION_STATUS.PENDING_APPROVAL,
      'AUTHORIZED': TRANSACTION_STATUS.AUTHORIZED,
      'SETTLED': TRANSACTION_STATUS.SETTLED
    };

    return mapping[bancsStatus] || bancsStatus;
  }

  /**
   * Generate request signature
   * @param {Object} config 
   * @returns {string}
   */
  generateSignature(config) {
    if (!this.bancsConfig.signatureKey) return null;

    const timestamp = config.headers['X-Timestamp'];
    const method = config.method.toUpperCase();
    const path = new URL(config.url, this.bancsConfig.baseUrl).pathname;
    const body = config.data ? JSON.stringify(config.data) : '';

    const signatureString = `${method}\n${path}\n${timestamp}\n${body}`;

    return crypto
      .createHmac('sha256', this.bancsConfig.signatureKey)
      .update(signatureString)
      .digest('hex');
  }

  /**
   * Encrypt sensitive data
   * @param {Object} data 
   * @returns {Object}
   */
  encryptData(data) {
    if (!this.bancsConfig.encryptionKey) return data;

    try {
      const algorithm = 'aes-256-cbc';
      const key = crypto.scryptSync(this.bancsConfig.encryptionKey, 'salt', 32);
      const iv = crypto.randomBytes(16);

      const cipher = crypto.createCipheriv(algorithm, key, iv);
      let encrypted = cipher.update(JSON.stringify(data), 'utf8', 'hex');
      encrypted += cipher.final('hex');

      return {
        encrypted: true,
        data: encrypted,
        iv: iv.toString('hex')
      };
    } catch (error) {
      return data; // Return unencrypted on error
    }
  }

  /**
   * Decrypt response data
   * @param {Object} data 
   * @returns {Object}
   */
  decryptData(data) {
    if (!data.encrypted || !this.bancsConfig.encryptionKey || !data.iv) return data;

    try {
      const algorithm = 'aes-256-cbc';
      const key = crypto.scryptSync(this.bancsConfig.encryptionKey, 'salt', 32);
      const iv = Buffer.from(data.iv, 'hex');

      const decipher = crypto.createDecipheriv(algorithm, key, iv);
      let decrypted = decipher.update(data.data, 'hex', 'utf8');
      decrypted += decipher.final('utf8');
      return JSON.parse(decrypted);
    } catch (error) {
      return data;
    }
  }

  /**
   * Transform account response to standard format
   * @param {Object} data 
   * @returns {Object}
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
   * @param {Object} data 
   * @returns {Object}
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
   * @param {Object} transaction 
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
}

module.exports = {
  EnhancedTCSBaNCSConnector,
  BANCS_ENDPOINTS
};