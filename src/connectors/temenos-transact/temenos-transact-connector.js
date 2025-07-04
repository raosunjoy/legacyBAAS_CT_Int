/**
 * Temenos Transact Core Banking Connector
 * European and global banking integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * European Market Focus with T24 Legacy Support
 */

const { BaseBankingConnector, TRANSACTION_STATUS, ERROR_CODES } = require('../base/base-banking-connector');
const axios = require('axios');
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
  defaultMeta: { service: 'temenos-transact-connector' }
});

/**
 * Temenos Transact API Endpoints
 */
const TRANSACT_ENDPOINTS = {
  // Authentication
  OAUTH_TOKEN: '/oauth/token',
  SESSION_CREATE: '/session/create',
  SESSION_VALIDATE: '/session/validate',
  
  // Core Banking APIs
  CUSTOMER_SERVICE: '/customers',
  ACCOUNT_SERVICE: '/accounts',
  TRANSACTION_SERVICE: '/transactions',
  PAYMENT_SERVICE: '/payments',
  
  // Product Services
  DEPOSITS_SERVICE: '/deposits',
  LOANS_SERVICE: '/loans',
  CARDS_SERVICE: '/cards',
  
  // Regulatory & Compliance
  AML_SERVICE: '/compliance/aml',
  KYC_SERVICE: '/compliance/kyc',
  FATCA_SERVICE: '/compliance/fatca',
  
  // Treasury & Markets
  FX_SERVICE: '/treasury/fx',
  MONEY_MARKET: '/treasury/mm',
  SECURITIES: '/treasury/securities',
  
  // Integration Services
  ESB_SERVICE: '/integration/esb',
  WORKFLOW_SERVICE: '/workflow',
  REPORTING_SERVICE: '/reports'
};

/**
 * Temenos Transaction Types
 */
const TRANSACT_TRANSACTION_TYPES = {
  // Core Banking
  DEPOSIT: 'AC.DEPOSIT',
  WITHDRAWAL: 'AC.WITHDRAWAL', 
  TRANSFER: 'FT.TRANSFER',
  STANDING_ORDER: 'FT.STANDING.ORDER',
  
  // Payments
  PAYMENT_LOCAL: 'PP.LOCAL',
  PAYMENT_INTERNATIONAL: 'PP.INTERNATIONAL',
  SEPA_CREDIT: 'PP.SEPA.CT',
  SEPA_DEBIT: 'PP.SEPA.DD',
  
  // Cards
  CARD_PAYMENT: 'CARD.PAYMENT',
  CARD_WITHDRAWAL: 'CARD.ATM',
  
  // Loans
  LOAN_DISBURSEMENT: 'AA.LOAN.DISBURSE',
  LOAN_REPAYMENT: 'AA.LOAN.REPAY',
  
  // Treasury
  FX_DEAL: 'FX.DEAL',
  MM_PLACEMENT: 'MM.MONEY.MARKET'
};

/**
 * Temenos Account Categories
 */
const TRANSACT_ACCOUNT_CATEGORIES = {
  CURRENT_ACCOUNT: 'CURRENT',
  SAVINGS_ACCOUNT: 'SAVINGS',
  TERM_DEPOSIT: 'TERM.DEPOSIT',
  LOAN_ACCOUNT: 'LOAN',
  OVERDRAFT: 'OVERDRAFT',
  NOSTRO: 'NOSTRO',
  VOSTRO: 'VOSTRO',
  INTERNAL: 'INTERNAL'
};

/**
 * Temenos Supported Currencies
 */
const TRANSACT_CURRENCIES = {
  EUR: { code: 'EUR', name: 'Euro', symbol: '€', decimals: 2 },
  USD: { code: 'USD', name: 'US Dollar', symbol: '$', decimals: 2 },
  GBP: { code: 'GBP', name: 'British Pound', symbol: '£', decimals: 2 },
  CHF: { code: 'CHF', name: 'Swiss Franc', symbol: 'CHF', decimals: 2 },
  JPY: { code: 'JPY', name: 'Japanese Yen', symbol: '¥', decimals: 0 },
  CAD: { code: 'CAD', name: 'Canadian Dollar', symbol: 'C$', decimals: 2 },
  AUD: { code: 'AUD', name: 'Australian Dollar', symbol: 'A$', decimals: 2 },
  SEK: { code: 'SEK', name: 'Swedish Krona', symbol: 'kr', decimals: 2 },
  NOK: { code: 'NOK', name: 'Norwegian Krone', symbol: 'kr', decimals: 2 },
  DKK: { code: 'DKK', name: 'Danish Krone', symbol: 'kr', decimals: 2 }
};

/**
 * Temenos Transact Connector Implementation
 * Provides integration with Temenos Transact core banking platform
 */
class TemenosTransactConnector extends BaseBankingConnector {
  constructor(config = {}) {
    super({
      bankCode: 'TEMENOS_TRANSACT',
      bankName: 'Temenos Transact Platform',
      apiVersion: 'v1.0',
      timeout: 30000,
      ...config
    });

    // Temenos specific configuration
    this.transactConfig = {
      // API Configuration
      baseUrl: config.baseUrl || process.env.TEMENOS_TRANSACT_API_URL,
      bankId: config.bankId || process.env.TEMENOS_BANK_ID,
      companyId: config.companyId || process.env.TEMENOS_COMPANY_ID,
      
      // Authentication
      clientId: config.clientId || process.env.TEMENOS_CLIENT_ID,
      clientSecret: config.clientSecret || process.env.TEMENOS_CLIENT_SECRET,
      username: config.username || process.env.TEMENOS_USERNAME,
      password: config.password || process.env.TEMENOS_PASSWORD,
      
      // T24 Integration
      t24Server: config.t24Server || process.env.TEMENOS_T24_SERVER,
      jbasePort: config.jbasePort || process.env.TEMENOS_JBASE_PORT || 20002,
      enableT24Bridge: config.enableT24Bridge !== false,
      
      // European Banking Features
      enableSEPA: config.enableSEPA !== false,
      enableSWIFTGPI: config.enableSWIFTGPI !== false,
      enableInstantPayments: config.enableInstantPayments !== false,
      
      // Compliance
      enableFATCA: config.enableFATCA !== false,
      enableCRS: config.enableCRS !== false,
      enableMiFIDII: config.enableMiFIDII !== false,
      
      // Multi-currency
      baseCurrency: config.baseCurrency || 'EUR',
      supportedCurrencies: config.supportedCurrencies || ['EUR', 'USD', 'GBP', 'CHF'],
      
      // Integration Options
      enableESB: config.enableESB !== false,
      enableWorkflow: config.enableWorkflow !== false,
      messageFormat: config.messageFormat || 'JSON' // JSON, XML, ISO20022
    };

    // Authentication tokens
    this.accessToken = null;
    this.sessionToken = null;
    this.tokenExpiry = null;
    this.isAuthenticated = false;

    // HTTP client
    this.httpClient = axios.create({
      baseURL: this.transactConfig.baseUrl,
      timeout: this.config.timeout,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        'User-Agent': 'LegacyBaaS-Transact-Connector/1.0',
        'X-Bank-ID': this.transactConfig.bankId,
        'X-Company-ID': this.transactConfig.companyId
      }
    });

    // Data caches
    this.accountCache = new Map();
    this.customerCache = new Map();
    this.currencyCache = new Map();
    
    // Request tracking
    this.requestTimes = [];
    
    // Temenos-specific metrics
    this.transactMetrics = {
      europeanTransactions: 0,
      sepaTransactions: 0,
      swiftGPITransactions: 0,
      complianceChecks: 0,
      multiCurrencyTransactions: 0,
      t24LegacyOperations: 0
    };

    logger.info('Temenos Transact connector initialized', {
      bankId: this.transactConfig.bankId,
      baseCurrency: this.transactConfig.baseCurrency,
      enableSEPA: this.transactConfig.enableSEPA
    });
  }

  /**
   * Authenticate with Temenos Transact using OAuth2
   * @returns {Promise<void>}
   */
  async authenticate() {
    try {
      logger.info('Authenticating with Temenos Transact', {
        bankId: this.transactConfig.bankId,
        companyId: this.transactConfig.companyId
      });

      // OAuth2 authentication
      const authData = {
        grant_type: 'password',
        client_id: this.transactConfig.clientId,
        client_secret: this.transactConfig.clientSecret,
        username: this.transactConfig.username,
        password: this.transactConfig.password,
        scope: 'ACCOUNTS TRANSACTIONS CUSTOMERS COMPLIANCE'
      };

      const response = await this.httpClient.post(TRANSACT_ENDPOINTS.OAUTH_TOKEN, authData, {
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded'
        }
      });

      this.accessToken = response.data.access_token;
      this.tokenExpiry = Date.now() + (response.data.expires_in * 1000);

      // Set authorization header
      this.httpClient.defaults.headers.common['Authorization'] = `Bearer ${this.accessToken}`;

      // Create session for stateful operations
      const sessionResponse = await this.httpClient.post(TRANSACT_ENDPOINTS.SESSION_CREATE, {
        bankId: this.transactConfig.bankId,
        companyId: this.transactConfig.companyId
      });

      this.sessionToken = sessionResponse.data.sessionToken;
      this.isAuthenticated = true;

      logger.info('Temenos authentication successful', {
        tokenType: response.data.token_type,
        expiresIn: response.data.expires_in
      });

    } catch (error) {
      this.metrics.authenticationFailures++;
      logger.error('Temenos authentication failed', {
        error: error.message,
        status: error.response?.status
      });
      throw new Error(`Transact authentication failed: ${error.message}`);
    }
  }

  /**
   * Test connection to Temenos Transact
   * @returns {Promise<boolean>}
   */
  async testConnection() {
    try {
      if (!this.accessToken || Date.now() >= this.tokenExpiry) {
        await this.authenticate();
      }

      const response = await this.httpClient.get('/health');
      return response.status === 200;

    } catch (error) {
      logger.warn('Temenos connection test failed', { error: error.message });
      return false;
    }
  }

  /**
   * Get account details from Temenos
   * @param {string} accountNumber 
   * @param {Object} options 
   * @returns {Promise<Object>}
   */
  async getAccountDetails(accountNumber, options = {}) {
    const cacheKey = `account:${accountNumber}`;
    
    // Check cache
    if (this.accountCache.has(cacheKey)) {
      const cached = this.accountCache.get(cacheKey);
      if (Date.now() - cached.timestamp < 300000) {
        return cached.data;
      }
    }

    try {
      await this.ensureAuthenticated();

      const response = await this.httpClient.get(`${TRANSACT_ENDPOINTS.ACCOUNT_SERVICE}/${accountNumber}`, {
        headers: {
          'X-Session-Token': this.sessionToken
        },
        params: {
          enrichment: options.enrichment || 'FULL',
          includeBalance: options.includeBalance !== false,
          includeLimits: options.includeLimits || false
        }
      });

      const accountData = response.data;

      const result = {
        accountNumber: accountData.accountNumber || accountData.accountId,
        accountType: accountData.accountType || this.mapTransactAccountType(accountData.category),
        accountStatus: this.mapTransactAccountStatus(accountData.status),
        customerId: accountData.customerId,
        productLine: accountData.productLine,
        currency: accountData.currency,
        openDate: accountData.openDate || accountData.openingDate,
        balance: parseFloat(accountData.balance?.available || accountData.availableBalance || 0),
        currentBalance: parseFloat(accountData.balance?.current || accountData.workingBalance || 0),
        availableBalance: parseFloat(accountData.balance?.available || accountData.availableBalance || 0),
        clearedBalance: parseFloat(accountData.clearedBalance || 0),
        interestRate: parseFloat(accountData.interestRate || 0),
        overdraftLimit: parseFloat(accountData.overdraftLimit || 0),
        branchCode: accountData.branchCode,
        accountTitle: accountData.accountTitle || accountData.accountName,
        jointHolders: accountData.jointHolders || [],
        restrictions: accountData.restrictions || [],
        postingRestrictions: accountData.postingRestrictions || []
      };

      // Cache result
      this.accountCache.set(cacheKey, {
        data: result,
        timestamp: Date.now()
      });

      return result;

    } catch (error) {
      logger.error('Failed to get Temenos account details', {
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
  async checkAccountBalance(accountNumber, currency = null) {
    try {
      await this.ensureAuthenticated();

      const response = await this.httpClient.get(`${TRANSACT_ENDPOINTS.ACCOUNT_SERVICE}/${accountNumber}/balances`, {
        headers: {
          'X-Session-Token': this.sessionToken
        },
        params: currency ? { currency } : {}
      });

      const balanceData = response.data;

      // Handle multi-currency response structure
      const multiCurrencyBalances = balanceData.balances || [];
      const targetBalance = multiCurrencyBalances.find(b => b.currency === currency) || 
                           balanceData.balances?.[0] || 
                           balanceData;

      return {
        accountNumber,
        currency: currency || targetBalance.currency || balanceData.currency,
        balance: parseFloat(targetBalance.available || targetBalance.availableBalance || 0),
        workingBalance: parseFloat(targetBalance.current || targetBalance.workingBalance || 0),
        availableBalance: parseFloat(targetBalance.available || targetBalance.availableBalance || 0),
        clearedBalance: parseFloat(targetBalance.clearedBalance || 0),
        blockedBalance: parseFloat(targetBalance.blockedBalance || 0),
        forwardAvailableBalance: parseFloat(targetBalance.forwardAvailableBalance || 0),
        overdraftLimit: parseFloat(targetBalance.overdraftLimit || 0),
        multiCurrencyBalances: multiCurrencyBalances,
        valueDated: balanceData.valueDated,
        lastMovement: balanceData.lastMovementDate || balanceData.lastUpdated
      };

    } catch (error) {
      logger.error('Failed to check Temenos account balance', {
        accountNumber,
        currency,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Validate transaction using Temenos business rules
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async validateTransaction(transaction) {
    try {
      const validation = {
        isValid: true,
        errors: [],
        warnings: [],
        temenosChecks: [],
        complianceChecks: []
      };

      await this.ensureAuthenticated();

      // Account validation
      if (transaction.fromAccount) {
        const account = await this.getAccountDetails(transaction.fromAccount);
        
        if (account.accountStatus !== 'LIVE') {
          validation.isValid = false;
          validation.errors.push('Source account is not active');
        }

        // Check available balance
        if (transaction.type === 'debit' && account.availableBalance < transaction.amount) {
          validation.isValid = false;
          validation.errors.push('Insufficient available balance');
        }

        // Check posting restrictions
        if (account.postingRestrictions.length > 0) {
          validation.warnings.push('Account has posting restrictions');
        }

        validation.temenosChecks.push('Account status and balance verified');
      }

      // European banking compliance checks
      if (transaction.amount > 10000 || transaction.isInternational) {
        const complianceResult = await this.performEuropeanComplianceCheck(transaction);
        if (!complianceResult.passed) {
          validation.isValid = false;
          validation.errors.push('European compliance check failed');
        }
        validation.temenosChecks.push('European compliance screening completed');
      }

      // SEPA validation for EUR transactions
      if (transaction.currency === 'EUR') {
        const sepaValidation = await this.validateSEPATransaction(transaction);
        if (!sepaValidation.isValid) {
          validation.warnings.push('SEPA validation issues detected');
        }
        validation.temenosChecks.push('SEPA validation completed');
        validation.complianceChecks.push('SEPA_VALIDATION');
      }

      // European compliance check
      if (transaction.amount > 1000 || transaction.currency === 'EUR') {
        validation.complianceChecks.push('EUROPEAN_COMPLIANCE');
      }

      return validation;

    } catch (error) {
      logger.error('Temenos transaction validation failed', {
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
      await this.ensureAuthenticated();

      const debitRequest = {
        debitAccountId: transaction.fromAccount,
        debitAmount: transaction.amount,
        debitCurrency: transaction.currency || this.transactConfig.baseCurrency,
        processingDate: new Date().toISOString().split('T')[0],
        valueDate: transaction.valueDate || new Date().toISOString().split('T')[0],
        narrative: transaction.description || 'Blockchain debit transaction',
        reference: transaction.reference || transaction.id,
        transactionType: TRANSACT_TRANSACTION_TYPES.WITHDRAWAL,
        channelId: 'API',
        userId: 'BLOCKCHAIN_API'
      };

      const response = await this.httpClient.post(TRANSACT_ENDPOINTS.TRANSACTION_SERVICE, debitRequest, {
        headers: {
          'X-Session-Token': this.sessionToken,
          'X-Transaction-Type': 'DEBIT'
        }
      });

      const result = response.data;
      this.transactMetrics.t24LegacyOperations++;
      
      // Track European transactions if EUR currency
      if (result.currency === 'EUR') {
        this.transactMetrics.europeanTransactions++;
      }
      
      // Track SEPA transactions
      if (result.currency === 'EUR' && transaction.isInternational) {
        this.transactMetrics.sepaTransactions++;
      }

      return {
        transactionId: result.transactionId,
        status: this.mapTransactTransactionStatus(result.status),
        amount: parseFloat(result.amount),
        currency: result.currency,
        processedAt: result.processingDate || result.executionTime,
        valueDate: result.valueDate,
        reference: result.customerReference,
        narrative: result.narrative,
        newBalance: parseFloat(result.newBalance || 0),
        fromAccount: result.fromAccount || transaction.fromAccount,
        toAccount: result.toAccount || transaction.toAccount,
        sepaReference: result.sepaReference,
        swiftGPIReference: result.swiftGPIReference,
        complianceStatus: result.complianceStatus
      };

    } catch (error) {
      logger.error('Temenos debit processing failed', {
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
      await this.ensureAuthenticated();

      const creditRequest = {
        creditAccountId: transaction.toAccount,
        creditAmount: transaction.amount,
        creditCurrency: transaction.currency || this.transactConfig.baseCurrency,
        processingDate: new Date().toISOString().split('T')[0],
        valueDate: transaction.valueDate || new Date().toISOString().split('T')[0],
        narrative: transaction.description || 'Blockchain credit transaction',
        reference: transaction.reference || transaction.id,
        transactionType: TRANSACT_TRANSACTION_TYPES.DEPOSIT,
        channelId: 'API',
        userId: 'BLOCKCHAIN_API'
      };

      const response = await this.httpClient.post(TRANSACT_ENDPOINTS.TRANSACTION_SERVICE, creditRequest, {
        headers: {
          'X-Session-Token': this.sessionToken,
          'X-Transaction-Type': 'CREDIT'
        }
      });

      const result = response.data;
      this.transactMetrics.t24LegacyOperations++;
      
      // Track European transactions if EUR currency
      if (result.currency === 'EUR') {
        this.transactMetrics.europeanTransactions++;
      }
      
      // Track multi-currency transactions
      if (result.currency !== this.transactConfig.baseCurrency) {
        this.transactMetrics.multiCurrencyTransactions++;
      }
      
      // Track SWIFT GPI transactions
      if (transaction.enableSWIFTGPI) {
        this.transactMetrics.swiftGPITransactions++;
      }

      return {
        transactionId: result.transactionId,
        status: this.mapTransactTransactionStatus(result.status),
        amount: parseFloat(result.amount),
        currency: result.currency,
        processedAt: result.processingDate || result.executionTime,
        valueDate: result.valueDate,
        reference: result.customerReference,
        narrative: result.narrative,
        newBalance: parseFloat(result.newBalance || 0),
        fromAccount: result.fromAccount || transaction.fromAccount,
        toAccount: result.toAccount || transaction.toAccount,
        swiftGPIReference: result.swiftGPIReference,
        trackingReference: result.trackingReference,
        exchangeRate: result.exchangeRate,
        convertedAmount: result.convertedAmount,
        convertedCurrency: result.convertedCurrency
      };

    } catch (error) {
      logger.error('Temenos credit processing failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get transaction status from Temenos
   * @param {string} transactionId 
   * @returns {Promise<Object>}
   */
  async getTransactionStatus(transactionId) {
    try {
      await this.ensureAuthenticated();

      const response = await this.httpClient.get(`${TRANSACT_ENDPOINTS.TRANSACTION_SERVICE}/${transactionId}`, {
        headers: {
          'X-Session-Token': this.sessionToken
        }
      });

      const statusData = response.data;

      return {
        transactionId,
        status: this.mapTransactTransactionStatus(statusData.status),
        amount: parseFloat(statusData.amount),
        currency: statusData.currency,
        processedAt: statusData.processingDate,
        valueDate: statusData.valueDate,
        narrative: statusData.narrative,
        reference: statusData.customerReference,
        authorizedBy: statusData.authorizedBy,
        authorizedAt: statusData.authorizedDateTime
      };

    } catch (error) {
      logger.error('Failed to get Temenos transaction status', {
        transactionId,
        error: error.message
      });
      throw error;
    }
  }

  // Utility methods

  async ensureAuthenticated() {
    if (!this.accessToken || Date.now() >= this.tokenExpiry - 60000) {
      await this.authenticate();
    }
  }

  async performEuropeanComplianceCheck(transaction) {
    try {
      const complianceRequest = {
        amount: transaction.amount,
        currency: transaction.currency,
        customerReference: transaction.reference,
        checkTypes: ['AML', 'KYC', 'SANCTIONS', 'FATCA', 'CRS']
      };

      const response = await this.httpClient.post(TRANSACT_ENDPOINTS.AML_SERVICE, complianceRequest, {
        headers: {
          'X-Session-Token': this.sessionToken
        }
      });

      this.transactMetrics.complianceChecks++;

      return {
        passed: response.data.overallStatus === 'CLEAR',
        riskScore: response.data.riskScore || 0,
        flags: response.data.alerts || [],
        mifidClassification: response.data.mifidClassification
      };

    } catch (error) {
      logger.warn('European compliance check failed', { error: error.message });
      return { passed: true, riskScore: 0, flags: [] };
    }
  }

  async validateSEPATransaction(transaction) {
    try {
      if (!this.transactConfig.enableSEPA) {
        return { isValid: false, reason: 'SEPA not enabled' };
      }

      // SEPA validation logic
      const sepaValidation = {
        isValid: true,
        warnings: []
      };

      // Check IBAN format
      if (transaction.toAccount && !this.isValidIBAN(transaction.toAccount)) {
        sepaValidation.isValid = false;
        sepaValidation.warnings.push('Invalid IBAN format');
      }

      // Check SEPA amount limits
      if (transaction.amount > 999999999.99) {
        sepaValidation.isValid = false;
        sepaValidation.warnings.push('Amount exceeds SEPA limit');
      }

      this.transactMetrics.sepaTransactions++;
      return sepaValidation;

    } catch (error) {
      logger.warn('SEPA validation failed', { error: error.message });
      return { isValid: true, warnings: [] };
    }
  }

  isValidIBAN(iban) {
    // Simplified IBAN validation
    const ibanRegex = /^[A-Z]{2}[0-9]{2}[A-Z0-9]{4}[0-9]{7}([A-Z0-9]?){0,16}$/;
    return ibanRegex.test(iban?.replace(/\s/g, ''));
  }

  mapTransactAccountType(category) {
    const types = {
      'CURRENT': 'CHECKING',
      'SAVINGS': 'SAVINGS',
      'TERM.DEPOSIT': 'CD',
      'LOAN': 'LOAN',
      'OVERDRAFT': 'CREDIT_LINE'
    };
    return types[category] || 'UNKNOWN';
  }

  mapTransactAccountStatus(status) {
    const statuses = {
      'LIVE': 'ACTIVE',
      'CLOSED': 'CLOSED',
      'DORMANT': 'DORMANT',
      'FROZEN': 'FROZEN'
    };
    return statuses[status] || 'UNKNOWN';
  }

  mapTransactTransactionStatus(status) {
    const statuses = {
      'LIVE': TRANSACTION_STATUS.CONFIRMED,
      'PENDING': TRANSACTION_STATUS.PENDING,
      'REVERSED': TRANSACTION_STATUS.FAILED,
      'AUTHORIZED': TRANSACTION_STATUS.AUTHORIZED,
      'REJECTED': TRANSACTION_STATUS.REJECTED
    };
    return statuses[status] || TRANSACTION_STATUS.PENDING;
  }

  /**
   * Get health status
   * @returns {Promise<Object>}
   */
  async getHealthStatus() {
    try {
      await this.ensureAuthenticated();
      
      const response = await this.httpClient.get('/health', {
        headers: {
          'X-Session-Token': this.sessionToken
        }
      });

      return {
        status: 'healthy',
        details: {
          transactCore: response.data.status === 'healthy' ? 'active' : 'inactive',
          t24Legacy: this.transactConfig.enableT24Bridge ? 'connected' : 'disabled',
          europeanBanking: 'operational',
          sepaService: this.transactConfig.enableSEPA ? 'active' : 'disabled',
          swiftGpi: this.transactConfig.enableSWIFTGPI ? 'active' : 'disabled',
          complianceEngine: 'active'
        },
        version: response.data.version || '2023.12',
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      return {
        status: 'degraded',
        details: {
          transactCore: 'inactive',
          t24Legacy: 'unknown',
          europeanBanking: 'unknown',
          sepaService: 'unknown',
          swiftGpi: 'unknown',
          complianceEngine: 'unknown'
        },
        error: error.message,
        timestamp: new Date().toISOString()
      };
    }
  }

  /**
   * Cleanup resources
   * @returns {Promise<void>}
   */
  async cleanup() {
    try {
      // Logout and invalidate session
      if (this.sessionToken) {
        await this.httpClient.delete('/session', {
          headers: {
            'X-Session-Token': this.sessionToken
          }
        });
      }

      // Clear tokens and authentication state
      this.accessToken = null;
      this.sessionToken = null;
      this.tokenExpiry = null;
      this.isAuthenticated = false;

      // Clear caches
      this.accountCache.clear();
      this.customerCache.clear();
      this.currencyCache.clear();

      // Update connection status
      this.isConnected = false;

      logger.info('Temenos Transact connector cleanup completed');
    } catch (error) {
      logger.warn('Cleanup error (ignored)', { error: error.message });
    }
  }

  /**
   * Get enhanced status with Temenos metrics
   * @returns {Object}
   */
  getStatus() {
    const baseStatus = super.getStatus();
    
    return {
      ...baseStatus,
      transactMetrics: this.transactMetrics,
      authStatus: {
        hasAccessToken: !!this.accessToken,
        hasSessionToken: !!this.sessionToken,
        tokenExpiry: this.tokenExpiry,
        timeToExpiry: this.tokenExpiry ? this.tokenExpiry - Date.now() : null
      },
      europeanBanking: {
        baseCurrency: this.transactConfig.baseCurrency,
        supportedCurrencies: this.transactConfig.supportedCurrencies,
        enableSEPA: this.transactConfig.enableSEPA,
        enableSWIFTGPI: this.transactConfig.enableSWIFTGPI
      },
      compliance: {
        enableFATCA: this.transactConfig.enableFATCA,
        enableCRS: this.transactConfig.enableCRS,
        enableMiFIDII: this.transactConfig.enableMiFIDII
      }
    };
  }
}

module.exports = {
  TemenosTransactConnector,
  TRANSACT_ENDPOINTS,
  TRANSACT_TRANSACTION_TYPES,
  TRANSACT_ACCOUNT_CATEGORIES,
  TRANSACT_CURRENCIES
};