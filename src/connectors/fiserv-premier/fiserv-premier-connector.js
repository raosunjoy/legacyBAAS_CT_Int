/**
 * Fiserv Premier Core Banking Connector
 * Community banking integration (legacy SOAP/REST hybrid)
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Community Banking Focus
 */

const { BaseBankingConnector, TRANSACTION_STATUS, ERROR_CODES } = require('../base/base-banking-connector');
const axios = require('axios');
const soap = require('soap');
const xml2js = require('xml2js');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'fiserv-premier-connector' }
});

/**
 * Fiserv Premier API Endpoints
 */
const PREMIER_ENDPOINTS = {
  // SOAP Services
  AUTHENTICATION_WSDL: '/services/AuthenticationService?wsdl',
  ACCOUNT_SERVICE_WSDL: '/services/AccountService?wsdl',
  TRANSACTION_SERVICE_WSDL: '/services/TransactionService?wsdl',
  
  // REST APIs (newer integration)
  REST_AUTH: '/api/v1/auth/login',
  REST_ACCOUNTS: '/api/v1/accounts',
  REST_TRANSACTIONS: '/api/v1/transactions',
  REST_CUSTOMERS: '/api/v1/customers',
  
  // Flat File Processing
  FILE_UPLOAD: '/api/v1/files/upload',
  FILE_STATUS: '/api/v1/files/status',
  FILE_DOWNLOAD: '/api/v1/files/download',
  
  // Real-time Services
  BALANCE_INQUIRY: '/api/v1/realtime/balance',
  FUNDS_VERIFICATION: '/api/v1/realtime/verify',
  
  // Compliance
  BSA_SCREENING: '/api/v1/compliance/bsa',
  CIP_VERIFICATION: '/api/v1/compliance/cip'
};

/**
 * Fiserv Premier Transaction Types
 */
const PREMIER_TRANSACTION_TYPES = {
  CHECKING_DEPOSIT: '100',
  CHECKING_WITHDRAWAL: '101',
  SAVINGS_DEPOSIT: '200',
  SAVINGS_WITHDRAWAL: '201',
  TRANSFER: '300',
  LOAN_PAYMENT: '400',
  FEE_CHARGE: '500',
  INTEREST_CREDIT: '600',
  ACH_DEBIT: '700',
  ACH_CREDIT: '701',
  WIRE_OUTGOING: '800',
  WIRE_INCOMING: '801'
};

/**
 * Fiserv Premier Account Types
 */
const PREMIER_ACCOUNT_TYPES = {
  CHECKING: '01',
  SAVINGS: '02',
  MONEY_MARKET: '03',
  CD: '04',
  IRA: '05',
  LOAN: '10',
  CREDIT_CARD: '11',
  LINE_OF_CREDIT: '12',
  MORTGAGE: '13',
  BUSINESS_CHECKING: '21',
  BUSINESS_SAVINGS: '22',
  BUSINESS_MONEY_MARKET: '23',
  ESCROW: '30',
  TRUST: '31'
};

/**
 * Fiserv Premier Flat File Layout
 */
const PREMIER_FILE_LAYOUT = {
  TRANSACTION_RECORD: {
    recordType: { start: 0, length: 2 },
    transactionDate: { start: 2, length: 8 },
    accountNumber: { start: 10, length: 16 },
    transactionCode: { start: 26, length: 3 },
    amount: { start: 29, length: 12 },
    description: { start: 41, length: 30 },
    reference: { start: 71, length: 15 },
    branchCode: { start: 86, length: 4 },
    userCode: { start: 90, length: 6 },
    checkNumber: { start: 96, length: 10 },
    status: { start: 106, length: 1 }
  },
  
  ACCOUNT_RECORD: {
    recordType: { start: 0, length: 2 },
    accountNumber: { start: 2, length: 16 },
    accountType: { start: 18, length: 2 },
    customerId: { start: 20, length: 12 },
    productCode: { start: 32, length: 6 },
    openDate: { start: 38, length: 8 },
    status: { start: 46, length: 1 },
    balance: { start: 47, length: 12 },
    availableBalance: { start: 59, length: 12 },
    rate: { start: 71, length: 8 },
    maturityDate: { start: 79, length: 8 }
  }
};

/**
 * Fiserv Premier Connector Implementation
 * Supports both legacy SOAP and modern REST APIs
 */
class FiservPremierConnector extends BaseBankingConnector {
  constructor(config = {}) {
    super({
      bankCode: 'FISERV_PREMIER',
      bankName: 'Fiserv Premier Platform',
      apiVersion: 'v1',
      timeout: 30000,
      ...config
    });

    // Fiserv Premier specific configuration
    this.premierConfig = {
      // API Configuration
      baseUrl: config.baseUrl || process.env.FISERV_PREMIER_API_URL,
      soapUrl: config.soapUrl || process.env.FISERV_PREMIER_SOAP_URL,
      institutionId: config.institutionId || process.env.FISERV_PREMIER_INSTITUTION_ID,
      
      // Authentication
      username: config.username || process.env.FISERV_PREMIER_USERNAME,
      password: config.password || process.env.FISERV_PREMIER_PASSWORD,
      applicationId: config.applicationId || process.env.FISERV_PREMIER_APP_ID,
      
      // Integration Options
      preferRESTOverSOAP: config.preferRESTOverSOAP !== false,
      enableFileProcessing: config.enableFileProcessing !== false,
      enableRealTimeAPIs: config.enableRealTimeAPIs !== false,
      
      // File Processing
      ftpHost: config.ftpHost || process.env.FISERV_PREMIER_FTP_HOST,
      ftpUsername: config.ftpUsername || process.env.FISERV_PREMIER_FTP_USER,
      ftpPassword: config.ftpPassword || process.env.FISERV_PREMIER_FTP_PASS,
      
      // Community Banking Features
      enableBranchIntegration: config.enableBranchIntegration !== false,
      enableTellerIntegration: config.enableTellerIntegration !== false,
      branchId: config.branchId || 'BR001',
      
      // Security
      enableSSL: config.enableSSL !== false,
      certificatePath: config.certificatePath
    };

    // Session management
    this.authToken = null;
    this.tokenExpiry = null;
    this.soapToken = null;
    this.soapTokenExpiry = null;
    this.restToken = null;
    this.restTokenExpiry = null;
    this.soapClients = new Map();

    // HTTP client for REST APIs
    this.httpClient = axios.create({
      baseURL: this.premierConfig.baseUrl,
      timeout: this.config.timeout,
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json',
        'User-Agent': 'LegacyBaaS-Premier-Connector/1.0',
        'X-Institution-ID': this.premierConfig.institutionId,
        'X-Application-ID': this.premierConfig.applicationId
      }
    });

    // Data caches
    this.accountCache = new Map();
    this.customerCache = new Map();
    
    // Premier-specific metrics
    this.premierMetrics = {
      soapCalls: 0,
      restCalls: 0,
      fileProcessingEvents: 0,
      flatFileRecords: 0,
      branchTransactions: 0,
      realTimeQueries: 0
    };

    logger.info('Fiserv Premier connector initialized', {
      institutionId: this.premierConfig.institutionId,
      preferRESTOverSOAP: this.premierConfig.preferRESTOverSOAP,
      enableFileProcessing: this.premierConfig.enableFileProcessing
    });
  }

  /**
   * Authenticate with Fiserv Premier
   * Supports both REST and SOAP authentication
   * @returns {Promise<void>}
   */
  async authenticate() {
    try {
      logger.info('Authenticating with Fiserv Premier', {
        institutionId: this.premierConfig.institutionId,
        method: this.premierConfig.preferRESTOverSOAP ? 'REST' : 'SOAP'
      });

      if (this.premierConfig.preferRESTOverSOAP) {
        await this.authenticateREST();
      } else {
        await this.authenticateSOAP();
      }

    } catch (error) {
      this.metrics.authenticationFailures++;
      logger.error('Premier authentication failed', {
        error: error.message
      });
      throw new Error(`Premier authentication failed: ${error.message}`);
    }
  }

  /**
   * Hybrid authentication (both SOAP and REST)
   * Used for comprehensive integration testing
   * @returns {Promise<void>}
   */
  async authenticateHybrid() {
    try {
      logger.info('Performing hybrid authentication with Fiserv Premier');

      // Authenticate with both SOAP and REST
      await this.authenticateSOAP();
      await this.authenticateREST();

      logger.info('Hybrid authentication successful');
    } catch (error) {
      this.metrics.authenticationFailures++;
      logger.error('Hybrid authentication failed', {
        error: error.message
      });
      throw new Error(`Hybrid authentication failed: ${error.message}`);
    }
  }

  /**
   * REST authentication
   */
  async authenticateREST() {
    const authData = {
      institutionId: this.premierConfig.institutionId,
      username: this.premierConfig.username,
      password: this.premierConfig.password,
      applicationId: this.premierConfig.applicationId
    };

    const response = await this.httpClient.post(PREMIER_ENDPOINTS.REST_AUTH, authData);

    this.restToken = response.data.accessToken;
    this.restTokenExpiry = Date.now() + (response.data.expiresIn * 1000);
    
    // Also set authToken for compatibility
    this.authToken = response.data.accessToken;
    this.tokenExpiry = this.restTokenExpiry;

    // Set authorization header
    this.httpClient.defaults.headers.common['Authorization'] = `Bearer ${this.authToken}`;

    logger.info('Premier REST authentication successful');
  }

  /**
   * SOAP authentication
   */
  async authenticateSOAP() {
    try {
      const wsdlUrl = this.premierConfig.soapUrl + PREMIER_ENDPOINTS.AUTHENTICATION_WSDL;
      const soapClient = await soap.createClientAsync(wsdlUrl);

      const authRequest = {
        institutionId: this.premierConfig.institutionId,
        username: this.premierConfig.username,
        password: this.premierConfig.password,
        applicationId: this.premierConfig.applicationId
      };

      const [result] = await soapClient.AuthenticateAsync(authRequest);

      if (!result || !result.sessionToken) {
        throw new Error('Premier SOAP authentication failed: Invalid response');
      }

      this.soapToken = result.sessionToken;
      this.soapTokenExpiry = Date.now() + (result.expiresIn * 1000);
      
      // Also set authToken for compatibility
      this.authToken = result.sessionToken;
      this.tokenExpiry = this.soapTokenExpiry;
      
      // Increment SOAP metrics
      this.premierMetrics.soapCalls++;

      // Store SOAP client for reuse
      this.soapClients.set('auth', soapClient);

      logger.info('Premier SOAP authentication successful');
    } catch (error) {
      this.metrics.authenticationFailures++;
      logger.error('Premier SOAP authentication failed', { error: error.message });
      throw new Error(`Premier SOAP authentication failed: ${error.message}`);
    }
  }

  /**
   * Test connection to Fiserv Premier
   * @returns {Promise<boolean>}
   */
  async testConnection() {
    try {
      if (!this.authToken || Date.now() >= this.tokenExpiry) {
        await this.authenticate();
      }

      if (this.premierConfig.preferRESTOverSOAP) {
        const response = await this.httpClient.get('/health');
        return response.status === 200;
      } else {
        // SOAP health check would go here
        return true;
      }

    } catch (error) {
      logger.warn('Premier connection test failed', { error: error.message });
      return false;
    }
  }

  /**
   * Get account details from Premier
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

      let accountData;
      
      if (this.premierConfig.preferRESTOverSOAP) {
        const response = await this.httpClient.get(`${PREMIER_ENDPOINTS.REST_ACCOUNTS}/${accountNumber}`);
        accountData = response.data;
        this.premierMetrics.restCalls++;
      } else {
        accountData = await this.getAccountDetailsSOAP(accountNumber);
        this.premierMetrics.soapCalls++;
      }

      const result = {
        accountNumber: accountData.accountNumber,
        accountType: this.mapPremierAccountType(accountData.accountType),
        accountStatus: this.mapPremierAccountStatus(accountData.status),
        customerId: accountData.customerId,
        productCode: accountData.productCode,
        openDate: accountData.openDate,
        currentBalance: parseFloat(accountData.currentBalance || 0),
        availableBalance: parseFloat(accountData.availableBalance || 0),
        interestRate: parseFloat(accountData.interestRate || 0),
        maturityDate: accountData.maturityDate,
        branchCode: accountData.branchCode,
        currency: 'USD'
      };

      // Cache result
      this.accountCache.set(cacheKey, {
        data: result,
        timestamp: Date.now()
      });

      return result;

    } catch (error) {
      logger.error('Failed to get Premier account details', {
        accountNumber,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get account details via SOAP
   * @param {string} accountNumber 
   * @returns {Promise<Object>}
   */
  async getAccountDetailsSOAP(accountNumber) {
    const wsdlUrl = this.premierConfig.soapUrl + PREMIER_ENDPOINTS.ACCOUNT_SERVICE_WSDL;
    
    let soapClient = this.soapClients.get('account');
    if (!soapClient) {
      soapClient = await soap.createClientAsync(wsdlUrl);
      this.soapClients.set('account', soapClient);
    }

    const request = {
      sessionToken: this.authToken,
      accountNumber: accountNumber,
      includeBalance: true,
      includeDetails: true
    };

    const [result] = await soapClient.GetAccountDetailsAsync(request);
    return result.accountDetails;
  }

  /**
   * Check account balance
   * @param {string} accountNumber 
   * @param {string} currency 
   * @returns {Promise<Object>}
   */
  async checkAccountBalance(accountNumber, currency = 'USD') {
    try {
      await this.ensureAuthenticated();

      let balanceData;

      if (this.premierConfig.enableRealTimeAPIs) {
        const response = await this.httpClient.get(PREMIER_ENDPOINTS.BALANCE_INQUIRY, {
          params: { accountNumber }
        });
        balanceData = response.data;
        this.premierMetrics.realTimeQueries++;
      } else {
        // Fall back to account details
        const account = await this.getAccountDetails(accountNumber);
        balanceData = {
          currentBalance: account.currentBalance,
          availableBalance: account.availableBalance,
          pendingAmount: 0,
          holdAmount: 0
        };
      }

      return {
        accountNumber,
        currency,
        currentBalance: parseFloat(balanceData.currentBalance || 0),
        availableBalance: parseFloat(balanceData.availableBalance || 0),
        pendingAmount: parseFloat(balanceData.pendingAmount || 0),
        holdAmount: parseFloat(balanceData.holdAmount || 0),
        lastUpdated: new Date().toISOString()
      };

    } catch (error) {
      logger.error('Failed to check Premier account balance', {
        accountNumber,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Validate transaction
   * @param {Object} transaction 
   * @returns {Promise<Object>}
   */
  async validateTransaction(transaction) {
    try {
      const validation = {
        isValid: true,
        errors: [],
        warnings: [],
        premierChecks: []
      };

      await this.ensureAuthenticated();

      // Basic account validation
      if (transaction.fromAccount) {
        const account = await this.getAccountDetails(transaction.fromAccount);
        
        if (account.accountStatus !== 'ACTIVE') {
          validation.isValid = false;
          validation.errors.push('Source account is not active');
        }

        if (transaction.type === 'debit' && account.availableBalance < transaction.amount) {
          validation.isValid = false;
          validation.errors.push('Insufficient available funds');
        }

        validation.premierChecks.push('Account validation completed');
      }

      // Community bank specific checks
      if (transaction.amount > 10000) {
        const bsaCheck = await this.performBSAScreening(transaction);
        if (!bsaCheck.passed) {
          validation.warnings.push('BSA reporting required');
        }
        validation.premierChecks.push('BSA screening completed');
      }

      return validation;

    } catch (error) {
      logger.error('Premier transaction validation failed', {
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
        sessionToken: this.authToken,
        accountNumber: transaction.fromAccount,
        amount: transaction.amount,
        transactionCode: PREMIER_TRANSACTION_TYPES.CHECKING_WITHDRAWAL,
        description: transaction.description || 'Blockchain debit',
        reference: transaction.reference || transaction.id,
        userCode: 'API_USER',
        branchCode: '0001'
      };

      let result;

      if (this.premierConfig.preferRESTOverSOAP) {
        const response = await this.httpClient.post(PREMIER_ENDPOINTS.REST_TRANSACTIONS, debitRequest);
        result = response.data;
        this.premierMetrics.restCalls++;
      } else {
        result = await this.processTransactionSOAP('ProcessDebit', debitRequest);
        this.premierMetrics.soapCalls++;
      }

      return {
        transactionId: result.transactionId,
        status: this.mapPremierTransactionStatus(result.status),
        amount: parseFloat(result.amount),
        processedAt: result.processedAt || new Date().toISOString(),
        reference: result.reference,
        confirmationNumber: result.confirmationNumber
      };

    } catch (error) {
      logger.error('Premier debit processing failed', {
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
        sessionToken: this.authToken,
        accountNumber: transaction.toAccount,
        amount: transaction.amount,
        transactionCode: PREMIER_TRANSACTION_TYPES.CHECKING_DEPOSIT,
        description: transaction.description || 'Blockchain credit',
        reference: transaction.reference || transaction.id,
        userCode: 'API_USER',
        branchCode: '0001'
      };

      let result;

      if (this.premierConfig.preferRESTOverSOAP) {
        const response = await this.httpClient.post(PREMIER_ENDPOINTS.REST_TRANSACTIONS, creditRequest);
        result = response.data;
        this.premierMetrics.restCalls++;
      } else {
        result = await this.processTransactionSOAP('ProcessCredit', creditRequest);
        this.premierMetrics.soapCalls++;
      }

      return {
        transactionId: result.transactionId,
        status: this.mapPremierTransactionStatus(result.status),
        amount: parseFloat(result.amount),
        processedAt: result.processedAt || new Date().toISOString(),
        reference: result.reference,
        confirmationNumber: result.confirmationNumber
      };

    } catch (error) {
      logger.error('Premier credit processing failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get transaction status
   * @param {string} transactionId 
   * @returns {Promise<Object>}
   */
  async getTransactionStatus(transactionId) {
    try {
      await this.ensureAuthenticated();

      let statusData;

      if (this.premierConfig.preferRESTOverSOAP) {
        const response = await this.httpClient.get(`${PREMIER_ENDPOINTS.REST_TRANSACTIONS}/${transactionId}`);
        statusData = response.data;
      } else {
        statusData = await this.getTransactionStatusSOAP(transactionId);
      }

      return {
        transactionId,
        status: this.mapPremierTransactionStatus(statusData.status),
        amount: parseFloat(statusData.amount),
        processedAt: statusData.processedAt,
        description: statusData.description,
        reference: statusData.reference,
        confirmationNumber: statusData.confirmationNumber
      };

    } catch (error) {
      logger.error('Failed to get Premier transaction status', {
        transactionId,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Process flat file (community bank batch processing)
   * @param {string} fileData 
   * @returns {Promise<Object>}
   */
  async processFlatFile(fileData) {
    try {
      const fileId = uuidv4();
      logger.info('Processing Premier flat file', { fileId });

      const records = fileData.split('\n').filter(line => line.trim());
      const results = [];

      for (const record of records) {
        try {
          const recordType = record.substring(0, 2);
          let parsed;

          if (recordType === '10') {
            // Transaction record
            parsed = this.parseFlatFileRecord(record, PREMIER_FILE_LAYOUT.TRANSACTION_RECORD);
            parsed.recordType = 'TRANSACTION';
          } else if (recordType === '20') {
            // Account record
            parsed = this.parseFlatFileRecord(record, PREMIER_FILE_LAYOUT.ACCOUNT_RECORD);
            parsed.recordType = 'ACCOUNT';
          }

          results.push({
            record: parsed,
            status: 'SUCCESS'
          });

          this.premierMetrics.flatFileRecords++;

        } catch (error) {
          results.push({
            record: record,
            status: 'ERROR',
            error: error.message
          });
        }
      }

      this.premierMetrics.fileProcessingEvents++;

      return {
        fileId,
        totalRecords: records.length,
        successfulRecords: results.filter(r => r.status === 'SUCCESS').length,
        errorRecords: results.filter(r => r.status === 'ERROR').length,
        results
      };

    } catch (error) {
      logger.error('Flat file processing failed', { error: error.message });
      throw error;
    }
  }

  // Utility methods

  async ensureAuthenticated() {
    if (!this.authToken || Date.now() >= this.tokenExpiry - 60000) {
      await this.authenticate();
    }
  }

  /**
   * Ensure SOAP authentication is valid
   */
  async ensureSOAPAuthenticated() {
    if (!this.soapToken || Date.now() >= this.soapTokenExpiry - 60000) {
      await this.authenticateSOAP();
    }
  }

  /**
   * Ensure REST authentication is valid  
   */
  async ensureRESTAuthenticated() {
    if (!this.restToken || Date.now() >= this.restTokenExpiry - 60000) {
      await this.authenticateREST();
    }
  }

  async processTransactionSOAP(operation, request) {
    const wsdlUrl = this.premierConfig.soapUrl + PREMIER_ENDPOINTS.TRANSACTION_SERVICE_WSDL;
    
    let soapClient = this.soapClients.get('transaction');
    if (!soapClient) {
      soapClient = await soap.createClientAsync(wsdlUrl);
      this.soapClients.set('transaction', soapClient);
    }

    const [result] = await soapClient[operation + 'Async'](request);
    return result;
  }

  async getTransactionStatusSOAP(transactionId) {
    const request = {
      sessionToken: this.authToken,
      transactionId: transactionId
    };

    return await this.processTransactionSOAP('GetTransactionStatus', request);
  }

  async performBSAScreening(transaction) {
    try {
      const response = await this.httpClient.post(PREMIER_ENDPOINTS.BSA_SCREENING, {
        amount: transaction.amount,
        accountNumber: transaction.fromAccount,
        description: transaction.description
      });

      return {
        passed: response.data.status === 'CLEAR',
        reportingRequired: response.data.reportingRequired || false,
        flags: response.data.flags || []
      };

    } catch (error) {
      logger.warn('BSA screening failed', { error: error.message });
      return { passed: true, reportingRequired: false, flags: [] };
    }
  }

  parseFlatFileRecord(record, layout) {
    const result = {};
    
    for (const [fieldName, fieldLayout] of Object.entries(layout)) {
      const value = record.substring(
        fieldLayout.start,
        fieldLayout.start + fieldLayout.length
      ).trim();
      
      if (value) {
        result[fieldName] = value;
      }
    }

    return result;
  }

  mapPremierAccountType(typeCode) {
    const types = {
      '01': 'CHECKING',
      '02': 'SAVINGS',
      '03': 'MONEY_MARKET',
      '04': 'CD',
      '05': 'IRA',
      '06': 'LOAN'
    };
    return types[typeCode] || 'UNKNOWN';
  }

  mapPremierAccountStatus(statusCode) {
    const statuses = {
      'A': 'ACTIVE',
      'C': 'CLOSED',
      'F': 'FROZEN',
      'D': 'DORMANT',
      'P': 'PENDING'
    };
    return statuses[statusCode] || 'UNKNOWN';
  }

  mapPremierTransactionStatus(statusCode) {
    const statuses = {
      'P': TRANSACTION_STATUS.PENDING,
      'A': TRANSACTION_STATUS.AUTHORIZED,
      'C': TRANSACTION_STATUS.CONFIRMED,
      'S': TRANSACTION_STATUS.SETTLED,
      'F': TRANSACTION_STATUS.FAILED,
      'R': TRANSACTION_STATUS.REJECTED
    };
    return statuses[statusCode] || TRANSACTION_STATUS.PENDING;
  }

  /**
   * Generic SOAP service call
   * @param {Object} request 
   * @returns {Promise<Object>}
   */
  async callSOAPService(request) {
    try {
      const envelope = this.buildSOAPEnvelope(request.method, request.parameters || {});
      
      const response = await this.httpClient.post('/soap', envelope, {
        headers: {
          'Content-Type': 'text/xml; charset=utf-8',
          'SOAPAction': request.method
        }
      });

      const parsed = this.parseSOAPResponse(response.data);
      
      // Check for SOAP fault
      if (parsed.faultstring) {
        throw new Error(parsed.faultstring);
      }

      this.premierMetrics.soapCalls++;
      return parsed;

    } catch (error) {
      logger.error('SOAP service call failed', {
        method: request.method,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Build SOAP envelope
   * @param {string} method 
   * @param {Object} parameters 
   * @returns {string}
   */
  buildSOAPEnvelope(method, parameters) {
    let parametersXml = '';
    
    for (const [key, value] of Object.entries(parameters)) {
      parametersXml += `<${key}>${value}</${key}>`;
    }

    return `
      <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
        <soap:Header>
          <SessionToken>${this.authToken || ''}</SessionToken>
        </soap:Header>
        <soap:Body>
          <${method}>
            ${parametersXml}
          </${method}>
        </soap:Body>
      </soap:Envelope>
    `.trim();
  }

  /**
   * Parse SOAP response
   * @param {string} soapXml 
   * @returns {Object}
   */
  parseSOAPResponse(soapXml) {
    try {
      const parser = new xml2js.Parser({ explicitArray: false });
      let result = {};
      
      parser.parseString(soapXml, (err, parsed) => {
        if (err) {
          throw new Error(`Failed to parse SOAP response: ${err.message}`);
        }
        
        const body = parsed['soap:Envelope']['soap:Body'];
        
        // Check for SOAP fault
        if (body['soap:Fault']) {
          const fault = body['soap:Fault'];
          result = {
            faultcode: fault.faultcode,
            faultstring: fault.faultstring,
            detail: fault.detail
          };
          return;
        }
        
        // Extract response data (assumes first element in body is the response)
        const responseKey = Object.keys(body)[0];
        if (responseKey) {
          result = body[responseKey];
        }
      });
      
      return result;
      
    } catch (error) {
      logger.error('Failed to parse SOAP response', { error: error.message });
      throw error;
    }
  }

  /**
   * Generic REST service call
   * @param {string} method 
   * @param {string} endpoint 
   * @param {Object} data 
   * @returns {Promise<Object>}
   */
  async callRESTService(method, endpoint, data = null) {
    try {
      await this.ensureRESTAuthenticated();
      
      const config = {
        method: method.toLowerCase(),
        url: endpoint,
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'Authorization': `Bearer ${this.restToken || this.authToken}`,
          'X-Institution-ID': this.premierConfig.institutionId,
          'X-Branch-ID': this.premierConfig.branchId
        }
      };
      
      if (data && (method.toUpperCase() === 'POST' || method.toUpperCase() === 'PUT')) {
        config.data = data;
      }
      
      const response = await this.httpClient(config);
      
      this.premierMetrics.restCalls++;
      return response.data;
      
    } catch (error) {
      logger.error('REST service call failed', {
        method,
        endpoint,
        error: error.message
      });
      
      // Extract error message from response if available
      if (error.response && error.response.data && error.response.data.message) {
        throw new Error(error.response.data.message);
      }
      
      throw error;
    }
  }

  /**
   * Get enhanced status with Premier metrics
   * @returns {Object}
   */
  getStatus() {
    const baseStatus = super.getStatus();
    
    return {
      ...baseStatus,
      premierMetrics: this.premierMetrics,
      authStatus: {
        hasToken: !!this.authToken,
        tokenExpiry: this.tokenExpiry,
        timeToExpiry: this.tokenExpiry ? this.tokenExpiry - Date.now() : null
      },
      integrationConfig: {
        preferRESTOverSOAP: this.premierConfig.preferRESTOverSOAP,
        enableFileProcessing: this.premierConfig.enableFileProcessing,
        enableRealTimeAPIs: this.premierConfig.enableRealTimeAPIs
      },
      soapClients: {
        activeClients: this.soapClients.size,
        clients: Array.from(this.soapClients.keys())
      }
    };
  }
}

module.exports = {
  FiservPremierConnector,
  PREMIER_ENDPOINTS,
  PREMIER_TRANSACTION_TYPES,
  PREMIER_ACCOUNT_TYPES,
  PREMIER_FILE_LAYOUT
};