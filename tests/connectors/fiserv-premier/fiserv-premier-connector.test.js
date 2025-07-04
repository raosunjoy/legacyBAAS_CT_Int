/**
 * Fiserv Premier Connector Complete Test Suite
 * Achieving 100% test coverage for Fiserv Premier community banking integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const axios = require('axios');
const soap = require('soap');
jest.mock('axios');
jest.mock('soap');

const { 
  FiservPremierConnector, 
  PREMIER_ENDPOINTS, 
  PREMIER_TRANSACTION_TYPES,
  PREMIER_ACCOUNT_TYPES 
} = require('../../../src/connectors/fiserv-premier/fiserv-premier-connector');

const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

describe('FiservPremierConnector - Complete Test Suite', () => {
  let connector;
  let mockHttpClient;
  let config;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock SOAP client
    const mockSoapClient = {
      AuthenticateAsync: jest.fn().mockResolvedValue([{
        sessionToken: 'SOAP_TOKEN_001',
        expiresIn: 3600
      }]),
      GetAccountAsync: jest.fn().mockResolvedValue([{
        accountNumber: '123456789',
        accountType: '01',
        status: 'A',
        currentBalance: '1000.00',
        availableBalance: '950.00'
      }]),
      ProcessTransactionAsync: jest.fn().mockResolvedValue([{
        transactionId: 'TXN123456',
        status: 'P',
        authCode: 'AUTH123'
      }])
    };
    
    soap.createClientAsync = jest.fn().mockResolvedValue(mockSoapClient);
    
    // Mock configuration
    config = {
      baseUrl: 'https://premier.fiserv.com/api',
      soapUrl: 'https://premier.fiserv.com/soap',
      institutionId: 'TEST_PREMIER_001',
      username: 'test_user',
      password: 'test_password',
      applicationId: 'TEST_APP_001',
      preferRESTOverSOAP: true,
      enableFileProcessing: true,
      enableRealTimeAPIs: true,
      enableBranchIntegration: true,
      enableTellerIntegration: true,
      enableSSL: true,
      testMode: true
    };

    // Create connector instance first
    connector = new FiservPremierConnector(config);
    
    // Mock the httpClient as a function that can be called directly
    mockHttpClient = jest.fn();
    mockHttpClient.defaults = { headers: { common: {} } };
    mockHttpClient.post = jest.fn();
    mockHttpClient.get = jest.fn();
    mockHttpClient.put = jest.fn();
    mockHttpClient.delete = jest.fn();
    
    // Make the main function return a proper response structure
    mockHttpClient.mockImplementation(() => Promise.resolve({
      data: {},
      status: 200,
      config: { metadata: { startTime: Date.now() } }
    }));
    
    // Replace the connector's httpClient with our mock
    connector.httpClient = mockHttpClient;
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('Constructor and Configuration', () => {
    test('should initialize with complete configuration', () => {
      expect(connector.config.bankCode).toBe('FISERV_PREMIER');
      expect(connector.config.bankName).toBe('Fiserv Premier Platform');
      expect(connector.premierConfig.institutionId).toBe('TEST_PREMIER_001');
      expect(connector.premierConfig.baseUrl).toBe('https://premier.fiserv.com/api');
      expect(connector.premierConfig.soapUrl).toBe('https://premier.fiserv.com/soap');
      expect(connector.premierConfig.preferRESTOverSOAP).toBe(true);
      expect(connector.premierConfig.enableFileProcessing).toBe(true);
      expect(connector.premierConfig.enableBranchIntegration).toBe(true);
    });

    test('should handle minimal configuration', () => {
      const minimalConnector = new FiservPremierConnector({});
      expect(minimalConnector.config.timeout).toBe(30000);
      expect(minimalConnector.premierConfig.preferRESTOverSOAP).toBe(true);
      expect(minimalConnector.premierConfig.enableFileProcessing).toBe(true);
      expect(minimalConnector.premierConfig.enableBranchIntegration).toBe(true);
    });

    test('should use environment variables when config not provided', () => {
      process.env.FISERV_PREMIER_API_URL = 'https://api.premier.com';
      process.env.FISERV_PREMIER_SOAP_URL = 'https://soap.premier.com';
      process.env.FISERV_PREMIER_INSTITUTION_ID = 'ENV_PREMIER_001';
      
      const envConnector = new FiservPremierConnector({});
      expect(envConnector.premierConfig.baseUrl).toBe('https://api.premier.com');
      expect(envConnector.premierConfig.soapUrl).toBe('https://soap.premier.com');
      expect(envConnector.premierConfig.institutionId).toBe('ENV_PREMIER_001');
      
      delete process.env.FISERV_PREMIER_API_URL;
      delete process.env.FISERV_PREMIER_SOAP_URL;
      delete process.env.FISERV_PREMIER_INSTITUTION_ID;
    });

    test('should initialize required components', () => {
      expect(connector.soapClients).toBeInstanceOf(Map);
      expect(connector.httpClient).toBeDefined();
      expect(connector.accountCache).toBeInstanceOf(Map);
      expect(connector.customerCache).toBeInstanceOf(Map);
      expect(connector.premierMetrics).toBeDefined();
    });

    test('should initialize metrics tracking', () => {
      expect(connector.premierMetrics).toEqual({
        soapCalls: 0,
        restCalls: 0,
        fileProcessingEvents: 0,
        flatFileRecords: 0,
        flatFileProcesses: 0,
        branchTransactions: 0,
        tellerTransactions: 0,
        branchOperations: 0,
        realTimeQueries: 0,
        bsaChecks: 0,
        cipVerifications: 0
      });
    });
  });

  describe('Authentication and Session Management', () => {
    test('should authenticate with SOAP service', async () => {
      const mockSoapResponse = {
        data: `
          <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
            <soap:Body>
              <AuthenticateResponse>
                <SessionToken>SOAP_TOKEN_001</SessionToken>
                <UserId>test_user</UserId>
                <BranchId>BR001</BranchId>
                <ExpiresIn>3600</ExpiresIn>
                <Status>SUCCESS</Status>
              </AuthenticateResponse>
            </soap:Body>
          </soap:Envelope>
        `
      };

      mockHttpClient.post.mockResolvedValue(mockSoapResponse);

      await connector.authenticateSOAP();

      expect(connector.soapToken).toBe('SOAP_TOKEN_001');
      expect(connector.soapTokenExpiry).toBeGreaterThan(Date.now());
      expect(connector.premierMetrics.soapCalls).toBe(1);
    });

    test('should authenticate with REST service', async () => {
      const mockRestResponse = {
        data: {
          accessToken: 'REST_TOKEN_001',
          tokenType: 'Bearer',
          expiresIn: 3600,
          scope: 'accounts transactions customers'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(mockRestResponse);

      await connector.authenticateREST();

      expect(connector.authToken).toBe('REST_TOKEN_001');
      expect(connector.tokenExpiry).toBeGreaterThan(Date.now());
    });

    test('should handle authentication failure', async () => {
      const authError = new Error('Authentication failed');
      authError.response = {
        status: 401,
        data: {
          error: 'INVALID_CREDENTIALS',
          description: 'Invalid user credentials'
        }
      };

      // Mock SOAP client to fail
      const mockFailingSoapClient = {
        AuthenticateAsync: jest.fn().mockRejectedValue(authError)
      };
      
      soap.createClientAsync = jest.fn().mockResolvedValue(mockFailingSoapClient);

      await expect(connector.authenticateSOAP()).rejects.toThrow('Premier SOAP authentication failed');
      expect(connector.metrics.authenticationFailures).toBe(1);
    });

    test('should handle hybrid authentication', async () => {
      // Override SOAP client mock for this test
      const mockSoapClient = {
        AuthenticateAsync: jest.fn().mockResolvedValue([{
          sessionToken: 'SOAP_001',
          expiresIn: 3600
        }])
      };
      
      soap.createClientAsync = jest.fn().mockResolvedValue(mockSoapClient);
      
      const restResponse = {
        data: {
          accessToken: 'REST_001',
          expiresIn: 3600
        }
      };

      mockHttpClient.post.mockResolvedValueOnce(restResponse);

      await connector.authenticateHybrid();

      expect(connector.soapToken).toBe('SOAP_001');
      expect(connector.restToken).toBe('REST_001');
    });

    test('should refresh expired tokens', async () => {
      connector.soapToken = 'expired_soap_token';
      connector.soapTokenExpiry = Date.now() - 1000;

      // Mock SOAP client to return new token
      const mockRefreshSoapClient = {
        AuthenticateAsync: jest.fn().mockResolvedValue([{
          sessionToken: 'NEW_SOAP_001',
          expiresIn: 3600
        }])
      };
      
      soap.createClientAsync = jest.fn().mockResolvedValue(mockRefreshSoapClient);

      await connector.ensureSOAPAuthenticated();

      expect(connector.soapToken).toBe('NEW_SOAP_001');
    });
  });

  describe('SOAP Service Integration', () => {
    beforeEach(() => {
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.soapTokenExpiry = Date.now() + 3600000;
    });

    test('should call SOAP service successfully', async () => {
      const soapRequest = {
        method: 'GetAccountDetails',
        parameters: {
          accountNumber: '1234567890',
          includeBalance: true
        }
      };

      const soapResponse = {
        data: `
          <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
            <soap:Body>
              <GetAccountDetailsResponse>
                <AccountNumber>1234567890</AccountNumber>
                <AccountName>JOHN DOE</AccountName>
                <AccountType>CHECKING</AccountType>
                <Balance>1000.00</Balance>
                <Status>ACTIVE</Status>
                <BranchId>BR001</BranchId>
              </GetAccountDetailsResponse>
            </soap:Body>
          </soap:Envelope>
        `
      };

      mockHttpClient.post.mockResolvedValue(soapResponse);

      const result = await connector.callSOAPService(soapRequest);

      expect(result.AccountNumber).toBe('1234567890');
      expect(result.AccountName).toBe('JOHN DOE');
      expect(result.Balance).toBe('1000.00');
      expect(connector.premierMetrics.soapCalls).toBe(1);
    });

    test('should handle SOAP fault', async () => {
      const soapFault = {
        data: `
          <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
            <soap:Body>
              <soap:Fault>
                <faultcode>Client</faultcode>
                <faultstring>Invalid account number</faultstring>
                <detail>
                  <ErrorCode>ACCT_NOT_FOUND</ErrorCode>
                </detail>
              </soap:Fault>
            </soap:Body>
          </soap:Envelope>
        `
      };

      mockHttpClient.post.mockResolvedValue(soapFault);

      await expect(connector.callSOAPService({ method: 'GetAccountDetails' })).rejects.toThrow('Invalid account number');
    });

    test('should build SOAP envelope correctly', () => {
      const method = 'GetAccountDetails';
      const parameters = {
        accountNumber: '1234567890',
        includeBalance: true
      };

      const envelope = connector.buildSOAPEnvelope(method, parameters);

      expect(envelope).toContain('<soap:Envelope');
      expect(envelope).toContain('<GetAccountDetails>');
      expect(envelope).toContain('<accountNumber>1234567890</accountNumber>');
      expect(envelope).toContain('<includeBalance>true</includeBalance>');
    });

    test('should parse SOAP response correctly', () => {
      const soapXml = `
        <soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
          <soap:Body>
            <GetAccountDetailsResponse>
              <AccountNumber>1234567890</AccountNumber>
              <Balance>1500.00</Balance>
            </GetAccountDetailsResponse>
          </soap:Body>
        </soap:Envelope>
      `;

      const parsed = connector.parseSOAPResponse(soapXml);

      expect(parsed.AccountNumber).toBe('1234567890');
      expect(parsed.Balance).toBe('1500.00');
    });
  });

  describe('REST Service Integration', () => {
    beforeEach(() => {
      connector.restToken = 'REST_TOKEN_001';
      connector.restTokenExpiry = Date.now() + 3600000;
    });

    test('should call REST service successfully', async () => {
      const restResponse = {
        data: {
          accountNumber: '1234567890',
          accountName: 'JANE SMITH',
          accountType: 'SAVINGS',
          balance: 2500.00,
          status: 'ACTIVE',
          branchId: 'BR001'
        }
      };

      mockHttpClient.mockResolvedValue(restResponse);

      const result = await connector.callRESTService('GET', '/accounts/1234567890');

      expect(result.accountNumber).toBe('1234567890');
      expect(result.accountName).toBe('JANE SMITH');
      expect(result.balance).toBe(2500.00);
      expect(connector.premierMetrics.restCalls).toBe(1);
    });

    test('should handle REST API errors', async () => {
      const restError = new Error('REST API error');
      restError.response = {
        status: 404,
        data: {
          error: 'NOT_FOUND',
          message: 'Account not found'
        }
      };

      mockHttpClient.mockRejectedValue(restError);

      await expect(connector.callRESTService('GET', '/accounts/9999999999')).rejects.toThrow('Account not found');
    });

    test('should include proper headers in REST calls', async () => {
      mockHttpClient.mockResolvedValue({ data: {} });

      await connector.callRESTService('POST', '/transactions', { amount: 100 });

      expect(mockHttpClient).toHaveBeenCalledWith(expect.objectContaining({
        headers: expect.objectContaining({
          'Authorization': 'Bearer REST_TOKEN_001',
          'Content-Type': 'application/json',
          'X-Institution-ID': 'TEST_PREMIER_001',
          'X-Branch-ID': 'BR001'
        })
      }));
    });
  });

  describe('Account Operations', () => {
    beforeEach(() => {
      connector.authToken = 'REST_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.soapTokenExpiry = Date.now() + 3600000;
    });

    test('should get account details via SOAP', async () => {
      // Force SOAP usage for this test
      connector.premierConfig.preferRESTOverSOAP = false;
      
      // Mock SOAP client
      const mockSoapClient = {
        GetAccountDetailsAsync: jest.fn().mockResolvedValue([{
          accountDetails: {
            accountNumber: '1234567890',
            accountName: 'COMMUNITY MEMBER',
            accountType: '01',
            currentBalance: '1500.00',
            availableBalance: '1450.00',
            status: 'A',
            branchCode: 'BR001',
            productCode: 'CHK001',
            openDate: '2023-01-15',
            lastActivity: '2023-12-01'
          }
        }])
      };
      
      soap.createClientAsync = jest.fn().mockResolvedValue(mockSoapClient);

      const result = await connector.getAccountDetails('1234567890');

      expect(result).toEqual({
        accountNumber: '1234567890',
        accountType: 'CHECKING',
        accountStatus: 'ACTIVE',
        customerId: undefined,
        productCode: 'CHK001',
        openDate: '2023-01-15',
        currentBalance: 1500.00,
        availableBalance: 1450.00,
        interestRate: 0,
        maturityDate: undefined,
        branchCode: 'BR001',
        currency: 'USD'
      });
    });

    test('should get account details via REST fallback', async () => {
      // Force REST usage for this test (default behavior)
      connector.premierConfig.preferRESTOverSOAP = true;
      
      // REST call succeeds
      const restResponse = {
        data: {
          accountNumber: '1234567890',
          accountType: '02',
          currentBalance: '2000.00',
          availableBalance: '1950.00',
          status: 'A',
          productCode: 'SAV001',
          openDate: '2023-01-15',
          branchCode: 'BR001'
        }
      };

      mockHttpClient.get.mockResolvedValue(restResponse);

      const result = await connector.getAccountDetails('1234567890');

      expect(result.accountNumber).toBe('1234567890');
      expect(result.accountType).toBe('SAVINGS');
      expect(result.currentBalance).toBe(2000.00);
      expect(result.availableBalance).toBe(1950.00);
      expect(result.accountStatus).toBe('ACTIVE');
    });

    test('should check account balance with both services', async () => {
      const balanceResponse = {
        data: {
          currentBalance: 2000.00,
          availableBalance: 1800.00,
          pendingAmount: 200.00,
          holdAmount: 0.00
        }
      };

      mockHttpClient.get.mockResolvedValue(balanceResponse);

      const result = await connector.checkAccountBalance('1234567890', 'USD');

      expect(result.accountNumber).toBe('1234567890');
      expect(result.currency).toBe('USD');
      expect(result.currentBalance).toBe(2000.00);
      expect(result.availableBalance).toBe(1800.00);
      expect(result.pendingAmount).toBe(200.00);
      expect(result.holdAmount).toBe(0.00);
      expect(result.lastUpdated).toBeDefined();
    });

    test('should handle account not found', async () => {
      const notFoundError = new Error('Account not found');
      notFoundError.response = {
        status: 404,
        data: {
          error: 'ACCOUNT_NOT_FOUND',
          message: 'Account not found'
        }
      };

      mockHttpClient.get.mockRejectedValue(notFoundError);

      await expect(connector.getAccountDetails('9999999999')).rejects.toThrow('Account not found');
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(() => {
      connector.authToken = 'REST_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.restToken = 'REST_TOKEN_001';
      connector.soapTokenExpiry = Date.now() + 3600000;
      connector.restTokenExpiry = Date.now() + 3600000;
    });

    test('should validate transaction', async () => {
      const transaction = {
        id: 'TXN_001',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 100.00,
        currency: 'USD'
      };

      // Mock account check (REST response)
      const accountResponse = {
        data: {
          accountNumber: '1234567890',
          accountType: '01',
          status: 'A',
          currentBalance: '1000.00',
          availableBalance: '950.00'
        }
      };

      mockHttpClient.get.mockResolvedValue(accountResponse);
      
      // Mock BSA screening if needed (though not triggered for amounts < $10,000)
      const bsaResponse = {
        data: {
          status: 'CLEAR',
          reportingRequired: false,
          flags: []
        }
      };
      mockHttpClient.post.mockResolvedValue(bsaResponse);

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(true);
      expect(result.errors).toEqual([]);
      expect(result.warnings).toEqual([]);
    });

    test('should process debit transaction via SOAP', async () => {
      // Force SOAP usage for this test
      connector.premierConfig.preferRESTOverSOAP = false;
      
      const transaction = {
        id: 'TXN_DEBIT_001',
        fromAccount: '1234567890',
        amount: 500.00,
        currency: 'USD',
        description: 'Community bank debit',
        reference: 'REF_001'
      };

      // Mock SOAP client
      const mockSoapClient = {
        ProcessDebitAsync: jest.fn().mockResolvedValue([{
          transactionId: 'TXN_DEBIT_001',
          status: 'C',
          amount: '500.00',
          processedAt: '2023-12-01T15:00:00Z',
          reference: 'REF_001',
          confirmationNumber: 'CONF_001'
        }])
      };
      
      soap.createClientAsync = jest.fn().mockResolvedValue(mockSoapClient);

      const result = await connector.processDebit(transaction);

      expect(result).toEqual({
        transactionId: 'TXN_DEBIT_001',
        status: TRANSACTION_STATUS.CONFIRMED,
        amount: 500.00,
        processedAt: '2023-12-01T15:00:00Z',
        reference: 'REF_001',
        confirmationNumber: 'CONF_001'
      });
    });

    test('should process credit transaction via REST', async () => {
      // Force REST usage for this test (default behavior)
      connector.premierConfig.preferRESTOverSOAP = true;
      
      const transaction = {
        id: 'TXN_CREDIT_001',
        toAccount: '9876543210',
        amount: 750.00,
        currency: 'USD',
        description: 'Community bank credit'
      };

      const creditResponse = {
        data: {
          transactionId: 'TXN_CREDIT_001',
          status: 'C',
          amount: '750.00',
          processedAt: '2023-12-01T15:05:00Z',
          reference: 'TXN_CREDIT_001',
          confirmationNumber: 'CONF_CREDIT_001'
        }
      };

      mockHttpClient.post.mockResolvedValue(creditResponse);

      const result = await connector.processCredit(transaction);

      expect(result).toEqual({
        transactionId: 'TXN_CREDIT_001',
        status: TRANSACTION_STATUS.CONFIRMED,
        amount: 750.00,
        processedAt: '2023-12-01T15:05:00Z',
        reference: 'TXN_CREDIT_001',
        confirmationNumber: 'CONF_CREDIT_001'
      });
    });

    test('should handle transaction failures', async () => {
      const transaction = {
        id: 'TXN_FAIL_001',
        fromAccount: '1234567890',
        amount: 10000.00
      };

      const failureError = new Error('Insufficient funds');
      failureError.response = {
        status: 400,
        data: {
          error: 'INSUF_FUNDS',
          message: 'Insufficient funds'
        }
      };

      mockHttpClient.post.mockRejectedValue(failureError);

      await expect(connector.processDebit(transaction)).rejects.toThrow('Insufficient funds');
    });

    test('should get transaction status', async () => {
      const statusResponse = {
        data: {
          transactionId: 'TXN_001',
          status: 'C',
          amount: '1000.00',
          processedAt: '2023-12-01T15:00:00Z',
          description: 'Test transaction',
          reference: 'REF_001',
          confirmationNumber: 'CONF_001'
        }
      };

      mockHttpClient.get.mockResolvedValue(statusResponse);

      const result = await connector.getTransactionStatus('TXN_001');

      expect(result).toEqual({
        transactionId: 'TXN_001',
        status: TRANSACTION_STATUS.CONFIRMED,
        amount: 1000.00,
        processedAt: '2023-12-01T15:00:00Z',
        description: 'Test transaction',
        reference: 'REF_001',
        confirmationNumber: 'CONF_001'
      });
    });
  });

  describe('Flat File Processing', () => {
    beforeEach(() => {
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.soapTokenExpiry = Date.now() + 3600000;
    });

    test('should process CSV flat file', async () => {
      const csvData = `
        TransactionType,AccountNumber,Amount,Currency,Date,Reference
        DEBIT,1234567890,100.00,USD,2023-12-01,REF001
        CREDIT,9876543210,200.00,USD,2023-12-01,REF002
        DEBIT,1111111111,150.00,USD,2023-12-01,REF003
      `;

      const batchResponse = {
        data: {
          batchId: 'BATCH_CSV_001',
          status: 'COMPLETED',
          totalRecords: 3,
          successfulRecords: 3,
          failedRecords: 0,
          results: [
            { transactionId: 'TXN_001', status: 'COMPLETED', reference: 'REF001' },
            { transactionId: 'TXN_002', status: 'COMPLETED', reference: 'REF002' },
            { transactionId: 'TXN_003', status: 'COMPLETED', reference: 'REF003' }
          ]
        }
      };

      mockHttpClient.mockResolvedValue(batchResponse);

      const result = await connector.processFlatFile(csvData, 'CSV');

      expect(result.batchId).toBe('BATCH_CSV_001');
      expect(result.totalRecords).toBe(3);
      expect(result.successfulRecords).toBe(3);
      expect(connector.premierMetrics.flatFileProcesses).toBe(1);
    });

    test('should process fixed-width flat file', async () => {
      const fixedWidthData = `
        DEBIT     1234567890000010000USD20231201REF001  
        CREDIT    9876543210000020000USD20231201REF002  
        DEBIT     1111111111000015000USD20231201REF003  
      `;

      const batchResponse = {
        data: {
          batchId: 'BATCH_FW_001',
          status: 'COMPLETED',
          totalRecords: 3,
          successfulRecords: 2,
          failedRecords: 1,
          errors: [
            { recordNumber: 3, error: 'Invalid account number' }
          ]
        }
      };

      mockHttpClient.mockResolvedValue(batchResponse);

      const result = await connector.processFlatFile(fixedWidthData, 'FIXED_WIDTH');

      expect(result.batchId).toBe('BATCH_FW_001');
      expect(result.failedRecords).toBe(1);
      expect(result.errors).toHaveLength(1);
    });

    test('should parse CSV record correctly', () => {
      const csvRecord = 'DEBIT,1234567890,500.00,USD,2023-12-01,REF123';
      
      const parsed = connector.parseCSVRecord(csvRecord);

      expect(parsed).toEqual({
        transactionType: 'DEBIT',
        accountNumber: '1234567890',
        amount: 500.00,
        currency: 'USD',
        date: '2023-12-01',
        reference: 'REF123'
      });
    });

    test('should parse fixed-width record correctly', () => {
      const fixedRecord = 'DEBIT     1234567890000050000USD20231201REF123  ';
      
      const parsed = connector.parseFixedWidthRecord(fixedRecord);

      expect(parsed).toEqual({
        transactionType: 'DEBIT',
        accountNumber: '1234567890',
        amount: 500.00,
        currency: 'USD',
        date: '2023-12-01',
        reference: 'REF123'
      });
    });

    test('should handle malformed flat file records', () => {
      const malformedRecord = 'INVALID,RECORD';
      
      expect(() => {
        connector.parseCSVRecord(malformedRecord);
      }).toThrow('Invalid CSV record format');
    });
  });

  describe('Community Banking Features', () => {
    beforeEach(() => {
      connector.authToken = 'REST_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.restToken = 'REST_TOKEN_001';
      connector.soapTokenExpiry = Date.now() + 3600000;
      connector.restTokenExpiry = Date.now() + 3600000;
    });

    test('should process teller transaction', async () => {
      const tellerTransaction = {
        tellerId: 'TELLER_001',
        transactionType: 'CASH_DEPOSIT',
        accountNumber: '1234567890',
        amount: 500.00,
        currency: 'USD',
        branchId: 'BR001'
      };

      const tellerResponse = {
        data: {
          transactionId: 'TELLER_TXN_001',
          status: 'COMPLETED',
          tellerId: 'TELLER_001',
          branchId: 'BR001',
          timestamp: '2023-12-01T15:30:00Z',
          receipt: 'RCPT_001'
        }
      };

      mockHttpClient.mockResolvedValue(tellerResponse);

      const result = await connector.processTellerTransaction(tellerTransaction);

      expect(result.transactionId).toBe('TELLER_TXN_001');
      expect(result.tellerId).toBe('TELLER_001');
      expect(result.receipt).toBe('RCPT_001');
      expect(connector.premierMetrics.tellerTransactions).toBe(1);
    });

    test('should handle branch operations', async () => {
      const branchOperation = {
        branchId: 'BR001',
        operationType: 'BALANCE_INQUIRY',
        accountNumber: '1234567890',
        operatorId: 'OP001'
      };

      const branchResponse = {
        data: {
          operationId: 'BRANCH_OP_001',
          branchId: 'BR001',
          result: {
            accountNumber: '1234567890',
            balance: 2500.00,
            status: 'ACTIVE'
          },
          timestamp: '2023-12-01T16:00:00Z'
        }
      };

      mockHttpClient.mockResolvedValue(branchResponse);

      const result = await connector.processBranchOperation(branchOperation);

      expect(result.operationId).toBe('BRANCH_OP_001');
      expect(result.result.balance).toBe(2500.00);
      expect(connector.premierMetrics.branchOperations).toBe(1);
    });

    test('should verify member information', async () => {
      const memberInfo = {
        memberId: 'MEM_001',
        ssn: '123-45-6789',
        dateOfBirth: '1980-01-15'
      };

      const verificationResponse = {
        data: {
          verificationId: 'VER_001',
          status: 'VERIFIED',
          memberName: 'JOHN DOE',
          memberSince: '2015-01-01',
          accountCount: 3,
          creditScore: 750
        }
      };

      mockHttpClient.mockResolvedValue(verificationResponse);

      const result = await connector.verifyMemberInformation(memberInfo);

      expect(result.status).toBe('VERIFIED');
      expect(result.memberName).toBe('JOHN DOE');
      expect(result.creditScore).toBe(750);
    });

    test('should handle loan origination inquiry', async () => {
      const loanInquiry = {
        applicantSSN: '123-45-6789',
        loanType: 'AUTO',
        requestedAmount: 25000.00,
        term: 60
      };

      const loanResponse = {
        data: {
          inquiryId: 'LOAN_INQ_001',
          preApprovalStatus: 'APPROVED',
          approvedAmount: 25000.00,
          interestRate: 5.25,
          monthlyPayment: 474.07,
          conditions: ['PROOF_OF_INCOME', 'VEHICLE_APPRAISAL']
        }
      };

      mockHttpClient.mockResolvedValue(loanResponse);

      const result = await connector.processLoanInquiry(loanInquiry);

      expect(result.preApprovalStatus).toBe('APPROVED');
      expect(result.approvedAmount).toBe(25000.00);
      expect(result.interestRate).toBe(5.25);
    });
  });

  describe('BSA Compliance', () => {
    beforeEach(() => {
      connector.authToken = 'REST_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.restToken = 'REST_TOKEN_001';
      connector.soapTokenExpiry = Date.now() + 3600000;
      connector.restTokenExpiry = Date.now() + 3600000;
    });

    test('should perform BSA compliance check', async () => {
      const transaction = {
        amount: 12000.00,
        currency: 'USD',
        customerSSN: '123-45-6789',
        accountNumber: '1234567890'
      };

      const bsaResponse = {
        data: {
          complianceId: 'BSA_001',
          ctrRequired: true,
          ctrThreshold: 10000.00,
          reportingRequired: true,
          exemptions: [],
          riskLevel: 'MEDIUM',
          filingDeadline: '2023-12-16'
        }
      };

      mockHttpClient.mockResolvedValue(bsaResponse);

      const result = await connector.performBSACheck(transaction);

      expect(result.ctrRequired).toBe(true);
      expect(result.reportingRequired).toBe(true);
      expect(result.riskLevel).toBe('MEDIUM');
      expect(connector.premierMetrics.bsaChecks).toBe(1);
    });

    test('should handle exempt transactions', async () => {
      const transaction = {
        amount: 15000.00,
        currency: 'USD',
        customerSSN: '987-65-4321',
        exemptionCode: 'BUSINESS_CUSTOMER'
      };

      const bsaResponse = {
        data: {
          ctrRequired: false,
          reportingRequired: false,
          exemptions: ['BUSINESS_CUSTOMER'],
          exemptionVerified: true
        }
      };

      mockHttpClient.mockResolvedValue(bsaResponse);

      const result = await connector.performBSACheck(transaction);

      expect(result.ctrRequired).toBe(false);
      expect(result.exemptions).toContain('BUSINESS_CUSTOMER');
    });

    test('should perform CIP verification', async () => {
      const customer = {
        name: 'JANE SMITH',
        ssn: '987-65-4321',
        dateOfBirth: '1985-05-20',
        address: {
          street: '123 MAIN ST',
          city: 'ANYTOWN',
          state: 'ST',
          zip: '12345'
        }
      };

      const cipResponse = {
        data: {
          verificationId: 'CIP_001',
          status: 'VERIFIED',
          identityVerified: true,
          addressVerified: true,
          ssnVerified: true,
          dobVerified: true,
          riskScore: 0.15
        }
      };

      mockHttpClient.mockResolvedValue(cipResponse);

      const result = await connector.performCIPVerification(customer);

      expect(result.status).toBe('VERIFIED');
      expect(result.identityVerified).toBe(true);
      expect(result.riskScore).toBe(0.15);
      expect(connector.premierMetrics.cipVerifications).toBe(1);
    });

    test('should handle CIP verification failure', async () => {
      const customer = {
        name: 'SUSPICIOUS PERSON',
        ssn: '000-00-0000'
      };

      const cipResponse = {
        data: {
          status: 'FAILED',
          identityVerified: false,
          reasons: ['INVALID_SSN', 'NO_IDENTITY_MATCH'],
          riskScore: 0.95
        }
      };

      mockHttpClient.mockResolvedValue(cipResponse);

      const result = await connector.performCIPVerification(customer);

      expect(result.status).toBe('FAILED');
      expect(result.identityVerified).toBe(false);
      expect(result.reasons).toContain('INVALID_SSN');
    });
  });

  describe('Error Handling and Status Mapping', () => {
    test('should map Premier status codes correctly', () => {
      expect(connector.mapPremierStatus('COMPLETED')).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(connector.mapPremierStatus('PENDING')).toBe(TRANSACTION_STATUS.PENDING);
      expect(connector.mapPremierStatus('PROCESSING')).toBe(TRANSACTION_STATUS.PROCESSING);
      expect(connector.mapPremierStatus('FAILED')).toBe(TRANSACTION_STATUS.FAILED);
      expect(connector.mapPremierStatus('REJECTED')).toBe(TRANSACTION_STATUS.REJECTED);
      expect(connector.mapPremierStatus('UNKNOWN')).toBe(TRANSACTION_STATUS.PENDING);
    });

    test('should map Premier error codes correctly', () => {
      const errorMappings = [
        { premier: 'INSUF_FUNDS', expected: ERROR_CODES.INSUFFICIENT_FUNDS },
        { premier: 'ACCT_NOT_FOUND', expected: ERROR_CODES.INVALID_ACCOUNT },
        { premier: 'ACCT_CLOSED', expected: ERROR_CODES.ACCOUNT_INACTIVE },
        { premier: 'DUPLICATE_TXN', expected: ERROR_CODES.DUPLICATE_TRANSACTION },
        { premier: 'LIMIT_EXCEEDED', expected: ERROR_CODES.LIMIT_EXCEEDED },
        { premier: 'AUTH_FAILED', expected: ERROR_CODES.AUTHORIZATION_FAILED },
        { premier: 'SERVICE_DOWN', expected: ERROR_CODES.SERVICE_UNAVAILABLE },
        { premier: 'UNKNOWN_ERROR', expected: ERROR_CODES.SERVICE_UNAVAILABLE }
      ];

      errorMappings.forEach(({ premier, expected }) => {
        const mapped = connector.mapPremierError({ code: premier });
        expect(mapped.code).toBe(expected);
      });
    });

    test('should handle SOAP faults gracefully', () => {
      const soapFault = {
        faultcode: 'Server',
        faultstring: 'Internal server error',
        detail: {
          errorCode: 'SRV_001'
        }
      };

      const mapped = connector.handleSOAPFault(soapFault);

      expect(mapped.category).toBe('SOAP_FAULT');
      expect(mapped.severity).toBe('HIGH');
      expect(mapped.message).toBe('Internal server error');
    });

    test('should handle network errors with retries', async () => {
      connector.authToken = 'REST_TOKEN_001';
      connector.tokenExpiry = Date.now() + 3600000;
      connector.restToken = 'REST_TOKEN_001';
      connector.restTokenExpiry = Date.now() + 3600000;

      const networkError = new Error('Network timeout');
      networkError.code = 'ECONNRESET';

      mockHttpClient
        .mockRejectedValueOnce(networkError)
        .mockResolvedValueOnce({ data: { success: true } });

      const result = await connector.callRESTService('GET', '/test');

      expect(result.success).toBe(true);
      expect(mockHttpClient).toHaveBeenCalledTimes(2);
    });
  });

  describe('Status and Health Monitoring', () => {
    test('should return enhanced status with Premier metrics', () => {
      connector.premierMetrics.soapCalls = 50;
      connector.premierMetrics.restCalls = 30;
      connector.premierMetrics.tellerTransactions = 20;
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.restToken = 'REST_TOKEN_001';
      connector.isConnected = true;

      const status = connector.getStatus();

      expect(status).toMatchObject({
        bankCode: 'FISERV_PREMIER',
        connectionStatus: 'CONNECTED',
        premierMetrics: expect.objectContaining({
          soapCalls: 50,
          restCalls: 30,
          tellerTransactions: 20
        }),
        authStatus: expect.objectContaining({
          soapAuthenticated: true,
          restAuthenticated: true
        }),
        communityBanking: expect.objectContaining({
          tellerIntegration: true,
          branchOperations: true,
          bsaCompliance: true
        })
      });
    });

    test('should provide health status', async () => {
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.restToken = 'REST_TOKEN_001';
      mockHttpClient.mockResolvedValue({ status: 200 });

      const health = await connector.getHealthStatus();

      expect(health).toEqual({
        status: 'healthy',
        details: {
          soapService: 'active',
          restService: 'active',
          flatFileProcessor: 'ready',
          bsaCompliance: 'active',
          tellerSystem: 'operational'
        },
        timestamp: expect.any(String)
      });
    });

    test('should detect service degradation', async () => {
      connector.soapToken = null; // SOAP service down
      connector.restToken = 'REST_TOKEN_001';

      const health = await connector.getHealthStatus();

      expect(health.status).toBe('degraded');
      expect(health.details.soapService).toBe('inactive');
      expect(health.details.restService).toBe('active');
    });
  });

  describe('Cleanup and Resource Management', () => {
    test('should cleanup resources properly', async () => {
      connector.soapToken = 'SOAP_TOKEN_001';
      connector.restToken = 'REST_TOKEN_001';
      
      mockHttpClient.mockResolvedValue({ data: { status: 'LOGGED_OUT' } });

      await connector.cleanup();

      expect(connector.soapToken).toBeNull();
      expect(connector.restToken).toBeNull();
      expect(connector.isConnected).toBe(false);
    });

    test('should handle cleanup errors gracefully', async () => {
      mockHttpClient.mockRejectedValue(new Error('Cleanup failed'));

      // Should not throw
      await expect(connector.cleanup()).resolves.not.toThrow();
    });
  });

  describe('Integration with Base Connector', () => {
    test('should properly extend BaseBankingConnector', () => {
      const BaseBankingConnector = require('../../../src/connectors/base/base-banking-connector').BaseBankingConnector;
      expect(connector).toBeInstanceOf(BaseBankingConnector);
    });

    test('should implement all required abstract methods', () => {
      const requiredMethods = [
        'authenticate',
        'testConnection',
        'getAccountDetails',
        'checkAccountBalance',
        'validateTransaction',
        'processDebit',
        'processCredit',
        'getTransactionStatus'
      ];

      requiredMethods.forEach(method => {
        expect(connector[method]).toBeDefined();
        expect(typeof connector[method]).toBe('function');
      });
    });

    test('should track base metrics', () => {
      expect(connector.metrics).toBeDefined();
      expect(connector.metrics.totalTransactions).toBe(0);
      expect(connector.metrics.successfulTransactions).toBe(0);
      expect(connector.metrics.failedTransactions).toBe(0);
    });
  });
});

describe('Premier Constants and Exports', () => {
  test('should export all required endpoints', () => {
    expect(PREMIER_ENDPOINTS).toBeDefined();
    expect(PREMIER_ENDPOINTS.AUTHENTICATION_WSDL).toBe('/services/AuthenticationService?wsdl');
    expect(PREMIER_ENDPOINTS.REST_AUTH).toBe('/api/v1/auth/login');
    expect(PREMIER_ENDPOINTS.REST_ACCOUNTS).toBe('/api/v1/accounts');
    expect(PREMIER_ENDPOINTS.REST_TRANSACTIONS).toBe('/api/v1/transactions');
    expect(PREMIER_ENDPOINTS.FILE_UPLOAD).toBe('/api/v1/files/upload');
    expect(PREMIER_ENDPOINTS.BSA_SCREENING).toBe('/api/v1/compliance/bsa');
    expect(PREMIER_ENDPOINTS.CIP_VERIFICATION).toBe('/api/v1/compliance/cip');
    expect(PREMIER_ENDPOINTS.BALANCE_INQUIRY).toBe('/api/v1/realtime/balance');
    expect(PREMIER_ENDPOINTS.FUNDS_VERIFICATION).toBe('/api/v1/realtime/verify');
  });

  test('should export transaction types', () => {
    expect(PREMIER_TRANSACTION_TYPES).toBeDefined();
    expect(PREMIER_TRANSACTION_TYPES.CHECKING_DEPOSIT).toBe('100');
    expect(PREMIER_TRANSACTION_TYPES.CHECKING_WITHDRAWAL).toBe('101');
    expect(PREMIER_TRANSACTION_TYPES.TRANSFER).toBe('300');
    expect(PREMIER_TRANSACTION_TYPES.SAVINGS_DEPOSIT).toBe('200');
    expect(PREMIER_TRANSACTION_TYPES.SAVINGS_WITHDRAWAL).toBe('201');
    expect(PREMIER_TRANSACTION_TYPES.ACH_DEBIT).toBe('700');
  });

  test('should export account types', () => {
    expect(PREMIER_ACCOUNT_TYPES).toBeDefined();
    expect(PREMIER_ACCOUNT_TYPES.CHECKING).toBe('01');
    expect(PREMIER_ACCOUNT_TYPES.SAVINGS).toBe('02');
    expect(PREMIER_ACCOUNT_TYPES.MONEY_MARKET).toBe('03');
    expect(PREMIER_ACCOUNT_TYPES.CD).toBe('04');
    expect(PREMIER_ACCOUNT_TYPES.LOAN).toBe('10');
    expect(PREMIER_ACCOUNT_TYPES.MORTGAGE).toBe('13');
    expect(PREMIER_ACCOUNT_TYPES.BUSINESS_CHECKING).toBe('21');
    expect(PREMIER_ACCOUNT_TYPES.BUSINESS_SAVINGS).toBe('22');
  });
});