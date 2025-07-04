/**
 * FIS Systematics Connector Complete Test Suite
 * Achieving 100% test coverage for FIS Systematics mainframe integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const axios = require('axios');
jest.mock('axios');

const { 
  FISSystematicsConnector, 
  SYSTEMATICS_ENDPOINTS, 
  SYSTEMATICS_TRANSACTION_TYPES,
  SYSTEMATICS_RECORD_LAYOUTS 
} = require('../../../src/connectors/fis-systematics/fis-systematics-connector');

const { TRANSACTION_STATUS, ERROR_CODES } = require('../../../src/connectors/base/base-banking-connector');

describe('FISSystematicsConnector - Complete Test Suite', () => {
  let connector;
  let mockHttpClient;
  let config;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock configuration
    config = {
      baseUrl: 'https://systematics.fis.com/api',
      institutionId: 'TEST_FIS_001',
      userId: 'test_user',
      password: 'test_password',
      terminalId: 'TERM001',
      environment: 'test',
      enableCICS: true,
      enableBatch: true,
      enableMainframe: true,
      mainframeHost: 'mainframe.fis.com',
      mainframePort: 23,
      cobolCopybooks: '/path/to/copybooks',
      fixedWidthLayouts: true,
      characterSet: 'EBCDIC',
      recordLength: 80,
      batchProcessingEnabled: true,
      sessionTimeout: 1800,
      testMode: true
    };

    // Create connector instance first
    connector = new FISSystematicsConnector(config);
    
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
      expect(connector.config.bankCode).toBe('FIS_SYSTEMATICS');
      expect(connector.config.bankName).toBe('FIS Systematics Platform');
      expect(connector.systematicsConfig.institutionId).toBe('TEST_FIS_001');
      expect(connector.systematicsConfig.mainframeHost).toBe('mainframe.fis.com');
      expect(connector.systematicsConfig.enableBatchProcessing).toBe(true);
      expect(connector.systematicsConfig.enableCOBOLTransformation).toBe(true);
      expect(connector.systematicsConfig.characterSet).toBe('EBCDIC');
    });

    test('should handle minimal configuration', () => {
      const minimalConnector = new FISSystematicsConnector({});
      expect(minimalConnector.config.timeout).toBe(45000);
      expect(minimalConnector.systematicsConfig.enableBatchProcessing).toBe(true);
      expect(minimalConnector.systematicsConfig.enableCOBOLTransformation).toBe(true);
      expect(minimalConnector.systematicsConfig.recordLength).toBe(256);
    });

    test('should use environment variables when config not provided', () => {
      process.env.FIS_SYSTEMATICS_API_URL = 'https://api.fis.com/systematics';
      process.env.FIS_SYSTEMATICS_INSTITUTION_ID = 'ENV_FIS_001';
      process.env.FIS_SYSTEMATICS_HOST = 'mainframe.env.com';
      
      const envConnector = new FISSystematicsConnector({});
      expect(envConnector.systematicsConfig.baseUrl).toBe('https://api.fis.com/systematics');
      expect(envConnector.systematicsConfig.institutionId).toBe('ENV_FIS_001');
      expect(envConnector.systematicsConfig.mainframeHost).toBe('mainframe.env.com');
      
      delete process.env.FIS_SYSTEMATICS_API_URL;
      delete process.env.FIS_SYSTEMATICS_INSTITUTION_ID;
      delete process.env.FIS_SYSTEMATICS_HOST;
    });

    test('should initialize required components', () => {
      expect(connector.parser).toBeDefined();
      expect(connector.httpClient).toBeDefined();
      expect(connector.accountCache).toBeInstanceOf(Map);
      expect(connector.customerCache).toBeInstanceOf(Map);
      expect(connector.systematicsMetrics).toBeDefined();
    });

    test('should initialize metrics tracking', () => {
      expect(connector.systematicsMetrics).toEqual({
        mainframeTransactions: 0,
        batchTransactions: 0,
        fixedWidthRecords: 0,
        cobolTransformations: 0,
        cicsTransactions: 0,
        fileProcessingTime: 0,
        sessionRenewals: 0,
        sessionTimeouts: 0,
        mainframeConnections: 0
      });
    });
  });

  describe('Authentication and Session Management', () => {
    test('should authenticate with mainframe session', async () => {
      const mockSessionResponse = {
        data: {
          sessionId: 'SESS_001',
          userId: 'test_user',
          terminalId: 'TERM001',
          status: 'ACTIVE',
          expiresIn: 1800,
          cicsRegion: 'CICSPROD'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(mockSessionResponse);

      await connector.authenticate();

      expect(connector.sessionId).toBe('SESS_001');
      expect(connector.sessionExpiry).toBeGreaterThan(Date.now());
      expect(connector.isConnected).toBe(true);
    });

    test('should handle CICS authentication', async () => {
      const cicsResponse = {
        data: {
          cicsSession: 'CICS_SESS_001',
          region: 'CICSPROD',
          transaction: 'LOGON',
          status: 'SUCCESSFUL',
          facilities: ['TN3270', 'BATCH', 'INQUIRY']
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(cicsResponse);

      await connector.authenticateCICS();

      expect(connector.cicsSession.sessionId).toBe('CICS_SESS_001');
      expect(connector.cicsSession.region).toBe('CICSPROD');
      expect(connector.cicsSession.facilities).toContain('TN3270');
    });

    test('should handle authentication failure', async () => {
      const authError = new Error('Authentication failed');
      authError.response = {
        status: 401,
        data: { 
          error: 'INVALID_CREDENTIALS',
          description: 'Invalid user ID or password',
          sysCode: 'SYS001'
        }
      };

      mockHttpClient.post.mockRejectedValue(authError);

      await expect(connector.authenticate()).rejects.toThrow('Systematics authentication failed');
      expect(connector.metrics.authenticationFailures).toBe(1);
      expect(connector.isConnected).toBe(false);
    });

    test('should handle session timeout', async () => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() - 1000; // Expired

      const renewResponse = {
        data: {
          sessionId: 'SESS_002',
          status: 'RENEWED',
          timeout: 1800
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(renewResponse);

      await connector.ensureSessionActive();

      expect(connector.sessionId).toBe('SESS_002');
    });

    test('should handle session renewal failure', async () => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() - 1000;

      const renewError = new Error('Session renewal failed');
      renewError.response = { status: 403 };

      mockHttpClient.post.mockRejectedValue(renewError);

      await expect(connector.ensureSessionActive()).rejects.toThrow();
      expect(connector.systematicsMetrics.sessionTimeouts).toBe(1);
    });
  });

  describe('Mainframe Integration', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000; // Valid for 1 hour
      connector.isConnected = true;
    });

    test('should establish mainframe connection', async () => {
      const connectionResponse = {
        data: {
          connectionId: 'MF_CONN_001',
          host: 'mainframe.fis.com',
          port: 23,
          protocol: 'TN3270',
          status: 'CONNECTED',
          luName: 'LU001'
        },
        status: 200,
        config: { metadata: { startTime: Date.now() } }
      };

      mockHttpClient.post.mockResolvedValue(connectionResponse);

      await connector.connectToMainframe();

      expect(connector.mainframeConnection.connectionId).toBe('MF_CONN_001');
      expect(connector.mainframeConnection.protocol).toBe('TN3270');
      expect(connector.systematicsMetrics.mainframeConnections).toBe(1);
    });

    test('should execute CICS transaction', async () => {
      const cicsRequest = {
        transactionId: 'ACCT',
        programName: 'ACCTINQ',
        commArea: {
          accountNumber: '1234567890',
          function: 'INQUIRY'
        }
      };

      const cicsResponse = {
        data: {
          transactionId: 'ACCT',
          returnCode: 'NORMAL',
          commArea: {
            accountNumber: '1234567890',
            accountName: 'JOHN DOE',
            balance: '00000100000', // Fixed width format
            status: 'A'
          },
          responseTime: 150
        }
      };

      mockHttpClient.post.mockResolvedValue(cicsResponse);

      const result = await connector.executeCICSTransaction(cicsRequest);

      expect(result.returnCode).toBe('NORMAL');
      expect(result.commArea.accountNumber).toBe('1234567890');
      expect(connector.systematicsMetrics.cicsTransactions).toBe(1);
    });

    test('should handle CICS transaction failure', async () => {
      const cicsRequest = {
        transactionId: 'ACCT',
        programName: 'ACCTINQ'
      };

      const cicsError = new Error('CICS abend');
      cicsError.response = {
        status: 500,
        data: {
          abendCode: 'ASRA',
          programName: 'ACCTINQ',
          errorMessage: 'Program check'
        }
      };

      mockHttpClient.mockRejectedValue(cicsError);

      await expect(connector.executeCICSTransaction(cicsRequest)).rejects.toThrow('CICS transaction failed');
    });

    test('should handle 3270 screen interactions', async () => {
      const screenData = {
        screenId: 'ACCT001',
        fields: [
          { name: 'ACCOUNT', value: '1234567890', position: [2, 10] },
          { name: 'FUNCTION', value: 'I', position: [3, 10] }
        ]
      };

      const screenResponse = {
        data: {
          screenId: 'ACCT002',
          fields: [
            { name: 'ACCOUNT', value: '1234567890', position: [2, 10] },
            { name: 'NAME', value: 'JOHN DOE', position: [4, 10] },
            { name: 'BALANCE', value: '1000.00', position: [5, 10] }
          ],
          cursor: [6, 1],
          status: 'READY'
        }
      };

      mockHttpClient.post.mockResolvedValue(screenResponse);

      const result = await connector.handle3270Screen(screenData.screenName, screenData.fields);

      expect(result.screenId).toBe('ACCT002');
      expect(result.fields).toHaveLength(3);
    });
  });

  describe('Fixed-Width Record Processing', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
    });

    test('should parse fixed-width account record', () => {
      // Build fixed-width record: accountNumber(20) + accountType(2) + status(1) + openDate(8) + balance(15) + availableBalance(15) + customerId(15) + productCode(10) + interestRate(8) + lastUpdateDate(8)
      const fixedWidthRecord = '1234567890          01A20231201000000100000000000009000000000123456789000   TEST_PROD 0005000020240101';
      
      const parsed = connector.parseFixedWidthRecord(fixedWidthRecord, SYSTEMATICS_RECORD_LAYOUTS.ACCOUNT_MASTER);

      expect(parsed).toEqual({
        accountNumber: '1234567890          ',
        accountType: '01',
        status: 'A',
        openDate: '20231201',
        balance: '000000100000000',
        availableBalance: '000009000000000',
        customerId: '123456789000   ',
        productCode: 'TEST_PROD ',
        interestRate: '00050000',
        lastUpdateDate: '20240101'
      });

      expect(connector.systematicsMetrics.fixedWidthRecords).toBe(1);
    });

    test('should parse fixed-width transaction record', () => {
      // Build record: transactionId(16) + transactionType(2) + accountNumber(20) + amount(15) + valueDate(8) + processDate(8) + description(40) + reference(20) + status(1) + errorCode(4)
      const txnRecord = 'TXN001          0112345678901234567890000000500000002202312012023120105 TRANSFER PAYMENT                     REF123456789012345  P0000';
      
      const parsed = connector.parseFixedWidthRecord(txnRecord, SYSTEMATICS_RECORD_LAYOUTS.TRANSACTION_RECORD);

      expect(parsed).toEqual({
        transactionId: 'TXN001          ',
        transactionType: '01',
        accountNumber: '12345678901234567890',
        amount: '000000500000002',
        valueDate: '20231201',
        processDate: '20231201',
        description: '05 TRANSFER PAYMENT                     ',
        reference: 'REF123456789012345  ',
        status: 'P',
        errorCode: '0000'
      });
    });

    test('should handle malformed fixed-width record', () => {
      const malformedRecord = 'INVALID_RECORD';
      
      expect(() => {
        connector.parseFixedWidthRecord(malformedRecord, 'ACCOUNT_RECORD');
      }).toThrow('Invalid record format');
    });

    test('should format fixed-width record for mainframe', () => {
      const data = {
        accountNumber: '1234567890',
        accountType: '01',
        status: 'A',
        openDate: '20231201',
        balance: '000000250000000',
        availableBalance: '000000200000000',
        customerId: '123456789000001',
        productCode: 'CHECKING  ',
        interestRate: '00000000',
        lastUpdateDate: '20240101'
      };

      const formatted = connector.formatFixedWidthRecord(data, SYSTEMATICS_RECORD_LAYOUTS.ACCOUNT_MASTER);

      expect(formatted).toHaveLength(102); // Total length based on layout
      expect(formatted.substring(0, 20)).toBe('1234567890          ');
      expect(formatted.substring(20, 22)).toBe('01');
      expect(formatted.substring(22, 23)).toBe('A');
    });

    test('should convert EBCDIC to ASCII', () => {
      const ebcdicData = Buffer.from([0xC1, 0xC2, 0xC3]); // ABC in EBCDIC
      
      const ascii = connector.convertEBCDICToASCII(ebcdicData);
      
      expect(ascii).toBe('ABC');
    });

    test('should convert ASCII to EBCDIC', () => {
      const asciiData = 'ABC';
      
      const ebcdic = connector.convertASCIIToEBCDIC(asciiData);
      
      expect(ebcdic).toBeInstanceOf(Buffer);
      expect(ebcdic[0]).toBe(0xC1); // A in EBCDIC
    });
  });

  describe('Account Operations', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000; // Set to future time
      connector.isConnected = true;
    });

    test('should get account details via CICS', async () => {
      const accountResponse = {
        data: {
          transactionId: 'ACQY',
          returnCode: 'NORMAL',
          recordData: '1234567890          CHA20230101000000000100000000000001000000001            CHK       0000000020230101'
        }
      };

      mockHttpClient.post.mockResolvedValue(accountResponse);

      const result = await connector.getAccountDetails('1234567890');

      expect(result).toEqual({
        accountNumber: '1234567890',
        accountType: 'CHECKING', 
        accountStatus: 'ACTIVE',
        customerId: '001',
        productCode: 'CHK',
        openDate: '2023-01-01',
        currentBalance: 100000,
        availableBalance: 1000000,
        interestRate: 0.00,
        lastUpdateDate: '2023-01-01',
        currency: 'USD'
      });
    });

    test('should handle account not found', async () => {
      const notFoundResponse = {
        data: {
          returnCode: 'ERROR',
          errorCode: 'ACCT404',
          errorMessage: 'Account not found'
        }
      };

      mockHttpClient.post.mockResolvedValue(notFoundResponse);

      await expect(connector.getAccountDetails('9999999999')).rejects.toThrow('Account not found');
    });

    test('should check account balance via mainframe', async () => {
      const balanceResponse = {
        data: {
          transactionId: 'BALQ',
          returnCode: 'NORMAL',
          recordData: '000000150000   000000150000   000000005000   000000000000   '
        }
      };

      mockHttpClient.post.mockResolvedValue(balanceResponse);

      const result = await connector.checkAccountBalance('1234567890', 'USD');

      expect(result).toEqual({
        accountNumber: '1234567890',
        currency: 'USD',
        availableBalance: 1500.00,
        currentBalance: 1500.00,
        pendingAmount: 0.00,
        holdAmount: 50.00,
        lastUpdated: expect.any(String)
      });
    });

    test('should handle different currencies', async () => {
      const eurBalanceResponse = {
        data: {
          transactionId: 'BALQ',
          returnCode: 'NORMAL',
          recordData: '000000100000   000000100000   000000000000   000000000000   '
        }
      };

      mockHttpClient.post.mockResolvedValue(eurBalanceResponse);

      const result = await connector.checkAccountBalance('1234567890', 'EUR');

      expect(result.currency).toBe('EUR');
      expect(result.availableBalance).toBe(1000.00);
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
    });

    test('should validate transaction', async () => {
      const transaction = {
        id: 'TXN_001',
        type: 'debit',
        fromAccount: '1234567890',
        amount: 100.00,
        currency: 'USD'
      };

      // Mock account details call
      const accountResponse = {
        data: {
          transactionId: 'ACQY',
          returnCode: 'NORMAL',
          recordData: '1234567890          CHA20230101000000000200000000000002000000001            CHK       0000000020230101'
        }
      };

      mockHttpClient.post.mockResolvedValue(accountResponse);

      const result = await connector.validateTransaction(transaction);

      expect(result.isValid).toBe(true);
      expect(result.errors).toEqual([]);
      expect(result.systematicsChecks).toContain('Account status verified');
    });

    test('should process debit transaction via CICS', async () => {
      const transaction = {
        id: 'TXN_DEBIT_001',
        fromAccount: '1234567890',
        amount: 500.00,
        currency: 'USD',
        description: 'Debit transaction',
        reference: 'REF_001'
      };

      const debitResponse = {
        data: {
          transactionId: 'DEBT',
          returnCode: 'NORMAL',
          recordData: 'TXN_DEBIT_001    1234567890      000050000USD 20231201Debit transaction      REF_001         P'
        }
      };

      mockHttpClient.post.mockResolvedValue(debitResponse);

      const result = await connector.processDebit(transaction);

      expect(result).toEqual({
        transactionId: 'TXN_DEBIT_001',
        status: expect.any(String),
        amount: 500.00,
        processDate: expect.any(String),
        valueDate: expect.any(String),
        reference: 'REF_001',
        errorCode: null
      });
    });

    test('should process credit transaction', async () => {
      const transaction = {
        id: 'TXN_CREDIT_001',
        toAccount: '9876543210',
        amount: 750.00,
        currency: 'USD',
        description: 'Credit transaction'
      };

      const creditResponse = {
        data: {
          returnCode: 'NORMAL',
          commArea: {
            transactionId: 'TXN_CREDIT_001',
            toAccount: '9876543210',
            amount: '00000075000',
            status: 'POSTED',
            newBalance: '00000175000'
          }
        }
      };

      mockHttpClient.mockResolvedValue(creditResponse);

      const result = await connector.processCredit(transaction);

      expect(result.transactionId).toBe('TXN_CREDIT_001');
      expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(result.amount).toBe(750.00);
      expect(result.newBalance).toBe(1750.00);
    });

    test('should handle transaction failures', async () => {
      const transaction = {
        id: 'TXN_FAIL_001',
        fromAccount: '1234567890',
        amount: 10000.00
      };

      const failureResponse = {
        data: {
          returnCode: 'ERROR',
          errorCode: 'INSUF',
          errorMessage: 'Insufficient funds'
        }
      };

      mockHttpClient.mockResolvedValue(failureResponse);

      await expect(connector.processDebit(transaction)).rejects.toThrow('Transaction failed: Insufficient funds');
    });

    test('should get transaction status', async () => {
      const statusResponse = {
        data: {
          returnCode: 'NORMAL',
          commArea: {
            transactionId: 'TXN_001',
            status: 'POSTED',
            amount: '00000100000',
            currency: 'USD',
            postingDate: '20231201',
            authCode: 'AUTH123'
          }
        }
      };

      mockHttpClient.mockResolvedValue(statusResponse);

      const result = await connector.getTransactionStatus('TXN_001');

      expect(result).toEqual({
        transactionId: 'TXN_001',
        status: TRANSACTION_STATUS.CONFIRMED,
        amount: 1000.00,
        currency: 'USD',
        processedAt: '2023-12-01',
        authorizationCode: 'AUTH123'
      });
    });
  });

  describe('Batch Processing', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
    });

    test('should process batch file', async () => {
      const batchData = [
        'TXN001    1234567890000010000020231201DEBIT     ',
        'TXN002    9876543210000020000020231201CREDIT    ',
        'TXN003    1111111111000015000020231201DEBIT     '
      ];

      const batchResponse = {
        data: {
          batchId: 'BATCH_001',
          status: 'COMPLETED',
          totalRecords: 3,
          successfulRecords: 3,
          failedRecords: 0,
          results: [
            { transactionId: 'TXN001', status: 'POSTED' },
            { transactionId: 'TXN002', status: 'POSTED' },
            { transactionId: 'TXN003', status: 'POSTED' }
          ]
        }
      };

      mockHttpClient.mockResolvedValue(batchResponse);

      const result = await connector.processBatchFile(batchData);

      expect(result.batchId).toBe('BATCH_001');
      expect(result.totalRecords).toBe(3);
      expect(result.successfulRecords).toBe(3);
      expect(connector.systematicsMetrics.batchProcesses).toBe(1);
    });

    test('should handle batch processing errors', async () => {
      const batchData = ['INVALID_RECORD'];

      const batchError = new Error('Batch processing failed');
      batchError.response = {
        status: 400,
        data: { error: 'Invalid batch format' }
      };

      mockHttpClient.mockRejectedValue(batchError);

      await expect(connector.processBatchFile(batchData)).rejects.toThrow('Batch processing failed');
    });

    test('should submit batch job to mainframe', async () => {
      const jobData = {
        jobName: 'ACCTUPD',
        jobClass: 'A',
        priority: 5,
        datasets: ['ACCT.MASTER', 'ACCT.TRANS'],
        program: 'ACCTUPDT'
      };

      const jobResponse = {
        data: {
          jobId: 'JOB12345',
          jobName: 'ACCTUPD',
          status: 'SUBMITTED',
          priority: 5,
          submittedAt: '2023-12-01T10:00:00Z'
        }
      };

      mockHttpClient.post.mockResolvedValue(jobResponse);

      const result = await connector.submitBatchJob(jobData);

      expect(result.jobId).toBe('JOB12345');
      expect(result.status).toBe('SUBMITTED');
    });

    test('should check batch job status', async () => {
      const jobStatusResponse = {
        data: {
          jobId: 'JOB12345',
          status: 'COMPLETED',
          returnCode: 0,
          completedAt: '2023-12-01T10:30:00Z',
          cpuTime: '00:02:15',
          steps: [
            { stepName: 'STEP01', status: 'COMPLETED', returnCode: 0 },
            { stepName: 'STEP02', status: 'COMPLETED', returnCode: 0 }
          ]
        }
      };

      mockHttpClient.mockResolvedValue(jobStatusResponse);

      const result = await connector.getBatchJobStatus('JOB12345');

      expect(result.status).toBe('COMPLETED');
      expect(result.returnCode).toBe(0);
      expect(result.steps).toHaveLength(2);
    });
  });

  describe('COBOL Integration', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
    });

    test('should call COBOL program', async () => {
      const programCall = {
        programName: 'ACCTUPDT',
        linkage: {
          accountNumber: '1234567890',
          updateType: 'BALANCE',
          newBalance: 150000
        }
      };

      const cobolResponse = {
        data: {
          programName: 'ACCTUPDT',
          returnCode: 0,
          linkage: {
            accountNumber: '1234567890',
            updateType: 'BALANCE',
            newBalance: 150000,
            status: 'SUCCESS',
            timestamp: '20231201120000'
          }
        }
      };

      mockHttpClient.mockResolvedValue(cobolResponse);

      const result = await connector.callCOBOLProgram(programCall);

      expect(result.returnCode).toBe(0);
      expect(result.linkage.status).toBe('SUCCESS');
      expect(connector.systematicsMetrics.cobolCalls).toBe(1);
    });

    test('should handle COBOL program errors', async () => {
      const programCall = {
        programName: 'BADPROG',
        linkage: {}
      };

      const cobolError = new Error('COBOL program failed');
      cobolError.response = {
        status: 500,
        data: {
          programName: 'BADPROG',
          returnCode: 8,
          errorMessage: 'Program not found'
        }
      };

      mockHttpClient.mockRejectedValue(cobolError);

      await expect(connector.callCOBOLProgram(programCall)).rejects.toThrow('COBOL program failed');
    });

    test('should load COBOL copybook', () => {
      const copybook = `
        01 ACCOUNT-RECORD.
           05 ACCT-NUMBER    PIC X(10).
           05 ACCT-NAME      PIC X(30).
           05 ACCT-BALANCE   PIC 9(10)V99.
           05 ACCT-STATUS    PIC X(1).
      `;

      const layout = connector.loadCOBOLCopybook('ACCOUNT-RECORD', copybook);

      expect(layout).toEqual({
        name: 'ACCOUNT-RECORD',
        fields: [
          { name: 'ACCT-NUMBER', type: 'X', length: 10, start: 0 },
          { name: 'ACCT-NAME', type: 'X', length: 30, start: 10 },
          { name: 'ACCT-BALANCE', type: '9', length: 12, start: 40, decimal: 2 },
          { name: 'ACCT-STATUS', type: 'X', length: 1, start: 52 }
        ]
      });
    });
  });

  describe('Compliance and Business Rules', () => {
    beforeEach(() => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
    });

    test('should perform OFAC screening', async () => {
      const screening = {
        customerName: 'JOHN DOE',
        accountNumber: '1234567890',
        amount: 15000.00
      };

      const ofacResponse = {
        data: {
          screeningId: 'OFAC_001',
          status: 'CLEAR',
          matches: [],
          riskScore: 0.05,
          screeningDate: '2023-12-01T10:00:00Z'
        }
      };

      mockHttpClient.mockResolvedValue(ofacResponse);

      const result = await connector.performOFACScreening(screening);

      expect(result.status).toBe('CLEAR');
      expect(result.riskScore).toBe(0.05);
      expect(result.matches).toEqual([]);
    });

    test('should handle OFAC match', async () => {
      const screening = {
        customerName: 'SUSPICIOUS NAME',
        amount: 50000.00
      };

      const ofacResponse = {
        data: {
          status: 'MATCH',
          matches: [
            {
              listName: 'SDN',
              matchName: 'SUSPICIOUS NAME',
              matchScore: 0.95,
              listDate: '2023-01-01'
            }
          ],
          riskScore: 0.95
        }
      };

      mockHttpClient.mockResolvedValue(ofacResponse);

      const result = await connector.performOFACScreening(screening);

      expect(result.status).toBe('MATCH');
      expect(result.matches).toHaveLength(1);
      expect(result.riskScore).toBe(0.95);
    });

    test('should check BSA/CTR requirements', async () => {
      const transaction = {
        amount: 15000.00,
        currency: 'USD',
        customerSSN: '123-45-6789'
      };

      const bsaResponse = {
        data: {
          ctrRequired: true,
          ctrThreshold: 10000.00,
          reportingRequired: true,
          exemptions: [],
          filingDeadline: '2023-12-16'
        }
      };

      mockHttpClient.mockResolvedValue(bsaResponse);

      const result = await connector.checkBSARequirements(transaction);

      expect(result.ctrRequired).toBe(true);
      expect(result.reportingRequired).toBe(true);
    });

    test('should validate business rules', async () => {
      const transaction = {
        id: 'TXN_001',
        fromAccount: '1234567890',
        amount: 5000.00,
        type: 'WIRE'
      };

      const rulesResponse = {
        data: {
          validationId: 'VAL_001',
          passed: true,
          rulesChecked: [
            'DAILY_LIMIT',
            'VELOCITY_CHECK',
            'GEOGRAPHIC_RESTRICTION'
          ],
          warnings: [],
          recommendations: []
        }
      };

      mockHttpClient.mockResolvedValue(rulesResponse);

      const result = await connector.validateBusinessRules(transaction);

      expect(result.passed).toBe(true);
      expect(result.rulesChecked).toContain('DAILY_LIMIT');
    });
  });

  describe('Error Handling and Status Mapping', () => {
    test('should map Systematics status codes correctly', () => {
      expect(connector.mapSystematicsStatus('POSTED')).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(connector.mapSystematicsStatus('PENDING')).toBe(TRANSACTION_STATUS.PENDING);
      expect(connector.mapSystematicsStatus('REJECTED')).toBe(TRANSACTION_STATUS.REJECTED);
      expect(connector.mapSystematicsStatus('FAILED')).toBe(TRANSACTION_STATUS.FAILED);
      expect(connector.mapSystematicsStatus('UNKNOWN')).toBe(TRANSACTION_STATUS.PENDING);
    });

    test('should map Systematics error codes correctly', () => {
      const errorMappings = [
        { sys: 'INSUF', expected: ERROR_CODES.INSUFFICIENT_FUNDS },
        { sys: 'ACCT404', expected: ERROR_CODES.INVALID_ACCOUNT },
        { sys: 'ACCTCLS', expected: ERROR_CODES.ACCOUNT_INACTIVE },
        { sys: 'LIMIT', expected: ERROR_CODES.LIMIT_EXCEEDED },
        { sys: 'AUTH', expected: ERROR_CODES.AUTHORIZATION_FAILED },
        { sys: 'SYS001', expected: ERROR_CODES.AUTHENTICATION_FAILED },
        { sys: 'TIMEOUT', expected: ERROR_CODES.REQUEST_TIMEOUT },
        { sys: 'UNKNOWN', expected: ERROR_CODES.SERVICE_UNAVAILABLE }
      ];

      errorMappings.forEach(({ sys, expected }) => {
        const mapped = connector.mapSystematicsError({ code: sys });
        expect(mapped.code).toBe(expected);
      });
    });

    test('should handle mainframe abends', () => {
      const abend = {
        abendCode: 'ASRA',
        programName: 'ACCTINQ',
        psw: '0780000000000000',
        registers: {}
      };

      const mapped = connector.handleMainframeAbend(abend);

      expect(mapped.severity).toBe('HIGH');
      expect(mapped.category).toBe('PROGRAM_CHECK');
      expect(mapped.recovery).toBe('RESTART_REQUIRED');
    });
  });

  describe('Status and Health Monitoring', () => {
    test('should return enhanced status with Systematics metrics', () => {
      connector.systematicsMetrics.cicsTransactions = 100;
      connector.systematicsMetrics.mainframeConnections = 5;
      connector.sessionId = 'SESS_001';
      connector.isConnected = true;

      const status = connector.getStatus();

      expect(status).toMatchObject({
        bankCode: 'FIS_SYSTEMATICS',
        connectionStatus: 'CONNECTED',
        systematicsMetrics: expect.objectContaining({
          cicsTransactions: 100,
          mainframeConnections: 5
        }),
        sessionStatus: expect.objectContaining({
          sessionId: 'SESS_001',
          isActive: true
        }),
        mainframeStatus: expect.objectContaining({
          connected: true,
          cicsRegion: expect.any(String)
        })
      });
    });

    test('should provide health status', async () => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
      mockHttpClient.mockResolvedValue({ status: 200 });

      const health = await connector.getHealthStatus();

      expect(health).toEqual({
        status: 'healthy',
        details: {
          mainframe: 'connected',
          cics: 'active',
          session: 'active',
          batchProcessor: 'ready'
        },
        timestamp: expect.any(String)
      });
    });
  });

  describe('Cleanup and Resource Management', () => {
    test('should cleanup resources properly', async () => {
      connector.sessionId = 'SESS_001';
      connector.sessionExpiry = Date.now() + 3600000;
      connector.isConnected = true;
      
      mockHttpClient.mockResolvedValue({ data: { status: 'DISCONNECTED' } });

      await connector.cleanup();

      expect(connector.sessionId).toBeNull();
      expect(connector.isConnected).toBe(false);
      expect(connector.mainframeConnection.connected).toBe(false);
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

describe('Systematics Constants and Exports', () => {
  test('should export all required endpoints', () => {
    expect(SYSTEMATICS_ENDPOINTS).toBeDefined();
    expect(SYSTEMATICS_ENDPOINTS.SESSION_CREATE).toBe('/session/create');
    expect(SYSTEMATICS_ENDPOINTS.CICS_EXECUTE).toBe('/cics/execute');
    expect(SYSTEMATICS_ENDPOINTS.MAINFRAME_CONNECT).toBe('/mainframe/connect');
    expect(SYSTEMATICS_ENDPOINTS.BATCH_SUBMIT).toBe('/batch/submit');
    expect(SYSTEMATICS_ENDPOINTS.COBOL_CALL).toBe('/cobol/call');
    expect(SYSTEMATICS_ENDPOINTS.OFAC_SCREEN).toBe('/compliance/ofac');
  });

  test('should export transaction types', () => {
    expect(SYSTEMATICS_TRANSACTION_TYPES).toBeDefined();
    expect(SYSTEMATICS_TRANSACTION_TYPES.DEBIT).toBe('DEBIT');
    expect(SYSTEMATICS_TRANSACTION_TYPES.CREDIT).toBe('CREDIT');
    expect(SYSTEMATICS_TRANSACTION_TYPES.TRANSFER).toBe('TRANSFER');
    expect(SYSTEMATICS_TRANSACTION_TYPES.INQUIRY).toBe('INQUIRY');
  });

  test('should export record layouts', () => {
    expect(SYSTEMATICS_RECORD_LAYOUTS).toBeDefined();
    expect(SYSTEMATICS_RECORD_LAYOUTS.ACCOUNT_RECORD).toBeDefined();
    expect(SYSTEMATICS_RECORD_LAYOUTS.TRANSACTION_RECORD).toBeDefined();
    expect(SYSTEMATICS_RECORD_LAYOUTS.CUSTOMER_RECORD).toBeDefined();
  });
});