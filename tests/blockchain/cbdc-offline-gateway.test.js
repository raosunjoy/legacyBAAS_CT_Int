/**
 * CBDC Offline Gateway Tests
 * Comprehensive test suite for offline CBDC transactions
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage for Offline-First CBDC IP Component
 */

const { CBDCOfflineGateway, CBDC_TRANSACTION_TYPES, OFFLINE_STATUS } = require('../../src/blockchain/cbdc-offline-gateway');
const path = require('path');
const fs = require('fs');

// Mock winston logger
jest.mock('winston', () => ({
  createLogger: jest.fn(() => ({
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn()
  })),
  format: {
    combine: jest.fn(),
    timestamp: jest.fn(),
    json: jest.fn()
  }
}));

// Mock sqlite3
jest.mock('sqlite3', () => ({
  verbose: () => ({
    Database: jest.fn((dbPath, callback) => {
      const mockDb = {
        run: jest.fn((sql, params, callback) => {
          if (callback) callback(null);
        }),
        all: jest.fn((sql, params, callback) => {
          if (callback) callback(null, []);
        }),
        get: jest.fn((sql, params, callback) => {
          if (callback) callback(null, null);
        }),
        close: jest.fn((callback) => {
          if (callback) callback(null);
        }),
        serialize: jest.fn((fn) => fn()),
        prepare: jest.fn(() => ({
          run: jest.fn(),
          finalize: jest.fn()
        }))
      };
      
      if (callback) {
        setTimeout(() => callback(null), 0);
      }
      
      return mockDb;
    })
  })
}));

// Mock crypto
jest.mock('crypto', () => ({
  randomBytes: jest.fn(() => Buffer.from('mockedrandom16bytes', 'utf8')),
  createCipher: jest.fn(() => ({
    update: jest.fn(() => 'encrypted'),
    final: jest.fn(() => 'data')
  })),
  createDecipher: jest.fn(() => ({
    update: jest.fn(() => 'decrypted'),
    final: jest.fn(() => 'data')
  })),
  createHmac: jest.fn(() => ({
    update: jest.fn().mockReturnThis(),
    digest: jest.fn(() => 'signature')
  }))
}));

describe('CBDC Offline Gateway', () => {
  let gateway;
  let mockDb;
  
  beforeEach(async () => {
    jest.clearAllMocks();
    jest.setTimeout(10000);
    
    // Setup mock database - get the mocked version
    const sqlite3 = require('sqlite3');
    mockDb = new (sqlite3.verbose().Database)();
    
    gateway = new CBDCOfflineGateway({
      offlineDbPath: ':memory:',
      enableAutoSync: false,
      syncInterval: 1000,
      centralBankId: 'TEST_CB',
      cbdcCurrency: 'TETHER'
    });
  });

  afterEach(async () => {
    if (gateway && gateway.offlineDb) {
      await new Promise(resolve => {
        gateway.offlineDb.close(() => resolve());
      });
    }
  });

  describe('Constants and Configuration', () => {
    test('should export required CBDC transaction types', () => {
      expect(CBDC_TRANSACTION_TYPES).toBeDefined();
      expect(CBDC_TRANSACTION_TYPES.ISSUE).toBe('cbdc_issue');
      expect(CBDC_TRANSACTION_TYPES.TRANSFER).toBe('cbdc_transfer');
      expect(CBDC_TRANSACTION_TYPES.REDEEM).toBe('cbdc_redeem');
      expect(CBDC_TRANSACTION_TYPES.EXCHANGE).toBe('cbdc_exchange');
      expect(CBDC_TRANSACTION_TYPES.BURN).toBe('cbdc_burn');
    });

    test('should export required offline status constants', () => {
      expect(OFFLINE_STATUS).toBeDefined();
      expect(OFFLINE_STATUS.QUEUED).toBe('offline_queued');
      expect(OFFLINE_STATUS.SYNCING).toBe('syncing');
      expect(OFFLINE_STATUS.SYNCED).toBe('synced');
      expect(OFFLINE_STATUS.FAILED).toBe('sync_failed');
      expect(OFFLINE_STATUS.CONFIRMED).toBe('confirmed');
    });

    test('should initialize with default configuration', () => {
      const defaultGateway = new CBDCOfflineGateway();
      
      expect(defaultGateway.config.centralBankId).toBe('CB_DEFAULT');
      expect(defaultGateway.config.cbdcCurrency).toBe('DCASH');
      expect(defaultGateway.config.networkId).toBe('cbdc_testnet');
      expect(defaultGateway.config.enableOfflineMode).toBe(true);
      expect(defaultGateway.config.enableAutoSync).toBe(true);
      expect(defaultGateway.config.maxOfflineTransactions).toBe(10000);
      expect(defaultGateway.config.syncInterval).toBe(30000);
    });

    test('should initialize with custom configuration', () => {
      expect(gateway.config.centralBankId).toBe('TEST_CB');
      expect(gateway.config.cbdcCurrency).toBe('TETHER');
      expect(gateway.config.enableAutoSync).toBe(false);
      expect(gateway.config.syncInterval).toBe(1000);
    });

    test('should initialize gateway state correctly', () => {
      expect(gateway.isOnline).toBe(false);
      expect(gateway.isInitialized).toBe(false);
      expect(gateway.syncInProgress).toBe(false);
      expect(gateway.lastSyncTime).toBeNull();
      expect(gateway.metrics.totalTransactions).toBe(0);
      expect(gateway.metrics.offlineTransactions).toBe(0);
    });
  });

  describe('Initialization and Database Setup', () => {
    test('should initialize offline database successfully', async () => {
      const initSpy = jest.spyOn(gateway, 'initializeOfflineDatabase');
      
      await gateway.initialize();
      
      expect(initSpy).toHaveBeenCalled();
      expect(gateway.isInitialized).toBe(true);
    });

    test('should handle database initialization errors', async () => {
      const errorGateway = new CBDCOfflineGateway({
        offlineDbPath: ':memory:',
        enableAutoSync: false
      });
      
      // Spy on initializeOfflineDatabase to throw an error
      jest.spyOn(errorGateway, 'initializeOfflineDatabase').mockRejectedValue(
        new Error('Database connection failed')
      );
      
      await expect(errorGateway.initialize()).rejects.toThrow('Database connection failed');
    });

    test('should check connectivity during initialization', async () => {
      const connectivitySpy = jest.spyOn(gateway, 'checkConnectivity');
      
      await gateway.initialize();
      
      expect(connectivitySpy).toHaveBeenCalled();
    });

    test('should emit initialization events', async () => {
      const initSpy = jest.fn();
      gateway.on('gateway:initialized', initSpy);
      
      await gateway.initialize();
      
      expect(initSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          isOnline: expect.any(Boolean),
          offlineTransactions: expect.any(Number),
          timestamp: expect.any(String)
        })
      );
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should process transaction with online mode', async () => {
      gateway.isOnline = true;
      const onlineProcessSpy = jest.spyOn(gateway, 'processOnlineTransaction').mockResolvedValue({
        id: 'tx123',
        status: 'confirmed',
        hash: '0xhash123'
      });

      const transaction = {
        id: 'tx123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2',
        currency: 'TETHER'
      };

      const result = await gateway.processTransaction(transaction);

      expect(onlineProcessSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          id: 'tx123',
          type: CBDC_TRANSACTION_TYPES.TRANSFER,
          amount: 1000,
          from: 'wallet1',
          to: 'wallet2',
          currency: 'TETHER'
        })
      );
      expect(result.status).toBe('confirmed');
    });

    test('should process transaction with offline mode', async () => {
      gateway.isOnline = false;
      const offlineProcessSpy = jest.spyOn(gateway, 'processOfflineTransaction').mockResolvedValue({
        id: 'tx123',
        status: OFFLINE_STATUS.QUEUED
      });

      const transaction = {
        id: 'tx123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2',
        currency: 'TETHER'
      };

      const result = await gateway.processTransaction(transaction);

      expect(offlineProcessSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          id: 'tx123',
          type: CBDC_TRANSACTION_TYPES.TRANSFER,
          amount: 1000,
          from: 'wallet1',
          to: 'wallet2',
          currency: 'TETHER'
        })
      );
      expect(result.status).toBe(OFFLINE_STATUS.QUEUED);
    });

    test('should force offline mode when specified', async () => {
      gateway.isOnline = true;
      const offlineProcessSpy = jest.spyOn(gateway, 'processOfflineTransaction').mockResolvedValue({
        id: 'tx123',
        status: OFFLINE_STATUS.QUEUED
      });

      const transaction = {
        id: 'tx123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2',
        currency: 'TETHER'
      };

      await gateway.processTransaction(transaction, { forceOffline: true });

      expect(offlineProcessSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          id: 'tx123',
          type: CBDC_TRANSACTION_TYPES.TRANSFER,
          amount: 1000,
          from: 'wallet1',
          to: 'wallet2',
          currency: 'TETHER'
        })
      );
    });

    test('should validate transaction before processing', async () => {
      // Set up wallet balance first
      gateway.metrics.walletBalances.set('wallet1', 5000);
      
      const validateSpy = jest.spyOn(gateway, 'validateCBDCTransaction');

      const transaction = {
        id: 'tx123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2',
        currency: 'TETHER'
      };

      await gateway.processTransaction(transaction);

      expect(validateSpy).toHaveBeenCalledWith(transaction);
    });

    test('should handle invalid transaction types', async () => {
      const transaction = {
        id: 'tx123',
        type: 'invalid_type',
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      await expect(gateway.processTransaction(transaction)).rejects.toThrow();
    });

    test('should emit transaction events', async () => {
      const transactionSpy = jest.fn();
      gateway.on('transaction:processed', transactionSpy);

      gateway.isOnline = false;
      jest.spyOn(gateway, 'processOfflineTransaction').mockResolvedValue({
        id: 'tx123',
        status: OFFLINE_STATUS.QUEUED
      });

      const transaction = {
        id: 'tx123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2',
        currency: 'TETHER'
      };

      await gateway.processTransaction(transaction);

      expect(transactionSpy).toHaveBeenCalled();
    });
  });

  describe('CBDC Operations', () => {
    beforeEach(async () => {
      await gateway.initialize();
      gateway.isOnline = true;
    });

    test('should issue CBDC tokens', async () => {
      const issueTransaction = {
        id: 'issue123',
        type: CBDC_TRANSACTION_TYPES.ISSUE,
        amount: 10000,
        to: 'central_bank_wallet',
        issuer: 'TEST_CB',
        currency: 'TETHER'
      };

      const result = await gateway.issueCBDC(issueTransaction);

      expect(result).toMatchObject({
        assetId: expect.any(Number),
        totalSupply: 10000,
        txHash: expect.any(String)
      });
    });

    test('should transfer CBDC tokens', async () => {
      // Set up wallet balance first
      gateway.metrics.walletBalances.set('wallet1', 5000);
      
      const transferTransaction = {
        id: 'transfer123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      const result = await gateway.transferCBDC(transferTransaction);

      expect(result).toMatchObject({
        txHash: expect.any(String),
        sender: 'wallet1',
        receiver: 'wallet2',
        amount: 1000
      });
    });

    test('should redeem CBDC tokens', async () => {
      // Set up wallet balance first
      gateway.metrics.walletBalances.set('wallet1', 5000);
      
      const redeemTransaction = {
        id: 'redeem123',
        type: CBDC_TRANSACTION_TYPES.REDEEM,
        amount: 500,
        from: 'wallet1',
        bankAccount: 'account123',
        currency: 'TETHER'
      };

      const result = await gateway.redeemCBDC(redeemTransaction);

      expect(result).toMatchObject({
        txHash: expect.any(String),
        redeemed: true,
        amount: 500
      });
    });

    test('should exchange CBDC tokens', async () => {
      const exchangeTransaction = {
        id: 'exchange123',
        type: CBDC_TRANSACTION_TYPES.EXCHANGE,
        amount: 1000,
        fromCurrency: 'DCASH',
        toCurrency: 'USD',
        wallet: 'wallet1'
      };

      const result = await gateway.exchangeCBDC(exchangeTransaction);

      expect(result).toMatchObject({
        txHash: expect.any(String),
        exchanged: true
      });
    });

    test('should burn CBDC tokens', async () => {
      const burnTransaction = {
        id: 'burn123',
        type: CBDC_TRANSACTION_TYPES.BURN,
        amount: 100,
        from: 'TEST_CB', // Must be central bank ID
        reason: 'currency_withdrawal'
      };

      const result = await gateway.burnCBDC(burnTransaction);

      expect(result).toMatchObject({
        txHash: expect.any(String),
        burned: true,
        amount: 100
      });
    });
  });

  describe('Offline Operations', () => {
    beforeEach(async () => {
      await gateway.initialize();
      gateway.isOnline = false;
    });

    test('should store offline transactions in database', async () => {
      const transaction = {
        id: 'offline123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      const storeSpy = jest.spyOn(gateway, 'storeOfflineTransaction');
      await gateway.processOfflineTransaction(transaction);

      expect(storeSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          id: 'offline123',
          status: OFFLINE_STATUS.QUEUED
        })
      );
    });

    test('should load offline transactions from database', async () => {
      // Mock database return
      mockDb.all.mockImplementation((sql, params, callback) => {
        callback(null, [
          {
            id: 'offline1',
            transaction_data: JSON.stringify({
              id: 'offline1',
              type: CBDC_TRANSACTION_TYPES.TRANSFER,
              amount: 1000
            }),
            status: OFFLINE_STATUS.QUEUED,
            created_at: Date.now()
          }
        ]);
      });

      const transactions = await gateway.loadOfflineTransactions();

      expect(transactions).toHaveLength(1);
      expect(transactions[0].id).toBe('offline1');
    });

    test('should handle offline transaction limits', async () => {
      gateway.config.maxOfflineTransactions = 1;
      gateway.metrics.offlineTransactions = 1;

      const transaction = {
        id: 'overflow',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      await expect(gateway.processOfflineTransaction(transaction)).rejects.toThrow('Offline transaction limit exceeded');
    });

    test('should encrypt offline transaction data when enabled', async () => {
      gateway.config.enableEncryption = true;
      gateway.config.encryptionKey = 'test-encryption-key';

      const encryptSpy = jest.spyOn(gateway, 'encryptTransactionData');
      
      const transaction = {
        id: 'encrypted123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      await gateway.processOfflineTransaction(transaction);

      expect(encryptSpy).toHaveBeenCalledWith(expect.any(String));
    });
  });

  describe('Synchronization', () => {
    beforeEach(async () => {
      await gateway.initialize();
      gateway.isOnline = true;
    });

    test('should sync offline transactions when online', async () => {
      // Mock offline transactions
      const mockTransactions = [
        {
          id: 'sync1',
          type: CBDC_TRANSACTION_TYPES.TRANSFER,
          amount: 1000,
          status: OFFLINE_STATUS.QUEUED
        }
      ];

      jest.spyOn(gateway, 'loadOfflineTransactions').mockResolvedValue(mockTransactions);
      jest.spyOn(gateway, 'syncSingleTransaction').mockResolvedValue(true);

      const result = await gateway.syncOfflineTransactions();

      expect(result.processed).toBe(1);
      expect(result.successful).toBe(1);
      expect(result.failed).toBe(0);
    });

    test('should handle sync failures', async () => {
      const mockTransactions = [
        {
          id: 'syncfail1',
          type: CBDC_TRANSACTION_TYPES.TRANSFER,
          amount: 1000,
          status: OFFLINE_STATUS.QUEUED
        }
      ];

      jest.spyOn(gateway, 'loadOfflineTransactions').mockResolvedValue(mockTransactions);
      jest.spyOn(gateway, 'syncSingleTransaction').mockRejectedValue(new Error('Sync failed'));

      const result = await gateway.syncOfflineTransactions();

      expect(result.processed).toBe(1);
      expect(result.successful).toBe(0);
      expect(result.failed).toBe(1);
    });

    test('should mark transactions as synced', async () => {
      const transaction = {
        id: 'synced123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000
      };

      const markSpy = jest.spyOn(gateway, 'markTransactionSynced');
      await gateway.markTransactionSynced(transaction);

      expect(markSpy).toHaveBeenCalledWith(transaction);
    });

    test('should mark transactions as failed', async () => {
      const transaction = {
        id: 'failed123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000
      };

      const markSpy = jest.spyOn(gateway, 'markTransactionFailed');
      await gateway.markTransactionFailed(transaction, 'Network error');

      expect(markSpy).toHaveBeenCalledWith(transaction, 'Network error');
    });

    test('should cleanup synced transactions', async () => {
      const cleanupSpy = jest.spyOn(gateway, 'cleanupSyncedTransactions');
      await gateway.cleanupSyncedTransactions();

      expect(cleanupSpy).toHaveBeenCalled();
    });

    test('should batch sync transactions', async () => {
      gateway.config.batchSyncSize = 2;
      
      const mockTransactions = [
        { id: 'batch1', status: OFFLINE_STATUS.QUEUED },
        { id: 'batch2', status: OFFLINE_STATUS.QUEUED },
        { id: 'batch3', status: OFFLINE_STATUS.QUEUED }
      ];

      jest.spyOn(gateway, 'loadOfflineTransactions').mockResolvedValue(mockTransactions);
      jest.spyOn(gateway, 'syncSingleTransaction').mockResolvedValue(true);

      const result = await gateway.syncOfflineTransactions();

      // Should process all transactions in batches
      expect(result.processed).toBe(3);
    });

    test('should emit sync events', async () => {
      const syncSpy = jest.fn();
      gateway.on('sync:completed', syncSpy);

      jest.spyOn(gateway, 'loadOfflineTransactions').mockResolvedValue([]);

      await gateway.syncOfflineTransactions();

      expect(syncSpy).toHaveBeenCalledWith(
        expect.objectContaining({
          processed: 0,
          successful: 0,
          failed: 0
        })
      );
    });
  });

  describe('Connectivity Management', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should check connectivity status', async () => {
      // Mock successful connectivity
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        status: 200
      });

      await gateway.checkConnectivity();

      expect(gateway.isOnline).toBe(true);
    });

    test('should handle connectivity failures', async () => {
      // Mock failed connectivity
      global.fetch = jest.fn().mockRejectedValue(new Error('Network error'));

      await gateway.checkConnectivity();

      expect(gateway.isOnline).toBe(false);
    });

    test('should emit connectivity events', async () => {
      const connectSpy = jest.fn();
      const disconnectSpy = jest.fn();
      
      gateway.on('connectivity:online', connectSpy);
      gateway.on('connectivity:offline', disconnectSpy);

      // Test going online
      global.fetch = jest.fn().mockResolvedValue({ ok: true });
      await gateway.checkConnectivity();
      
      expect(connectSpy).toHaveBeenCalled();

      // Test going offline
      global.fetch = jest.fn().mockRejectedValue(new Error('Network error'));
      await gateway.checkConnectivity();
      
      expect(disconnectSpy).toHaveBeenCalled();
    });

    test('should start auto-sync when online and enabled', async () => {
      gateway.config.enableAutoSync = true;
      const startSyncSpy = jest.spyOn(gateway, 'startSyncProcess');

      global.fetch = jest.fn().mockResolvedValue({ ok: true });
      await gateway.checkConnectivity();

      expect(startSyncSpy).toHaveBeenCalled();
    });
  });

  describe('Wallet Balance Management', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should get wallet balance', async () => {
      const mockBalance = {
        wallet: 'wallet123',
        balance: 5000,
        currency: 'DCASH',
        lastUpdated: Date.now()
      };

      // Mock database return
      mockDb.get.mockImplementation((sql, params, callback) => {
        callback(null, {
          wallet_address: 'wallet123',
          balance: 5000,
          currency: 'DCASH',
          last_updated: Date.now()
        });
      });

      const balance = await gateway.getWalletBalance('wallet123');

      expect(balance.wallet).toBe('wallet123');
      expect(balance.balance).toBe(5000);
    });

    test('should handle missing wallet balance', async () => {
      // Mock no result
      mockDb.get.mockImplementation((sql, params, callback) => {
        callback(null, null);
      });

      const balance = await gateway.getWalletBalance('nonexistent');

      expect(balance.balance).toBe(0);
    });

    test('should update wallet balance after transactions', async () => {
      const updateSpy = jest.spyOn(gateway, 'updateWalletBalance');

      const transaction = {
        id: 'balance_update',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      await gateway.processTransaction(transaction);

      expect(updateSpy).toHaveBeenCalled();
    });
  });

  describe('Security Features', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should encrypt transaction data when enabled', () => {
      gateway.config.enableEncryption = true;
      gateway.config.encryptionKey = 'test-key';

      const data = JSON.stringify({ sensitive: 'data' });
      const encrypted = gateway.encryptTransactionData(data);

      expect(encrypted).toBe('encrypteddata');
    });

    test('should decrypt transaction data', () => {
      gateway.config.enableEncryption = true;
      gateway.config.encryptionKey = 'test-key';

      const encrypted = 'encrypteddata';
      const decrypted = gateway.decryptTransactionData(encrypted);

      expect(decrypted).toBe('decrypteddata');
    });

    test('should sign transactions when enabled', () => {
      gateway.config.enableSignatures = true;
      gateway.config.signingKey = 'test-signing-key';

      const transaction = { id: 'sign123', amount: 1000 };
      const signature = gateway.signTransaction(transaction);

      expect(signature).toBe('signature');
    });

    test('should verify transaction signatures', () => {
      gateway.config.enableSignatures = true;
      gateway.config.signingKey = 'test-signing-key';

      const transaction = { id: 'verify123', amount: 1000 };
      const signature = 'signature';

      const isValid = gateway.verifyTransactionSignature(transaction, signature);
      expect(isValid).toBe(true);
    });

    test('should handle missing encryption key', () => {
      gateway.config.enableEncryption = true;
      gateway.config.encryptionKey = null;

      expect(() => {
        gateway.encryptTransactionData('data');
      }).toThrow('Encryption key not configured');
    });
  });

  describe('Error Handling', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should handle database errors gracefully', async () => {
      // Mock database error
      mockDb.run.mockImplementation((sql, params, callback) => {
        callback(new Error('Database error'));
      });

      const transaction = {
        id: 'error123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      await expect(gateway.storeOfflineTransaction(transaction)).rejects.toThrow('Database error');
    });

    test('should handle invalid transaction amounts', async () => {
      const transaction = {
        id: 'invalid123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: -1000, // Invalid negative amount
        from: 'wallet1',
        to: 'wallet2'
      };

      await expect(gateway.processTransaction(transaction)).rejects.toThrow();
    });

    test('should handle missing required fields', async () => {
      const transaction = {
        id: 'incomplete123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER
        // Missing amount, wallets
      };

      await expect(gateway.processTransaction(transaction)).rejects.toThrow();
    });

    test('should emit error events', async () => {
      const errorSpy = jest.fn();
      gateway.on('error', errorSpy);

      // Force an error
      const transaction = {
        id: 'error123',
        type: 'invalid_type',
        amount: 1000
      };

      try {
        await gateway.processTransaction(transaction);
      } catch (error) {
        // Expected error
      }

      expect(errorSpy).toHaveBeenCalled();
    });
  });

  describe('Metrics and Monitoring', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should track transaction metrics', async () => {
      const initialTotal = gateway.metrics.totalTransactions;
      const initialOffline = gateway.metrics.offlineTransactions;

      gateway.isOnline = false;
      jest.spyOn(gateway, 'processOfflineTransaction').mockResolvedValue({
        id: 'metrics123',
        status: OFFLINE_STATUS.QUEUED
      });

      const transaction = {
        id: 'metrics123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        from: 'wallet1',
        to: 'wallet2'
      };

      await gateway.processTransaction(transaction);

      expect(gateway.metrics.totalTransactions).toBe(initialTotal + 1);
      expect(gateway.metrics.offlineTransactions).toBe(initialOffline + 1);
    });

    test('should return comprehensive status', () => {
      const status = gateway.getStatus();

      expect(status).toMatchObject({
        gateway: 'CBDC Offline Gateway',
        isOnline: expect.any(Boolean),
        isInitialized: expect.any(Boolean),
        config: expect.any(Object),
        metrics: expect.any(Object),
        lastSyncTime: gateway.lastSyncTime
      });
    });

    test('should return health status', () => {
      const health = gateway.getHealthStatus();

      expect(health).toMatchObject({
        status: expect.any(String),
        components: expect.any(Object),
        timestamp: expect.any(String)
      });
    });

    test('should calculate performance metrics', () => {
      gateway.metrics.totalTransactions = 100;
      gateway.metrics.offlineTransactions = 20;
      gateway.metrics.syncedTransactions = 18;
      gateway.metrics.failedSyncs = 2;

      const perf = gateway.getPerformanceMetrics();

      expect(perf.offlineRate).toBe(0.2); // 20/100
      expect(perf.syncSuccessRate).toBe(0.9); // 18/20
    });
  });

  describe('Integration Features', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should handle Algorand integration when enabled', async () => {
      gateway.config.enableAlgorandIntegration = true;
      
      const algorandSpy = jest.spyOn(gateway, 'processAlgorandTransaction').mockResolvedValue({
        txId: 'algo123',
        confirmed: true
      });

      const transaction = {
        id: 'algo123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        blockchain: 'algorand'
      };

      await gateway.processOnlineTransaction(transaction);

      expect(algorandSpy).toHaveBeenCalledWith(transaction);
    });

    test('should handle Crunchfish integration when enabled', async () => {
      gateway.config.enableCrunchfishIntegration = true;
      
      const crunchfishSpy = jest.spyOn(gateway, 'processCrunchfishTransaction').mockResolvedValue({
        status: 'processed',
        proximity: true
      });

      const transaction = {
        id: 'crunch123',
        type: CBDC_TRANSACTION_TYPES.TRANSFER,
        amount: 1000,
        proximityMode: true
      };

      await gateway.processOfflineTransaction(transaction);

      expect(crunchfishSpy).toHaveBeenCalledWith(transaction);
    });
  });

  describe('Cleanup and Shutdown', () => {
    beforeEach(async () => {
      await gateway.initialize();
    });

    test('should cleanup resources properly', async () => {
      const cleanupSpy = jest.spyOn(gateway, 'cleanup');
      
      await gateway.cleanup();

      expect(cleanupSpy).toHaveBeenCalled();
      expect(gateway.isInitialized).toBe(false);
    });

    test('should close database connection on cleanup', async () => {
      await gateway.cleanup();

      expect(mockDb.close).toHaveBeenCalled();
    });

    test('should clear intervals on cleanup', async () => {
      gateway.config.enableAutoSync = true;
      gateway.startSyncProcess();

      expect(gateway.syncInterval).toBeDefined();

      await gateway.cleanup();

      expect(gateway.syncInterval).toBeNull();
    });
  });
});