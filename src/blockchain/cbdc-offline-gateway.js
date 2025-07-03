/**
 * CBDC Offline Gateway
 * Central Bank Digital Currency with offline transaction support
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Offline-first CBDC implementation with sync capabilities
 */

const winston = require('winston');
const sqlite3 = require('sqlite3').verbose();
const { v4: uuidv4 } = require('uuid');
const crypto = require('crypto');
const EventEmitter = require('events');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'cbdc-offline-gateway' }
});

/**
 * CBDC Transaction Types
 */
const CBDC_TRANSACTION_TYPES = {
  ISSUE: 'cbdc_issue',
  TRANSFER: 'cbdc_transfer',
  REDEEM: 'cbdc_redeem',
  EXCHANGE: 'cbdc_exchange',
  BURN: 'cbdc_burn'
};

/**
 * Transaction Status for Offline Operations
 */
const OFFLINE_STATUS = {
  QUEUED: 'offline_queued',
  SYNCING: 'syncing',
  SYNCED: 'synced',
  FAILED: 'sync_failed',
  CONFIRMED: 'confirmed'
};

/**
 * CBDC Offline Gateway Class
 * Supports offline transactions with automatic sync when connectivity is restored
 */
class CBDCOfflineGateway extends EventEmitter {
  constructor(config = {}) {
    super();

    this.config = {
      // CBDC configuration
      centralBankId: config.centralBankId || 'CB_DEFAULT',
      cbdcCurrency: config.cbdcCurrency || 'DCASH',
      networkId: config.networkId || 'cbdc_testnet',
      
      // Offline configuration
      enableOfflineMode: config.enableOfflineMode !== false,
      offlineDbPath: config.offlineDbPath || './cbdc_offline.db',
      maxOfflineTransactions: config.maxOfflineTransactions || 10000,
      offlineTimeout: config.offlineTimeout || 24 * 60 * 60 * 1000, // 24 hours
      
      // Sync configuration
      syncInterval: config.syncInterval || 30000, // 30 seconds
      batchSyncSize: config.batchSyncSize || 100,
      enableAutoSync: config.enableAutoSync !== false,
      
      // Security configuration
      enableEncryption: config.enableEncryption !== false,
      encryptionKey: config.encryptionKey || process.env.CBDC_ENCRYPTION_KEY,
      enableSignatures: config.enableSignatures !== false,
      signingKey: config.signingKey || process.env.CBDC_SIGNING_KEY,
      
      // Integration configuration
      algorandNodeUrl: config.algorandNodeUrl || 'https://testnet-api.algonode.cloud',
      algorandIndexerUrl: config.algorandIndexerUrl || 'https://testnet-idx.algonode.cloud',
      enableCrunchfishIntegration: config.enableCrunchfishIntegration || false,
      
      ...config
    };

    // Gateway state
    this.isOnline = false;
    this.isInitialized = false;
    this.offlineDb = null;
    this.syncInProgress = false;
    this.lastSyncTime = null;
    
    // Performance metrics
    this.metrics = {
      totalTransactions: 0,
      offlineTransactions: 0,
      syncedTransactions: 0,
      failedSyncs: 0,
      averageOfflineTime: 0,
      walletBalances: new Map(),
      transactionVolume: new Map(),
      issuanceTotal: 0,
      circulatingSupply: 0
    };

    // Transaction queues
    this.offlineQueue = [];
    this.syncQueue = [];
    this.pendingTransactions = new Map();

    // Initialize gateway
    this.initialize();

    logger.info('CBDC Offline Gateway initialized', {
      centralBankId: this.config.centralBankId,
      cbdcCurrency: this.config.cbdcCurrency,
      offlineMode: this.config.enableOfflineMode
    });
  }

  /**
   * Initialize the CBDC gateway
   */
  async initialize() {
    try {
      // Initialize offline database
      await this.initializeOfflineDatabase();
      
      // Check network connectivity
      await this.checkConnectivity();
      
      // Start sync process if online
      if (this.isOnline && this.config.enableAutoSync) {
        this.startSyncProcess();
      }

      this.isInitialized = true;
      
      this.emit('gateway:initialized', {
        isOnline: this.isOnline,
        offlineTransactions: this.offlineQueue.length,
        timestamp: new Date().toISOString()
      });

      logger.info('CBDC Gateway initialization completed', {
        isOnline: this.isOnline,
        offlineMode: this.config.enableOfflineMode
      });

    } catch (error) {
      logger.error('CBDC Gateway initialization failed', { error: error.message });
      throw error;
    }
  }

  /**
   * Process CBDC transaction (online or offline)
   * @param {Object} transaction - CBDC transaction details
   * @param {Object} options - Processing options
   * @returns {Promise<Object>} Transaction result
   */
  async processTransaction(transaction, options = {}) {
    const transactionId = transaction.id || uuidv4();
    const startTime = Date.now();

    try {
      // Validate transaction
      this.validateCBDCTransaction(transaction);

      // Add metadata
      const enrichedTransaction = {
        ...transaction,
        id: transactionId,
        timestamp: new Date().toISOString(),
        gateway: 'cbdc_offline',
        currency: this.config.cbdcCurrency,
        centralBank: this.config.centralBankId,
        networkId: this.config.networkId
      };

      let result;

      if (this.isOnline && !options.forceOffline) {
        // Process online
        result = await this.processOnlineTransaction(enrichedTransaction);
      } else {
        // Process offline
        result = await this.processOfflineTransaction(enrichedTransaction);
      }

      // Update metrics
      this.updateMetrics(enrichedTransaction, result, Date.now() - startTime);

      this.emit('transaction:processed', {
        transactionId,
        type: enrichedTransaction.type,
        mode: this.isOnline && !options.forceOffline ? 'online' : 'offline',
        result
      });

      logger.info('CBDC transaction processed', {
        transactionId,
        type: enrichedTransaction.type,
        amount: enrichedTransaction.amount,
        mode: result.mode
      });

      return result;

    } catch (error) {
      logger.error('CBDC transaction processing failed', {
        transactionId,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Process transaction online
   * @param {Object} transaction - Transaction to process
   * @returns {Promise<Object>} Online processing result
   */
  async processOnlineTransaction(transaction) {
    try {
      let blockchainResult;

      // Handle Algorand integration if enabled and specified
      if (this.config.enableAlgorandIntegration && transaction.blockchain === 'algorand') {
        blockchainResult = await this.processAlgorandTransaction(transaction);
      } else {
        switch (transaction.type) {
          case CBDC_TRANSACTION_TYPES.ISSUE:
            blockchainResult = await this.issueCBDC(transaction);
            break;
            
          case CBDC_TRANSACTION_TYPES.TRANSFER:
            blockchainResult = await this.transferCBDC(transaction);
            break;
            
          case CBDC_TRANSACTION_TYPES.REDEEM:
            blockchainResult = await this.redeemCBDC(transaction);
            break;
            
          case CBDC_TRANSACTION_TYPES.EXCHANGE:
            blockchainResult = await this.exchangeCBDC(transaction);
            break;
            
          case CBDC_TRANSACTION_TYPES.BURN:
            blockchainResult = await this.burnCBDC(transaction);
            break;
            
          default:
            throw new Error(`Unsupported CBDC transaction type: ${transaction.type}`);
        }
      }

      return {
        transactionId: transaction.id,
        status: 'confirmed',
        mode: 'online',
        blockchainTxHash: blockchainResult.txHash || blockchainResult.txId,
        blockNumber: blockchainResult.blockNumber,
        confirmations: blockchainResult.confirmations || (blockchainResult.confirmed ? 1 : 0),
        fees: blockchainResult.fees || 0,
        timestamp: new Date().toISOString(),
        processingTime: Date.now() - new Date(transaction.timestamp).getTime()
      };

    } catch (error) {
      throw new Error(`Online CBDC processing failed: ${error.message}`);
    }
  }

  /**
   * Process transaction offline
   * @param {Object} transaction - Transaction to process offline
   * @returns {Promise<Object>} Offline processing result
   */
  async processOfflineTransaction(transaction) {
    try {
      // Check offline transaction limits
      if (this.metrics.offlineTransactions >= this.config.maxOfflineTransactions) {
        throw new Error('Offline transaction limit exceeded');
      }

      // Handle Crunchfish integration if enabled and proximity mode specified
      if (this.config.enableCrunchfishIntegration && transaction.proximityMode) {
        await this.processCrunchfishTransaction(transaction);
      }

      // Generate offline transaction record
      const offlineRecord = {
        ...transaction,
        offlineId: uuidv4(),
        status: OFFLINE_STATUS.QUEUED,
        createdAt: new Date().toISOString(),
        signature: this.config.enableSignatures ? this.signTransaction(transaction) : null,
        encrypted: this.config.enableEncryption ? this.encryptTransaction(transaction) : null
      };

      // Store in offline database
      await this.storeOfflineTransaction(offlineRecord);
      
      // Add to offline queue
      this.offlineQueue.push(offlineRecord);
      
      // Update local wallet balances (optimistic)
      this.updateLocalBalances(transaction);

      // Emit offline event
      this.emit('transaction:offline', {
        transactionId: transaction.id,
        offlineId: offlineRecord.offlineId,
        queuePosition: this.offlineQueue.length
      });

      return {
        transactionId: transaction.id,
        offlineId: offlineRecord.offlineId,
        status: OFFLINE_STATUS.QUEUED,
        mode: 'offline',
        queuePosition: this.offlineQueue.length,
        estimatedSyncTime: this.estimateSyncTime(),
        localConfirmation: true,
        timestamp: new Date().toISOString()
      };

    } catch (error) {
      throw new Error(`Offline CBDC processing failed: ${error.message}`);
    }
  }

  /**
   * Issue new CBDC tokens (Central Bank only)
   * @param {Object} transaction - Issuance transaction
   * @returns {Promise<Object>} Issuance result
   */
  async issueCBDC(transaction) {
    // Validate central bank authority
    if (transaction.issuer !== this.config.centralBankId) {
      throw new Error('Unauthorized CBDC issuance attempt');
    }

    // Simulate Algorand asset issuance
    const algorandTx = {
      type: 'acfg',
      from: transaction.issuer,
      assetName: this.config.cbdcCurrency,
      assetUnitName: 'CBDC',
      assetTotal: transaction.amount,
      assetDecimals: 2,
      assetDefaultFrozen: false,
      assetManager: transaction.issuer,
      assetReserve: transaction.issuer,
      assetFreeze: transaction.issuer,
      assetClawback: transaction.issuer
    };

    // Process on Algorand network
    const txHash = crypto.randomBytes(32).toString('hex');
    
    // Update metrics
    this.metrics.issuanceTotal += transaction.amount;
    this.metrics.circulatingSupply += transaction.amount;

    return {
      txHash,
      blockNumber: Math.floor(Math.random() * 1000000),
      assetId: Math.floor(Math.random() * 1000000),
      totalSupply: this.metrics.circulatingSupply,
      fees: 0.001
    };
  }

  /**
   * Transfer CBDC between wallets
   * @param {Object} transaction - Transfer transaction
   * @returns {Promise<Object>} Transfer result
   */
  async transferCBDC(transaction) {
    // Validate wallet balances
    const senderBalance = this.metrics.walletBalances.get(transaction.from) || 0;
    if (senderBalance < transaction.amount) {
      throw new Error('Insufficient CBDC balance');
    }

    // Simulate Algorand asset transfer
    const algorandTx = {
      type: 'axfer',
      from: transaction.from,
      to: transaction.to,
      amount: transaction.amount,
      assetIndex: transaction.assetId || 12345
    };

    const txHash = crypto.randomBytes(32).toString('hex');

    // Update local balances
    this.metrics.walletBalances.set(transaction.from, senderBalance - transaction.amount);
    this.metrics.walletBalances.set(
      transaction.to, 
      (this.metrics.walletBalances.get(transaction.to) || 0) + transaction.amount
    );

    return {
      txHash,
      sender: transaction.from,
      receiver: transaction.to,
      amount: transaction.amount,
      blockNumber: Math.floor(Math.random() * 1000000),
      fees: 0.001
    };
  }

  /**
   * Redeem CBDC for fiat currency
   * @param {Object} transaction - Redemption transaction
   * @returns {Promise<Object>} Redemption result
   */
  async redeemCBDC(transaction) {
    // Validate redemption request
    const holderBalance = this.metrics.walletBalances.get(transaction.from) || 0;
    if (holderBalance < transaction.amount) {
      throw new Error('Insufficient CBDC balance for redemption');
    }

    // Process redemption (burn CBDC, credit fiat)
    const txHash = crypto.randomBytes(32).toString('hex');

    // Update balances
    this.metrics.walletBalances.set(transaction.from, holderBalance - transaction.amount);
    this.metrics.circulatingSupply -= transaction.amount;

    return {
      txHash,
      redeemed: true,
      amount: transaction.amount,
      blockNumber: Math.floor(Math.random() * 1000000),
      redemptionAmount: transaction.amount,
      fiatCurrency: transaction.fiatCurrency || 'USD',
      fees: 0.001
    };
  }

  /**
   * Exchange CBDC for other digital currencies
   * @param {Object} transaction - Exchange transaction  
   * @returns {Promise<Object>} Exchange result
   */
  async exchangeCBDC(transaction) {
    // Simulate cross-currency exchange
    const exchangeRate = transaction.exchangeRate || 1.0;
    const outputAmount = transaction.amount * exchangeRate;

    const txHash = crypto.randomBytes(32).toString('hex');

    return {
      txHash,
      exchanged: true,
      blockNumber: Math.floor(Math.random() * 1000000),
      inputAmount: transaction.amount,
      inputCurrency: this.config.cbdcCurrency,
      outputAmount,
      outputCurrency: transaction.targetCurrency,
      exchangeRate,
      fees: transaction.amount * 0.001 // 0.1% fee
    };
  }

  /**
   * Burn CBDC tokens (Central Bank only)
   * @param {Object} transaction - Burn transaction
   * @returns {Promise<Object>} Burn result
   */
  async burnCBDC(transaction) {
    // Validate central bank authority
    if (transaction.from !== this.config.centralBankId) {
      throw new Error('Unauthorized CBDC burn attempt');
    }

    const txHash = crypto.randomBytes(32).toString('hex');
    
    // Update supply metrics
    this.metrics.circulatingSupply -= transaction.amount;

    return {
      txHash,
      burned: true,
      amount: transaction.amount,
      blockNumber: Math.floor(Math.random() * 1000000),
      burnedAmount: transaction.amount,
      remainingSupply: this.metrics.circulatingSupply,
      fees: 0
    };
  }

  /**
   * Sync offline transactions when connectivity is restored
   * @returns {Promise<Object>} Sync result
   */
  async syncOfflineTransactions() {
    if (this.syncInProgress) {
      logger.warn('Sync already in progress');
      return { status: 'already_syncing' };
    }

    this.syncInProgress = true;
    const syncStartTime = Date.now();

    try {
      logger.info('Starting offline transaction sync', {
        queuedTransactions: this.offlineQueue.length
      });

      // Load offline transactions from database
      const offlineTransactions = await this.loadOfflineTransactions();
      
      const syncResults = {
        totalTransactions: offlineTransactions.length,
        syncedTransactions: 0,
        failedTransactions: 0,
        errors: []
      };

      // Process transactions in batches
      for (let i = 0; i < offlineTransactions.length; i += this.config.batchSyncSize) {
        const batch = offlineTransactions.slice(i, i + this.config.batchSyncSize);
        
        const batchResults = await Promise.allSettled(
          batch.map(tx => this.syncSingleTransaction(tx))
        );

        for (const [index, result] of batchResults.entries()) {
          if (result.status === 'fulfilled') {
            syncResults.syncedTransactions++;
            await this.markTransactionSynced(batch[index]);
          } else {
            syncResults.failedTransactions++;
            syncResults.errors.push({
              transactionId: batch[index].id,
              error: result.reason.message
            });
            await this.markTransactionFailed(batch[index], result.reason.message);
          }
        }

        // Emit progress update
        this.emit('sync:progress', {
          processed: i + batch.length,
          total: offlineTransactions.length,
          synced: syncResults.syncedTransactions,
          failed: syncResults.failedTransactions
        });
      }

      // Clean up synced transactions
      await this.cleanupSyncedTransactions();
      
      // Update sync metrics
      this.lastSyncTime = new Date().toISOString();
      this.metrics.syncedTransactions += syncResults.syncedTransactions;
      this.metrics.failedSyncs += syncResults.failedTransactions;

      const syncDuration = Date.now() - syncStartTime;

      this.emit('sync:completed', {
        ...syncResults,
        syncDuration,
        timestamp: this.lastSyncTime
      });

      logger.info('Offline transaction sync completed', {
        ...syncResults,
        syncDuration
      });

      return {
        status: 'completed',
        processed: syncResults.totalTransactions,
        successful: syncResults.syncedTransactions,
        failed: syncResults.failedTransactions,
        errors: syncResults.errors,
        syncDuration
      };

    } catch (error) {
      logger.error('Offline transaction sync failed', { error: error.message });
      this.emit('sync:failed', { error: error.message });
      throw error;
    } finally {
      this.syncInProgress = false;
    }
  }

  /**
   * Check network connectivity
   * @returns {Promise<boolean>} Connectivity status
   */
  async checkConnectivity() {
    try {
      // Test connection to Algorand node
      const response = await fetch(`${this.config.algorandNodeUrl}/health`, {
        method: 'GET',
        timeout: 5000
      });

      const wasOnline = this.isOnline;
      this.isOnline = response.ok;

      if (this.isOnline !== wasOnline) {
        this.emit('connectivity:changed', {
          isOnline: this.isOnline,
          timestamp: new Date().toISOString()
        });

        if (this.isOnline) {
          this.emit('connected');
          if (this.offlineQueue.length > 0 && this.config.autoSync) {
            // Trigger sync when coming back online
            this.emit('auto-sync:start');
            setTimeout(() => this.syncOfflineTransactions(), 1000);
          }
        } else {
          this.emit('disconnected');
        }
      }

      this.lastConnectivityCheck = new Date().toISOString();
      return this.isOnline;

    } catch (error) {
      const wasOnline = this.isOnline;
      this.isOnline = false;
      
      // Emit disconnected event if state changed
      if (wasOnline !== this.isOnline) {
        this.emit('disconnected');
      }
      
      return false;
    }
  }

  /**
   * Get CBDC wallet balance
   * @param {string} walletAddress - Wallet address
   * @returns {Promise<Object>} Wallet balance information
   */
  async getWalletBalance(walletAddress) {
    try {
      if (this.isOnline) {
        // Fetch from blockchain
        const balance = await this.fetchOnChainBalance(walletAddress);
        this.metrics.walletBalances.set(walletAddress, balance);
        return {
          walletAddress,
          balance,
          currency: this.config.cbdcCurrency,
          source: 'blockchain',
          timestamp: new Date().toISOString()
        };
      } else {
        // Return local balance
        const balance = this.metrics.walletBalances.get(walletAddress) || 0;
        return {
          walletAddress,
          balance,
          currency: this.config.cbdcCurrency,
          source: 'local_cache',
          timestamp: new Date().toISOString(),
          warning: 'Offline mode - balance may not be current'
        };
      }
    } catch (error) {
      logger.error('Balance retrieval failed', { walletAddress, error: error.message });
      throw error;
    }
  }

  /**
   * Get gateway status and metrics
   * @returns {Object} Gateway status
   */
  getStatus() {
    return {
      gateway: 'CBDC Offline Gateway',
      isOnline: this.isOnline,
      isInitialized: this.isInitialized,
      syncInProgress: this.syncInProgress,
      lastSyncTime: this.lastSyncTime,
      offlineQueueSize: this.offlineQueue.length,
      metrics: {
        ...this.metrics,
        uptimeHours: this.isInitialized ? (Date.now() - this.metrics.initTime) / (1000 * 60 * 60) : 0
      },
      config: {
        centralBankId: this.config.centralBankId,
        cbdcCurrency: this.config.cbdcCurrency,
        networkId: this.config.networkId,
        offlineMode: this.config.enableOfflineMode
      }
    };
  }

  // ===== UTILITY METHODS =====

  async initializeOfflineDatabase() {
    return new Promise((resolve, reject) => {
      // Add timeout to prevent hanging during database initialization
      const timeout = setTimeout(() => {
        reject(new Error('Database initialization timeout'));
      }, 15000); // 15 second timeout for initialization

      this.offlineDb = new sqlite3.Database(this.config.offlineDbPath, (err) => {
        if (err) {
          clearTimeout(timeout);
          reject(new Error('Database connection failed: ' + err.message));
          return;
        }

        // Create tables
        this.offlineDb.serialize(() => {
          this.offlineDb.run(`
            CREATE TABLE IF NOT EXISTS offline_transactions (
              id TEXT PRIMARY KEY,
              offline_id TEXT UNIQUE,
              transaction_data TEXT,
              status TEXT,
              created_at TEXT,
              synced_at TEXT,
              error_message TEXT
            )
          `);

          this.offlineDb.run(`
            CREATE TABLE IF NOT EXISTS wallet_balances (
              wallet_address TEXT PRIMARY KEY,
              balance REAL,
              currency TEXT,
              updated_at TEXT
            )
          `);
        });

        clearTimeout(timeout);
        resolve();
      });
    });
  }

  async storeOfflineTransaction(transaction) {
    return new Promise((resolve, reject) => {
      // Check database state
      if (!this.offlineDb) {
        reject(new Error('Database not initialized'));
        return;
      }

      // Add timeout to prevent hanging
      const timeout = setTimeout(() => {
        reject(new Error('Database operation timeout'));
      }, 10000); // 10 second timeout

      const stmt = this.offlineDb.prepare(`
        INSERT INTO offline_transactions 
        (id, offline_id, transaction_data, status, created_at)
        VALUES (?, ?, ?, ?, ?)
      `);

      stmt.run(
        transaction.id,
        transaction.offlineId,
        JSON.stringify(transaction),
        transaction.status,
        transaction.createdAt,
        (err) => {
          // Clear timeout
          clearTimeout(timeout);
          
          // Always finalize the statement to free resources
          stmt.finalize((finalizeErr) => {
            if (finalizeErr) {
              logger.warn('Statement finalization failed', { error: finalizeErr.message });
            }
          });

          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  async loadOfflineTransactions() {
    return new Promise((resolve, reject) => {
      // Check database state
      if (!this.offlineDb) {
        reject(new Error('Database not initialized'));
        return;
      }

      // Add timeout to prevent hanging
      const timeout = setTimeout(() => {
        reject(new Error('Database operation timeout'));
      }, 10000); // 10 second timeout

      this.offlineDb.all(
        'SELECT * FROM offline_transactions WHERE status = ?',
        [OFFLINE_STATUS.QUEUED],
        (err, rows) => {
          // Clear timeout
          clearTimeout(timeout);
          
          if (err) {
            reject(err);
          } else {
            const transactions = rows.map(row => JSON.parse(row.transaction_data));
            resolve(transactions);
          }
        }
      );
    });
  }

  async syncSingleTransaction(transaction) {
    // Decrypt if necessary
    const decryptedTx = transaction.encrypted 
      ? this.decryptTransaction(transaction.encrypted)
      : transaction;

    // Verify signature if enabled
    if (this.config.enableSignatures && transaction.signature) {
      if (!this.verifyTransactionSignature(decryptedTx, transaction.signature)) {
        throw new Error('Transaction signature verification failed');
      }
    }

    // Process the transaction online
    return await this.processOnlineTransaction(decryptedTx);
  }

  async markTransactionSynced(transaction) {
    return new Promise((resolve, reject) => {
      // Check database state
      if (!this.offlineDb) {
        reject(new Error('Database not initialized'));
        return;
      }

      // Add timeout to prevent hanging
      const timeout = setTimeout(() => {
        reject(new Error('Database operation timeout'));
      }, 10000); // 10 second timeout

      this.offlineDb.run(
        'UPDATE offline_transactions SET status = ?, synced_at = ? WHERE id = ?',
        [OFFLINE_STATUS.SYNCED, new Date().toISOString(), transaction.id],
        (err) => {
          // Clear timeout
          clearTimeout(timeout);
          
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  async markTransactionFailed(transaction, errorMessage) {
    return new Promise((resolve, reject) => {
      // Check database state
      if (!this.offlineDb) {
        reject(new Error('Database not initialized'));
        return;
      }

      // Add timeout to prevent hanging
      const timeout = setTimeout(() => {
        reject(new Error('Database operation timeout'));
      }, 10000); // 10 second timeout

      this.offlineDb.run(
        'UPDATE offline_transactions SET status = ?, error_message = ? WHERE id = ?',
        [OFFLINE_STATUS.FAILED, errorMessage, transaction.id],
        (err) => {
          // Clear timeout
          clearTimeout(timeout);
          
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  async cleanupSyncedTransactions() {
    // Remove synced transactions older than retention period
    const cutoffDate = new Date(Date.now() - this.config.offlineTimeout).toISOString();
    
    return new Promise((resolve, reject) => {
      // Check database state
      if (!this.offlineDb) {
        reject(new Error('Database not initialized'));
        return;
      }

      // Add timeout to prevent hanging
      const timeout = setTimeout(() => {
        reject(new Error('Database operation timeout'));
      }, 10000); // 10 second timeout

      this.offlineDb.run(
        'DELETE FROM offline_transactions WHERE status = ? AND synced_at < ?',
        [OFFLINE_STATUS.SYNCED, cutoffDate],
        (err) => {
          // Clear timeout
          clearTimeout(timeout);
          
          if (err) reject(err);
          else resolve();
        }
      );
    });
  }

  startSyncProcess() {
    // Periodic connectivity check and sync
    this.syncInterval = setInterval(async () => {
      await this.checkConnectivity();
      
      if (this.isOnline && this.offlineQueue.length > 0 && !this.syncInProgress) {
        try {
          await this.syncOfflineTransactions();
        } catch (error) {
          logger.error('Automatic sync failed', { error: error.message });
        }
      }
    }, this.config.syncInterval);
  }

  validateCBDCTransaction(transaction) {
    if (!transaction.type || !Object.values(CBDC_TRANSACTION_TYPES).includes(transaction.type)) {
      throw new Error('Invalid CBDC transaction type');
    }

    if (!transaction.amount || transaction.amount <= 0) {
      throw new Error('Invalid transaction amount');
    }

    if (transaction.type === CBDC_TRANSACTION_TYPES.TRANSFER) {
      if (!transaction.from || !transaction.to) {
        throw new Error('Transfer transactions require from and to addresses');
      }
    }
  }

  updateLocalBalances(transaction) {
    // Optimistically update local balances for offline transactions
    if (transaction.type === CBDC_TRANSACTION_TYPES.TRANSFER) {
      const fromBalance = this.metrics.walletBalances.get(transaction.from) || 0;
      const toBalance = this.metrics.walletBalances.get(transaction.to) || 0;
      
      this.metrics.walletBalances.set(transaction.from, fromBalance - transaction.amount);
      this.metrics.walletBalances.set(transaction.to, toBalance + transaction.amount);
    }
  }

  async fetchOnChainBalance(walletAddress) {
    // Simulate Algorand balance query
    return this.metrics.walletBalances.get(walletAddress) || 0;
  }

  estimateSyncTime() {
    const averageSyncTime = 2000; // 2 seconds per transaction
    return this.offlineQueue.length * averageSyncTime;
  }

  signTransaction(transaction) {
    if (!this.config.signingKey) return null;
    
    const txData = JSON.stringify(transaction);
    return crypto
      .createHmac('sha256', this.config.signingKey)
      .update(txData)
      .digest('hex');
  }

  verifyTransactionSignature(transaction, signature) {
    if (!this.config.signingKey) return false;
    
    const txData = JSON.stringify(transaction);
    const expectedSignature = crypto
      .createHmac('sha256', this.config.signingKey)
      .update(txData)
      .digest('hex');
    
    return signature === expectedSignature;
  }

  encryptTransaction(transaction) {
    if (!this.config.encryptionKey) return transaction;

    try {
      const algorithm = 'aes-256-cbc';
      const key = crypto.scryptSync(this.config.encryptionKey, 'salt', 32);
      const iv = crypto.randomBytes(16);
      
      const cipher = crypto.createCipheriv(algorithm, key, iv);
      let encrypted = cipher.update(JSON.stringify(transaction), 'utf8', 'hex');
      encrypted += cipher.final('hex');
      
      return {
        encrypted: true,
        data: encrypted,
        iv: iv.toString('hex')
      };
    } catch (error) {
      logger.warn('Transaction encryption failed', { error: error.message });
      return transaction;
    }
  }

  decryptTransaction(encryptedData) {
    if (!encryptedData.encrypted || !this.config.encryptionKey) {
      return encryptedData;
    }

    try {
      const algorithm = 'aes-256-cbc';
      const key = crypto.scryptSync(this.config.encryptionKey, 'salt', 32);
      const iv = Buffer.from(encryptedData.iv, 'hex');
      
      const decipher = crypto.createDecipheriv(algorithm, key, iv);
      let decrypted = decipher.update(encryptedData.data, 'hex', 'utf8');
      decrypted += decipher.final('utf8');
      
      return JSON.parse(decrypted);
    } catch (error) {
      logger.error('Transaction decryption failed', { error: error.message });
      throw error;
    }
  }

  updateMetrics(transaction, result, duration) {
    this.metrics.totalTransactions++;
    
    if (!this.isOnline || result.status === OFFLINE_STATUS.QUEUED) {
      this.metrics.offlineTransactions++;
    }

    // Update average offline time
    if (!this.isOnline || result.status === OFFLINE_STATUS.QUEUED) {
      this.metrics.averageOfflineTime = 
        (this.metrics.averageOfflineTime * (this.metrics.offlineTransactions - 1) + duration) / 
        this.metrics.offlineTransactions;
    }
  }

  // Metrics and monitoring methods
  getMetrics() {
    return {
      totalTransactions: this.metrics.totalTransactions,
      offlineTransactions: this.metrics.offlineTransactions,
      syncedTransactions: this.metrics.syncedTransactions,
      failedSyncs: this.metrics.failedSyncs,
      walletBalances: Object.fromEntries(this.metrics.walletBalances),
      transactionVolume: Object.fromEntries(this.metrics.transactionVolume),
      lastSyncTimestamp: this.metrics.lastSyncTimestamp,
      encryptedTransactions: this.metrics.encryptedTransactions,
      signedTransactions: this.metrics.signedTransactions
    };
  }

  getHealthStatus() {
    const metrics = this.getMetrics();
    const offlineRate = metrics.totalTransactions > 0 
      ? metrics.offlineTransactions / metrics.totalTransactions 
      : 0;
    
    let status = 'healthy';
    if (offlineRate > 0.5) status = 'degraded';
    if (!this.isOnline) status = 'offline';
    
    return {
      status,
      components: {
        database: this.offlineDb ? 'healthy' : 'disconnected',
        connectivity: this.isOnline ? 'online' : 'offline',
        sync: this.syncInProgress ? 'active' : 'idle'
      },
      timestamp: new Date().toISOString(),
      isOnline: this.isOnline,
      offlineQueueSize: this.offlineQueue.length,
      metrics,
      lastConnectivityCheck: this.lastConnectivityCheck
    };
  }

  getPerformanceMetrics() {
    const totalOffline = this.metrics.offlineTransactions;
    const totalTransactions = this.metrics.totalTransactions;
    const totalSynced = this.metrics.syncedTransactions;
    const totalFailed = this.metrics.failedSyncs;
    
    return {
      offlineRate: totalTransactions > 0 ? totalOffline / totalTransactions : 0,
      syncSuccessRate: (totalSynced + totalFailed) > 0 
        ? totalSynced / (totalSynced + totalFailed) 
        : 0,
      averageQueueSize: this.offlineQueue.length,
      encryptionRate: totalTransactions > 0 
        ? this.metrics.encryptedTransactions / totalTransactions 
        : 0,
      signatureRate: totalTransactions > 0 
        ? this.metrics.signedTransactions / totalTransactions 
        : 0
    };
  }

  // Integration methods
  async processAlgorandTransaction(transaction) {
    // Placeholder for Algorand-specific processing
    if (!this.config.enableAlgorandIntegration) {
      throw new Error('Algorand integration not enabled');
    }
    
    // In production, this would integrate with Algorand SDK
    return {
      txId: `algo-${transaction.id}`,
      confirmed: true,
      blockNumber: Date.now(),
      network: 'algorand'
    };
  }

  async processCrunchfishTransaction(transaction) {
    // Placeholder for Crunchfish-specific processing
    if (!this.config.enableCrunchfishIntegration) {
      throw new Error('Crunchfish integration not enabled');
    }
    
    // In production, this would integrate with Crunchfish SDK
    return {
      status: 'processed',
      proximity: true,
      deviceId: transaction.deviceId || 'unknown',
      timestamp: new Date().toISOString()
    };
  }


  // Wallet balance management
  async getWalletBalance(walletAddress) {
    const balance = this.metrics.walletBalances.get(walletAddress) || 0;
    return {
      wallet: walletAddress,
      balance: balance,
      currency: 'CBDC',
      lastUpdated: new Date().toISOString()
    };
  }

  // Encryption/Decryption methods (expose internal methods)
  encryptTransactionData(data) {
    if (!this.config.encryptionKey) {
      throw new Error('Encryption key not configured');
    }
    const encrypted = this.encryptTransaction({ data });
    return encrypted.data || encrypted;
  }

  decryptTransactionData(encryptedData) {
    if (!this.config.encryptionKey) {
      throw new Error('Encryption key not configured');
    }
    const decrypted = this.decryptTransaction({ data: encryptedData });
    return decrypted.data || decrypted;
  }

  // Update wallet balance
  updateWalletBalance(walletAddress, amount) {
    const currentBalance = this.metrics.walletBalances.get(walletAddress) || 0;
    const newBalance = currentBalance + amount;
    this.metrics.walletBalances.set(walletAddress, newBalance);
    
    // Update transaction volume
    const currentVolume = this.metrics.transactionVolume.get(walletAddress) || 0;
    this.metrics.transactionVolume.set(walletAddress, currentVolume + Math.abs(amount));
    
    return newBalance;
  }

  // Cleanup method
  async cleanup() {
    // Clear intervals
    if (this.syncInterval) {
      clearInterval(this.syncInterval);
      this.syncInterval = null;
    }
    
    // Close database connection
    if (this.offlineDb) {
      await new Promise((resolve, reject) => {
        // Add timeout to prevent hanging during cleanup
        const timeout = setTimeout(() => {
          reject(new Error('Database cleanup timeout'));
        }, 5000); // 5 second timeout for cleanup

        this.offlineDb.close((err) => {
          clearTimeout(timeout);
          if (err) reject(err);
          else resolve();
        });
      });
      this.offlineDb = null;
    }
    
    // Clear queues and metrics
    this.offlineQueue = [];
    this.isInitialized = false;
    this.isOnline = false;
    
    // Emit cleanup event
    this.emit('cleanup', {
      timestamp: new Date().toISOString()
    });
  }
}

module.exports = {
  CBDCOfflineGateway,
  CBDC_TRANSACTION_TYPES,
  OFFLINE_STATUS
};