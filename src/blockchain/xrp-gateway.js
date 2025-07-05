/**
 * XRP Ledger Gateway
 * Implements XRP Ledger integration for cross-border payments
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Optimized for high-liquidity corridors and fast settlement
 */

const { BaseBlockchainGateway, TRANSACTION_STATUS, NETWORK_TYPES } = require('./base-gateway');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// XRP Ledger specific imports (optional dependencies)
let xrpl;
try {
  // Using newer xrpl library instead of deprecated ripple-lib
  xrpl = require('xrpl');
} catch (error) {
  // Fallback for environments without XRP library
  xrpl = null;
}

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'xrp-gateway' }
});

/**
 * XRP-specific transaction types
 */
const XRP_TRANSACTION_TYPES = {
  PAYMENT: 'Payment',
  ESCROW_CREATE: 'EscrowCreate',
  ESCROW_FINISH: 'EscrowFinish',
  PAYMENT_CHANNEL_CREATE: 'PaymentChannelCreate',
  PAYMENT_CHANNEL_FUND: 'PaymentChannelFund'
};

/**
 * XRP Ledger Gateway Class
 * Handles XRP Ledger transactions for cross-border payments
 */
class XRPGateway extends BaseBlockchainGateway {
  constructor(config = {}) {
    super(NETWORK_TYPES.XRP, config);
    
    this.config = {
      // XRP-specific configuration
      server: config.server || (config.testMode 
        ? 'wss://s.altnet.rippletest.net:51233' 
        : 'wss://xrplcluster.com'),
      
      // Wallet configuration
      walletSeed: config.walletSeed || process.env.XRP_WALLET_SEED,
      walletAddress: config.walletAddress || process.env.XRP_WALLET_ADDRESS,
      
      // Transaction settings
      fee: config.fee || '12', // 12 drops (0.000012 XRP)
      maxLedgerVersionOffset: config.maxLedgerVersionOffset || 75,
      
      // Path finding settings
      enablePathFinding: config.enablePathFinding !== false,
      maxPaths: config.maxPaths || 3,
      
      // Liquidity settings
      minLiquidity: config.minLiquidity || 1000, // Minimum XRP for operations
      
      ...this.config // Inherit base configuration
    };
    
    // XRP client and wallet
    this.client = null;
    this.wallet = null;
    
    // Payment channels for frequent payments
    this.paymentChannels = new Map();
    
    // Currency exchange rates cache
    this.exchangeRates = new Map();
    this.ratesLastUpdated = null;
    
    logger.info('XRP Gateway initialized', {
      server: this.config.server,
      testMode: this.config.testMode,
      enablePathFinding: this.config.enablePathFinding
    });
  }

  /**
   * Connect to XRP Ledger
   * @returns {Promise<boolean>} Connection success
   */
  async connect() {
    try {
      if (!xrpl) {
        throw new Error('XRP library not available. Install with: npm install xrpl');
      }

      logger.info('Connecting to XRP Ledger', {
        server: this.config.server,
        testMode: this.config.testMode
      });

      // Create client
      this.client = new xrpl.Client(this.config.server);
      
      // Connect to network
      await this.client.connect();
      
      // Initialize wallet if credentials provided
      if (this.config.walletSeed) {
        this.wallet = xrpl.Wallet.fromSeed(this.config.walletSeed);
        logger.info('XRP wallet initialized', {
          address: this.wallet.address,
          testMode: this.config.testMode
        });
      } else {
        logger.warn('No wallet credentials provided, using address-only mode');
      }
      
      // Verify connection
      const serverInfo = await this.client.request({
        command: 'server_info'
      });
      
      this.isConnected = true;
      this.connectionAttempts = 0;
      
      logger.info('Connected to XRP Ledger successfully', {
        server: this.config.server,
        ledgerVersion: serverInfo.result.info.validated_ledger.seq,
        networkId: serverInfo.result.info.network_id
      });
      
      // Start monitoring for transaction confirmations
      this.startTransactionMonitoring();
      
      return true;
      
    } catch (error) {
      this.connectionAttempts++;
      logger.error('Failed to connect to XRP Ledger', {
        server: this.config.server,
        attempt: this.connectionAttempts,
        error: error.message
      });
      
      throw new Error(`XRP Ledger connection failed: ${error.message}`);
    }
  }

  /**
   * Disconnect from XRP Ledger
   * @returns {Promise<boolean>} Disconnection success
   */
  async disconnect() {
    try {
      if (this.client && this.client.isConnected()) {
        await this.client.disconnect();
      }
      
      this.isConnected = false;
      this.client = null;
      
      logger.info('Disconnected from XRP Ledger');
      return true;
      
    } catch (error) {
      logger.error('Error disconnecting from XRP Ledger', {
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Submit transaction to XRP Ledger
   * @param {Object} transaction - Transaction to submit
   * @returns {Promise<Object>} Transaction result
   */
  async submitTransaction(transaction) {
    try {
      if (!this.client || !this.client.isConnected()) {
        throw new Error('XRP client not connected');
      }

      logger.info('Submitting XRP transaction', {
        transactionId: transaction.id,
        amount: transaction.amount,
        currency: transaction.currency,
        sender: transaction.sender?.name || 'Unknown',
        receiver: transaction.receiver?.name || 'Unknown'
      });

      // Convert to XRP transaction format
      const xrpTransaction = await this.convertToXRPTransaction(transaction);
      
      // Submit and wait for validation
      const response = await this.client.submitAndWait(xrpTransaction, {
        autofill: true,
        wallet: this.wallet
      });
      
      // Process response
      const result = this.processTransactionResponse(response, transaction);
      
      logger.info('XRP transaction submitted successfully', {
        transactionId: transaction.id,
        xrpHash: response.result.hash,
        ledgerIndex: response.result.ledger_index,
        status: result.status
      });
      
      return result;
      
    } catch (error) {
      logger.error('XRP transaction submission failed', {
        transactionId: transaction.id,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get transaction status from XRP Ledger
   * @param {string} transactionId - Transaction ID to query
   * @returns {Promise<Object>} Transaction status
   */
  async getTransactionStatus(transactionId) {
    try {
      const tracked = this.transactionHistory.get(transactionId);
      if (!tracked) {
        throw new Error(`Transaction ${transactionId} not found in history`);
      }

      // Query XRP Ledger for current status
      const response = await this.client.request({
        command: 'tx',
        transaction: tracked.networkTransactionId
      });

      const status = this.mapXRPStatus(response.result);
      
      // Update tracked transaction if status changed
      if (tracked.status !== status) {
        tracked.status = status;
        tracked.lastUpdated = new Date().toISOString();
        
        // Remove from pending if confirmed/failed
        if (status === TRANSACTION_STATUS.CONFIRMED || status === TRANSACTION_STATUS.FAILED) {
          this.pendingTransactions.delete(transactionId);
        }
      }

      return {
        transactionId,
        networkTransactionId: tracked.networkTransactionId,
        status,
        ledgerIndex: response.result.ledger_index,
        fee: response.result.Fee,
        lastUpdated: tracked.lastUpdated
      };
      
    } catch (error) {
      logger.error('Failed to get XRP transaction status', {
        transactionId,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get XRP Ledger network health
   * @returns {Promise<Object>} Network health metrics
   */
  async getNetworkHealth() {
    try {
      if (!this.client || !this.client.isConnected()) {
        throw new Error('XRP client not connected');
      }

      const [serverInfo, ledgerInfo] = await Promise.all([
        this.client.request({ command: 'server_info' }),
        this.client.request({ command: 'ledger_current' })
      ]);

      const health = {
        networkType: this.networkType,
        isHealthy: true,
        ledgerVersion: serverInfo.result.info.validated_ledger.seq,
        currentLedger: ledgerInfo.result.ledger_current_index,
        serverState: serverInfo.result.info.server_state,
        fee: serverInfo.result.info.validated_ledger.base_fee_xrp,
        reserve: serverInfo.result.info.validated_ledger.reserve_base_xrp,
        load: serverInfo.result.info.load_factor,
        peers: serverInfo.result.info.peers,
        uptime: serverInfo.result.info.uptime,
        timestamp: new Date().toISOString()
      };

      // Determine if network is healthy
      health.isHealthy = 
        health.serverState === 'full' && 
        health.load < 256 && // Normal load
        health.peers > 10; // Well connected

      return health;
      
    } catch (error) {
      logger.error('Failed to get XRP network health', {
        error: error.message
      });
      
      return {
        networkType: this.networkType,
        isHealthy: false,
        error: error.message,
        timestamp: new Date().toISOString()
      };
    }
  }

  /**
   * Convert internal transaction to XRP transaction format
   * @param {Object} transaction - Internal transaction format
   * @returns {Promise<Object>} XRP transaction object
   */
  async convertToXRPTransaction(transaction) {
    const xrpTx = {
      TransactionType: XRP_TRANSACTION_TYPES.PAYMENT,
      Account: this.wallet.address,
      Destination: await this.resolveDestinationAddress(transaction.receiver),
      Fee: this.config.fee
    };

    // Handle different currencies
    if (transaction.currency === 'XRP') {
      // Native XRP payment
      xrpTx.Amount = xrpl.xrpToDrops(transaction.amount.toString());
    } else {
      // Non-XRP currency (IOU)
      xrpTx.Amount = {
        currency: transaction.currency,
        value: transaction.amount.toString(),
        issuer: await this.getIssuerAddress(transaction.currency)
      };
      
      // Add path finding for cross-currency payments
      if (this.config.enablePathFinding) {
        const paths = await this.findPaymentPaths(
          this.wallet.address,
          xrpTx.Destination,
          xrpTx.Amount
        );
        if (paths && paths.length > 0) {
          xrpTx.Paths = paths;
        }
      }
    }

    // Add destination tag if provided
    if (transaction.receiver.tag) {
      xrpTx.DestinationTag = parseInt(transaction.receiver.tag);
    }

    // Add memo for transaction reference
    if (transaction.transactionReference) {
      xrpTx.Memos = [{
        Memo: {
          MemoType: xrpl.convertStringToHex('reference'),
          MemoData: xrpl.convertStringToHex(transaction.transactionReference)
        }
      }];
    }

    return xrpTx;
  }

  /**
   * Process XRP transaction response
   * @param {Object} response - XRP Ledger response
   * @param {Object} originalTransaction - Original transaction
   * @returns {Object} Processed result
   */
  processTransactionResponse(response, originalTransaction) {
    const result = response.result;
    
    return {
      id: result.hash,
      status: this.mapXRPStatus(result),
      ledgerIndex: result.ledger_index,
      fee: result.Fee,
      transactionId: originalTransaction.id,
      networkType: this.networkType,
      timestamp: new Date().toISOString()
    };
  }

  /**
   * Map XRP transaction status to internal status
   * @param {Object} xrpResult - XRP transaction result
   * @returns {string} Internal status
   */
  mapXRPStatus(xrpResult) {
    if (xrpResult.meta && xrpResult.meta.TransactionResult) {
      switch (xrpResult.meta.TransactionResult) {
        case 'tesSUCCESS':
          return TRANSACTION_STATUS.CONFIRMED;
        case 'tecPATH_DRY':
        case 'tecUNFUNDED_PAYMENT':
        case 'tecNO_DST':
          return TRANSACTION_STATUS.FAILED;
        default:
          if (xrpResult.meta.TransactionResult.startsWith('tec')) {
            return TRANSACTION_STATUS.FAILED;
          }
          return TRANSACTION_STATUS.PENDING;
      }
    }
    
    if (xrpResult.validated === true) {
      return TRANSACTION_STATUS.CONFIRMED;
    }
    
    return TRANSACTION_STATUS.SUBMITTED;
  }

  /**
   * Resolve destination address from receiver information
   * @param {Object} receiver - Receiver information
   * @returns {Promise<string>} XRP address
   */
  async resolveDestinationAddress(receiver) {
    // In production, this would resolve bank routing information to XRP addresses
    // For now, return a test address or the provided address
    if (receiver.xrpAddress) {
      return receiver.xrpAddress;
    }
    
    // Mock resolution based on receiver name/account
    if (this.config.testMode) {
      // Return a test address
      return 'rUn84CJzdHmV3QpNfkp6dbxBaM5sBLrAr'; // Test address
    }
    
    throw new Error('Cannot resolve XRP destination address for receiver');
  }

  /**
   * Get issuer address for currency
   * @param {string} currency - Currency code
   * @returns {Promise<string>} Issuer address
   */
  async getIssuerAddress(currency) {
    // In production, this would resolve currency issuers
    // For now, return mock addresses for testing
    const mockIssuers = {
      'USD': 'rhub8VRN55s94qWKDv6jmDy1pUykJzF3wq',
      'EUR': 'rLNaPoKeeBjZe2qs6x52yVPZpZ8td4dc6w',
      'GBP': 'rMwjYedjc7qqtKYVLiAccJSmCwih4LnE2q'
    };
    
    const issuer = mockIssuers[currency];
    if (!issuer) {
      throw new Error(`No issuer found for currency ${currency}`);
    }
    
    return issuer;
  }

  /**
   * Find payment paths for cross-currency payments
   * @param {string} source - Source address
   * @param {string} destination - Destination address
   * @param {Object} amount - Payment amount
   * @returns {Promise<Array>} Payment paths
   */
  async findPaymentPaths(source, destination, amount) {
    try {
      const response = await this.client.request({
        command: 'ripple_path_find',
        source_account: source,
        destination_account: destination,
        destination_amount: amount
      });
      
      return response.result.alternatives?.slice(0, this.config.maxPaths) || [];
      
    } catch (error) {
      logger.warn('Path finding failed, proceeding without paths', {
        source,
        destination,
        error: error.message
      });
      return [];
    }
  }

  /**
   * Start monitoring pending transactions
   */
  startTransactionMonitoring() {
    if (this.monitoringInterval) {
      clearInterval(this.monitoringInterval);
    }
    
    this.monitoringInterval = setInterval(async () => {
      try {
        for (const [transactionId] of this.pendingTransactions) {
          await this.getTransactionStatus(transactionId);
        }
      } catch (error) {
        logger.error('Transaction monitoring error', {
          error: error.message
        });
      }
    }, 10000); // Check every 10 seconds
  }

  /**
   * Get account info for address
   * @param {string} address - XRP address
   * @returns {Promise<Object>} Account information
   */
  async getAccountInfo(address) {
    try {
      const response = await this.client.request({
        command: 'account_info',
        account: address
      });
      
      return response.result.account_data;
      
    } catch (error) {
      logger.error('Failed to get account info', {
        address,
        error: error.message
      });
      throw error;
    }
  }

  /**
   * Get current exchange rates
   * @returns {Promise<Object>} Exchange rates
   */
  async getExchangeRates() {
    // Cache rates for 5 minutes
    const now = Date.now();
    if (this.ratesLastUpdated && (now - this.ratesLastUpdated) < 300000) {
      return Object.fromEntries(this.exchangeRates);
    }
    
    try {
      // Try to fetch from XRP Ledger order books
      if (this.client && this.isConnected) {
        try {
          // Try to get orderbook data for exchange rates
          await this.client.request({
            command: 'book_offers',
            taker_gets: { currency: 'USD' },
            taker_pays: { currency: 'XRP' }
          });
        } catch (orderbookError) {
          // If orderbook fails, return empty object
          return {};
        }
      }
      
      // For now, return mock rates if orderbook succeeds or not connected
      const rates = {
        'USD': 1.0,
        'EUR': 0.85,
        'GBP': 0.73,
        'XRP': 0.52 // Mock XRP/USD rate
      };
      
      this.exchangeRates.clear();
      for (const [currency, rate] of Object.entries(rates)) {
        this.exchangeRates.set(currency, rate);
      }
      
      this.ratesLastUpdated = now;
      return rates;
      
    } catch (error) {
      logger.error('Failed to get exchange rates', {
        error: error.message
      });
      return {};
    }
  }

  /**
   * Deploy COBOL-generated payment contract to XRP Ledger
   * @param {Object} contractData - Contract deployment data
   * @param {Object} options - Deployment options
   * @returns {Promise<Object>} Deployment result
   */
  async deployCobolPaymentContract(contractData, options = {}) {
    const startTime = Date.now();
    const deploymentId = uuidv4();
    
    try {
      logger.info('Deploying COBOL-generated payment contract', {
        deploymentId,
        contractName: contractData.name,
        bankingSystem: contractData.metadata?.bankingSystem,
        network: this.networkType
      });

      // Validate deployment requirements
      this.validatePaymentContractDeployment(contractData);

      // Ensure network connection
      await this.ensureConnection();

      if (!this.wallet) {
        throw new Error('Wallet not initialized - cannot deploy payment contracts');
      }

      // Prepare payment contract based on COBOL logic
      let deploymentResult;
      const contractType = contractData.contractType || 'ESCROW';

      if (contractType === 'ESCROW') {
        deploymentResult = await this.deployEscrowContract(contractData, deploymentId);
      } else if (contractType === 'PAYMENT_CHANNEL') {
        deploymentResult = await this.deployPaymentChannelContract(contractData, deploymentId);
      } else {
        throw new Error(`Unsupported XRP contract type: ${contractType}`);
      }

      const deploymentTime = Date.now() - startTime;

      logger.info('COBOL payment contract deployed successfully', {
        deploymentId,
        contractType,
        txHash: deploymentResult.txHash,
        deploymentTime
      });

      // Emit deployment event
      this.emit('paymentContractDeployed', {
        deploymentId,
        contractName: contractData.name,
        contractType,
        txHash: deploymentResult.txHash,
        deploymentTime,
        networkType: this.networkType
      });

      return {
        success: true,
        deploymentId,
        contractType,
        transactionHash: deploymentResult.txHash,
        ledgerIndex: deploymentResult.ledgerIndex,
        deploymentTime,
        networkType: this.networkType,
        contractInfo: deploymentResult.contractInfo
      };

    } catch (error) {
      const deploymentTime = Date.now() - startTime;
      
      logger.error('COBOL payment contract deployment failed', {
        deploymentId,
        contractName: contractData.name,
        error: error.message,
        deploymentTime
      });

      // Emit deployment failure event
      this.emit('paymentContractDeploymentFailed', {
        deploymentId,
        contractName: contractData.name,
        error: error.message,
        deploymentTime,
        networkType: this.networkType
      });

      throw new Error(`Payment contract deployment failed: ${error.message}`);
    }
  }

  /**
   * Deploy escrow contract on XRP Ledger
   * @param {Object} contractData - Contract data
   * @param {string} deploymentId - Deployment ID
   * @returns {Promise<Object>} Deployment result
   */
  async deployEscrowContract(contractData, deploymentId) {
    const escrowConfig = contractData.contractConfig || {};
    
    // Calculate finish time based on COBOL business logic
    const finishAfter = escrowConfig.finishAfter || Date.now() + (24 * 60 * 60); // 24 hours default
    const condition = escrowConfig.condition || null;
    const fulfillment = escrowConfig.fulfillment || null;

    const escrowCreate = {
      TransactionType: 'EscrowCreate',
      Account: this.wallet.classicAddress,
      Destination: escrowConfig.destination,
      Amount: String(escrowConfig.amount || '1000000'), // 1 XRP default in drops
      FinishAfter: Math.floor(finishAfter / 1000), // Convert to XRP time
      ...(condition && { Condition: condition }),
      ...(fulfillment && { Fulfillment: fulfillment }),
      Memos: [{
        Memo: {
          MemoType: Buffer.from('COBOL_CONTRACT', 'utf8').toString('hex').toUpperCase(),
          MemoData: Buffer.from(JSON.stringify({
            deploymentId,
            sourceProgram: contractData.metadata?.sourceProgram,
            bankingSystem: contractData.metadata?.bankingSystem
          }), 'utf8').toString('hex').toUpperCase()
        }
      }]
    };

    // Submit escrow creation transaction
    const prepared = await this.client.autofill(escrowCreate);
    const signed = this.wallet.sign(prepared);
    const result = await this.client.submitAndWait(signed.tx_blob);

    if (result.result.meta.TransactionResult !== 'tesSUCCESS') {
      throw new Error(`Escrow creation failed: ${result.result.meta.TransactionResult}`);
    }

    return {
      txHash: result.result.hash,
      ledgerIndex: result.result.ledger_index,
      contractInfo: {
        type: 'ESCROW',
        escrowSequence: result.result.Sequence,
        destination: escrowConfig.destination,
        amount: escrowConfig.amount,
        finishAfter,
        deploymentId,
        metadata: contractData.metadata
      }
    };
  }

  /**
   * Deploy payment channel contract on XRP Ledger
   * @param {Object} contractData - Contract data
   * @param {string} deploymentId - Deployment ID
   * @returns {Promise<Object>} Deployment result
   */
  async deployPaymentChannelContract(contractData, deploymentId) {
    const channelConfig = contractData.contractConfig || {};
    
    const paymentChannelCreate = {
      TransactionType: 'PaymentChannelCreate',
      Account: this.wallet.classicAddress,
      Destination: channelConfig.destination,
      Amount: String(channelConfig.amount || '10000000'), // 10 XRP default in drops
      SettleDelay: channelConfig.settleDelay || 86400, // 1 day default
      PublicKey: channelConfig.publicKey || this.wallet.publicKey,
      Memos: [{
        Memo: {
          MemoType: Buffer.from('COBOL_PAYMENT_CHANNEL', 'utf8').toString('hex').toUpperCase(),
          MemoData: Buffer.from(JSON.stringify({
            deploymentId,
            sourceProgram: contractData.metadata?.sourceProgram,
            bankingSystem: contractData.metadata?.bankingSystem
          }), 'utf8').toString('hex').toUpperCase()
        }
      }]
    };

    // Submit payment channel creation transaction
    const prepared = await this.client.autofill(paymentChannelCreate);
    const signed = this.wallet.sign(prepared);
    const result = await this.client.submitAndWait(signed.tx_blob);

    if (result.result.meta.TransactionResult !== 'tesSUCCESS') {
      throw new Error(`Payment channel creation failed: ${result.result.meta.TransactionResult}`);
    }

    return {
      txHash: result.result.hash,
      ledgerIndex: result.result.ledger_index,
      contractInfo: {
        type: 'PAYMENT_CHANNEL',
        channelId: result.result.hash, // Use transaction hash as channel ID
        destination: channelConfig.destination,
        amount: channelConfig.amount,
        settleDelay: channelConfig.settleDelay,
        deploymentId,
        metadata: contractData.metadata
      }
    };
  }

  /**
   * Validate payment contract deployment requirements
   * @param {Object} contractData - Contract data to validate
   */
  validatePaymentContractDeployment(contractData) {
    if (!contractData) {
      throw new Error('Contract data is required for deployment');
    }

    if (!contractData.name) {
      throw new Error('Contract name is required');
    }

    if (!contractData.contractConfig) {
      throw new Error('Contract configuration is required');
    }

    if (!contractData.contractConfig.destination) {
      throw new Error('Destination address is required for payment contracts');
    }

    // Validate COBOL-specific metadata
    if (contractData.metadata?.sourceType === 'cobol') {
      if (!contractData.metadata.bankingSystem) {
        throw new Error('Banking system is required for COBOL contracts');
      }

      if (!contractData.metadata.sourceProgram) {
        throw new Error('Source COBOL program identifier is required');
      }
    }
  }

  /**
   * Get payment contract deployment status
   * @param {string} deploymentId - Deployment ID to check
   * @returns {Promise<Object>} Deployment status
   */
  async getPaymentContractDeploymentStatus(deploymentId) {
    try {
      // Search for transactions with the deployment ID in memos
      const accountTx = await this.client.request({
        command: 'account_tx',
        account: this.wallet.classicAddress,
        limit: 100
      });

      for (const txEntry of accountTx.result.transactions) {
        const tx = txEntry.tx;
        if (tx.Memos) {
          for (const memo of tx.Memos) {
            try {
              const memoData = Buffer.from(memo.Memo.MemoData, 'hex').toString('utf8');
              const data = JSON.parse(memoData);
              
              if (data.deploymentId === deploymentId) {
                const validated = txEntry.validated;
                const result = txEntry.meta?.TransactionResult;
                
                return {
                  status: validated && result === 'tesSUCCESS' ? 'confirmed' : 'failed',
                  deploymentId,
                  transactionHash: tx.hash,
                  ledgerIndex: tx.ledger_index,
                  result: result,
                  validated: validated,
                  metadata: data
                };
              }
            } catch (e) {
              // Skip invalid memo data
            }
          }
        }
      }

      return {
        status: 'not_found',
        deploymentId,
        message: 'Payment contract deployment not found'
      };

    } catch (error) {
      logger.error('Failed to get payment contract deployment status', {
        deploymentId,
        error: error.message
      });

      return {
        status: 'error',
        deploymentId,
        error: error.message
      };
    }
  }

  /**
   * List deployed payment contracts
   * @returns {Promise<Array>} List of deployed contracts
   */
  async listDeployedPaymentContracts() {
    try {
      const contracts = [];
      
      // Get recent transactions
      const accountTx = await this.client.request({
        command: 'account_tx',
        account: this.wallet.classicAddress,
        limit: 200
      });

      for (const txEntry of accountTx.result.transactions) {
        const tx = txEntry.tx;
        
        // Check for escrow or payment channel transactions with COBOL memos
        if ((tx.TransactionType === 'EscrowCreate' || tx.TransactionType === 'PaymentChannelCreate') && tx.Memos) {
          for (const memo of tx.Memos) {
            try {
              const memoType = Buffer.from(memo.Memo.MemoType, 'hex').toString('utf8');
              if (memoType.includes('COBOL')) {
                const memoData = Buffer.from(memo.Memo.MemoData, 'hex').toString('utf8');
                const data = JSON.parse(memoData);
                
                contracts.push({
                  deploymentId: data.deploymentId,
                  contractType: tx.TransactionType === 'EscrowCreate' ? 'ESCROW' : 'PAYMENT_CHANNEL',
                  transactionHash: tx.hash,
                  bankingSystem: data.bankingSystem,
                  sourceProgram: data.sourceProgram,
                  deployedAt: new Date(946684800000 + (tx.date * 1000)).toISOString(), // XRP epoch conversion
                  validated: txEntry.validated
                });
              }
            } catch (e) {
              // Skip invalid memo data
            }
          }
        }
      }

      return contracts;

    } catch (error) {
      logger.error('Failed to list deployed payment contracts', {
        error: error.message
      });
      return [];
    }
  }

  /**
   * Cleanup resources
   */
  async cleanup() {
    if (this.monitoringInterval) {
      clearInterval(this.monitoringInterval);
      this.monitoringInterval = null;
    }
    
    await this.disconnect();
  }
}

module.exports = {
  XRPGateway,
  XRP_TRANSACTION_TYPES
};