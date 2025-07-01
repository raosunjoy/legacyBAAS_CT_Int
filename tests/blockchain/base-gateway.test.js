/**
 * Base Blockchain Gateway Tests
 * Comprehensive test suite for the abstract base gateway class
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const { BaseBlockchainGateway, TRANSACTION_STATUS, NETWORK_TYPES } = require('../../src/blockchain/base-gateway');

// Mock implementation for testing
class MockBlockchainGateway extends BaseBlockchainGateway {
  constructor(config = {}) {
    super(NETWORK_TYPES.XRP, config);
    this.mockConnected = false;
    this.mockSubmissionResult = { id: 'mock-tx-123', status: TRANSACTION_STATUS.SUBMITTED };
    this.mockNetworkHealth = { isHealthy: true, latency: 100 };
  }

  async connect() {
    this.mockConnected = true;
    this.isConnected = true;
    return true;
  }

  async disconnect() {
    this.mockConnected = false;
    this.isConnected = false;
    return true;
  }

  async submitTransaction(transaction) {
    if (!this.mockConnected) {
      throw new Error('Not connected to network');
    }
    
    if (transaction.id === 'fail-transaction') {
      throw new Error('Mock transaction failure');
    }
    
    return this.mockSubmissionResult;
  }

  async getTransactionStatus(transactionId) {
    const tracked = this.transactionHistory.get(transactionId);
    if (!tracked) {
      throw new Error(`Transaction ${transactionId} not found`);
    }
    return tracked;
  }

  async getNetworkHealth() {
    if (!this.mockConnected) {
      throw new Error('Network not available');
    }
    return this.mockNetworkHealth;
  }
}

describe('Base Blockchain Gateway', () => {
  let gateway;
  let mockTransaction;

  beforeEach(() => {
    gateway = new MockBlockchainGateway({
      timeout: 5000,
      retryAttempts: 2,
      testMode: true
    });

    mockTransaction = {
      id: 'test-tx-123',
      amount: 1000,
      currency: 'USD',
      sender: { name: 'John Doe', account: 'ACC123' },
      receiver: { name: 'Jane Smith', account: 'ACC456' },
      transactionReference: 'REF789'
    };
  });

  afterEach(() => {
    gateway.removeAllListeners();
    gateway.clearTransactionHistory();
  });

  describe('Constructor', () => {
    test('should throw error when instantiated directly', () => {
      expect(() => new BaseBlockchainGateway()).toThrow('BaseBlockchainGateway is abstract and cannot be instantiated directly');
    });

    test('should initialize with default configuration', () => {
      expect(gateway.networkType).toBe(NETWORK_TYPES.XRP);
      expect(gateway.config.timeout).toBe(5000);
      expect(gateway.config.retryAttempts).toBe(2);
      expect(gateway.config.testMode).toBe(true);
    });

    test('should initialize with empty state', () => {
      expect(gateway.isConnected).toBe(false);
      expect(gateway.connectionAttempts).toBe(0);
      expect(gateway.transactionHistory).toBeInstanceOf(Map);
      expect(gateway.pendingTransactions).toBeInstanceOf(Map);
      expect(gateway.metrics.totalTransactions).toBe(0);
    });
  });

  describe('Abstract Methods', () => {
    let abstractGateway;

    beforeEach(() => {
      // Create instance that doesn't override abstract methods
      class IncompleteGateway extends BaseBlockchainGateway {
        constructor() {
          super(NETWORK_TYPES.XRP);
        }
      }
      abstractGateway = new IncompleteGateway();
    });

    test('should throw error for unimplemented connect method', async () => {
      await expect(abstractGateway.connect()).rejects.toThrow('connect() method must be implemented by subclass');
    });

    test('should throw error for unimplemented disconnect method', async () => {
      await expect(abstractGateway.disconnect()).rejects.toThrow('disconnect() method must be implemented by subclass');
    });

    test('should throw error for unimplemented submitTransaction method', async () => {
      await expect(abstractGateway.submitTransaction({})).rejects.toThrow('submitTransaction() method must be implemented by subclass');
    });

    test('should throw error for unimplemented getTransactionStatus method', async () => {
      await expect(abstractGateway.getTransactionStatus('123')).rejects.toThrow('getTransactionStatus() method must be implemented by subclass');
    });

    test('should throw error for unimplemented getNetworkHealth method', async () => {
      await expect(abstractGateway.getNetworkHealth()).rejects.toThrow('getNetworkHealth() method must be implemented by subclass');
    });
  });

  describe('Transaction Validation', () => {
    test('should validate valid transaction', () => {
      expect(() => gateway.validateTransaction(mockTransaction)).not.toThrow();
    });

    test('should throw error for null transaction', () => {
      expect(() => gateway.validateTransaction(null)).toThrow('Transaction is required');
    });

    test('should throw error for missing transaction ID', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.id;
      expect(() => gateway.validateTransaction(invalidTx)).toThrow('Transaction ID is required');
    });

    test('should throw error for invalid amount', () => {
      const invalidTx = { ...mockTransaction, amount: 0 };
      expect(() => gateway.validateTransaction(invalidTx)).toThrow('Valid transaction amount is required');
      
      const negativeTx = { ...mockTransaction, amount: -100 };
      expect(() => gateway.validateTransaction(negativeTx)).toThrow('Valid transaction amount is required');
    });

    test('should throw error for missing currency', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.currency;
      expect(() => gateway.validateTransaction(invalidTx)).toThrow('Transaction currency is required');
    });

    test('should throw error for missing sender', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.sender;
      expect(() => gateway.validateTransaction(invalidTx)).toThrow('Transaction sender is required');
    });

    test('should throw error for missing receiver', () => {
      const invalidTx = { ...mockTransaction };
      delete invalidTx.receiver;
      expect(() => gateway.validateTransaction(invalidTx)).toThrow('Transaction receiver is required');
    });
  });

  describe('Connection Management', () => {
    test('should connect successfully', async () => {
      const result = await gateway.connect();
      expect(result).toBe(true);
      expect(gateway.isConnected).toBe(true);
    });

    test('should disconnect successfully', async () => {
      await gateway.connect();
      const result = await gateway.disconnect();
      expect(result).toBe(true);
      expect(gateway.isConnected).toBe(false);
    });

    test('should ensure connection before operations', async () => {
      expect(gateway.isConnected).toBe(false);
      await gateway.ensureConnection();
      expect(gateway.isConnected).toBe(true);
    });

    test('should perform health check after timeout', async () => {
      await gateway.connect();
      
      // Mock old health check
      gateway.lastHealthCheck = Date.now() - 70000; // 70 seconds ago
      
      await gateway.ensureConnection();
      expect(gateway.lastHealthCheck).toBeGreaterThan(Date.now() - 5000);
    });

    test('should reconnect on health check failure', async () => {
      await gateway.connect();
      
      // Mock health check failure
      gateway.mockNetworkHealth = null;
      gateway.getNetworkHealth = jest.fn().mockRejectedValue(new Error('Health check failed'));
      gateway.lastHealthCheck = Date.now() - 70000;
      
      const connectSpy = jest.spyOn(gateway, 'connect');
      await gateway.ensureConnection();
      
      expect(connectSpy).toHaveBeenCalled();
    });
  });

  describe('Transaction Processing', () => {
    beforeEach(async () => {
      await gateway.connect();
    });

    test('should process transaction successfully', async () => {
      const events = [];
      gateway.on('transactionSubmitted', (event) => events.push(event));

      const result = await gateway.processTransaction(mockTransaction);

      expect(result.success).toBe(true);
      expect(result.transactionId).toBe(mockTransaction.id);
      expect(result.networkTransactionId).toBe('mock-tx-123');
      expect(result.networkType).toBe(NETWORK_TYPES.XRP);
      expect(typeof result.latency).toBe('number');

      // Check event emission
      expect(events).toHaveLength(1);
      expect(events[0].transactionId).toBe(mockTransaction.id);
    });

    test('should handle transaction processing errors', async () => {
      const errorEvents = [];
      gateway.on('transactionFailed', (event) => errorEvents.push(event));

      const failingTransaction = { ...mockTransaction, id: 'fail-transaction' };

      await expect(gateway.processTransaction(failingTransaction)).rejects.toThrow('Mock transaction failure');

      // Check error event emission
      expect(errorEvents).toHaveLength(1);
      expect(errorEvents[0].transactionId).toBe('fail-transaction');
    });

    test('should track successful transactions', async () => {
      await gateway.processTransaction(mockTransaction);

      const history = gateway.getTransactionHistory();
      expect(history).toHaveLength(1);
      expect(history[0].id).toBe(mockTransaction.id);
      expect(history[0].networkTransactionId).toBe('mock-tx-123');
    });

    test('should update metrics on transaction processing', async () => {
      const initialMetrics = gateway.getMetrics();
      expect(initialMetrics.totalTransactions).toBe(0);

      await gateway.processTransaction(mockTransaction);

      const updatedMetrics = gateway.getMetrics();
      expect(updatedMetrics.totalTransactions).toBe(1);
      expect(updatedMetrics.successfulTransactions).toBe(1);
      expect(updatedMetrics.successRate).toBe(1);
      expect(updatedMetrics.averageLatency).toBeGreaterThan(0);
    });
  });

  describe('Retry Logic', () => {
    beforeEach(async () => {
      await gateway.connect();
    });

    test('should retry failed submissions', async () => {
      let attempts = 0;
      gateway.submitTransaction = jest.fn().mockImplementation(() => {
        attempts++;
        if (attempts < 2) {
          throw new Error('Network error');
        }
        return { id: 'retry-success', status: TRANSACTION_STATUS.SUBMITTED };
      });

      const result = await gateway.submitWithRetry(mockTransaction);
      
      expect(attempts).toBe(2);
      expect(result.id).toBe('retry-success');
    });

    test('should not retry validation errors', async () => {
      gateway.submitTransaction = jest.fn().mockRejectedValue(new Error('Invalid transaction format'));

      await expect(gateway.submitWithRetry(mockTransaction)).rejects.toThrow('Invalid transaction format');
      expect(gateway.submitTransaction).toHaveBeenCalledTimes(1);
    });

    test('should exhaust retry attempts and throw final error', async () => {
      gateway.submitTransaction = jest.fn().mockRejectedValue(new Error('Network timeout'));

      await expect(gateway.submitWithRetry(mockTransaction)).rejects.toThrow('Transaction submission failed after 2 attempts');
      expect(gateway.submitTransaction).toHaveBeenCalledTimes(2);
    });

    test('should implement exponential backoff', async () => {
      const sleepSpy = jest.spyOn(gateway, 'sleep').mockResolvedValue();
      gateway.submitTransaction = jest.fn().mockRejectedValue(new Error('Network error'));

      await expect(gateway.submitWithRetry(mockTransaction)).rejects.toThrow();

      expect(sleepSpy).toHaveBeenCalledWith(1000); // First retry delay
    });
  });

  describe('Validation Error Detection', () => {
    test('should identify validation errors', () => {
      const validationErrors = [
        new Error('Invalid transaction format'),
        new Error('Validation failed for amount'),
        new Error('Required field missing'),
        new Error('Format error in address')
      ];

      validationErrors.forEach(error => {
        expect(gateway.isValidationError(error)).toBe(true);
      });
    });

    test('should not identify network errors as validation errors', () => {
      const networkErrors = [
        new Error('Network timeout'),
        new Error('Connection refused'),
        new Error('Server error 500')
      ];

      networkErrors.forEach(error => {
        expect(gateway.isValidationError(error)).toBe(false);
      });
    });
  });

  describe('Metrics and Monitoring', () => {
    beforeEach(async () => {
      await gateway.connect();
    });

    test('should return comprehensive metrics', () => {
      const metrics = gateway.getMetrics();

      expect(metrics).toHaveProperty('totalTransactions', 0);
      expect(metrics).toHaveProperty('successfulTransactions', 0);
      expect(metrics).toHaveProperty('failedTransactions', 0);
      expect(metrics).toHaveProperty('successRate', 0);
      expect(metrics).toHaveProperty('averageLatency', 0);
      expect(metrics).toHaveProperty('networkType', NETWORK_TYPES.XRP);
      expect(metrics).toHaveProperty('isConnected', true);
    });

    test('should calculate success rate correctly', async () => {
      // Process successful transaction
      await gateway.processTransaction(mockTransaction);

      // Process failed transaction
      try {
        await gateway.processTransaction({ ...mockTransaction, id: 'fail-transaction' });
      } catch (error) {
        // Expected failure
      }

      const metrics = gateway.getMetrics();
      expect(metrics.totalTransactions).toBe(2);
      expect(metrics.successfulTransactions).toBe(1);
      expect(metrics.failedTransactions).toBe(1);
      expect(metrics.successRate).toBe(0.5);
    });

    test('should track transaction history with limits', async () => {
      // Add multiple transactions
      for (let i = 0; i < 5; i++) {
        await gateway.processTransaction({ ...mockTransaction, id: `tx-${i}` });
      }

      const history = gateway.getTransactionHistory(3);
      expect(history).toHaveLength(3);
      
      // Should be sorted by timestamp (newest first)
      expect(history[0].id).toBe('tx-4');
      expect(history[1].id).toBe('tx-3');
      expect(history[2].id).toBe('tx-2');
    });

    test('should clear transaction history', async () => {
      await gateway.processTransaction(mockTransaction);
      expect(gateway.getTransactionHistory()).toHaveLength(1);

      gateway.clearTransactionHistory();
      expect(gateway.getTransactionHistory()).toHaveLength(0);
      expect(gateway.transactionHistory.size).toBe(0);
      expect(gateway.pendingTransactions.size).toBe(0);
    });
  });

  describe('Network Information', () => {
    test('should return network information', () => {
      const info = gateway.getNetworkInfo();

      expect(info.networkType).toBe(NETWORK_TYPES.XRP);
      expect(info.isConnected).toBe(false);
      expect(info.config.testMode).toBe(true);
      expect(info.config.timeout).toBe(5000);
      expect(info.metrics).toBeDefined();
    });
  });

  describe('Transaction Tracking', () => {
    beforeEach(async () => {
      await gateway.connect();
    });

    test('should track pending transactions', async () => {
      gateway.mockSubmissionResult = { id: 'pending-tx', status: TRANSACTION_STATUS.PENDING };
      
      await gateway.processTransaction(mockTransaction);
      
      expect(gateway.pendingTransactions.has(mockTransaction.id)).toBe(true);
    });

    test('should remove completed transactions from pending', async () => {
      gateway.mockSubmissionResult = { id: 'confirmed-tx', status: TRANSACTION_STATUS.CONFIRMED };
      
      await gateway.processTransaction(mockTransaction);
      
      expect(gateway.pendingTransactions.has(mockTransaction.id)).toBe(false);
    });
  });

  describe('Utility Methods', () => {
    test('should implement sleep utility', async () => {
      const start = Date.now();
      await gateway.sleep(100);
      const elapsed = Date.now() - start;
      
      expect(elapsed).toBeGreaterThanOrEqual(90); // Allow for timing variance
    });
  });

  describe('Constants Export', () => {
    test('should export transaction status constants', () => {
      expect(TRANSACTION_STATUS.PENDING).toBe('pending');
      expect(TRANSACTION_STATUS.SUBMITTED).toBe('submitted');
      expect(TRANSACTION_STATUS.CONFIRMED).toBe('confirmed');
      expect(TRANSACTION_STATUS.FAILED).toBe('failed');
      expect(TRANSACTION_STATUS.REJECTED).toBe('rejected');
    });

    test('should export network type constants', () => {
      expect(NETWORK_TYPES.XRP).toBe('xrp-ledger');
      expect(NETWORK_TYPES.CORDA).toBe('r3-corda');
      expect(NETWORK_TYPES.ETHEREUM_L2).toBe('ethereum-polygon');
      expect(NETWORK_TYPES.ALGORAND).toBe('algorand');
    });
  });
});