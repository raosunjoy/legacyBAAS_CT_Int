/**
 * Corda Gateway Tests
 * Comprehensive test suite for R3 Corda integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const { CordaGateway, CORDA_FLOW_TYPES, CORDA_STATES } = require('../../src/blockchain/corda-gateway');
const { TRANSACTION_STATUS, NETWORK_TYPES } = require('../../src/blockchain/base-gateway');

// Mock Hyperledger Cactus Corda Connector
jest.mock('@hyperledger/cactus-plugin-ledger-connector-corda', () => {
  return {
    PluginLedgerConnectorCorda: jest.fn()
  };
});

describe('Corda Gateway', () => {
  let gateway;
  let mockTransaction;
  let mockCorda;
  let mockClient;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Set up detailed mocks for Corda connector
    mockClient = {
      invokeContractV1: jest.fn(),
      shutdown: jest.fn().mockResolvedValue(true),
      nodeInfo: jest.fn(),
      networkMap: jest.fn(),
      vaultQuery: jest.fn(),
      startFlow: jest.fn(),
      getFlowProgress: jest.fn(),
      flowStatus: jest.fn(),
      flowKill: jest.fn(),
      networkHealthCheck: jest.fn(),
      partyFromX500Name: jest.fn(),
      getFlowsForTransactionType: jest.fn(),
      getNodeInfo: jest.fn(),
      performHealthCheck: jest.fn()
    };
    
    // Mock Corda Connector
    mockCorda = require('@hyperledger/cactus-plugin-ledger-connector-corda');
    mockCorda.PluginLedgerConnectorCorda.mockReturnValue(mockClient);
    
    gateway = new CordaGateway({
      testMode: true,
      nodeUrl: 'http://localhost:10005',
      username: 'testuser',
      password: 'testpass',
      legalName: 'O=TestBank,L=London,C=GB',
      retryAttempts: 2
    });

    mockTransaction = {
      id: 'test-tx-123',
      amount: 50000,
      currency: 'GBP',
      sender: { 
        name: 'HSBC Bank', 
        account: 'ACC123',
        legalName: 'O=HSBC,L=London,C=GB'
      },
      receiver: { 
        name: 'Barclays Bank', 
        account: 'ACC456',
        legalName: 'O=Barclays,L=London,C=GB'
      },
      transactionReference: 'REF789'
    };
  });

  afterEach(() => {
    gateway.removeAllListeners();
    gateway.clearTransactionHistory();
    if (gateway.vaultMonitoringInterval) {
      clearInterval(gateway.vaultMonitoringInterval);
    }
  });

  describe('Constructor', () => {
    test('should initialize with Corda-specific configuration', () => {
      expect(gateway.networkType).toBe(NETWORK_TYPES.CORDA);
      expect(gateway.config.nodeUrl).toBe('http://localhost:10005');
      expect(gateway.config.testMode).toBe(true);
      expect(gateway.config.legalName).toBe('O=TestBank,L=London,C=GB');
      expect(gateway.config.flowTimeout).toBe(300000);
      expect(gateway.config.fatfReporting).toBe(true);
    });

    test('should use production URL when not in test mode', () => {
      process.env.CORDA_NODE_URL = 'http://prod-node:10005';
      const prodGateway = new CordaGateway({ testMode: false });
      expect(prodGateway.config.nodeUrl).toBe('http://prod-node:10005');
      delete process.env.CORDA_NODE_URL;
    });

    test('should initialize collections and state', () => {
      expect(gateway.activeFlows).toBeInstanceOf(Map);
      expect(gateway.vaultStates).toBeInstanceOf(Map);
      expect(gateway.partyCache).toBeInstanceOf(Map);
      expect(gateway.cordaClient).toBeNull();
      expect(gateway.nodeInfo).toBeNull();
    });
  });

  describe('Connection Management', () => {
    test('should connect to Corda Node successfully', async () => {
      mockClient.invokeContractV1.mockResolvedValue({
        callData: { success: true }
      });

      const result = await gateway.connect();

      expect(result).toBe(true);
      expect(gateway.isConnected).toBe(true);
      expect(mockCorda.PluginLedgerConnectorCorda).toHaveBeenCalledWith({
        rpcApiHttpHost: 'localhost',
        rpcApiHttpPort: 10005,
        rpcApiUsername: gateway.config.username,
        rpcApiPassword: gateway.config.password,
        logLevel: 'INFO',
        instanceId: expect.any(String),
        apiClientTimeoutMs: 30000
      });
      expect(gateway.nodeInfo.platformVersion).toBe(4.8);
    });

    test('should handle connection failure', async () => {
      mockCorda.PluginLedgerConnectorCorda.mockImplementation(() => {
        throw new Error('Connection failed');
      });

      await expect(gateway.connect()).rejects.toThrow('Corda Node connection failed: Connection failed');
      expect(gateway.isConnected).toBe(false);
      expect(gateway.connectionAttempts).toBe(1);
    });

    test('should disconnect successfully', async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();
      const result = await gateway.disconnect();

      expect(result).toBe(true);
      expect(gateway.isConnected).toBe(false);
      expect(gateway.cordaClient).toBeNull();
      expect(gateway.nodeInfo).toBeNull();
    });

    test('should handle disconnection when not connected', async () => {
      const result = await gateway.disconnect();
      expect(result).toBe(true);
    });

    test('should warn on legal name mismatch', async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [
          { name: 'O=DifferentBank,L=London,C=GB' }
        ]
      });

      await gateway.connect();
      expect(gateway.isConnected).toBe(true);
    });
  });

  describe('Transaction Submission', () => {
    beforeEach(async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      mockClient.networkMap.mockResolvedValue([
        {
          legalIdentities: [
            { name: 'O=HSBC,L=London,C=GB', owningKey: 'hsbc-key' }
          ]
        },
        {
          legalIdentities: [
            { name: 'O=Barclays,L=London,C=GB', owningKey: 'barclays-key' }
          ]
        }
      ]);
      await gateway.connect();
    });

    test('should submit Corda flow successfully', async () => {
      const mockFlowResult = {
        stateRef: 'state-ref-123',
        stateRefs: ['state-ref-123'],
        id: 'tx-hash-456',
        notarised: true
      };

      // Mock invokeContractV1 which is what the gateway actually calls
      mockClient.invokeContractV1.mockResolvedValue({
        callData: {
          flowId: 'flow-123',
          success: true,
          stateRef: 'state-ref-123',
          result: mockFlowResult
        }
      });

      const result = await gateway.submitTransaction(mockTransaction);

      expect(result.id).toBe('state-ref-123');
      expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(result.notarised).toBe(true);
      expect(mockClient.startFlow).toHaveBeenCalledWith(
        expect.objectContaining({
          flowClassName: CORDA_FLOW_TYPES.PAYMENT,
          flowArgs: expect.objectContaining({
            amount: expect.objectContaining({
              quantity: 5000000, // 50000 * 100
              token: expect.objectContaining({
                currency: 'GBP'
              })
            })
          })
        })
      );
    });

    test('should include FATF reporting for transactions >= $1000', async () => {
      const mockFlowResult = { stateRef: 'state-ref-123', notarised: true };

      // Mock invokeContractV1 for high-value transaction
      mockClient.invokeContractV1.mockResolvedValue({
        callData: {
          flowId: 'flow-123',
          success: true,
          stateRef: 'state-ref-123',
          result: mockFlowResult
        }
      });

      await gateway.submitTransaction(mockTransaction);

      expect(mockClient.invokeContractV1).toHaveBeenCalledWith(
        expect.objectContaining({
          params: expect.arrayContaining([
            expect.objectContaining({
              fatfReporting: expect.objectContaining({
                travelRule: true,
                originatorInfo: expect.objectContaining({
                  name: 'HSBC Bank'
                }),
                beneficiaryInfo: expect.objectContaining({
                  name: 'Barclays Bank'
                })
              })
            })
          ])
        })
      );
    });

    test('should include regulatory reporting for high-value transactions', async () => {
      const highValueTx = { ...mockTransaction, amount: 15000 };
      const mockFlowHandle = { id: 'flow-123' };
      const mockFlowResult = { stateRef: 'state-ref-123', notarised: true };

      mockClient.startFlow.mockResolvedValue(mockFlowHandle);
      mockClient.getFlowProgress.mockResolvedValue({
        finished: true,
        result: mockFlowResult
      });

      await gateway.submitTransaction(highValueTx);

      expect(mockClient.startFlow).toHaveBeenCalledWith(
        expect.objectContaining({
          flowArgs: expect.objectContaining({
            regulatoryReporting: expect.objectContaining({
              reportingRequirement: 'SUSPICIOUS_ACTIVITY',
              jurisdictions: ['GB', 'EU'],
              thresholdAmount: 10000
            })
          })
        })
      );
    });

    test('should handle flow execution failure', async () => {
      const mockFlowHandle = { id: 'flow-123' };
      
      mockClient.startFlow.mockResolvedValue(mockFlowHandle);
      mockClient.getFlowProgress.mockResolvedValue({
        finished: true,
        error: 'Insufficient funds'
      });

      await expect(gateway.submitTransaction(mockTransaction)).rejects.toThrow('Flow failed: Insufficient funds');
      expect(gateway.activeFlows.has(mockTransaction.id)).toBe(false);
    });

    test('should handle flow timeout', async () => {
      gateway.config.flowTimeout = 100; // Very short timeout
      const mockFlowHandle = { id: 'flow-123' };
      
      mockClient.startFlow.mockResolvedValue(mockFlowHandle);
      mockClient.getFlowProgress.mockResolvedValue({
        finished: false,
        progress: 0.5
      });

      await expect(gateway.submitTransaction(mockTransaction)).rejects.toThrow('Flow timeout after 100ms');
    });

    test('should throw error when not connected', async () => {
      gateway.cordaClient = null;
      
      await expect(gateway.submitTransaction(mockTransaction)).rejects.toThrow('Corda client not connected');
    });

    test('should handle trade finance transactions', async () => {
      const tradeTransaction = { ...mockTransaction, tradeFinance: true };
      const mockFlowHandle = { id: 'flow-123' };
      const mockFlowResult = { stateRef: 'state-ref-123', notarised: true };

      mockClient.startFlow.mockResolvedValue(mockFlowHandle);
      mockClient.getFlowProgress.mockResolvedValue({
        finished: true,
        result: mockFlowResult
      });

      await gateway.submitTransaction(tradeTransaction);

      expect(mockClient.startFlow).toHaveBeenCalledWith(
        expect.objectContaining({
          flowClassName: CORDA_FLOW_TYPES.TRADE_FINANCE
        })
      );
    });
  });

  describe('Transaction Status Queries', () => {
    beforeEach(async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();
      
      // Add a tracked transaction
      gateway.trackTransaction('test-tx-123', {
        id: 'state-ref-123',
        status: TRANSACTION_STATUS.SUBMITTED
      });
    });

    test('should get status for active flow', async () => {
      // Add active flow
      gateway.activeFlows.set('test-tx-123', {
        flowId: 'flow-123',
        startTime: Date.now(),
        transaction: mockTransaction
      });

      mockClient.getFlowProgress.mockResolvedValue({
        finished: false,
        progress: 0.7
      });

      const status = await gateway.getTransactionStatus('test-tx-123');

      expect(status.transactionId).toBe('test-tx-123');
      expect(status.status).toBe(TRANSACTION_STATUS.PENDING);
      expect(status.flowId).toBe('flow-123');
      expect(status.progress).toBe(0.7);
    });

    test('should get status from vault for completed transaction', async () => {
      const mockVaultState = {
        ref: 'state-ref-123',
        state: {
          data: {
            participants: ['party1', 'party2'],
            lifecycle: 'UNCONSUMED',
            linearId: { externalId: 'test-tx-123' }
          },
          notary: 'O=Notary,L=London,C=GB'
        },
        notarySignature: 'signature-123'
      };

      mockClient.vaultQuery.mockResolvedValue({
        states: [mockVaultState]
      });

      const status = await gateway.getTransactionStatus('test-tx-123');

      expect(status.transactionId).toBe('test-tx-123');
      expect(status.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(status.stateRef).toBe('state-ref-123');
      expect(status.participants).toBe(2);
    });

    test('should throw error for unknown transaction', async () => {
      await expect(gateway.getTransactionStatus('unknown-tx')).rejects.toThrow('Transaction unknown-tx not found in history');
    });

    test('should handle vault query failure', async () => {
      mockClient.vaultQuery.mockResolvedValue({
        states: []
      });

      const status = await gateway.getTransactionStatus('test-tx-123');
      expect(status.status).toBe(TRANSACTION_STATUS.FAILED);
      expect(status.error).toBe('Transaction not found in vault');
    });
  });

  describe('Network Health', () => {
    beforeEach(async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();
    });

    test('should get network health successfully', async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [
          { name: 'O=TestBank,L=London,C=GB' }
        ]
      });

      mockClient.networkMap.mockResolvedValue([
        { legalIdentities: [{ name: 'O=Bank1,L=London,C=GB' }] },
        { legalIdentities: [{ name: 'O=Bank2,L=London,C=GB' }] }
      ]);

      mockClient.listFlows.mockResolvedValue([
        { id: 'flow1' },
        { id: 'flow2' }
      ]);

      mockClient.vaultQuery.mockResolvedValue({
        totalStatesAvailable: 150
      });

      const health = await gateway.getNetworkHealth();

      expect(health.networkType).toBe(NETWORK_TYPES.CORDA);
      expect(health.isHealthy).toBe(true);
      expect(health.nodeVersion).toBe(4.8);
      expect(health.networkNodes).toBe(2);
      expect(health.activeFlows).toBe(2);
      expect(health.vaultSize).toBe(150);
    });

    test('should detect unhealthy network conditions', async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 3.5, // Old version
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });

      mockClient.networkMap.mockResolvedValue([]); // No nodes
      mockClient.listFlows.mockResolvedValue([]);
      mockClient.vaultQuery.mockResolvedValue({ totalStatesAvailable: 0 });

      const health = await gateway.getNetworkHealth();

      expect(health.isHealthy).toBe(false);
    });

    test('should handle network health check failure', async () => {
      gateway.cordaClient = null;

      const health = await gateway.getNetworkHealth();

      expect(health.networkType).toBe(NETWORK_TYPES.CORDA);
      expect(health.isHealthy).toBe(false);
      expect(health.error).toBeDefined();
    });
  });

  describe('Party Resolution', () => {
    beforeEach(async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();
    });

    test('should resolve party from network map', async () => {
      mockClient.networkMap.mockResolvedValue([
        {
          legalIdentities: [
            { name: 'O=HSBC,L=London,C=GB', owningKey: 'hsbc-key' }
          ]
        }
      ]);

      const party = await gateway.resolveParty({ name: 'HSBC' });
      
      expect(party.name).toBe('O=HSBC,L=London,C=GB');
      expect(party.owningKey).toBe('hsbc-key');
    });

    test('should use cached party resolution', async () => {
      const cachedParty = { name: 'O=HSBC,L=London,C=GB', owningKey: 'cached-key' };
      gateway.partyCache.set('HSBC Bank', cachedParty);

      const party = await gateway.resolveParty({ name: 'HSBC Bank' });
      
      expect(party).toBe(cachedParty);
      expect(mockClient.networkMap).not.toHaveBeenCalled();
    });

    test('should return test party in test mode for unknown party', async () => {
      mockClient.networkMap.mockResolvedValue([]);

      const party = await gateway.resolveParty({ name: 'Unknown Bank' });
      
      expect(party.name).toContain('Unknown Bank');
      expect(party.owningKey).toContain('test-key');
    });

    test('should throw error for unknown party in production mode', async () => {
      const prodGateway = new CordaGateway({ testMode: false });
      
      // Set up the client for production gateway
      prodGateway.cordaClient = mockClient;
      mockClient.networkMap.mockResolvedValue([]);
      
      await expect(prodGateway.resolveParty({ name: 'Unknown Bank' })).rejects.toThrow('Cannot resolve Corda party for Unknown Bank');
    });
  });

  describe('Vault Operations', () => {
    beforeEach(async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();
    });

    test('should query vault with criteria', async () => {
      const mockStates = [
        { ref: 'state1', state: { data: { amount: 1000 } } },
        { ref: 'state2', state: { data: { amount: 2000 } } }
      ];

      mockClient.vaultQuery.mockResolvedValue({
        states: mockStates
      });

      const criteria = {
        contractStateType: CORDA_STATES.CASH_STATE,
        criteria: { 'amount.quantity': { '$gte': 1000 } }
      };

      const states = await gateway.queryVault(criteria);

      expect(states).toHaveLength(2);
      expect(mockClient.vaultQuery).toHaveBeenCalledWith(
        expect.objectContaining({
          contractStateType: CORDA_STATES.CASH_STATE,
          criteria: criteria.criteria
        })
      );
    });

    test('should get vault size', async () => {
      mockClient.vaultQuery.mockResolvedValue({
        totalStatesAvailable: 42
      });

      const size = await gateway.getVaultSize();
      expect(size).toBe(42);
    });

    test('should handle vault size query failure', async () => {
      mockClient.vaultQuery.mockRejectedValue(new Error('Vault error'));

      const size = await gateway.getVaultSize();
      expect(size).toBe(0);
    });

    test('should update vault cache', async () => {
      const mockStates = [
        {
          state: {
            data: {
              linearId: { externalId: 'tx-1' }
            }
          }
        },
        {
          state: {
            data: {
              linearId: { externalId: 'tx-2' }
            }
          }
        }
      ];

      mockClient.vaultQuery.mockResolvedValue({
        states: mockStates
      });

      await gateway.updateVaultCache();

      expect(gateway.vaultStates.size).toBe(2);
      expect(gateway.vaultStates.has('tx-1')).toBe(true);
      expect(gateway.vaultStates.has('tx-2')).toBe(true);
    });
  });

  describe('Flow Management', () => {
    test('should map Corda flow status correctly', () => {
      const successResult = { notarised: true, stateRefs: ['ref1'] };
      expect(gateway.mapCordaFlowStatus(successResult)).toBe(TRANSACTION_STATUS.CONFIRMED);

      const errorResult = { error: 'Flow failed' };
      expect(gateway.mapCordaFlowStatus(errorResult)).toBe(TRANSACTION_STATUS.FAILED);

      const submittedResult = { stateRefs: ['ref1'] };
      expect(gateway.mapCordaFlowStatus(submittedResult)).toBe(TRANSACTION_STATUS.SUBMITTED);

      const pendingResult = {};
      expect(gateway.mapCordaFlowStatus(pendingResult)).toBe(TRANSACTION_STATUS.PENDING);
    });

    test('should map vault state status correctly', () => {
      const confirmedState = {
        state: { notary: 'notary1', data: { lifecycle: 'UNCONSUMED' } },
        notarySignature: 'sig1'
      };
      expect(gateway.mapCordaStatus(confirmedState)).toBe(TRANSACTION_STATUS.CONFIRMED);

      const submittedState = {
        state: { data: { lifecycle: 'UNCONSUMED' } }
      };
      expect(gateway.mapCordaStatus(submittedState)).toBe(TRANSACTION_STATUS.SUBMITTED);
    });

    test('should extract originator and beneficiary info', () => {
      const sender = {
        name: 'John Doe',
        address: '123 Main St',
        account: 'ACC123',
        nationalId: 'ID123'
      };

      const originatorInfo = gateway.extractOriginatorInfo(sender);
      expect(originatorInfo.name).toBe('John Doe');
      expect(originatorInfo.address).toBe('123 Main St');
      expect(originatorInfo.accountNumber).toBe('ACC123');
      expect(originatorInfo.nationalId).toBe('ID123');
    });
  });

  describe('Transaction Monitoring', () => {
    beforeEach(async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();
    });

    test('should start vault monitoring', () => {
      expect(gateway.vaultMonitoringInterval).toBeDefined();
    });

    test('should stop monitoring on cleanup', async () => {
      const intervalId = gateway.vaultMonitoringInterval;
      expect(intervalId).toBeDefined();

      await gateway.cleanup();
      expect(gateway.vaultMonitoringInterval).toBeNull();
    });

    test('should cancel active flows on cleanup', async () => {
      gateway.activeFlows.set('tx-1', { flowId: 'flow-1' });
      gateway.activeFlows.set('tx-2', { flowId: 'flow-2' });

      await gateway.cleanup();

      expect(mockClient.killFlow).toHaveBeenCalledWith('flow-1');
      expect(mockClient.killFlow).toHaveBeenCalledWith('flow-2');
    });
  });

  describe('Error Handling', () => {
    test('should handle missing Corda SDK gracefully', () => {
      // Mock missing Corda SDK
      jest.doMock('@hyperledger/cactus-plugin-ledger-connector-corda', () => {
        throw new Error('Module not found');
      });

      expect(() => {
        delete require.cache[require.resolve('../../src/blockchain/corda-gateway')];
        require('../../src/blockchain/corda-gateway');
      }).not.toThrow();
    });

    test('should handle vault monitoring errors gracefully', async () => {
      mockClient.nodeInfo.mockResolvedValue({
        platformVersion: 4.8,
        legalIdentities: [{ name: 'O=TestBank,L=London,C=GB' }]
      });
      await gateway.connect();

      mockClient.vaultQuery.mockRejectedValue(new Error('Vault error'));

      // Should not throw when vault monitoring fails
      await expect(gateway.updateVaultCache()).resolves.toBeUndefined();
    });
  });

  describe('Constants Export', () => {
    test('should export Corda flow types', () => {
      expect(CORDA_FLOW_TYPES.PAYMENT).toBe('PaymentFlow');
      expect(CORDA_FLOW_TYPES.TRADE_FINANCE).toBe('TradeFinanceFlow');
      expect(CORDA_FLOW_TYPES.COMPLIANCE_CHECK).toBe('ComplianceFlow');
    });

    test('should export Corda state types', () => {
      expect(CORDA_STATES.CASH_STATE).toBe('CashState');
      expect(CORDA_STATES.OBLIGATION_STATE).toBe('ObligationState');
      expect(CORDA_STATES.TRADE_STATE).toBe('TradeState');
    });
  });
});