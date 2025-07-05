/**
 * Blockchain Gateway Integration Tests for COBOL Transpiler
 * Tests for smart contract deployment across Ethereum, Corda, and XRP networks
 * 
 * Task 2.4: Blockchain Gateway Integration
 * Testing contract deployment, flow deployment, and transaction status tracking
 */

const { EthereumL2Gateway } = require('../../src/blockchain/ethereum-l2-gateway');
const { CordaGateway } = require('../../src/blockchain/corda-gateway');
const { XRPGateway } = require('../../src/blockchain/xrp-gateway');
const { BlockchainDeploymentTracker, DEPLOYMENT_STATUS, DEPLOYMENT_TYPES } = require('../../src/blockchain/deployment-tracker');

describe('Blockchain Gateway COBOL Integration', () => {
  let ethereumGateway;
  let cordaGateway;
  let xrpGateway;
  let deploymentTracker;
  
  beforeEach(() => {
    // Initialize gateways in test mode
    ethereumGateway = new EthereumL2Gateway({
      testMode: true,
      privateKey: '0x' + '1'.repeat(64), // Test private key
      rpcUrl: 'http://localhost:8545'
    });
    
    cordaGateway = new CordaGateway({
      testMode: true,
      nodeUrl: 'http://localhost:10005',
      username: 'test',
      password: 'test'
    });
    
    xrpGateway = new XRPGateway({
      testMode: true,
      walletSeed: 'test_seed_for_testing_only',
      server: 'wss://s.altnet.rippletest.net:51233'
    });
    
    // Initialize deployment tracker
    deploymentTracker = new BlockchainDeploymentTracker({
      statusUpdateInterval: 1000 // 1 second for testing
    });
    
    // Register gateways with tracker
    deploymentTracker.registerGateway('ethereum-polygon', ethereumGateway);
    deploymentTracker.registerGateway('r3-corda', cordaGateway);
    deploymentTracker.registerGateway('xrp-ledger', xrpGateway);
  });
  
  afterEach(async () => {
    await deploymentTracker.cleanup();
    if (ethereumGateway.cleanup) await ethereumGateway.cleanup();
    if (cordaGateway.cleanup) await cordaGateway.cleanup();
    if (xrpGateway.cleanup) await xrpGateway.cleanup();
  });

  describe('Ethereum L2 Smart Contract Deployment', () => {
    it('should deploy COBOL-generated smart contract to Ethereum L2', async () => {
      // Arrange
      const contractData = {
        name: 'FISLoanCalculator',
        compiled: {
          abi: [
            {
              "inputs": [{"name": "amount", "type": "uint256"}],
              "name": "calculateLoan",
              "outputs": [{"name": "", "type": "uint256"}],
              "type": "function"
            }
          ],
          bytecode: '0x608060405234801561001057600080fd5b50610150806100206000396000f3fe608060405234801561001057600080fd5b506004361061002b5760003560e01c80632e52d60614610030575b600080fd5b61004a600480360381019061004591906100a7565b610060565b60405161005791906100e6565b60405180910390f35b60008160056100859190610130565b9050919050565b600080fd5b6000819050919050565b6100a481610091565b81146100af57600080fd5b50565b6000813590506100c18161009b565b92915050565b6000602082840312156100dd576100dc61008c565b5b60006100eb848285016100b2565b91505092915050565b6100fd81610091565b82525050565b600060208201905061011860008301846100f4565b92915050565b7f4e487b7100000000000000000000000000000000000000000000000000000000600052601160045260246000fd5b600061015982610091565b915061016483610091565b925082820261017281610091565b9150828204841483151761018957610188610122565b5b5092915050565b600a80910190565b600080fd5b600080fd5b600080fd5b600080fd5b6000801b81526000602082019050919050565b600080fd5b50565b7f416c6c6f77616e636520657863656564656400000000000000000000000000006000820152505056fea2646970667358221220' + '0'.repeat(64) + '64736f6c63430008110033'
        },
        constructorArgs: [],
        metadata: {
          sourceType: 'cobol',
          bankingSystem: 'FIS_SYSTEMATICS',
          sourceProgram: 'LOAN-CALC-01',
          transpilerVersion: '1.0.0'
        }
      };

      // Mock the gateway methods
      jest.spyOn(ethereumGateway, 'ensureConnection').mockResolvedValue(true);
      jest.spyOn(ethereumGateway, 'getGasPrice').mockResolvedValue({
        maxFeePerGas: '30000000000',
        maxPriorityFeePerGas: '2000000000'
      });
      
      // Mock ethers ContractFactory behavior
      const mockContract = {
        getAddress: jest.fn().mockResolvedValue('0x742d35Cc6436C0532925a3b8A11735f5a6dd5CB8'),
        deploymentTransaction: jest.fn().mockReturnValue({
          wait: jest.fn().mockResolvedValue({
            hash: '0xabcd1234',
            blockNumber: 12345,
            gasUsed: { toString: () => '500000' }
          })
        }),
        attach: jest.fn().mockReturnThis()
      };
      
      // Mock provider methods
      ethereumGateway.provider = {
        estimateGas: jest.fn().mockResolvedValue(BigInt(500000))
      };
      
      ethereumGateway.wallet = {
        address: '0x742d35Cc6436C0532925a3b8A11735f5a6dd5CB8'
      };
      
      // Mock ethers ContractFactory
      const mockFactory = {
        deploy: jest.fn().mockResolvedValue(mockContract),
        getDeployTransaction: jest.fn().mockResolvedValue({})
      };
      
      // Override the ContractFactory constructor
      const originalEthers = require('ethers');
      jest.spyOn(originalEthers, 'ContractFactory').mockImplementation(() => mockFactory);

      // Act
      const result = await ethereumGateway.deployCobolContract(contractData);

      // Assert
      expect(result.success).toBe(true);
      expect(result.contractAddress).toBe('0x742d35Cc6436C0532925a3b8A11735f5a6dd5CB8');
      expect(result.transactionHash).toBe('0xabcd1234');
      expect(result.networkType).toBe('ethereum-polygon');
      expect(result.contractInfo.metadata.bankingSystem).toBe('FIS_SYSTEMATICS');
    });

    it('should validate contract deployment requirements', () => {
      // Test missing contract data
      expect(() => ethereumGateway.validateContractDeployment(null))
        .toThrow('Contract data is required for deployment');
      
      // Test missing name
      expect(() => ethereumGateway.validateContractDeployment({}))
        .toThrow('Contract name is required');
      
      // Test missing compiled data
      expect(() => ethereumGateway.validateContractDeployment({ name: 'Test' }))
        .toThrow('Compiled contract data is required');
      
      // Test COBOL-specific validation
      const cobolContract = {
        name: 'Test',
        compiled: { abi: [], bytecode: '0x123' },
        metadata: { sourceType: 'cobol' }
      };
      
      expect(() => ethereumGateway.validateContractDeployment(cobolContract))
        .toThrow('Banking system is required for COBOL contracts');
    });
  });

  describe('Corda Flow Deployment', () => {
    it('should deploy COBOL-generated Corda flow', async () => {
      // Arrange
      const flowData = {
        name: 'FISPaymentFlow',
        compiled: {
          className: 'com.fis.flows.PaymentFlow'
        },
        deploymentArgs: [
          { type: 'Party', value: 'O=Bank,L=London,C=GB' },
          { type: 'Amount', value: { amount: 1000, currency: 'USD' } }
        ],
        metadata: {
          sourceType: 'cobol',
          bankingSystem: 'FIS_SYSTEMATICS',
          sourceProgram: 'PAYMENT-PROC-01',
          transpilerVersion: '1.0.0'
        }
      };

      // Mock Corda client
      cordaGateway.cordaClient = {
        startFlow: jest.fn().mockResolvedValue({ flowId: 'flow-123' }),
        waitForFlowCompletion: jest.fn().mockResolvedValue({ 
          status: 'SUCCESS',
          result: 'Payment processed successfully'
        })
      };
      
      jest.spyOn(cordaGateway, 'ensureConnection').mockResolvedValue(true);

      // Act
      const result = await cordaGateway.deployCobolFlow(flowData);

      // Assert
      expect(result.success).toBe(true);
      expect(result.flowId).toBe('flow-123');
      expect(result.flowName).toBe('com.fis.flows.PaymentFlow');
      expect(result.networkType).toBe('r3-corda');
      expect(result.flowInfo.metadata.bankingSystem).toBe('FIS_SYSTEMATICS');
    });

    it('should prepare flow arguments correctly for Corda', () => {
      // Arrange
      const args = [
        { type: 'Party', value: 'O=Bank,L=London,C=GB' },
        { type: 'Amount', value: { amount: 1000, currency: 'USD' } },
        { type: 'UniqueIdentifier', value: 'test-id' }
      ];
      
      const metadata = {
        sourceType: 'cobol',
        sourceProgram: 'TEST-PROG',
        bankingSystem: 'FIS_SYSTEMATICS'
      };

      // Act
      const preparedArgs = cordaGateway.prepareFlowArguments(args, metadata);

      // Assert
      expect(preparedArgs).toHaveLength(4); // 3 original + 1 metadata
      expect(preparedArgs[0].type).toBe('net.corda.core.identity.Party');
      expect(preparedArgs[1].type).toBe('net.corda.core.contracts.Amount');
      expect(preparedArgs[2].type).toBe('net.corda.core.contracts.UniqueIdentifier');
      expect(preparedArgs[3].type).toBe('java.lang.String');
      
      const metadataArg = JSON.parse(preparedArgs[3].value);
      expect(metadataArg.sourceProgram).toBe('TEST-PROG');
      expect(metadataArg.bankingSystem).toBe('FIS_SYSTEMATICS');
    });
  });

  describe('XRP Payment Contract Deployment', () => {
    it('should deploy COBOL-generated escrow contract to XRP Ledger', async () => {
      // Arrange
      const contractData = {
        name: 'FISEscrowContract',
        contractType: 'ESCROW',
        contractConfig: {
          destination: 'rDestinationAddress123456789',
          amount: '1000000', // 1 XRP in drops
          finishAfter: Date.now() + (24 * 60 * 60 * 1000) // 24 hours
        },
        metadata: {
          sourceType: 'cobol',
          bankingSystem: 'FISERV_DNA',
          sourceProgram: 'ESCROW-PROC-01',
          transpilerVersion: '1.0.0'
        }
      };

      // Mock XRP client and wallet
      xrpGateway.client = {
        autofill: jest.fn().mockResolvedValue({
          TransactionType: 'EscrowCreate',
          Sequence: 12345
        }),
        submitAndWait: jest.fn().mockResolvedValue({
          result: {
            hash: 'ABC123DEF456',
            ledger_index: 98765,
            Sequence: 12345,
            meta: { TransactionResult: 'tesSUCCESS' }
          }
        })
      };
      
      xrpGateway.wallet = {
        classicAddress: 'rSourceAddress123456789',
        sign: jest.fn().mockReturnValue({
          tx_blob: 'signed_transaction_blob'
        })
      };
      
      jest.spyOn(xrpGateway, 'ensureConnection').mockResolvedValue(true);

      // Act
      const result = await xrpGateway.deployCobolPaymentContract(contractData);

      // Assert
      expect(result.success).toBe(true);
      expect(result.contractType).toBe('ESCROW');
      expect(result.transactionHash).toBe('ABC123DEF456');
      expect(result.networkType).toBe('xrp-ledger');
      expect(result.contractInfo.metadata.bankingSystem).toBe('FISERV_DNA');
    });

    it('should deploy COBOL-generated payment channel contract to XRP Ledger', async () => {
      // Arrange
      const contractData = {
        name: 'FISPaymentChannel',
        contractType: 'PAYMENT_CHANNEL',
        contractConfig: {
          destination: 'rDestinationAddress123456789',
          amount: '10000000', // 10 XRP in drops
          settleDelay: 86400, // 1 day
          publicKey: 'ED' + '0'.repeat(62)
        },
        metadata: {
          sourceType: 'cobol',
          bankingSystem: 'FISERV_DNA',
          sourceProgram: 'PAYMENT-CHANNEL-01'
        }
      };

      // Mock XRP client and wallet
      xrpGateway.client = {
        autofill: jest.fn().mockResolvedValue({
          TransactionType: 'PaymentChannelCreate'
        }),
        submitAndWait: jest.fn().mockResolvedValue({
          result: {
            hash: 'CHANNEL123DEF456',
            ledger_index: 98766,
            meta: { TransactionResult: 'tesSUCCESS' }
          }
        })
      };
      
      xrpGateway.wallet = {
        classicAddress: 'rSourceAddress123456789',
        publicKey: 'ED' + '0'.repeat(62),
        sign: jest.fn().mockReturnValue({
          tx_blob: 'signed_transaction_blob'
        })
      };
      
      jest.spyOn(xrpGateway, 'ensureConnection').mockResolvedValue(true);

      // Act
      const result = await xrpGateway.deployCobolPaymentContract(contractData);

      // Assert
      expect(result.success).toBe(true);
      expect(result.contractType).toBe('PAYMENT_CHANNEL');
      expect(result.transactionHash).toBe('CHANNEL123DEF456');
      expect(result.contractInfo.type).toBe('PAYMENT_CHANNEL');
      expect(result.contractInfo.channelId).toBe('CHANNEL123DEF456');
    });
  });

  describe('Deployment Status Tracking', () => {
    it('should track deployment status across all networks', async () => {
      // Arrange
      const deploymentInfo = {
        deploymentId: 'deploy-123',
        networkType: 'ethereum-polygon',
        deploymentType: DEPLOYMENT_TYPES.SMART_CONTRACT,
        contractName: 'TestContract',
        bankingSystem: 'FIS_SYSTEMATICS',
        sourceProgram: 'TEST-PROG-01'
      };

      // Act
      const trackingId = deploymentTracker.trackDeployment(deploymentInfo);
      
      // Assert
      expect(trackingId).toBeDefined();
      
      const status = deploymentTracker.getDeploymentStatus(trackingId);
      expect(status).toBeDefined();
      expect(status.trackingId).toBe(trackingId);
      expect(status.status).toBe(DEPLOYMENT_STATUS.PENDING);
      expect(status.networkType).toBe('ethereum-polygon');
      expect(status.contractName).toBe('TestContract');
    });

    it('should update deployment status when gateway events are emitted', async () => {
      // Arrange
      const deploymentInfo = {
        deploymentId: 'deploy-456',
        networkType: 'ethereum-polygon',
        deploymentType: DEPLOYMENT_TYPES.SMART_CONTRACT,
        contractName: 'TestContract',
        bankingSystem: 'FIS_SYSTEMATICS'
      };
      
      const trackingId = deploymentTracker.trackDeployment(deploymentInfo);

      // Act - Simulate gateway success event
      ethereumGateway.emit('contractDeployed', {
        deploymentId: 'deploy-456',
        contractName: 'TestContract',
        address: '0x123',
        txHash: '0xabc',
        deploymentTime: 5000
      });

      // Wait a bit for event processing
      await new Promise(resolve => setTimeout(resolve, 100));

      // Assert
      const status = deploymentTracker.getDeploymentStatus(trackingId);
      expect(status.status).toBe(DEPLOYMENT_STATUS.CONFIRMED);
      expect(status.metadata.transactionHash).toBe('0xabc');
      expect(status.metadata.contractAddress).toBe('0x123');
    });

    it('should track metrics correctly', () => {
      // Arrange & Act
      deploymentTracker.trackDeployment({
        deploymentId: 'deploy-1',
        networkType: 'ethereum-polygon',
        deploymentType: DEPLOYMENT_TYPES.SMART_CONTRACT,
        contractName: 'Contract1',
        bankingSystem: 'FIS_SYSTEMATICS'
      });
      
      deploymentTracker.trackDeployment({
        deploymentId: 'deploy-2',
        networkType: 'r3-corda',
        deploymentType: DEPLOYMENT_TYPES.CORDA_FLOW,
        contractName: 'Flow1',
        bankingSystem: 'TEMENOS_TRANSACT'
      });

      // Assert
      const metrics = deploymentTracker.getMetrics();
      expect(metrics.totalDeployments).toBe(2);
      expect(metrics.activeDeployments).toBe(2);
      expect(metrics.networkDeployments['ethereum-polygon'].total).toBe(1);
      expect(metrics.networkDeployments['r3-corda'].total).toBe(1);
    });
  });

  describe('Error Handling and Validation', () => {
    it('should handle deployment failures gracefully', async () => {
      // Arrange
      const deploymentInfo = {
        deploymentId: 'deploy-fail',
        networkType: 'ethereum-polygon',
        deploymentType: DEPLOYMENT_TYPES.SMART_CONTRACT,
        contractName: 'FailContract',
        bankingSystem: 'FIS_SYSTEMATICS'
      };
      
      const trackingId = deploymentTracker.trackDeployment(deploymentInfo);

      // Act - Simulate gateway failure event
      ethereumGateway.emit('contractDeploymentFailed', {
        deploymentId: 'deploy-fail',
        contractName: 'FailContract',
        error: 'Insufficient gas',
        deploymentTime: 2000
      });

      await new Promise(resolve => setTimeout(resolve, 100));

      // Assert
      const status = deploymentTracker.getDeploymentStatus(trackingId);
      expect(status.status).toBe(DEPLOYMENT_STATUS.FAILED);
      expect(status.metadata.error).toBe('Insufficient gas');
    });

    it('should validate payment contract requirements for XRP', () => {
      // Test missing destination
      const invalidContract = {
        name: 'TestContract',
        contractConfig: {}
      };
      
      expect(() => xrpGateway.validatePaymentContractDeployment(invalidContract))
        .toThrow('Destination address is required for payment contracts');
      
      // Test COBOL-specific validation
      const cobolContract = {
        name: 'TestContract',
        contractConfig: { destination: 'rABC123' },
        metadata: { sourceType: 'cobol' }
      };
      
      expect(() => xrpGateway.validatePaymentContractDeployment(cobolContract))
        .toThrow('Banking system is required for COBOL contracts');
    });
  });
});