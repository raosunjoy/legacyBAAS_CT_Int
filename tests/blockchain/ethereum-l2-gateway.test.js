/**
 * Ethereum L2 Gateway Tests
 * Comprehensive test suite for Ethereum L2 (Polygon) integration
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const { EthereumL2Gateway, CONTRACT_TYPES, TOKEN_STANDARDS, DEFAULT_CONTRACTS } = require('../../src/blockchain/ethereum-l2-gateway');
const { TRANSACTION_STATUS, NETWORK_TYPES } = require('../../src/blockchain/base-gateway');

// Mock ethers.js library
jest.mock('ethers', () => {
  const mockProvider = {
    getNetwork: jest.fn(),
    getBlockNumber: jest.fn(),
    getGasPrice: jest.fn(),
    getFeeData: jest.fn(),
    getTransactionReceipt: jest.fn(),
    resolveName: jest.fn()
  };

  const mockWallet = {
    address: '0x742d35Cc6634C0532925a3b8D4c8C9df4C5C1234',
    connect: jest.fn().mockReturnThis()
  };

  const mockContract = {
    transfer: jest.fn(),
    approve: jest.fn(),
    balanceOf: jest.fn(),
    decimals: jest.fn(),
    symbol: jest.fn(),
    name: jest.fn()
  };
  
  // Add estimateGas to contract methods
  mockContract.transfer.estimateGas = jest.fn();
  mockContract.approve.estimateGas = jest.fn();

  const WalletConstructor = jest.fn(() => mockWallet);
  WalletConstructor.fromPhrase = jest.fn(() => mockWallet);

  return {
    JsonRpcProvider: jest.fn(() => mockProvider),
    Wallet: WalletConstructor,
    Contract: jest.fn(() => mockContract),
    isAddress: jest.fn(addr => addr && addr.startsWith('0x') && addr.length === 42),
    parseUnits: jest.fn((value, decimals) => BigInt(value) * BigInt(10 ** decimals)),
    formatUnits: jest.fn((value, decimals) => {
      const num = typeof value === 'bigint' ? Number(value) : Number(value);
      if (decimals === 'gwei') decimals = 9;
      return (num / Math.pow(10, decimals)).toString();
    }),
    parseEther: jest.fn(value => BigInt(value) * BigInt(10 ** 18)),
    formatEther: jest.fn(value => (Number(value) / 10 ** 18).toString())
  };
});

describe('Ethereum L2 Gateway', () => {
  let gateway;
  let mockTransaction;
  let mockEthers;
  let mockProvider;
  let mockWallet;
  let mockContract;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Set up detailed mocks
    mockEthers = require('ethers');
    mockProvider = new mockEthers.JsonRpcProvider();
    mockWallet = { 
      address: '0x742d35Cc6634C0532925a3b8D4c8C9df4C5C1234',
      connect: jest.fn().mockReturnThis()
    };
    mockContract = {
      transfer: jest.fn(),
      approve: jest.fn(),
      balanceOf: jest.fn(),
      decimals: jest.fn(),
      symbol: jest.fn(),
      name: jest.fn()
    };
    
    // Add estimateGas methods
    mockContract.transfer.estimateGas = jest.fn().mockResolvedValue(BigInt(21000));
    mockContract.approve.estimateGas = jest.fn().mockResolvedValue(BigInt(21000));

    // Mock constructor returns
    mockEthers.JsonRpcProvider.mockReturnValue(mockProvider);
    mockEthers.Wallet.mockReturnValue(mockWallet);
    mockEthers.Wallet.fromPhrase.mockReturnValue(mockWallet);
    mockEthers.Contract.mockReturnValue(mockContract);
    
    gateway = new EthereumL2Gateway({
      testMode: true,
      rpcUrl: 'https://rpc-mumbai.maticvigil.com',
      chainId: 80001,
      privateKey: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
      retryAttempts: 2
    });

    mockTransaction = {
      id: 'test-tx-123',
      amount: 1000,
      currency: 'USDC',
      type: 'tokenTransfer',
      sender: { 
        name: 'Alice', 
        account: 'ACC123',
        ethereumAddress: '0x1234567890123456789012345678901234567890'
      },
      receiver: { 
        name: 'Bob', 
        account: 'ACC456',
        ethereumAddress: '0x0987654321098765432109876543210987654321'
      }
    };
  });

  afterEach(() => {
    gateway.removeAllListeners();
    gateway.clearTransactionHistory();
    if (gateway.gasPriceMonitoringInterval) {
      clearInterval(gateway.gasPriceMonitoringInterval);
    }
  });

  describe('Constructor', () => {
    test('should initialize with Ethereum L2-specific configuration', () => {
      expect(gateway.networkType).toBe(NETWORK_TYPES.ETHEREUM_L2);
      expect(gateway.config.rpcUrl).toBe('https://rpc-mumbai.maticvigil.com');
      expect(gateway.config.chainId).toBe(80001);
      expect(gateway.config.testMode).toBe(true);
      expect(gateway.config.gasLimit).toBe(2000000);
      expect(gateway.config.enableYieldFarming).toBe(true);
    });

    test('should use production RPC when not in test mode', () => {
      const prodGateway = new EthereumL2Gateway({ testMode: false });
      expect(prodGateway.config.rpcUrl).toBe('https://polygon-rpc.com');
      expect(prodGateway.config.chainId).toBe(137);
    });

    test('should initialize collections and state', () => {
      expect(gateway.contracts).toBeInstanceOf(Map);
      expect(gateway.balances).toBeInstanceOf(Map);
      expect(gateway.provider).toBeNull();
      expect(gateway.wallet).toBeNull();
    });

    test('should include default contract addresses', () => {
      expect(gateway.config.contracts.USDC).toBe(DEFAULT_CONTRACTS.USDC);
      expect(gateway.config.contracts.USDT).toBe(DEFAULT_CONTRACTS.USDT);
      expect(gateway.config.contracts.DAI).toBe(DEFAULT_CONTRACTS.DAI);
    });
  });

  describe('Connection Management', () => {
    test('should connect to Ethereum L2 network successfully', async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      mockProvider.getBlockNumber.mockResolvedValue(1000000);

      const result = await gateway.connect();

      expect(result).toBe(true);
      expect(gateway.isConnected).toBe(true);
      expect(mockEthers.JsonRpcProvider).toHaveBeenCalledWith(gateway.config.rpcUrl);
      expect(gateway.wallet.address).toBe('0x742d35Cc6634C0532925a3b8D4c8C9df4C5C1234');
    });

    test('should handle chain ID mismatch', async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(1) }); // Wrong chain

      await expect(gateway.connect()).rejects.toThrow('Chain ID mismatch: expected 80001, got 1');
      expect(gateway.isConnected).toBe(false);
    });

    test('should connect with mnemonic', async () => {
      const mnemonicGateway = new EthereumL2Gateway({
        testMode: true,
        mnemonic: 'test mnemonic phrase'
      });

      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      mockProvider.getBlockNumber.mockResolvedValue(1000000);

      await mnemonicGateway.connect();
      expect(mockEthers.Wallet.fromPhrase).toHaveBeenCalledWith('test mnemonic phrase');
    });

    test('should connect in read-only mode without wallet', async () => {
      const readOnlyGateway = new EthereumL2Gateway({ testMode: true });

      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      mockProvider.getBlockNumber.mockResolvedValue(1000000);

      await readOnlyGateway.connect();
      expect(readOnlyGateway.wallet).toBeNull();
    });

    test('should disconnect successfully', async () => {
      await gateway.connect();
      const result = await gateway.disconnect();

      expect(result).toBe(true);
      expect(gateway.isConnected).toBe(false);
      expect(gateway.provider).toBeNull();
      expect(gateway.wallet).toBeNull();
    });

    test('should handle connection failure', async () => {
      // Create a new gateway instance for this test to avoid affecting other tests
      const failingGateway = new EthereumL2Gateway({
        testMode: true,
        rpcUrl: 'https://rpc-mumbai.maticvigil.com',
        chainId: 80001,
        privateKey: '0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef',
        retryAttempts: 2
      });

      // Mock provider constructor to throw error for this specific test
      const originalImplementation = mockEthers.JsonRpcProvider;
      mockEthers.JsonRpcProvider.mockImplementationOnce(() => {
        throw new Error('Network error');
      });

      await expect(failingGateway.connect()).rejects.toThrow('Ethereum L2 connection failed: Network error');
      expect(failingGateway.connectionAttempts).toBe(1);
      
      // Restore original implementation
      mockEthers.JsonRpcProvider = originalImplementation;
    });
  });

  describe('Transaction Submission', () => {
    beforeEach(async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      mockProvider.getBlockNumber.mockResolvedValue(1000000);
      mockContract.decimals.mockResolvedValue(6); // USDC has 6 decimals
      await gateway.connect();
    });

    test('should submit token transfer successfully', async () => {
      const mockTxResponse = {
        hash: '0xabc123def456',
        wait: jest.fn().mockResolvedValue({
          hash: '0xabc123def456',
          blockNumber: 1000001,
          gasUsed: BigInt(21000),
          effectiveGasPrice: BigInt('20000000000'),
          status: 1
        })
      };

      mockContract.transfer.mockResolvedValue(mockTxResponse);

      const result = await gateway.submitTransaction(mockTransaction);

      expect(result.id).toBe('0xabc123def456');
      expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(result.blockNumber).toBe(1000001);
      expect(mockContract.transfer).toHaveBeenCalledWith(
        mockTransaction.receiver.ethereumAddress,
        expect.any(BigInt),
        expect.objectContaining({
          gasLimit: expect.any(BigInt),
          maxFeePerGas: expect.any(String),
          maxPriorityFeePerGas: expect.any(String)
        })
      );
    });

    test('should submit token deposit successfully', async () => {
      const depositTransaction = { ...mockTransaction, type: 'tokenDeposit' };
      
      const mockApproveTx = {
        wait: jest.fn().mockResolvedValue({ status: 1 })
      };
      const mockDepositTx = {
        hash: '0xdeposit123',
        wait: jest.fn().mockResolvedValue({
          hash: '0xdeposit123',
          blockNumber: 1000002,
          gasUsed: BigInt(50000),
          status: 1
        })
      };

      mockContract.approve.mockResolvedValue(mockApproveTx);
      mockContract.decimals.mockResolvedValue(6);
      
      // Mock vault contract
      const mockVaultContract = {
        address: '0xvault123',
        deposit: jest.fn().mockResolvedValue(mockDepositTx)
      };
      gateway.contracts.set(CONTRACT_TYPES.DEPOSIT_VAULT, mockVaultContract);

      const result = await gateway.submitTransaction(depositTransaction);

      expect(result.id).toBe('0xdeposit123');
      expect(result.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(mockContract.approve).toHaveBeenCalled();
      expect(mockVaultContract.deposit).toHaveBeenCalled();
    });

    test('should submit yield farming stake successfully', async () => {
      const farmTransaction = { 
        ...mockTransaction, 
        type: 'yieldFarm',
        action: 'stake'
      };
      
      const mockApproveTx = { wait: jest.fn().mockResolvedValue({ status: 1 }) };
      const mockStakeTx = {
        hash: '0xstake123',
        wait: jest.fn().mockResolvedValue({
          hash: '0xstake123',
          blockNumber: 1000003,
          gasUsed: BigInt(75000),
          status: 1
        })
      };

      mockContract.approve.mockResolvedValue(mockApproveTx);
      
      // Mock farm contract
      const mockFarmContract = {
        address: '0xfarm123',
        stake: jest.fn().mockResolvedValue(mockStakeTx)
      };
      gateway.contracts.set(CONTRACT_TYPES.YIELD_FARM, mockFarmContract);

      const result = await gateway.submitTransaction(farmTransaction);

      expect(result.id).toBe('0xstake123');
      expect(mockFarmContract.stake).toHaveBeenCalled();
    });

    test('should handle yield farming when disabled', async () => {
      gateway.config.enableYieldFarming = false;
      const farmTransaction = { 
        ...mockTransaction, 
        type: 'yieldFarm',
        action: 'stake'
      };

      await expect(gateway.submitTransaction(farmTransaction)).rejects.toThrow('Yield farming is disabled');
    });

    test('should submit lending supply successfully', async () => {
      const lendingTransaction = { 
        ...mockTransaction, 
        type: 'lending',
        action: 'supply'
      };
      
      const mockApproveTx = { wait: jest.fn().mockResolvedValue({ status: 1 }) };
      const mockSupplyTx = {
        hash: '0xlend123',
        wait: jest.fn().mockResolvedValue({
          hash: '0xlend123',
          blockNumber: 1000004,
          gasUsed: BigInt(85000),
          status: 1
        })
      };

      mockContract.approve.mockResolvedValue(mockApproveTx);
      
      // Mock lending contract
      const mockLendingContract = {
        address: '0xlending123',
        supply: jest.fn().mockResolvedValue(mockSupplyTx)
      };
      gateway.contracts.set(CONTRACT_TYPES.LENDING_POOL, mockLendingContract);

      const result = await gateway.submitTransaction(lendingTransaction);

      expect(result.id).toBe('0xlend123');
      expect(mockLendingContract.supply).toHaveBeenCalled();
    });

    test('should throw error when provider not connected', async () => {
      gateway.provider = null;
      
      await expect(gateway.submitTransaction(mockTransaction)).rejects.toThrow('Ethereum L2 provider not connected');
    });

    test('should throw error when wallet not initialized', async () => {
      gateway.wallet = null;
      
      await expect(gateway.submitTransaction(mockTransaction)).rejects.toThrow('Ethereum wallet not initialized');
    });

    test('should handle transaction failure', async () => {
      mockContract.transfer.mockRejectedValue(new Error('Insufficient balance'));

      await expect(gateway.submitTransaction(mockTransaction)).rejects.toThrow('Insufficient balance');
    });
  });

  describe('Transaction Status Queries', () => {
    beforeEach(async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      await gateway.connect();
      
      // Add a tracked transaction
      gateway.trackTransaction('test-tx-123', {
        id: '0xabc123def456',
        status: TRANSACTION_STATUS.SUBMITTED
      });
    });

    test('should get transaction status successfully', async () => {
      mockProvider.getTransactionReceipt.mockResolvedValue({
        hash: '0xabc123def456',
        blockNumber: 1000001,
        gasUsed: BigInt(21000),
        effectiveGasPrice: BigInt('20000000000'),
        status: 1
      });

      const status = await gateway.getTransactionStatus('test-tx-123');

      expect(status.transactionId).toBe('test-tx-123');
      expect(status.status).toBe(TRANSACTION_STATUS.CONFIRMED);
      expect(status.blockNumber).toBe(1000001);
      expect(status.gasUsed).toBe('21000');
    });

    test('should return pending for transaction not yet mined', async () => {
      mockProvider.getTransactionReceipt.mockResolvedValue(null);

      const status = await gateway.getTransactionStatus('test-tx-123');

      expect(status.transactionId).toBe('test-tx-123');
      expect(status.status).toBe(TRANSACTION_STATUS.PENDING);
    });

    test('should return failed for failed transaction', async () => {
      mockProvider.getTransactionReceipt.mockResolvedValue({
        hash: '0xabc123def456',
        blockNumber: 1000001,
        gasUsed: BigInt(21000),
        status: 0 // Failed
      });

      const status = await gateway.getTransactionStatus('test-tx-123');

      expect(status.status).toBe(TRANSACTION_STATUS.FAILED);
    });

    test('should throw error for unknown transaction', async () => {
      await expect(gateway.getTransactionStatus('unknown-tx')).rejects.toThrow('Transaction unknown-tx not found in history');
    });
  });

  describe('Network Health', () => {
    beforeEach(async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      await gateway.connect();
    });

    test('should get network health successfully', async () => {
      mockProvider.getBlockNumber.mockResolvedValue(1000000);
      mockProvider.getGasPrice.mockResolvedValue(BigInt('20000000000')); // 20 gwei
      mockProvider.getFeeData.mockResolvedValue({
        gasPrice: BigInt('20000000000'),
        maxFeePerGas: BigInt('25000000000'),
        maxPriorityFeePerGas: BigInt('2000000000')
      });

      const health = await gateway.getNetworkHealth();

      expect(health.networkType).toBe(NETWORK_TYPES.ETHEREUM_L2);
      expect(health.isHealthy).toBe(true);
      expect(health.blockNumber).toBe(1000000);
      expect(health.gasPrice).toBe('20000000000');
      expect(health.chainId).toBe(80001);
    });

    test('should detect unhealthy network conditions', async () => {
      mockProvider.getBlockNumber.mockResolvedValue(1000000);
      mockProvider.getGasPrice.mockResolvedValue(BigInt('150000000000')); // 150 gwei - very high
      mockProvider.getFeeData.mockResolvedValue({
        gasPrice: BigInt('150000000000')
      });

      const health = await gateway.getNetworkHealth();

      expect(health.isHealthy).toBe(false);
    });

    test('should handle network health check failure', async () => {
      gateway.provider = null;

      const health = await gateway.getNetworkHealth();

      expect(health.networkType).toBe(NETWORK_TYPES.ETHEREUM_L2);
      expect(health.isHealthy).toBe(false);
      expect(health.error).toBeDefined();
    });
  });

  describe('Address Resolution', () => {
    test('should resolve Ethereum address from receiver', async () => {
      const address = await gateway.resolveAddress(mockTransaction.receiver);
      expect(address).toBe('0x0987654321098765432109876543210987654321');
    });

    test('should resolve ENS name', async () => {
      // Connect the gateway first to initialize provider
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      mockProvider.getBlockNumber.mockResolvedValue(1000000);
      await gateway.connect();
      
      mockProvider.resolveName.mockResolvedValue('0x1234567890123456789012345678901234567890');
      
      const receiver = { ensName: 'alice.eth' };
      const address = await gateway.resolveAddress(receiver);
      
      expect(address).toBe('0x1234567890123456789012345678901234567890');
      expect(mockProvider.resolveName).toHaveBeenCalledWith('alice.eth');
    });

    test('should return test address in test mode for unknown receiver', async () => {
      const unknownReceiver = { name: 'Unknown' };
      const address = await gateway.resolveAddress(unknownReceiver);
      
      expect(address).toBe('0x742d35Cc6634C0532925a3b8D4c8C9df4C5C1234');
    });

    test('should throw error for unknown receiver in production mode', async () => {
      const prodGateway = new EthereumL2Gateway({ testMode: false });
      const unknownReceiver = { name: 'Unknown' };
      
      await expect(prodGateway.resolveAddress(unknownReceiver)).rejects.toThrow('Cannot resolve Ethereum address for receiver');
    });
  });

  describe('Token Operations', () => {
    beforeEach(async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      await gateway.connect();
    });

    test('should get token balance successfully', async () => {
      mockContract.balanceOf.mockResolvedValue(BigInt('1000000000')); // 1000 USDC
      mockContract.decimals.mockResolvedValue(6);

      const balance = await gateway.getTokenBalance('USDC', '0x1234567890123456789012345678901234567890');

      expect(balance).toBe('1000');
      expect(mockContract.balanceOf).toHaveBeenCalledWith('0x1234567890123456789012345678901234567890');
    });

    test('should handle balance query failure', async () => {
      mockContract.balanceOf.mockRejectedValue(new Error('Contract call failed'));

      await expect(gateway.getTokenBalance('USDC', '0x1234567890123456789012345678901234567890'))
        .rejects.toThrow('Contract call failed');
    });

    test('should throw error for unknown token', async () => {
      await expect(gateway.getTokenContract('UNKNOWN')).rejects.toThrow('Token contract not found for symbol: UNKNOWN');
    });
  });

  describe('Gas Price Management', () => {
    beforeEach(async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      await gateway.connect();
    });

    test('should get current gas price', async () => {
      mockProvider.getFeeData.mockResolvedValue({
        gasPrice: BigInt('20000000000'),
        maxFeePerGas: BigInt('25000000000'),
        maxPriorityFeePerGas: BigInt('2000000000')
      });

      const gasPrice = await gateway.getGasPrice();

      expect(gasPrice.gasPrice).toBe('20000000000');
      expect(gasPrice.maxFeePerGas).toBe('25000000000');
      expect(gasPrice.gasPriceGwei).toBe('20');
    });

    test('should start gas price monitoring', () => {
      expect(gateway.gasPriceMonitoringInterval).toBeDefined();
    });

    test('should stop monitoring on cleanup', async () => {
      const intervalId = gateway.gasPriceMonitoringInterval;
      expect(intervalId).toBeDefined();

      await gateway.cleanup();
      expect(gateway.gasPriceMonitoringInterval).toBeNull();
    });
  });

  describe('Contract Management', () => {
    beforeEach(async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      await gateway.connect();
    });

    test('should load contract instances on connection', () => {
      expect(gateway.contracts.size).toBeGreaterThan(0);
      expect(gateway.contracts.has('USDC')).toBe(true);
      expect(gateway.contracts.has('USDT')).toBe(true);
    });

    test('should get contract by type', async () => {
      const mockVaultContract = { address: '0xvault123' };
      gateway.contracts.set(CONTRACT_TYPES.DEPOSIT_VAULT, mockVaultContract);

      const contract = await gateway.getContract(CONTRACT_TYPES.DEPOSIT_VAULT);
      expect(contract).toBe(mockVaultContract);
    });

    test('should throw error for unknown contract type', async () => {
      await expect(gateway.getContract('UNKNOWN_TYPE')).rejects.toThrow('Contract not found for type: UNKNOWN_TYPE');
    });
  });

  describe('Error Handling', () => {
    test('should handle missing ethers library gracefully', () => {
      // Mock missing ethers library
      jest.doMock('ethers', () => {
        throw new Error('Module not found');
      });

      expect(() => {
        delete require.cache[require.resolve('../../src/blockchain/ethereum-l2-gateway')];
        require('../../src/blockchain/ethereum-l2-gateway');
      }).not.toThrow();
    });

    test('should handle gas price monitoring errors gracefully', async () => {
      mockProvider.getNetwork.mockResolvedValue({ chainId: BigInt(80001) });
      await gateway.connect();

      mockProvider.getFeeData.mockRejectedValue(new Error('Network error'));

      // Should not throw when gas price monitoring fails
      // The monitoring runs in background and logs errors
      expect(gateway.gasPriceMonitoringInterval).toBeDefined();
    });
  });

  describe('Constants Export', () => {
    test('should export contract types', () => {
      expect(CONTRACT_TYPES.ERC20_TOKEN).toBe('ERC20Token');
      expect(CONTRACT_TYPES.DEPOSIT_VAULT).toBe('DepositVault');
      expect(CONTRACT_TYPES.LENDING_POOL).toBe('LendingPool');
      expect(CONTRACT_TYPES.YIELD_FARM).toBe('YieldFarm');
    });

    test('should export token standards', () => {
      expect(TOKEN_STANDARDS.ERC20).toBe('ERC20');
      expect(TOKEN_STANDARDS.ERC721).toBe('ERC721');
      expect(TOKEN_STANDARDS.ERC1155).toBe('ERC1155');
    });

    test('should export default contracts', () => {
      expect(DEFAULT_CONTRACTS.USDC).toBe('0x2791Bca1f2de4661ED88A30C99A7a9449Aa84174');
      expect(DEFAULT_CONTRACTS.USDT).toBe('0xc2132D05D31c914a87C6611C10748AEb04B58e8F');
      expect(DEFAULT_CONTRACTS.DAI).toBe('0x8f3Cf7ad23Cd3CaDbD9735AFf958023239c6A063');
    });
  });
});