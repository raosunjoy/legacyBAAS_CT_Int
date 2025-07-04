/**
 * Blockchain network type definitions
 */

export interface BlockchainNetwork {
  networkId: string;
  name: string;
  type: 'mainnet' | 'testnet' | 'layer2';
  status: 'active' | 'maintenance' | 'deprecated';
  chainId?: number;
  nativeCurrency: {
    symbol: string;
    name: string;
    decimals: number;
  };
  rpcUrls: string[];
  blockExplorerUrls?: string[];
  capabilities: {
    smartContracts: boolean;
    multiSig: boolean;
    crossChain: boolean;
    privacyFeatures: boolean;
  };
  fees: {
    avgTransactionFee: number;
    avgConfirmationTime: number;
    currency: string;
  };
  metrics: {
    tps: number;
    uptime: number;
    totalTransactions: number;
  };
}

export interface BlockchainTransaction {
  transactionId: string;
  amount: string;
  currency: string;
  fromAddress: string;
  toAddress: string;
  network: string;
  metadata?: Record<string, any>;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
  swiftReference?: string;
}

export interface TransactionResult {
  transactionId: string;
  blockchainTxId: string;
  network: string;
  status: 'pending' | 'confirmed' | 'failed' | 'routing';
  fees: {
    networkFee: number;
    platformFee: number;
    totalFee: number;
    currency: string;
  };
  timestamps: {
    submitted: string;
    confirmed?: string;
    finalized?: string;
  };
  confirmations: number;
  blockNumber?: number;
  estimatedTime?: string;
}

export interface WalletData {
  address: string;
  network: string;
  balance: string;
  currency: string;
  isActive: boolean;
  createdAt: string;
  metadata?: Record<string, any>;
}

export interface GasEstimate {
  network: string;
  estimatedGas: number;
  gasPrice: number;
  totalCost: number;
  currency: string;
  confidence: number;
  estimatedTime: string;
}