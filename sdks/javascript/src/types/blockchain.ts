/**
 * Blockchain Types and Interfaces
 */

/**
 * Blockchain Transaction Interface
 */
export interface BlockchainTransaction {
  id?: string;
  from: string;
  to: string;
  amount: number;
  currency: string;
  network?: string;
  memo?: string;
  metadata?: Record<string, any>;
}

/**
 * Blockchain Route Information
 */
export interface BlockchainRoute {
  sourceNetwork: string;
  targetNetwork: string;
  estimatedTime: number; // seconds
  estimatedFees: {
    source: number;
    target: number;
    bridge?: number;
    total: number;
    currency: string;
  };
  confidence: number; // 0-1
  path: string[];
  warnings?: string[];
}

/**
 * Routing Options
 */
export interface RoutingOptions {
  preferredNetwork?: string;
  costOptimization?: boolean;
  speedOptimization?: boolean;
  confirmationRequirement?: 'instant' | 'standard' | 'secure';
  enableFailover?: boolean;
  maxSlippage?: number;
}

/**
 * Transaction Result
 */
export interface TransactionResult {
  transactionId: string;
  networkTransactionId?: string;
  network: string;
  status: 'pending' | 'confirmed' | 'failed' | 'cancelled';
  confirmations: number;
  requiredConfirmations: number;
  blockHash?: string;
  blockNumber?: number;
  gasUsed?: number;
  gasPrice?: number;
  fees: {
    amount: number;
    currency: string;
  };
  timestamp: string;
  metadata?: Record<string, any>;
}

/**
 * Blockchain Status
 */
export interface BlockchainStatus {
  transactionId: string;
  network: string;
  status: 'pending' | 'confirmed' | 'failed' | 'cancelled';
  confirmations: number;
  requiredConfirmations: number;
  estimatedCompletionTime?: string;
  blockHash?: string;
  blockNumber?: number;
  timestamp: string;
  lastUpdated: string;
}

/**
 * Network Information
 */
export interface NetworkInfo {
  id: string;
  name: string;
  type: 'mainnet' | 'testnet' | 'devnet';
  status: 'active' | 'maintenance' | 'deprecated';
  capabilities: string[];
  nativeCurrency: {
    symbol: string;
    decimals: number;
  };
  blockTime: number; // seconds
  finalityTime: number; // seconds
  fees: {
    standard: number;
    fast: number;
    instant: number;
    currency: string;
  };
  explorer?: string;
  documentation?: string;
}

/**
 * Cross-Chain Bridge Information
 */
export interface BridgeInfo {
  id: string;
  sourceNetwork: string;
  targetNetwork: string;
  supportedTokens: string[];
  minimumAmount: number;
  maximumAmount: number;
  fees: {
    percentage: number;
    fixed: number;
    currency: string;
  };
  estimatedTime: number; // seconds
  status: 'active' | 'maintenance' | 'disabled';
}

/**
 * Token Information
 */
export interface TokenInfo {
  address: string;
  symbol: string;
  name: string;
  decimals: number;
  network: string;
  type: 'native' | 'erc20' | 'bep20' | 'spl' | 'custom';
  verified: boolean;
  metadata?: {
    logo?: string;
    website?: string;
    description?: string;
  };
}

/**
 * Balance Information
 */
export interface BalanceInfo {
  address: string;
  network: string;
  balances: {
    token: TokenInfo;
    balance: string;
    balanceFormatted: number;
    usdValue?: number;
  }[];
  totalUsdValue?: number;
  lastUpdated: string;
}