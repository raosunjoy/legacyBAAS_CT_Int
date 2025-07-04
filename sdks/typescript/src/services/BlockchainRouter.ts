/**
 * Blockchain Router Service
 * Smart routing across 11+ blockchain networks
 */

import { LegacyBaaSClient } from '../client/LegacyBaaSClient';
import { ApiResponse } from '../types';
import { LegacyBaaSError } from '../errors';

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

export interface RoutingPreferences {
  preferredNetworks?: string[];
  maxFee?: number;
  maxTime?: number;
  prioritizeSpeed?: boolean;
  prioritizeCost?: boolean;
  prioritizeSecurity?: boolean;
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

export interface NetworkInfo {
  networkId: string;
  name: string;
  type: 'mainnet' | 'testnet' | 'layer2';
  status: 'active' | 'maintenance' | 'deprecated';
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

export class BlockchainRouter {
  constructor(private client: LegacyBaaSClient) {}

  /**
   * Route transaction using AI-powered smart routing
   */
  async routeTransaction(
    transaction: BlockchainTransaction,
    preferences: RoutingPreferences = {}
  ): Promise<TransactionResult> {
    const response = await this.client.makeRequest<TransactionResult>(
      'POST',
      '/blockchain/route',
      {
        transaction,
        preferences: {
          preferredNetworks: preferences.preferredNetworks || [],
          maxFee: preferences.maxFee,
          maxTime: preferences.maxTime,
          prioritizeSpeed: preferences.prioritizeSpeed || false,
          prioritizeCost: preferences.prioritizeCost || false,
          prioritizeSecurity: preferences.prioritizeSecurity || true
        }
      }
    );

    return response.data;
  }

  /**
   * Get optimal routing recommendation
   */
  async getRoutingRecommendation(
    transaction: BlockchainTransaction,
    preferences: RoutingPreferences = {}
  ): Promise<{
    recommendations: Array<{
      network: string;
      score: number;
      estimatedFee: number;
      estimatedTime: string;
      pros: string[];
      cons: string[];
    }>;
    aiInsights: {
      marketConditions: string;
      networkCongestion: Record<string, number>;
      recommendation: string;
      confidence: number;
    };
  }> {
    const response = await this.client.makeRequest<{
      recommendations: Array<{
        network: string;
        score: number;
        estimatedFee: number;
        estimatedTime: string;
        pros: string[];
        cons: string[];
      }>;
      aiInsights: {
        marketConditions: string;
        networkCongestion: Record<string, number>;
        recommendation: string;
        confidence: number;
      };
    }>('POST', '/blockchain/route/recommend', {
      transaction,
      preferences
    });

    return response.data;
  }

  /**
   * Get transaction status
   */
  async getTransactionStatus(transactionId: string): Promise<TransactionResult> {
    const response = await this.client.makeRequest<TransactionResult>(
      'GET',
      `/blockchain/transactions/${transactionId}`
    );

    return response.data;
  }

  /**
   * Track transaction by blockchain hash
   */
  async trackTransaction(blockchainTxId: string): Promise<{
    transactionId: string;
    blockchainTxId: string;
    network: string;
    status: string;
    confirmations: number;
    blockNumber?: number;
    gasUsed?: number;
    gasFee?: number;
    timestamp: string;
    details: Record<string, any>;
  }> {
    const response = await this.client.makeRequest<{
      transactionId: string;
      blockchainTxId: string;
      network: string;
      status: string;
      confirmations: number;
      blockNumber?: number;
      gasUsed?: number;
      gasFee?: number;
      timestamp: string;
      details: Record<string, any>;
    }>('GET', `/blockchain/track/${blockchainTxId}`);

    return response.data;
  }

  /**
   * Get supported networks
   */
  async getSupportedNetworks(): Promise<{
    networks: NetworkInfo[];
    totalNetworks: number;
    activeNetworks: number;
  }> {
    const response = await this.client.makeRequest<{
      networks: NetworkInfo[];
      totalNetworks: number;
      activeNetworks: number;
    }>('GET', '/blockchain/networks');

    return response.data;
  }

  /**
   * Get specific network information
   */
  async getNetworkInfo(networkId: string): Promise<NetworkInfo> {
    const response = await this.client.makeRequest<NetworkInfo>(
      'GET',
      `/blockchain/networks/${networkId}`
    );

    return response.data;
  }

  /**
   * Get network health metrics
   */
  async getNetworkHealth(): Promise<{
    networks: Record<string, {
      status: string;
      uptime: number;
      avgResponseTime: number;
      errorRate: number;
      lastChecked: string;
    }>;
    overallHealth: number;
  }> {
    const response = await this.client.makeRequest<{
      networks: Record<string, {
        status: string;
        uptime: number;
        avgResponseTime: number;
        errorRate: number;
        lastChecked: string;
      }>;
      overallHealth: number;
    }>('GET', '/blockchain/health');

    return response.data;
  }

  /**
   * Estimate gas fees for transaction
   */
  async estimateGas(
    transaction: BlockchainTransaction,
    networkId?: string
  ): Promise<GasEstimate[]> {
    const response = await this.client.makeRequest<GasEstimate[]>(
      'POST',
      '/blockchain/estimate-gas',
      {
        transaction,
        networkId
      }
    );

    return response.data;
  }

  /**
   * Create new wallet
   */
  async createWallet(walletData: {
    network: string;
    walletType: 'personal' | 'business' | 'escrow';
    metadata?: Record<string, any>;
  }): Promise<{
    address: string;
    network: string;
    walletType: string;
    publicKey: string;
    createdAt: string;
    walletId: string;
  }> {
    const response = await this.client.makeRequest<{
      address: string;
      network: string;
      walletType: string;
      publicKey: string;
      createdAt: string;
      walletId: string;
    }>('POST', '/blockchain/wallets', walletData);

    return response.data;
  }

  /**
   * Get wallet balance
   */
  async getWalletBalance(
    address: string,
    network: string
  ): Promise<{
    address: string;
    network: string;
    balances: Array<{
      asset: string;
      balance: string;
      usdValue: number;
      lastUpdated: string;
    }>;
    totalUsdValue: number;
  }> {
    const response = await this.client.makeRequest<{
      address: string;
      network: string;
      balances: Array<{
        asset: string;
        balance: string;
        usdValue: number;
        lastUpdated: string;
      }>;
      totalUsdValue: number;
    }>('GET', `/blockchain/balance/${address}`, undefined, {
      headers: { 'X-Network': network }
    });

    return response.data;
  }

  /**
   * List wallets
   */
  async listWallets(filters: {
    network?: string;
    walletType?: string;
    isActive?: boolean;
    limit?: number;
    offset?: number;
  } = {}): Promise<{
    wallets: WalletData[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
  }> {
    const response = await this.client.makeRequest<{
      wallets: WalletData[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
    }>('GET', '/blockchain/wallets', undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Get cross-chain bridge status
   */
  async getCrossChainBridgeStatus(): Promise<{
    bridges: Array<{
      bridgeId: string;
      fromNetwork: string;
      toNetwork: string;
      status: 'active' | 'maintenance' | 'disabled';
      avgFee: number;
      avgTime: string;
      supportedAssets: string[];
    }>;
    totalBridges: number;
    activeBridges: number;
  }> {
    const response = await this.client.makeRequest<{
      bridges: Array<{
        bridgeId: string;
        fromNetwork: string;
        toNetwork: string;
        status: 'active' | 'maintenance' | 'disabled';
        avgFee: number;
        avgTime: string;
        supportedAssets: string[];
      }>;
      totalBridges: number;
      activeBridges: number;
    }>('GET', '/blockchain/bridges');

    return response.data;
  }

  /**
   * Get blockchain metrics
   */
  async getMetrics(): Promise<{
    totalTransactions: number;
    successRate: number;
    avgProcessingTime: number;
    networkDistribution: Record<string, number>;
    volumeByNetwork: Record<string, number>;
    gasSavings: number;
  }> {
    const response = await this.client.makeRequest<{
      totalTransactions: number;
      successRate: number;
      avgProcessingTime: number;
      networkDistribution: Record<string, number>;
      volumeByNetwork: Record<string, number>;
      gasSavings: number;
    }>('GET', '/blockchain/metrics');

    return response.data;
  }
}