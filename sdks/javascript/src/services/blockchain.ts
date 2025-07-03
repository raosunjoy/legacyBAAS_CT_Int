/**
 * Multi-Blockchain Router Service
 * Handle cross-blockchain transactions and routing
 */

import { AxiosInstance } from 'axios';
import { LegacyBaaSError } from '../errors';
import { 
  BlockchainTransaction,
  BlockchainRoute,
  BlockchainStatus,
  NetworkInfo,
  TransactionResult,
  RoutingOptions
} from '../types/blockchain';
import { ApiResponse } from '../types/common';

/**
 * Blockchain Router
 */
export class BlockchainRouter {
  constructor(private http: AxiosInstance) {}

  /**
   * Route transaction across blockchain networks
   * @param transaction Transaction to route
   * @param options Routing options
   * @returns Transaction result
   */
  async routeTransaction(
    transaction: BlockchainTransaction,
    options: RoutingOptions = {}
  ): Promise<TransactionResult> {
    try {
      const response = await this.http.post<ApiResponse<TransactionResult>>(
        '/blockchain/route',
        {
          transaction,
          options: {
            preferredNetwork: options.preferredNetwork,
            costOptimization: options.costOptimization !== false,
            speedOptimization: options.speedOptimization || false,
            confirmationRequirement: options.confirmationRequirement || 'standard',
            enableFailover: options.enableFailover !== false
          }
        }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Blockchain routing failed: ${error}`);
    }
  }

  /**
   * Get optimal route for transaction
   * @param transaction Transaction to analyze
   * @returns Recommended route
   */
  async getOptimalRoute(transaction: BlockchainTransaction): Promise<BlockchainRoute> {
    try {
      const response = await this.http.post<ApiResponse<BlockchainRoute>>(
        '/blockchain/route/optimal',
        { transaction }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get optimal route: ${error}`);
    }
  }

  /**
   * Send transaction to specific network
   * @param networkId Target network ID
   * @param transaction Transaction data
   * @returns Transaction result
   */
  async sendToNetwork(
    networkId: string, 
    transaction: BlockchainTransaction
  ): Promise<TransactionResult> {
    try {
      const response = await this.http.post<ApiResponse<TransactionResult>>(
        `/blockchain/networks/${networkId}/send`,
        { transaction }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to send to network ${networkId}: ${error}`);
    }
  }

  /**
   * Get transaction status
   * @param transactionId Transaction ID
   * @param networkId Optional network ID
   * @returns Transaction status
   */
  async getTransactionStatus(
    transactionId: string,
    networkId?: string
  ): Promise<BlockchainStatus> {
    try {
      const endpoint = networkId 
        ? `/blockchain/networks/${networkId}/transactions/${transactionId}`
        : `/blockchain/transactions/${transactionId}`;
        
      const response = await this.http.get<ApiResponse<BlockchainStatus>>(endpoint);
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get transaction status: ${error}`);
    }
  }

  /**
   * Get supported blockchain networks
   * @returns List of network information
   */
  async getSupportedNetworks(): Promise<NetworkInfo[]> {
    try {
      const response = await this.http.get<ApiResponse<NetworkInfo[]>>(
        '/blockchain/networks'
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get supported networks: ${error}`);
    }
  }

  /**
   * Get network status and health
   * @param networkId Network ID to check
   * @returns Network status
   */
  async getNetworkStatus(networkId: string): Promise<NetworkInfo> {
    try {
      const response = await this.http.get<ApiResponse<NetworkInfo>>(
        `/blockchain/networks/${networkId}/status`
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get network status: ${error}`);
    }
  }

  /**
   * Get account balance on specific network
   * @param networkId Network ID
   * @param address Account address
   * @returns Balance information
   */
  async getBalance(networkId: string, address: string): Promise<any> {
    try {
      const response = await this.http.get<ApiResponse<any>>(
        `/blockchain/networks/${networkId}/balance/${address}`
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get balance: ${error}`);
    }
  }

  /**
   * Estimate transaction fees
   * @param transaction Transaction to estimate
   * @param networkId Target network
   * @returns Fee estimation
   */
  async estimateFees(
    transaction: BlockchainTransaction,
    networkId?: string
  ): Promise<any> {
    try {
      const endpoint = networkId 
        ? `/blockchain/networks/${networkId}/estimate-fees`
        : '/blockchain/estimate-fees';
        
      const response = await this.http.post<ApiResponse<any>>(
        endpoint,
        { transaction }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to estimate fees: ${error}`);
    }
  }

  /**
   * Get transaction history
   * @param filters Query filters
   * @returns Transaction history
   */
  async getTransactionHistory(filters: {
    networkId?: string;
    address?: string;
    startDate?: string;
    endDate?: string;
    status?: string;
    limit?: number;
  } = {}): Promise<TransactionResult[]> {
    try {
      const response = await this.http.get<ApiResponse<TransactionResult[]>>(
        '/blockchain/transactions/history',
        { params: filters }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get transaction history: ${error}`);
    }
  }

  /**
   * Cancel pending transaction (if supported)
   * @param transactionId Transaction ID to cancel
   * @param networkId Network ID
   * @returns Cancellation result
   */
  async cancelTransaction(
    transactionId: string,
    networkId: string
  ): Promise<{ success: boolean; message: string }> {
    try {
      const response = await this.http.post<ApiResponse<{ success: boolean; message: string }>>(
        `/blockchain/networks/${networkId}/transactions/${transactionId}/cancel`
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to cancel transaction: ${error}`);
    }
  }

  /**
   * Get cross-chain bridge status
   * @param fromNetwork Source network
   * @param toNetwork Target network
   * @returns Bridge status
   */
  async getBridgeStatus(fromNetwork: string, toNetwork: string): Promise<any> {
    try {
      const response = await this.http.get<ApiResponse<any>>(
        `/blockchain/bridge/${fromNetwork}/${toNetwork}/status`
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get bridge status: ${error}`);
    }
  }
}