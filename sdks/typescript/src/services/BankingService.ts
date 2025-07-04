/**
 * Banking Service
 * Integration with core banking systems: FIS, Fiserv, Temenos, TCS BaNCS
 */

import { LegacyBaaSClient } from '../client/LegacyBaaSClient';
import { ApiResponse } from '../types';
import { LegacyBaaSError } from '../errors';

export interface BankingAccount {
  accountId: string;
  accountNumber: string;
  accountType: 'checking' | 'savings' | 'credit' | 'loan' | 'investment';
  currency: string;
  balance: {
    current: number;
    available: number;
    pending: number;
    lastUpdated: string;
  };
  status: 'active' | 'inactive' | 'closed' | 'frozen';
  holder: {
    name: string;
    customerId: string;
    type: 'individual' | 'business';
  };
  bankInfo: {
    bankId: string;
    bankName: string;
    system: 'fis' | 'fiserv' | 'temenos' | 'tcs' | 'other';
    branchCode?: string;
    routingNumber?: string;
    swiftCode?: string;
  };
  metadata?: Record<string, any>;
}

export interface BankingTransaction {
  transactionId: string;
  accountId: string;
  type: 'debit' | 'credit' | 'transfer' | 'payment' | 'fee' | 'interest';
  amount: number;
  currency: string;
  description: string;
  referenceNumber?: string;
  counterparty?: {
    name: string;
    accountNumber: string;
    bankCode: string;
  };
  status: 'pending' | 'processed' | 'failed' | 'reversed';
  timestamps: {
    initiated: string;
    processed?: string;
    settled?: string;
  };
  fees?: {
    amount: number;
    currency: string;
    description: string;
  };
  metadata?: Record<string, any>;
}

export interface TransferRequest {
  fromAccountId: string;
  toAccountId: string;
  amount: number;
  currency: string;
  description?: string;
  referenceNumber?: string;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
  scheduledDate?: string;
  recurring?: {
    frequency: 'daily' | 'weekly' | 'monthly' | 'yearly';
    endDate?: string;
    maxOccurrences?: number;
  };
}

export interface PaymentRequest {
  accountId: string;
  paymentMethod: 'ach' | 'wire' | 'check' | 'card' | 'rtp' | 'fedwire';
  amount: number;
  currency: string;
  recipient: {
    name: string;
    accountNumber: string;
    routingNumber: string;
    address?: string;
  };
  description?: string;
  referenceNumber?: string;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
  scheduledDate?: string;
}

export interface StatementRequest {
  accountId: string;
  startDate: string;
  endDate: string;
  format?: 'pdf' | 'csv' | 'json' | 'mt940' | 'bai2';
  includeImages?: boolean;
  includeMetadata?: boolean;
}

export class BankingService {
  constructor(private client: LegacyBaaSClient) {}

  /**
   * Get account information
   */
  async getAccount(accountId: string): Promise<BankingAccount> {
    const response = await this.client.makeRequest<BankingAccount>(
      'GET',
      `/banking/accounts/${accountId}`
    );

    return response.data;
  }

  /**
   * List accounts
   */
  async listAccounts(filters: {
    customerId?: string;
    accountType?: string;
    status?: string;
    bankSystem?: string;
    limit?: number;
    offset?: number;
  } = {}): Promise<{
    accounts: BankingAccount[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
  }> {
    const response = await this.client.makeRequest<{
      accounts: BankingAccount[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
    }>('GET', '/banking/accounts', undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Get account balance
   */
  async getAccountBalance(accountId: string): Promise<{
    accountId: string;
    balance: {
      current: number;
      available: number;
      pending: number;
      lastUpdated: string;
    };
    currency: string;
    status: string;
  }> {
    const response = await this.client.makeRequest<{
      accountId: string;
      balance: {
        current: number;
        available: number;
        pending: number;
        lastUpdated: string;
      };
      currency: string;
      status: string;
    }>('GET', `/banking/accounts/${accountId}/balance`);

    return response.data;
  }

  /**
   * Get transaction history
   */
  async getTransactionHistory(
    accountId: string,
    filters: {
      startDate?: string;
      endDate?: string;
      transactionType?: string;
      minAmount?: number;
      maxAmount?: number;
      status?: string;
      limit?: number;
      offset?: number;
    } = {}
  ): Promise<{
    transactions: BankingTransaction[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
    summary: {
      totalDebits: number;
      totalCredits: number;
      netAmount: number;
      transactionCount: number;
    };
  }> {
    const response = await this.client.makeRequest<{
      transactions: BankingTransaction[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
      summary: {
        totalDebits: number;
        totalCredits: number;
        netAmount: number;
        transactionCount: number;
      };
    }>('GET', `/banking/accounts/${accountId}/transactions`, undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Get transaction details
   */
  async getTransaction(transactionId: string): Promise<BankingTransaction> {
    const response = await this.client.makeRequest<BankingTransaction>(
      'GET',
      `/banking/transactions/${transactionId}`
    );

    return response.data;
  }

  /**
   * Transfer funds between accounts
   */
  async transferFunds(transferRequest: TransferRequest): Promise<{
    transactionId: string;
    status: 'pending' | 'processed' | 'failed';
    transferId: string;
    estimatedSettlement: string;
    fees?: {
      amount: number;
      currency: string;
      description: string;
    };
    message: string;
  }> {
    const response = await this.client.makeRequest<{
      transactionId: string;
      status: 'pending' | 'processed' | 'failed';
      transferId: string;
      estimatedSettlement: string;
      fees?: {
        amount: number;
        currency: string;
        description: string;
      };
      message: string;
    }>('POST', '/banking/transfers', transferRequest);

    return response.data;
  }

  /**
   * Make a payment
   */
  async makePayment(paymentRequest: PaymentRequest): Promise<{
    transactionId: string;
    paymentId: string;
    status: 'pending' | 'processed' | 'failed';
    trackingNumber?: string;
    estimatedDelivery: string;
    fees?: {
      amount: number;
      currency: string;
      description: string;
    };
    message: string;
  }> {
    const response = await this.client.makeRequest<{
      transactionId: string;
      paymentId: string;
      status: 'pending' | 'processed' | 'failed';
      trackingNumber?: string;
      estimatedDelivery: string;
      fees?: {
        amount: number;
        currency: string;
        description: string;
      };
      message: string;
    }>('POST', '/banking/payments', paymentRequest);

    return response.data;
  }

  /**
   * Get payment status
   */
  async getPaymentStatus(paymentId: string): Promise<{
    paymentId: string;
    status: 'pending' | 'processed' | 'failed' | 'cancelled';
    trackingNumber?: string;
    statusUpdates: Array<{
      status: string;
      timestamp: string;
      message: string;
    }>;
    estimatedDelivery?: string;
    actualDelivery?: string;
  }> {
    const response = await this.client.makeRequest<{
      paymentId: string;
      status: 'pending' | 'processed' | 'failed' | 'cancelled';
      trackingNumber?: string;
      statusUpdates: Array<{
        status: string;
        timestamp: string;
        message: string;
      }>;
      estimatedDelivery?: string;
      actualDelivery?: string;
    }>('GET', `/banking/payments/${paymentId}/status`);

    return response.data;
  }

  /**
   * Generate account statement
   */
  async generateStatement(statementRequest: StatementRequest): Promise<{
    statementId: string;
    downloadUrl: string;
    format: string;
    generatedAt: string;
    expiresAt: string;
    summary: {
      accountId: string;
      period: {
        startDate: string;
        endDate: string;
      };
      transactionCount: number;
      totalDebits: number;
      totalCredits: number;
      openingBalance: number;
      closingBalance: number;
    };
  }> {
    const response = await this.client.makeRequest<{
      statementId: string;
      downloadUrl: string;
      format: string;
      generatedAt: string;
      expiresAt: string;
      summary: {
        accountId: string;
        period: {
          startDate: string;
          endDate: string;
        };
        transactionCount: number;
        totalDebits: number;
        totalCredits: number;
        openingBalance: number;
        closingBalance: number;
      };
    }>('POST', '/banking/statements', statementRequest);

    return response.data;
  }

  /**
   * Get supported banking systems
   */
  async getSupportedSystems(): Promise<{
    systems: Array<{
      systemId: string;
      name: string;
      type: 'core_banking' | 'payment_processor' | 'digital_wallet';
      status: 'active' | 'maintenance' | 'deprecated';
      capabilities: {
        realTimeBalances: boolean;
        instantTransfers: boolean;
        scheduledPayments: boolean;
        recurringPayments: boolean;
        statementGeneration: boolean;
        swiftIntegration: boolean;
        blockchainRouting: boolean;
      };
      supportedCurrencies: string[];
      supportedRegions: string[];
    }>;
    totalSystems: number;
    activeSystems: number;
  }> {
    const response = await this.client.makeRequest<{
      systems: Array<{
        systemId: string;
        name: string;
        type: 'core_banking' | 'payment_processor' | 'digital_wallet';
        status: 'active' | 'maintenance' | 'deprecated';
        capabilities: {
          realTimeBalances: boolean;
          instantTransfers: boolean;
          scheduledPayments: boolean;
          recurringPayments: boolean;
          statementGeneration: boolean;
          swiftIntegration: boolean;
          blockchainRouting: boolean;
        };
        supportedCurrencies: string[];
        supportedRegions: string[];
      }>;
      totalSystems: number;
      activeSystems: number;
    }>('GET', '/banking/systems');

    return response.data;
  }

  /**
   * Get banking metrics
   */
  async getMetrics(): Promise<{
    totalAccounts: number;
    totalTransactions: number;
    totalVolume: number;
    systemDistribution: Record<string, number>;
    transactionSuccess: number;
    avgProcessingTime: number;
    complianceScore: number;
  }> {
    const response = await this.client.makeRequest<{
      totalAccounts: number;
      totalTransactions: number;
      totalVolume: number;
      systemDistribution: Record<string, number>;
      transactionSuccess: number;
      avgProcessingTime: number;
      complianceScore: number;
    }>('GET', '/banking/metrics');

    return response.data;
  }

  /**
   * Validate account
   */
  async validateAccount(accountNumber: string, routingNumber: string): Promise<{
    isValid: boolean;
    accountType?: string;
    bankName?: string;
    branchInfo?: {
      name: string;
      address: string;
      phone: string;
    };
    validationScore: number;
    warnings: string[];
  }> {
    const response = await this.client.makeRequest<{
      isValid: boolean;
      accountType?: string;
      bankName?: string;
      branchInfo?: {
        name: string;
        address: string;
        phone: string;
      };
      validationScore: number;
      warnings: string[];
    }>('POST', '/banking/validate-account', {
      accountNumber,
      routingNumber
    });

    return response.data;
  }
}