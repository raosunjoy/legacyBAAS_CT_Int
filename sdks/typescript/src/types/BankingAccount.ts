/**
 * Banking account type definitions
 */

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