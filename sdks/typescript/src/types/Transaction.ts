/**
 * Transaction type definitions
 */

export interface Transaction {
  transactionId: string;
  type: 'swift' | 'blockchain' | 'banking' | 'internal';
  status: 'pending' | 'processing' | 'completed' | 'failed' | 'cancelled';
  amount: number;
  currency: string;
  sender: {
    id: string;
    type: 'account' | 'wallet' | 'institution';
    details: Record<string, any>;
  };
  receiver: {
    id: string;
    type: 'account' | 'wallet' | 'institution';
    details: Record<string, any>;
  };
  timestamps: {
    created: string;
    submitted?: string;
    processed?: string;
    completed?: string;
  };
  fees: {
    amount: number;
    currency: string;
    breakdown: Array<{
      type: string;
      amount: number;
      description: string;
    }>;
  };
  metadata?: Record<string, any>;
}

export interface TransactionRequest {
  type: 'swift' | 'blockchain' | 'banking';
  amount: number;
  currency: string;
  sender: {
    id: string;
    type: 'account' | 'wallet' | 'institution';
  };
  receiver: {
    id: string;
    type: 'account' | 'wallet' | 'institution';
  };
  description?: string;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
  metadata?: Record<string, any>;
}

export interface TransactionStatus {
  transactionId: string;
  status: 'pending' | 'processing' | 'completed' | 'failed' | 'cancelled';
  progress: number;
  statusMessage: string;
  estimatedCompletion?: string;
  lastUpdated: string;
  steps: Array<{
    step: string;
    status: 'pending' | 'processing' | 'completed' | 'failed';
    timestamp?: string;
    details?: string;
  }>;
}