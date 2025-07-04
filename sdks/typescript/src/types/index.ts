/**
 * Core type definitions for LegacyBAAS SDK
 */

export interface ClientConfig {
  baseUrl: string;
  version: string;
  timeout: number;
  maxRetries: number;
  retryDelay: number;
  enableRealTime: boolean;
  enableLogging: boolean;
  logLevel: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';
  apiKey?: string;
  clientId?: string;
  clientSecret?: string;
}

export interface ApiResponse<T> {
  success: boolean;
  data: T;
  message?: string;
  error?: {
    code: string;
    message: string;
    details?: any;
  };
  metadata?: {
    requestId: string;
    timestamp: string;
    version: string;
    processingTime?: number;
  };
}

export interface PaginatedResponse<T> {
  data: T[];
  pagination: {
    total: number;
    limit: number;
    offset: number;
    hasMore: boolean;
  };
}

export interface ErrorResponse {
  success: false;
  error: {
    code: string;
    message: string;
    details?: any;
  };
  metadata?: {
    requestId: string;
    timestamp: string;
    version: string;
  };
}

// Re-export service types
export * from './Transaction';
export * from './SwiftMessage';
export * from './BlockchainNetwork';
export * from './BankingAccount';
export * from './ApiResponse';
export * from './ClientConfig';
export * from './Webhook';
export * from './Compliance';