/**
 * Client configuration types
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

export interface AuthConfig {
  clientId: string;
  clientSecret: string;
  scope?: string;
  audience?: string;
}

export interface RetryConfig {
  maxRetries: number;
  retryDelay: number;
  exponentialBackoff: boolean;
  retryableStatusCodes?: number[];
}