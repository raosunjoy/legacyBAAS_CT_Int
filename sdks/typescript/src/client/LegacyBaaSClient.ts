/**
 * LegacyBAAS Main Client
 * Entry point for all SDK operations
 */

import { EventEmitter } from 'events';
import { SwiftProcessor } from '../services/SwiftProcessor';
import { BlockchainRouter } from '../services/BlockchainRouter';
import { BankingService } from '../services/BankingService';
import { AnalyticsService } from '../services/AnalyticsService';
import { WebhookHandler } from '../services/WebhookHandler';
import { ComplianceService } from '../services/ComplianceService';
import { ClientConfig, ApiResponse } from '../types';
import { AuthenticationError, LegacyBaaSError } from '../errors';
import { Logger } from '../utils/Logger';
import { RetryHandler } from '../utils/RetryHandler';

export interface LegacyBaaSClientConfig {
  apiKey?: string;
  clientId?: string;
  clientSecret?: string;
  baseUrl?: string;
  environment?: 'production' | 'sandbox' | 'development';
  timeout?: number;
  maxRetries?: number;
  retryDelay?: number;
  enableRealTime?: boolean;
  enableLogging?: boolean;
  logLevel?: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';
}

export class LegacyBaaSClient extends EventEmitter {
  private config: ClientConfig;
  private accessToken?: string;
  private tokenExpiry?: number;
  private logger: Logger;
  private retryHandler: RetryHandler;
  
  // Service instances
  public readonly swift: SwiftProcessor;
  public readonly blockchain: BlockchainRouter;
  public readonly banking: BankingService;
  public readonly analytics: AnalyticsService;
  public readonly webhooks: WebhookHandler;
  public readonly compliance: ComplianceService;

  constructor(config: LegacyBaaSClientConfig) {
    super();
    
    // Set default configuration
    this.config = {
      baseUrl: this.getBaseUrl(config.environment || 'production'),
      version: 'v1',
      timeout: config.timeout || 30000,
      maxRetries: config.maxRetries || 3,
      retryDelay: config.retryDelay || 1000,
      enableRealTime: config.enableRealTime !== false,
      enableLogging: config.enableLogging !== false,
      logLevel: config.logLevel || 'INFO',
      ...config
    };

    // Initialize utilities
    this.logger = new Logger(this.config.logLevel);
    this.retryHandler = new RetryHandler(this.config);

    // Initialize services
    this.swift = new SwiftProcessor(this);
    this.blockchain = new BlockchainRouter(this);
    this.banking = new BankingService(this);
    this.analytics = new AnalyticsService(this);
    this.webhooks = new WebhookHandler(this);
    this.compliance = new ComplianceService(this);

    this.logger.info('LegacyBAAS Client initialized', {
      environment: config.environment,
      baseUrl: this.config.baseUrl,
      enableRealTime: this.config.enableRealTime
    });
  }

  private getBaseUrl(environment: string): string {
    switch (environment) {
      case 'production':
        return 'https://api.legacybaas.com/v1';
      case 'sandbox':
        return 'https://sandbox.legacybaas.com/v1';
      case 'development':
        return 'http://localhost:3000/v1';
      default:
        return 'https://api.legacybaas.com/v1';
    }
  }

  /**
   * Authenticate with the LegacyBAAS API
   */
  public async authenticate(): Promise<void> {
    if (!this.config.clientId || !this.config.clientSecret) {
      throw new AuthenticationError('Client ID and Client Secret are required for authentication');
    }

    try {
      const response = await this.makeRequest<{
        access_token: string;
        token_type: string;
        expires_in: number;
      }>('POST', '/auth/token', {
        grant_type: 'client_credentials',
        client_id: this.config.clientId,
        client_secret: this.config.clientSecret,
        scope: 'banking:read banking:write blockchain:execute compliance:screen'
      }, { skipAuth: true });

      this.accessToken = response.data.access_token;
      this.tokenExpiry = Date.now() + (response.data.expires_in * 1000);
      
      this.logger.info('Authentication successful');
      this.emit('authenticated', { expiresIn: response.data.expires_in });
      
    } catch (error) {
      this.logger.error('Authentication failed', error);
      throw new AuthenticationError('Failed to authenticate with LegacyBAAS API');
    }
  }

  /**
   * Check if the current token is valid
   */
  public isAuthenticated(): boolean {
    return !!(this.accessToken && this.tokenExpiry && Date.now() < this.tokenExpiry);
  }

  /**
   * Ensure we have a valid token, refresh if necessary
   */
  public async ensureAuthenticated(): Promise<void> {
    if (!this.isAuthenticated()) {
      await this.authenticate();
    }
  }

  /**
   * Make HTTP request to the API
   */
  public async makeRequest<T>(
    method: 'GET' | 'POST' | 'PUT' | 'DELETE',
    endpoint: string,
    data?: any,
    options: { skipAuth?: boolean; headers?: Record<string, string> } = {}
  ): Promise<ApiResponse<T>> {
    
    if (!options.skipAuth) {
      await this.ensureAuthenticated();
    }

    const url = `${this.config.baseUrl}${endpoint}`;
    const headers: Record<string, string> = {
      'Content-Type': 'application/json',
      'User-Agent': 'LegacyBAAS-SDK-TS/2.0.0',
      ...options.headers
    };

    if (this.accessToken && !options.skipAuth) {
      headers.Authorization = `Bearer ${this.accessToken}`;
    }

    const requestOptions: RequestInit = {
      method,
      headers,
      signal: AbortSignal.timeout(this.config.timeout)
    };

    if (data && (method === 'POST' || method === 'PUT')) {
      requestOptions.body = JSON.stringify(data);
    }

    try {
      const response = await this.retryHandler.retry(async () => {
        this.logger.debug(`Making ${method} request to ${endpoint}`, { data });
        
        const fetchResponse = await fetch(url, requestOptions);
        
        if (!fetchResponse.ok) {
          const errorData = await fetchResponse.json().catch(() => ({}));
          throw new LegacyBaaSError(
            errorData.message || `HTTP ${fetchResponse.status}`,
            fetchResponse.status,
            errorData.code
          );
        }

        return fetchResponse.json();
      });

      this.logger.debug(`Request completed: ${method} ${endpoint}`, { 
        status: response.success ? 'success' : 'error' 
      });

      return response;
      
    } catch (error) {
      this.logger.error(`Request failed: ${method} ${endpoint}`, error);
      throw error;
    }
  }

  /**
   * Get client configuration
   */
  public getConfig(): ClientConfig {
    return { ...this.config };
  }

  /**
   * Update client configuration
   */
  public updateConfig(updates: Partial<LegacyBaaSClientConfig>): void {
    this.config = { ...this.config, ...updates };
    this.logger.info('Client configuration updated', updates);
  }

  /**
   * Get API health status
   */
  public async getHealth(): Promise<ApiResponse<{
    status: string;
    version: string;
    timestamp: string;
    services: Record<string, string>;
  }>> {
    return this.makeRequest('GET', '/health');
  }

  /**
   * Get platform metrics
   */
  public async getMetrics(): Promise<ApiResponse<{
    system: Record<string, number>;
    api: Record<string, number>;
    banking: Record<string, number>;
    blockchain: Record<string, number>;
  }>> {
    return this.makeRequest('GET', '/admin/metrics');
  }

  /**
   * Close client and cleanup resources
   */
  public async close(): Promise<void> {
    this.removeAllListeners();
    this.accessToken = undefined;
    this.tokenExpiry = undefined;
    this.logger.info('LegacyBAAS Client closed');
  }
}