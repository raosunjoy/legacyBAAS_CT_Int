/**
 * Main Legacy B2BaaS Client
 * Central SDK client for all platform operations
 */

import axios, { AxiosInstance, AxiosResponse } from 'axios';
import { EventEmitter } from 'eventemitter3';
import WebSocket from 'ws';

import { SwiftProcessor } from './services/swift';
import { BlockchainRouter } from './services/blockchain';
import { BancsIntegration } from './services/bancs';
import { AnalyticsService } from './services/analytics';
import { WebhookHandler } from './services/webhooks';

import { ClientConfig, ApiResponse, HealthStatus } from './types/common';
import { LegacyBaaSError } from './errors';

/**
 * Main Legacy B2BaaS Platform Client
 */
export class LegacyBaaSClient extends EventEmitter {
  private http: AxiosInstance;
  private ws?: WebSocket;
  
  // Service instances
  public swift: SwiftProcessor;
  public blockchain: BlockchainRouter;
  public bancs: BancsIntegration;
  public analytics: AnalyticsService;
  public webhooks: WebhookHandler;

  constructor(
    private apiKey: string,
    private config: ClientConfig
  ) {
    super();
    
    // Validate API key
    if (!apiKey || typeof apiKey !== 'string') {
      throw new LegacyBaaSError('API key is required and must be a string');
    }
    
    // Initialize HTTP client
    this.http = axios.create({
      baseURL: `${config.baseUrl}/api/${config.version}`,
      timeout: config.timeout,
      headers: {
        'Authorization': `Bearer ${apiKey}`,
        'Content-Type': 'application/json',
        'User-Agent': `LegacyBaaS-JS-SDK/1.0.0`,
        'X-SDK-Version': '1.0.0'
      }
    });
    
    // Add response interceptor for error handling
    this.http.interceptors.response.use(
      (response: AxiosResponse) => response,
      (error) => {
        const apiError = new LegacyBaaSError(
          error.response?.data?.message || error.message,
          error.response?.status,
          error.response?.data?.code
        );
        this.emit('error', apiError);
        throw apiError;
      }
    );
    
    // Initialize services
    this.swift = new SwiftProcessor(this.http);
    this.blockchain = new BlockchainRouter(this.http);
    this.bancs = new BancsIntegration(this.http);
    this.analytics = new AnalyticsService(this.http);
    this.webhooks = new WebhookHandler(this.http);
    
    // Auto-connect WebSocket if enabled
    if (config.enableRealTime) {
      this.connectWebSocket();
    }
  }

  /**
   * Connect to real-time WebSocket feed
   */
  public async connectWebSocket(): Promise<void> {
    try {
      const wsUrl = this.config.baseUrl.replace('http', 'ws') + '/ws';
      
      this.ws = new WebSocket(wsUrl, {
        headers: {
          'Authorization': `Bearer ${this.apiKey}`
        }
      });
      
      this.ws.on('open', () => {
        this.emit('connected');
      });
      
      this.ws.on('message', (data: string) => {
        try {
          const message = JSON.parse(data);
          this.emit('message', message);
          
          // Emit specific event types
          if (message.type) {
            this.emit(message.type, message.data);
          }
        } catch (error) {
          this.emit('error', new LegacyBaaSError('Invalid WebSocket message format'));
        }
      });
      
      this.ws.on('close', () => {
        this.emit('disconnected');
      });
      
      this.ws.on('error', (error: Error) => {
        this.emit('error', new LegacyBaaSError(`WebSocket error: ${error.message}`));
      });
      
    } catch (error) {
      throw new LegacyBaaSError(`Failed to connect WebSocket: ${error}`);
    }
  }

  /**
   * Disconnect WebSocket
   */
  public disconnectWebSocket(): void {
    if (this.ws) {
      this.ws.close();
      this.ws = undefined;
    }
  }

  /**
   * Check platform health status
   */
  public async health(): Promise<HealthStatus> {
    try {
      const response = await this.http.get<ApiResponse<HealthStatus>>('/health');
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Health check failed: ${error}`);
    }
  }

  /**
   * Test API connectivity
   */
  public async ping(): Promise<{ message: string; timestamp: string }> {
    try {
      const response = await this.http.get<ApiResponse<{ message: string; timestamp: string }>>('/ping');
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Ping failed: ${error}`);
    }
  }

  /**
   * Get account information
   */
  public async getAccount(): Promise<any> {
    try {
      const response = await this.http.get<ApiResponse<any>>('/account');
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get account info: ${error}`);
    }
  }

  /**
   * Get API usage statistics
   */
  public async getUsage(period: 'day' | 'week' | 'month' = 'day'): Promise<any> {
    try {
      const response = await this.http.get<ApiResponse<any>>(`/usage?period=${period}`);
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get usage statistics: ${error}`);
    }
  }

  /**
   * Close all connections and cleanup
   */
  public close(): void {
    this.disconnectWebSocket();
    this.removeAllListeners();
  }
}