/**
 * Webhook Handler Service
 * Manage webhooks for real-time event notifications
 */

import { LegacyBaaSClient } from '../client/LegacyBaaSClient';
import { ApiResponse } from '../types';

export interface WebhookConfig {
  webhookId?: string;
  url: string;
  events: string[];
  secret?: string;
  description?: string;
  isActive?: boolean;
  retryConfig?: {
    maxRetries: number;
    retryDelay: number;
    exponentialBackoff: boolean;
  };
  filters?: Record<string, any>;
  headers?: Record<string, string>;
}

export interface WebhookEvent {
  eventId: string;
  eventType: string;
  timestamp: string;
  data: Record<string, any>;
  metadata: {
    source: string;
    version: string;
    correlationId?: string;
    traceId?: string;
  };
}

export interface WebhookDelivery {
  deliveryId: string;
  webhookId: string;
  eventId: string;
  status: 'pending' | 'delivered' | 'failed' | 'retrying';
  attempts: number;
  lastAttempt: string;
  nextAttempt?: string;
  responseCode?: number;
  responseBody?: string;
  error?: string;
  deliveredAt?: string;
}

export interface WebhookStats {
  webhookId: string;
  totalEvents: number;
  deliveredEvents: number;
  failedEvents: number;
  deliveryRate: number;
  avgResponseTime: number;
  lastDelivery?: string;
  errorRate: number;
  retryRate: number;
}

export class WebhookHandler {
  constructor(private client: LegacyBaaSClient) {}

  /**
   * Create webhook
   */
  async createWebhook(config: WebhookConfig): Promise<{
    webhookId: string;
    url: string;
    secret: string;
    events: string[];
    status: 'active' | 'inactive';
    createdAt: string;
  }> {
    const response = await this.client.makeRequest<{
      webhookId: string;
      url: string;
      secret: string;
      events: string[];
      status: 'active' | 'inactive';
      createdAt: string;
    }>('POST', '/webhooks', {
      ...config,
      retryConfig: config.retryConfig || {
        maxRetries: 3,
        retryDelay: 1000,
        exponentialBackoff: true
      }
    });

    return response.data;
  }

  /**
   * Get webhook
   */
  async getWebhook(webhookId: string): Promise<WebhookConfig & {
    webhookId: string;
    createdAt: string;
    updatedAt: string;
    status: 'active' | 'inactive';
  }> {
    const response = await this.client.makeRequest<WebhookConfig & {
      webhookId: string;
      createdAt: string;
      updatedAt: string;
      status: 'active' | 'inactive';
    }>('GET', `/webhooks/${webhookId}`);

    return response.data;
  }

  /**
   * List webhooks
   */
  async listWebhooks(filters: {
    isActive?: boolean;
    eventType?: string;
    limit?: number;
    offset?: number;
  } = {}): Promise<{
    webhooks: Array<{
      webhookId: string;
      url: string;
      events: string[];
      isActive: boolean;
      createdAt: string;
      lastDelivery?: string;
      deliveryRate: number;
    }>;
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
  }> {
    const response = await this.client.makeRequest<{
      webhooks: Array<{
        webhookId: string;
        url: string;
        events: string[];
        isActive: boolean;
        createdAt: string;
        lastDelivery?: string;
        deliveryRate: number;
      }>;
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
    }>('GET', '/webhooks', undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Update webhook
   */
  async updateWebhook(
    webhookId: string,
    updates: Partial<WebhookConfig>
  ): Promise<{
    webhookId: string;
    status: 'updated';
    updatedAt: string;
  }> {
    const response = await this.client.makeRequest<{
      webhookId: string;
      status: 'updated';
      updatedAt: string;
    }>('PUT', `/webhooks/${webhookId}`, updates);

    return response.data;
  }

  /**
   * Delete webhook
   */
  async deleteWebhook(webhookId: string): Promise<{
    webhookId: string;
    status: 'deleted';
    deletedAt: string;
  }> {
    const response = await this.client.makeRequest<{
      webhookId: string;
      status: 'deleted';
      deletedAt: string;
    }>('DELETE', `/webhooks/${webhookId}`);

    return response.data;
  }

  /**
   * Test webhook
   */
  async testWebhook(webhookId: string, testEvent?: WebhookEvent): Promise<{
    webhookId: string;
    testResult: {
      success: boolean;
      statusCode?: number;
      responseTime: number;
      responseBody?: string;
      error?: string;
    };
    timestamp: string;
  }> {
    const response = await this.client.makeRequest<{
      webhookId: string;
      testResult: {
        success: boolean;
        statusCode?: number;
        responseTime: number;
        responseBody?: string;
        error?: string;
      };
      timestamp: string;
    }>('POST', `/webhooks/${webhookId}/test`, testEvent ? { event: testEvent } : {});

    return response.data;
  }

  /**
   * Get webhook deliveries
   */
  async getWebhookDeliveries(
    webhookId: string,
    filters: {
      status?: 'pending' | 'delivered' | 'failed' | 'retrying';
      startDate?: string;
      endDate?: string;
      limit?: number;
      offset?: number;
    } = {}
  ): Promise<{
    deliveries: WebhookDelivery[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
  }> {
    const response = await this.client.makeRequest<{
      deliveries: WebhookDelivery[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
    }>('GET', `/webhooks/${webhookId}/deliveries`, undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Get delivery details
   */
  async getDeliveryDetails(deliveryId: string): Promise<WebhookDelivery & {
    event: WebhookEvent;
    requestHeaders: Record<string, string>;
    responseHeaders?: Record<string, string>;
    duration: number;
  }> {
    const response = await this.client.makeRequest<WebhookDelivery & {
      event: WebhookEvent;
      requestHeaders: Record<string, string>;
      responseHeaders?: Record<string, string>;
      duration: number;
    }>('GET', `/webhooks/deliveries/${deliveryId}`);

    return response.data;
  }

  /**
   * Retry webhook delivery
   */
  async retryDelivery(deliveryId: string): Promise<{
    deliveryId: string;
    status: 'retrying';
    nextAttempt: string;
  }> {
    const response = await this.client.makeRequest<{
      deliveryId: string;
      status: 'retrying';
      nextAttempt: string;
    }>('POST', `/webhooks/deliveries/${deliveryId}/retry`);

    return response.data;
  }

  /**
   * Get webhook statistics
   */
  async getWebhookStats(
    webhookId: string,
    timeRange: { start: string; end: string }
  ): Promise<WebhookStats & {
    timeline: Array<{
      timestamp: string;
      delivered: number;
      failed: number;
      avgResponseTime: number;
    }>;
    topErrors: Array<{
      error: string;
      count: number;
      percentage: number;
    }>;
  }> {
    const response = await this.client.makeRequest<WebhookStats & {
      timeline: Array<{
        timestamp: string;
        delivered: number;
        failed: number;
        avgResponseTime: number;
      }>;
      topErrors: Array<{
        error: string;
        count: number;
        percentage: number;
      }>;
    }>('GET', `/webhooks/${webhookId}/stats`, undefined, {
      headers: { 'X-Time-Range': JSON.stringify(timeRange) }
    });

    return response.data;
  }

  /**
   * Get available events
   */
  async getAvailableEvents(): Promise<{
    events: Array<{
      eventType: string;
      category: string;
      description: string;
      samplePayload: Record<string, any>;
    }>;
    categories: Array<{
      category: string;
      description: string;
      eventCount: number;
    }>;
  }> {
    const response = await this.client.makeRequest<{
      events: Array<{
        eventType: string;
        category: string;
        description: string;
        samplePayload: Record<string, any>;
      }>;
      categories: Array<{
        category: string;
        description: string;
        eventCount: number;
      }>;
    }>('GET', '/webhooks/events');

    return response.data;
  }

  /**
   * Verify webhook signature
   */
  static verifySignature(
    payload: string,
    signature: string,
    secret: string
  ): boolean {
    const crypto = require('crypto');
    const expectedSignature = crypto
      .createHmac('sha256', secret)
      .update(payload, 'utf8')
      .digest('hex');
    
    const providedSignature = signature.startsWith('sha256=') 
      ? signature.slice(7) 
      : signature;

    return crypto.timingSafeEqual(
      Buffer.from(expectedSignature, 'hex'),
      Buffer.from(providedSignature, 'hex')
    );
  }

  /**
   * Parse webhook payload
   */
  static parseWebhookPayload(payload: string): WebhookEvent {
    try {
      return JSON.parse(payload);
    } catch (error) {
      throw new Error('Invalid webhook payload format');
    }
  }

  /**
   * Get webhook health
   */
  async getWebhookHealth(): Promise<{
    totalWebhooks: number;
    activeWebhooks: number;
    totalDeliveries: number;
    successfulDeliveries: number;
    failedDeliveries: number;
    avgDeliveryTime: number;
    overallHealthScore: number;
    issues: Array<{
      webhookId: string;
      issue: string;
      severity: 'low' | 'medium' | 'high';
      lastOccurrence: string;
    }>;
  }> {
    const response = await this.client.makeRequest<{
      totalWebhooks: number;
      activeWebhooks: number;
      totalDeliveries: number;
      successfulDeliveries: number;
      failedDeliveries: number;
      avgDeliveryTime: number;
      overallHealthScore: number;
      issues: Array<{
        webhookId: string;
        issue: string;
        severity: 'low' | 'medium' | 'high';
        lastOccurrence: string;
      }>;
    }>('GET', '/webhooks/health');

    return response.data;
  }
}