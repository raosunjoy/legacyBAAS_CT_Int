/**
 * Webhook type definitions
 */

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