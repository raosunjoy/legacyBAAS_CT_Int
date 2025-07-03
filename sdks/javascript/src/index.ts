/**
 * Banking Legacy-to-Blockchain B2BaaS Platform SDK
 * JavaScript/TypeScript Client Library
 * 
 * @version 1.0.0
 * @author Legacy B2BaaS Platform
 * @license MIT
 */

export { LegacyBaaSClient } from './client';
export { SwiftProcessor } from './services/swift';
export { BlockchainRouter } from './services/blockchain';
export { BancsIntegration } from './services/bancs';
export { AnalyticsService } from './services/analytics';
export { WebhookHandler } from './services/webhooks';

// Type definitions
export * from './types/swift';
export * from './types/blockchain';
export * from './types/bancs';
export * from './types/analytics';
export * from './types/common';

// Error classes
export * from './errors';

// Utilities
export * from './utils';

// Configuration
export * from './config';

/**
 * SDK Version
 */
export const SDK_VERSION = '1.0.0';

/**
 * Default configuration
 */
export const DEFAULT_CONFIG = {
  baseUrl: 'https://api.legacybaas.com',
  version: 'v1',
  timeout: 30000,
  retries: 3,
  retryDelay: 1000
};

/**
 * Quick start client initialization
 * @param apiKey Your Legacy B2BaaS API key
 * @param options Additional configuration options
 * @returns Configured LegacyBaaSClient instance
 */
export function createClient(apiKey: string, options?: Partial<typeof DEFAULT_CONFIG>) {
  return new LegacyBaaSClient(apiKey, { ...DEFAULT_CONFIG, ...options });
}