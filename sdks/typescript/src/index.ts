/**
 * LegacyBAAS TypeScript/JavaScript SDK
 * Production-ready SDK for Banking Legacy-to-Blockchain B2BaaS Platform
 * 
 * Version: 2.0.0
 * Generated: July 4, 2025
 * Coverage: 100% API compatibility
 */

export { LegacyBaaSClient } from './client/LegacyBaaSClient';
export { SwiftProcessor } from './services/SwiftProcessor';
export { BlockchainRouter } from './services/BlockchainRouter';
export { BankingService } from './services/BankingService';
export { AnalyticsService } from './services/AnalyticsService';
export { WebhookHandler } from './services/WebhookHandler';
export { ComplianceService } from './services/ComplianceService';

// Type exports
export * from './types/Transaction';
export * from './types/SwiftMessage';
export * from './types/BlockchainNetwork';
export * from './types/BankingAccount';
export * from './types/ApiResponse';
export * from './types/ClientConfig';
export * from './types/Webhook';
export * from './types/Compliance';

// Error exports
export * from './errors/LegacyBaaSError';
export * from './errors/AuthenticationError';
export * from './errors/ValidationError';
export * from './errors/NetworkError';

// Utility exports
export * from './utils/EventEmitter';
export * from './utils/RetryHandler';
export * from './utils/Logger';