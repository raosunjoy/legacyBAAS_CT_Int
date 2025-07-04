/**
 * Error classes for LegacyBAAS SDK
 */

export class LegacyBaaSError extends Error {
  public readonly statusCode?: number;
  public readonly errorCode?: string;
  public readonly details?: any;

  constructor(
    message: string,
    statusCode?: number,
    errorCode?: string,
    details?: any
  ) {
    super(message);
    this.name = 'LegacyBaaSError';
    this.statusCode = statusCode;
    this.errorCode = errorCode;
    this.details = details;

    // Maintain proper stack trace for where our error was thrown
    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, LegacyBaaSError);
    }
  }
}

export class AuthenticationError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 401, errorCode: string = 'AUTHENTICATION_FAILED') {
    super(message, statusCode, errorCode);
    this.name = 'AuthenticationError';
  }
}

export class ValidationError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 400, errorCode: string = 'VALIDATION_ERROR', details?: any) {
    super(message, statusCode, errorCode, details);
    this.name = 'ValidationError';
  }
}

export class NetworkError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 500, errorCode: string = 'NETWORK_ERROR') {
    super(message, statusCode, errorCode);
    this.name = 'NetworkError';
  }
}

export class RateLimitError extends LegacyBaaSError {
  public readonly retryAfter?: number;

  constructor(message: string, retryAfter?: number) {
    super(message, 429, 'RATE_LIMIT_EXCEEDED');
    this.name = 'RateLimitError';
    this.retryAfter = retryAfter;
  }
}

export class ComplianceError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 403, errorCode: string = 'COMPLIANCE_VIOLATION') {
    super(message, statusCode, errorCode);
    this.name = 'ComplianceError';
  }
}

export class SwiftProcessingError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 422, errorCode: string = 'SWIFT_PROCESSING_ERROR') {
    super(message, statusCode, errorCode);
    this.name = 'SwiftProcessingError';
  }
}

export class BlockchainError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 422, errorCode: string = 'BLOCKCHAIN_ERROR') {
    super(message, statusCode, errorCode);
    this.name = 'BlockchainError';
  }
}

export class BankingError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 422, errorCode: string = 'BANKING_ERROR') {
    super(message, statusCode, errorCode);
    this.name = 'BankingError';
  }
}

// Re-export individual error classes
export * from './LegacyBaaSError';
export * from './AuthenticationError';
export * from './ValidationError';
export * from './NetworkError';