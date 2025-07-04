/**
 * Retry handler utility for LegacyBAAS SDK
 */

import { Logger } from './Logger';

export interface RetryConfig {
  maxRetries: number;
  retryDelay: number;
  exponentialBackoff?: boolean;
  retryableStatusCodes?: number[];
  retryableErrors?: string[];
}

export class RetryHandler {
  private config: RetryConfig;
  private logger: Logger;

  constructor(config: Partial<RetryConfig>, logger?: Logger) {
    this.config = {
      maxRetries: config.maxRetries || 3,
      retryDelay: config.retryDelay || 1000,
      exponentialBackoff: config.exponentialBackoff !== false,
      retryableStatusCodes: config.retryableStatusCodes || [429, 500, 502, 503, 504],
      retryableErrors: config.retryableErrors || ['NETWORK_ERROR', 'TIMEOUT', 'CONNECTION_RESET']
    };
    this.logger = logger || new Logger('WARN', 'RetryHandler');
  }

  /**
   * Execute function with retry logic
   */
  async retry<T>(
    fn: () => Promise<T>,
    context?: string
  ): Promise<T> {
    let lastError: Error;
    
    for (let attempt = 0; attempt <= this.config.maxRetries; attempt++) {
      try {
        if (attempt > 0) {
          const delay = this.calculateDelay(attempt);
          this.logger.debug(`Retrying ${context || 'operation'} - attempt ${attempt}/${this.config.maxRetries} after ${delay}ms`);
          await this.sleep(delay);
        }

        const result = await fn();
        
        if (attempt > 0) {
          this.logger.info(`${context || 'Operation'} succeeded on attempt ${attempt + 1}`);
        }
        
        return result;
        
      } catch (error) {
        lastError = error as Error;
        
        if (!this.isRetryableError(error)) {
          this.logger.debug(`Non-retryable error for ${context || 'operation'}:`, error);
          throw error;
        }
        
        if (attempt === this.config.maxRetries) {
          this.logger.error(`Max retries (${this.config.maxRetries}) reached for ${context || 'operation'}`, error);
          break;
        }
        
        this.logger.warn(`Attempt ${attempt + 1} failed for ${context || 'operation'}:`, error);
      }
    }
    
    throw lastError!;
  }

  /**
   * Check if error is retryable
   */
  private isRetryableError(error: any): boolean {
    // Check status codes
    if (error.statusCode || error.status) {
      const statusCode = error.statusCode || error.status;
      if (this.config.retryableStatusCodes!.includes(statusCode)) {
        return true;
      }
    }
    
    // Check error codes
    if (error.errorCode || error.code) {
      const errorCode = error.errorCode || error.code;
      if (this.config.retryableErrors!.includes(errorCode)) {
        return true;
      }
    }
    
    // Check error message patterns
    const message = error.message?.toLowerCase() || '';
    const retryablePatterns = [
      'timeout',
      'connection reset',
      'network error',
      'econnreset',
      'enotfound',
      'socket hang up'
    ];
    
    return retryablePatterns.some(pattern => message.includes(pattern));
  }

  /**
   * Calculate retry delay with optional exponential backoff
   */
  private calculateDelay(attempt: number): number {
    if (this.config.exponentialBackoff) {
      return Math.min(
        this.config.retryDelay * Math.pow(2, attempt - 1),
        30000 // Max 30 seconds
      );
    }
    
    return this.config.retryDelay;
  }

  /**
   * Sleep for specified milliseconds
   */
  private sleep(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Update retry configuration
   */
  updateConfig(updates: Partial<RetryConfig>): void {
    this.config = { ...this.config, ...updates };
    this.logger.debug('Retry configuration updated', updates);
  }

  /**
   * Get current configuration
   */
  getConfig(): RetryConfig {
    return { ...this.config };
  }
}