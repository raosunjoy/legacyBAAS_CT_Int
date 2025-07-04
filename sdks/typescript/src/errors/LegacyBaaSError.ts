/**
 * Base error class for LegacyBAAS SDK
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

  /**
   * Create error from API response
   */
  static fromApiResponse(response: any): LegacyBaaSError {
    const message = response.error?.message || response.message || 'Unknown error';
    const statusCode = response.status || response.statusCode;
    const errorCode = response.error?.code || response.code;
    const details = response.error?.details || response.details;

    return new LegacyBaaSError(message, statusCode, errorCode, details);
  }

  /**
   * Convert to JSON
   */
  toJSON() {
    return {
      name: this.name,
      message: this.message,
      statusCode: this.statusCode,
      errorCode: this.errorCode,
      details: this.details,
      stack: this.stack
    };
  }
}