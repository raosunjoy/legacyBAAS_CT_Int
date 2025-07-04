/**
 * Validation error class
 */

import { LegacyBaaSError } from './LegacyBaaSError';

export class ValidationError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 400, errorCode: string = 'VALIDATION_ERROR', details?: any) {
    super(message, statusCode, errorCode, details);
    this.name = 'ValidationError';
  }
}