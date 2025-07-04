/**
 * Authentication error class
 */

import { LegacyBaaSError } from './LegacyBaaSError';

export class AuthenticationError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 401, errorCode: string = 'AUTHENTICATION_FAILED') {
    super(message, statusCode, errorCode);
    this.name = 'AuthenticationError';
  }
}