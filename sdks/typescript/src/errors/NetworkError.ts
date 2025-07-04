/**
 * Network error class
 */

import { LegacyBaaSError } from './LegacyBaaSError';

export class NetworkError extends LegacyBaaSError {
  constructor(message: string, statusCode: number = 500, errorCode: string = 'NETWORK_ERROR') {
    super(message, statusCode, errorCode);
    this.name = 'NetworkError';
  }
}