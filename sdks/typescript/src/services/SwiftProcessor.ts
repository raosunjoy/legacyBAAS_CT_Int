/**
 * SWIFT Message Processing Service
 * Enhanced SwiftParser integration with blockchain routing
 */

import { LegacyBaaSClient } from '../client/LegacyBaaSClient';
import { ApiResponse } from '../types';
import { LegacyBaaSError } from '../errors';

export interface SwiftMessage {
  messageId: string;
  messageType: string;
  content: string;
  sender: string;
  receiver: string;
  timestamp: string;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
  metadata?: Record<string, any>;
}

export interface SwiftProcessingOptions {
  validateOnly?: boolean;
  asyncProcessing?: boolean;
  priorityLevel?: 'low' | 'normal' | 'high' | 'urgent';
  enableBlockchain?: boolean;
  routingPreference?: 'fastest' | 'cheapest' | 'most_secure';
}

export interface SwiftProcessingResult {
  messageId: string;
  status: 'processed' | 'failed' | 'pending' | 'routing';
  blockchainTxId?: string;
  routingDecision?: {
    selectedNetwork: string;
    estimatedFees: number;
    estimatedTime: string;
    confidence: number;
  };
  validationResult: {
    isValid: boolean;
    errors: string[];
    warnings: string[];
  };
  processingTime: number;
  timestamp: string;
}

export interface MT103Data {
  transactionReference: string;
  bankOperationCode?: string;
  instructionCode?: string;
  senderToReceiverInfo?: string;
  orderingCustomer: string;
  orderingInstitution?: string;
  sendersCorrespondent?: string;
  receiversCorrespondent?: string;
  intermediaryInstitution?: string;
  accountWithInstitution?: string;
  beneficiaryCustomer: string;
  remittanceInformation?: string;
  amount: string;
  currency: string;
  valueDate: string;
}

export interface MT202Data {
  transactionReference: string;
  relatedReference?: string;
  timeIndication?: string;
  sendersCorrespondent?: string;
  receiversCorrespondent?: string;
  intermediaryInstitution?: string;
  accountWithInstitution: string;
  beneficiaryInstitution: string;
  amount: string;
  currency: string;
  valueDate: string;
}

export class SwiftProcessor {
  constructor(private client: LegacyBaaSClient) {}

  /**
   * Process SWIFT message with enhanced routing
   */
  async processMessage(
    message: SwiftMessage,
    options: SwiftProcessingOptions = {}
  ): Promise<SwiftProcessingResult> {
    const response = await this.client.makeRequest<SwiftProcessingResult>(
      'POST',
      '/swift/process',
      {
        message,
        options: {
          validateOnly: options.validateOnly || false,
          asyncProcessing: options.asyncProcessing || false,
          priorityLevel: options.priorityLevel || 'normal',
          enableBlockchain: options.enableBlockchain !== false,
          routingPreference: options.routingPreference || 'fastest'
        }
      }
    );

    return response.data;
  }

  /**
   * Process MT103 Customer Credit Transfer
   */
  async processMT103(
    mt103Data: MT103Data,
    options: SwiftProcessingOptions = {}
  ): Promise<SwiftProcessingResult> {
    const message: SwiftMessage = {
      messageId: mt103Data.transactionReference,
      messageType: 'MT103',
      content: this.formatMT103(mt103Data),
      sender: mt103Data.orderingCustomer,
      receiver: mt103Data.beneficiaryCustomer,
      timestamp: new Date().toISOString(),
      priority: options.priorityLevel || 'high'
    };

    return this.processMessage(message, {
      ...options,
      enableBlockchain: true,
      priorityLevel: 'high'
    });
  }

  /**
   * Process MT202 Financial Institution Transfer
   */
  async processMT202(
    mt202Data: MT202Data,
    options: SwiftProcessingOptions = {}
  ): Promise<SwiftProcessingResult> {
    const message: SwiftMessage = {
      messageId: mt202Data.transactionReference,
      messageType: 'MT202',
      content: this.formatMT202(mt202Data),
      sender: mt202Data.accountWithInstitution,
      receiver: mt202Data.beneficiaryInstitution,
      timestamp: new Date().toISOString(),
      priority: options.priorityLevel || 'high'
    };

    return this.processMessage(message, {
      ...options,
      enableBlockchain: true,
      priorityLevel: 'high'
    });
  }

  /**
   * Parse raw SWIFT message
   */
  async parseMessage(rawMessage: string): Promise<SwiftMessage> {
    const response = await this.client.makeRequest<SwiftMessage>(
      'POST',
      '/swift/parse',
      { rawMessage }
    );

    return response.data;
  }

  /**
   * Validate SWIFT message format
   */
  async validateMessage(message: SwiftMessage): Promise<{
    isValid: boolean;
    errors: string[];
    warnings: string[];
    messageType: string;
  }> {
    const response = await this.client.makeRequest<{
      isValid: boolean;
      errors: string[];
      warnings: string[];
      messageType: string;
    }>('POST', '/swift/validate', { message });

    return response.data;
  }

  /**
   * Get message processing status
   */
  async getMessageStatus(messageId: string): Promise<{
    messageId: string;
    status: string;
    progress: number;
    blockchainTxId?: string;
    errors?: string[];
    lastUpdate: string;
  }> {
    const response = await this.client.makeRequest<{
      messageId: string;
      status: string;
      progress: number;
      blockchainTxId?: string;
      errors?: string[];
      lastUpdate: string;
    }>('GET', `/swift/status/${messageId}`);

    return response.data;
  }

  /**
   * Get processing history
   */
  async getProcessingHistory(filters: {
    startDate?: string;
    endDate?: string;
    messageType?: string;
    status?: string;
    limit?: number;
    offset?: number;
  } = {}): Promise<{
    results: SwiftProcessingResult[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
  }> {
    const response = await this.client.makeRequest<{
      results: SwiftProcessingResult[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
    }>('GET', '/swift/history', undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Get supported message types
   */
  async getSupportedMessageTypes(): Promise<{
    messageTypes: string[];
    capabilities: Record<string, {
      blockchainSupported: boolean;
      validationRules: string[];
      processingTime: string;
    }>;
  }> {
    const response = await this.client.makeRequest<{
      messageTypes: string[];
      capabilities: Record<string, {
        blockchainSupported: boolean;
        validationRules: string[];
        processingTime: string;
      }>;
    }>('GET', '/swift/message-types');

    return response.data;
  }

  /**
   * Get SWIFT network metrics
   */
  async getMetrics(): Promise<{
    totalMessages: number;
    successRate: number;
    averageProcessingTime: number;
    blockchainRouted: number;
    networkHealth: Record<string, number>;
  }> {
    const response = await this.client.makeRequest<{
      totalMessages: number;
      successRate: number;
      averageProcessingTime: number;
      blockchainRouted: number;
      networkHealth: Record<string, number>;
    }>('GET', '/swift/metrics');

    return response.data;
  }

  /**
   * Format MT103 message
   */
  private formatMT103(data: MT103Data): string {
    const lines = [
      `{1:${data.transactionReference}}`,
      `{2:${data.amount}${data.currency}}`,
      `{3:${data.valueDate}}`,
      `{4:`,
      `:20:${data.transactionReference}`,
      data.bankOperationCode ? `:23B:${data.bankOperationCode}` : '',
      data.instructionCode ? `:23E:${data.instructionCode}` : '',
      `:32A:${data.valueDate}${data.currency}${data.amount}`,
      `:50K:${data.orderingCustomer}`,
      data.orderingInstitution ? `:52A:${data.orderingInstitution}` : '',
      data.sendersCorrespondent ? `:53A:${data.sendersCorrespondent}` : '',
      data.receiversCorrespondent ? `:54A:${data.receiversCorrespondent}` : '',
      data.intermediaryInstitution ? `:56A:${data.intermediaryInstitution}` : '',
      data.accountWithInstitution ? `:57A:${data.accountWithInstitution}` : '',
      `:59:${data.beneficiaryCustomer}`,
      data.remittanceInformation ? `:70:${data.remittanceInformation}` : '',
      data.senderToReceiverInfo ? `:72:${data.senderToReceiverInfo}` : '',
      `-}`
    ];

    return lines.filter(line => line.trim()).join('\n');
  }

  /**
   * Format MT202 message
   */
  private formatMT202(data: MT202Data): string {
    const lines = [
      `{1:${data.transactionReference}}`,
      `{2:${data.amount}${data.currency}}`,
      `{3:${data.valueDate}}`,
      `{4:`,
      `:20:${data.transactionReference}`,
      data.relatedReference ? `:21:${data.relatedReference}` : '',
      data.timeIndication ? `:13C:${data.timeIndication}` : '',
      `:32A:${data.valueDate}${data.currency}${data.amount}`,
      data.sendersCorrespondent ? `:53A:${data.sendersCorrespondent}` : '',
      data.receiversCorrespondent ? `:54A:${data.receiversCorrespondent}` : '',
      data.intermediaryInstitution ? `:56A:${data.intermediaryInstitution}` : '',
      `:57A:${data.accountWithInstitution}`,
      `:58A:${data.beneficiaryInstitution}`,
      `-}`
    ];

    return lines.filter(line => line.trim()).join('\n');
  }
}