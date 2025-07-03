/**
 * SWIFT Message Processing Service
 * Handle SWIFT MT messages and banking operations
 */

import { AxiosInstance } from 'axios';
import { LegacyBaaSError } from '../errors';
import { 
  SwiftMessage, 
  SwiftProcessingResult, 
  SwiftValidationResult,
  MT103Message,
  MT202Message,
  MT700Message,
  SwiftProcessingOptions 
} from '../types/swift';
import { ApiResponse } from '../types/common';

/**
 * SWIFT Message Processor
 */
export class SwiftProcessor {
  constructor(private http: AxiosInstance) {}

  /**
   * Process SWIFT message
   * @param message SWIFT message to process
   * @param options Processing options
   * @returns Processing result
   */
  async processMessage(
    message: SwiftMessage, 
    options: SwiftProcessingOptions = {}
  ): Promise<SwiftProcessingResult> {
    try {
      const response = await this.http.post<ApiResponse<SwiftProcessingResult>>(
        '/swift/process',
        {
          message,
          options: {
            validateOnly: options.validateOnly || false,
            asyncProcessing: options.asyncProcessing || false,
            priorityLevel: options.priorityLevel || 'normal',
            enableBlockchain: options.enableBlockchain !== false
          }
        }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`SWIFT processing failed: ${error}`);
    }
  }

  /**
   * Validate SWIFT message format
   * @param message SWIFT message to validate
   * @returns Validation result
   */
  async validateMessage(message: SwiftMessage): Promise<SwiftValidationResult> {
    try {
      const response = await this.http.post<ApiResponse<SwiftValidationResult>>(
        '/swift/validate',
        { message }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`SWIFT validation failed: ${error}`);
    }
  }

  /**
   * Process MT103 (Customer Credit Transfer)
   * @param mt103 MT103 message data
   * @returns Processing result
   */
  async processMT103(mt103: MT103Message): Promise<SwiftProcessingResult> {
    try {
      const swiftMessage: SwiftMessage = {
        type: 'MT103',
        content: mt103,
        messageId: mt103.transactionReference,
        timestamp: new Date().toISOString()
      };
      
      return await this.processMessage(swiftMessage, {
        enableBlockchain: true,
        priorityLevel: 'high'
      });
    } catch (error) {
      throw new LegacyBaaSError(`MT103 processing failed: ${error}`);
    }
  }

  /**
   * Process MT202 (Financial Institution Transfer)
   * @param mt202 MT202 message data
   * @returns Processing result
   */
  async processMT202(mt202: MT202Message): Promise<SwiftProcessingResult> {
    try {
      const swiftMessage: SwiftMessage = {
        type: 'MT202',
        content: mt202,
        messageId: mt202.transactionReference,
        timestamp: new Date().toISOString()
      };
      
      return await this.processMessage(swiftMessage, {
        enableBlockchain: true,
        priorityLevel: 'high'
      });
    } catch (error) {
      throw new LegacyBaaSError(`MT202 processing failed: ${error}`);
    }
  }

  /**
   * Process MT700 (Letter of Credit)
   * @param mt700 MT700 message data
   * @returns Processing result
   */
  async processMT700(mt700: MT700Message): Promise<SwiftProcessingResult> {
    try {
      const swiftMessage: SwiftMessage = {
        type: 'MT700',
        content: mt700,
        messageId: mt700.creditNumber,
        timestamp: new Date().toISOString()
      };
      
      return await this.processMessage(swiftMessage, {
        enableBlockchain: true,
        priorityLevel: 'normal'
      });
    } catch (error) {
      throw new LegacyBaaSError(`MT700 processing failed: ${error}`);
    }
  }

  /**
   * Get SWIFT message status
   * @param messageId Message ID to check
   * @returns Message status
   */
  async getMessageStatus(messageId: string): Promise<any> {
    try {
      const response = await this.http.get<ApiResponse<any>>(
        `/swift/status/${messageId}`
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get SWIFT message status: ${error}`);
    }
  }

  /**
   * Get SWIFT processing history
   * @param filters Optional filters
   * @returns Processing history
   */
  async getProcessingHistory(filters: {
    startDate?: string;
    endDate?: string;
    messageType?: string;
    status?: string;
    limit?: number;
  } = {}): Promise<SwiftProcessingResult[]> {
    try {
      const response = await this.http.get<ApiResponse<SwiftProcessingResult[]>>(
        '/swift/history',
        { params: filters }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get SWIFT history: ${error}`);
    }
  }

  /**
   * Parse raw SWIFT message
   * @param rawMessage Raw SWIFT message string
   * @returns Parsed message
   */
  async parseRawMessage(rawMessage: string): Promise<SwiftMessage> {
    try {
      const response = await this.http.post<ApiResponse<SwiftMessage>>(
        '/swift/parse',
        { rawMessage }
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to parse SWIFT message: ${error}`);
    }
  }

  /**
   * Get supported SWIFT message types
   * @returns List of supported message types
   */
  async getSupportedMessageTypes(): Promise<string[]> {
    try {
      const response = await this.http.get<ApiResponse<string[]>>(
        '/swift/message-types'
      );
      
      return response.data.data;
    } catch (error) {
      throw new LegacyBaaSError(`Failed to get supported message types: ${error}`);
    }
  }
}