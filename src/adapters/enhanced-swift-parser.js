/**
 * Enhanced SWIFT Message Parser
 * Open-Source Implementation (Apache 2.0 License)
 * 
 * Supports MT103, MT202, MT515, MT700, and ISO 20022 messages
 * Multi-format parser for BaNCS XML, FIS fixed-width, Temenos JSON
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * First-Mover IP Component for TCS Quartz Integration
 */

const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const xml2js = require('xml2js');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'enhanced-swift-parser' }
});

/**
 * Enhanced SWIFT Message Field Definitions
 * Supporting multiple message types for comprehensive use case coverage
 */
const SWIFT_FIELDS = {
  // Cross-border payments
  MT103: {
    '20': 'transaction_reference',
    '23B': 'bank_operation_code',
    '32A': 'value_date_currency_amount',
    '50K': 'ordering_customer',
    '52A': 'ordering_institution',
    '53A': 'senders_correspondent',
    '56A': 'intermediary_institution',
    '57A': 'account_with_institution',
    '59': 'beneficiary_customer',
    '70': 'remittance_information',
    '71A': 'details_of_charges'
  },
  
  // Financial institution transfers
  MT202: {
    '20': 'transaction_reference',
    '21': 'related_reference',
    '32A': 'value_date_currency_amount',
    '52A': 'ordering_institution',
    '53A': 'senders_correspondent',
    '56A': 'intermediary_institution',
    '57A': 'account_with_institution',
    '58A': 'beneficiary_institution',
    '72': 'sender_to_receiver_information'
  },
  
  // Securities transactions (Tokenized Assets)
  MT515: {
    '20C': 'reference',
    '23G': 'function',
    '22F': 'indicator',
    '97A': 'safekeeping_account',
    '35B': 'security_identification',
    '36B': 'quantity_of_financial_instrument',
    '69A': 'trade_date',
    '69B': 'settlement_date',
    '90A': 'dealing_price',
    '19A': 'amount'
  },
  
  // Trade Finance (Letters of Credit)
  MT700: {
    '20': 'documentary_credit_number',
    '31C': 'date_of_issue',
    '31D': 'date_and_place_of_expiry',
    '32B': 'currency_amount',
    '39A': 'percentage_credit_amount_tolerance',
    '41A': 'available_with_by',
    '42C': 'drafts_at',
    '43P': 'partial_shipments',
    '43T': 'transhipment',
    '44A': 'loading_on_board',
    '44B': 'for_transportation_to',
    '44C': 'latest_date_of_shipment',
    '45A': 'description_of_goods',
    '46A': 'documents_required',
    '47A': 'additional_conditions',
    '50': 'applicant',
    '59': 'beneficiary'
  }
};

/**
 * ISO 20022 Message Types for Modern Banking Integration
 */
const ISO20022_MESSAGES = {
  'pacs.008': 'CustomerCreditTransferInitiation',
  'pacs.002': 'PaymentStatusReport', 
  'setr.010': 'SubscriptionOrderInitiation',
  'setr.012': 'SubscriptionOrderConfirmation',
  'tsin.004': 'TradeServiceInitiation',
  'tsin.008': 'TradeServiceStatusNotification'
};

/**
 * Multi-System Format Support
 * BaNCS XML, FIS fixed-width, Temenos JSON parsers
 */
const SYSTEM_PARSERS = {
  BANCS_XML: 'parseBANCSXML',
  FIS_FIXED: 'parseFISFixedWidth', 
  TEMENOS_JSON: 'parseTemenosJSON',
  SWIFT_MT: 'parseSWIFTMessage',
  ISO20022: 'parseISO20022'
};

/**
 * Enhanced SWIFT Parser Class
 * Open-source component for community adoption
 */
class EnhancedSWIFTParser {
  constructor(config = {}) {
    this.config = {
      // Parser configuration
      strictValidation: config.strictValidation !== false,
      enableLogging: config.enableLogging !== false,
      outputFormat: config.outputFormat || 'json', // json, xml, blockchain
      
      // Multi-system support
      supportedSystems: config.supportedSystems || ['SWIFT', 'BANCS', 'FIS', 'TEMENOS'],
      enableBatchProcessing: config.enableBatchProcessing || true,
      
      // Blockchain integration
      blockchainFormat: config.blockchainFormat || 'ethereum', // ethereum, hyperledger, ripple
      tokenizationSupport: config.tokenizationSupport || false,
      
      // Compliance features
      enableComplianceExtraction: config.enableComplianceExtraction || true,
      privacyPreserving: config.privacyPreserving || false,
      
      ...config
    };

    // Parser state
    this.parseHistory = new Map();
    this.errorLog = [];
    this.supportedMessageTypes = ['MT103', 'MT202', 'MT515', 'MT700'];
    
    // Performance metrics
    this.metrics = {
      totalParsed: 0,
      successfulParses: 0,
      failedParses: 0,
      averageParseTime: 0,
      messageTypeStats: new Map()
    };

    logger.info('Enhanced SWIFT Parser initialized', {
      supportedSystems: this.config.supportedSystems,
      supportedMessages: this.supportedMessageTypes
    });
  }

  /**
   * Universal message parser - detects format and routes to appropriate parser
   * @param {string} message - Raw message data
   * @param {string} messageType - Optional message type hint
   * @param {Object} options - Parsing options
   * @returns {Object} Parsed message in standardized format
   */
  async parseMessage(message, messageType = null, options = {}) {
    const startTime = process.hrtime.bigint();
    const parseId = uuidv4();

    try {
      // Auto-detect message format if not specified
      const detectedFormat = messageType || this.detectMessageFormat(message);
      
      logger.debug('Parsing message', {
        parseId,
        detectedFormat,
        messageLength: message.length
      });

      let parsedResult;

      // Route to appropriate parser based on format
      switch (detectedFormat) {
        case 'MT103':
        case 'MT202': 
        case 'MT515':
        case 'MT700':
          parsedResult = await this.parseSWIFTMessage(message, detectedFormat);
          break;
          
        case 'ISO20022':
          parsedResult = await this.parseISO20022(message, options);
          break;
          
        case 'BANCS_XML':
          parsedResult = await this.parseBANCSXML(message);
          break;
          
        case 'FIS_FIXED':
          parsedResult = await this.parseFISFixedWidth(message);
          break;
          
        case 'TEMENOS_JSON':
          parsedResult = await this.parseTemenosJSON(message);
          break;
          
        default:
          throw new Error(`Unsupported message format: ${detectedFormat}`);
      }

      // Add metadata
      parsedResult.parseMetadata = {
        parseId,
        originalFormat: detectedFormat,
        parseTime: Math.max(1, Number((process.hrtime.bigint() - startTime) / BigInt(1000000))), // Convert nanoseconds to milliseconds, min 1ms
        timestamp: new Date().toISOString(),
        parserVersion: '2.0.0'
      };

      // Extract compliance data if enabled
      if (this.config.enableComplianceExtraction) {
        parsedResult.complianceData = this.extractComplianceData(parsedResult);
      }

      // Convert to blockchain format if requested
      if (options.blockchainFormat) {
        parsedResult.blockchainPayload = this.convertToBlockchainFormat(
          parsedResult, 
          options.blockchainFormat
        );
      }

      // Update metrics
      this.updateMetrics('success', parsedResult.parseMetadata.parseTime, detectedFormat);
      
      // Store in history
      this.parseHistory.set(parseId, {
        input: message.substring(0, 100) + '...', // Truncated for privacy
        output: parsedResult,
        timestamp: new Date().toISOString()
      });

      logger.info('Message parsed successfully', {
        parseId,
        format: detectedFormat,
        parseTime: parsedResult.parseMetadata.parseTime
      });

      return parsedResult;

    } catch (error) {
      this.updateMetrics('failure', Number((process.hrtime.bigint() - startTime) / BigInt(1000000)));
      this.errorLog.push({
        parseId,
        error: error.message,
        timestamp: new Date().toISOString(),
        messagePreview: message.substring(0, 50)
      });

      logger.error('Message parsing failed', {
        parseId,
        error: error.message
      });

      throw error;
    }
  }

  /**
   * Parse SWIFT MT messages (MT103, MT202, MT515, MT700)
   * @param {string} message - SWIFT message
   * @param {string} messageType - MT message type
   * @returns {Object} Parsed SWIFT message
   */
  async parseSWIFTMessage(message, messageType) {
    const fields = SWIFT_FIELDS[messageType];
    if (!fields) {
      throw new Error(`Unsupported SWIFT message type: ${messageType}`);
    }

    const result = {
      messageType,
      standard: 'SWIFT',
      fields: {},
      rawMessage: this.config.includeRawMessage ? message : undefined
    };

    // Parse each field using regex patterns
    for (const [fieldCode, fieldName] of Object.entries(fields)) {
      const pattern = this.getSWIFTFieldPattern(fieldCode, messageType);
      const match = message.match(pattern);
      
      if (match) {
        result.fields[fieldName] = this.processSWIFTField(fieldCode, match, messageType);
      }
    }

    // Validate required fields
    this.validateSWIFTMessage(result, messageType);

    // Add use case classification
    result.useCase = this.classifyUseCase(messageType, result.fields);

    return result;
  }

  /**
   * Parse ISO 20022 XML messages for modern banking
   * @param {string} xmlMessage - ISO 20022 XML message
   * @param {Object} options - Parsing options
   * @returns {Object} Parsed ISO 20022 message
   */
  async parseISO20022(xmlMessage, options = {}) {
    try {
      const parser = new xml2js.Parser();
      const xmlData = await parser.parseStringPromise(xmlMessage);

      // Extract message type from XML namespace
      const messageType = this.extractISO20022MessageType(xmlData);
      
      const result = {
        messageType,
        standard: 'ISO20022',
        document: xmlData,
        fields: {},
        rawMessage: this.config.includeRawMessage ? xmlMessage : undefined
      };

      // Parse common payment fields
      if (messageType.includes('pacs.008')) {
        result.fields = this.parseCustomerCreditTransfer(xmlData);
        result.useCase = 'cross_border_payment';
      } else if (messageType.includes('setr.010')) {
        result.fields = this.parseSecuritiesTransaction(xmlData);
        result.useCase = 'tokenized_assets';
      } else if (messageType.includes('tsin.004')) {
        result.fields = this.parseTradeServiceInitiation(xmlData);
        result.useCase = 'trade_finance';
      }

      return result;

    } catch (error) {
      throw new Error(`ISO 20022 parsing failed: ${error.message}`);
    }
  }

  /**
   * Parse TCS BaNCS XML format
   * @param {string} xmlData - BaNCS XML message
   * @returns {Object} Parsed BaNCS message
   */
  async parseBANCSXML(xmlData) {
    try {
      const parser = new xml2js.Parser();
      const bancsData = await parser.parseStringPromise(xmlData);

      return {
        messageType: 'BANCS_XML',
        standard: 'TCS_BANCS',
        fields: {
          transactionId: this.extractXMLValue(bancsData, 'Transaction.TransactionID'),
          amount: this.extractXMLValue(bancsData, 'Transaction.Amount'),
          currency: this.extractXMLValue(bancsData, 'Transaction.Currency'),
          senderAccount: this.extractXMLValue(bancsData, 'Transaction.SenderAccount'),
          receiverAccount: this.extractXMLValue(bancsData, 'Transaction.ReceiverAccount'),
          senderName: this.extractXMLValue(bancsData, 'Transaction.SenderName'),
          receiverName: this.extractXMLValue(bancsData, 'Transaction.ReceiverName'),
          purpose: this.extractXMLValue(bancsData, 'Transaction.Purpose'),
          valueDate: this.extractXMLValue(bancsData, 'Transaction.ValueDate')
        },
        bankingSystem: 'TCS_BANCS',
        useCase: 'cross_border_payment'
      };

    } catch (error) {
      throw new Error(`BaNCS XML parsing failed: ${error.message}`);
    }
  }

  /**
   * Parse FIS fixed-width format (Systematics)
   * @param {string} fixedWidthData - FIS fixed-width record
   * @returns {Object} Parsed FIS message
   */
  async parseFISFixedWidth(fixedWidthData) {
    // FIS Systematics fixed-width format specification
    const layout = {
      transactionId: { start: 0, length: 16 },
      valueDate: { start: 16, length: 6 },
      currency: { start: 22, length: 3 },
      amount: { start: 25, length: 11 },
      senderAccount: { start: 37, length: 23 },
      receiverAccount: { start: 60, length: 20 },
      senderName: { start: 80, length: 35 },
      receiverName: { start: 115, length: 35 },
      purpose: { start: 150, length: 35 }
    };

    const result = {
      messageType: 'FIS_FIXED',
      standard: 'FIS_SYSTEMATICS',
      fields: {},
      bankingSystem: 'FIS',
      useCase: 'cross_border_payment'
    };

    // Extract fields based on fixed positions
    for (const [fieldName, layout_info] of Object.entries(layout)) {
      const value = fixedWidthData.substring(
        layout_info.start, 
        layout_info.start + layout_info.length
      ).trim();
      
      if (value) {
        result.fields[fieldName] = value;
      }
    }

    return result;
  }

  /**
   * Parse Temenos JSON format
   * @param {string} jsonData - Temenos JSON message
   * @returns {Object} Parsed Temenos message
   */
  async parseTemenosJSON(jsonData) {
    try {
      const temenosData = JSON.parse(jsonData);

      return {
        messageType: 'TEMENOS_JSON',
        standard: 'TEMENOS_TRANSACT',
        fields: {
          transactionId: temenosData.transactionId || temenosData.id,
          amount: temenosData.amount || temenosData.transactionAmount,
          currency: temenosData.currency || temenosData.currencyCode,
          senderAccount: temenosData.debitAccount || temenosData.fromAccount,
          receiverAccount: temenosData.creditAccount || temenosData.toAccount,
          senderName: temenosData.debitCustomer || temenosData.fromCustomer,
          receiverName: temenosData.creditCustomer || temenosData.toCustomer,
          purpose: temenosData.narrative || temenosData.description,
          valueDate: temenosData.valueDate || temenosData.processingDate
        },
        bankingSystem: 'TEMENOS',
        useCase: this.classifyTemenosUseCase(temenosData)
      };

    } catch (error) {
      throw new Error(`Temenos JSON parsing failed: ${error.message}`);
    }
  }

  /**
   * Auto-detect message format based on content analysis
   * @param {string} message - Raw message data
   * @returns {string} Detected format
   */
  detectMessageFormat(message) {
    // SWIFT MT message detection
    if (message.includes(':20:') && message.includes(':32A:')) {
      if (message.includes(':59:')) return 'MT103';
      if (message.includes(':58A:')) return 'MT202';
      if (message.includes(':35B:')) return 'MT515';
      if (message.includes(':45A:')) return 'MT700';
    }

    // ISO 20022 detection
    if (message.includes('<Document') && message.includes('xmlns')) {
      return 'ISO20022';
    }

    // BaNCS XML detection
    if (message.includes('<Transaction') || message.includes('<BaNCS')) {
      return 'BANCS_XML';
    }

    // Temenos JSON detection  
    if (message.trim().startsWith('{') && 
        (message.includes('transactionId') || message.includes('debitAccount'))) {
      return 'TEMENOS_JSON';
    }

    // FIS fixed-width detection (by length and pattern - fixed-width banks records)
    if (message.length >= 80 && !message.includes('<') && !message.includes('{') && 
        !message.includes(':') && !message.includes('\n') && 
        /^\d+\s+\d{6}[A-Z]{3}\d+\.\d{2}/.test(message)) {
      return 'FIS_FIXED';
    }

    throw new Error('Unable to detect message format');
  }

  /**
   * Extract compliance-relevant data for KYC/AML processing
   * @param {Object} parsedMessage - Parsed message object
   * @returns {Object} Compliance data extract
   */
  extractComplianceData(parsedMessage) {
    const compliance = {
      customerData: {},
      transactionData: {},
      riskIndicators: [],
      requiredChecks: []
    };

    const fields = parsedMessage.fields;

    // Extract customer information
    if (fields.senderName || fields.ordering_customer || fields.applicant) {
      compliance.customerData.sender = {
        name: fields.senderName || fields.ordering_customer || fields.applicant,
        account: fields.senderAccount
      };
      compliance.requiredChecks.push('sender_kyc');
    }

    if (fields.receiverName || fields.beneficiary_customer || fields.beneficiary) {
      compliance.customerData.receiver = {
        name: fields.receiverName || fields.beneficiary_customer || fields.beneficiary,
        account: fields.receiverAccount
      };
      compliance.requiredChecks.push('receiver_kyc');
    }

    // For securities transactions (MT515), check safekeeping account
    if (fields.safekeeping_account) {
      compliance.customerData.sender = {
        account: fields.safekeeping_account,
        name: 'Account Holder' // Generic name for safekeeping account
      };
      compliance.requiredChecks.push('sender_kyc');
      compliance.requiredChecks.push('securities_compliance');
    }

    // Extract transaction information from parsed message
    compliance.transactionData = {
      amount: parsedMessage.amount || fields.amount || (fields.value_date_currency_amount && fields.value_date_currency_amount.amount),
      currency: parsedMessage.currency || fields.currency || (fields.value_date_currency_amount && fields.value_date_currency_amount.currency),
      purpose: parsedMessage.remittanceInfo || fields.purpose || fields.remittance_information,
      valueDate: parsedMessage.valueDate || fields.valueDate || (fields.value_date_currency_amount && fields.value_date_currency_amount.valueDate),
      // For securities transactions
      securityId: fields.security_identification,
      quantity: fields.quantity_of_financial_instrument
    };

    // Risk indicators based on amount and purpose
    const amount = parseFloat(parsedMessage.amount || fields.amount || (fields.value_date_currency_amount && fields.value_date_currency_amount.amount) || 0);
    if (amount > 10000) {
      compliance.riskIndicators.push('high_value_transaction');
      compliance.requiredChecks.push('enhanced_due_diligence');
    }

    if (fields.purpose && 
        (fields.purpose.toLowerCase().includes('cash') || 
         fields.purpose.toLowerCase().includes('currency'))) {
      compliance.riskIndicators.push('cash_related');
      compliance.requiredChecks.push('aml_screening');
    }

    // Add trade finance specific compliance for MT700
    if (parsedMessage.messageType === 'MT700') {
      compliance.requiredChecks.push('aml_screening');
      compliance.requiredChecks.push('trade_finance_compliance');
    }

    // Add sanctions screening requirement
    compliance.requiredChecks.push('sanctions_screening');

    return compliance;
  }

  /**
   * Convert parsed message to blockchain-compatible format
   * @param {Object} parsedMessage - Parsed message
   * @param {string} blockchainType - Target blockchain format
   * @returns {Object} Blockchain payload
   */
  convertToBlockchainFormat(parsedMessage, blockchainType) {
    const fields = parsedMessage.fields;
    
    switch (blockchainType) {
      case 'ethereum':
        return {
          to: parsedMessage.receiver?.account || fields.receiverAccount,
          value: parsedMessage.amount || fields.amount,
          data: this.encodeEthereumData(parsedMessage),
          gasLimit: 21000,
          metadata: {
            currency: parsedMessage.currency || fields.currency,
            purpose: parsedMessage.remittanceInfo || fields.purpose,
            sender: parsedMessage.sender?.name || fields.senderName
          }
        };

      case 'ripple':
        return {
          TransactionType: 'Payment',
          Account: parsedMessage.sender?.account || fields.senderAccount,
          Destination: parsedMessage.receiver?.account || fields.receiverAccount,
          Amount: {
            currency: parsedMessage.currency || fields.currency,
            value: parsedMessage.amount || fields.amount,
            issuer: parsedMessage.orderingInstitution || fields.ordering_institution
          },
          Memos: [{
            Memo: {
              MemoType: Buffer.from('purpose', 'utf8').toString('hex'),
              MemoData: Buffer.from(parsedMessage.remittanceInfo || fields.purpose || '', 'utf8').toString('hex')
            }
          }]
        };

      case 'hyperledger':
        return {
          fcn: 'transferFunds',
          args: [
            parsedMessage.sender?.account || fields.senderAccount,
            parsedMessage.receiver?.account || fields.receiverAccount,
            parsedMessage.amount || fields.amount,
            parsedMessage.currency || fields.currency,
            parsedMessage.remittanceInfo || fields.purpose || '',
            parsedMessage.parseMetadata?.parseId || 'unknown'
          ],
          chaincode: 'banking-chaincode'
        };

      default:
        throw new Error(`Unsupported blockchain format: ${blockchainType}`);
    }
  }

  /**
   * Get parser performance metrics
   * @returns {Object} Performance metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate: this.metrics.totalParsed > 0 
        ? this.metrics.successfulParses / this.metrics.totalParsed 
        : 0,
      messageTypes: Array.from(this.metrics.messageTypeStats.entries()),
      errorCount: this.errorLog.length,
      recentErrors: this.errorLog.slice(-5)
    };
  }

  /**
   * Batch processing for high-volume scenarios
   * @param {Array} messages - Array of messages to parse
   * @param {Object} options - Batch processing options
   * @returns {Array} Array of parsed messages
   */
  async batchParse(messages, options = {}) {
    const batchSize = options.batchSize || 100;
    const concurrency = options.concurrency || 10;
    const results = [];

    for (let i = 0; i < messages.length; i += batchSize) {
      const batch = messages.slice(i, i + batchSize);
      const batchPromises = batch.map(async (message, index) => {
        try {
          return await this.parseMessage(message.data, message.type, options);
        } catch (error) {
          return {
            error: error.message,
            index: i + index,
            message: message.data.substring(0, 50)
          };
        }
      });

      const batchResults = await Promise.all(batchPromises);
      results.push(...batchResults);

      logger.info('Batch processed', {
        batchNumber: Math.floor(i / batchSize) + 1,
        processedCount: results.length,
        totalMessages: messages.length
      });
    }

    return results;
  }

  // ===== UTILITY METHODS =====

  getSWIFTFieldPattern(fieldCode, messageType) {
    // Return regex patterns for SWIFT fields
    const patterns = {
      '20': /:20:([A-Z0-9]+)/,
      '32A': /:32A:(\d{6})([A-Z]{3})(\d+,\d{2})/,
      '50K': /:50K:\/(.+)\n\s*(.+)/,
      '59': /:59:\/(.+)\n\s*(.+)/,
      // Add more patterns as needed
    };
    return patterns[fieldCode] || new RegExp(`:${fieldCode}:(.+)`);
  }

  processSWIFTField(fieldCode, match, messageType) {
    // Process and format SWIFT field values
    if (fieldCode === '32A') {
      return {
        valueDate: match[1],
        currency: match[2], 
        amount: match[3]
      };
    }
    return match[1] || match[0];
  }

  validateSWIFTMessage(parsedMessage, messageType) {
    const requiredFields = {
      'MT103': ['transaction_reference', 'value_date_currency_amount'],
      'MT202': ['transaction_reference', 'value_date_currency_amount'],
      'MT515': ['reference', 'security_identification'],
      'MT700': ['documentary_credit_number', 'currency_amount']
    };

    const required = requiredFields[messageType] || [];
    for (const field of required) {
      if (!parsedMessage.fields[field]) {
        throw new Error(`Missing required field: ${field}`);
      }
    }
  }

  classifyUseCase(messageType, fields) {
    switch (messageType) {
      case 'MT103': return 'cross_border_payment';
      case 'MT202': return 'financial_institution_transfer';
      case 'MT515': return 'tokenized_assets';
      case 'MT700': return 'trade_finance';
      default: return 'unknown';
    }
  }

  updateMetrics(type, duration, messageType = 'unknown') {
    this.metrics.totalParsed++;
    
    if (type === 'success') {
      this.metrics.successfulParses++;
    } else {
      this.metrics.failedParses++;
    }

    // Update average parse time
    this.metrics.averageParseTime = 
      (this.metrics.averageParseTime * (this.metrics.totalParsed - 1) + duration) / 
      this.metrics.totalParsed;

    // Update message type stats
    if (!this.metrics.messageTypeStats.has(messageType)) {
      this.metrics.messageTypeStats.set(messageType, 0);
    }
    this.metrics.messageTypeStats.set(
      messageType, 
      this.metrics.messageTypeStats.get(messageType) + 1
    );
  }

  extractXMLValue(xmlData, path) {
    // Extract value from parsed XML data
    const pathArray = path.split('.');
    let current = xmlData;
    
    for (const key of pathArray) {
      if (current && current[key]) {
        current = current[key];
      } else {
        return null;
      }
    }
    
    return current && current[0] ? current[0] : current;
  }

  extractISO20022MessageType(xmlData) {
    // Extract message type from ISO 20022 XML namespace
    const doc = xmlData.Document || xmlData;
    if (doc && doc.$) {
      const xmlns = doc.$.xmlns;
      if (xmlns && xmlns.includes('pacs.008')) return 'pacs.008';
      if (xmlns && xmlns.includes('setr.010')) return 'setr.010';
      if (xmlns && xmlns.includes('tsin.004')) return 'tsin.004';
    }
    return 'unknown';
  }

  parseCustomerCreditTransfer(xmlData) {
    // Parse ISO 20022 Customer Credit Transfer
    return {
      messageId: this.extractXMLValue(xmlData, 'Document.FIToFICstmrCdtTrf.GrpHdr.MsgId'),
      amount: this.extractXMLValue(xmlData, 'Document.FIToFICstmrCdtTrf.CdtTrfTxInf.Amt.InstdAmt'),
      currency: this.extractXMLValue(xmlData, 'Document.FIToFICstmrCdtTrf.CdtTrfTxInf.Amt.InstdAmt.Ccy'),
      debtorName: this.extractXMLValue(xmlData, 'Document.FIToFICstmrCdtTrf.CdtTrfTxInf.Dbtr.Nm'),
      creditorName: this.extractXMLValue(xmlData, 'Document.FIToFICstmrCdtTrf.CdtTrfTxInf.Cdtr.Nm')
    };
  }

  parseSecuritiesTransaction(xmlData) {
    // Parse ISO 20022 Securities Transaction
    return {
      messageId: this.extractXMLValue(xmlData, 'Document.SbcptOrdr.MsgId'),
      securityId: this.extractXMLValue(xmlData, 'Document.SbcptOrdr.OrdrDtls.FinInstrmDtls.Id'),
      quantity: this.extractXMLValue(xmlData, 'Document.SbcptOrdr.OrdrDtls.UnitsNb'),
      amount: this.extractXMLValue(xmlData, 'Document.SbcptOrdr.OrdrDtls.NetAmt')
    };
  }

  parseTradeServiceInitiation(xmlData) {
    // Parse ISO 20022 Trade Service Initiation
    return {
      messageId: this.extractXMLValue(xmlData, 'Document.TradSvcInitn.MsgId'),
      tradeId: this.extractXMLValue(xmlData, 'Document.TradSvcInitn.TxId'),
      amount: this.extractXMLValue(xmlData, 'Document.TradSvcInitn.PurchsOrdr.TtlAmt'),
      buyer: this.extractXMLValue(xmlData, 'Document.TradSvcInitn.Buyr.Nm'),
      seller: this.extractXMLValue(xmlData, 'Document.TradSvcInitn.Sellr.Nm')
    };
  }

  classifyTemenosUseCase(temenosData) {
    if (temenosData.transactionType === 'SECURITIES') return 'tokenized_assets';
    if (temenosData.transactionType === 'TRADE') return 'trade_finance';
    if (temenosData.transactionType === 'CBDC') return 'cbdc';
    return 'cross_border_payment';
  }

  encodeEthereumData(parsedMessage) {
    // Encode additional data for Ethereum transaction
    const data = {
      purpose: parsedMessage.fields.purpose || '',
      reference: parsedMessage.fields.transactionId || '',
      sender: parsedMessage.fields.senderName || ''
    };
    return Buffer.from(JSON.stringify(data), 'utf8').toString('hex');
  }
}

module.exports = {
  EnhancedSWIFTParser,
  SWIFT_FIELDS,
  ISO20022_MESSAGES,
  SYSTEM_PARSERS
};