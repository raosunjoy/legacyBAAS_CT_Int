/**
 * Enhanced SWIFT Parser Tests
 * Comprehensive test suite for multi-format parser
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage for Open-Source IP Component
 */

const { EnhancedSWIFTParser, SWIFT_FIELDS, SYSTEM_PARSERS } = require('../../src/adapters/enhanced-swift-parser');

describe('Enhanced SWIFT Parser', () => {
  let parser;

  beforeEach(() => {
    parser = new EnhancedSWIFTParser({
      strictValidation: true,
      enableLogging: false,
      enableComplianceExtraction: true
    });
  });

  afterEach(async () => {
    // Cleanup if needed
  });

  describe('Initialization', () => {
    test('should initialize with default configuration', () => {
      const defaultParser = new EnhancedSWIFTParser();
      
      expect(defaultParser.config.strictValidation).toBe(true);
      expect(defaultParser.config.outputFormat).toBe('json');
      expect(defaultParser.supportedMessageTypes).toContain('MT103');
      expect(defaultParser.supportedMessageTypes).toContain('MT515');
      expect(defaultParser.supportedMessageTypes).toContain('MT700');
    });

    test('should initialize with custom configuration', () => {
      const customParser = new EnhancedSWIFTParser({
        strictValidation: false,
        outputFormat: 'xml',
        blockchainFormat: 'ripple',
        tokenizationSupport: true
      });

      expect(customParser.config.strictValidation).toBe(false);
      expect(customParser.config.outputFormat).toBe('xml');
      expect(customParser.config.blockchainFormat).toBe('ripple');
      expect(customParser.config.tokenizationSupport).toBe(true);
    });

    test('should initialize performance metrics', () => {
      expect(parser.metrics.totalParsed).toBe(0);
      expect(parser.metrics.successfulParses).toBe(0);
      expect(parser.metrics.averageParseTime).toBe(0);
      expect(parser.metrics.messageTypeStats).toBeInstanceOf(Map);
    });
  });

  describe('Message Format Detection', () => {
    test('should detect MT103 format', () => {
      const mt103Message = `:20:12345678\n:32A:230101USD1000,00\n:59:/98765432\nBENEFICIARY NAME`;
      expect(parser.detectMessageFormat(mt103Message)).toBe('MT103');
    });

    test('should detect MT202 format', () => {
      const mt202Message = `:20:12345678\n:32A:230101USD1000,00\n:58A:/98765432\nBENEFICIARY BANK`;
      expect(parser.detectMessageFormat(mt202Message)).toBe('MT202');
    });

    test('should detect MT515 format', () => {
      const mt515Message = `:20:12345678\n:32A:230101USD1000,00\n:35B:ISIN US1234567890\nSECURITY NAME`;
      expect(parser.detectMessageFormat(mt515Message)).toBe('MT515');
    });

    test('should detect MT700 format', () => {
      const mt700Message = `:20:12345678\n:32A:230101USD1000,00\n:45A:GOODS DESCRIPTION\nCOMMODITY DETAILS`;
      expect(parser.detectMessageFormat(mt700Message)).toBe('MT700');
    });

    test('should detect ISO 20022 format', () => {
      const iso20022Message = `<?xml version="1.0"?>
        <Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.02">
          <FIToFICstmrCdtTrf>
            <GrpHdr><MsgId>123456</MsgId></GrpHdr>
          </FIToFICstmrCdtTrf>
        </Document>`;
      expect(parser.detectMessageFormat(iso20022Message)).toBe('ISO20022');
    });

    test('should detect BaNCS XML format', () => {
      const bancsMessage = `<Transaction><TransactionID>123456</TransactionID><Amount>1000</Amount></Transaction>`;
      expect(parser.detectMessageFormat(bancsMessage)).toBe('BANCS_XML');
    });

    test('should detect Temenos JSON format', () => {
      const temenosMessage = `{"transactionId":"123456","amount":"1000","debitAccount":"12345"}`;
      expect(parser.detectMessageFormat(temenosMessage)).toBe('TEMENOS_JSON');
    });

    test('should detect FIS fixed-width format', () => {
      const fisMessage = '123456789       230101USD1000.00    SENDER BANK                 BENEFICIARY NAME           ';
      expect(parser.detectMessageFormat(fisMessage)).toBe('FIS_FIXED');
    });

    test('should throw error for unknown format', () => {
      const unknownMessage = 'This is not a valid financial message format';
      expect(() => parser.detectMessageFormat(unknownMessage)).toThrow('Unable to detect message format');
    });
  });

  describe('SWIFT MT103 Parsing', () => {
    const sampleMT103 = `:20:TXN123456789
:23B:CRED
:32A:230601USD1000,00
:50K:/12345678901
ORDERING CUSTOMER NAME
CUSTOMER ADDRESS LINE 1
:52A:DEUTDEFF
:59:/98765432109
BENEFICIARY CUSTOMER NAME
BENEFICIARY ADDRESS LINE 1
:70:PAYMENT FOR SERVICES
:71A:OUR`;

    test('should parse MT103 message successfully', async () => {
      const result = await parser.parseMessage(sampleMT103, 'MT103');

      expect(result.messageType).toBe('MT103');
      expect(result.standard).toBe('SWIFT');
      expect(result.fields.transaction_reference).toBeTruthy();
      expect(result.useCase).toBe('cross_border_payment');
      expect(result.parseMetadata).toBeDefined();
      expect(result.parseMetadata.parseTime).toBeGreaterThan(0);
    });

    test('should extract compliance data from MT103', async () => {
      const result = await parser.parseMessage(sampleMT103, 'MT103');

      expect(result.complianceData).toBeDefined();
      expect(result.complianceData.customerData.sender).toBeDefined();
      expect(result.complianceData.customerData.receiver).toBeDefined();
      expect(result.complianceData.transactionData.amount).toBeDefined();
      expect(result.complianceData.requiredChecks).toContain('sender_kyc');
      expect(result.complianceData.requiredChecks).toContain('receiver_kyc');
    });

    test('should convert MT103 to blockchain format', async () => {
      const result = await parser.parseMessage(sampleMT103, 'MT103', {
        blockchainFormat: 'ethereum'
      });

      expect(result.blockchainPayload).toBeDefined();
      expect(result.blockchainPayload.value).toBeDefined();
      expect(result.blockchainPayload.gasLimit).toBe(21000);
      expect(result.blockchainPayload.metadata).toBeDefined();
    });

    test('should handle validation errors', async () => {
      const invalidMT103 = ':20:TXN123\n:23B:CRED\n'; // Missing required fields
      
      await expect(parser.parseMessage(invalidMT103, 'MT103')).rejects.toThrow();
    });
  });

  describe('SWIFT MT515 Parsing (Tokenized Assets)', () => {
    const sampleMT515 = `:20C::SEME//TXN123456789
:23G:NEWM
:22F::TRAD//TRAD
:97A::SAFE//12345678901
:35B:ISIN US1234567890
MICROSOFT CORP
:36B::SETT//UNIT/1000,
:69A::TRAD//20230601
:69B::SETT//20230603
:90A::DEAL//PRCT/102,50
:19A::SETT//USD102500,`;

    test('should parse MT515 message successfully', async () => {
      const result = await parser.parseMessage(sampleMT515, 'MT515');

      expect(result.messageType).toBe('MT515');
      expect(result.standard).toBe('SWIFT');
      expect(result.useCase).toBe('tokenized_assets');
      expect(result.fields.security_identification).toBeTruthy();
    });

    test('should extract tokenization data from MT515', async () => {
      const result = await parser.parseMessage(sampleMT515, 'MT515');

      expect(result.fields.security_identification).toContain('ISIN');
      expect(result.fields.quantity_of_financial_instrument).toBeTruthy();
      expect(result.complianceData.requiredChecks).toContain('sender_kyc');
    });
  });

  describe('SWIFT MT700 Parsing (Trade Finance)', () => {
    const sampleMT700 = `:20:LC123456789
:31C:230601
:31D:231201NEW YORK
:32B:USD50000,
:39A:10/10
:41A:BY PAYMENT
:42C:AT SIGHT
:43P:PARTIAL SHIPMENTS PROHIBITED
:43T:TRANSHIPMENT PROHIBITED
:44A:SINGAPORE
:44B:NEW YORK
:44C:20231130
:45A:ELECTRONIC COMPONENTS AS PER PROFORMA INVOICE NO. PI123456
:46A:1. COMMERCIAL INVOICE IN TRIPLICATE
2. PACKING LIST IN DUPLICATE
:47A:ALL DOCUMENTS TO BE PRESENTED WITHIN 15 DAYS
:50:IMPORTING COMPANY LIMITED
123 MAIN STREET
SINGAPORE 123456
:59:EXPORTING CORPORATION
456 BROADWAY
NEW YORK NY 10001 USA`;

    test('should parse MT700 message successfully', async () => {
      const result = await parser.parseMessage(sampleMT700, 'MT700');

      expect(result.messageType).toBe('MT700');
      expect(result.standard).toBe('SWIFT');
      expect(result.useCase).toBe('trade_finance');
      expect(result.fields.documentary_credit_number).toBeTruthy();
      expect(result.fields.currency_amount).toBeTruthy();
    });

    test('should extract trade finance compliance data', async () => {
      const result = await parser.parseMessage(sampleMT700, 'MT700');

      expect(result.complianceData.customerData.sender).toBeDefined();
      expect(result.complianceData.customerData.receiver).toBeDefined();
      expect(result.complianceData.requiredChecks).toContain('aml_screening');
    });
  });

  describe('Multi-System Format Parsing', () => {
    test('should parse BaNCS XML format', async () => {
      const bancsXML = `<?xml version="1.0"?>
        <Transaction>
          <TransactionID>TXN123456</TransactionID>
          <Amount>1000.00</Amount>
          <Currency>USD</Currency>
          <SenderAccount>12345678901</SenderAccount>
          <ReceiverAccount>98765432109</ReceiverAccount>
          <SenderName>SENDER BANK</SenderName>
          <ReceiverName>BENEFICIARY NAME</ReceiverName>
          <Purpose>PAYMENT FOR SERVICES</Purpose>
          <ValueDate>2023-06-01</ValueDate>
        </Transaction>`;

      const result = await parser.parseMessage(bancsXML, 'BANCS_XML');

      expect(result.messageType).toBe('BANCS_XML');
      expect(result.standard).toBe('TCS_BANCS');
      expect(result.bankingSystem).toBe('TCS_BANCS');
      expect(result.fields.transactionId).toBe('TXN123456');
      expect(result.fields.amount).toBe('1000.00');
      expect(result.fields.currency).toBe('USD');
    });

    test('should parse FIS fixed-width format', async () => {
      const fisFixed = 'TXN123456       230601USD0001000.00 12345678901234567890SENDER BANK                 BENEFICIARY NAME            PAYMENT FOR SERVICES           ';

      const result = await parser.parseMessage(fisFixed, 'FIS_FIXED');

      expect(result.messageType).toBe('FIS_FIXED');
      expect(result.standard).toBe('FIS_SYSTEMATICS');
      expect(result.bankingSystem).toBe('FIS');
      expect(result.fields.transactionId).toBe('TXN123456');
      expect(result.fields.currency).toBe('USD');
      expect(result.fields.amount).toBe('0001000.00');
    });

    test('should parse Temenos JSON format', async () => {
      const temenosJSON = `{
        "transactionId": "TXN123456",
        "amount": "1000.00",
        "currency": "USD",
        "debitAccount": "12345678901",
        "creditAccount": "98765432109",
        "debitCustomer": "SENDER BANK",
        "creditCustomer": "BENEFICIARY NAME",
        "narrative": "PAYMENT FOR SERVICES",
        "valueDate": "2023-06-01"
      }`;

      const result = await parser.parseMessage(temenosJSON, 'TEMENOS_JSON');

      expect(result.messageType).toBe('TEMENOS_JSON');
      expect(result.standard).toBe('TEMENOS_TRANSACT');
      expect(result.bankingSystem).toBe('TEMENOS');
      expect(result.fields.transactionId).toBe('TXN123456');
      expect(result.fields.amount).toBe('1000.00');
      expect(result.fields.currency).toBe('USD');
    });
  });

  describe('ISO 20022 Parsing', () => {
    test('should parse Customer Credit Transfer (pacs.008)', async () => {
      const iso20022XML = `<?xml version="1.0"?>
        <Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.02">
          <FIToFICstmrCdtTrf>
            <GrpHdr>
              <MsgId>MSG123456</MsgId>
            </GrpHdr>
            <CdtTrfTxInf>
              <Amt>
                <InstdAmt Ccy="USD">1000.00</InstdAmt>
              </Amt>
              <Dbtr>
                <Nm>SENDER BANK</Nm>
              </Dbtr>
              <Cdtr>
                <Nm>BENEFICIARY NAME</Nm>
              </Cdtr>
            </CdtTrfTxInf>
          </FIToFICstmrCdtTrf>
        </Document>`;

      const result = await parser.parseMessage(iso20022XML, 'ISO20022');

      expect(result.messageType).toContain('pacs.008');
      expect(result.standard).toBe('ISO20022');
      expect(result.useCase).toBe('cross_border_payment');
    });
  });

  describe('Blockchain Format Conversion', () => {
    const sampleTransaction = {
      messageType: 'MT103',
      fields: {
        transactionId: 'TXN123456',
        amount: '1000.00',
        currency: 'USD',
        senderAccount: '12345678901',
        receiverAccount: '98765432109',
        senderName: 'SENDER BANK',
        purpose: 'PAYMENT FOR SERVICES'
      }
    };

    test('should convert to Ethereum format', () => {
      const ethPayload = parser.convertToBlockchainFormat(sampleTransaction, 'ethereum');

      expect(ethPayload.to).toBe('98765432109');
      expect(ethPayload.value).toBe('1000.00');
      expect(ethPayload.gasLimit).toBe(21000);
      expect(ethPayload.metadata.currency).toBe('USD');
    });

    test('should convert to Ripple format', () => {
      const xrpPayload = parser.convertToBlockchainFormat(sampleTransaction, 'ripple');

      expect(xrpPayload.TransactionType).toBe('Payment');
      expect(xrpPayload.Account).toBe('12345678901');
      expect(xrpPayload.Destination).toBe('98765432109');
      expect(xrpPayload.Amount.value).toBe('1000.00');
      expect(xrpPayload.Amount.currency).toBe('USD');
    });

    test('should convert to Hyperledger format', () => {
      const hlPayload = parser.convertToBlockchainFormat(sampleTransaction, 'hyperledger');

      expect(hlPayload.fcn).toBe('transferFunds');
      expect(hlPayload.args).toHaveLength(6);
      expect(hlPayload.args[0]).toBe('12345678901');
      expect(hlPayload.args[1]).toBe('98765432109');
      expect(hlPayload.args[2]).toBe('1000.00');
      expect(hlPayload.chaincode).toBe('banking-chaincode');
    });

    test('should throw error for unsupported blockchain format', () => {
      expect(() => {
        parser.convertToBlockchainFormat(sampleTransaction, 'unsupported');
      }).toThrow('Unsupported blockchain format: unsupported');
    });
  });

  describe('Batch Processing', () => {
    test('should process multiple messages in batch', async () => {
      const messages = [
        { data: ':20:TXN001\n:32A:230101USD1000,00\n:59:/123\nBENE1', type: 'MT103' },
        { data: ':20:TXN002\n:32A:230101EUR500,00\n:59:/456\nBENE2', type: 'MT103' },
        { data: ':20:TXN003\n:32A:230101GBP750,00\n:59:/789\nBENE3', type: 'MT103' }
      ];

      const results = await parser.batchParse(messages, { batchSize: 2 });

      expect(results).toHaveLength(3);
      expect(results[0].messageType).toBe('MT103');
      expect(results[1].messageType).toBe('MT103');
      expect(results[2].messageType).toBe('MT103');
    });

    test('should handle batch processing errors gracefully', async () => {
      const messages = [
        { data: ':20:TXN001\n:32A:230101USD1000,00\n:59:/123\nBENE1', type: 'MT103' },
        { data: 'invalid message format', type: 'MT103' },
        { data: ':20:TXN003\n:32A:230101GBP750,00\n:59:/789\nBENE3', type: 'MT103' }
      ];

      const results = await parser.batchParse(messages);

      expect(results).toHaveLength(3);
      expect(results[0].messageType).toBe('MT103');
      expect(results[1].error).toBeDefined();
      expect(results[2].messageType).toBe('MT103');
    });
  });

  describe('Performance Metrics', () => {
    test('should track parsing metrics', async () => {
      const sampleMessage = ':20:TXN123\n:32A:230101USD1000,00\n:59:/123\nBENE';
      
      await parser.parseMessage(sampleMessage, 'MT103');
      await parser.parseMessage(sampleMessage, 'MT103');

      const metrics = parser.getMetrics();

      expect(metrics.totalParsed).toBe(2);
      expect(metrics.successfulParses).toBe(2);
      expect(metrics.failedParses).toBe(0);
      expect(metrics.successRate).toBe(1);
      expect(metrics.averageParseTime).toBeGreaterThan(0);
      expect(metrics.messageTypes).toContainEqual(['MT103', 2]);
    });

    test('should track parsing failures', async () => {
      try {
        await parser.parseMessage('invalid', 'MT103');
      } catch (error) {
        // Expected to fail
      }

      const metrics = parser.getMetrics();
      expect(metrics.failedParses).toBe(1);
      expect(metrics.errorCount).toBe(1);
      expect(metrics.recentErrors).toHaveLength(1);
    });
  });

  describe('Compliance Data Extraction', () => {
    test('should extract high-value transaction indicators', async () => {
      const highValueMT103 = ':20:TXN123\n:32A:230101USD15000,00\n:50K:/123\nSENDER\n:59:/456\nBENE\n:70:CASH TRANSFER';
      
      const result = await parser.parseMessage(highValueMT103, 'MT103');

      expect(result.complianceData.riskIndicators).toContain('high_value_transaction');
      expect(result.complianceData.riskIndicators).toContain('cash_related');
      expect(result.complianceData.requiredChecks).toContain('enhanced_due_diligence');
      expect(result.complianceData.requiredChecks).toContain('aml_screening');
    });

    test('should extract standard compliance requirements', async () => {
      const standardMT103 = ':20:TXN123\n:32A:230101USD500,00\n:50K:/123\nSENDER\n:59:/456\nBENE\n:70:GOODS PAYMENT';
      
      const result = await parser.parseMessage(standardMT103, 'MT103');

      expect(result.complianceData.requiredChecks).toContain('sender_kyc');
      expect(result.complianceData.requiredChecks).toContain('receiver_kyc');
      expect(result.complianceData.requiredChecks).toContain('sanctions_screening');
      expect(result.complianceData.riskIndicators).not.toContain('high_value_transaction');
    });
  });

  describe('Error Handling', () => {
    test('should handle invalid message format gracefully', async () => {
      const invalidMessage = 'This is not a valid financial message';
      
      await expect(parser.parseMessage(invalidMessage)).rejects.toThrow('Unable to detect message format');
    });

    test('should handle missing required fields', async () => {
      const incompleteMT103 = ':20:TXN123\n:23B:CRED'; // Missing required fields
      
      await expect(parser.parseMessage(incompleteMT103, 'MT103')).rejects.toThrow();
    });

    test('should handle malformed XML gracefully', async () => {
      const malformedXML = '<Transaction><TransactionID>123<Amount>1000</Transaction>';
      
      await expect(parser.parseMessage(malformedXML, 'BANCS_XML')).rejects.toThrow();
    });

    test('should handle malformed JSON gracefully', async () => {
      const malformedJSON = '{"transactionId":"123","amount":}';
      
      await expect(parser.parseMessage(malformedJSON, 'TEMENOS_JSON')).rejects.toThrow();
    });
  });

  describe('Use Case Classification', () => {
    test('should classify cross-border payments correctly', () => {
      const useCase = parser.classifyUseCase('MT103', {});
      expect(useCase).toBe('cross_border_payment');
    });

    test('should classify tokenized assets correctly', () => {
      const useCase = parser.classifyUseCase('MT515', {});
      expect(useCase).toBe('tokenized_assets');
    });

    test('should classify trade finance correctly', () => {
      const useCase = parser.classifyUseCase('MT700', {});
      expect(useCase).toBe('trade_finance');
    });

    test('should classify unknown messages', () => {
      const useCase = parser.classifyUseCase('UNKNOWN', {});
      expect(useCase).toBe('unknown');
    });
  });

  describe('Constants and Configuration', () => {
    test('should export SWIFT field definitions', () => {
      expect(SWIFT_FIELDS.MT103).toBeDefined();
      expect(SWIFT_FIELDS.MT103['20']).toBe('transaction_reference');
      expect(SWIFT_FIELDS.MT515).toBeDefined();
      expect(SWIFT_FIELDS.MT700).toBeDefined();
    });

    test('should export system parsers', () => {
      expect(SYSTEM_PARSERS.BANCS_XML).toBe('parseBANCSXML');
      expect(SYSTEM_PARSERS.FIS_FIXED).toBe('parseFISFixedWidth');
      expect(SYSTEM_PARSERS.TEMENOS_JSON).toBe('parseTemenosJSON');
      expect(SYSTEM_PARSERS.SWIFT_MT).toBe('parseSWIFTMessage');
      expect(SYSTEM_PARSERS.ISO20022).toBe('parseISO20022');
    });
  });
});