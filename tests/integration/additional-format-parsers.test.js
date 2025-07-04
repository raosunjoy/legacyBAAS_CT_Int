/**
 * Additional Format Parsers Test Suite
 * Comprehensive testing for ALL missing format implementations
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Testing: SWIFT MT (798,950,101), BaNCS, FIS, Temenos, SEPA, ACH/NACHA, EDIFACT, MTS
 */

const { EnhancedSWIFTParser } = require('../../src/adapters/enhanced-swift-parser');

describe('Additional Format Parsers - Complete Test Coverage', () => {
  let parser;
  
  beforeAll(() => {
    parser = new EnhancedSWIFTParser({
      enableValidation: true,
      enableMetrics: true,
      strictValidation: true
    });
  });

  beforeEach(() => {
    // Reset metrics for each test
    parser.resetMetrics();
  });

  describe('SWIFT MT Additional Message Types', () => {
    describe('MT798 - Proprietary Message', () => {
      const mt798Message = `{2:I798BANKGB22AXXX}
{4:
:20:REF123456789
:21:RELREF123456
:77A:Proprietary message content for MT798
:12:SUB001
:77E:Envelope content data
:32A:231201EUR1000,00
:50K:ORDERING CUSTOMER NAME
ADDRESS LINE 1
:59:BENEFICIARY NAME
ADDRESS LINE 1
-}`;

      test('should parse MT798 proprietary message successfully', async () => {
        const result = await parser.parseSWIFTMessage(mt798Message, 'MT798');
        
        expect(result.messageType).toBe('MT798');
        expect(result.standard).toBe('SWIFT');
        expect(result.useCase).toBe('proprietary_message');
        expect(result.fields.reference).toBeDefined();
        expect(result.fields.proprietary_message).toBe('Proprietary message content for MT798');
        expect(result.fields.sub_message_type).toBe('SUB001');
        expect(result.fields.envelope_contents).toBe('Envelope content data');
      });

      test('should validate required fields for MT798', () => {
        const invalidMessage = mt798Message.replace(':20:REF123456789', '');
        
        expect(async () => {
          await parser.parseSWIFTMessage(invalidMessage, 'MT798');
        }).rejects.toThrow('Missing required field: reference');
      });

      test('should handle MT798 with minimal required fields', async () => {
        const minimalMT798 = `{2:I798BANKGB22AXXX}
{4:
:20:REF123456789
:77A:Minimal proprietary message
-}`;

        const result = await parser.parseSWIFTMessage(minimalMT798, 'MT798');
        
        expect(result.messageType).toBe('MT798');
        expect(result.fields.reference).toBeDefined();
        expect(result.fields.proprietary_message).toBe('Minimal proprietary message');
      });
    });

    describe('MT950 - Statement Message', () => {
      const mt950Message = `{2:I950BANKGB22AXXX}
{4:
:20:STMT123456789
:25:GB29NWBK60161331926819
:28C:00001/001
:60F:C231130EUR5000,00
:61:2311301201CR1000,00NTRFNONREF//TXN123456789
Payment received
:62F:C231201EUR6000,00
:64:C231201EUR6000,00
:86:Account statement information
-}`;

      test('should parse MT950 statement message successfully', async () => {
        const result = await parser.parseSWIFTMessage(mt950Message, 'MT950');
        
        expect(result.messageType).toBe('MT950');
        expect(result.standard).toBe('SWIFT');
        expect(result.useCase).toBe('statement_message');
        expect(result.fields.transaction_reference).toBe('STMT123456789');
        expect(result.fields.account_identification).toBe('GB29NWBK60161331926819');
        expect(result.fields.statement_number).toBe('00001/001');
        expect(result.fields.opening_balance).toBe('C231130EUR5000,00');
        expect(result.fields.closing_balance).toBe('C231201EUR6000,00');
      });

      test('should validate required fields for MT950', () => {
        const invalidMessage = mt950Message.replace(':20:STMT123456789', '');
        
        expect(async () => {
          await parser.parseSWIFTMessage(invalidMessage, 'MT950');
        }).rejects.toThrow('Missing required field: transaction_reference');
      });
    });

    describe('MT101 - Request for Transfer', () => {
      const mt101Message = `{2:I101BANKGB22AXXX}
{4:
:20:REQ123456789
:23:CRED
:32A:231201EUR1000,00
:50A:INSTRUCTING PARTY
:51A:SENDING INSTITUTION
:52A:ACCOUNT WITH INSTITUTION
:53A:ACCOUNT WITH INSTITUTION 2
:54A:RECEIVING INSTITUTION
:59:BENEFICIARY CUSTOMER NAME
:70:Request for transfer payment
:71A:SHA
-}`;

      test('should parse MT101 request for transfer successfully', async () => {
        const result = await parser.parseSWIFTMessage(mt101Message, 'MT101');
        
        expect(result.messageType).toBe('MT101');
        expect(result.standard).toBe('SWIFT');
        expect(result.useCase).toBe('request_for_transfer');
        expect(result.fields.transaction_reference).toBe('REQ123456789');
        expect(result.fields.instruction_code).toBe('CRED');
        expect(result.fields.instructing_party).toBe('INSTRUCTING PARTY');
        expect(result.fields.beneficiary_customer).toBe('BENEFICIARY CUSTOMER NAME');
      });

      test('should validate required fields for MT101', () => {
        const invalidMessage = mt101Message.replace(':32A:231201EUR1000,00', '');
        
        expect(async () => {
          await parser.parseSWIFTMessage(invalidMessage, 'MT101');
        }).rejects.toThrow('Missing required field: value_date_currency_amount');
      });
    });
  });

  describe('TCS BaNCS Additional Formats', () => {
    describe('BaNCS Flat File Format', () => {
      const bancsFlat = `H|20231201|100000|001|BANK001
TXN001|1000.00|EUR|GB29NWBK60161331926819|DE89370400440532013000|JOHN DOE|JANE SMITH|Payment for services|20231201
TXN002|2500.00|USD|US1234567890123456|GB29NWBK60161331926820|ALICE BROWN|BOB WILSON|Invoice payment|20231201`;

      test('should parse BaNCS flat file successfully', async () => {
        const result = await parser.parseBANCSFlatFile(bancsFlat);
        
        expect(result.messageType).toBe('BANCS_FLAT');
        expect(result.standard).toBe('TCS_BANCS');
        expect(result.useCase).toBe('batch_processing');
        expect(result.header.recordType).toBe('H');
        expect(result.header.fileDate).toBe('20231201');
        expect(result.transactions).toHaveLength(2);
        expect(result.transactions[0].transactionId).toBe('TXN001');
        expect(result.transactions[0].amount).toBe('1000.00');
        expect(result.transactions[1].transactionId).toBe('TXN002');
      });

      test('should handle empty BaNCS flat file', async () => {
        const emptyFlat = 'H|20231201|100000|001|BANK001';
        const result = await parser.parseBANCSFlatFile(emptyFlat);
        
        expect(result.transactions).toHaveLength(0);
        expect(result.header.recordType).toBe('H');
      });
    });

    describe('BaNCS JSON API Format', () => {
      const bancsJSON = JSON.stringify({
        txnId: 'BNC123456789',
        amount: '1500.00',
        currency: 'EUR',
        debitAccount: 'GB29NWBK60161331926819',
        creditAccount: 'DE89370400440532013000',
        debitCustomer: 'MERCHANT CORP',
        creditCustomer: 'SUPPLIER LTD',
        purpose: 'Trade payment',
        valueDate: '2023-12-01',
        status: 'PENDING',
        txnRef: 'REF789456123'
      });

      test('should parse BaNCS JSON successfully', async () => {
        const result = await parser.parseBANCSJSON(bancsJSON);
        
        expect(result.messageType).toBe('BANCS_JSON');
        expect(result.standard).toBe('TCS_BANCS');
        expect(result.fields.transactionId).toBe('BNC123456789');
        expect(result.fields.amount).toBe('1500.00');
        expect(result.fields.currency).toBe('EUR');
        expect(result.fields.status).toBe('PENDING');
      });

      test('should handle malformed BaNCS JSON', () => {
        const malformedJSON = '{"txnId": "BNC123", invalid}';
        
        expect(async () => {
          await parser.parseBANCSJSON(malformedJSON);
        }).rejects.toThrow('BaNCS JSON parsing failed');
      });
    });
  });

  describe('FIS Additional Formats', () => {
    describe('FIS Profile JSON Format', () => {
      const fisJSON = JSON.stringify({
        transactionId: 'FIS789456123',
        amount: '2500.00',
        currencyCode: 'USD',
        fromAccount: '123456789',
        toAccount: '987654321',
        fromCustomer: 'CORP AMERICA',
        toCustomer: 'VENDOR INTL',
        description: 'Wire transfer',
        effectiveDate: '2023-12-01',
        transactionStatus: 'PROCESSED',
        batchNumber: 'BATCH001',
        productCode: 'WIRE'
      });

      test('should parse FIS JSON successfully', async () => {
        const result = await parser.parseFISJSON(fisJSON);
        
        expect(result.messageType).toBe('FIS_JSON');
        expect(result.standard).toBe('FIS_SYSTEMATICS');
        expect(result.fields.transactionId).toBe('FIS789456123');
        expect(result.fields.amount).toBe('2500.00');
        expect(result.fields.currency).toBe('USD');
        expect(result.fields.systemCode).toBe('WIRE');
      });
    });

    describe('FIS Delimited Format', () => {
      const fisDelimited = `TransactionID,Amount,Currency,FromAccount,ToAccount,CustomerName,Description,Date
FIS001,1000.00,EUR,ACC001,ACC002,JOHN SMITH,Payment,2023-12-01
FIS002,1500.50,USD,ACC003,ACC004,JANE DOE,Transfer,2023-12-01`;

      test('should parse FIS delimited format successfully', async () => {
        const result = await parser.parseFISDelimited(fisDelimited);
        
        expect(result.messageType).toBe('FIS_DELIMITED');
        expect(result.standard).toBe('FIS_SYSTEMATICS');
        expect(result.useCase).toBe('batch_processing');
        expect(result.transactions).toHaveLength(2);
        expect(result.transactions[0].transactionid).toBe('FIS001');
        expect(result.transactions[1].amount).toBe('1500.50');
      });

      test('should handle tab-delimited FIS format', async () => {
        const tabDelimited = `TransactionID\tAmount\tCurrency\nFIS001\t1000.00\tEUR`;
        const result = await parser.parseFISDelimited(tabDelimited);
        
        expect(result.transactions).toHaveLength(1);
        expect(result.transactions[0].transactionid).toBe('FIS001');
      });
    });
  });

  describe('Temenos XML Format', () => {
    const temenosXML = `<?xml version="1.0" encoding="UTF-8"?>
<Transaction>
  <TransactionId>TMN123456789</TransactionId>
  <Amount>3000.00</Amount>
  <CurrencyCode>GBP</CurrencyCode>
  <DebitAccount>GB29NWBK60161331926819</DebitAccount>
  <CreditAccount>DE89370400440532013000</CreditAccount>
  <DebitCustomer>UK CORP LTD</DebitCustomer>
  <CreditCustomer>GERMAN GMBH</CreditCustomer>
  <Description>International transfer</Description>
  <ValueDate>2023-12-01</ValueDate>
  <Status>COMPLETED</Status>
  <ProductCode>INTL</ProductCode>
</Transaction>`;

    test('should parse Temenos XML successfully', async () => {
      const result = await parser.parseTemenosXML(temenosXML);
      
      expect(result.messageType).toBe('TEMENOS_XML');
      expect(result.standard).toBe('TEMENOS_TRANSACT');
      expect(result.fields.transactionId).toBe('TMN123456789');
      expect(result.fields.amount).toBe('3000.00');
      expect(result.fields.currency).toBe('GBP');
      expect(result.fields.productCode).toBe('INTL');
    });

    test('should handle malformed Temenos XML', () => {
      const malformedXML = '<Transaction><Invalid></Transaction>';
      
      expect(async () => {
        await parser.parseTemenosXML(malformedXML);
      }).rejects.toThrow('Temenos XML parsing failed');
    });
  });

  describe('SEPA Format', () => {
    const sepaXML = `<?xml version="1.0" encoding="UTF-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03">
  <CstmrCdtTrfInitn>
    <GrpHdr>
      <MsgId>SEPA123456789</MsgId>
      <CreDtTm>2023-12-01T10:00:00Z</CreDtTm>
      <NbOfTxs>1</NbOfTxs>
      <CtrlSum>1000.00</CtrlSum>
    </GrpHdr>
    <PmtInf>
      <Dbtr>
        <Nm>SEPA SENDER</Nm>
      </Dbtr>
      <DbtrAcct>
        <Id>
          <IBAN>DE89370400440532013000</IBAN>
        </Id>
      </DbtrAcct>
      <DbtrAgt>
        <FinInstnId>
          <BIC>DEUTDEFF</BIC>
        </FinInstnId>
      </DbtrAgt>
    </PmtInf>
  </CstmrCdtTrfInitn>
</Document>`;

    test('should parse SEPA format successfully', async () => {
      const result = await parser.parseSEPA(sepaXML);
      
      expect(result.messageType).toBe('SEPA');
      expect(result.standard).toBe('SEPA');
      expect(result.useCase).toBe('sepa_credit_transfer');
      expect(result.fields.messageId).toBe('SEPA123456789');
      expect(result.fields.currency).toBe('EUR');
      expect(result.compliance.sepaCompliant).toBe(true);
      expect(result.compliance.euRegulatory).toBe(true);
    });
  });

  describe('ACH/NACHA Format', () => {
    const nachData = `101 123456789 987654321231201103012345671234567890123456789012345678901
5200COMPANY NAME           1234567890PPDDESCRIPTION   231201   1123456780000001
622123456789012345678901234567890000010000ID NUMBER    RECEIVER NAME           1123456780000001
822000000010123456780000000100000000000012345678901123456780000001
9000001000001000000001012345678000000010000000000001234567890                         `;

    test('should parse ACH/NACHA format successfully', async () => {
      const result = await parser.parseACHNACHA(nachData);
      
      expect(result.messageType).toBe('ACH_NACHA');
      expect(result.standard).toBe('ACH_NACHA');
      expect(result.useCase).toBe('ach_payment');
      expect(result.fileHeader).toBeDefined();
      expect(result.fileHeader.recordType).toBe('1');
      expect(result.batches).toHaveLength(1);
      expect(result.batches[0].entries).toHaveLength(1);
    });
  });

  describe('EDIFACT Format', () => {
    const edifactData = `UNH+1+PAYMUL:D:96A:UN'BGM+452+DOC123'DTM+137:20231201'MOA+9:1000.00:EUR'UNT+5+1'`;

    test('should parse EDIFACT format successfully', async () => {
      const result = await parser.parseEDIFACT(edifactData);
      
      expect(result.messageType).toBe('EDIFACT');
      expect(result.standard).toBe('EDIFACT');
      expect(result.useCase).toBe('edi_payment');
      expect(result.segments.UNH).toBeDefined();
      expect(result.fields.messageReference).toBe('1');
      expect(result.fields.documentNumber).toBe('DOC123');
    });
  });

  describe('MTS Format', () => {
    const mtsData = `TXNID=MTS123456789
AMT=2000.00
CCY=USD
FROM_ACCT=123456789
TO_ACCT=987654321
SENDER=CORP USA
RECEIVER=INTL VENDOR
PURPOSE=Trade payment`;

    test('should parse MTS format successfully', async () => {
      const result = await parser.parseMTS(mtsData);
      
      expect(result.messageType).toBe('MTS');
      expect(result.standard).toBe('MTS');
      expect(result.useCase).toBe('mts_transfer');
      expect(result.fields.transactionId).toBe('MTS123456789');
      expect(result.fields.amount).toBe('2000.00');
      expect(result.fields.currency).toBe('USD');
    });
  });

  describe('Error Handling and Edge Cases', () => {
    test('should handle unsupported SWIFT message type', () => {
      expect(async () => {
        await parser.parseSWIFTMessage('invalid', 'MT999');
      }).rejects.toThrow('Unsupported SWIFT message type: MT999');
    });

    test('should handle empty input gracefully', async () => {
      await expect(parser.parseBANCSFlatFile('')).rejects.toThrow('Empty BaNCS flat file data provided');
    });

    test('should track metrics for all format types', async () => {
      parser.resetMetrics();
      
      const mt798 = `{2:I798BANKGB22AXXX}{4::20:REF123:77A:Test-}`;
      await parser.parseSWIFTMessage(mt798, 'MT798');
      
      const metrics = parser.getISO20022Metrics();
      expect(metrics.totalMessagesParsed).toBe(1);
    });
  });

  describe('Integration Tests', () => {
    test('should handle mixed format batch processing', async () => {
      const results = [];
      
      // Process multiple formats
      const mt798 = `{2:I798BANKGB22AXXX}{4::20:REF123:77A:Test-}`;
      results.push(await parser.parseSWIFTMessage(mt798, 'MT798'));
      
      const bancsJSON = JSON.stringify({ txnId: 'BNC123', amount: '100' });
      results.push(await parser.parseBANCSJSON(bancsJSON));
      
      const mtsData = 'TXNID=MTS123\nAMT=200.00';
      results.push(await parser.parseMTS(mtsData));
      
      expect(results).toHaveLength(3);
      expect(results[0].messageType).toBe('MT798');
      expect(results[1].messageType).toBe('BANCS_JSON');
      expect(results[2].messageType).toBe('MTS');
    });
  });
});