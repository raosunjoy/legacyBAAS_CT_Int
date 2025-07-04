/**
 * ISO 20022 SwiftParser Compatibility Verification
 * Critical analysis and testing for SWIFT standards compliance
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * ISO 20022 SWIFT Financial Messaging Standards Validation
 */

const { EnhancedSWIFTParser } = require('../../src/adapters/enhanced-swift-parser');
const fs = require('fs').promises;
const path = require('path');

describe('ISO 20022 SwiftParser Verification', () => {
  let swiftParser;
  let iso20022TestMessages;

  beforeAll(async () => {
    swiftParser = new EnhancedSWIFTParser({
      enableValidation: true,
      enableMetrics: true,
      iso20022Compliance: true
    });

    // ISO 20022 test message samples
    iso20022TestMessages = {
      pain001: {
        // Customer Credit Transfer Initiation
        messageType: 'pain.001.001.03',
        xml: `<?xml version="1.0" encoding="UTF-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pain.001.001.03">
  <CstmrCdtTrfInitn>
    <GrpHdr>
      <MsgId>MSG123456789</MsgId>
      <CreDtTm>2023-12-01T10:00:00Z</CreDtTm>
      <NbOfTxs>1</NbOfTxs>
      <CtrlSum>1000.00</CtrlSum>
      <InitgPty>
        <Nm>Example Bank</Nm>
        <Id>
          <OrgId>
            <BICOrBEI>EXAMPLEBANK</BICOrBEI>
          </OrgId>
        </Id>
      </InitgPty>
    </GrpHdr>
    <PmtInf>
      <PmtInfId>PMT123456789</PmtInfId>
      <PmtMtd>TRF</PmtMtd>
      <ReqdExctnDt>2023-12-01</ReqdExctnDt>
      <Dbtr>
        <Nm>JOHN DOE</Nm>
      </Dbtr>
      <DbtrAcct>
        <Id>
          <IBAN>GB29NWBK60161331926819</IBAN>
        </Id>
      </DbtrAcct>
      <DbtrAgt>
        <FinInstnId>
          <BIC>NWBKGB21</BIC>
        </FinInstnId>
      </DbtrAgt>
      <CdtTrfTxInf>
        <PmtId>
          <EndToEndId>E2E123456789</EndToEndId>
        </PmtId>
        <Amt>
          <InstdAmt Ccy="EUR">1000.00</InstdAmt>
        </Amt>
        <CdtrAgt>
          <FinInstnId>
            <BIC>DEUTDEFF</BIC>
          </FinInstnId>
        </CdtrAgt>
        <Cdtr>
          <Nm>JANE SMITH</Nm>
        </Cdtr>
        <CdtrAcct>
          <Id>
            <IBAN>DE89370400440532013000</IBAN>
          </Id>
        </CdtrAcct>
        <RmtInf>
          <Ustrd>Payment for invoice 12345</Ustrd>
        </RmtInf>
      </CdtTrfTxInf>
    </PmtInf>
  </CstmrCdtTrfInitn>
</Document>`
      },
      pacs008: {
        // Financial Institution Credit Transfer
        messageType: 'pacs.008.001.02',
        xml: `<?xml version="1.0" encoding="UTF-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:pacs.008.001.02">
  <FIToFICstmrCdtTrf>
    <GrpHdr>
      <MsgId>FI123456789</MsgId>
      <CreDtTm>2023-12-01T10:00:00Z</CreDtTm>
      <NbOfTxs>1</NbOfTxs>
      <TtlIntrBkSttlmAmt Ccy="EUR">1000.00</TtlIntrBkSttlmAmt>
      <IntrBkSttlmDt>2023-12-01</IntrBkSttlmDt>
      <SttlmInf>
        <SttlmMtd>CLRG</SttlmMtd>
      </SttlmInf>
      <InstgAgt>
        <FinInstnId>
          <BIC>NWBKGB21</BIC>
        </FinInstnId>
      </InstgAgt>
      <InstdAgt>
        <FinInstnId>
          <BIC>DEUTDEFF</BIC>
        </FinInstnId>
      </InstdAgt>
    </GrpHdr>
    <CdtTrfTxInf>
      <PmtId>
        <InstrId>INSTR123456789</InstrId>
        <EndToEndId>E2E123456789</EndToEndId>
        <TxId>TXN123456789</TxId>
      </PmtId>
      <IntrBkSttlmAmt Ccy="EUR">1000.00</IntrBkSttlmAmt>
      <ChrgBr>SLEV</ChrgBr>
      <Dbtr>
        <Nm>JOHN DOE</Nm>
      </Dbtr>
      <DbtrAcct>
        <Id>
          <IBAN>GB29NWBK60161331926819</IBAN>
        </Id>
      </DbtrAcct>
      <DbtrAgt>
        <FinInstnId>
          <BIC>NWBKGB21</BIC>
        </FinInstnId>
      </DbtrAgt>
      <CdtrAgt>
        <FinInstnId>
          <BIC>DEUTDEFF</BIC>
        </FinInstnId>
      </CdtrAgt>
      <Cdtr>
        <Nm>JANE SMITH</Nm>
      </Cdtr>
      <CdtrAcct>
        <Id>
          <IBAN>DE89370400440532013000</IBAN>
        </Id>
      </CdtrAcct>
      <RmtInf>
        <Ustrd>Payment for invoice 12345</Ustrd>
      </RmtInf>
    </CdtTrfTxInf>
  </FIToFICstmrCdtTrf>
</Document>`
      },
      camt053: {
        // Bank to Customer Statement
        messageType: 'camt.053.001.02',
        xml: `<?xml version="1.0" encoding="UTF-8"?>
<Document xmlns="urn:iso:std:iso:20022:tech:xsd:camt.053.001.02">
  <BkToCstmrStmt>
    <GrpHdr>
      <MsgId>STMT123456789</MsgId>
      <CreDtTm>2023-12-01T10:00:00Z</CreDtTm>
      <MsgRcpt>
        <Nm>JOHN DOE</Nm>
        <Id>
          <OrgId>
            <Othr>
              <Id>12345</Id>
            </Othr>
          </OrgId>
        </Id>
      </MsgRcpt>
    </GrpHdr>
    <Stmt>
      <Id>STMT20231201001</Id>
      <ElctrncSeqNb>1</ElctrncSeqNb>
      <CreDtTm>2023-12-01T10:00:00Z</CreDtTm>
      <FrToDt>
        <FrDtTm>2023-12-01T00:00:00Z</FrDtTm>
        <ToDtTm>2023-12-01T23:59:59Z</ToDtTm>
      </FrToDt>
      <Acct>
        <Id>
          <IBAN>GB29NWBK60161331926819</IBAN>
        </Id>
        <Ccy>EUR</Ccy>
        <Nm>JOHN DOE CURRENT ACCOUNT</Nm>
        <Svcr>
          <FinInstnId>
            <BIC>NWBKGB21</BIC>
          </FinInstnId>
        </Svcr>
      </Acct>
      <Bal>
        <Tp>
          <CdOrPrtry>
            <Cd>OPBD</Cd>
          </CdOrPrtry>
        </Tp>
        <Amt Ccy="EUR">5000.00</Amt>
        <CdtDbtInd>CRDT</CdtDbtInd>
        <Dt>
          <Dt>2023-12-01</Dt>
        </Dt>
      </Bal>
      <Bal>
        <Tp>
          <CdOrPrtry>
            <Cd>CLBD</Cd>
          </CdOrPrtry>
        </Tp>
        <Amt Ccy="EUR">6000.00</Amt>
        <CdtDbtInd>CRDT</CdtDbtInd>
        <Dt>
          <Dt>2023-12-01</Dt>
        </Dt>
      </Bal>
    </Stmt>
  </BkToCstmrStmt>
</Document>`
      }
    };
  });

  describe('ISO 20022 Message Type Support', () => {
    test('should support pain.001 (Customer Credit Transfer Initiation)', async () => {
      const message = iso20022TestMessages.pain001;
      
      const result = await swiftParser.parseISO20022(message.xml);
      
      expect(result.messageType).toBe('pain.001.001.03');
      expect(result.isValid).toBe(true);
      expect(result.parsedData.groupHeader.messageId).toBe('MSG123456789');
      expect(result.parsedData.groupHeader.numberOfTransactions).toBe('1');
      expect(result.parsedData.paymentInformation).toBeDefined();
      expect(result.parsedData.paymentInformation.paymentMethod).toBe('TRF');
    });

    test('should support pacs.008 (Financial Institution Credit Transfer)', async () => {
      const message = iso20022TestMessages.pacs008;
      
      const result = await swiftParser.parseISO20022(message.xml);
      
      expect(result.messageType).toBe('pacs.008.001.02');
      expect(result.isValid).toBe(true);
      expect(result.parsedData.groupHeader.messageId).toBe('FI123456789');
      expect(result.parsedData.creditTransferTransactionInformation).toBeDefined();
      expect(result.parsedData.creditTransferTransactionInformation.chargeBearing).toBe('SLEV');
    });

    test('should support camt.053 (Bank to Customer Statement)', async () => {
      const message = iso20022TestMessages.camt053;
      
      const result = await swiftParser.parseISO20022(message.xml);
      
      expect(result.messageType).toBe('camt.053.001.02');
      expect(result.isValid).toBe(true);
      expect(result.parsedData.groupHeader.messageId).toBe('STMT123456789');
      expect(result.parsedData.statement).toBeDefined();
      expect(result.parsedData.statement.account.currency).toBe('EUR');
    });
  });

  describe('ISO 20022 Field Validation', () => {
    test('should validate IBAN format in ISO 20022 messages', async () => {
      const validIBANs = [
        'GB29NWBK60161331926819',
        'DE89370400440532013000',
        'FR1420041010050500013M02606',
        'CH9300762011623852957'
      ];

      for (const iban of validIBANs) {
        const isValid = swiftParser.validateIBAN(iban);
        expect(isValid).toBe(true);
      }
    });

    test('should validate BIC format in ISO 20022 messages', async () => {
      const validBICs = [
        'NWBKGB21',
        'DEUTDEFF',
        'CHASUS33',
        'BNPAFRPP'
      ];

      for (const bic of validBICs) {
        const isValid = swiftParser.validateBIC(bic);
        expect(isValid).toBe(true);
      }
    });

    test('should validate currency codes (ISO 4217)', async () => {
      const validCurrencies = ['EUR', 'USD', 'GBP', 'CHF', 'JPY'];
      const invalidCurrencies = ['XXX', '123', 'EURO'];

      for (const currency of validCurrencies) {
        const isValid = swiftParser.validateCurrencyCode(currency);
        expect(isValid).toBe(true);
      }

      for (const currency of invalidCurrencies) {
        const isValid = swiftParser.validateCurrencyCode(currency);
        expect(isValid).toBe(false);
      }
    });

    test('should validate amount format and precision', async () => {
      const validAmounts = [
        { amount: '1000.00', currency: 'EUR', valid: true },
        { amount: '0.01', currency: 'USD', valid: true },
        { amount: '999999.99', currency: 'GBP', valid: true },
        { amount: '1000', currency: 'JPY', valid: true }, // JPY has no decimals
      ];

      const invalidAmounts = [
        { amount: '1000.001', currency: 'EUR', valid: false }, // Too many decimals
        { amount: '-100.00', currency: 'USD', valid: false }, // Negative
        { amount: '1000.1', currency: 'JPY', valid: false }, // JPY shouldn't have decimals
      ];

      [...validAmounts, ...invalidAmounts].forEach(test => {
        const isValid = swiftParser.validateAmount(test.amount, test.currency);
        expect(isValid).toBe(test.valid);
      });
    });
  });

  describe('ISO 20022 Message Structure Validation', () => {
    test('should validate mandatory fields in pain.001', async () => {
      const message = iso20022TestMessages.pain001;
      
      const result = await swiftParser.parseISO20022(message.xml);
      
      expect(result.validation.mandatoryFields).toEqual(
        expect.arrayContaining([
          'groupHeader.messageId',
          'groupHeader.creationDateTime',
          'groupHeader.numberOfTransactions',
          'paymentInformation.paymentInformationId',
          'paymentInformation.paymentMethod',
          'paymentInformation.requestedExecutionDate'
        ])
      );
      expect(result.validation.missingFields).toHaveLength(0);
    });

    test('should validate namespace and schema compliance', async () => {
      const message = iso20022TestMessages.pain001;
      
      const result = await swiftParser.parseISO20022(message.xml);
      
      expect(result.validation.namespaceCompliant).toBe(true);
      expect(result.validation.schemaVersion).toBe('pain.001.001.03');
      expect(result.validation.namespace).toBe('urn:iso:std:iso:20022:tech:xsd:pain.001.001.03');
    });

    test('should detect and report schema violations', async () => {
      const invalidMessage = iso20022TestMessages.pain001.xml
        .replace('<MsgId>MSG123456789</MsgId>', '') // Remove mandatory field
        .replace('pain.001.001.03', 'pain.001.001.99'); // Invalid version

      const result = await swiftParser.parseISO20022(invalidMessage);
      
      expect(result.isValid).toBe(false);
      expect(result.validation.errors).toEqual(
        expect.arrayContaining([
          expect.stringContaining('Missing mandatory field: MsgId'),
          expect.stringContaining('Invalid schema version')
        ])
      );
    });
  });

  describe('ISO 20022 Business Rules Validation', () => {
    test('should validate SEPA compliance rules', async () => {
      const sepaTransaction = {
        debtorIBAN: 'DE89370400440532013000',
        creditorIBAN: 'FR1420041010050500013M02606',
        amount: '999.99',
        currency: 'EUR',
        executionDate: '2023-12-01'
      };

      const isSepaCompliant = swiftParser.validateSEPACompliance(sepaTransaction);
      
      expect(isSepaCompliant.isValid).toBe(true);
      expect(isSepaCompliant.rules).toEqual(
        expect.arrayContaining([
          'SEPA_CURRENCY_EUR',
          'SEPA_AMOUNT_LIMIT',
          'SEPA_IBAN_FORMAT',
          'SEPA_EXECUTION_TIME'
        ])
      );
    });

    test('should validate SWIFT GPI tracking requirements', async () => {
      const gpiTransaction = {
        messageType: 'pacs.008.001.02',
        uetr: '550e8400-e29b-41d4-a716-446655440000', // UUID format
        instructionId: 'INSTR123456789',
        endToEndId: 'E2E123456789',
        transactionId: 'TXN123456789'
      };

      const isGPICompliant = swiftParser.validateSWIFTGPICompliance(gpiTransaction);
      
      expect(isGPICompliant.isValid).toBe(true);
      expect(isGPICompliant.features).toEqual(
        expect.arrayContaining([
          'UETR_TRACKING',
          'END_TO_END_TRACKING',
          'TRANSACTION_TRACEABILITY'
        ])
      );
    });

    test('should validate cross-border payment requirements', async () => {
      const crossBorderPayment = {
        debtorCountry: 'GB',
        creditorCountry: 'DE',
        amount: '5000.00',
        currency: 'EUR',
        purpose: 'GOODS_PURCHASE',
        regulatoryReporting: true
      };

      const isCrossBorderCompliant = swiftParser.validateCrossBorderCompliance(crossBorderPayment);
      
      expect(isCrossBorderCompliant.isValid).toBe(true);
      expect(isCrossBorderCompliant.requirements).toEqual(
        expect.arrayContaining([
          'REGULATORY_REPORTING',
          'PURPOSE_CODE',
          'BENEFICIARY_DETAILS',
          'COMPLIANCE_SCREENING'
        ])
      );
    });
  });

  describe('ISO 20022 Performance and Scalability', () => {
    test('should process large batch of ISO 20022 messages efficiently', async () => {
      const batchSize = 100;
      const messages = Array(batchSize).fill(iso20022TestMessages.pain001.xml);
      
      const startTime = Date.now();
      const results = await Promise.all(
        messages.map(message => swiftParser.parseISO20022(message))
      );
      const endTime = Date.now();
      
      expect(results).toHaveLength(batchSize);
      expect(results.every(result => result.isValid)).toBe(true);
      expect(endTime - startTime).toBeLessThan(5000); // Should complete within 5 seconds
    });

    test('should handle concurrent message processing', async () => {
      const concurrentMessages = [
        iso20022TestMessages.pain001.xml,
        iso20022TestMessages.pacs008.xml,
        iso20022TestMessages.camt053.xml
      ];

      const startTime = Date.now();
      const results = await Promise.all(
        concurrentMessages.map(message => swiftParser.parseISO20022(message))
      );
      const endTime = Date.now();

      expect(results).toHaveLength(3);
      expect(results.every(result => result.isValid)).toBe(true);
      expect(endTime - startTime).toBeLessThan(2000); // Should complete within 2 seconds
    });
  });

  describe('Legacy SWIFT MT to ISO 20022 Migration', () => {
    test('should convert MT103 to pain.001/pacs.008', async () => {
      const mt103Message = {
        messageType: 'MT103',
        transactionReference: '20:TXN123456789',
        valueDate: '32A:231201EUR1000,00',
        orderingCustomer: '50K:JOHN DOE\nGB29NWBK60161331926819',
        beneficiaryCustomer: '59:JANE SMITH\nDE89370400440532013000',
        remittanceInformation: '70:PAYMENT FOR INVOICE 12345'
      };

      const convertedMessage = await swiftParser.convertMT103ToISO20022(mt103Message);
      
      expect(convertedMessage.messageType).toBe('pain.001.001.03');
      expect(convertedMessage.xml).toContain('pain.001.001.03');
      expect(convertedMessage.mapping.success).toBe(true);
      expect(convertedMessage.mapping.fieldsConverted).toBeGreaterThan(5);
    });

    test('should maintain data integrity during conversion', async () => {
      const mt515Message = {
        messageType: 'MT515',
        transactionReference: '20:SEC123456789',
        securities: '35B:ISIN DE0001234567\n100 DEUTSCHE BANK AG',
        tradeDate: '98A:20231201'
      };

      const convertedMessage = await swiftParser.convertMT515ToISO20022(mt515Message);
      
      expect(convertedMessage.messageType).toBe('semt.013.001.03');
      expect(convertedMessage.validation.dataIntegrityCheck).toBe(true);
      expect(convertedMessage.validation.losslessConversion).toBe(true);
    });
  });

  describe('Regulatory Compliance Integration', () => {
    test('should support European regulatory requirements', async () => {
      const europeanPayment = iso20022TestMessages.pain001.xml;
      
      const complianceResult = await swiftParser.validateEuropeanCompliance(europeanPayment);
      
      expect(complianceResult.psd2Compliant).toBe(true);
      expect(complianceResult.gdprCompliant).toBe(true);
      expect(complianceResult.amlCompliant).toBe(true);
      expect(complianceResult.regulations).toEqual(
        expect.arrayContaining([
          'PSD2_STRONG_AUTHENTICATION',
          'GDPR_DATA_PROTECTION',
          'AML_SCREENING',
          'SEPA_REGULATION'
        ])
      );
    });

    test('should support US regulatory requirements', async () => {
      const usPayment = iso20022TestMessages.pacs008.xml
        .replace('EUR', 'USD')
        .replace('DEUTDEFF', 'CHASUS33')
        .replace('DE89370400440532013000', 'US1234567890123456');

      const complianceResult = await swiftParser.validateUSCompliance(usPayment);
      
      expect(complianceResult.bsaCompliant).toBe(true);
      expect(complianceResult.ofacCompliant).toBe(true);
      expect(complianceResult.fedwireCompliant).toBe(true);
      expect(complianceResult.regulations).toEqual(
        expect.arrayContaining([
          'BSA_REPORTING',
          'OFAC_SCREENING',
          'FEDWIRE_FORMAT',
          'CTR_COMPLIANCE'
        ])
      );
    });
  });

  describe('Error Handling and Recovery', () => {
    test('should handle malformed XML gracefully', async () => {
      const malformedXML = '<Document><InvalidXML></Document>';
      
      const result = await swiftParser.parseISO20022(malformedXML);
      
      expect(result.isValid).toBe(false);
      expect(result.errors).toEqual(
        expect.arrayContaining([
          expect.stringContaining('XML_PARSE_ERROR')
        ])
      );
    });

    test('should provide detailed validation feedback', async () => {
      const invalidMessage = iso20022TestMessages.pain001.xml
        .replace('EUR', 'XXX') // Invalid currency
        .replace('1000.00', 'invalid_amount'); // Invalid amount format

      const result = await swiftParser.parseISO20022(invalidMessage);
      
      expect(result.isValid).toBe(false);
      expect(result.validation.fieldErrors).toEqual(
        expect.arrayContaining([
          expect.objectContaining({
            field: 'InstdAmt',
            error: 'INVALID_CURRENCY_CODE'
          }),
          expect.objectContaining({
            field: 'InstdAmt',
            error: 'INVALID_AMOUNT_FORMAT'
          })
        ])
      );
    });
  });

  describe('ISO 20022 Metrics and Monitoring', () => {
    test('should collect comprehensive parsing metrics', async () => {
      await swiftParser.parseISO20022(iso20022TestMessages.pain001.xml);
      await swiftParser.parseISO20022(iso20022TestMessages.pacs008.xml);
      await swiftParser.parseISO20022(iso20022TestMessages.camt053.xml);

      const metrics = swiftParser.getISO20022Metrics();
      
      expect(metrics.totalMessagesParsed).toBe(3);
      expect(metrics.messageTypes).toEqual(
        expect.objectContaining({
          'pain.001.001.03': 1,
          'pacs.008.001.02': 1,
          'camt.053.001.02': 1
        })
      );
      expect(metrics.averageParsingTime).toBeGreaterThan(0);
      expect(metrics.validationSuccessRate).toBeGreaterThan(0.95);
    });

    test('should track compliance validation statistics', async () => {
      // Process various compliance scenarios
      await swiftParser.validateSEPACompliance({
        debtorIBAN: 'DE89370400440532013000',
        creditorIBAN: 'FR1420041010050500013M02606',
        amount: '999.99',
        currency: 'EUR'
      });

      const complianceMetrics = swiftParser.getComplianceMetrics();
      
      expect(complianceMetrics.sepaValidations).toBeGreaterThan(0);
      expect(complianceMetrics.complianceSuccessRate).toBeGreaterThan(0.9);
      expect(complianceMetrics.averageValidationTime).toBeGreaterThan(0);
    });
  });
});