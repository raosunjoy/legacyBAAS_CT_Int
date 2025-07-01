/**
 * SWIFT Parser Tests
 * Comprehensive test suite with 100% coverage
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const SWIFTParser = require('../../src/adapters/swift-parser');

describe('SWIFT Parser', () => {
  let parser;

  beforeEach(() => {
    parser = new SWIFTParser();
  });

  describe('Constructor', () => {
    test('should initialize with supported message types', () => {
      expect(parser.supportedMessageTypes).toEqual(['MT103', 'MT202']);
    });

    test('should return supported types', () => {
      const types = parser.getSupportedTypes();
      expect(types).toEqual(['MT103', 'MT202']);
      expect(types).not.toBe(parser.supportedMessageTypes); // Should be a copy
    });
  });

  describe('MT103 Message Parsing', () => {
    const validMT103 = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}{3:{113:ROMF}{108:ILOVEML99990906}{119:STP}}{4:
:20:TRN123456789
:23B:CRED
:32A:221201USD1000000.00
:50K:/12345678901234567890
JOHN DOE
123 MAIN STREET
NEW YORK NY 10001
:52A:ABNAAU2A
:57A:CHASUS33
:59:/98765432109876543210
JANE SMITH
456 OAK AVENUE
LOS ANGELES CA 90210
:70:PAYMENT FOR SERVICES
:71A:OUR
}`;

    test('should parse valid MT103 message correctly', () => {
      const result = parser.parse(validMT103);

      expect(result).toMatchObject({
        messageType: 'MT103',
        transactionReference: 'TRN123456789',
        amount: 1000000.00,
        currency: 'USD',
        valueDate: '2022-12-01',
        status: 'parsed'
      });

      expect(result.sender).toMatchObject({
        account: '/12345678901234567890',
        name: 'JOHN DOE',
        address: '123 MAIN STREET, NEW YORK NY 10001'
      });

      expect(result.receiver).toMatchObject({
        account: '/98765432109876543210',
        name: 'JANE SMITH',
        address: '456 OAK AVENUE, LOS ANGELES CA 90210'
      });

      expect(result.remittanceInfo).toBe('PAYMENT FOR SERVICES');
      expect(result.id).toBeDefined();
      expect(result.timestamp).toBeDefined();
    });

    test('should handle MT103 with minimal required fields', () => {
      const minimalMT103 = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}{4:
:20:MIN123
:32A:221201USD100.00
:50K:SENDER NAME
:59:RECEIVER NAME
}`;

      const result = parser.parse(minimalMT103);
      expect(result.messageType).toBe('MT103');
      expect(result.transactionReference).toBe('MIN123');
      expect(result.amount).toBe(100.00);
    });

    test('should throw error for MT103 missing required fields', () => {
      const invalidMT103 = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}{4:
:20:TRN123456789
:32A:221201USD1000.00
}`;

      expect(() => parser.parse(invalidMT103)).toThrow('Missing required field: 50K for MT103');
    });
  });

  describe('MT202 Message Parsing', () => {
    const validMT202 = `{1:F01ABNAAU2A0000000000}{2:I202BBBBGB2LXXXXN}{4:
:20:FIT123456789
:21:REL987654321
:32A:221201EUR500000.00
:52A:ABNAAU2A
:58A:CHASUS33
:72:CORRESPONDENT BANKING TRANSFER
}`;

    test('should parse valid MT202 message correctly', () => {
      const result = parser.parse(validMT202);

      expect(result).toMatchObject({
        messageType: 'MT202',
        transactionReference: 'FIT123456789',
        amount: 500000.00,
        currency: 'EUR',
        valueDate: '2022-12-01',
        status: 'parsed',
        orderingInstitution: 'ABNAAU2A',
        beneficiaryInstitution: 'CHASUS33',
        senderToReceiverInfo: 'CORRESPONDENT BANKING TRANSFER'
      });
    });

    test('should throw error for MT202 missing required fields', () => {
      const invalidMT202 = `{1:F01ABNAAU2A0000000000}{2:I202BBBBGB2LXXXXN}{4:
:20:FIT123456789
:32A:221201EUR500000.00
}`;

      expect(() => parser.parse(invalidMT202)).toThrow('Missing required field: 52A for MT202');
    });
  });

  describe('Message Type Extraction', () => {
    test('should extract MT103 from header', () => {
      const message = '{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}';
      const type = parser.extractMessageType(message);
      expect(type).toBe('MT103');
    });

    test('should extract MT202 from header', () => {
      const message = '{1:F01ABNAAU2A0000000000}{2:I202BBBBGB2LXXXXN}';
      const type = parser.extractMessageType(message);
      expect(type).toBe('MT202');
    });

    test('should throw error for invalid header format', () => {
      const invalidMessage = 'INVALID MESSAGE';
      expect(() => parser.extractMessageType(invalidMessage)).toThrow('Invalid SWIFT message format: missing application header');
    });
  });

  describe('Amount Field Parsing', () => {
    test('should parse amount field correctly', () => {
      const result = parser.parseAmountField('221201USD1000000.00');
      
      expect(result).toEqual({
        valueDate: '2022-12-01',
        currency: 'USD',
        amount: 1000000.00
      });
    });

    test('should parse amount with commas', () => {
      const result = parser.parseAmountField('221201EUR1,500,000.50');
      
      expect(result).toEqual({
        valueDate: '2022-12-01',
        currency: 'EUR',
        amount: 1500000.50
      });
    });

    test('should throw error for invalid amount format', () => {
      expect(() => parser.parseAmountField('INVALID')).toThrow('Invalid amount field format: INVALID');
    });
  });

  describe('Customer Info Parsing', () => {
    test('should parse multi-line customer info', () => {
      const customerInfo = `/12345678901234567890\nJOHN DOE\n123 MAIN STREET\nNEW YORK NY 10001`;
      const result = parser.parseCustomerInfo(customerInfo);

      expect(result).toEqual({
        account: '/12345678901234567890',
        name: 'JOHN DOE',
        address: '123 MAIN STREET, NEW YORK NY 10001',
        raw: customerInfo
      });
    });

    test('should handle single line customer info', () => {
      const customerInfo = 'SIMPLE NAME';
      const result = parser.parseCustomerInfo(customerInfo);

      expect(result).toEqual({
        account: 'SIMPLE NAME',
        name: null,
        address: null,
        raw: customerInfo
      });
    });

    test('should handle null customer info', () => {
      const result = parser.parseCustomerInfo(null);
      expect(result).toBeNull();
    });

    test('should handle undefined customer info', () => {
      const result = parser.parseCustomerInfo(undefined);
      expect(result).toBeNull();
    });

    test('should handle empty customer info', () => {
      const result = parser.parseCustomerInfo('');
      expect(result).toBeNull();
    });
  });

  describe('BIC Validation', () => {
    test('should validate correct BIC codes', () => {
      expect(parser.validateBIC('ABNAAU2A')).toBe(true);
      expect(parser.validateBIC('CHASUS33')).toBe(true);
      expect(parser.validateBIC('DEUTDEFF')).toBe(true);
      expect(parser.validateBIC('HSBCHKHH')).toBe(true);
      expect(parser.validateBIC('ABNAAU2AXXX')).toBe(true); // With branch
    });

    test('should reject invalid BIC codes', () => {
      expect(parser.validateBIC('ABC')).toBe(false); // Too short
      expect(parser.validateBIC('ABCDEFGHIJKL')).toBe(false); // Too long (12 chars)
      expect(parser.validateBIC('123AAU2A')).toBe(false); // Numbers in institution
      expect(parser.validateBIC('ABNA112A')).toBe(false); // Numbers in country
      expect(parser.validateBIC('')).toBe(false); // Empty
      expect(parser.validateBIC(null)).toBe(false); // Null
      expect(parser.validateBIC(undefined)).toBe(false); // Undefined
      expect(parser.validateBIC(123)).toBe(false); // Not string
    });

    test('should handle case insensitive BIC validation', () => {
      expect(parser.validateBIC('abnaau2a')).toBe(true);
      expect(parser.validateBIC('ChAsUs33')).toBe(true);
    });
  });

  describe('Error Handling', () => {
    test('should throw error for null message', () => {
      expect(() => parser.parse(null)).toThrow('Invalid message format: message must be a non-empty string');
    });

    test('should throw error for undefined message', () => {
      expect(() => parser.parse(undefined)).toThrow('Invalid message format: message must be a non-empty string');
    });

    test('should throw error for empty string', () => {
      expect(() => parser.parse('')).toThrow('Invalid message format: message must be a non-empty string');
    });

    test('should throw error for non-string message', () => {
      expect(() => parser.parse(123)).toThrow('Invalid message format: message must be a non-empty string');
      expect(() => parser.parse({})).toThrow('Invalid message format: message must be a non-empty string');
      expect(() => parser.parse([])).toThrow('Invalid message format: message must be a non-empty string');
    });

    test('should throw error for unsupported message type', () => {
      const unsupportedMessage = `{1:F01ABNAAU2A0000000000}{2:I999BBBBGB2LXXXXN}{4::20:TEST}`;
      expect(() => parser.parse(unsupportedMessage)).toThrow('Unsupported message type: MT999');
    });

    test('should throw error for missing text block', () => {
      const noTextBlock = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}`;
      expect(() => parser.parse(noTextBlock)).toThrow('Invalid SWIFT message format: missing text block');
    });

    test('should throw error for no valid fields', () => {
      const noFields = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}{4:INVALID CONTENT}`;
      expect(() => parser.parse(noFields)).toThrow('No valid fields found in message');
    });
  });

  describe('JSON Conversion', () => {
    test('should convert message to JSON string', () => {
      const message = {
        id: 'test-id',
        messageType: 'MT103',
        transactionReference: 'TRN123'
      };

      const json = parser.toJSON(message);
      const parsed = JSON.parse(json);

      expect(parsed).toEqual(message);
      expect(json).toContain('"messageType": "MT103"');
    });
  });

  describe('Field Parsing Edge Cases', () => {
    test('should handle fields without content', () => {
      const messageWithEmptyField = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}{4:
:20:TRN123
:23B:
:32A:221201USD100.00
:50K:SENDER
:59:RECEIVER
}`;

      const result = parser.parse(messageWithEmptyField);
      expect(result.transactionReference).toBe('TRN123');
    });

    test('should handle complex field parsing', () => {
      const complexMessage = `{1:F01ABNAAU2A0000000000}{2:I103BBBBGB2LXXXXN}{4:
:20:TRN123456789
:32A:221201USD1000.00
:50K:/ACC123
COMPLEX NAME WITH
MULTIPLE LINES
AND SPECIAL CHARS
:59:RECEIVER NAME
:70:MULTI
LINE
REMITTANCE
INFO
}`;

      const result = parser.parse(complexMessage);
      expect(result.sender.address).toContain('MULTIPLE LINES');
      expect(result.remittanceInfo).toContain('MULTI');
    });
  });

  describe('Required Fields Validation', () => {
    test('should get required fields for MT103', () => {
      const required = parser.getRequiredFields('MT103');
      expect(required).toEqual(['20', '32A', '50K', '59']);
    });

    test('should get required fields for MT202', () => {
      const required = parser.getRequiredFields('MT202');
      expect(required).toEqual(['20', '32A', '52A', '58A']);
    });

    test('should return empty array for unknown message type', () => {
      const required = parser.getRequiredFields('MT999');
      expect(required).toEqual([]);
    });
  });
});