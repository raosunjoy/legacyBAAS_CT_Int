/**
 * Jest Test Setup for Banking Legacy-to-Blockchain B2BaaS Platform
 * Global test configuration and utilities
 */

// Extend Jest with additional matchers
require('jest-extended');

// Set test environment variables
process.env.NODE_ENV = 'test';
process.env.LOG_LEVEL = 'error'; // Reduce noise in tests
process.env.PORT = '0'; // Use random port for tests

// Global test timeout for async operations
jest.setTimeout(30000);

// Mock external services by default
jest.mock('axios');
jest.mock('winston', () => ({
  createLogger: jest.fn(() => ({
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
    debug: jest.fn()
  })),
  format: {
    combine: jest.fn(),
    timestamp: jest.fn(),
    errors: jest.fn(),
    json: jest.fn(),
    simple: jest.fn()
  },
  transports: {
    File: jest.fn(),
    Console: jest.fn()
  }
}));

// Global test utilities
global.testHelpers = {
  // Mock blockchain transaction
  mockTransaction: {
    id: 'test-tx-123',
    from: 'test-address-1',
    to: 'test-address-2',
    amount: '100.00',
    currency: 'USD',
    network: 'XRP',
    status: 'pending',
    timestamp: new Date().toISOString()
  },
  
  // Mock SWIFT message
  mockSWIFTMessage: {
    messageType: 'MT103',
    reference: 'TRN123456789',
    sender: 'TESTBIC1XXX',
    receiver: 'TESTBIC2XXX',
    amount: '1000.00',
    currency: 'USD',
    narrative: 'Test payment'
  },
  
  // Mock bank configuration
  mockBankConfig: {
    id: 'test-bank-1',
    name: 'Test Bank Ltd',
    bic: 'TESTBIC1XXX',
    country: 'SG',
    coreSystem: 'Temenos T24',
    endpoints: {
      api: 'https://api.testbank.com',
      swift: 'swift.testbank.com:9999'
    }
  },
  
  // Wait utility for async operations
  wait: (ms) => new Promise(resolve => setTimeout(resolve, ms)),
  
  // Generate random test data
  randomString: (length = 10) => Math.random().toString(36).substring(2, length + 2),
  randomAmount: () => (Math.random() * 10000).toFixed(2),
  randomCurrency: () => ['USD', 'EUR', 'GBP', 'SGD', 'JPY'][Math.floor(Math.random() * 5)]
};

// Global teardown for database connections, etc.
afterAll(async () => {
  // Clean up any open handles
  await new Promise(resolve => setTimeout(resolve, 100));
});

// Console override to reduce test noise
const originalConsole = console;
global.console = {
  ...originalConsole,
  // Suppress console.log in tests unless explicitly needed
  log: process.env.VERBOSE_TESTS ? originalConsole.log : jest.fn(),
  info: process.env.VERBOSE_TESTS ? originalConsole.info : jest.fn(),
  warn: originalConsole.warn,
  error: originalConsole.error
};

// Unhandled promise rejection handler
process.on('unhandledRejection', (reason, promise) => {
  console.error('Unhandled Rejection at:', promise, 'reason:', reason);
  // Fail the test if there are unhandled rejections
  throw reason;
});