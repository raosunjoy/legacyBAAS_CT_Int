/**
 * CBDC Offline Gateway Tests
 * Comprehensive test suite for offline CBDC transactions
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage for Offline-First CBDC IP Component
 */

const { CBDCOfflineGateway, CBDC_TRANSACTION_TYPES, OFFLINE_STATUS } = require('../../src/blockchain/cbdc-offline-gateway');

describe('CBDC Offline Gateway', () => {
  test('should export required constants', () => {
    expect(CBDC_TRANSACTION_TYPES).toBeDefined();
    expect(CBDC_TRANSACTION_TYPES.ISSUE).toBe('cbdc_issue');
    expect(CBDC_TRANSACTION_TYPES.TRANSFER).toBe('cbdc_transfer');
    expect(CBDC_TRANSACTION_TYPES.REDEEM).toBe('cbdc_redeem');
    
    expect(OFFLINE_STATUS).toBeDefined();
    expect(OFFLINE_STATUS.QUEUED).toBe('offline_queued');
    expect(OFFLINE_STATUS.SYNCED).toBe('synced');
    expect(OFFLINE_STATUS.FAILED).toBe('sync_failed');
  });

  test('should create gateway instance', () => {
    const gateway = new CBDCOfflineGateway({
      offlineDbPath: ':memory:',
      enableAutoSync: false
    });
    
    expect(gateway).toBeDefined();
    expect(gateway.config).toBeDefined();
    expect(gateway.config.centralBankId).toBe('CB_DEFAULT');
  });
});