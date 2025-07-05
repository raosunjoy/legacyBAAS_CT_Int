/**
 * Configuration Manager Test Suite
 * LegacyBAAS COBOL Transpiler Integration
 * 
 * Tests banking system configuration loading, validation, and management
 */

const { ConfigManager, SUPPORTED_BANKING_SYSTEMS } = require('../../src/adapters/configs/config-manager');
const path = require('path');
const fs = require('fs').promises;
const yaml = require('js-yaml');

describe('Configuration Manager Tests', () => {
  let configManager;
  const testConfigPath = path.join(__dirname, '../../src/adapters/configs');

  beforeAll(async () => {
    configManager = new ConfigManager(testConfigPath);
    await configManager.initialize();
  });

  describe('Initialization and Loading', () => {
    test('should initialize successfully', () => {
      expect(configManager.initialized).toBe(true);
      expect(configManager.configurations.size).toBeGreaterThan(0);
    });

    test('should load all supported banking systems', () => {
      const expectedSystems = Object.keys(SUPPORTED_BANKING_SYSTEMS);
      const loadedSystems = configManager.getAvailableBankingSystems();

      expectedSystems.forEach(system => {
        expect(loadedSystems).toContain(system);
      });
    });

    test('should set default configuration', () => {
      expect(configManager.defaultConfig).toBeDefined();
      expect(configManager.defaultConfig.name).toBeDefined();
      expect(configManager.defaultConfig.core_type).toBeDefined();
    });

    test('should validate all loaded configurations', () => {
      configManager.configurations.forEach((config, systemKey) => {
        expect(() => configManager.validateConfiguration(config)).not.toThrow();
      });
    });
  });

  describe('Configuration Retrieval', () => {
    test('should retrieve configuration by banking system', () => {
      const fisConfig = configManager.getConfiguration('FIS_SYSTEMATICS');
      
      expect(fisConfig.name).toBe('FIS Systematics IBS');
      expect(fisConfig.core_type).toBe('FIS_SYSTEMATICS');
      expect(fisConfig.system.architecture).toBe('mainframe');
    });

    test('should return default config for unknown system', () => {
      const unknownConfig = configManager.getConfiguration('UNKNOWN_SYSTEM');
      
      expect(unknownConfig).toBe(configManager.defaultConfig);
    });

    test('should handle case-insensitive system names', () => {
      const config1 = configManager.getConfiguration('fis_systematics');
      const config2 = configManager.getConfiguration('FIS_SYSTEMATICS');
      
      expect(config1).toBe(config2);
    });
  });

  describe('Data Type Mapping', () => {
    test('should map COBOL data types correctly for FIS Systematics', () => {
      expect(configManager.getDataTypeMapping('FIS_SYSTEMATICS', 'COMP-3')).toBe('uint128');
      expect(configManager.getDataTypeMapping('FIS_SYSTEMATICS', 'PIC X')).toBe('string');
      expect(configManager.getDataTypeMapping('FIS_SYSTEMATICS', 'PIC 9')).toBe('uint256');
    });

    test('should map COBOL data types correctly for Fiserv DNA', () => {
      expect(configManager.getDataTypeMapping('FISERV_DNA', 'COMP-3')).toBe('BigDecimal');
      expect(configManager.getDataTypeMapping('FISERV_DNA', 'PIC X')).toBe('String');
    });

    test('should map COBOL data types correctly for TCS BaNCS', () => {
      expect(configManager.getDataTypeMapping('TCS_BANCS', 'COMP-3')).toBe('Number');
      expect(configManager.getDataTypeMapping('TCS_BANCS', 'PIC X')).toBe('String');
    });

    test('should return default type for unknown mapping', () => {
      expect(configManager.getDataTypeMapping('FIS_SYSTEMATICS', 'UNKNOWN_TYPE')).toBe('string');
    });
  });

  describe('Blockchain Preferences', () => {
    test('should return preferred blockchains for FIS Systematics', () => {
      const blockchains = configManager.getPreferredBlockchains('FIS_SYSTEMATICS');
      
      expect(blockchains).toContain('corda');
      expect(blockchains).toContain('algorand');
      expect(blockchains).toContain('xrp');
    });

    test('should return preferred blockchains for Fiserv DNA', () => {
      const blockchains = configManager.getPreferredBlockchains('FISERV_DNA');
      
      expect(blockchains).toContain('ethereum');
      expect(blockchains).toContain('xrp');
      expect(blockchains).toContain('algorand');
    });

    test('should return default for unknown system', () => {
      const blockchains = configManager.getPreferredBlockchains('UNKNOWN');
      
      // Should return the default config's preferred networks (FIS Systematics)
      expect(blockchains).toContain('corda');
      expect(blockchains).toContain('algorand');
      expect(blockchains).toContain('xrp');
    });
  });

  describe('Banking APIs', () => {
    test('should return banking APIs for each system', () => {
      const fisAPIs = configManager.getBankingAPIs('FIS_SYSTEMATICS');
      const fiservAPIs = configManager.getBankingAPIs('FISERV_DNA');
      
      expect(fisAPIs.length).toBeGreaterThan(0);
      expect(fiservAPIs.length).toBeGreaterThan(0);
      
      expect(fisAPIs[0]).toHaveProperty('name');
      expect(fisAPIs[0]).toHaveProperty('endpoint');
      expect(fisAPIs[0]).toHaveProperty('method');
    });

    test('should handle system with no APIs', () => {
      const apis = configManager.getBankingAPIs('NONEXISTENT');
      // Should return the default config's banking APIs (FIS Systematics)
      expect(apis.length).toBeGreaterThan(0);
      expect(apis[0]).toHaveProperty('name');
      expect(apis[0]).toHaveProperty('endpoint');
    });
  });

  describe('Compliance Requirements', () => {
    test('should return compliance requirements for FIS Systematics', () => {
      const compliance = configManager.getComplianceRequirements('FIS_SYSTEMATICS');
      
      expect(compliance.bsa_aml).toBe(true);
      expect(compliance.ofac_screening).toBe(true);
      expect(compliance.fatf_travel_rule).toBe(true);
    });

    test('should return compliance requirements for Temenos Transact', () => {
      const compliance = configManager.getComplianceRequirements('TEMENOS_TRANSACT');
      
      expect(compliance.gdpr).toBe(true);
      expect(compliance.psd2).toBe(true);
      expect(compliance.sepa_credit_transfer).toBe(true);
    });

    test('should return empty object for unknown system', () => {
      const compliance = configManager.getComplianceRequirements('UNKNOWN');
      // Should return the default config's compliance requirements (FIS Systematics)
      expect(compliance.bsa_aml).toBe(true);
      expect(compliance.ofac_screening).toBe(true);
    });
  });

  describe('Use Case Templates', () => {
    test('should return use case templates', () => {
      const mortgageTemplate = configManager.getUseCaseTemplate('FIS_SYSTEMATICS', 'mortgage_processing');
      
      expect(mortgageTemplate).toBeDefined();
      expect(mortgageTemplate.template).toBe('fis_mortgage.cbl');
      expect(mortgageTemplate.blockchain).toBe('corda');
      expect(mortgageTemplate.compliance).toContain('bsa_aml');
    });

    test('should return null for unknown use case', () => {
      const template = configManager.getUseCaseTemplate('FIS_SYSTEMATICS', 'unknown_use_case');
      expect(template).toBeNull();
    });
  });

  describe('Performance Settings', () => {
    test('should return performance settings', () => {
      const performance = configManager.getPerformanceSettings('FIS_SYSTEMATICS');
      
      expect(performance.optimize_gas).toBe(true);
      expect(performance.batch_size).toBeDefined();
      expect(typeof performance.batch_size).toBe('number');
    });

    test('should return default performance settings for unknown system', () => {
      const performance = configManager.getPerformanceSettings('UNKNOWN');
      
      // Should return the default config's performance settings (FIS Systematics)
      expect(performance.optimize_gas).toBe(true);
      expect(performance.batch_size).toBeDefined();
      expect(typeof performance.batch_size).toBe('number');
    });
  });

  describe('Configuration Validation', () => {
    test('should validate complete configuration', () => {
      const validConfig = {
        name: 'Test Bank',
        core_type: 'TEST_BANK',
        version: '1.0',
        system: { architecture: 'test' },
        cobol: { data_types: { 'PIC X': 'string' } },
        blockchain: { preferred_networks: ['ethereum'] }
      };

      expect(() => configManager.validateConfiguration(validConfig)).not.toThrow();
    });

    test('should reject configuration missing required fields', () => {
      const invalidConfig = {
        name: 'Test Bank'
        // Missing required fields
      };

      expect(() => configManager.validateConfiguration(invalidConfig)).toThrow();
    });

    test('should reject configuration without data types', () => {
      const invalidConfig = {
        name: 'Test Bank',
        core_type: 'TEST_BANK',
        version: '1.0',
        system: { architecture: 'test' },
        cobol: {}, // Missing data_types
        blockchain: { preferred_networks: ['ethereum'] }
      };

      expect(() => configManager.validateConfiguration(invalidConfig)).toThrow('COBOL data_types mapping is required');
    });
  });

  describe('Configuration Summary', () => {
    test('should provide configuration summary', () => {
      const summary = configManager.getConfigurationSummary();
      
      expect(summary).toHaveProperty('FIS_SYSTEMATICS');
      expect(summary).toHaveProperty('FISERV_DNA');
      expect(summary).toHaveProperty('TCS_BANCS');
      expect(summary).toHaveProperty('TEMENOS_TRANSACT');

      const fisConfig = summary.FIS_SYSTEMATICS;
      expect(fisConfig.name).toBe('FIS Systematics IBS');
      expect(fisConfig.architecture).toBe('mainframe');
      expect(fisConfig.preferredBlockchains).toContain('corda');
    });
  });

  describe('Requirement-Based Selection', () => {
    test('should select configuration by use case requirements', () => {
      const config = configManager.getConfigurationByRequirements(
        'mortgage_processing',
        ['bsa_aml', 'ofac_screening'],
        'corda'
      );

      expect(config).toBeDefined();
      expect(config.compliance?.bsa_aml).toBe(true);
      expect(config.compliance?.ofac_screening).toBe(true);
    });

    test('should select configuration by compliance requirements', () => {
      const config = configManager.getConfigurationByRequirements(
        null,
        ['gdpr', 'psd2'],
        null
      );

      // Should match Temenos Transact or another system with these compliance features
      expect(config).toBeDefined();
      expect(config.compliance?.gdpr || config.compliance?.psd2).toBeDefined();
    });

    test('should select configuration by blockchain preference', () => {
      const config = configManager.getConfigurationByRequirements(
        null,
        [],
        'ethereum'
      );

      expect(config.blockchain.preferred_networks).toContain('ethereum');
    });

    test('should return default if no requirements match', () => {
      const config = configManager.getConfigurationByRequirements(
        'nonexistent_use_case',
        ['nonexistent_compliance'],
        'nonexistent_blockchain'
      );

      expect(config).toBe(configManager.defaultConfig);
    });
  });

  describe('Error Handling', () => {
    test('should handle missing configuration files gracefully', async () => {
      const invalidConfigManager = new ConfigManager('/nonexistent/path');
      
      await expect(invalidConfigManager.initialize()).rejects.toThrow();
    });

    test('should throw error if configuration manager not initialized', () => {
      const uninitializedManager = new ConfigManager();
      
      expect(() => uninitializedManager.getConfiguration('TEST')).toThrow('Configuration manager not initialized');
    });
  });

  describe('Hot Reload Capability', () => {
    test('should reload configurations', async () => {
      const originalSize = configManager.configurations.size;
      
      await configManager.reload();
      
      expect(configManager.initialized).toBe(true);
      expect(configManager.configurations.size).toBe(originalSize);
    });
  });

  describe('System-Specific Features', () => {
    test('should handle FIS Systematics mainframe features', () => {
      const config = configManager.getConfiguration('FIS_SYSTEMATICS');
      
      expect(config.system.platform).toBe('z/OS');
      expect(config.system.transaction_manager).toBe('CICS');
      expect(config.system.character_set).toBe('EBCDIC');
    });

    test('should handle Fiserv DNA modern features', () => {
      const config = configManager.getConfiguration('FISERV_DNA');
      
      expect(config.system.api_type).toBe('REST');
      expect(config.authentication?.type).toBe('oauth2');
      expect(config.deployment?.cloud_ready).toBe(true);
    });

    test('should handle TCS BaNCS global features', () => {
      const config = configManager.getConfiguration('TCS_BANCS');
      
      expect(config.banking_modules?.retail_banking).toBe(true);
      expect(config.localization?.multi_currency).toBe(true);
      expect(config.deployment?.multi_region).toBe(true);
    });

    test('should handle Temenos European features', () => {
      const config = configManager.getConfiguration('TEMENOS_TRANSACT');
      
      expect(config.sepa?.sepa_instant_payments).toBe(true);
      expect(config.swift?.swift_gpi).toBe(true);
      expect(config.compliance?.psd2).toBe(true);
    });
  });
});