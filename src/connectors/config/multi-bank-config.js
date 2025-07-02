/**
 * Multi-Bank Connector Configuration
 * Configuration templates for various banking systems
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Multi-Bank Architecture Configuration
 */

const { SUPPORTED_BANKS } = require('../base/connector-factory');

/**
 * Default configuration templates for different banking systems
 */
const BANK_CONFIGURATIONS = {
  [SUPPORTED_BANKS.TCS_BANCS]: {
    type: SUPPORTED_BANKS.TCS_BANCS,
    bankName: 'TCS BaNCS Core Banking',
    initialConnectors: 2,
    maxConnectors: 5,
    config: {
      baseUrl: process.env.BANCS_BASE_URL || 'https://bancs-api.tcs.com',
      authMethod: 'oauth2',
      timeout: 30000,
      retryAttempts: 3,
      enableEncryption: true,
      enableHealthChecks: true,
      institutionId: process.env.BANCS_INSTITUTION_ID,
      branchCode: process.env.BANCS_BRANCH_CODE,
      webhookEndpoint: '/webhooks/bancs'
    }
  },

  [SUPPORTED_BANKS.FINACLE]: {
    type: SUPPORTED_BANKS.FINACLE,
    bankName: 'Infosys Finacle Core Banking',
    initialConnectors: 1,
    maxConnectors: 3,
    config: {
      baseUrl: process.env.FINACLE_BASE_URL || 'https://finacle-api.infosys.com',
      authMethod: 'oauth2',
      timeout: 25000,
      retryAttempts: 3,
      enableEncryption: true,
      enableHealthChecks: true,
      serviceId: process.env.FINACLE_SERVICE_ID,
      branchCode: process.env.FINACLE_BRANCH_CODE
    }
  },

  [SUPPORTED_BANKS.FLEXCUBE]: {
    type: SUPPORTED_BANKS.FLEXCUBE,
    bankName: 'Oracle Flexcube Universal Banking',
    initialConnectors: 1,
    maxConnectors: 3,
    config: {
      baseUrl: process.env.FLEXCUBE_BASE_URL || 'https://flexcube-api.oracle.com',
      authMethod: 'mutual_tls',
      timeout: 35000,
      retryAttempts: 2,
      enableEncryption: true,
      enableHealthChecks: true,
      institutionCode: process.env.FLEXCUBE_INSTITUTION_CODE,
      branchCode: process.env.FLEXCUBE_BRANCH_CODE
    }
  },

  [SUPPORTED_BANKS.TEMENOS_T24]: {
    type: SUPPORTED_BANKS.TEMENOS_T24,
    bankName: 'Temenos T24 Core Banking',
    initialConnectors: 1,
    maxConnectors: 4,
    config: {
      baseUrl: process.env.T24_BASE_URL || 'https://t24-api.temenos.com',
      authMethod: 'oauth2',
      timeout: 30000,
      retryAttempts: 3,
      enableEncryption: true,
      enableHealthChecks: true,
      companyId: process.env.T24_COMPANY_ID,
      branchCode: process.env.T24_BRANCH_CODE
    }
  },

  [SUPPORTED_BANKS.FINASTRA_FUSION]: {
    type: SUPPORTED_BANKS.FINASTRA_FUSION,
    bankName: 'Finastra Fusion Banking',
    initialConnectors: 1,
    maxConnectors: 3,
    config: {
      baseUrl: process.env.FUSION_BASE_URL || 'https://fusion-api.finastra.com',
      authMethod: 'oauth2',
      timeout: 28000,
      retryAttempts: 3,
      enableEncryption: true,
      enableHealthChecks: true,
      tenantId: process.env.FUSION_TENANT_ID,
      organizationId: process.env.FUSION_ORG_ID
    }
  }
};

/**
 * Environment-specific configurations
 */
const ENVIRONMENT_CONFIGS = {
  development: {
    testMode: true,
    enableDetailedLogging: true,
    healthCheckInterval: 30000,
    connectionPoolSize: 5,
    maxConnectorsPerBank: 2
  },

  staging: {
    testMode: true,
    enableDetailedLogging: true,
    healthCheckInterval: 60000,
    connectionPoolSize: 8,
    maxConnectorsPerBank: 3
  },

  production: {
    testMode: false,
    enableDetailedLogging: false,
    healthCheckInterval: 60000,
    connectionPoolSize: 15,
    maxConnectorsPerBank: 5
  }
};

/**
 * Load balancing strategies configuration
 */
const LOAD_BALANCING_CONFIGS = {
  'round-robin': {
    description: 'Distribute requests evenly across all healthy connectors',
    recommended: 'High volume, uniform request patterns'
  },

  'least-connections': {
    description: 'Route to connector with fewest active connections',
    recommended: 'Variable request processing times'
  },

  'random': {
    description: 'Randomly select from healthy connectors',
    recommended: 'Simple distribution, stateless applications'
  }
};

/**
 * Get bank configuration by type
 * @param {string} bankType - Bank type from SUPPORTED_BANKS
 * @param {Object} overrides - Configuration overrides
 * @returns {Object} Bank configuration
 */
function getBankConfig(bankType, overrides = {}) {
  const baseConfig = BANK_CONFIGURATIONS[bankType];
  if (!baseConfig) {
    throw new Error(`Unsupported bank type: ${bankType}`);
  }

  return {
    ...baseConfig,
    config: {
      ...baseConfig.config,
      ...overrides
    }
  };
}

/**
 * Get environment-specific factory configuration
 * @param {string} environment - Environment name (development, staging, production)
 * @param {Object} overrides - Configuration overrides
 * @returns {Object} Factory configuration
 */
function getFactoryConfig(environment = 'development', overrides = {}) {
  const envConfig = ENVIRONMENT_CONFIGS[environment];
  if (!envConfig) {
    throw new Error(`Unsupported environment: ${environment}`);
  }

  return {
    ...envConfig,
    loadBalancingStrategy: 'round-robin',
    enableFailover: true,
    enableMetrics: true,
    ...overrides
  };
}

/**
 * Create a complete multi-bank setup configuration
 * @param {Array} banks - Array of bank configurations
 * @param {string} environment - Environment name
 * @param {Object} factoryOverrides - Factory configuration overrides
 * @returns {Object} Complete configuration
 */
function createMultiBankConfig(banks = [], environment = 'development', factoryOverrides = {}) {
  const factoryConfig = getFactoryConfig(environment, factoryOverrides);
  
  const bankConfigs = banks.map(bank => {
    if (typeof bank === 'string') {
      // If just bank type string provided, use default config
      return {
        bankCode: bank.toUpperCase().replace('-', '_'),
        ...getBankConfig(bank)
      };
    } else {
      // If object provided, merge with default config
      return {
        bankCode: bank.bankCode || bank.type.toUpperCase().replace('-', '_'),
        ...getBankConfig(bank.type, bank.config || {})
      };
    }
  });

  return {
    factory: factoryConfig,
    banks: bankConfigs,
    environment,
    createdAt: new Date().toISOString()
  };
}

/**
 * Validate bank configuration
 * @param {Object} config - Bank configuration to validate
 * @returns {Object} Validation result
 */
function validateBankConfig(config) {
  const errors = [];
  const warnings = [];

  // Required fields
  if (!config.type) errors.push('Bank type is required');
  if (!config.config?.clientId) errors.push('Client ID is required');
  if (!config.config?.clientSecret) errors.push('Client secret is required');
  if (!config.config?.baseUrl) errors.push('Base URL is required');

  // Recommended fields
  if (!config.bankCode) warnings.push('Bank code not specified, will be auto-generated');
  if (!config.config?.timeout) warnings.push('Timeout not specified, using default');
  if (!config.config?.retryAttempts) warnings.push('Retry attempts not specified, using default');

  // Security checks
  if (!config.config?.enableEncryption) {
    warnings.push('Encryption is disabled, consider enabling for production');
  }
  if (!config.config?.enableHealthChecks) {
    warnings.push('Health checks are disabled');
  }

  return {
    isValid: errors.length === 0,
    errors,
    warnings
  };
}

module.exports = {
  BANK_CONFIGURATIONS,
  ENVIRONMENT_CONFIGS,
  LOAD_BALANCING_CONFIGS,
  SUPPORTED_BANKS,
  getBankConfig,
  getFactoryConfig,
  createMultiBankConfig,
  validateBankConfig
};