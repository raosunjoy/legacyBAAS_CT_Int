/**
 * Banking System Configuration Manager
 * LegacyBAAS COBOL Transpiler Integration
 * 
 * Manages configuration loading and validation for different banking systems
 * Supports FIS Systematics, Fiserv DNA, Temenos Transact, TCS BaNCS
 */

const winston = require('winston');
const yaml = require('js-yaml');
const path = require('path');
const fs = require('fs').promises;

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'config-manager' }
});

/**
 * Supported Banking Systems
 */
const SUPPORTED_BANKING_SYSTEMS = {
  FIS_SYSTEMATICS: 'fis-systematics.yaml',
  FISERV_DNA: 'fiserv-dna.yaml',
  TCS_BANCS: 'tcs-bancs.yaml',
  TEMENOS_TRANSACT: 'temenos-transact.yaml'
};

/**
 * Configuration Manager Class
 */
class ConfigManager {
  constructor(configPath = null) {
    this.configPath = configPath || path.join(__dirname);
    this.configurations = new Map();
    this.defaultConfig = null;
    this.initialized = false;
  }

  /**
   * Initialize configuration manager
   */
  async initialize() {
    try {
      logger.info('Initializing configuration manager', {
        configPath: this.configPath
      });

      // Load all banking system configurations
      await this._loadAllConfigurations();

      // Set default configuration
      this._setDefaultConfiguration();

      this.initialized = true;
      
      logger.info('Configuration manager initialized successfully', {
        configurationsLoaded: this.configurations.size,
        supportedSystems: Object.keys(SUPPORTED_BANKING_SYSTEMS)
      });

    } catch (error) {
      logger.error('Failed to initialize configuration manager', {
        error: error.message,
        configPath: this.configPath
      });
      throw error;
    }
  }

  /**
   * Get configuration for a specific banking system
   * @param {string} bankingSystem - Banking system identifier
   * @returns {Object} Configuration object
   */
  getConfiguration(bankingSystem) {
    if (!this.initialized) {
      throw new Error('Configuration manager not initialized');
    }

    const systemKey = bankingSystem.toUpperCase();
    
    if (!this.configurations.has(systemKey)) {
      logger.warn('Banking system configuration not found, using default', {
        requestedSystem: bankingSystem,
        availableSystems: Array.from(this.configurations.keys())
      });
      return this.defaultConfig;
    }

    const config = this.configurations.get(systemKey);
    
    logger.debug('Configuration retrieved', {
      bankingSystem: systemKey,
      configVersion: config.version
    });

    return config;
  }

  /**
   * Get data type mapping for a banking system
   * @param {string} bankingSystem - Banking system identifier
   * @param {string} cobolType - COBOL data type (e.g., "PIC X", "COMP-3")
   * @returns {string} Blockchain data type
   */
  getDataTypeMapping(bankingSystem, cobolType) {
    const config = this.getConfiguration(bankingSystem);
    
    if (!config.cobol || !config.cobol.data_types) {
      logger.warn('No data type mappings found for banking system', {
        bankingSystem
      });
      return 'string'; // Default fallback
    }

    const mappedType = config.cobol.data_types[cobolType];
    if (!mappedType) {
      logger.debug('Data type mapping not found, using default', {
        bankingSystem,
        cobolType,
        fallback: 'string'
      });
      return 'string';
    }

    return mappedType;
  }

  /**
   * Get preferred blockchain networks for a banking system
   * @param {string} bankingSystem - Banking system identifier
   * @returns {Array} Array of preferred blockchain networks
   */
  getPreferredBlockchains(bankingSystem) {
    const config = this.getConfiguration(bankingSystem);
    
    if (!config.blockchain || !config.blockchain.preferred_networks) {
      logger.warn('No blockchain preferences found for banking system', {
        bankingSystem
      });
      return ['ethereum']; // Default fallback
    }

    return config.blockchain.preferred_networks;
  }

  /**
   * Get banking APIs for a system
   * @param {string} bankingSystem - Banking system identifier
   * @returns {Array} Array of banking API configurations
   */
  getBankingAPIs(bankingSystem) {
    const config = this.getConfiguration(bankingSystem);
    
    if (!config.banking_apis) {
      logger.warn('No banking APIs found for banking system', {
        bankingSystem
      });
      return [];
    }

    return config.banking_apis;
  }

  /**
   * Get compliance requirements for a banking system
   * @param {string} bankingSystem - Banking system identifier
   * @returns {Object} Compliance configuration
   */
  getComplianceRequirements(bankingSystem) {
    const config = this.getConfiguration(bankingSystem);
    
    if (!config.compliance) {
      logger.warn('No compliance requirements found for banking system', {
        bankingSystem
      });
      return {};
    }

    return config.compliance;
  }

  /**
   * Get use case template for a banking system
   * @param {string} bankingSystem - Banking system identifier
   * @param {string} useCase - Use case name
   * @returns {Object} Use case configuration
   */
  getUseCaseTemplate(bankingSystem, useCase) {
    const config = this.getConfiguration(bankingSystem);
    
    if (!config.use_cases || !config.use_cases[useCase]) {
      logger.warn('Use case template not found', {
        bankingSystem,
        useCase,
        availableUseCases: config.use_cases ? Object.keys(config.use_cases) : []
      });
      return null;
    }

    return config.use_cases[useCase];
  }

  /**
   * Get performance settings for a banking system
   * @param {string} bankingSystem - Banking system identifier
   * @returns {Object} Performance configuration
   */
  getPerformanceSettings(bankingSystem) {
    const config = this.getConfiguration(bankingSystem);
    
    return config.performance || {
      optimize_gas: true,
      batch_size: 1000,
      cache_enabled: true
    };
  }

  /**
   * Validate configuration for completeness
   * @param {Object} config - Configuration object to validate
   * @returns {boolean} True if valid, throws error if invalid
   */
  validateConfiguration(config) {
    const requiredFields = [
      'name',
      'core_type',
      'version',
      'system',
      'cobol',
      'blockchain'
    ];

    for (const field of requiredFields) {
      if (!config[field]) {
        throw new Error(`Required configuration field missing: ${field}`);
      }
    }

    // Validate COBOL configuration
    if (!config.cobol.data_types) {
      throw new Error('COBOL data_types mapping is required');
    }

    // Validate blockchain configuration
    if (!config.blockchain.preferred_networks || config.blockchain.preferred_networks.length === 0) {
      throw new Error('At least one preferred blockchain network is required');
    }

    logger.debug('Configuration validation passed', {
      configName: config.name,
      coreType: config.core_type
    });

    return true;
  }

  /**
   * Get all available banking systems
   * @returns {Array} Array of available banking system identifiers
   */
  getAvailableBankingSystems() {
    return Array.from(this.configurations.keys());
  }

  /**
   * Get configuration summary for all systems
   * @returns {Object} Summary of all configurations
   */
  getConfigurationSummary() {
    const summary = {};
    
    for (const [system, config] of this.configurations) {
      summary[system] = {
        name: config.name,
        version: config.version,
        architecture: config.system.architecture,
        preferredBlockchains: config.blockchain.preferred_networks,
        complianceModules: Object.keys(config.compliance || {}),
        supportedUseCases: Object.keys(config.use_cases || {})
      };
    }

    return summary;
  }

  /**
   * Load all configuration files
   */
  async _loadAllConfigurations() {
    const loadPromises = Object.entries(SUPPORTED_BANKING_SYSTEMS).map(
      async ([systemKey, filename]) => {
        try {
          const config = await this._loadConfigurationFile(filename);
          this.validateConfiguration(config);
          this.configurations.set(systemKey, config);
          
          logger.debug('Configuration loaded successfully', {
            system: systemKey,
            filename,
            version: config.version
          });
        } catch (error) {
          logger.error('Failed to load configuration', {
            system: systemKey,
            filename,
            error: error.message
          });
          throw error;
        }
      }
    );

    await Promise.all(loadPromises);
  }

  /**
   * Load a single configuration file
   */
  async _loadConfigurationFile(filename) {
    const filePath = path.join(this.configPath, filename);
    
    try {
      const fileContent = await fs.readFile(filePath, 'utf8');
      const config = yaml.load(fileContent);
      
      return config;
    } catch (error) {
      if (error.code === 'ENOENT') {
        throw new Error(`Configuration file not found: ${filePath}`);
      }
      throw new Error(`Failed to parse configuration file ${filename}: ${error.message}`);
    }
  }

  /**
   * Set default configuration (first available or FIS if exists)
   */
  _setDefaultConfiguration() {
    if (this.configurations.has('FIS_SYSTEMATICS')) {
      this.defaultConfig = this.configurations.get('FIS_SYSTEMATICS');
    } else {
      // Use first available configuration as default
      this.defaultConfig = this.configurations.values().next().value;
    }

    if (this.defaultConfig) {
      logger.info('Default configuration set', {
        defaultSystem: this.defaultConfig.core_type,
        name: this.defaultConfig.name
      });
    }
  }

  /**
   * Reload configurations (useful for hot-reloading)
   */
  async reload() {
    logger.info('Reloading configurations');
    
    this.configurations.clear();
    this.defaultConfig = null;
    this.initialized = false;
    
    await this.initialize();
  }

  /**
   * Get configuration by use case requirements
   * @param {string} useCase - Use case name
   * @param {Array} complianceRequirements - Required compliance modules
   * @param {string} preferredBlockchain - Preferred blockchain network
   * @returns {Object} Best matching configuration
   */
  getConfigurationByRequirements(useCase, complianceRequirements = [], preferredBlockchain = null) {
    let bestMatch = null;
    let bestScore = 0;

    for (const [systemKey, config] of this.configurations) {
      let score = 0;

      // Score based on use case availability
      if (config.use_cases && config.use_cases[useCase]) {
        score += 10;
      }

      // Score based on compliance requirements
      if (complianceRequirements.length > 0 && config.compliance) {
        const matchingCompliance = complianceRequirements.filter(
          req => config.compliance[req]
        );
        score += matchingCompliance.length * 2;
      }

      // Score based on blockchain preference
      if (preferredBlockchain && config.blockchain.preferred_networks) {
        if (config.blockchain.preferred_networks.includes(preferredBlockchain)) {
          score += 5;
        }
      }

      if (score > bestScore) {
        bestScore = score;
        bestMatch = { systemKey, config, score };
      }
    }

    if (bestMatch) {
      logger.info('Configuration selected by requirements', {
        selectedSystem: bestMatch.systemKey,
        score: bestMatch.score,
        useCase,
        complianceRequirements,
        preferredBlockchain
      });
      return bestMatch.config;
    }

    logger.warn('No configuration matched requirements, using default', {
      useCase,
      complianceRequirements,
      preferredBlockchain
    });
    return this.defaultConfig;
  }
}

module.exports = {
  ConfigManager,
  SUPPORTED_BANKING_SYSTEMS
};