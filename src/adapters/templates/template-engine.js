/**
 * Template Engine for Smart Contract Generation
 * LegacyBAAS COBOL Transpiler Integration
 * 
 * Generates blockchain-specific smart contracts from COBOL AST
 * Supports Solidity (Ethereum), Corda (Kotlin), and Hyperledger Fabric (Go)
 */

const winston = require('winston');
const path = require('path');
const fs = require('fs').promises;
const Handlebars = require('handlebars');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'template-engine' }
});

/**
 * Blockchain Platform Configurations
 */
const BLOCKCHAIN_PLATFORMS = {
  ethereum: {
    language: 'solidity',
    extension: '.sol',
    templateFile: 'solidity.hbs',
    gasOptimizations: true,
    features: ['evm_compatible', 'smart_contracts', 'defi_integration']
  },
  corda: {
    language: 'kotlin',
    extension: '.kt',
    templateFile: 'corda.hbs',
    gasOptimizations: false,
    features: ['privacy_focused', 'enterprise_ready', 'regulatory_compliant']
  },
  fabric: {
    language: 'go',
    extension: '.go',
    templateFile: 'fabric.hbs',
    gasOptimizations: false,
    features: ['permissioned', 'enterprise', 'chaincode']
  },
  algorand: {
    language: 'teal',
    extension: '.teal',
    templateFile: 'algorand.hbs',
    gasOptimizations: true,
    features: ['cbdc_ready', 'fast_settlement', 'low_cost']
  }
};

/**
 * Template Engine Class
 */
class TemplateEngine {
  constructor(config = {}) {
    this.config = {
      templatePath: config.templatePath || path.join(__dirname, 'blockchain'),
      outputPath: config.outputPath || path.join(process.cwd(), 'build', 'contracts'),
      enableOptimizations: config.enableOptimizations !== false,
      enableValidation: config.enableValidation !== false,
      ...config
    };
    
    this.templates = new Map();
    this.handlebars = Handlebars.create();
    
    // Register custom helpers
    this._registerHelpers();
    
    logger.info('TemplateEngine initialized', {
      templatePath: this.config.templatePath,
      optimizations: this.config.enableOptimizations
    });
  }

  /**
   * Initialize template engine by loading all templates
   */
  async initialize() {
    try {
      await this._loadTemplates();
      logger.info('Template engine initialized successfully', {
        templatesLoaded: this.templates.size
      });
    } catch (error) {
      logger.error('Failed to initialize template engine', { error: error.message });
      throw error;
    }
  }

  /**
   * Generate smart contract from COBOL AST
   * @param {Object} ast - COBOL Abstract Syntax Tree
   * @param {string} blockchain - Target blockchain platform
   * @param {Object} options - Generation options
   * @returns {Object} Generated contract and metadata
   */
  async generateContract(ast, blockchain, options = {}) {
    const startTime = Date.now();
    
    try {
      // Validate inputs
      this._validateInputs(ast, blockchain);
      
      // Get blockchain configuration
      const blockchainConfig = BLOCKCHAIN_PLATFORMS[blockchain.toLowerCase()];
      if (!blockchainConfig) {
        throw new Error(`Unsupported blockchain platform: ${blockchain}`);
      }

      // Prepare template context
      const context = this._prepareTemplateContext(ast, blockchainConfig, options);
      
      // Get template
      const template = this.templates.get(blockchainConfig.templateFile);
      if (!template) {
        throw new Error(`Template not found: ${blockchainConfig.templateFile}`);
      }

      // Generate contract code
      const contractCode = template(context);
      
      // Apply optimizations if enabled
      const optimizedCode = this.config.enableOptimizations ?
        this._optimizeContract(contractCode, blockchainConfig) : contractCode;

      // Validate generated contract
      if (this.config.enableValidation) {
        this._validateContract(optimizedCode, blockchainConfig);
      }

      const result = {
        contractCode: optimizedCode,
        metadata: {
          blockchain: blockchain.toLowerCase(),
          language: blockchainConfig.language,
          programId: ast.program.programId,
          generatedAt: new Date().toISOString(),
          generationTime: Date.now() - startTime,
          features: blockchainConfig.features,
          optimized: this.config.enableOptimizations,
          estimatedGas: context.estimatedGas,
          complexity: context.complexity
        },
        context: {
          variables: context.variables,
          functions: context.functions,
          bankingSystem: context.bankingSystem
        }
      };

      logger.info('Contract generated successfully', {
        programId: ast.program.programId,
        blockchain,
        generationTime: Date.now() - startTime,
        codeLength: optimizedCode.length
      });

      return result;

    } catch (error) {
      logger.error('Contract generation failed', {
        programId: ast.program?.programId || 'unknown',
        blockchain,
        error: error.message,
        generationTime: Date.now() - startTime
      });
      throw error;
    }
  }

  /**
   * Load all template files
   */
  async _loadTemplates() {
    try {
      const templateFiles = await fs.readdir(this.config.templatePath);
      
      for (const file of templateFiles) {
        if (file.endsWith('.hbs')) {
          const filePath = path.join(this.config.templatePath, file);
          const templateContent = await fs.readFile(filePath, 'utf8');
          const compiledTemplate = this.handlebars.compile(templateContent);
          this.templates.set(file, compiledTemplate);
          
          logger.debug('Template loaded', { file, path: filePath });
        }
      }
    } catch (error) {
      if (error.code === 'ENOENT') {
        logger.warn('Template directory not found, creating default templates');
        await this._createDefaultTemplates();
        await this._loadTemplates(); // Retry loading
      } else {
        throw error;
      }
    }
  }

  /**
   * Create default templates if none exist
   */
  async _createDefaultTemplates() {
    try {
      await fs.mkdir(this.config.templatePath, { recursive: true });
      
      // Create default Solidity template
      const solidityTemplate = this._getDefaultSolidityTemplate();
      await fs.writeFile(
        path.join(this.config.templatePath, 'solidity.hbs'),
        solidityTemplate
      );
    } catch (error) {
      logger.warn('Could not create template files, using in-memory templates', { error: error.message });
      
      // Use in-memory templates instead
      this.templates.set('solidity.hbs', this.handlebars.compile(this._getDefaultSolidityTemplate()));
      this.templates.set('corda.hbs', this.handlebars.compile(this._getDefaultCordaTemplate()));
      return;
    }

    // Create default Corda template
    const cordaTemplate = this._getDefaultCordaTemplate();
    await fs.writeFile(
      path.join(this.config.templatePath, 'corda.hbs'),
      cordaTemplate
    );

    logger.info('Default templates created');
  }

  /**
   * Prepare template context from AST
   */
  _prepareTemplateContext(ast, blockchainConfig, options) {
    const context = {
      // Basic contract information
      contractName: this._generateContractName(ast.program.programId),
      programId: ast.program.programId,
      author: ast.program.author,
      dateWritten: ast.program.dateWritten,
      generatedAt: new Date().toISOString(),
      
      // Blockchain specific
      blockchain: blockchainConfig,
      language: blockchainConfig.language,
      
      // Variables converted to blockchain types
      variables: this._convertVariables(ast.data.variables, blockchainConfig),
      
      // Functions converted from COBOL procedures
      functions: this._convertProcedures(ast.procedure.operations, blockchainConfig),
      
      // Banking system context
      bankingSystem: options.bankingSystem || 'GENERIC',
      
      // Performance estimates
      complexity: ast.procedure.complexity,
      estimatedGas: ast.procedure.estimatedGas,
      
      // Options
      optimizeGas: this.config.enableOptimizations && blockchainConfig.gasOptimizations,
      enableEvents: options.enableEvents !== false,
      enableModifiers: options.enableModifiers !== false
    };

    return context;
  }

  /**
   * Convert COBOL variables to blockchain variables
   */
  _convertVariables(cobolVariables, blockchainConfig) {
    return cobolVariables.map(variable => ({
      name: variable.name,
      originalName: variable.originalName,
      type: this._getBlockchainType(variable.type, blockchainConfig),
      picture: variable.picture,
      value: variable.value,
      level: variable.level,
      visibility: variable.level === 1 ? 'public' : 'private',
      isConstant: !!variable.value
    }));
  }

  /**
   * Convert COBOL procedures to blockchain functions
   */
  _convertProcedures(operations, blockchainConfig) {
    const functions = [];
    
    // Group operations by type and create appropriate functions
    const computeOps = operations.filter(op => op.type === 'compute');
    const conditionalOps = operations.filter(op => op.type === 'conditional');
    
    // Create main execute function
    if (computeOps.length > 0 || conditionalOps.length > 0) {
      functions.push({
        name: 'execute',
        visibility: 'public',
        returns: this._getReturnType(computeOps, blockchainConfig),
        operations: operations,
        isPure: this._isPureFunction(operations),
        isView: this._isViewFunction(operations)
      });
    }

    // Create specialized functions for complex operations
    computeOps.forEach((op, index) => {
      if (this._isComplexComputation(op)) {
        functions.push({
          name: `compute${index + 1}`,
          visibility: 'private',
          returns: 'uint256',
          operation: op,
          isPure: true,
          isView: false
        });
      }
    });

    return functions;
  }

  /**
   * Get blockchain-specific data type
   */
  _getBlockchainType(cobolType, blockchainConfig) {
    const typeMap = {
      ethereum: {
        'string': 'string',
        'uint256': 'uint256',
        'uint128': 'uint128',
        'int256': 'int256',
        'bytes': 'bytes'
      },
      corda: {
        'string': 'String',
        'uint256': 'BigInteger',
        'uint128': 'Long',
        'int256': 'BigInteger',
        'bytes': 'ByteArray'
      },
      algorand: {
        'string': 'byte[]',
        'uint256': 'uint64',
        'uint128': 'uint64',
        'int256': 'int64',
        'bytes': 'byte[]'
      }
    };

    return typeMap[blockchainConfig.language]?.[cobolType] || cobolType;
  }

  /**
   * Generate appropriate contract name
   */
  _generateContractName(programId) {
    return programId
      .replace(/[^A-Z0-9-]/g, '')
      .split(/[-_]/)
      .map(word => word.charAt(0).toUpperCase() + word.slice(1).toLowerCase())
      .join('');
  }

  /**
   * Determine return type for functions
   */
  _getReturnType(computeOps, blockchainConfig) {
    if (computeOps.length === 0) return 'bool';
    
    // Analyze the expression to determine best return type
    const hasDecimals = computeOps.some(op => op.expression.includes('.'));
    if (hasDecimals && blockchainConfig.language === 'solidity') {
      return 'uint256'; // Use uint256 for decimal calculations in Solidity
    }
    
    return 'uint256';
  }

  /**
   * Check if function is pure (no state changes)
   */
  _isPureFunction(operations) {
    return operations.every(op => op.type === 'compute');
  }

  /**
   * Check if function is view (reads state but doesn't modify)
   */
  _isViewFunction(operations) {
    return operations.some(op => op.type === 'move' || op.type === 'perform');
  }

  /**
   * Check if computation is complex enough for separate function
   */
  _isComplexComputation(operation) {
    return operation.expression.split(/[+\-*/()]/).length > 5;
  }

  /**
   * Optimize generated contract code
   */
  _optimizeContract(contractCode, blockchainConfig) {
    let optimized = contractCode;

    if (blockchainConfig.gasOptimizations) {
      // Gas optimization strategies
      optimized = optimized
        .replace(/uint256/g, 'uint128') // Use smaller types when possible
        .replace(/\s+/g, ' ') // Minimize whitespace
        .trim();
    }

    return optimized;
  }

  /**
   * Validate generated contract syntax
   */
  _validateContract(contractCode, blockchainConfig) {
    // Basic syntax validation based on language
    switch (blockchainConfig.language) {
      case 'solidity':
        if (!contractCode.includes('pragma solidity')) {
          throw new Error('Invalid Solidity contract: Missing pragma directive');
        }
        if (!contractCode.includes('contract ')) {
          throw new Error('Invalid Solidity contract: Missing contract declaration');
        }
        break;
      case 'kotlin':
        if (!contractCode.includes('class ') && !contractCode.includes('data class ')) {
          throw new Error('Invalid Kotlin contract: Missing class declaration');
        }
        break;
    }
  }

  /**
   * Validate inputs
   */
  _validateInputs(ast, blockchain) {
    if (!ast || typeof ast !== 'object') {
      throw new Error('Invalid AST provided');
    }
    
    if (!ast.program?.programId) {
      throw new Error('AST missing program ID');
    }
    
    if (!blockchain || typeof blockchain !== 'string') {
      throw new Error('Invalid blockchain platform specified');
    }
  }

  /**
   * Register Handlebars helpers
   */
  _registerHelpers() {
    // Helper for capitalizing first letter
    this.handlebars.registerHelper('capitalize', (str) => {
      return str.charAt(0).toUpperCase() + str.slice(1);
    });

    // Helper for converting to lowercase
    this.handlebars.registerHelper('lowercase', (str) => {
      return str.toLowerCase();
    });

    // Helper for converting to camelCase
    this.handlebars.registerHelper('camelCase', (str) => {
      return str.replace(/_([a-z])/g, (match, letter) => letter.toUpperCase());
    });

    // Helper for conditional rendering
    this.handlebars.registerHelper('if_eq', function(a, b, options) {
      return a === b ? options.fn(this) : options.inverse(this);
    });

    // Helper for loop indexing
    this.handlebars.registerHelper('inc', (value) => {
      return parseInt(value) + 1;
    });
  }

  /**
   * Get default Solidity template
   */
  _getDefaultSolidityTemplate() {
    return `// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * {{contractName}} - Generated from COBOL Program {{programId}}
 * Original Author: {{author}}
 * Generated: {{generatedAt}}
 * Banking System: {{bankingSystem}}
 */
contract {{contractName}} {
    {{#each variables}}
    {{type}} {{#if visibility}}{{visibility}}{{else}}public{{/if}} {{name}}{{#if value}} = {{value}}{{/if}};
    {{/each}}

    {{#if enableEvents}}
    event Executed(uint256 result, address indexed caller);
    {{/if}}

    {{#each functions}}
    function {{name}}({{#each parameters}}{{type}} {{name}}{{#unless @last}}, {{/unless}}{{/each}}) {{visibility}}{{#if isPure}} pure{{/if}}{{#if isView}} view{{/if}} returns ({{returns}}) {
        {{#each operations}}
        {{#if_eq type 'compute'}}
        return {{expression}};
        {{/if_eq}}
        {{#if_eq type 'conditional'}}
        if ({{condition}}) {
            // {{action}}
        }
        {{/if_eq}}
        {{/each}}
    }
    {{/each}}
}`;
  }

  /**
   * Get default Corda template
   */
  _getDefaultCordaTemplate() {
    return `/**
 * {{contractName}} - Generated from COBOL Program {{programId}}
 * Original Author: {{author}}
 * Generated: {{generatedAt}}
 * Banking System: {{bankingSystem}}
 */

@BelongsToContract({{contractName}}Contract::class)
data class {{contractName}}State(
    {{#each variables}}
    val {{name}}: {{type}}{{#unless @last}},{{/unless}}
    {{/each}}
) : ContractState {
    override val participants: List<AbstractParty> = listOf()

    {{#each functions}}
    fun {{name}}(): {{returns}} {
        {{#each operations}}
        {{#if_eq type 'compute'}}
        return {{expression}}
        {{/if_eq}}
        {{/each}}
    }
    {{/each}}
}`;
  }
}

module.exports = {
  TemplateEngine,
  BLOCKCHAIN_PLATFORMS
};