/**
 * COBOL Transpiler API Routes
 * LegacyBAAS Platform Integration
 * 
 * Provides RESTful endpoints for COBOL transpilation services
 * Extends existing banking API with modernization capabilities
 */

const express = require('express');
const router = express.Router();
const { CobolTranspiler } = require('../../adapters/cobol-transpiler');
const { ConfigManager } = require('../../adapters/configs/config-manager');
const { AuthManager, PERMISSIONS } = require('../../auth/auth-middleware');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'cobol-api' }
});

// Initialize managers
const configManager = new ConfigManager();
const authManager = new AuthManager();

// Initialize transpiler instance
let transpiler;

/**
 * Initialize COBOL transpiler with default configuration
 */
async function initializeTranspiler() {
  if (!transpiler) {
    try {
      await configManager.initialize();
      transpiler = new CobolTranspiler({
        bankingSystem: 'FIS_SYSTEMATICS',
        targetBlockchain: 'ethereum',
        optimizeGas: true,
        enableValidation: true,
        auth: {
          jwtSecret: process.env.JWT_SECRET || 'legacy-baas-secret'
        }
      });
      logger.info('COBOL transpiler initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize COBOL transpiler', { error: error.message });
      throw error;
    }
  }
}

/**
 * POST /api/v1/cobol/transpile
 * Main transpilation endpoint - converts COBOL to smart contracts
 */
router.post('/transpile', authManager.requireCobolAccess(PERMISSIONS.COBOL_TRANSPILE), async (req, res) => {
  const requestId = uuidv4();
  const startTime = Date.now();
  
  try {
    logger.info('COBOL transpilation request received', {
      requestId,
      userId: req.user.userId,
      customerId: req.user.customerId,
      bankingSystem: req.body.bankingSystem,
      targetBlockchain: req.body.targetBlockchain
    });

    // Validate request body
    if (!req.body.cobolCode) {
      return res.status(400).json({
        error: 'VALIDATION_ERROR',
        message: 'COBOL code is required',
        requestId
      });
    }

    // Initialize transpiler if needed
    await initializeTranspiler();

    // Configure transpiler for specific request
    const transpilerConfig = {
      bankingSystem: req.body.bankingSystem || 'FIS_SYSTEMATICS',
      targetBlockchain: req.body.targetBlockchain || 'ethereum',
      optimizeGas: req.body.optimizeGas !== false,
      enableValidation: req.body.enableValidation !== false
    };

    // Create request-specific transpiler instance
    const requestTranspiler = new CobolTranspiler({
      ...transpilerConfig,
      auth: { jwtSecret: process.env.JWT_SECRET || 'legacy-baas-secret' }
    });

    // Parse COBOL to AST
    const ast = await requestTranspiler.parse(req.body.cobolCode);

    // Generate smart contract
    const contractResult = await requestTranspiler.generateContract(ast, {
      blockchain: transpilerConfig.targetBlockchain,
      enableEvents: req.body.enableEvents !== false,
      enableModifiers: req.body.enableModifiers !== false,
      complianceChecks: req.body.complianceChecks !== false
    });

    // Prepare response
    const response = {
      requestId,
      success: true,
      transpilation: {
        ast: {
          id: ast.id,
          programId: ast.program.programId,
          author: ast.program.author,
          variableCount: ast.data.variables.length,
          operationCount: ast.procedure.operations.length,
          complexity: ast.procedure.complexity,
          estimatedGas: ast.procedure.estimatedGas
        },
        smartContract: {
          blockchain: contractResult.metadata.blockchain,
          language: contractResult.metadata.language,
          contractCode: contractResult.contractCode,
          optimized: contractResult.metadata.optimized,
          features: contractResult.metadata.features
        },
        configuration: {
          bankingSystem: transpilerConfig.bankingSystem,
          targetBlockchain: transpilerConfig.targetBlockchain
        },
        metadata: {
          transpilationTime: Date.now() - startTime,
          generationTime: contractResult.metadata.generationTime,
          timestamp: new Date().toISOString()
        }
      },
      links: {
        status: `/api/v1/cobol/transpile/${requestId}/status`,
        download: `/api/v1/cobol/transpile/${requestId}/download`,
        deploy: `/api/v1/cobol/transpile/${requestId}/deploy`
      }
    };

    logger.info('COBOL transpilation completed successfully', {
      requestId,
      userId: req.user.userId,
      programId: ast.program.programId,
      totalTime: Date.now() - startTime,
      contractLength: contractResult.contractCode.length
    });

    res.status(200).json(response);

  } catch (error) {
    logger.error('COBOL transpilation failed', {
      requestId,
      userId: req.user?.userId,
      error: error.message,
      totalTime: Date.now() - startTime
    });

    res.status(500).json({
      error: 'TRANSPILATION_ERROR',
      message: error.message,
      requestId,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * POST /api/v1/cobol/parse
 * Parse COBOL to AST only (no smart contract generation)
 */
router.post('/parse', authManager.requirePermissions(PERMISSIONS.COBOL_VIEW), async (req, res) => {
  const requestId = uuidv4();
  const startTime = Date.now();

  try {
    logger.info('COBOL parsing request received', {
      requestId,
      userId: req.user.userId,
      bankingSystem: req.body.bankingSystem
    });

    if (!req.body.cobolCode) {
      return res.status(400).json({
        error: 'VALIDATION_ERROR',
        message: 'COBOL code is required',
        requestId
      });
    }

    await initializeTranspiler();

    const requestTranspiler = new CobolTranspiler({
      bankingSystem: req.body.bankingSystem || 'FIS_SYSTEMATICS',
      enableValidation: req.body.enableValidation !== false
    });

    const ast = await requestTranspiler.parse(req.body.cobolCode);

    const response = {
      requestId,
      success: true,
      parsing: {
        ast: {
          id: ast.id,
          program: ast.program,
          data: {
            variables: ast.data.variables,
            sections: ast.data.sections
          },
          procedure: {
            operations: ast.procedure.operations,
            complexity: ast.procedure.complexity,
            estimatedGas: ast.procedure.estimatedGas
          },
          metadata: ast.metadata
        },
        statistics: {
          variableCount: ast.data.variables.length,
          operationCount: ast.procedure.operations.length,
          complexity: ast.procedure.complexity,
          parseTime: Date.now() - startTime
        }
      },
      timestamp: new Date().toISOString()
    };

    logger.info('COBOL parsing completed successfully', {
      requestId,
      userId: req.user.userId,
      programId: ast.program.programId,
      parseTime: Date.now() - startTime
    });

    res.status(200).json(response);

  } catch (error) {
    logger.error('COBOL parsing failed', {
      requestId,
      userId: req.user?.userId,
      error: error.message,
      parseTime: Date.now() - startTime
    });

    res.status(500).json({
      error: 'PARSING_ERROR',
      message: error.message,
      requestId,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * GET /api/v1/cobol/templates
 * List available smart contract templates
 */
router.get('/templates', authManager.requirePermissions(PERMISSIONS.COBOL_VIEW), async (req, res) => {
  try {
    await initializeTranspiler();

    const blockchainFilter = req.query.blockchain;
    const bankingSystemFilter = req.query.bankingSystem;

    const templates = {
      blockchain: {
        ethereum: {
          name: 'Ethereum (Solidity)',
          language: 'solidity',
          features: ['events', 'modifiers', 'inheritance', 'interfaces'],
          gasOptimization: true,
          complianceSupport: true
        },
        corda: {
          name: 'R3 Corda',
          language: 'kotlin',
          features: ['privacy', 'notary', 'flows', 'states'],
          gasOptimization: false,
          complianceSupport: true
        },
        algorand: {
          name: 'Algorand',
          language: 'teal',
          features: ['low_cost', 'atomic_swaps', 'stateful', 'stateless'],
          gasOptimization: true,
          complianceSupport: true
        }
      },
      bankingSystems: configManager.getAvailableBankingSystems().map(system => ({
        id: system,
        name: configManager.getConfiguration(system).name,
        preferredBlockchains: configManager.getPreferredBlockchains(system),
        complianceRequirements: configManager.getComplianceRequirements(system)
      })),
      useCases: {
        mortgage_processing: {
          name: 'Mortgage Processing',
          supportedSystems: ['FIS_SYSTEMATICS', 'FISERV_DNA'],
          recommendedBlockchain: 'corda',
          complianceRequirements: ['bsa_aml', 'ofac_screening']
        },
        trade_finance: {
          name: 'Trade Finance',
          supportedSystems: ['TEMENOS_TRANSACT', 'TCS_BANCS'],
          recommendedBlockchain: 'ethereum',
          complianceRequirements: ['fatf_travel_rule', 'sanctions_screening']
        },
        payment_processing: {
          name: 'Payment Processing',
          supportedSystems: ['FISERV_DNA', 'TCS_BANCS'],
          recommendedBlockchain: 'algorand',
          complianceRequirements: ['bsa_aml', 'pci_dss']
        }
      }
    };

    // Apply filters
    let filteredTemplates = templates;
    if (blockchainFilter && templates.blockchain[blockchainFilter]) {
      filteredTemplates.blockchain = {
        [blockchainFilter]: templates.blockchain[blockchainFilter]
      };
    }

    if (bankingSystemFilter) {
      filteredTemplates.bankingSystems = templates.bankingSystems.filter(
        system => system.id === bankingSystemFilter
      );
    }

    res.json({
      success: true,
      templates: filteredTemplates,
      timestamp: new Date().toISOString()
    });

  } catch (error) {
    logger.error('Template listing failed', { error: error.message });
    res.status(500).json({
      error: 'TEMPLATE_ERROR',
      message: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * POST /api/v1/cobol/validate
 * Validate COBOL syntax without transpilation
 */
router.post('/validate', authManager.requirePermissions(PERMISSIONS.COBOL_VIEW), async (req, res) => {
  const requestId = uuidv4();
  const startTime = Date.now();

  try {
    if (!req.body.cobolCode) {
      return res.status(400).json({
        error: 'VALIDATION_ERROR',
        message: 'COBOL code is required',
        requestId
      });
    }

    await initializeTranspiler();

    const requestTranspiler = new CobolTranspiler({
      bankingSystem: req.body.bankingSystem || 'FIS_SYSTEMATICS',
      enableValidation: true
    });

    // Attempt to parse - validation happens during parsing
    const ast = await requestTranspiler.parse(req.body.cobolCode);

    const response = {
      requestId,
      success: true,
      validation: {
        valid: true,
        programId: ast.program.programId,
        warnings: [],
        statistics: {
          variableCount: ast.data.variables.length,
          operationCount: ast.procedure.operations.length,
          complexity: ast.procedure.complexity
        }
      },
      validationTime: Date.now() - startTime,
      timestamp: new Date().toISOString()
    };

    // Add warnings for potential issues
    if (ast.data.variables.length === 0) {
      response.validation.warnings.push('No variables found in COBOL program');
    }

    if (ast.procedure.operations.length === 0) {
      response.validation.warnings.push('No procedure operations found');
    }

    res.json(response);

  } catch (error) {
    // Return validation error details
    res.status(422).json({
      requestId,
      success: false,
      validation: {
        valid: false,
        error: error.message,
        line: null // Would need enhanced parser for line numbers
      },
      validationTime: Date.now() - startTime,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * GET /api/v1/cobol/systems
 * List supported banking systems
 */
router.get('/systems', authManager.requirePermissions(PERMISSIONS.COBOL_VIEW), async (req, res) => {
  try {
    await initializeTranspiler();

    const systems = configManager.getAvailableBankingSystems().map(systemId => {
      const config = configManager.getConfiguration(systemId);
      return {
        id: systemId,
        name: config.name,
        version: config.version,
        architecture: config.system?.architecture,
        platform: config.system?.platform,
        preferredBlockchains: configManager.getPreferredBlockchains(systemId),
        supportedDataTypes: Object.keys(config.cobol?.data_types || {}),
        complianceRequirements: configManager.getComplianceRequirements(systemId),
        features: config.system?.features || [],
        apis: configManager.getBankingAPIs(systemId)
      };
    });

    res.json({
      success: true,
      systems,
      totalCount: systems.length,
      timestamp: new Date().toISOString()
    });

  } catch (error) {
    logger.error('Banking systems listing failed', { error: error.message });
    res.status(500).json({
      error: 'SYSTEMS_ERROR',
      message: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * GET /api/v1/cobol/metrics
 * Get transpiler metrics and statistics
 */
router.get('/metrics', authManager.requirePermissions(PERMISSIONS.ANALYTICS_VIEW), async (req, res) => {
  try {
    await initializeTranspiler();

    const metrics = transpiler.getMetrics();
    const configSummary = configManager.getConfigurationSummary();

    const response = {
      success: true,
      metrics: {
        transpiler: {
          totalTranspilations: metrics.totalTranspilations,
          successfulTranspilations: metrics.successfulTranspilations,
          failedTranspilations: metrics.failedTranspilations,
          successRate: metrics.successRate,
          averageTranspilationTime: metrics.averageTranspilationTime,
          lastTranspilationTime: metrics.lastTranspilationTime
        },
        authentication: {
          totalAttempts: metrics.authenticationAttempts,
          failures: metrics.authenticationFailures,
          successRate: metrics.authSuccessRate
        },
        configuration: {
          supportedSystems: Object.keys(configSummary).length,
          availableBlockchains: ['ethereum', 'corda', 'algorand'],
          systemDetails: configSummary
        },
        platform: {
          uptime: process.uptime(),
          memory: process.memoryUsage(),
          version: process.env.npm_package_version || '1.0.0'
        }
      },
      timestamp: new Date().toISOString()
    };

    res.json(response);

  } catch (error) {
    logger.error('Metrics retrieval failed', { error: error.message });
    res.status(500).json({
      error: 'METRICS_ERROR',
      message: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

/**
 * GET /api/v1/cobol/health
 * Health check for COBOL transpiler service
 */
router.get('/health', async (req, res) => {
  try {
    await initializeTranspiler();

    const health = {
      status: 'healthy',
      components: {
        transpiler: 'operational',
        configManager: configManager.initialized ? 'operational' : 'initializing',
        authManager: 'operational',
        templates: 'operational'
      },
      capabilities: {
        parsing: true,
        transpilation: true,
        validation: true,
        multiBlockchain: true,
        multiBankingSystem: true
      },
      timestamp: new Date().toISOString()
    };

    res.json(health);

  } catch (error) {
    logger.error('Health check failed', { error: error.message });
    res.status(503).json({
      status: 'unhealthy',
      error: error.message,
      timestamp: new Date().toISOString()
    });
  }
});

module.exports = router;