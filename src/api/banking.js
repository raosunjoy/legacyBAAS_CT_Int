/**
 * Banking API with Transpiler Integration
 * LegacyBAAS Platform - Task 2.1
 * 
 * Extends existing banking services with COBOL transpiler capabilities
 * Provides enterprise-grade endpoints for banking modernization
 */

const express = require('express');
const router = express.Router();
const { CobolTranspiler } = require('../adapters/cobol-transpiler');
const { ConfigManager } = require('../adapters/configs/config-manager');
const { AuthManager, PERMISSIONS } = require('../auth/auth-middleware');
const { SmartRouter } = require('../router/smart-router');
const { ZKProofComplianceService } = require('../compliance/zk-proof-compliance');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const multer = require('multer');
const path = require('path');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'banking-api' }
});

// Configure file upload
const storage = multer.memoryStorage();
const upload = multer({
  storage,
  limits: {
    fileSize: 10 * 1024 * 1024 // 10MB limit
  },
  fileFilter: (req, file, cb) => {
    const allowedTypes = ['.cbl', '.cob', '.txt', '.cobol'];
    const ext = path.extname(file.originalname).toLowerCase();
    cb(null, allowedTypes.includes(ext));
  }
});

// Initialize managers
const configManager = new ConfigManager();
const authManager = new AuthManager();
const smartRouter = new SmartRouter();
const complianceEngine = new ZKProofComplianceService();

// Transpilation status tracking
const transpilationJobs = new Map();

/**
 * Initialize all required services
 */
async function initializeServices() {
  try {
    await configManager.initialize();
    await smartRouter.initialize();
    await complianceEngine.initialize();
    logger.info('Banking API services initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize banking API services', { error: error.message });
    throw error;
  }
}

/**
 * POST /api/v1/banking/transpile
 * Main transpilation endpoint - converts COBOL files to smart contracts
 */
router.post('/transpile', 
  authManager.requireCobolAccess(PERMISSIONS.COBOL_TRANSPILE),
  upload.array('cobolFiles', 10),
  async (req, res) => {
    const requestId = uuidv4();
    const startTime = Date.now();
    
    try {
      logger.info('Banking transpilation request received', {
        requestId,
        userId: req.user.userId,
        customerId: req.user.customerId,
        bankingSystem: req.body.bankingSystem,
        targetBlockchain: req.body.targetBlockchain,
        fileCount: req.files?.length || 0
      });

      // Validate request
      if (!req.files || req.files.length === 0) {
        if (!req.body.cobolCode) {
          return res.status(400).json({
            error: 'VALIDATION_ERROR',
            message: 'COBOL files or code is required',
            requestId
          });
        }
      }

      // Initialize services
      await initializeServices();

      // Parse configuration from request
      const config = {
        bankingSystem: req.body.bankingSystem || 'FIS',
        targetBlockchain: req.body.targetBlockchain || 'Ethereum',
        projectName: req.body.projectName || `Project_${requestId.substring(0, 8)}`,
        optimizeGas: req.body.optimizeGas !== false,
        enableCompliance: req.body.enableCompliance !== false,
        deploymentTarget: req.body.deploymentTarget || 'testnet'
      };

      // Create transpiler instance
      const transpiler = new CobolTranspiler({
        bankingSystem: config.bankingSystem,
        targetBlockchain: config.targetBlockchain,
        optimizeGas: config.optimizeGas,
        enableValidation: true,
        auth: { jwtSecret: process.env.JWT_SECRET || 'legacy-baas-secret' }
      });

      // Track job status
      const jobStatus = {
        requestId,
        status: 'processing',
        phase: 'parsing',
        progress: 0,
        startTime: new Date().toISOString(),
        config,
        files: req.files?.map(f => ({
          name: f.originalname,
          size: f.size,
          status: 'pending'
        })) || [],
        results: [],
        errors: []
      };
      
      transpilationJobs.set(requestId, jobStatus);

      // Process COBOL files or code
      const transpilationResults = [];
      let totalFiles = req.files?.length || 1;
      let processedFiles = 0;

      // Process uploaded files
      if (req.files && req.files.length > 0) {
        for (const file of req.files) {
          try {
            jobStatus.phase = `processing_${file.originalname}`;
            jobStatus.progress = Math.round((processedFiles / totalFiles) * 80); // Reserve 20% for post-processing
            
            const cobolCode = file.buffer.toString('utf-8');
            
            // Parse COBOL
            const ast = await transpiler.parse(cobolCode);
            
            // Generate smart contract
            const contractResult = await transpiler.generateContract(ast, {
              blockchain: config.targetBlockchain,
              enableEvents: req.body.enableEvents !== false,
              enableModifiers: req.body.enableModifiers !== false,
              complianceChecks: config.enableCompliance
            });

            // Get optimal blockchain recommendation
            const routingRecommendation = await smartRouter.getOptimalBlockchain({
              bankingSystem: config.bankingSystem,
              transactionType: 'cobol_contract',
              complexity: ast.procedure.complexity,
              complianceRequirements: config.enableCompliance
            });

            transpilationResults.push({
              fileName: file.originalname,
              fileSize: file.size,
              ast: {
                id: ast.id,
                programId: ast.program.programId,
                variableCount: ast.data.variables.length,
                operationCount: ast.procedure.operations.length,
                complexity: ast.procedure.complexity
              },
              smartContract: {
                blockchain: contractResult.metadata.blockchain,
                contractCode: contractResult.contractCode,
                optimized: contractResult.metadata.optimized,
                features: contractResult.metadata.features,
                estimatedGas: ast.procedure.estimatedGas
              },
              routing: routingRecommendation,
              complianceStatus: config.enableCompliance ? 'checked' : 'skipped'
            });

            // Update file status
            const fileStatus = jobStatus.files.find(f => f.name === file.originalname);
            if (fileStatus) fileStatus.status = 'completed';
            
            processedFiles++;
            
          } catch (fileError) {
            logger.error('Failed to process COBOL file', {
              requestId,
              fileName: file.originalname,
              error: fileError.message
            });
            
            jobStatus.errors.push({
              fileName: file.originalname,
              error: fileError.message
            });

            const fileStatus = jobStatus.files.find(f => f.name === file.originalname);
            if (fileStatus) fileStatus.status = 'failed';
          }
        }
      } else {
        // Process inline COBOL code
        const ast = await transpiler.parse(req.body.cobolCode);
        const contractResult = await transpiler.generateContract(ast, {
          blockchain: config.targetBlockchain,
          enableEvents: req.body.enableEvents !== false,
          enableModifiers: req.body.enableModifiers !== false,
          complianceChecks: config.enableCompliance
        });

        const routingRecommendation = await smartRouter.getOptimalBlockchain({
          bankingSystem: config.bankingSystem,
          transactionType: 'cobol_contract',
          complexity: ast.procedure.complexity,
          complianceRequirements: config.enableCompliance
        });

        transpilationResults.push({
          fileName: 'inline_code.cbl',
          fileSize: req.body.cobolCode.length,
          ast: {
            id: ast.id,
            programId: ast.program.programId,
            variableCount: ast.data.variables.length,
            operationCount: ast.procedure.operations.length,
            complexity: ast.procedure.complexity
          },
          smartContract: {
            blockchain: contractResult.metadata.blockchain,
            contractCode: contractResult.contractCode,
            optimized: contractResult.metadata.optimized,
            features: contractResult.metadata.features,
            estimatedGas: ast.procedure.estimatedGas
          },
          routing: routingRecommendation,
          complianceStatus: config.enableCompliance ? 'checked' : 'skipped'
        });
      }

      // Compliance screening if enabled
      let complianceReport = null;
      if (config.enableCompliance) {
        jobStatus.phase = 'compliance_screening';
        jobStatus.progress = 85;
        
        complianceReport = await complianceEngine.screenTranspilation({
          requestId,
          customerId: req.user.customerId,
          transpilationResults,
          bankingSystem: config.bankingSystem,
          targetBlockchain: config.targetBlockchain
        });
      }

      // Finalize job status
      jobStatus.status = 'completed';
      jobStatus.phase = 'completed';
      jobStatus.progress = 100;
      jobStatus.completedTime = new Date().toISOString();
      jobStatus.results = transpilationResults;
      jobStatus.complianceReport = complianceReport;

      // Prepare response
      const response = {
        requestId,
        success: true,
        project: {
          name: config.projectName,
          configuration: config,
          filesProcessed: transpilationResults.length,
          totalFiles: totalFiles,
          successfulTranspilations: transpilationResults.length,
          failedTranspilations: jobStatus.errors.length
        },
        transpilationResults,
        complianceReport,
        recommendations: {
          optimalBlockchain: transpilationResults[0]?.routing?.recommendedBlockchain || config.targetBlockchain,
          estimatedCosts: transpilationResults.reduce((total, result) => 
            total + (result.smartContract.estimatedGas || 0), 0),
          deploymentStrategy: transpilationResults.length > 1 ? 'batch_deployment' : 'single_deployment'
        },
        metadata: {
          transpilationTime: Date.now() - startTime,
          timestamp: new Date().toISOString(),
          version: '1.0.0'
        },
        links: {
          status: `/api/v1/banking/transpile/status/${requestId}`,
          download: `/api/v1/banking/transpile/${requestId}/download`,
          deploy: `/api/v1/banking/transpile/${requestId}/deploy`
        }
      };

      logger.info('Banking transpilation completed successfully', {
        requestId,
        userId: req.user.userId,
        filesProcessed: transpilationResults.length,
        totalTime: Date.now() - startTime
      });

      res.status(200).json(response);

    } catch (error) {
      // Update job status on error
      if (transpilationJobs.has(requestId)) {
        const jobStatus = transpilationJobs.get(requestId);
        jobStatus.status = 'failed';
        jobStatus.phase = 'error';
        jobStatus.error = error.message;
        jobStatus.completedTime = new Date().toISOString();
      }

      logger.error('Banking transpilation failed', {
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
  }
);

/**
 * GET /api/v1/banking/transpile/status/:id
 * Status tracking endpoint for transpilation jobs
 */
router.get('/transpile/status/:id', 
  authManager.requirePermissions(PERMISSIONS.COBOL_VIEW), 
  async (req, res) => {
    try {
      const requestId = req.params.id;
      
      if (!transpilationJobs.has(requestId)) {
        return res.status(404).json({
          error: 'JOB_NOT_FOUND',
          message: 'Transpilation job not found',
          requestId
        });
      }

      const jobStatus = transpilationJobs.get(requestId);
      
      // Add real-time metrics if job is in progress
      if (jobStatus.status === 'processing') {
        jobStatus.elapsedTime = Date.now() - new Date(jobStatus.startTime).getTime();
        jobStatus.estimatedRemaining = jobStatus.progress > 0 ? 
          Math.round((jobStatus.elapsedTime / jobStatus.progress) * (100 - jobStatus.progress)) : null;
      }

      const response = {
        requestId,
        status: jobStatus.status,
        phase: jobStatus.phase,
        progress: jobStatus.progress,
        startTime: jobStatus.startTime,
        completedTime: jobStatus.completedTime,
        elapsedTime: jobStatus.elapsedTime,
        estimatedRemaining: jobStatus.estimatedRemaining,
        project: {
          name: jobStatus.config?.projectName,
          configuration: jobStatus.config
        },
        files: jobStatus.files,
        results: jobStatus.results,
        errors: jobStatus.errors,
        complianceReport: jobStatus.complianceReport,
        links: {
          details: `/api/v1/banking/transpile/${requestId}`,
          download: jobStatus.status === 'completed' ? `/api/v1/banking/transpile/${requestId}/download` : null,
          deploy: jobStatus.status === 'completed' ? `/api/v1/banking/transpile/${requestId}/deploy` : null
        },
        timestamp: new Date().toISOString()
      };

      res.json(response);

    } catch (error) {
      logger.error('Status check failed', { 
        requestId: req.params.id, 
        error: error.message 
      });
      
      res.status(500).json({
        error: 'STATUS_ERROR',
        message: error.message,
        requestId: req.params.id,
        timestamp: new Date().toISOString()
      });
    }
  }
);

/**
 * POST /api/v1/banking/transpile/validate
 * COBOL validation endpoint with banking system specific checks
 */
router.post('/transpile/validate', 
  authManager.requirePermissions(PERMISSIONS.COBOL_VIEW),
  upload.array('cobolFiles', 5),
  async (req, res) => {
    const requestId = uuidv4();
    const startTime = Date.now();

    try {
      logger.info('Banking COBOL validation request received', {
        requestId,
        userId: req.user.userId,
        bankingSystem: req.body.bankingSystem,
        fileCount: req.files?.length || 0
      });

      // Validate request
      if (!req.files || req.files.length === 0) {
        if (!req.body.cobolCode) {
          return res.status(400).json({
            error: 'VALIDATION_ERROR',
            message: 'COBOL files or code is required for validation',
            requestId
          });
        }
      }

      await initializeServices();

      const bankingSystem = req.body.bankingSystem || 'FIS';
      const validationResults = [];

      // Create transpiler for validation
      const transpiler = new CobolTranspiler({
        bankingSystem,
        enableValidation: true,
        strictMode: req.body.strictMode !== false
      });

      // Validate files or inline code
      if (req.files && req.files.length > 0) {
        for (const file of req.files) {
          try {
            const cobolCode = file.buffer.toString('utf-8');
            
            // Parse for validation
            const ast = await transpiler.parse(cobolCode);
            
            // Banking system specific validation
            const bankingValidation = await configManager.validateCobolForBankingSystem(
              ast, bankingSystem
            );

            const validationResult = {
              fileName: file.originalname,
              fileSize: file.size,
              valid: true,
              programId: ast.program.programId,
              statistics: {
                variableCount: ast.data.variables.length,
                operationCount: ast.procedure.operations.length,
                complexity: ast.procedure.complexity,
                estimatedGas: ast.procedure.estimatedGas
              },
              bankingValidation: {
                systemCompatibility: bankingValidation.compatible,
                supportedFeatures: bankingValidation.supportedFeatures,
                unsupportedFeatures: bankingValidation.unsupportedFeatures,
                recommendations: bankingValidation.recommendations
              },
              warnings: [],
              errors: []
            };

            // Add warnings
            if (ast.data.variables.length === 0) {
              validationResult.warnings.push('No variables found in COBOL program');
            }
            
            if (ast.procedure.operations.length === 0) {
              validationResult.warnings.push('No procedure operations found');
            }

            if (!bankingValidation.compatible) {
              validationResult.warnings.push(`Limited compatibility with ${bankingSystem} system`);
            }

            validationResults.push(validationResult);

          } catch (fileError) {
            validationResults.push({
              fileName: file.originalname,
              fileSize: file.size,
              valid: false,
              error: fileError.message,
              errorType: 'SYNTAX_ERROR',
              bankingValidation: {
                systemCompatibility: false,
                supportedFeatures: [],
                unsupportedFeatures: [],
                recommendations: ['Fix syntax errors before banking system validation']
              }
            });
          }
        }
      } else {
        // Validate inline code
        try {
          const ast = await transpiler.parse(req.body.cobolCode);
          const bankingValidation = await configManager.validateCobolForBankingSystem(
            ast, bankingSystem
          );

          validationResults.push({
            fileName: 'inline_code.cbl',
            fileSize: req.body.cobolCode.length,
            valid: true,
            programId: ast.program.programId,
            statistics: {
              variableCount: ast.data.variables.length,
              operationCount: ast.procedure.operations.length,
              complexity: ast.procedure.complexity,
              estimatedGas: ast.procedure.estimatedGas
            },
            bankingValidation: {
              systemCompatibility: bankingValidation.compatible,
              supportedFeatures: bankingValidation.supportedFeatures,
              unsupportedFeatures: bankingValidation.unsupportedFeatures,
              recommendations: bankingValidation.recommendations
            },
            warnings: [],
            errors: []
          });

        } catch (validationError) {
          validationResults.push({
            fileName: 'inline_code.cbl',
            fileSize: req.body.cobolCode.length,
            valid: false,
            error: validationError.message,
            errorType: 'SYNTAX_ERROR'
          });
        }
      }

      // Calculate overall validation summary
      const totalFiles = validationResults.length;
      const validFiles = validationResults.filter(r => r.valid).length;
      const invalidFiles = totalFiles - validFiles;
      const compatibleFiles = validationResults.filter(r => 
        r.valid && r.bankingValidation?.systemCompatibility
      ).length;

      const response = {
        requestId,
        success: true,
        validation: {
          overall: {
            totalFiles,
            validFiles,
            invalidFiles,
            compatibleFiles,
            validationRate: Math.round((validFiles / totalFiles) * 100),
            compatibilityRate: Math.round((compatibleFiles / totalFiles) * 100)
          },
          bankingSystem,
          strictMode: req.body.strictMode !== false,
          results: validationResults,
          summary: {
            totalVariables: validationResults.reduce((sum, r) => 
              sum + (r.statistics?.variableCount || 0), 0),
            totalOperations: validationResults.reduce((sum, r) => 
              sum + (r.statistics?.operationCount || 0), 0),
            averageComplexity: validationResults.length > 0 ? 
              validationResults.reduce((sum, r) => 
                sum + (r.statistics?.complexity || 0), 0) / validationResults.length : 0
          }
        },
        validationTime: Date.now() - startTime,
        timestamp: new Date().toISOString()
      };

      logger.info('Banking COBOL validation completed', {
        requestId,
        userId: req.user.userId,
        totalFiles,
        validFiles,
        validationTime: Date.now() - startTime
      });

      res.json(response);

    } catch (error) {
      logger.error('Banking COBOL validation failed', {
        requestId,
        userId: req.user?.userId,
        error: error.message,
        validationTime: Date.now() - startTime
      });

      res.status(500).json({
        error: 'VALIDATION_ERROR',
        message: error.message,
        requestId,
        timestamp: new Date().toISOString()
      });
    }
  }
);

/**
 * GET /api/v1/banking/transpile/templates
 * Available templates endpoint with banking system specific templates
 */
router.get('/transpile/templates', 
  authManager.requirePermissions(PERMISSIONS.COBOL_VIEW), 
  async (req, res) => {
    try {
      await initializeServices();

      const bankingSystemFilter = req.query.bankingSystem;
      const blockchainFilter = req.query.blockchain;
      const useCaseFilter = req.query.useCase;

      // Get templates from config manager
      const templates = {
        bankingSystems: {
          FIS: {
            name: 'FIS Systematics',
            version: '2.1',
            cobolDialect: 'COBOL-85',
            preferredBlockchains: ['Ethereum', 'Corda'],
            templates: {
              mortgage_processing: {
                name: 'Mortgage Processing',
                description: 'Complete mortgage application and approval workflow',
                complexity: 'high',
                estimatedGas: 180000,
                complianceRequirements: ['BSA_AML', 'OFAC_SCREENING'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. MORTGAGE-CALC.\n...',
                targetContracts: ['Solidity', 'Corda Flow']
              },
              account_management: {
                name: 'Account Management',
                description: 'Customer account operations and management',
                complexity: 'medium',
                estimatedGas: 120000,
                complianceRequirements: ['BSA_AML'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. ACCT-MGMT.\n...',
                targetContracts: ['Solidity']
              }
            }
          },
          Fiserv: {
            name: 'Fiserv DNA',
            version: '4.2',
            cobolDialect: 'COBOL-2002',
            preferredBlockchains: ['Algorand', 'XRP'],
            templates: {
              payment_processing: {
                name: 'Payment Processing',
                description: 'Real-time payment processing and settlement',
                complexity: 'medium',
                estimatedGas: 95000,
                complianceRequirements: ['PCI_DSS', 'BSA_AML'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. PAY-PROC.\n...',
                targetContracts: ['Algorand TEAL', 'XRP Ledger']
              },
              transaction_monitoring: {
                name: 'Transaction Monitoring',
                description: 'Real-time transaction monitoring and fraud detection',
                complexity: 'high',
                estimatedGas: 150000,
                complianceRequirements: ['BSA_AML', 'FATF_TRAVEL_RULE'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TXN-MON.\n...',
                targetContracts: ['Algorand TEAL']
              }
            }
          },
          Temenos: {
            name: 'Temenos Transact',
            version: 'R23',
            cobolDialect: 'COBOL-85',
            preferredBlockchains: ['Ethereum', 'Corda'],
            templates: {
              trade_finance: {
                name: 'Trade Finance',
                description: 'Letters of credit and trade finance operations',
                complexity: 'high',
                estimatedGas: 200000,
                complianceRequirements: ['FATF_TRAVEL_RULE', 'SANCTIONS_SCREENING'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TRADE-FIN.\n...',
                targetContracts: ['Corda Flow', 'Solidity']
              }
            }
          },
          'TCS_BaNCS': {
            name: 'TCS BaNCS',
            version: '14.0',
            cobolDialect: 'COBOL-2014',
            preferredBlockchains: ['Ethereum', 'Algorand', 'XRP'],
            templates: {
              core_banking: {
                name: 'Core Banking Operations',
                description: 'Essential banking operations and account management',
                complexity: 'high',
                estimatedGas: 175000,
                complianceRequirements: ['BSA_AML', 'OFAC_SCREENING'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. CORE-BANK.\n...',
                targetContracts: ['Solidity', 'Algorand TEAL']
              },
              regulatory_reporting: {
                name: 'Regulatory Reporting',
                description: 'Automated regulatory compliance reporting',
                complexity: 'medium',
                estimatedGas: 110000,
                complianceRequirements: ['REGULATORY_REPORTING', 'BSA_AML'],
                sampleCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. REG-RPT.\n...',
                targetContracts: ['Solidity']
              }
            }
          }
        },
        blockchainTemplates: {
          Ethereum: {
            name: 'Ethereum (Solidity)',
            language: 'solidity',
            features: ['events', 'modifiers', 'inheritance', 'libraries'],
            gasOptimization: true,
            complianceSupport: true,
            deploymentNetworks: ['mainnet', 'sepolia', 'polygon']
          },
          Corda: {
            name: 'R3 Corda',
            language: 'kotlin',
            features: ['privacy', 'notary', 'flows', 'states', 'contracts'],
            gasOptimization: false,
            complianceSupport: true,
            deploymentNetworks: ['corda_network', 'corda_testnet']
          },
          Algorand: {
            name: 'Algorand',
            language: 'teal',
            features: ['atomic_swaps', 'smart_contracts', 'asset_management'],
            gasOptimization: true,
            complianceSupport: true,
            deploymentNetworks: ['mainnet', 'testnet', 'betanet']
          },
          XRP: {
            name: 'XRP Ledger',
            language: 'hooks',
            features: ['payment_channels', 'escrow', 'decentralized_exchange'],
            gasOptimization: true,
            complianceSupport: true,
            deploymentNetworks: ['mainnet', 'testnet']
          }
        },
        useCases: {
          mortgage_processing: {
            name: 'Mortgage Processing',
            supportedSystems: ['FIS'],
            recommendedBlockchain: 'Corda',
            complexity: 'high',
            estimatedImplementationTime: '4-6 weeks'
          },
          payment_processing: {
            name: 'Payment Processing',
            supportedSystems: ['Fiserv', 'TCS_BaNCS'],
            recommendedBlockchain: 'Algorand',
            complexity: 'medium',
            estimatedImplementationTime: '2-4 weeks'
          },
          trade_finance: {
            name: 'Trade Finance',
            supportedSystems: ['Temenos', 'TCS_BaNCS'],
            recommendedBlockchain: 'Corda',
            complexity: 'high',
            estimatedImplementationTime: '6-8 weeks'
          },
          core_banking: {
            name: 'Core Banking Operations',
            supportedSystems: ['TCS_BaNCS', 'FIS'],
            recommendedBlockchain: 'Ethereum',
            complexity: 'high',
            estimatedImplementationTime: '8-12 weeks'
          }
        }
      };

      // Apply filters
      let filteredTemplates = { ...templates };

      if (bankingSystemFilter && templates.bankingSystems[bankingSystemFilter]) {
        filteredTemplates.bankingSystems = {
          [bankingSystemFilter]: templates.bankingSystems[bankingSystemFilter]
        };
      }

      if (blockchainFilter && templates.blockchainTemplates[blockchainFilter]) {
        filteredTemplates.blockchainTemplates = {
          [blockchainFilter]: templates.blockchainTemplates[blockchainFilter]
        };
      }

      if (useCaseFilter && templates.useCases[useCaseFilter]) {
        filteredTemplates.useCases = {
          [useCaseFilter]: templates.useCases[useCaseFilter]
        };
      }

      const response = {
        success: true,
        templates: filteredTemplates,
        filters: {
          bankingSystem: bankingSystemFilter,
          blockchain: blockchainFilter,
          useCase: useCaseFilter
        },
        metadata: {
          totalBankingSystems: Object.keys(filteredTemplates.bankingSystems).length,
          totalBlockchains: Object.keys(filteredTemplates.blockchainTemplates).length,
          totalUseCases: Object.keys(filteredTemplates.useCases).length,
          totalTemplates: Object.values(filteredTemplates.bankingSystems).reduce(
            (sum, system) => sum + Object.keys(system.templates).length, 0
          )
        },
        links: {
          validate: '/api/v1/banking/transpile/validate',
          transpile: '/api/v1/banking/transpile',
          documentation: '/docs/api/banking/transpiler'
        },
        timestamp: new Date().toISOString()
      };

      logger.info('Banking templates retrieved successfully', {
        userId: req.user?.userId,
        filters: { bankingSystemFilter, blockchainFilter, useCaseFilter },
        totalTemplates: response.metadata.totalTemplates
      });

      res.json(response);

    } catch (error) {
      logger.error('Banking templates retrieval failed', { error: error.message });
      res.status(500).json({
        error: 'TEMPLATES_ERROR',
        message: error.message,
        timestamp: new Date().toISOString()
      });
    }
  }
);

/**
 * Error handler middleware
 */
router.use((error, req, res, next) => {
  logger.error('Banking API error', {
    error: error.message,
    stack: error.stack,
    path: req.path,
    method: req.method
  });

  res.status(500).json({
    error: 'INTERNAL_SERVER_ERROR',
    message: 'An unexpected error occurred',
    timestamp: new Date().toISOString()
  });
});

module.exports = router;