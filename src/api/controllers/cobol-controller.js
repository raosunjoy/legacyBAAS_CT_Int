/**
 * COBOL Transpiler Controller
 * LegacyBAAS Platform Integration
 * 
 * Business logic for COBOL transpilation operations
 * Handles orchestration between transpiler, router, and compliance engines
 */

const { CobolTranspiler } = require('../../adapters/cobol-transpiler');
const { ConfigManager } = require('../../adapters/configs/config-manager');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'cobol-controller' }
});

class CobolController {
  constructor() {
    this.configManager = null;
    this.transpilerInstances = new Map();
    this.requestCache = new Map();
    this.metrics = {
      requestsProcessed: 0,
      requestsSuccessful: 0,
      requestsFailed: 0,
      averageProcessingTime: 0
    };
  }

  /**
   * Initialize controller dependencies
   */
  async initialize() {
    if (!this.configManager) {
      this.configManager = new ConfigManager();
      await this.configManager.initialize();
      logger.info('COBOL Controller initialized successfully');
    }
  }

  /**
   * Get or create transpiler instance for specific configuration
   */
  async getTranspilerInstance(config) {
    const configKey = `${config.bankingSystem}-${config.targetBlockchain}`;
    
    if (!this.transpilerInstances.has(configKey)) {
      const transpiler = new CobolTranspiler({
        bankingSystem: config.bankingSystem,
        targetBlockchain: config.targetBlockchain,
        optimizeGas: config.optimizeGas !== false,
        enableValidation: config.enableValidation !== false,
        auth: {
          jwtSecret: process.env.JWT_SECRET || 'legacy-baas-secret'
        }
      });
      
      this.transpilerInstances.set(configKey, transpiler);
      logger.debug('Created new transpiler instance', { configKey });
    }
    
    return this.transpilerInstances.get(configKey);
  }

  /**
   * Validate transpilation request
   */
  validateTranspilationRequest(body) {
    const errors = [];

    if (!body.cobolCode || typeof body.cobolCode !== 'string') {
      errors.push('COBOL code is required and must be a string');
    }

    if (body.cobolCode && body.cobolCode.length > 100000) {
      errors.push('COBOL code exceeds maximum length (100,000 characters)');
    }

    if (body.bankingSystem && !this.configManager.getAvailableBankingSystems().includes(body.bankingSystem)) {
      errors.push(`Unsupported banking system: ${body.bankingSystem}`);
    }

    const supportedBlockchains = ['ethereum', 'corda', 'algorand'];
    if (body.targetBlockchain && !supportedBlockchains.includes(body.targetBlockchain)) {
      errors.push(`Unsupported blockchain: ${body.targetBlockchain}`);
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  /**
   * Enhanced transpilation with smart routing
   */
  async transpileWithSmartRouting(cobolCode, options, userContext) {
    const startTime = Date.now();
    const requestId = uuidv4();

    try {
      await this.initialize();

      // Validate request
      const validation = this.validateTranspilationRequest({ cobolCode, ...options });
      if (!validation.valid) {
        throw new Error(`Validation failed: ${validation.errors.join(', ')}`);
      }

      // Determine optimal configuration
      const optimalConfig = await this.determineOptimalConfiguration(cobolCode, options, userContext);
      
      logger.info('Starting smart transpilation', {
        requestId,
        userId: userContext.userId,
        optimalConfig,
        cobolLength: cobolCode.length
      });

      // Get transpiler instance
      const transpiler = await this.getTranspilerInstance(optimalConfig);

      // Parse COBOL
      const ast = await transpiler.parse(cobolCode);

      // Apply compliance checks if required
      if (options.complianceChecks !== false) {
        await this.performComplianceChecks(ast, optimalConfig, userContext);
      }

      // Generate smart contract
      const contractResult = await transpiler.generateContract(ast, {
        blockchain: optimalConfig.targetBlockchain,
        enableEvents: options.enableEvents !== false,
        enableModifiers: options.enableModifiers !== false,
        ...options
      });

      // Cache result for future retrieval
      const cacheKey = `${requestId}`;
      this.requestCache.set(cacheKey, {
        ast,
        contractResult,
        configuration: optimalConfig,
        timestamp: new Date().toISOString(),
        userContext: {
          userId: userContext.userId,
          customerId: userContext.customerId
        }
      });

      // Update metrics
      this.updateMetrics(Date.now() - startTime, true);

      const response = {
        requestId,
        success: true,
        transpilation: {
          ast: this.sanitizeASTForResponse(ast),
          smartContract: {
            blockchain: contractResult.metadata.blockchain,
            language: contractResult.metadata.language,
            contractCode: contractResult.contractCode,
            optimized: contractResult.metadata.optimized,
            features: contractResult.metadata.features,
            gasEstimate: ast.procedure.estimatedGas
          },
          configuration: optimalConfig,
          routing: {
            reason: optimalConfig.routingReason,
            alternatives: optimalConfig.alternatives || []
          }
        },
        metadata: {
          transpilationTime: Date.now() - startTime,
          generationTime: contractResult.metadata.generationTime,
          cacheExpiry: new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString(), // 24 hours
          timestamp: new Date().toISOString()
        }
      };

      logger.info('Smart transpilation completed successfully', {
        requestId,
        userId: userContext.userId,
        programId: ast.program.programId,
        totalTime: Date.now() - startTime,
        blockchain: optimalConfig.targetBlockchain
      });

      return response;

    } catch (error) {
      this.updateMetrics(Date.now() - startTime, false);
      
      logger.error('Smart transpilation failed', {
        requestId,
        userId: userContext?.userId,
        error: error.message,
        totalTime: Date.now() - startTime
      });

      throw error;
    }
  }

  /**
   * Determine optimal configuration using smart routing
   */
  async determineOptimalConfiguration(cobolCode, options, userContext) {
    // Default configuration
    let optimalConfig = {
      bankingSystem: options.bankingSystem || 'FIS_SYSTEMATICS',
      targetBlockchain: options.targetBlockchain || 'ethereum',
      optimizeGas: options.optimizeGas !== false,
      enableValidation: options.enableValidation !== false,
      routingReason: 'default_configuration'
    };

    try {
      // Analyze COBOL code for patterns
      const analysis = this.analyzeCobolCode(cobolCode);

      // Banking system routing
      if (!options.bankingSystem) {
        const detectedSystem = this.detectBankingSystem(analysis);
        if (detectedSystem) {
          optimalConfig.bankingSystem = detectedSystem;
          optimalConfig.routingReason = 'detected_banking_system';
        }
      }

      // Blockchain routing based on requirements
      if (!options.targetBlockchain) {
        const optimalBlockchain = this.selectOptimalBlockchain(analysis, optimalConfig.bankingSystem, userContext);
        optimalConfig.targetBlockchain = optimalBlockchain.blockchain;
        optimalConfig.routingReason = optimalBlockchain.reason;
        optimalConfig.alternatives = optimalBlockchain.alternatives;
      }

      // Compliance-based routing
      const complianceRequirements = this.configManager.getComplianceRequirements(optimalConfig.bankingSystem);
      if (Object.keys(complianceRequirements).length > 0) {
        const complianceOptimal = this.optimizeForCompliance(optimalConfig, complianceRequirements);
        optimalConfig = { ...optimalConfig, ...complianceOptimal };
      }

      return optimalConfig;

    } catch (error) {
      logger.warn('Smart routing failed, using defaults', { error: error.message });
      return optimalConfig;
    }
  }

  /**
   * Analyze COBOL code for routing hints
   */
  analyzeCobolCode(cobolCode) {
    const analysis = {
      complexity: 0,
      dataTypes: [],
      operations: [],
      patterns: [],
      estimatedSize: cobolCode.length
    };

    // Count variables and operations
    const varMatches = cobolCode.match(/\d{2}\s+[A-Z0-9-]+(?:\s+PIC\s+[^.]+)?/g) || [];
    analysis.variableCount = varMatches.length;

    // Detect data types
    const picMatches = cobolCode.match(/PIC\s+([^.\s]+)/g) || [];
    analysis.dataTypes = picMatches.map(match => match.replace('PIC ', ''));

    // Count operations
    const computeMatches = cobolCode.match(/COMPUTE\s+/g) || [];
    const ifMatches = cobolCode.match(/IF\s+/g) || [];
    const performMatches = cobolCode.match(/PERFORM\s+/g) || [];
    
    analysis.operations = [
      { type: 'compute', count: computeMatches.length },
      { type: 'conditional', count: ifMatches.length },
      { type: 'perform', count: performMatches.length }
    ];

    // Calculate complexity
    analysis.complexity = varMatches.length + (computeMatches.length * 2) + (ifMatches.length * 2) + performMatches.length;

    // Detect patterns
    if (cobolCode.includes('MORTGAGE') || cobolCode.includes('LOAN')) {
      analysis.patterns.push('mortgage_processing');
    }
    if (cobolCode.includes('PAYMENT') || cobolCode.includes('TRANSFER')) {
      analysis.patterns.push('payment_processing');
    }
    if (cobolCode.includes('TRADE') || cobolCode.includes('FINANCE')) {
      analysis.patterns.push('trade_finance');
    }

    return analysis;
  }

  /**
   * Detect banking system from COBOL patterns
   */
  detectBankingSystem(analysis) {
    // FIS patterns
    if (analysis.dataTypes.some(type => type.includes('COMP-3'))) {
      return 'FIS_SYSTEMATICS';
    }

    // Fiserv patterns (REST-oriented, modern data types)
    if (analysis.patterns.includes('payment_processing')) {
      return 'FISERV_DNA';
    }

    // Temenos patterns (European banking)
    if (analysis.patterns.includes('trade_finance')) {
      return 'TEMENOS_TRANSACT';
    }

    // TCS BaNCS patterns (universal banking)
    if (analysis.complexity > 10) {
      return 'TCS_BANCS';
    }

    return null;
  }

  /**
   * Select optimal blockchain based on requirements
   */
  selectOptimalBlockchain(analysis, bankingSystem, userContext) {
    const preferredBlockchains = this.configManager.getPreferredBlockchains(bankingSystem);
    const alternatives = [];

    // High complexity -> Corda for privacy
    if (analysis.complexity > 15) {
      alternatives.push({ blockchain: 'ethereum', reason: 'high_complexity_evm' });
      alternatives.push({ blockchain: 'algorand', reason: 'high_complexity_low_cost' });
      return {
        blockchain: 'corda',
        reason: 'high_complexity_privacy_required',
        alternatives
      };
    }

    // Payment processing -> Algorand for speed and cost
    if (analysis.patterns.includes('payment_processing')) {
      alternatives.push({ blockchain: 'ethereum', reason: 'payment_defi_integration' });
      alternatives.push({ blockchain: 'corda', reason: 'payment_privacy' });
      return {
        blockchain: 'algorand',
        reason: 'payment_processing_optimized',
        alternatives
      };
    }

    // Trade finance -> Ethereum for ecosystem
    if (analysis.patterns.includes('trade_finance')) {
      alternatives.push({ blockchain: 'corda', reason: 'trade_privacy' });
      alternatives.push({ blockchain: 'algorand', reason: 'trade_cost_efficiency' });
      return {
        blockchain: 'ethereum',
        reason: 'trade_finance_ecosystem',
        alternatives
      };
    }

    // Default to system preferred
    const preferred = preferredBlockchains[0] || 'ethereum';
    return {
      blockchain: preferred,
      reason: 'banking_system_preference',
      alternatives: preferredBlockchains.slice(1).map(bc => ({ blockchain: bc, reason: 'alternative_preference' }))
    };
  }

  /**
   * Optimize configuration for compliance requirements
   */
  optimizeForCompliance(config, complianceRequirements) {
    const optimizations = {};

    // BSA/AML requirements prefer privacy-focused blockchains
    if (complianceRequirements.bsa_aml && config.targetBlockchain === 'ethereum') {
      optimizations.privacyEnhanced = true;
      optimizations.auditTrail = true;
    }

    // GDPR requirements
    if (complianceRequirements.gdpr) {
      optimizations.dataMinimization = true;
      optimizations.rightToErasure = true;
    }

    // PSD2 requirements
    if (complianceRequirements.psd2) {
      optimizations.strongAuthentication = true;
      optimizations.openBanking = true;
    }

    return optimizations;
  }

  /**
   * Perform compliance checks on AST
   */
  async performComplianceChecks(ast, config, userContext) {
    const complianceResults = {
      checks: [],
      warnings: [],
      errors: []
    };

    // Check for sensitive data patterns
    ast.data.variables.forEach(variable => {
      if (variable.name.includes('ssn') || variable.name.includes('social_security')) {
        complianceResults.warnings.push(`Sensitive data detected: ${variable.name}`);
      }
      
      if (variable.name.includes('account') && !variable.name.includes('number')) {
        complianceResults.checks.push(`Account data field: ${variable.name}`);
      }
    });

    // Check operation compliance
    ast.procedure.operations.forEach(operation => {
      if (operation.type === 'compute' && operation.expression.includes('INTEREST')) {
        complianceResults.checks.push('Interest calculation detected - requires regulatory validation');
      }
    });

    logger.info('Compliance checks completed', {
      programId: ast.program.programId,
      userId: userContext.userId,
      checksCount: complianceResults.checks.length,
      warningsCount: complianceResults.warnings.length,
      errorsCount: complianceResults.errors.length
    });

    if (complianceResults.errors.length > 0) {
      throw new Error(`Compliance validation failed: ${complianceResults.errors.join(', ')}`);
    }

    return complianceResults;
  }

  /**
   * Sanitize AST for API response
   */
  sanitizeASTForResponse(ast) {
    return {
      id: ast.id,
      programId: ast.program.programId,
      author: ast.program.author,
      dateWritten: ast.program.dateWritten,
      variableCount: ast.data.variables.length,
      operationCount: ast.procedure.operations.length,
      complexity: ast.procedure.complexity,
      estimatedGas: ast.procedure.estimatedGas,
      metadata: ast.metadata
    };
  }

  /**
   * Update controller metrics
   */
  updateMetrics(processingTime, success) {
    this.metrics.requestsProcessed++;
    
    if (success) {
      this.metrics.requestsSuccessful++;
    } else {
      this.metrics.requestsFailed++;
    }

    // Calculate running average
    const total = this.metrics.requestsProcessed;
    this.metrics.averageProcessingTime = 
      ((this.metrics.averageProcessingTime * (total - 1)) + processingTime) / total;
  }

  /**
   * Get controller metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate: this.metrics.requestsProcessed > 0 ? 
        (this.metrics.requestsSuccessful / this.metrics.requestsProcessed) * 100 : 0,
      activeInstances: this.transpilerInstances.size,
      cacheSize: this.requestCache.size,
      timestamp: new Date().toISOString()
    };
  }

  /**
   * Get cached transpilation result
   */
  getCachedResult(requestId) {
    return this.requestCache.get(requestId) || null;
  }

  /**
   * Clear expired cache entries
   */
  clearExpiredCache() {
    const now = Date.now();
    const expiry = 24 * 60 * 60 * 1000; // 24 hours

    for (const [key, entry] of this.requestCache.entries()) {
      const entryTime = new Date(entry.timestamp).getTime();
      if (now - entryTime > expiry) {
        this.requestCache.delete(key);
      }
    }
  }
}

module.exports = { CobolController };