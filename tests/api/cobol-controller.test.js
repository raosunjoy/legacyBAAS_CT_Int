/**
 * COBOL Controller Test Suite
 * LegacyBAAS Platform Integration Tests
 * 
 * Tests COBOL controller business logic, smart routing, and optimization
 */

const { CobolController } = require('../../src/api/controllers/cobol-controller');
const { ConfigManager } = require('../../src/adapters/configs/config-manager');
const { USER_ROLES } = require('../../src/auth/auth-middleware');

describe('COBOL Controller Tests', () => {
  let controller;
  let mockUserContext;

  beforeAll(async () => {
    controller = new CobolController();
    await controller.initialize();

    mockUserContext = {
      userId: 'test-user',
      customerId: 'test-customer',
      role: USER_ROLES.BANK_ADMIN,
      permissions: ['cobol:transpile', 'cobol:view'],
      features: {
        cobol_transpiler: {
          enabled: true,
          quota: { transpilations_per_month: 100 }
        }
      }
    };
  });

  const sampleCobolCode = `
    IDENTIFICATION DIVISION.
    PROGRAM-ID. MORTGAGE-CALC.
    AUTHOR. TEST-BANK.
    
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 PRINCIPAL PIC 9(10)V99 COMP-3.
    01 INTEREST-RATE PIC 9(3)V99 COMP-3.
    01 TERM PIC 9(3) COMP.
    01 MONTHLY-PAYMENT PIC 9(10)V99 COMP-3.
    
    PROCEDURE DIVISION.
    MAIN-PARAGRAPH.
        MOVE 250000.00 TO PRINCIPAL.
        MOVE 4.25 TO INTEREST-RATE.
        MOVE 360 TO TERM.
        COMPUTE MONTHLY-PAYMENT = 
            (PRINCIPAL * INTEREST-RATE / 1200) / 
            (1 - (1 + INTEREST-RATE / 1200) ** (-TERM)).
        IF MONTHLY-PAYMENT > 5000
            DISPLAY "HIGH PAYMENT WARNING".
        STOP RUN.
  `;

  describe('Controller Initialization', () => {
    test('should initialize successfully', async () => {
      const newController = new CobolController();
      await newController.initialize();
      
      expect(newController.configManager).toBeDefined();
      expect(newController.configManager.initialized).toBe(true);
    });

    test('should create transpiler instances', async () => {
      const config = {
        bankingSystem: 'FIS_SYSTEMATICS',
        targetBlockchain: 'ethereum'
      };
      
      const transpiler = await controller.getTranspilerInstance(config);
      expect(transpiler).toBeDefined();
      expect(controller.transpilerInstances.size).toBe(1);
    });

    test('should reuse existing transpiler instances', async () => {
      const config = {
        bankingSystem: 'FIS_SYSTEMATICS',
        targetBlockchain: 'ethereum'
      };
      
      const transpiler1 = await controller.getTranspilerInstance(config);
      const transpiler2 = await controller.getTranspilerInstance(config);
      
      expect(transpiler1).toBe(transpiler2);
      expect(controller.transpilerInstances.size).toBe(1);
    });
  });

  describe('Request Validation', () => {
    test('should validate correct transpilation request', () => {
      const request = {
        cobolCode: sampleCobolCode,
        bankingSystem: 'FIS_SYSTEMATICS',
        targetBlockchain: 'ethereum'
      };
      
      const validation = controller.validateTranspilationRequest(request);
      expect(validation.valid).toBe(true);
      expect(validation.errors).toHaveLength(0);
    });

    test('should reject request without COBOL code', () => {
      const request = {
        bankingSystem: 'FIS_SYSTEMATICS'
      };
      
      const validation = controller.validateTranspilationRequest(request);
      expect(validation.valid).toBe(false);
      expect(validation.errors).toContain('COBOL code is required and must be a string');
    });

    test('should reject oversized COBOL code', () => {
      const request = {
        cobolCode: 'A'.repeat(100001) // > 100KB
      };
      
      const validation = controller.validateTranspilationRequest(request);
      expect(validation.valid).toBe(false);
      expect(validation.errors).toContain('COBOL code exceeds maximum length (100,000 characters)');
    });

    test('should reject unsupported banking system', () => {
      const request = {
        cobolCode: sampleCobolCode,
        bankingSystem: 'UNSUPPORTED_SYSTEM'
      };
      
      const validation = controller.validateTranspilationRequest(request);
      expect(validation.valid).toBe(false);
      expect(validation.errors.some(e => e.includes('Unsupported banking system'))).toBe(true);
    });

    test('should reject unsupported blockchain', () => {
      const request = {
        cobolCode: sampleCobolCode,
        targetBlockchain: 'unsupported-chain'
      };
      
      const validation = controller.validateTranspilationRequest(request);
      expect(validation.valid).toBe(false);
      expect(validation.errors.some(e => e.includes('Unsupported blockchain'))).toBe(true);
    });
  });

  describe('COBOL Code Analysis', () => {
    test('should analyze COBOL code structure', () => {
      const analysis = controller.analyzeCobolCode(sampleCobolCode);
      
      expect(analysis).toHaveProperty('complexity');
      expect(analysis).toHaveProperty('dataTypes');
      expect(analysis).toHaveProperty('operations');
      expect(analysis).toHaveProperty('patterns');
      expect(analysis).toHaveProperty('estimatedSize');
      
      expect(analysis.complexity).toBeGreaterThan(0);
      expect(analysis.variableCount).toBeGreaterThan(0);
      expect(analysis.dataTypes).toContain('9(10)V99');
      expect(analysis.operations.some(op => op.type === 'compute')).toBe(true);
    });

    test('should detect mortgage processing pattern', () => {
      const mortgageCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. MORTGAGE-PROC.
        
        DATA DIVISION.
        01 MORTGAGE-AMOUNT PIC 9(10)V99.
        
        PROCEDURE DIVISION.
        COMPUTE MORTGAGE-PAYMENT = MORTGAGE-AMOUNT * 0.05.
      `;
      
      const analysis = controller.analyzeCobolCode(mortgageCobol);
      expect(analysis.patterns).toContain('mortgage_processing');
    });

    test('should detect payment processing pattern', () => {
      const paymentCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYMENT-PROC.
        
        DATA DIVISION.
        01 PAYMENT-AMOUNT PIC 9(8)V99.
        01 TRANSFER-FEE PIC 9(4)V99.
        
        PROCEDURE DIVISION.
        COMPUTE TOTAL = PAYMENT-AMOUNT + TRANSFER-FEE.
      `;
      
      const analysis = controller.analyzeCobolCode(paymentCobol);
      expect(analysis.patterns).toContain('payment_processing');
    });

    test('should detect trade finance pattern', () => {
      const tradeCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TRADE-FINANCE.
        
        DATA DIVISION.
        01 TRADE-AMOUNT PIC 9(12)V99.
        01 FINANCE-RATE PIC 9(3)V99.
        
        PROCEDURE DIVISION.
        COMPUTE FINANCE-COST = TRADE-AMOUNT * FINANCE-RATE.
      `;
      
      const analysis = controller.analyzeCobolCode(tradeCobol);
      expect(analysis.patterns).toContain('trade_finance');
    });
  });

  describe('Banking System Detection', () => {
    test('should detect FIS Systematics from COMP-3 usage', () => {
      const fisCobol = `
        DATA DIVISION.
        01 AMOUNT PIC 9(8)V99 COMP-3.
      `;
      
      const analysis = controller.analyzeCobolCode(fisCobol);
      const detected = controller.detectBankingSystem(analysis);
      expect(detected).toBe('FIS_SYSTEMATICS');
    });

    test('should detect Fiserv DNA from payment patterns', () => {
      const fiservCobol = `
        PROGRAM-ID. PAYMENT-PROCESSOR.
        01 PAYMENT-AMOUNT PIC 9(8)V99.
      `;
      
      const analysis = controller.analyzeCobolCode(fiservCobol);
      const detected = controller.detectBankingSystem(analysis);
      expect(detected).toBe('FISERV_DNA');
    });

    test('should detect Temenos from trade finance patterns', () => {
      const temenosCobol = `
        PROGRAM-ID. TRADE-FINANCE-CALC.
        01 TRADE-AMOUNT PIC 9(12)V99.
      `;
      
      const analysis = controller.analyzeCobolCode(temenosCobol);
      const detected = controller.detectBankingSystem(analysis);
      expect(detected).toBe('TEMENOS_TRANSACT');
    });

    test('should detect TCS BaNCS from high complexity', () => {
      // Create complex COBOL with many variables and operations
      const complexCobol = `
        DATA DIVISION.
        01 VAR1 PIC 9(8).
        01 VAR2 PIC 9(8).
        01 VAR3 PIC 9(8).
        01 VAR4 PIC 9(8).
        01 VAR5 PIC 9(8).
        
        PROCEDURE DIVISION.
        COMPUTE VAR1 = VAR2 + VAR3.
        COMPUTE VAR2 = VAR3 * VAR4.
        COMPUTE VAR3 = VAR4 / VAR5.
        IF VAR1 > 100 THEN PERFORM COMPLEX-CALC.
        IF VAR2 > 200 THEN PERFORM ANOTHER-CALC.
        IF VAR3 > 300 THEN PERFORM THIRD-CALC.
        PERFORM VAR-CHECK.
        PERFORM VAR-UPDATE.
        PERFORM VAR-VALIDATE.
        PERFORM VAR-PROCESS.
        PERFORM FINAL-CHECK.
      `;
      
      const analysis = controller.analyzeCobolCode(complexCobol);
      const detected = controller.detectBankingSystem(analysis);
      expect(detected).toBe('TCS_BANCS');
    });
  });

  describe('Blockchain Selection', () => {
    test('should select Corda for high complexity', () => {
      const analysis = { complexity: 20, patterns: [] };
      const selection = controller.selectOptimalBlockchain(analysis, 'FIS_SYSTEMATICS', mockUserContext);
      
      expect(selection.blockchain).toBe('corda');
      expect(selection.reason).toBe('high_complexity_privacy_required');
      expect(selection.alternatives).toBeDefined();
    });

    test('should select Algorand for payment processing', () => {
      const analysis = { complexity: 5, patterns: ['payment_processing'] };
      const selection = controller.selectOptimalBlockchain(analysis, 'FISERV_DNA', mockUserContext);
      
      expect(selection.blockchain).toBe('algorand');
      expect(selection.reason).toBe('payment_processing_optimized');
    });

    test('should select Ethereum for trade finance', () => {
      const analysis = { complexity: 8, patterns: ['trade_finance'] };
      const selection = controller.selectOptimalBlockchain(analysis, 'TEMENOS_TRANSACT', mockUserContext);
      
      expect(selection.blockchain).toBe('ethereum');
      expect(selection.reason).toBe('trade_finance_ecosystem');
    });

    test('should fallback to system preference', () => {
      const analysis = { complexity: 3, patterns: [] };
      const selection = controller.selectOptimalBlockchain(analysis, 'FIS_SYSTEMATICS', mockUserContext);
      
      expect(selection.reason).toBe('banking_system_preference');
      expect(selection.alternatives).toBeDefined();
    });
  });

  describe('Smart Transpilation', () => {
    test('should perform end-to-end smart transpilation', async () => {
      const options = {
        bankingSystem: 'FIS_SYSTEMATICS',
        targetBlockchain: 'ethereum'
      };
      
      const result = await controller.transpileWithSmartRouting(sampleCobolCode, options, mockUserContext);
      
      expect(result).toHaveProperty('success', true);
      expect(result).toHaveProperty('requestId');
      expect(result).toHaveProperty('transpilation');
      expect(result.transpilation).toHaveProperty('ast');
      expect(result.transpilation).toHaveProperty('smartContract');
      expect(result.transpilation).toHaveProperty('configuration');
      expect(result.transpilation).toHaveProperty('routing');
      
      expect(result.transpilation.ast.programId).toBe('MORTGAGE-CALC');
      expect(result.transpilation.smartContract.blockchain).toBe('ethereum');
    });

    test('should use smart routing when no explicit system specified', async () => {
      const options = {}; // No explicit configuration
      
      const result = await controller.transpileWithSmartRouting(sampleCobolCode, options, mockUserContext);
      
      expect(result.transpilation.routing).toHaveProperty('reason');
      expect(result.transpilation.configuration).toHaveProperty('bankingSystem');
      expect(result.transpilation.configuration).toHaveProperty('targetBlockchain');
    });

    test('should cache transpilation results', async () => {
      const options = { bankingSystem: 'FIS_SYSTEMATICS' };
      
      const result = await controller.transpileWithSmartRouting(sampleCobolCode, options, mockUserContext);
      const cached = controller.getCachedResult(result.requestId);
      
      expect(cached).toBeDefined();
      expect(cached.ast.program.programId).toBe('MORTGAGE-CALC');
      expect(cached.userContext.userId).toBe(mockUserContext.userId);
    });

    test('should update metrics on successful transpilation', async () => {
      const initialMetrics = controller.getMetrics();
      const initialSuccessful = initialMetrics.requestsSuccessful;
      
      await controller.transpileWithSmartRouting(sampleCobolCode, {}, mockUserContext);
      
      const updatedMetrics = controller.getMetrics();
      expect(updatedMetrics.requestsSuccessful).toBe(initialSuccessful + 1);
      expect(updatedMetrics.successRate).toBeGreaterThan(0);
    });

    test('should update metrics on failed transpilation', async () => {
      const initialMetrics = controller.getMetrics();
      const initialFailed = initialMetrics.requestsFailed;
      
      try {
        await controller.transpileWithSmartRouting('INVALID COBOL', {}, mockUserContext);
      } catch (error) {
        // Expected to fail
      }
      
      const updatedMetrics = controller.getMetrics();
      expect(updatedMetrics.requestsFailed).toBe(initialFailed + 1);
    });
  });

  describe('Compliance Checks', () => {
    test('should perform compliance checks on AST', async () => {
      const ast = {
        program: { programId: 'TEST-COMPLIANCE' },
        data: {
          variables: [
            { name: 'account_number', originalName: 'ACCOUNT-NUMBER' },
            { name: 'ssn', originalName: 'SSN' }
          ]
        },
        procedure: {
          operations: [
            { type: 'compute', expression: 'INTEREST * RATE' }
          ]
        }
      };
      
      const config = { bankingSystem: 'FIS_SYSTEMATICS' };
      const result = await controller.performComplianceChecks(ast, config, mockUserContext);
      
      expect(result).toHaveProperty('checks');
      expect(result).toHaveProperty('warnings');
      expect(result).toHaveProperty('errors');
      expect(result.warnings.some(w => w.includes('ssn'))).toBe(true);
      expect(result.checks.some(c => c.includes('Interest calculation'))).toBe(true);
    });

    test('should throw error for compliance violations', async () => {
      const ast = {
        program: { programId: 'TEST-VIOLATION' },
        data: { variables: [] },
        procedure: { operations: [] }
      };
      
      const config = { bankingSystem: 'FIS_SYSTEMATICS' };
      
      // Mock compliance check that adds errors
      const originalCheck = controller.performComplianceChecks;
      controller.performComplianceChecks = async () => ({
        checks: [],
        warnings: [],
        errors: ['Critical compliance violation']
      });
      
      await expect(
        controller.performComplianceChecks(ast, config, mockUserContext)
      ).rejects.toThrow('Compliance validation failed');
      
      // Restore original method
      controller.performComplianceChecks = originalCheck;
    });
  });

  describe('Configuration Optimization', () => {
    test('should optimize for BSA/AML compliance', () => {
      const config = { targetBlockchain: 'ethereum' };
      const requirements = { bsa_aml: true };
      
      const optimized = controller.optimizeForCompliance(config, requirements);
      
      expect(optimized).toHaveProperty('privacyEnhanced', true);
      expect(optimized).toHaveProperty('auditTrail', true);
    });

    test('should optimize for GDPR compliance', () => {
      const config = {};
      const requirements = { gdpr: true };
      
      const optimized = controller.optimizeForCompliance(config, requirements);
      
      expect(optimized).toHaveProperty('dataMinimization', true);
      expect(optimized).toHaveProperty('rightToErasure', true);
    });

    test('should optimize for PSD2 compliance', () => {
      const config = {};
      const requirements = { psd2: true };
      
      const optimized = controller.optimizeForCompliance(config, requirements);
      
      expect(optimized).toHaveProperty('strongAuthentication', true);
      expect(optimized).toHaveProperty('openBanking', true);
    });
  });

  describe('AST Sanitization', () => {
    test('should sanitize AST for API response', () => {
      const ast = {
        id: 'test-id',
        program: {
          programId: 'TEST-PROGRAM',
          author: 'TEST-AUTHOR',
          dateWritten: '2025-07-04'
        },
        data: {
          variables: [
            { name: 'var1', type: 'uint256' },
            { name: 'var2', type: 'string' }
          ]
        },
        procedure: {
          operations: [
            { type: 'compute', target: 'result' }
          ],
          complexity: 5,
          estimatedGas: 50000
        },
        metadata: {
          parseTime: 150,
          timestamp: '2025-07-04T12:00:00Z'
        }
      };
      
      const sanitized = controller.sanitizeASTForResponse(ast);
      
      expect(sanitized).toHaveProperty('id', 'test-id');
      expect(sanitized).toHaveProperty('programId', 'TEST-PROGRAM');
      expect(sanitized).toHaveProperty('author', 'TEST-AUTHOR');
      expect(sanitized).toHaveProperty('variableCount', 2);
      expect(sanitized).toHaveProperty('operationCount', 1);
      expect(sanitized).toHaveProperty('complexity', 5);
      expect(sanitized).toHaveProperty('estimatedGas', 50000);
      expect(sanitized).toHaveProperty('metadata');
      
      // Ensure sensitive internal data is not exposed
      expect(sanitized).not.toHaveProperty('data');
      expect(sanitized).not.toHaveProperty('procedure');
    });
  });

  describe('Cache Management', () => {
    test('should cache transpilation results', async () => {
      const result = await controller.transpileWithSmartRouting(sampleCobolCode, {}, mockUserContext);
      const cached = controller.getCachedResult(result.requestId);
      
      expect(cached).toBeDefined();
      expect(cached.ast).toBeDefined();
      expect(cached.contractResult).toBeDefined();
      expect(cached.configuration).toBeDefined();
      expect(cached.userContext.userId).toBe(mockUserContext.userId);
    });

    test('should return null for non-existent cache entries', () => {
      const cached = controller.getCachedResult('non-existent-id');
      expect(cached).toBeNull();
    });

    test('should clear expired cache entries', () => {
      // Add a test entry
      controller.requestCache.set('test-entry', {
        timestamp: new Date(Date.now() - 25 * 60 * 60 * 1000).toISOString() // 25 hours ago
      });
      
      expect(controller.requestCache.has('test-entry')).toBe(true);
      
      controller.clearExpiredCache();
      
      expect(controller.requestCache.has('test-entry')).toBe(false);
    });
  });

  describe('Metrics and Monitoring', () => {
    test('should track controller metrics', () => {
      const metrics = controller.getMetrics();
      
      expect(metrics).toHaveProperty('requestsProcessed');
      expect(metrics).toHaveProperty('requestsSuccessful');
      expect(metrics).toHaveProperty('requestsFailed');
      expect(metrics).toHaveProperty('averageProcessingTime');
      expect(metrics).toHaveProperty('successRate');
      expect(metrics).toHaveProperty('activeInstances');
      expect(metrics).toHaveProperty('cacheSize');
      expect(metrics).toHaveProperty('timestamp');
    });

    test('should calculate success rate correctly', () => {
      // Reset metrics for clean test
      controller.metrics = {
        requestsProcessed: 10,
        requestsSuccessful: 8,
        requestsFailed: 2,
        averageProcessingTime: 150
      };
      
      const metrics = controller.getMetrics();
      expect(metrics.successRate).toBe(80);
    });

    test('should track average processing time', () => {
      controller.updateMetrics(100, true);
      controller.updateMetrics(200, true);
      
      const metrics = controller.getMetrics();
      expect(metrics.averageProcessingTime).toBeGreaterThan(0);
    });
  });
});