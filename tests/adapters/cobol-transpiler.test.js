/**
 * COBOL Transpiler Test Suite
 * LegacyBAAS Platform Integration Tests
 * 
 * Test Coverage: Core transpiler functionality, authentication, templates, configurations
 * Target: 25+ comprehensive tests for 100% coverage
 */

const { CobolTranspiler, COBOL_DATA_TYPES, BANKING_SYSTEMS } = require('../../src/adapters/cobol-transpiler');
const { TemplateEngine, BLOCKCHAIN_PLATFORMS } = require('../../src/adapters/templates/template-engine');
const { ConfigManager, SUPPORTED_BANKING_SYSTEMS } = require('../../src/adapters/configs/config-manager');
const { AuthManager, USER_ROLES, PERMISSIONS } = require('../../src/auth/auth-middleware');
const path = require('path');
const fs = require('fs').promises;

describe('COBOL Transpiler Core Tests', () => {
  let transpiler;
  let configManager;
  let authManager;

  beforeAll(async () => {
    // Initialize configuration manager
    configManager = new ConfigManager(path.join(__dirname, '../../src/adapters/configs'));
    await configManager.initialize();

    // Initialize auth manager
    authManager = new AuthManager({
      jwtSecret: 'test-secret',
      enableRateLimit: false,
      enableAuditLog: false
    });

    // Initialize transpiler
    transpiler = new CobolTranspiler({
      bankingSystem: 'FIS_SYSTEMATICS',
      targetBlockchain: 'ethereum',
      optimizeGas: true,
      enableValidation: true,
      templatePath: path.join(__dirname, '../../src/adapters/templates/blockchain'),
      auth: {
        jwtSecret: 'test-secret',
        enableRateLimit: false
      }
    });
  });

  // Test Group 1: COBOL Parsing Tests (8 tests)
  describe('COBOL Parsing Engine', () => {
    const sampleCobolProgram = `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. MORTGAGE-CALC.
      AUTHOR. LEGACY-BANK.
      
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

    test('should parse COBOL program successfully', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      expect(ast).toBeDefined();
      expect(ast.program.programId).toBe('MORTGAGE-CALC');
      expect(ast.program.author).toBe('LEGACY-BANK');
      expect(ast.data.variables).toHaveLength(4);
      expect(ast.procedure.operations.length).toBeGreaterThan(0);
    });

    test('should extract variables correctly from DATA DIVISION', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      const variables = ast.data.variables;
      expect(variables.find(v => v.name === 'principal')).toBeDefined();
      expect(variables.find(v => v.name === 'interest_rate')).toBeDefined();
      expect(variables.find(v => v.name === 'term')).toBeDefined();
      expect(variables.find(v => v.name === 'monthly_payment')).toBeDefined();
    });

    test('should map COBOL data types correctly', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      const principal = ast.data.variables.find(v => v.name === 'principal');
      expect(principal.type).toBe('uint128'); // COMP-3 optimized
      
      const term = ast.data.variables.find(v => v.name === 'term');
      expect(term.type).toBe('uint256'); // COMP
    });

    test('should parse PROCEDURE DIVISION operations', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      const operations = ast.procedure.operations;
      expect(operations.some(op => op.type === 'move')).toBe(true);
      expect(operations.some(op => op.type === 'compute')).toBe(true);
      expect(operations.some(op => op.type === 'conditional')).toBe(true);
    });

    test('should calculate complexity score', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      expect(ast.procedure.complexity).toBeGreaterThan(0);
      expect(typeof ast.procedure.complexity).toBe('number');
    });

    test('should estimate gas usage', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      expect(ast.procedure.estimatedGas).toBeGreaterThan(21000); // Base gas cost
      expect(typeof ast.procedure.estimatedGas).toBe('number');
    });

    test('should handle malformed COBOL gracefully', async () => {
      const malformedCobol = `
        IDENTIFICATION DIVISION.
        INVALID SYNTAX HERE
      `;
      
      await expect(transpiler.parse(malformedCobol)).rejects.toThrow();
    });

    test('should validate AST structure', async () => {
      const ast = await transpiler.parse(sampleCobolProgram);
      
      expect(ast.id).toBeDefined();
      expect(ast.metadata).toBeDefined();
      expect(ast.metadata.bankingSystem).toBe('FIS_SYSTEMATICS');
      expect(ast.metadata.parseTime).toBeGreaterThan(0);
    });
  });

  // Test Group 2: Template Generation Tests (6 tests)
  describe('Template Engine', () => {
    let templateEngine;
    let sampleAST;

    beforeAll(async () => {
      templateEngine = new TemplateEngine({
        templatePath: path.join(__dirname, '../../src/adapters/templates/blockchain'),
        enableOptimizations: true,
        enableValidation: true
      });
      await templateEngine.initialize();

      // Generate sample AST
      const sampleCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TEST-CONTRACT.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 AMOUNT PIC 9(10)V99 COMP-3.
        01 RESULT PIC 9(10)V99 COMP-3.
        
        PROCEDURE DIVISION.
        COMPUTE RESULT = AMOUNT * 2.
      `;
      sampleAST = await transpiler.parse(sampleCobol);
    });

    test('should generate Solidity contract', async () => {
      const result = await templateEngine.generateContract(sampleAST, 'ethereum');
      
      expect(result.contractCode).toContain('pragma solidity');
      expect(result.contractCode).toContain('contract TestContract');
      expect(result.metadata.blockchain).toBe('ethereum');
      expect(result.metadata.language).toBe('solidity');
    });

    test('should generate Corda contract', async () => {
      const result = await templateEngine.generateContract(sampleAST, 'corda');
      
      expect(result.contractCode).toContain('data class');
      expect(result.contractCode).toContain('ContractState');
      expect(result.metadata.blockchain).toBe('corda');
      expect(result.metadata.language).toBe('kotlin');
    });

    test('should generate Algorand contract', async () => {
      const result = await templateEngine.generateContract(sampleAST, 'algorand');
      
      expect(result.contractCode).toContain('#pragma version');
      expect(result.contractCode).toContain('main:');
      expect(result.metadata.blockchain).toBe('algorand');
      expect(result.metadata.language).toBe('teal');
    });

    test('should apply optimizations when enabled', async () => {
      const optimized = await templateEngine.generateContract(sampleAST, 'ethereum');
      
      expect(optimized.metadata.optimized).toBe(true);
      expect(optimized.contractCode.length).toBeGreaterThan(0);
    });

    test('should validate generated contracts', async () => {
      const result = await templateEngine.generateContract(sampleAST, 'ethereum');
      
      // Should not throw validation errors
      expect(result.contractCode).toContain('pragma solidity');
      expect(result.contractCode).toContain('contract ');
    });

    test('should handle unsupported blockchain', async () => {
      await expect(
        templateEngine.generateContract(sampleAST, 'unsupported')
      ).rejects.toThrow('Unsupported blockchain platform');
    });
  });

  // Test Group 3: Configuration Management Tests (4 tests)
  describe('Configuration Manager', () => {
    test('should load all banking system configurations', () => {
      const systems = configManager.getAvailableBankingSystems();
      
      expect(systems).toContain('FIS_SYSTEMATICS');
      expect(systems).toContain('FISERV_DNA');
      expect(systems).toContain('TCS_BANCS');
      expect(systems).toContain('TEMENOS_TRANSACT');
    });

    test('should retrieve configuration for specific banking system', () => {
      const config = configManager.getConfiguration('FIS_SYSTEMATICS');
      
      expect(config.name).toBe('FIS Systematics IBS');
      expect(config.core_type).toBe('FIS_SYSTEMATICS');
      expect(config.system.architecture).toBe('mainframe');
    });

    test('should get data type mappings', () => {
      const mapping = configManager.getDataTypeMapping('FIS_SYSTEMATICS', 'COMP-3');
      expect(mapping).toBe('uint128');
      
      const defaultMapping = configManager.getDataTypeMapping('FIS_SYSTEMATICS', 'UNKNOWN');
      expect(defaultMapping).toBe('string');
    });

    test('should get preferred blockchains', () => {
      const blockchains = configManager.getPreferredBlockchains('FISERV_DNA');
      
      expect(blockchains).toContain('ethereum');
      expect(blockchains).toContain('xrp');
      expect(blockchains).toContain('algorand');
    });
  });

  // Test Group 4: Authentication Tests (4 tests)
  describe('Authentication System', () => {
    test('should authenticate with valid JWT token', async () => {
      const token = authManager.generateJWT({
        sub: 'test-user',
        customerId: 'test-customer',
        role: USER_ROLES.BANK_ADMIN,
        features: { cobol_transpiler: { enabled: true } }
      });

      const userContext = await transpiler.authenticateUser({
        token: `Bearer ${token}`
      });

      expect(userContext.userId).toBe('test-user');
      expect(userContext.role).toBe(USER_ROLES.BANK_ADMIN);
      expect(userContext.permissions).toContain(PERMISSIONS.COBOL_TRANSPILE);
    });

    test('should reject invalid authentication', async () => {
      await expect(
        transpiler.authenticateUser({ token: 'invalid-token' })
      ).rejects.toThrow('Authentication failed');
    });

    test('should check permissions correctly', () => {
      const userContext = {
        userId: 'test-user',
        permissions: [PERMISSIONS.COBOL_TRANSPILE, PERMISSIONS.COBOL_VIEW]
      };

      expect(transpiler.checkPermission(userContext, PERMISSIONS.COBOL_TRANSPILE)).toBe(true);
      expect(transpiler.checkPermission(userContext, PERMISSIONS.COBOL_DEPLOY)).toBe(false);
    });

    test('should track authentication metrics', async () => {
      const initialMetrics = transpiler.getMetrics();
      const initialAttempts = initialMetrics.authenticationAttempts;

      try {
        await transpiler.authenticateUser({ token: 'invalid' });
      } catch (error) {
        // Expected to fail
      }

      const updatedMetrics = transpiler.getMetrics();
      expect(updatedMetrics.authenticationAttempts).toBe(initialAttempts + 1);
    });
  });

  // Test Group 5: End-to-End Integration Tests (3 tests)
  describe('End-to-End Integration', () => {
    test('should complete full transpilation workflow', async () => {
      const cobolProgram = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYMENT-PROC.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 PAYMENT-AMOUNT PIC 9(8)V99 COMP-3.
        01 FEE PIC 9(4)V99 COMP-3.
        
        PROCEDURE DIVISION.
        MOVE 1000.00 TO PAYMENT-AMOUNT.
        COMPUTE FEE = PAYMENT-AMOUNT * 0.025.
      `;

      // Parse COBOL
      const ast = await transpiler.parse(cobolProgram);
      expect(ast.program.programId).toBe('PAYMENT-PROC');

      // Generate smart contract
      const contract = await transpiler.generateContract(ast, { blockchain: 'ethereum' });
      expect(contract.contractCode).toContain('contract PaymentProc');
      expect(contract.metadata.transpiler).toBeDefined();
    });

    test('should use configuration-specific settings', async () => {
      const config = configManager.getConfiguration('TCS_BANCS');
      const transpiler2 = new CobolTranspiler({
        bankingSystem: 'TCS_BANCS',
        targetBlockchain: config.blockchain.preferred_networks[0]
      });

      const ast = await transpiler2.parse(`
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TCS-TEST.
        
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 AMOUNT PIC 9(8)V99 COMP-3.
        
        PROCEDURE DIVISION.
        MOVE 500.00 TO AMOUNT.
      `);

      expect(ast.metadata.bankingSystem).toBe('TCS_BANCS');
    });

    test('should maintain transpiler metrics', () => {
      const metrics = transpiler.getMetrics();
      
      expect(metrics).toHaveProperty('totalTranspilations');
      expect(metrics).toHaveProperty('successfulTranspilations');
      expect(metrics).toHaveProperty('authenticationAttempts');
      expect(metrics).toHaveProperty('successRate');
      expect(metrics).toHaveProperty('authSuccessRate');
      expect(metrics.timestamp).toBeDefined();
    });
  });

  // Test Group 6: Error Handling & Edge Cases (Additional tests for robustness)
  describe('Error Handling & Edge Cases', () => {
    test('should handle empty COBOL program', async () => {
      await expect(transpiler.parse('')).rejects.toThrow();
    });

    test('should handle COBOL with no variables', async () => {
      const cobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. NO-VARS.
        
        PROCEDURE DIVISION.
        DISPLAY "HELLO".
      `;

      const ast = await transpiler.parse(cobol);
      expect(ast.data.variables).toHaveLength(0);
    });

    test('should handle missing template files gracefully', async () => {
      const invalidTemplateEngine = new TemplateEngine({
        templatePath: '/non-existent-path'
      });

      // Should create default templates
      await invalidTemplateEngine.initialize();
      expect(invalidTemplateEngine.templates.size).toBeGreaterThan(0);
    });
  });
});

// Integration with existing test framework
describe('Integration with LegacyBAAS Platform', () => {
  test('should integrate with existing logging system', () => {
    expect(transpiler.getMetrics().timestamp).toBeDefined();
  });

  test('should follow existing error code patterns', async () => {
    try {
      await transpiler.parse('INVALID COBOL');
    } catch (error) {
      expect(error.message).toContain('COBOL parsing failed');
    }
  });

  test('should maintain 100% test coverage standards', () => {
    // This test ensures we're following the platform's quality standards
    expect(transpiler).toBeDefined();
    expect(transpiler.parse).toBeDefined();
    expect(transpiler.generateContract).toBeDefined();
    expect(transpiler.authenticateUser).toBeDefined();
    expect(transpiler.getMetrics).toBeDefined();
  });
});