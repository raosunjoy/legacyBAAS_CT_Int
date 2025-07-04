/**
 * Template Engine Test Suite
 * LegacyBAAS COBOL Transpiler Integration
 * 
 * Tests template generation, Handlebars helpers, and blockchain-specific output
 */

const { TemplateEngine, BLOCKCHAIN_PLATFORMS } = require('../../src/adapters/templates/template-engine');
const path = require('path');
const fs = require('fs').promises;

describe('Template Engine Tests', () => {
  let templateEngine;
  let mockAST;

  beforeAll(async () => {
    templateEngine = new TemplateEngine({
      templatePath: path.join(__dirname, '../../src/adapters/templates/blockchain'),
      enableOptimizations: true,
      enableValidation: true
    });

    await templateEngine.initialize();

    // Create mock AST for testing
    mockAST = {
      program: {
        programId: 'TEST-PROGRAM',
        author: 'TEST-AUTHOR',
        dateWritten: '2025-07-04'
      },
      data: {
        variables: [
          {
            name: 'test_variable',
            originalName: 'TEST-VARIABLE',
            type: 'uint256',
            picture: '9(8)',
            value: null,
            level: 1
          },
          {
            name: 'amount',
            originalName: 'AMOUNT',
            type: 'uint128',
            picture: '9(10)V99 COMP-3',
            value: '1000.00',
            level: 1
          }
        ]
      },
      procedure: {
        operations: [
          {
            type: 'compute',
            target: 'result',
            expression: 'amount * 2',
            line: 10
          },
          {
            type: 'conditional',
            condition: 'amount > 500',
            action: 'DISPLAY HIGH-AMOUNT',
            line: 12
          }
        ],
        complexity: 3,
        estimatedGas: 50000
      }
    };
  });

  describe('Template Loading and Initialization', () => {
    test('should load all blockchain templates', () => {
      expect(templateEngine.templates.size).toBeGreaterThanOrEqual(3);
      expect(templateEngine.templates.has('solidity.hbs')).toBe(true);
      expect(templateEngine.templates.has('corda.hbs')).toBe(true);
      expect(templateEngine.templates.has('algorand.hbs')).toBe(true);
    });

    test('should register Handlebars helpers', () => {
      const handlebars = templateEngine.handlebars;
      expect(handlebars.helpers.capitalize).toBeDefined();
      expect(handlebars.helpers.camelCase).toBeDefined();
      expect(handlebars.helpers.if_eq).toBeDefined();
    });

    test('should handle missing template directory', async () => {
      const newEngine = new TemplateEngine({
        templatePath: '/non-existent-path'
      });

      await newEngine.initialize();
      expect(newEngine.templates.size).toBeGreaterThan(0);
    });
  });

  describe('Solidity Template Generation', () => {
    test('should generate valid Solidity contract', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.contractCode).toContain('pragma solidity ^0.8.0');
      expect(result.contractCode).toContain('contract TestProgram');
      expect(result.contractCode).toContain('uint256 public test_variable');
      expect(result.contractCode).toContain('uint128 public amount');
    });

    test('should include constructor for initialized variables', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.contractCode).toContain('constructor');
      expect(result.contractCode).toContain('_amount');
    });

    test('should generate execute function with operations', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.contractCode).toContain('function execute()');
      expect(result.contractCode).toContain('amount * 2');
      expect(result.contractCode).toContain('if (amount > 500)');
    });

    test('should apply gas optimizations', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum', {
        optimizeGas: true
      });

      expect(result.metadata.optimized).toBe(true);
      expect(result.contractCode).toContain('uint128');
    });
  });

  describe('Corda Template Generation', () => {
    test('should generate valid Corda state', async () => {
      const result = await templateEngine.generateContract(mockAST, 'corda');

      expect(result.contractCode).toContain('data class TestProgramState');
      expect(result.contractCode).toContain('ContractState');
      expect(result.contractCode).toContain('val testVariable: BigInteger');
      expect(result.contractCode).toContain('val amount: Long');
    });

    test('should include validation function', async () => {
      const result = await templateEngine.generateContract(mockAST, 'corda');

      expect(result.contractCode).toContain('fun validate()');
      expect(result.contractCode).toContain('require(');
    });

    test('should generate contract class', async () => {
      const result = await templateEngine.generateContract(mockAST, 'corda');

      expect(result.contractCode).toContain('class TestProgramContract');
      expect(result.contractCode).toContain('Contract');
      expect(result.contractCode).toContain('Commands');
    });
  });

  describe('Algorand Template Generation', () => {
    test('should generate valid TEAL contract', async () => {
      const result = await templateEngine.generateContract(mockAST, 'algorand');

      expect(result.contractCode).toContain('#pragma version 8');
      expect(result.contractCode).toContain('main:');
      expect(result.contractCode).toContain('app_global_put');
    });

    test('should include application creation logic', async () => {
      const result = await templateEngine.generateContract(mockAST, 'algorand');

      expect(result.contractCode).toContain('create_app:');
      expect(result.contractCode).toContain('int 0');
      expect(result.contractCode).toContain('==');
    });

    test('should handle COBOL operations in TEAL', async () => {
      const result = await templateEngine.generateContract(mockAST, 'algorand');

      expect(result.contractCode).toContain('execute:');
      expect(result.contractCode).toContain('byte "result"');
    });
  });

  describe('Template Context Preparation', () => {
    test('should convert variables correctly', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.context.variables).toBeDefined();
      expect(result.context.variables.length).toBe(2);
      expect(result.context.variables[0].visibility).toBe('public');
    });

    test('should convert procedures to functions', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.context.functions).toBeDefined();
      expect(result.context.functions.length).toBeGreaterThan(0);
      expect(result.context.functions[0].name).toBe('execute');
    });

    test('should calculate return types correctly', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      const executeFunction = result.context.functions.find(f => f.name === 'execute');
      expect(executeFunction.returns).toBeDefined();
      expect(['uint256', 'bool'].includes(executeFunction.returns)).toBe(true);
    });
  });

  describe('Handlebars Helpers', () => {
    test('should capitalize strings correctly', () => {
      const helper = templateEngine.handlebars.helpers.capitalize;
      expect(helper('test')).toBe('Test');
      expect(helper('TEST')).toBe('TEST');
    });

    test('should convert to camelCase correctly', () => {
      const helper = templateEngine.handlebars.helpers.camelCase;
      expect(helper('test_variable')).toBe('testVariable');
      expect(helper('my_long_variable_name')).toBe('myLongVariableName');
    });

    test('should handle conditional equality', () => {
      const helper = templateEngine.handlebars.helpers.if_eq;
      const mockOptions = {
        fn: () => 'true_branch',
        inverse: () => 'false_branch'
      };

      expect(helper('a', 'a', mockOptions)).toBe('true_branch');
      expect(helper('a', 'b', mockOptions)).toBe('false_branch');
    });
  });

  describe('Error Handling and Validation', () => {
    test('should validate contract syntax', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');
      
      // Should not throw validation errors
      expect(result.contractCode).toContain('pragma solidity');
      expect(result.contractCode).toContain('contract ');
    });

    test('should handle invalid blockchain platform', async () => {
      await expect(
        templateEngine.generateContract(mockAST, 'invalid-blockchain')
      ).rejects.toThrow('Unsupported blockchain platform');
    });

    test('should validate AST input', async () => {
      await expect(
        templateEngine.generateContract(null, 'ethereum')
      ).rejects.toThrow('Invalid AST provided');

      await expect(
        templateEngine.generateContract({}, 'ethereum')
      ).rejects.toThrow('AST missing program ID');
    });

    test('should handle missing template gracefully', async () => {
      // Remove a template temporarily
      const originalTemplate = templateEngine.templates.get('solidity.hbs');
      templateEngine.templates.delete('solidity.hbs');

      await expect(
        templateEngine.generateContract(mockAST, 'ethereum')
      ).rejects.toThrow('Template not found');

      // Restore template
      templateEngine.templates.set('solidity.hbs', originalTemplate);
    });
  });

  describe('Optimization and Performance', () => {
    test('should apply optimizations when enabled', async () => {
      const optimized = await templateEngine.generateContract(mockAST, 'ethereum');
      const unoptimized = new TemplateEngine({ enableOptimizations: false });
      await unoptimized.initialize();
      const unoptimizedResult = await unoptimized.generateContract(mockAST, 'ethereum');

      expect(optimized.metadata.optimized).toBe(true);
      expect(unoptimizedResult.metadata.optimized).toBe(false);
    });

    test('should track generation time', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.metadata.generationTime).toBeGreaterThan(0);
      expect(typeof result.metadata.generationTime).toBe('number');
    });

    test('should include complexity and gas estimates', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum');

      expect(result.metadata.complexity).toBe(3);
      expect(result.metadata.estimatedGas).toBe(50000);
    });
  });

  describe('Blockchain-Specific Features', () => {
    test('should handle Ethereum-specific features', async () => {
      const result = await templateEngine.generateContract(mockAST, 'ethereum', {
        enableEvents: true,
        enableModifiers: true
      });

      expect(result.contractCode).toContain('event ');
      expect(result.contractCode).toContain('modifier ');
    });

    test('should adapt to different blockchain capabilities', async () => {
      const ethResult = await templateEngine.generateContract(mockAST, 'ethereum');
      const cordaResult = await templateEngine.generateContract(mockAST, 'corda');

      expect(ethResult.metadata.features).toContain('evm_compatible');
      expect(cordaResult.metadata.features).toContain('privacy_focused');
    });

    test('should use appropriate data types per blockchain', async () => {
      const ethResult = await templateEngine.generateContract(mockAST, 'ethereum');
      const cordaResult = await templateEngine.generateContract(mockAST, 'corda');

      expect(ethResult.contractCode).toContain('uint256');
      expect(cordaResult.contractCode).toContain('BigInteger');
    });
  });
});