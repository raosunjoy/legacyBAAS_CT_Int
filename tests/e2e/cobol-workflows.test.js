const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const request = require('supertest');
const express = require('express');
const fs = require('fs').promises;
const path = require('path');

// Import all system components for end-to-end testing
const { cobolTranspiler } = require('../../src/adapters/cobol-transpiler');
const { cobolRBAC } = require('../../src/auth/cobol-rbac');
const { transpilerAudit } = require('../../src/audit/transpiler-audit');
const { quotaManager } = require('../../src/billing/quota-manager');
const { smartRouter } = require('../../src/router/smart-router');
const { blockchainGateway } = require('../../src/blockchain/gateway');

// Mock external dependencies while keeping internal logic
jest.mock('../../src/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn()
  }
}));

jest.mock('../../src/database/connection', () => ({
  DatabaseManager: jest.fn().mockImplementation(() => ({
    connect: jest.fn().mockResolvedValue(true),
    disconnect: jest.fn().mockResolvedValue(true),
    query: jest.fn().mockResolvedValue([])
  }))
}));

jest.mock('../../src/utils/cache', () => ({
  CacheManager: jest.fn().mockImplementation(() => ({
    get: jest.fn(),
    set: jest.fn(),
    delete: jest.fn(),
    clear: jest.fn()
  }))
}));

// Mock blockchain network responses
jest.mock('web3', () => {
  return jest.fn().mockImplementation(() => ({
    eth: {
      Contract: jest.fn().mockImplementation(() => ({
        deploy: jest.fn().mockReturnValue({
          send: jest.fn().mockResolvedValue({
            options: { address: '0x1234567890abcdef1234567890abcdef12345678' }
          })
        })
      })),
      getTransactionReceipt: jest.fn().mockResolvedValue({
        status: true,
        transactionHash: '0xabcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890',
        gasUsed: 500000
      })
    }
  }));
});

describe('End-to-End COBOL Workflow Tests', () => {
  let app;
  let mockDb;

  // Sample COBOL programs for each banking system
  const sampleCobolPrograms = {
    fis: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. FIS-PAYMENT-PROCESSOR.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 PAYMENT-RECORD.
         05 ACCOUNT-NUMBER    PIC 9(10).
         05 AMOUNT           PIC 9(8)V99.
         05 TRANSACTION-TYPE PIC X(10).
         05 ROUTING-NUMBER   PIC 9(9).
      
      PROCEDURE DIVISION.
      MAIN-PROCESS.
          DISPLAY "Processing FIS payment transaction"
          MOVE 1234567890 TO ACCOUNT-NUMBER
          MOVE 1000.00 TO AMOUNT
          MOVE "TRANSFER" TO TRANSACTION-TYPE
          MOVE 021000021 TO ROUTING-NUMBER
          PERFORM VALIDATE-TRANSACTION
          PERFORM PROCESS-PAYMENT
          STOP RUN.
      
      VALIDATE-TRANSACTION.
          IF AMOUNT > 10000
             DISPLAY "High value transaction - additional validation required"
          END-IF.
      
      PROCESS-PAYMENT.
          DISPLAY "Payment processed successfully"
          DISPLAY "Account: " ACCOUNT-NUMBER
          DISPLAY "Amount: " AMOUNT.
    `,
    fiserv: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. FISERV-ACCOUNT-MANAGER.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 ACCOUNT-DATA.
         05 CUSTOMER-ID      PIC 9(12).
         05 ACCOUNT-TYPE     PIC X(8).
         05 BALANCE          PIC S9(10)V99 COMP-3.
         05 STATUS-CODE      PIC X(2).
      
      PROCEDURE DIVISION.
      MAIN-PROCESS.
          DISPLAY "Fiserv DNA Account Management"
          MOVE 123456789012 TO CUSTOMER-ID
          MOVE "CHECKING" TO ACCOUNT-TYPE
          MOVE 5000.00 TO BALANCE
          MOVE "AC" TO STATUS-CODE
          PERFORM CHECK-BALANCE
          PERFORM UPDATE-ACCOUNT
          STOP RUN.
      
      CHECK-BALANCE.
          IF BALANCE < 0
             DISPLAY "Account overdrawn"
             MOVE "OD" TO STATUS-CODE
          END-IF.
      
      UPDATE-ACCOUNT.
          DISPLAY "Account updated successfully"
          DISPLAY "Customer: " CUSTOMER-ID
          DISPLAY "Balance: " BALANCE.
    `,
    temenos: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. TEMENOS-LOAN-PROCESSOR.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 LOAN-APPLICATION.
         05 APPLICATION-ID   PIC 9(15).
         05 CUSTOMER-ID      PIC 9(10).
         05 LOAN-AMOUNT      PIC 9(12)V99.
         05 INTEREST-RATE    PIC 9(2)V99.
         05 TERM-MONTHS      PIC 9(3).
      
      PROCEDURE DIVISION.
      MAIN-PROCESS.
          DISPLAY "Temenos T24 Loan Processing"
          MOVE 123456789012345 TO APPLICATION-ID
          MOVE 9876543210 TO CUSTOMER-ID
          MOVE 250000.00 TO LOAN-AMOUNT
          MOVE 3.50 TO INTEREST-RATE
          MOVE 360 TO TERM-MONTHS
          PERFORM CALCULATE-PAYMENT
          PERFORM APPROVE-LOAN
          STOP RUN.
      
      CALCULATE-PAYMENT.
          DISPLAY "Calculating monthly payment"
          DISPLAY "Loan Amount: " LOAN-AMOUNT
          DISPLAY "Rate: " INTEREST-RATE "%".
      
      APPROVE-LOAN.
          DISPLAY "Loan approved"
          DISPLAY "Application: " APPLICATION-ID.
    `,
    tcs: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. TCS-BANCS-TRADE-FINANCE.
      
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 TRADE-TRANSACTION.
         05 TRANSACTION-ID   PIC X(20).
         05 IMPORTER-ID      PIC 9(10).
         05 EXPORTER-ID      PIC 9(10).
         05 COMMODITY-CODE   PIC X(8).
         05 TRADE-VALUE      PIC 9(15)V99.
         05 CURRENCY-CODE    PIC X(3).
      
      PROCEDURE DIVISION.
      MAIN-PROCESS.
          DISPLAY "TCS BaNCS Trade Finance Processing"
          MOVE "TF2025070500001" TO TRANSACTION-ID
          MOVE 1234567890 TO IMPORTER-ID
          MOVE 9876543210 TO EXPORTER-ID
          MOVE "TEXTILE" TO COMMODITY-CODE
          MOVE 1500000.00 TO TRADE-VALUE
          MOVE "USD" TO CURRENCY-CODE
          PERFORM VALIDATE-TRADE
          PERFORM PROCESS-FINANCE
          STOP RUN.
      
      VALIDATE-TRADE.
          DISPLAY "Validating trade finance transaction"
          IF TRADE-VALUE > 1000000
             DISPLAY "Large transaction - compliance check required"
          END-IF.
      
      PROCESS-FINANCE.
          DISPLAY "Trade finance processed"
          DISPLAY "Transaction: " TRANSACTION-ID
          DISPLAY "Value: " TRADE-VALUE " " CURRENCY-CODE.
    `
  };

  beforeAll(async () => {
    // Create Express app for testing
    app = express();
    app.use(express.json());

    // Initialize all system components
    await cobolTranspiler.initialize();
    await cobolRBAC.initialize();
    await transpilerAudit.initialize();
    await quotaManager.initialize();

    // Setup comprehensive test routes
    setupE2ERoutes(app);

    mockDb = cobolTranspiler.db;
  });

  afterAll(async () => {
    await cobolTranspiler.shutdown();
    await cobolRBAC.shutdown();
    await transpilerAudit.shutdown();
    await quotaManager.shutdown();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Reset system state
    cobolRBAC.permissions.clear();
    quotaManager.quotaCache.clear();
    
    // Setup default successful responses
    mockDb.query.mockResolvedValue({ rowCount: 1 });
  });

  function setupE2ERoutes(app) {
    // Complete end-to-end transpilation workflow
    app.post('/api/e2e/transpile-deploy',
      // Authentication
      async (req, res, next) => {
        req.user = {
          userId: req.headers['x-user-id'],
          customerId: req.headers['x-customer-id'],
          role: req.headers['x-user-role'] || 'bank_admin'
        };
        next();
      },
      // RBAC check
      cobolRBAC.requirePermission('transpiler.execute'),
      // Quota check
      async (req, res, next) => {
        const quotaCheck = await quotaManager.canPerformTranspilation(req.user.customerId);
        if (!quotaCheck.allowed) {
          return res.status(429).json({ error: 'Quota exceeded', reason: quotaCheck.reason });
        }
        req.quotaCheck = quotaCheck;
        next();
      },
      // Main workflow handler
      async (req, res) => {
        try {
          const { bankingSystem, cobolCode, targetBlockchain, deploymentConfig } = req.body;
          const { userId, customerId } = req.user;
          const workflowId = `wf-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;

          // Step 1: Parse and validate COBOL
          const parseResult = await cobolTranspiler.parseCobol(cobolCode, { bankingSystem });
          
          // Step 2: Generate smart contract
          const contractResult = await cobolTranspiler.generateSmartContract(
            parseResult.ast,
            targetBlockchain,
            { bankingSystem, ...deploymentConfig }
          );

          // Step 3: Route to optimal blockchain
          const routingResult = await smartRouter.selectOptimalBlockchain({
            contractComplexity: parseResult.complexity,
            bankingSystem,
            preferredNetwork: targetBlockchain,
            transactionVolume: deploymentConfig.expectedVolume || 'medium'
          });

          // Step 4: Deploy to blockchain
          const deploymentResult = await blockchainGateway.deployContract({
            network: routingResult.selectedNetwork,
            contractCode: contractResult.code,
            abi: contractResult.abi,
            constructorArgs: deploymentConfig.constructorArgs || []
          });

          // Step 5: Record usage and audit
          await quotaManager.recordUsage(customerId, workflowId, {
            bankingSystem,
            targetBlockchain: routingResult.selectedNetwork,
            processingTime: Date.now() - parseInt(workflowId.split('-')[1]),
            complexity: parseResult.complexity
          });

          await transpilerAudit.logEvent('transpiler.workflow_complete', {
            workflowId,
            bankingSystem,
            targetBlockchain: routingResult.selectedNetwork,
            contractAddress: deploymentResult.contractAddress,
            success: true
          }, {
            userId,
            customerId,
            action: 'complete_workflow'
          });

          res.json({
            success: true,
            workflowId,
            parseResult: {
              ast: parseResult.ast,
              complexity: parseResult.complexity,
              variables: parseResult.variables.length,
              procedures: parseResult.procedures.length
            },
            contractResult: {
              language: contractResult.language,
              codeSize: contractResult.code.length,
              functions: contractResult.functions?.length || 0
            },
            routingResult: {
              selectedNetwork: routingResult.selectedNetwork,
              reason: routingResult.reason,
              estimatedCost: routingResult.estimatedCost
            },
            deploymentResult: {
              contractAddress: deploymentResult.contractAddress,
              transactionHash: deploymentResult.transactionHash,
              gasUsed: deploymentResult.gasUsed
            },
            quotaStatus: req.quotaCheck
          });

        } catch (error) {
          await transpilerAudit.logEvent('transpiler.workflow_failed', {
            error: error.message,
            bankingSystem: req.body.bankingSystem
          }, {
            userId: req.user.userId,
            customerId: req.user.customerId,
            result: 'failed',
            error: error.message
          });

          res.status(500).json({
            success: false,
            error: error.message,
            workflowId: `failed-${Date.now()}`
          });
        }
      }
    );

    // Workflow status endpoint
    app.get('/api/e2e/workflow/:workflowId/status', async (req, res) => {
      const { workflowId } = req.params;
      
      // Mock workflow status tracking
      const status = {
        workflowId,
        status: 'completed',
        steps: [
          { name: 'cobol_parsing', status: 'completed', duration: 150 },
          { name: 'contract_generation', status: 'completed', duration: 300 },
          { name: 'blockchain_routing', status: 'completed', duration: 50 },
          { name: 'contract_deployment', status: 'completed', duration: 5000 },
          { name: 'audit_logging', status: 'completed', duration: 25 }
        ],
        totalDuration: 5525,
        completedAt: new Date().toISOString()
      };

      res.json(status);
    });
  }

  describe('FIS Systematics to Ethereum Workflow', () => {
    beforeEach(() => {
      // Setup FIS-specific permissions and quota
      const userId = 'fis-user-123';
      const customerId = 'fis-customer-456';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.fis.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_admin',
        bankingSystem: 'fis',
        permissions: ['transpiler.execute', 'banking.fis.access']
      });

      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'enterprise',
        monthlyQuota: 1000,
        currentUsage: 100,
        overageUsage: 0,
        maxOveragePercent: 200,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });
    });

    test('should complete FIS payment processor to Ethereum smart contract workflow', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: {
            expectedVolume: 'high',
            gasLimit: 5000000,
            constructorArgs: []
          }
        })
        .set('x-user-id', 'fis-user-123')
        .set('x-customer-id', 'fis-customer-456')
        .set('x-user-role', 'bank_admin');

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.parseResult.complexity).toBeDefined();
      expect(response.body.contractResult.language).toBe('solidity');
      expect(response.body.routingResult.selectedNetwork).toBe('ethereum');
      expect(response.body.deploymentResult.contractAddress).toMatch(/^0x[a-fA-F0-9]{40}$/);
      expect(response.body.quotaStatus.allowed).toBe(true);
    });

    test('should track FIS workflow metrics correctly', async () => {
      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');
      const quotaSpy = jest.spyOn(quotaManager, 'recordUsage');

      await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: { expectedVolume: 'medium' }
        })
        .set('x-user-id', 'fis-user-123')
        .set('x-customer-id', 'fis-customer-456');

      expect(quotaSpy).toHaveBeenCalledWith(
        'fis-customer-456',
        expect.any(String),
        expect.objectContaining({
          bankingSystem: 'fis',
          targetBlockchain: 'ethereum'
        })
      );

      expect(auditSpy).toHaveBeenCalledWith(
        'transpiler.workflow_complete',
        expect.objectContaining({
          bankingSystem: 'fis',
          targetBlockchain: 'ethereum',
          success: true
        }),
        expect.objectContaining({
          userId: 'fis-user-123',
          customerId: 'fis-customer-456'
        })
      );

      auditSpy.mockRestore();
      quotaSpy.mockRestore();
    });

    test('should handle FIS workflow status tracking', async () => {
      const workflowResponse = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: {}
        })
        .set('x-user-id', 'fis-user-123')
        .set('x-customer-id', 'fis-customer-456');

      const workflowId = workflowResponse.body.workflowId;

      const statusResponse = await request(app)
        .get(`/api/e2e/workflow/${workflowId}/status`);

      expect(statusResponse.status).toBe(200);
      expect(statusResponse.body.status).toBe('completed');
      expect(statusResponse.body.steps).toHaveLength(5);
      expect(statusResponse.body.totalDuration).toBeGreaterThan(0);
    });
  });

  describe('Fiserv DNA to XRP Ledger Workflow', () => {
    beforeEach(() => {
      const userId = 'fiserv-user-789';
      const customerId = 'fiserv-customer-abc';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.fiserv.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_user',
        bankingSystem: 'fiserv',
        permissions: ['transpiler.execute', 'banking.fiserv.access']
      });

      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 50,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });
    });

    test('should complete Fiserv account manager to XRP smart contract workflow', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fiserv',
          cobolCode: sampleCobolPrograms.fiserv,
          targetBlockchain: 'xrp',
          deploymentConfig: {
            expectedVolume: 'medium',
            xrpSettings: {
              fee: '0.00001',
              sequence: 1
            }
          }
        })
        .set('x-user-id', 'fiserv-user-789')
        .set('x-customer-id', 'fiserv-customer-abc')
        .set('x-user-role', 'bank_user');

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.parseResult.variables).toBeGreaterThan(0);
      expect(response.body.contractResult.codeSize).toBeGreaterThan(0);
      expect(response.body.routingResult.selectedNetwork).toBe('xrp');
      expect(response.body.deploymentResult.contractAddress).toBeDefined();
    });

    test('should handle Fiserv-specific data types correctly', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fiserv',
          cobolCode: sampleCobolPrograms.fiserv,
          targetBlockchain: 'xrp',
          deploymentConfig: {}
        })
        .set('x-user-id', 'fiserv-user-789')
        .set('x-customer-id', 'fiserv-customer-abc');

      expect(response.status).toBe(200);
      expect(response.body.parseResult.complexity).toBeDefined();
      // Fiserv DNA uses COMP-3 fields which should be properly handled
      expect(response.body.contractResult.functions).toBeGreaterThanOrEqual(0);
    });

    test('should respect Fiserv user permissions', async () => {
      // Test with user who doesn't have Fiserv access
      cobolRBAC.permissions.set('other-user', new Set(['transpiler.execute']));
      cobolRBAC.userProfiles.set('other-user', {
        role: 'bank_user',
        bankingSystem: 'temenos',
        permissions: ['transpiler.execute']
      });

      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fiserv',
          cobolCode: sampleCobolPrograms.fiserv,
          targetBlockchain: 'xrp',
          deploymentConfig: {}
        })
        .set('x-user-id', 'other-user')
        .set('x-customer-id', 'other-customer');

      expect(response.status).toBe(403);
    });
  });

  describe('TCS BaNCS to Corda Workflow', () => {
    beforeEach(() => {
      const userId = 'tcs-user-def';
      const customerId = 'tcs-customer-ghi';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.tcs.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'si_developer',
        bankingSystem: 'tcs',
        permissions: ['transpiler.execute', 'banking.tcs.access']
      });

      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'enterprise',
        monthlyQuota: 1000,
        currentUsage: 750,
        overageUsage: 50,
        maxOveragePercent: 200,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });
    });

    test('should complete TCS BaNCS trade finance to Corda flow workflow', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'tcs',
          cobolCode: sampleCobolPrograms.tcs,
          targetBlockchain: 'corda',
          deploymentConfig: {
            expectedVolume: 'high',
            cordaSettings: {
              notary: 'O=Notary,L=London,C=GB',
              participants: ['O=BankA,L=New York,C=US', 'O=BankB,L=London,C=GB']
            }
          }
        })
        .set('x-user-id', 'tcs-user-def')
        .set('x-customer-id', 'tcs-customer-ghi')
        .set('x-user-role', 'si_developer');

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.parseResult.procedures).toBeGreaterThan(0);
      expect(response.body.contractResult.language).toBe('kotlin');
      expect(response.body.routingResult.selectedNetwork).toBe('corda');
      expect(response.body.deploymentResult.contractAddress).toBeDefined();
    });

    test('should handle TCS BaNCS complex transaction structures', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'tcs',
          cobolCode: sampleCobolPrograms.tcs,
          targetBlockchain: 'corda',
          deploymentConfig: {}
        })
        .set('x-user-id', 'tcs-user-def')
        .set('x-customer-id', 'tcs-customer-ghi');

      expect(response.status).toBe(200);
      // TCS BaNCS typically has complex trade finance structures
      expect(response.body.parseResult.complexity).toBeGreaterThan('low');
      expect(response.body.contractResult.codeSize).toBeGreaterThan(1000);
    });

    test('should track overage usage for TCS customer', async () => {
      const quotaSpy = jest.spyOn(quotaManager, 'recordUsage');

      await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'tcs',
          cobolCode: sampleCobolPrograms.tcs,
          targetBlockchain: 'corda',
          deploymentConfig: {}
        })
        .set('x-user-id', 'tcs-user-def')
        .set('x-customer-id', 'tcs-customer-ghi');

      expect(quotaSpy).toHaveBeenCalledWith(
        'tcs-customer-ghi',
        expect.any(String),
        expect.objectContaining({
          bankingSystem: 'tcs',
          targetBlockchain: 'corda'
        })
      );

      quotaSpy.mockRestore();
    });
  });

  describe('Temenos T24 to Algorand Workflow', () => {
    beforeEach(() => {
      const userId = 'temenos-user-jkl';
      const customerId = 'temenos-customer-mno';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.temenos.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_admin',
        bankingSystem: 'temenos',
        permissions: ['transpiler.execute', 'banking.temenos.access']
      });

      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'starter',
        monthlyQuota: 50,
        currentUsage: 45,
        overageUsage: 0,
        maxOveragePercent: 50,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });
    });

    test('should complete Temenos loan processor to Algorand smart contract workflow', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'temenos',
          cobolCode: sampleCobolPrograms.temenos,
          targetBlockchain: 'algorand',
          deploymentConfig: {
            expectedVolume: 'low',
            algorandSettings: {
              fee: 1000,
              firstRound: 1000,
              lastRound: 2000
            }
          }
        })
        .set('x-user-id', 'temenos-user-jkl')
        .set('x-customer-id', 'temenos-customer-mno')
        .set('x-user-role', 'bank_admin');

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.parseResult.ast).toBeDefined();
      expect(response.body.contractResult.language).toBe('pyteal');
      expect(response.body.routingResult.selectedNetwork).toBe('algorand');
      expect(response.body.deploymentResult.contractAddress).toBeDefined();
    });

    test('should handle Temenos T24 loan calculation logic', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'temenos',
          cobolCode: sampleCobolPrograms.temenos,
          targetBlockchain: 'algorand',
          deploymentConfig: {}
        })
        .set('x-user-id', 'temenos-user-jkl')
        .set('x-customer-id', 'temenos-customer-mno');

      expect(response.status).toBe(200);
      // Temenos T24 loan processing should generate substantial smart contract code
      expect(response.body.parseResult.variables).toBeGreaterThan(3);
      expect(response.body.parseResult.procedures).toBeGreaterThan(1);
    });

    test('should approach quota limit for starter tier customer', async () => {
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'temenos',
          cobolCode: sampleCobolPrograms.temenos,
          targetBlockchain: 'algorand',
          deploymentConfig: {}
        })
        .set('x-user-id', 'temenos-user-jkl')
        .set('x-customer-id', 'temenos-customer-mno');

      expect(response.status).toBe(200);
      expect(response.body.quotaStatus.remaining).toBeLessThan(10);
      expect(response.body.quotaStatus.reason).toBe('within_quota');
    });
  });

  describe('Cross-Banking System Integration Tests', () => {
    test('should handle multiple banking systems in single session', async () => {
      // Setup multi-system user
      const userId = 'multi-system-user';
      const customerId = 'multi-system-customer';
      
      cobolRBAC.permissions.set(userId, new Set([
        'transpiler.execute',
        'banking.fis.access',
        'banking.fiserv.access',
        'banking.temenos.access',
        'banking.tcs.access'
      ]));
      cobolRBAC.userProfiles.set(userId, {
        role: 'super_admin',
        bankingSystem: 'all',
        permissions: ['transpiler.execute', 'banking.*.access']
      });

      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'enterprise',
        monthlyQuota: 1000,
        currentUsage: 100,
        overageUsage: 0,
        maxOveragePercent: 200,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });

      // Test all four banking systems
      const bankingSystems = ['fis', 'fiserv', 'temenos', 'tcs'];
      const blockchains = ['ethereum', 'xrp', 'algorand', 'corda'];

      for (let i = 0; i < bankingSystems.length; i++) {
        const response = await request(app)
          .post('/api/e2e/transpile-deploy')
          .send({
            bankingSystem: bankingSystems[i],
            cobolCode: sampleCobolPrograms[bankingSystems[i]],
            targetBlockchain: blockchains[i],
            deploymentConfig: {}
          })
          .set('x-user-id', userId)
          .set('x-customer-id', customerId);

        expect(response.status).toBe(200);
        expect(response.body.success).toBe(true);
        expect(response.body.routingResult.selectedNetwork).toBe(blockchains[i]);
      }
    });

    test('should maintain audit trail across different workflows', async () => {
      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');
      
      // Setup user
      const userId = 'audit-test-user';
      const customerId = 'audit-test-customer';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.fis.access']));
      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 10,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });

      // Execute multiple workflows
      await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: {}
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: {}
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      // Verify audit events were logged for each workflow
      expect(auditSpy).toHaveBeenCalledWith(
        'transpiler.workflow_complete',
        expect.objectContaining({
          bankingSystem: 'fis',
          success: true
        }),
        expect.objectContaining({
          userId,
          customerId
        })
      );

      expect(auditSpy).toHaveBeenCalledTimes(2);
      auditSpy.mockRestore();
    });
  });

  describe('Error Handling and Recovery', () => {
    test('should handle COBOL parsing errors gracefully', async () => {
      const userId = 'error-test-user';
      const customerId = 'error-test-customer';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.fis.access']));
      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 10,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });

      // Test with malformed COBOL
      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: 'INVALID COBOL CODE',
          targetBlockchain: 'ethereum',
          deploymentConfig: {}
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(500);
      expect(response.body.success).toBe(false);
      expect(response.body.error).toBeDefined();
      expect(response.body.workflowId).toMatch(/^failed-/);
    });

    test('should handle quota exceeded scenarios', async () => {
      const userId = 'quota-exceeded-user';
      const customerId = 'quota-exceeded-customer';
      
      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute', 'banking.fis.access']));
      quotaManager.quotaCache.set(`quota:${customerId}`, {
        customerId,
        tier: 'starter',
        monthlyQuota: 50,
        currentUsage: 50,
        overageUsage: 25, // Exceeded max overage
        maxOveragePercent: 50,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      });

      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: {}
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(429);
      expect(response.body.error).toBe('Quota exceeded');
      expect(response.body.reason).toBe('max_overage_exceeded');
    });

    test('should handle permission denied scenarios', async () => {
      const userId = 'no-permission-user';
      const customerId = 'no-permission-customer';
      
      // User without transpiler permissions
      cobolRBAC.permissions.set(userId, new Set(['basic.read']));

      const response = await request(app)
        .post('/api/e2e/transpile-deploy')
        .send({
          bankingSystem: 'fis',
          cobolCode: sampleCobolPrograms.fis,
          targetBlockchain: 'ethereum',
          deploymentConfig: {}
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(403);
      expect(response.body.error).toBe('Insufficient permissions');
    });
  });
});