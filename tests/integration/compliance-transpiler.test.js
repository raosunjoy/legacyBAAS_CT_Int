/**
 * Compliance Engine Integration Tests
 * Tests for COBOL Transpiler compliance screening integration
 * 
 * Task 2.2: Compliance Engine Integration
 * Testing AML/KYC, sanctions screening, FATF Travel Rule, and risk scoring
 */

const { CobolTranspiler } = require('../../src/adapters/cobol-transpiler');
const { ZKProofComplianceService } = require('../../src/compliance/zk-proof-compliance');

describe('COBOL Transpiler Compliance Integration', () => {
  let transpiler;
  let mockConfig;
  let mockComplianceService;

  beforeEach(() => {
    mockConfig = {
      bankingSystem: 'FIS_SYSTEMATICS',
      targetBlockchain: 'ethereum',
      compliance: {
        enabledChecks: ['anti_money_laundering', 'sanctions_screening', 'financial_action_task_force'],
        enableBatchProofs: true
      }
    };

    transpiler = new CobolTranspiler(mockConfig);
    
    // Mock the compliance service methods
    mockComplianceService = transpiler.complianceService;
    jest.spyOn(mockComplianceService, 'performComplianceCheck');
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  describe('AML/KYC Screening', () => {
    it('should perform AML/KYC screening for COBOL transactions', async () => {
      // Arrange
      const customerData = {
        customerId: 'CUST123',
        name: 'John Doe',
        countryCode: 'US',
        customerType: 'retail'
      };

      const transactionData = {
        amount: 5000,
        currency: 'USD',
        type: 'wire_transfer'
      };

      const mockComplianceResult = {
        overallStatus: 'passed',
        riskScore: 0.3,
        riskLevel: 'low',
        checkResults: [
          { checkType: 'anti_money_laundering', passed: true, score: 0.2 },
          { checkType: 'know_your_customer', passed: true, score: 0.4 }
        ]
      };

      mockComplianceService.performComplianceCheck.mockResolvedValue(mockComplianceResult);

      // Act
      const result = await transpiler.performAMLKYCScreening(customerData, transactionData);

      // Assert
      expect(result.passed).toBe(true);
      expect(result.riskLevel).toBe('low');
      expect(result.checkResults).toHaveLength(2);
      expect(mockComplianceService.performComplianceCheck).toHaveBeenCalledTimes(1);
      expect(mockComplianceService.performComplianceCheck).toHaveBeenCalledWith(
        transactionData,
        customerData,
        ['anti_money_laundering', 'know_your_customer']
      );
    });

    it('should fail AML/KYC screening for high-risk transactions', async () => {
      // Arrange
      const customerData = {
        customerId: 'CUST456',
        name: 'High Risk Customer',
        countryCode: 'XX', // High-risk country
        customerType: 'corporate'
      };

      const transactionData = {
        amount: 100000,
        currency: 'USD',
        type: 'wire_transfer'
      };

      const mockComplianceResult = {
        overallStatus: 'failed',
        riskScore: 0.85,
        riskLevel: 'critical',
        checkResults: [
          { checkType: 'anti_money_laundering', passed: false, score: 0.9 },
          { checkType: 'know_your_customer', passed: true, score: 0.8 }
        ]
      };

      mockComplianceService.performComplianceCheck.mockResolvedValue(mockComplianceResult);

      // Act
      const result = await transpiler.performAMLKYCScreening(customerData, transactionData);

      // Assert
      expect(result.passed).toBe(false);
      expect(result.riskLevel).toBe('critical');
      expect(result.riskScore).toBe(0.85);
    });
  });

  describe('Sanctions Screening', () => {
    it('should perform sanctions screening for all parties', async () => {
      // Arrange
      const parties = [
        { id: 'P1', name: 'Alice Smith', type: 'originator' },
        { id: 'P2', name: 'Bob Johnson', type: 'beneficiary' }
      ];

      const mockComplianceResults = [
        {
          overallStatus: 'passed',
          checkResults: [{ checkType: 'sanctions_screening', passed: true }]
        },
        {
          overallStatus: 'passed',
          checkResults: [{ checkType: 'sanctions_screening', passed: true }]
        }
      ];

      mockComplianceService.performComplianceCheck
        .mockResolvedValueOnce(mockComplianceResults[0])
        .mockResolvedValueOnce(mockComplianceResults[1]);

      // Act
      const result = await transpiler.performSanctionsScreening(parties);

      // Assert
      expect(result.overallPassed).toBe(true);
      expect(result.screeningResults).toHaveLength(2);
      expect(result.screeningResults[0].passed).toBe(true);
      expect(result.screeningResults[1].passed).toBe(true);
      expect(mockComplianceService.performComplianceCheck).toHaveBeenCalledTimes(2);
    });

    it('should fail sanctions screening if any party matches sanctions list', async () => {
      // Arrange
      const parties = [
        { id: 'P1', name: 'Clean Party', type: 'originator' },
        { id: 'P2', name: 'Sanctioned Entity', type: 'beneficiary' }
      ];

      const mockComplianceResults = [
        {
          overallStatus: 'passed',
          checkResults: [{ checkType: 'sanctions_screening', passed: true }]
        },
        {
          overallStatus: 'failed',
          checkResults: [{ checkType: 'sanctions_screening', passed: false }]
        }
      ];

      mockComplianceService.performComplianceCheck
        .mockResolvedValueOnce(mockComplianceResults[0])
        .mockResolvedValueOnce(mockComplianceResults[1]);

      // Act
      const result = await transpiler.performSanctionsScreening(parties);

      // Assert
      expect(result.overallPassed).toBe(false);
      expect(result.screeningResults[0].passed).toBe(true);
      expect(result.screeningResults[1].passed).toBe(false);
      expect(result.screeningResults[1].matchFound).toBe(true);
    });
  });

  describe('FATF Travel Rule Compliance', () => {
    it('should identify transactions requiring FATF Travel Rule compliance', () => {
      // Test amounts above $3,000 threshold
      const transactionData1 = { amount: 3500, currency: 'USD' };
      const transactionData2 = { amount: 5000, currency: 'EUR' };
      const transactionData3 = { amount: 2500, currency: 'USD' };

      expect(transpiler.checkFATFTravelRuleRequired(transactionData1)).toBe(true);
      expect(transpiler.checkFATFTravelRuleRequired(transactionData2)).toBe(true);
      expect(transpiler.checkFATFTravelRuleRequired(transactionData3)).toBe(false);
    });

    it('should handle different currencies with FX conversion', () => {
      // Test with different currencies
      const transactions = [
        { amount: 2500, currency: 'EUR' }, // ~$2,750 USD
        { amount: 2400, currency: 'GBP' }, // ~$3,120 USD
        { amount: 500000, currency: 'JPY' }, // ~$3,350 USD
      ];

      expect(transpiler.checkFATFTravelRuleRequired(transactions[0])).toBe(false);
      expect(transpiler.checkFATFTravelRuleRequired(transactions[1])).toBe(true);
      expect(transpiler.checkFATFTravelRuleRequired(transactions[2])).toBe(true);
    });

    it('should handle missing transaction data gracefully', () => {
      expect(transpiler.checkFATFTravelRuleRequired({})).toBe(false);
      expect(transpiler.checkFATFTravelRuleRequired({ amount: 5000 })).toBe(false);
      expect(transpiler.checkFATFTravelRuleRequired({ currency: 'USD' })).toBe(false);
    });
  });

  describe('Risk Scoring for COBOL-Generated Contracts', () => {
    it('should calculate risk score based on multiple factors', () => {
      // Arrange
      const contractData = {
        blockchain: 'ethereum',
        contractCode: 'pragma solidity ^0.8.0; contract Test { function transfer() public {} }',
        metadata: { contractId: 'CONTRACT123' }
      };

      const transactionData = {
        amount: 25000,
        currency: 'USD',
        type: 'wire_transfer'
      };

      // Act
      const riskScore = transpiler.calculateContractRiskScore(contractData, transactionData);

      // Assert
      expect(riskScore).toHaveProperty('score');
      expect(riskScore).toHaveProperty('level');
      expect(riskScore).toHaveProperty('factors');
      expect(riskScore.score).toBeGreaterThanOrEqual(0);
      expect(riskScore.score).toBeLessThanOrEqual(1);
      expect(riskScore.factors).toHaveLength(4);
      expect(['low', 'medium', 'high', 'critical']).toContain(riskScore.level);
    });

    it('should assign higher risk scores to large transactions', () => {
      const contractData = { blockchain: 'ethereum', contractCode: 'test' };
      
      const smallTransaction = { amount: 1000, currency: 'USD' };
      const largeTransaction = { amount: 500000, currency: 'USD' };

      const smallRisk = transpiler.calculateContractRiskScore(contractData, smallTransaction);
      const largeRisk = transpiler.calculateContractRiskScore(contractData, largeTransaction);

      expect(largeRisk.score).toBeGreaterThan(smallRisk.score);
    });

    it('should consider banking system risk in scoring', () => {
      const contractData = { blockchain: 'ethereum', contractCode: 'test' };
      const transactionData = { amount: 10000, currency: 'USD' };

      // Test with different banking systems
      const fisTranspiler = new CobolTranspiler({ bankingSystem: 'FIS_SYSTEMATICS' });
      const fiservTranspiler = new CobolTranspiler({ bankingSystem: 'FISERV_DNA' });

      const fisRisk = fisTranspiler.calculateContractRiskScore(contractData, transactionData);
      const fiservRisk = fiservTranspiler.calculateContractRiskScore(contractData, transactionData);

      // FIS (legacy mainframe) should have higher risk than Fiserv (modern API)
      expect(fisRisk.factors.find(f => f.factor === 'banking_system').score)
        .toBeGreaterThan(fiservRisk.factors.find(f => f.factor === 'banking_system').score);
    });

    it('should factor in contract complexity', () => {
      const transactionData = { amount: 10000, currency: 'USD' };
      
      const simpleContract = {
        blockchain: 'ethereum',
        contractCode: 'contract Simple { uint value; }'
      };
      
      const complexContract = {
        blockchain: 'ethereum',
        contractCode: `contract Complex { 
          ${Array(25).fill('function test() public {}').join('\n')}
          ${Array(100000).fill('a').join('')}
        }`
      };

      const simpleRisk = transpiler.calculateContractRiskScore(simpleContract, transactionData);
      const complexRisk = transpiler.calculateContractRiskScore(complexContract, transactionData);

      expect(complexRisk.score).toBeGreaterThan(simpleRisk.score);
    });
  });

  describe('Comprehensive Compliance Screening', () => {
    it('should perform full compliance screening with all checks', async () => {
      // Arrange
      const contractData = {
        metadata: {
          contractId: 'CONTRACT789',
          transpiler: { programId: 'LOAN-CALC' }
        },
        blockchain: 'ethereum',
        contractCode: 'contract LoanCalc { /* ... */ }'
      };

      const transactionData = {
        amount: 50000,
        currency: 'USD',
        type: 'loan_processing'
      };

      const customerData = {
        customerId: 'CUST999',
        name: 'Test Customer',
        countryCode: 'US'
      };

      const mockComplianceResult = {
        overallStatus: 'passed',
        riskScore: 0.45,
        riskLevel: 'medium',
        checkResults: [
          { checkType: 'anti_money_laundering', passed: true },
          { checkType: 'sanctions_screening', passed: true },
          { checkType: 'financial_action_task_force', passed: true }
        ],
        recommendations: [
          { type: 'travel_rule_compliance', priority: 'medium', description: 'Include Travel Rule info' }
        ]
      };

      mockComplianceService.performComplianceCheck.mockResolvedValue(mockComplianceResult);

      // Act
      const result = await transpiler.performComplianceScreening(
        contractData,
        transactionData,
        customerData
      );

      // Assert
      expect(result.overallStatus).toBe('passed');
      expect(result.riskLevel).toBe('medium');
      expect(result.cobolContext).toBeDefined();
      expect(result.cobolContext.sourceProgram).toBe('LOAN-CALC');
      expect(result.cobolContext.bankingSystem).toBe('FIS_SYSTEMATICS');
      expect(result.recommendations.length).toBeGreaterThanOrEqual(1);
      expect(transpiler.metrics.complianceChecks).toBe(1);
    });

    it('should handle compliance screening failures gracefully', async () => {
      // Arrange
      mockComplianceService.performComplianceCheck.mockRejectedValue(
        new Error('Compliance service unavailable')
      );

      const contractData = { metadata: {}, blockchain: 'ethereum' };
      const transactionData = { amount: 1000, currency: 'USD' };
      const customerData = { customerId: 'CUST123' };

      // Act & Assert
      await expect(
        transpiler.performComplianceScreening(contractData, transactionData, customerData)
      ).rejects.toThrow('Compliance screening failed: Compliance service unavailable');
      
      expect(transpiler.metrics.complianceFailures).toBe(1);
    });
  });

  describe('Compliance Metrics', () => {
    it('should track compliance metrics accurately', async () => {
      // Arrange
      const mockResult = {
        overallStatus: 'passed',
        riskScore: 0.3,
        riskLevel: 'low',
        checkResults: [{ checkType: 'anti_money_laundering', passed: true }]
      };
      mockComplianceService.performComplianceCheck.mockResolvedValue(mockResult);

      // Perform compliance operations
      // performAMLKYCScreening and performSanctionsScreening don't increment complianceChecks
      // Only performComplianceScreening increments it
      await transpiler.performAMLKYCScreening({}, {});
      await transpiler.performSanctionsScreening([{ name: 'Test' }]);
      
      // Reset mock for compliance screening
      mockComplianceService.performComplianceCheck.mockResolvedValue(mockResult);
      await transpiler.performComplianceScreening(
        { blockchain: 'ethereum', metadata: {} }, // Proper contract data
        { amount: 1000, currency: 'USD' }, // Proper transaction data
        { customerId: 'TEST' } // Proper customer data
      ); // 1 check (success)
      
      // Force one failure
      mockComplianceService.performComplianceCheck.mockRejectedValue(new Error('Failed'));
      try {
        await transpiler.performComplianceScreening(
          { blockchain: 'ethereum', metadata: {} },
          { amount: 1000, currency: 'USD' },
          { customerId: 'TEST' }
        ); // 1 check (fails)
      } catch (e) {
        // Expected failure
      }

      // Get metrics
      const metrics = transpiler.getMetrics();

      // Assert
      expect(metrics.complianceChecks).toBe(2); // Only performComplianceScreening calls
      expect(metrics.complianceFailures).toBe(1);
      expect(metrics.complianceSuccessRate).toBeCloseTo(50.0, 1);
    });
  });
});