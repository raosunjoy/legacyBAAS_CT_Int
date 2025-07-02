/**
 * Zero-Knowledge Proof Compliance Tests
 * Comprehensive test suite for privacy-preserving compliance
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Test Coverage for ZK-SNARK Compliance IP Component
 */

const { ZKProofComplianceService, COMPLIANCE_TYPES, RISK_LEVELS, ZK_CIRCUITS } = require('../../src/compliance/zk-proof-compliance');

describe('ZK Proof Compliance Service', () => {
  let complianceService;
  let mockTransactionData;
  let mockCustomerData;

  beforeEach(() => {
    complianceService = new ZKProofComplianceService({
      zkFramework: 'zokrates',
      enableBatchProofs: true,
      enableFullPrivacy: true,
      enableAuditTrail: true,
      dataRetentionDays: 30
    });

    mockTransactionData = {
      id: 'txn_123456',
      amount: 5000,
      currency: 'USD',
      type: 'cross_border_payment',
      originJurisdiction: 'US',
      destinationJurisdiction: 'EU'
    };

    mockCustomerData = {
      id: 'cust_789012',
      name: 'John Doe',
      age: 35,
      income: 75000,
      address: '123 Main St, New York, NY',
      documentId: 'passport_123456',
      jurisdiction: 'US',
      sourceOfFunds: 'employment',
      transactionHistory: [],
      riskFactors: [],
      politicalConnections: [],
      familyConnections: [],
      businessConnections: []
    };
  });

  afterEach(async () => {
    // Cleanup if needed
  });

  describe('Initialization', () => {
    test('should initialize with default configuration', () => {
      const defaultService = new ZKProofComplianceService();
      
      expect(defaultService.config.zkFramework).toBe('zokrates');
      expect(defaultService.config.enableBatchProofs).toBe(true);
      expect(defaultService.config.enableFullPrivacy).toBe(true);
      expect(defaultService.config.enabledChecks).toContain(COMPLIANCE_TYPES.KYC);
      expect(defaultService.config.enabledChecks).toContain(COMPLIANCE_TYPES.AML);
    });

    test('should initialize with custom configuration', () => {
      const customService = new ZKProofComplianceService({
        zkFramework: 'circom',
        enableBatchProofs: false,
        enableFullPrivacy: false,
        dataRetentionDays: 60,
        complianceProviders: ['chainalysis', 'elliptic']
      });

      expect(customService.config.zkFramework).toBe('circom');
      expect(customService.config.enableBatchProofs).toBe(false);
      expect(customService.config.enableFullPrivacy).toBe(false);
      expect(customService.config.dataRetentionDays).toBe(60);
      expect(customService.config.complianceProviders).toContain('chainalysis');
    });

    test('should initialize performance metrics', () => {
      expect(complianceService.metrics.totalProofs).toBe(0);
      expect(complianceService.metrics.successfulProofs).toBe(0);
      expect(complianceService.metrics.privacyLevel).toBe('high');
      expect(complianceService.proofCache).toBeInstanceOf(Map);
      expect(complianceService.complianceHistory).toBeInstanceOf(Map);
    });
  });

  describe('Comprehensive Compliance Checks', () => {
    test('should perform full compliance check successfully', async () => {
      const result = await complianceService.performComplianceCheck(
        mockTransactionData,
        mockCustomerData,
        [COMPLIANCE_TYPES.KYC, COMPLIANCE_TYPES.AML, COMPLIANCE_TYPES.SANCTIONS]
      );

      expect(result.complianceId).toBeDefined();
      expect(result.transactionId).toBe(mockTransactionData.id);
      expect(result.customerId).toBe(mockCustomerData.id);
      expect(result.overallPassed).toBeDefined();
      expect(result.overallRiskScore).toBeGreaterThanOrEqual(0);
      expect(result.overallRiskScore).toBeLessThanOrEqual(1);
      expect(result.riskLevel).toBeDefined();
      expect(result.zkProofs).toBeDefined();
      expect(result.processingTime).toBeGreaterThan(0);
    });

    test('should pass compliance for low-risk transaction', async () => {
      const lowRiskTransaction = {
        ...mockTransactionData,
        amount: 100
      };

      const result = await complianceService.performComplianceCheck(
        lowRiskTransaction,
        mockCustomerData,
        [COMPLIANCE_TYPES.KYC, COMPLIANCE_TYPES.AML]
      );

      expect(result.overallPassed).toBe(true);
      expect([RISK_LEVELS.LOW, RISK_LEVELS.MEDIUM]).toContain(result.riskLevel);
      expect(result.requiresManualReview).toBe(false);
    });

    test('should handle high-risk transaction appropriately', async () => {
      const highRiskCustomer = {
        ...mockCustomerData,
        riskFactors: ['high_risk_country', 'cash_intensive_business'],
        politicalConnections: ['government_official']
      };

      const highRiskTransaction = {
        ...mockTransactionData,
        amount: 50000
      };

      const result = await complianceService.performComplianceCheck(
        highRiskTransaction,
        highRiskCustomer,
        [COMPLIANCE_TYPES.KYC, COMPLIANCE_TYPES.AML, COMPLIANCE_TYPES.PEP, COMPLIANCE_TYPES.ENHANCED_DD]
      );

      expect(result.overallRiskScore).toBeGreaterThan(0.1);
      expect(result.checkResults).toBeDefined();
      expect(result.zkProofs.aggregateProof).toBeDefined();
    });

    test('should generate privacy-preserving compliance report', async () => {
      const result = await complianceService.performComplianceCheck(
        mockTransactionData,
        mockCustomerData
      );

      // Verify privacy preservation
      expect(result.zkProofs.individualProofs).toBeDefined();
      expect(result.zkProofs.verificationKeys).toBeDefined();
      expect(result.zkProofs.circuitHashes).toBeDefined();
      expect(result.dataPrivacyLevel).toBe('maximum');
      
      // Ensure sensitive data is not exposed
      for (const [checkType, checkResult] of Object.entries(result.checkResults)) {
        expect(checkResult.verificationLevel).toBe('privacy_preserving');
        expect(checkResult).not.toHaveProperty('customerAge');
        expect(checkResult).not.toHaveProperty('customerIncome');
        expect(checkResult).not.toHaveProperty('documentHash');
      }
    });
  });

  describe('Individual Compliance Checks', () => {
    describe('KYC (Know Your Customer)', () => {
      test('should generate KYC zero-knowledge proof', async () => {
        const complianceContext = {
          complianceId: 'comp_123',
          transaction: mockTransactionData,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.KYC]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.KYC, complianceContext);

        expect(proof.proofId).toBeDefined();
        expect(proof.circuitType).toBe(ZK_CIRCUITS.IDENTITY_VERIFICATION);
        expect(proof.proofData).toBeDefined();
        expect(proof.proofData.pi_a).toBeDefined();
        expect(proof.proofData.pi_b).toBeDefined();
        expect(proof.proofData.pi_c).toBeDefined();
        expect(proof.publicSignals.meetsAgeRequirement).toBeDefined();
        expect(proof.publicSignals.hasValidDocuments).toBeDefined();
      });

      test('should verify KYC proof successfully', async () => {
        const complianceContext = {
          complianceId: 'comp_123',
          transaction: mockTransactionData,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.KYC]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.KYC, complianceContext);
        const verification = await complianceService.verifyComplianceProof(COMPLIANCE_TYPES.KYC, proof, complianceContext);

        expect(verification.passed).toBeDefined();
        expect(verification.riskScore).toBeGreaterThanOrEqual(0);
        expect(verification.riskScore).toBeLessThanOrEqual(1);
        expect(verification.confidence).toBeGreaterThan(0);
        expect(verification.verificationTimestamp).toBeDefined();
      });
    });

    describe('AML (Anti-Money Laundering)', () => {
      test('should generate AML zero-knowledge proof', async () => {
        const complianceContext = {
          complianceId: 'comp_123',
          transaction: mockTransactionData,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.AML]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.AML, complianceContext);

        expect(proof.proofId).toBeDefined();
        expect(proof.circuitType).toBe(ZK_CIRCUITS.TRANSACTION_PATTERN);
        expect(proof.proofData).toBeDefined();
        expect(proof.publicSignals.passesPatternAnalysis).toBeDefined();
        expect(proof.publicSignals.riskScore).toBeGreaterThanOrEqual(0);
        expect(proof.publicSignals.requiresReporting).toBeDefined();
      });

      test('should identify high-value transaction reporting requirement', async () => {
        const highValueTransaction = {
          ...mockTransactionData,
          amount: 15000 // Above threshold
        };

        const complianceContext = {
          complianceId: 'comp_123',
          transaction: highValueTransaction,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.AML]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.AML, complianceContext);
        
        expect(proof.publicSignals.requiresReporting).toBe(true);
      });
    });

    describe('Sanctions Screening', () => {
      test('should generate sanctions screening proof', async () => {
        const complianceContext = {
          complianceId: 'comp_123',
          transaction: mockTransactionData,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.SANCTIONS]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.SANCTIONS, complianceContext);

        expect(proof.proofId).toBeDefined();
        expect(proof.circuitType).toBe(ZK_CIRCUITS.SANCTIONS_CHECK);
        expect(proof.proofData).toBeDefined();
        expect(proof.publicSignals.notOnSanctionsList).toBeDefined();
        expect(proof.publicSignals.screeningCompleted).toBeDefined();
        expect(proof.publicSignals.lastScreeningDate).toBeDefined();
      });

      test('should verify sanctions screening result', async () => {
        const complianceContext = {
          complianceId: 'comp_123',
          transaction: mockTransactionData,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.SANCTIONS]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.SANCTIONS, complianceContext);
        const verification = await complianceService.verifyComplianceProof(COMPLIANCE_TYPES.SANCTIONS, proof, complianceContext);

        expect(verification.passed).toBe(true);
        expect(verification.riskScore).toBeLessThan(0.1); // Low risk for clean customer
        expect(verification.confidence).toBeGreaterThan(0.9);
      });
    });

    describe('PEP (Politically Exposed Person)', () => {
      test('should generate PEP screening proof', async () => {
        const pepCustomer = {
          ...mockCustomerData,
          politicalConnections: ['government_minister'],
          familyConnections: ['spouse_of_official']
        };

        const complianceContext = {
          complianceId: 'comp_123',
          transaction: mockTransactionData,
          customer: pepCustomer,
          requiredChecks: [COMPLIANCE_TYPES.PEP]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.PEP, complianceContext);

        expect(proof.proofId).toBeDefined();
        expect(proof.circuitType).toBe(ZK_CIRCUITS.RISK_SCORING);
        expect(proof.publicSignals.isPEP).toBeDefined();
        expect(proof.publicSignals.requiresEnhancedDD).toBeDefined();
        expect(proof.publicSignals.riskCategory).toBeDefined();
      });
    });

    describe('Enhanced Due Diligence', () => {
      test('should generate enhanced DD proof for high-value transaction', async () => {
        const highValueTransaction = {
          ...mockTransactionData,
          amount: 30000
        };

        const complianceContext = {
          complianceId: 'comp_123',
          transaction: highValueTransaction,
          customer: mockCustomerData,
          requiredChecks: [COMPLIANCE_TYPES.ENHANCED_DD]
        };

        const proof = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.ENHANCED_DD, complianceContext);

        expect(proof.proofId).toBeDefined();
        expect(proof.circuitType).toBe(ZK_CIRCUITS.RISK_SCORING);
        expect(proof.publicSignals.eddCompleted).toBeDefined();
        expect(proof.publicSignals.riskMitigation).toBeDefined();
        expect(proof.publicSignals.approvalLevel).toBeDefined();
      });
    });
  });

  describe('Aggregate Proof Generation', () => {
    test('should generate aggregate proof from individual proofs', async () => {
      const individualProofs = new Map();
      
      // Mock individual proofs
      individualProofs.set(COMPLIANCE_TYPES.KYC, {
        proofId: 'kyc_proof_123',
        proofData: { pi_a: ['test'], pi_b: ['test'], pi_c: ['test'] },
        publicSignals: { meetsAgeRequirement: true }
      });
      
      individualProofs.set(COMPLIANCE_TYPES.AML, {
        proofId: 'aml_proof_456',
        proofData: { pi_a: ['test'], pi_b: ['test'], pi_c: ['test'] },
        publicSignals: { passesPatternAnalysis: true }
      });

      const overallResult = {
        passed: true,
        riskScore: 0.2,
        requiresManualReview: false
      };

      const aggregateProof = await complianceService.generateAggregateProof(individualProofs, overallResult);

      expect(aggregateProof.aggregateProofId).toBeDefined();
      expect(aggregateProof.aggregateProofData).toBeDefined();
      expect(aggregateProof.numberOfChecks).toBe(2);
      expect(aggregateProof.overallResult).toBe(true);
      expect(aggregateProof.verificationLevel).toBe('comprehensive');
    });
  });

  describe('Risk Assessment', () => {
    test('should classify risk levels correctly', () => {
      expect(complianceService.classifyRiskLevel(0.1)).toBe(RISK_LEVELS.LOW);
      expect(complianceService.classifyRiskLevel(0.5)).toBe(RISK_LEVELS.MEDIUM);
      expect(complianceService.classifyRiskLevel(0.7)).toBe(RISK_LEVELS.HIGH);
      expect(complianceService.classifyRiskLevel(0.9)).toBe(RISK_LEVELS.CRITICAL);
    });

    test('should calculate overall compliance correctly', () => {
      const complianceResults = new Map();
      
      complianceResults.set(COMPLIANCE_TYPES.KYC, {
        passed: true,
        riskScore: 0.1,
        requiresManualReview: false
      });
      
      complianceResults.set(COMPLIANCE_TYPES.AML, {
        passed: true,
        riskScore: 0.2,
        requiresManualReview: false
      });
      
      complianceResults.set(COMPLIANCE_TYPES.SANCTIONS, {
        passed: true,
        riskScore: 0.05,
        requiresManualReview: false
      });

      const overallResult = complianceService.calculateOverallCompliance(complianceResults);

      expect(overallResult.passed).toBe(true);
      expect(overallResult.riskScore).toBeCloseTo(0.117, 2); // Average
      expect(overallResult.requiresManualReview).toBe(false);
      expect(overallResult.totalChecks).toBe(3);
      expect(overallResult.passedChecks).toBe(3);
    });

    test('should fail overall compliance if any check fails', () => {
      const complianceResults = new Map();
      
      complianceResults.set(COMPLIANCE_TYPES.KYC, {
        passed: true,
        riskScore: 0.1,
        requiresManualReview: false
      });
      
      complianceResults.set(COMPLIANCE_TYPES.SANCTIONS, {
        passed: false,
        riskScore: 1.0,
        requiresManualReview: true
      });

      const overallResult = complianceService.calculateOverallCompliance(complianceResults);

      expect(overallResult.passed).toBe(false);
      expect(overallResult.requiresManualReview).toBe(true);
      expect(overallResult.passedChecks).toBe(1);
    });
  });

  describe('Privacy Preservation', () => {
    test('should sanitize check results for privacy', () => {
      const complianceResults = new Map();
      
      complianceResults.set(COMPLIANCE_TYPES.KYC, {
        passed: true,
        riskScore: 0.15,
        requiresManualReview: false,
        customerAge: 35, // This should be removed
        customerIncome: 75000 // This should be removed
      });

      const sanitized = complianceService.sanitizeCheckResults(complianceResults);

      expect(sanitized[COMPLIANCE_TYPES.KYC].passed).toBe(true);
      expect(sanitized[COMPLIANCE_TYPES.KYC].riskLevel).toBe(RISK_LEVELS.LOW);
      expect(sanitized[COMPLIANCE_TYPES.KYC].verificationLevel).toBe('privacy_preserving');
      expect(sanitized[COMPLIANCE_TYPES.KYC]).not.toHaveProperty('customerAge');
      expect(sanitized[COMPLIANCE_TYPES.KYC]).not.toHaveProperty('customerIncome');
    });

    test('should sanitize proofs to protect privacy', () => {
      const complianceProofs = new Map();
      
      complianceProofs.set(COMPLIANCE_TYPES.KYC, {
        proofId: 'kyc_proof_123',
        proofData: { pi_a: ['sensitive'], pi_b: ['data'], pi_c: ['here'] },
        circuitType: ZK_CIRCUITS.IDENTITY_VERIFICATION,
        timestamp: '2023-06-01T12:00:00Z'
      });

      const sanitized = complianceService.sanitizeProofs(complianceProofs);

      expect(sanitized[COMPLIANCE_TYPES.KYC].proofId).toBe('kyc_proof_123');
      expect(sanitized[COMPLIANCE_TYPES.KYC].proofHash).toBeDefined();
      expect(sanitized[COMPLIANCE_TYPES.KYC].circuitType).toBe(ZK_CIRCUITS.IDENTITY_VERIFICATION);
      expect(sanitized[COMPLIANCE_TYPES.KYC]).not.toHaveProperty('proofData');
    });
  });

  describe('Compliance Standards and Jurisdictions', () => {
    test('should identify applicable compliance standards', () => {
      const highValueUS = { amount: 15000, jurisdiction: 'US' };
      const euCustomer = { jurisdiction: 'EU' };
      
      const standards = complianceService.getApplicableStandards(highValueUS, euCustomer);
      
      expect(standards).toContain('FATF');
      expect(standards).toContain('BSA');
      expect(standards).toContain('GDPR');
      expect(standards).toContain('MiFID II');
    });

    test('should identify relevant jurisdictions', () => {
      const transaction = {
        originJurisdiction: 'US',
        destinationJurisdiction: 'EU'
      };
      const customer = { jurisdiction: 'UK' };
      
      const jurisdictions = complianceService.identifyJurisdictions(transaction, customer);
      
      expect(jurisdictions).toContain('UK');
      expect(jurisdictions).toContain('US');
      expect(jurisdictions).toContain('EU');
      expect(jurisdictions).toHaveLength(3);
    });
  });

  describe('Performance Metrics', () => {
    test('should track compliance service metrics', async () => {
      await complianceService.performComplianceCheck(
        mockTransactionData,
        mockCustomerData,
        [COMPLIANCE_TYPES.KYC]
      );

      const metrics = complianceService.getMetrics();

      expect(metrics.totalProofs).toBeGreaterThan(0);
      expect(metrics.successfulProofs).toBeGreaterThan(0);
      expect(metrics.successRate).toBeGreaterThan(0);
      expect(metrics.averageProofTime).toBeGreaterThanOrEqual(0);
      expect(metrics.privacyLevel).toBe('maximum');
      expect(metrics.supportedStandards).toContain('GDPR');
      expect(metrics.supportedStandards).toContain('FATF');
    });

    test('should track compliance type statistics', async () => {
      await complianceService.performComplianceCheck(
        mockTransactionData,
        mockCustomerData,
        [COMPLIANCE_TYPES.KYC, COMPLIANCE_TYPES.AML]
      );

      const metrics = complianceService.getMetrics();
      const typeStats = metrics.complianceTypeStats || new Map();

      expect(typeStats.get(COMPLIANCE_TYPES.KYC)).toBeGreaterThan(0);
      expect(typeStats.get(COMPLIANCE_TYPES.AML)).toBeGreaterThan(0);
    });
  });

  describe('Error Handling', () => {
    test('should handle unsupported compliance check type', async () => {
      const complianceContext = {
        complianceId: 'comp_123',
        transaction: mockTransactionData,
        customer: mockCustomerData,
        requiredChecks: ['UNSUPPORTED_CHECK']
      };

      await expect(
        complianceService.generateComplianceProof('UNSUPPORTED_CHECK', complianceContext)
      ).rejects.toThrow('Unsupported compliance check type');
    });

    test('should handle proof verification failure gracefully', async () => {
      const invalidProof = {
        proofId: 'invalid_proof',
        circuitType: ZK_CIRCUITS.IDENTITY_VERIFICATION,
        proofData: null, // Invalid proof data
        publicSignals: {}
      };

      const complianceContext = {
        complianceId: 'comp_123',
        transaction: mockTransactionData,
        customer: mockCustomerData
      };

      const result = await complianceService.verifyComplianceProof(
        COMPLIANCE_TYPES.KYC,
        invalidProof,
        complianceContext
      );

      expect(result.passed).toBe(false);
      expect(result.riskScore).toBe(1.0);
      expect(result.requiresManualReview).toBe(true);
      expect(result.error).toBeDefined();
    });
  });

  describe('Caching and Performance', () => {
    test('should cache proofs when enabled', async () => {
      // Enable caching
      complianceService.config.proofCaching = true;

      const complianceContext = {
        complianceId: 'comp_123',
        transaction: mockTransactionData,
        customer: mockCustomerData,
        requiredChecks: [COMPLIANCE_TYPES.KYC]
      };

      // First call should generate proof
      const proof1 = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.KYC, complianceContext);
      
      // Second call with same data should use cache
      const proof2 = await complianceService.generateComplianceProof(COMPLIANCE_TYPES.KYC, complianceContext);

      expect(proof1.proofId).toBeDefined();
      expect(proof2.proofId).toBeDefined();
      expect(complianceService.proofCache.size).toBeGreaterThan(0);
    });
  });

  describe('Data Retention and Cleanup', () => {
    test('should store compliance results with privacy preservation', async () => {
      const result = await complianceService.performComplianceCheck(
        mockTransactionData,
        mockCustomerData
      );

      expect(complianceService.complianceHistory.has(result.complianceId)).toBe(true);
      
      const storedResult = complianceService.complianceHistory.get(result.complianceId);
      expect(storedResult.zkProofs.verificationLevel).toBe('privacy_preserving');
      expect(storedResult.zkProofs).not.toHaveProperty('individualProofs');
    });
  });

  describe('Constants and Exports', () => {
    test('should export compliance types correctly', () => {
      expect(COMPLIANCE_TYPES.KYC).toBe('know_your_customer');
      expect(COMPLIANCE_TYPES.AML).toBe('anti_money_laundering');
      expect(COMPLIANCE_TYPES.SANCTIONS).toBe('sanctions_screening');
      expect(COMPLIANCE_TYPES.PEP).toBe('politically_exposed_person');
      expect(COMPLIANCE_TYPES.ENHANCED_DD).toBe('enhanced_due_diligence');
    });

    test('should export risk levels correctly', () => {
      expect(RISK_LEVELS.LOW).toBe('low');
      expect(RISK_LEVELS.MEDIUM).toBe('medium');
      expect(RISK_LEVELS.HIGH).toBe('high');
      expect(RISK_LEVELS.CRITICAL).toBe('critical');
    });

    test('should export ZK circuits correctly', () => {
      expect(ZK_CIRCUITS.IDENTITY_VERIFICATION).toBe('identity_verification_circuit');
      expect(ZK_CIRCUITS.TRANSACTION_PATTERN).toBe('transaction_pattern_circuit');
      expect(ZK_CIRCUITS.SANCTIONS_CHECK).toBe('sanctions_check_circuit');
      expect(ZK_CIRCUITS.RISK_SCORING).toBe('risk_scoring_circuit');
    });
  });
});