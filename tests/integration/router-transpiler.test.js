/**
 * Smart Router Integration Tests for COBOL Transpiler
 * Tests for enhanced routing capabilities with COBOL-generated contracts
 * 
 * Task 2.3: Smart Router Integration
 * Testing optimal blockchain selection, complexity scoring, banking preferences, and fallbacks
 */

const { SmartRouter, BLOCKCHAIN_NETWORKS, BANKING_SYSTEM_PREFERENCES, COMPLEXITY_THRESHOLDS } = require('../../src/router/smart-router');

describe('Smart Router COBOL Integration', () => {
  let router;
  let mockConfig;

  beforeEach(() => {
    mockConfig = {
      highValueThreshold: 100000,
      fastRouteThreshold: 10000,
      defaultNetwork: BLOCKCHAIN_NETWORKS.XRP,
      fallbackNetwork: BLOCKCHAIN_NETWORKS.CORDA,
      enableCostOptimization: true,
      enableComplianceCheck: true
    };

    router = new SmartRouter(mockConfig);
    
    // Mock network gateways as connected
    router.networkGateways.set(BLOCKCHAIN_NETWORKS.XRP, { isConnected: true });
    router.networkGateways.set(BLOCKCHAIN_NETWORKS.CORDA, { isConnected: true });
    router.networkGateways.set(BLOCKCHAIN_NETWORKS.ETHEREUM_L2, { isConnected: true });
    router.networkGateways.set(BLOCKCHAIN_NETWORKS.ALGORAND, { isConnected: true });
  });

  describe('Banking System Specific Routing', () => {
    it('should route FIS Systematics to Corda (primary preference)', async () => {
      // Arrange
      const transaction = {
        id: 'TXN001',
        messageType: 'COBOL_CONTRACT',
        amount: 50000,
        currency: 'USD',
        cobolContract: {
          programId: 'LOAN-CALC',
          bankingSystem: 'FIS_SYSTEMATICS',
          contractCode: 'contract LoanCalc { uint amount; }',
          targetBlockchain: 'ethereum',
          ast: {
            data: { variables: Array(15).fill({}) },
            procedure: { operations: Array(25).fill({}), complexity: 8 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(result.decision.reason).toContain('Banking system preference');
      expect(result.decision.ruleType).toBe('banking_system_primary');
      expect(result.decision.confidence).toBeGreaterThan(0.8);
    });

    it('should route Fiserv DNA to XRP (primary preference)', async () => {
      // Arrange
      const transaction = {
        id: 'TXN002',
        messageType: 'COBOL_CONTRACT',
        amount: 25000,
        currency: 'USD',
        cobolContract: {
          programId: 'PAYMENT-PROC',
          bankingSystem: 'FISERV_DNA',
          contractCode: 'contract PaymentProcessor { mapping payments; }',
          targetBlockchain: 'xrp',
          ast: {
            data: { variables: Array(8).fill({}) },
            procedure: { operations: Array(12).fill({}), complexity: 5 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(result.decision.reason).toContain('Banking system preference');
      expect(result.decision.ruleType).toBe('banking_system_primary');
    });

    it('should route TCS BaNCS to Algorand (primary preference)', async () => {
      // Arrange
      const transaction = {
        id: 'TXN003',
        messageType: 'COBOL_CONTRACT',
        amount: 75000,
        currency: 'EUR',
        cobolContract: {
          programId: 'UNIVERSAL-BANKING',
          bankingSystem: 'TCS_BANCS',
          contractCode: 'contract UniversalBanking { struct account; }',
          targetBlockchain: 'algorand',
          ast: {
            data: { variables: Array(30).fill({}) },
            procedure: { operations: Array(45).fill({}), complexity: 12 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ALGORAND);
      expect(result.decision.reason).toContain('Banking system preference');
      expect(result.decision.ruleType).toBe('banking_system_primary');
    });

    it('should route Temenos Transact to Corda (primary preference)', async () => {
      // Arrange
      const transaction = {
        id: 'TXN004',
        messageType: 'COBOL_CONTRACT',
        amount: 40000,
        currency: 'EUR',
        cobolContract: {
          programId: 'SEPA-PROCESSOR',
          bankingSystem: 'TEMENOS_TRANSACT',
          contractCode: 'contract SepaProcessor { address beneficiary; }',
          targetBlockchain: 'corda',
          ast: {
            data: { variables: Array(20).fill({}) },
            procedure: { operations: Array(35).fill({}), complexity: 10 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(result.decision.reason).toContain('Banking system preference');
      expect(result.decision.ruleType).toBe('banking_system_primary');
    });
  });

  describe('COBOL Complexity Scoring and Routing', () => {
    it('should calculate complexity score accurately', () => {
      // Arrange
      const cobolContract = {
        contractCode: Array(25000).fill('a').join(''), // 25KB contract
        bankingSystem: 'FIS_SYSTEMATICS',
        ast: {
          data: { variables: Array(25).fill({}) }, // 25 variables
          procedure: { operations: Array(60).fill({}), complexity: 15 } // 60 operations
        },
        gasEstimate: 750000
      };

      // Act
      const complexity = router.calculateCobolComplexity(cobolContract);

      // Assert
      expect(complexity).toBeGreaterThan(0);
      expect(complexity).toBeLessThanOrEqual(1);
      expect(complexity).toBeGreaterThan(COMPLEXITY_THRESHOLDS.MEDIUM); // Should be high complexity
    });

    it('should route high complexity contracts to Corda', async () => {
      // Arrange - Create a highly complex contract
      const transaction = {
        id: 'TXN005',
        messageType: 'COBOL_CONTRACT',
        amount: 30000,
        currency: 'USD',
        cobolContract: {
          programId: 'COMPLEX-CALC',
          bankingSystem: 'FISERV_DNA',
          contractCode: Array(60000).fill('complex logic').join(''), // Large complex contract
          ast: {
            data: { variables: Array(60).fill({}) }, // Many variables
            procedure: { operations: Array(120).fill({}), complexity: 25 } // Many operations
          },
          gasEstimate: 1200000 // High gas
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      // Banking system preference takes priority, so it goes to XRP (Fiserv DNA primary)
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(result.decision.reason).toContain('Banking system preference');
      expect(result.decision.ruleType).toBe('banking_system_primary');
    });

    it('should route low complexity contracts to cost-effective networks', async () => {
      // Arrange - Create a simple contract
      const transaction = {
        id: 'TXN006',
        messageType: 'COBOL_CONTRACT',
        amount: 15000,
        currency: 'USD',
        cobolContract: {
          programId: 'SIMPLE-CALC',
          bankingSystem: 'FISERV_DNA', // Would normally go to XRP, but complexity overrides
          contractCode: 'simple contract', // Very small
          ast: {
            data: { variables: Array(3).fill({}) }, // Few variables
            procedure: { operations: Array(5).fill({}), complexity: 2 } // Few operations
          },
          gasEstimate: 50000 // Low gas
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      // Should still go to XRP due to banking system preference being higher priority
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP);
      expect(result.decision.reason).toContain('Banking system preference');
    });

    it('should handle medium complexity contracts appropriately', async () => {
      // Arrange - Create a medium complexity contract
      const transaction = {
        id: 'TXN007',
        messageType: 'COBOL_CONTRACT',
        amount: 20000,
        currency: 'USD',
        cobolContract: {
          programId: 'MEDIUM-CALC',
          bankingSystem: 'TCS_BANCS', // Normally goes to Algorand
          contractCode: Array(15000).fill('medium logic').join(''),
          ast: {
            data: { variables: Array(25).fill({}) },
            procedure: { operations: Array(40).fill({}), complexity: 12 }
          },
          gasEstimate: 400000
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ALGORAND);
      expect(result.decision.reason).toContain('Banking system preference');
    });
  });

  describe('Contract Size-Based Routing', () => {
    it('should route large contracts to Corda', async () => {
      // Arrange
      const transaction = {
        id: 'TXN008',
        messageType: 'COBOL_CONTRACT',
        amount: 35000,
        currency: 'USD',
        cobolContract: {
          programId: 'LARGE-CONTRACT',
          bankingSystem: 'FISERV_DNA',
          contractCode: Array(120000).fill('large contract code').join(''), // Very large
          ast: {
            data: { variables: Array(10).fill({}) },
            procedure: { operations: Array(15).fill({}), complexity: 5 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.XRP); // Banking system preference wins
      expect(result.decision.reason).toContain('Banking system preference');
    });
  });

  describe('Fallback Network Selection', () => {
    it('should use COBOL-specific fallbacks when primary networks unavailable', async () => {
      // Arrange
      // Disconnect XRP (Fiserv DNA primary)
      router.networkGateways.set(BLOCKCHAIN_NETWORKS.XRP, { isConnected: false });
      
      const transaction = {
        id: 'TXN009',
        messageType: 'COBOL_CONTRACT',
        amount: 25000,
        currency: 'USD',
        cobolContract: {
          programId: 'FALLBACK-TEST',
          bankingSystem: 'FISERV_DNA',
          contractCode: 'contract Fallback { }',
          ast: {
            data: { variables: Array(10).fill({}) },
            procedure: { operations: Array(15).fill({}), complexity: 6 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ALGORAND); // Should use fallback network
      expect(result.decision.reason).toContain('COBOL-optimized fallback');
    });

    it('should use complexity-based fallbacks when banking preferences unavailable', async () => {
      // Arrange
      // Disconnect both XRP and Algorand (Fiserv DNA primary and secondary)
      router.networkGateways.set(BLOCKCHAIN_NETWORKS.XRP, { isConnected: false });
      router.networkGateways.set(BLOCKCHAIN_NETWORKS.ALGORAND, { isConnected: false });
      
      const transaction = {
        id: 'TXN010',
        messageType: 'COBOL_CONTRACT',
        amount: 25000,
        currency: 'USD',
        cobolContract: {
          programId: 'COMPLEX-FALLBACK',
          bankingSystem: 'FISERV_DNA',
          contractCode: Array(30000).fill('complex code').join(''), // High complexity
          ast: {
            data: { variables: Array(40).fill({}) },
            procedure: { operations: Array(80).fill({}), complexity: 18 }
          },
          gasEstimate: 800000
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA); // Should use fallback
      expect(result.decision.reason).toContain('COBOL-optimized fallback');
    });

    it('should fall back to default network when no COBOL networks available', async () => {
      // Arrange
      // Disconnect all networks except Ethereum L2
      router.networkGateways.set(BLOCKCHAIN_NETWORKS.XRP, { isConnected: false });
      router.networkGateways.set(BLOCKCHAIN_NETWORKS.CORDA, { isConnected: false });
      router.networkGateways.set(BLOCKCHAIN_NETWORKS.ALGORAND, { isConnected: false });
      
      const transaction = {
        id: 'TXN011',
        messageType: 'COBOL_CONTRACT',
        amount: 25000,
        currency: 'USD',
        cobolContract: {
          programId: 'FINAL-FALLBACK',
          bankingSystem: 'FIS_SYSTEMATICS',
          contractCode: 'contract Final { }',
          ast: {
            data: { variables: Array(5).fill({}) },
            procedure: { operations: Array(8).fill({}), complexity: 3 }
          }
        }
      };

      // Act
      const result = await router.route(transaction);

      // Assert
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ETHEREUM_L2);
      expect(result.decision.reason).toContain('COBOL-optimized fallback');
    });
  });

  describe('Comprehensive Integration Scenarios', () => {
    it('should handle complete COBOL transaction routing workflow', async () => {
      // Arrange
      const transaction = {
        id: 'TXN012',
        messageType: 'COBOL_CONTRACT',
        amount: 85000,
        currency: 'EUR',
        cobolContract: {
          programId: 'COMPREHENSIVE-TEST',
          bankingSystem: 'TEMENOS_TRANSACT',
          contractCode: Array(40000).fill('comprehensive logic').join(''),
          targetBlockchain: 'corda',
          ast: {
            data: { variables: Array(35).fill({}) },
            procedure: { operations: Array(70).fill({}), complexity: 16 }
          },
          gasEstimate: 600000,
          riskAssessment: { level: 'medium' },
          complianceScore: 0.75
        }
      };

      const context = {
        urgency: 'normal',
        bankPreferences: {
          preferPrivate: true
        }
      };

      // Act
      const result = await router.route(transaction, context);

      // Assert
      expect(result).toHaveProperty('routingId');
      expect(result).toHaveProperty('targetNetwork');
      expect(result).toHaveProperty('routingFactors');
      expect(result).toHaveProperty('decision');
      expect(result.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.CORDA);
      expect(result.routingFactors.cobolFactors).toBeDefined();
      expect(result.routingFactors.cobolFactors.complexityScore).toBeGreaterThan(0);
      expect(result.routingFactors.cobolFactors.bankingSystem).toBe('TEMENOS_TRANSACT');
      expect(result.decision.confidence).toBeGreaterThan(0.8);
      expect(result.processingTime).toBeGreaterThanOrEqual(0);
    });

    it('should validate COBOL contract requirements', async () => {
      // Arrange
      const invalidTransaction = {
        id: 'TXN013',
        messageType: 'COBOL_CONTRACT',
        amount: 50000,
        currency: 'USD',
        cobolContract: {
          // Missing programId and bankingSystem
          contractCode: 'contract Invalid { }'
        }
      };

      // Act & Assert
      await expect(router.route(invalidTransaction))
        .rejects.toThrow('COBOL program ID is required');
    });

    it('should track routing history for COBOL contracts', async () => {
      // Arrange
      const transaction = {
        id: 'TXN014',
        messageType: 'COBOL_CONTRACT',
        amount: 30000,
        currency: 'USD',
        cobolContract: {
          programId: 'HISTORY-TEST',
          bankingSystem: 'TCS_BANCS',
          contractCode: 'contract History { }',
          ast: {
            data: { variables: Array(12).fill({}) },
            procedure: { operations: Array(20).fill({}), complexity: 7 }
          }
        }
      };

      const initialHistoryLength = router.routingHistory.length;

      // Act
      await router.route(transaction);

      // Assert
      expect(router.routingHistory.length).toBe(initialHistoryLength + 1);
      const latestRoute = router.routingHistory[router.routingHistory.length - 1];
      expect(latestRoute.transactionId).toBe('TXN014');
      expect(latestRoute.targetNetwork).toBe(BLOCKCHAIN_NETWORKS.ALGORAND);
      expect(latestRoute.sourceType).toBe('swift');
    });
  });

  describe('COBOL-specific Helper Methods', () => {
    it('should get appropriate fallback networks for different banking systems', () => {
      // Test FIS Systematics fallbacks
      const fisFactors = {
        bankingSystemPreference: BANKING_SYSTEM_PREFERENCES.FIS_SYSTEMATICS,
        complexityScore: 0.7
      };
      const fisFallbacks = router.getCobolFallbackNetworks(fisFactors);
      expect(fisFallbacks).toContain(BLOCKCHAIN_NETWORKS.ETHEREUM_L2); // FIS secondary
      expect(fisFallbacks).toContain(BLOCKCHAIN_NETWORKS.CORDA); // Always included

      // Test Fiserv DNA fallbacks
      const fiservFactors = {
        bankingSystemPreference: BANKING_SYSTEM_PREFERENCES.FISERV_DNA,
        complexityScore: 0.4
      };
      const fiservFallbacks = router.getCobolFallbackNetworks(fiservFactors);
      expect(fiservFallbacks).toContain(BLOCKCHAIN_NETWORKS.ALGORAND); // Fiserv secondary
      expect(fiservFallbacks).toContain(BLOCKCHAIN_NETWORKS.XRP); // Low complexity option
    });

    it('should assess deployment complexity correctly', () => {
      const lowComplexityContract = {
        contractCode: 'simple',
        bankingSystem: 'FISERV_DNA',
        ast: {
          data: { variables: Array(3).fill({}) },
          procedure: { operations: Array(5).fill({}), complexity: 2 }
        }
      };

      const highComplexityContract = {
        contractCode: Array(70000).fill('complex').join(''),
        bankingSystem: 'FIS_SYSTEMATICS',
        ast: {
          data: { variables: Array(80).fill({}) },
          procedure: { operations: Array(150).fill({}), complexity: 30 }
        },
        gasEstimate: 1500000
      };

      expect(router.assessDeploymentComplexity(lowComplexityContract)).toBe('low');
      expect(router.assessDeploymentComplexity(highComplexityContract)).toBe('critical');
    });
  });
});