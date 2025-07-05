import Foundation
import LegacyBaaSSDK
import Combine

/**
 * Comprehensive examples for COBOL Transpiler Service on iOS
 * Demonstrates all major features including transpilation, templates, monitoring, and analytics
 */
class CobolTranspilerExamples {
    private let client: LegacyBaaSClient
    private var cancellables = Set<AnyCancellable>()
    
    init(apiKey: String) {
        self.client = LegacyBaaSClient(apiKey: apiKey)
    }
    
    // MARK: - Basic Transpilation Examples
    
    /**
     * Example 1: Basic COBOL to Solidity transpilation
     */
    func basicCobolToSolidity() {
        let cobolCode = """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TRANSFER-FUNDS.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 ACCOUNT-FROM     PIC X(10).
        01 ACCOUNT-TO       PIC X(10).
        01 AMOUNT           PIC 9(8)V99 COMP-3.
        01 RESULT           PIC X(20).
        
        PROCEDURE DIVISION.
        MAIN-LOGIC.
            MOVE "1234567890" TO ACCOUNT-FROM.
            MOVE "0987654321" TO ACCOUNT-TO.
            MOVE 10000.50 TO AMOUNT.
            PERFORM TRANSFER-FUNDS.
            STOP RUN.
        
        TRANSFER-FUNDS.
            MOVE "TRANSFER SUCCESSFUL" TO RESULT.
        """
        
        let request = CobolTranspilerService.CobolTranspileRequest(
            sourceCode: cobolCode,
            targetLanguage: .solidity,
            bankingSystem: .fisBb,
            blockchainNetwork: .ethereum,
            options: CobolTranspilerService.TranspileOptions(
                optimizeForGas: true,
                includeComments: true,
                generateInterface: true
            )
        )
        
        if #available(iOS 13.0, *) {
            client.cobol.transpile(request)
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Transpilation completed successfully")
                        case .failure(let error):
                            print("❌ Transpilation failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { result in
                        print("🎉 Transpilation Result:")
                        print("   Transaction ID: \(result.transactionId)")
                        print("   Success: \(result.success)")
                        if let contractAddress = result.contractAddress {
                            print("   Contract Address: \(contractAddress)")
                        }
                        if let gasEstimate = result.gasEstimate {
                            print("   Estimated Gas: \(gasEstimate)")
                        }
                        if let sourceCode = result.sourceCode {
                            print("   Generated Solidity Code:")
                            print("   \(sourceCode)")
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    /**
     * Example 2: COBOL to Corda transpilation with compliance
     */
    func cobolToCordaWithCompliance() {
        let cobolCode = """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. TRADE-SETTLEMENT.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 TRADE-ID         PIC X(12).
        01 PARTY-A          PIC X(20).
        01 PARTY-B          PIC X(20).
        01 AMOUNT           PIC 9(10)V99 COMP-3.
        01 CURRENCY         PIC X(3).
        01 SETTLEMENT-DATE  PIC X(10).
        
        PROCEDURE DIVISION.
        MAIN-LOGIC.
            MOVE "TRD123456789" TO TRADE-ID.
            MOVE "BANK-A" TO PARTY-A.
            MOVE "BANK-B" TO PARTY-B.
            MOVE 5000000.00 TO AMOUNT.
            MOVE "USD" TO CURRENCY.
            MOVE "2024-12-31" TO SETTLEMENT-DATE.
            PERFORM SETTLE-TRADE.
            STOP RUN.
        
        SETTLE-TRADE.
            DISPLAY "TRADE SETTLED: " TRADE-ID.
        """
        
        let request = CobolTranspilerService.CobolTranspileRequest(
            sourceCode: cobolCode,
            targetLanguage: .corda,
            bankingSystem: .tcsBancs,
            blockchainNetwork: .corda,
            options: CobolTranspilerService.TranspileOptions(
                optimizeForGas: false,
                includeComments: true,
                generateInterface: true,
                strictTypeChecking: true,
                generateValidators: true,
                includeAuditTrail: true
            ),
            compliance: CobolTranspilerService.ComplianceConfig(
                riskThreshold: .medium,
                requireApproval: true,
                generateReport: true
            )
        )
        
        if #available(iOS 13.0, *) {
            client.cobol.transpile(request)
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Corda transpilation completed with compliance")
                        case .failure(let error):
                            print("❌ Corda transpilation failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { result in
                        print("🎉 Corda Transpilation Result:")
                        print("   Transaction ID: \(result.transactionId)")
                        print("   Success: \(result.success)")
                        if let compliance = result.compliance {
                            print("   Risk Score: \(compliance.riskScore)")
                            print("   Compliance Check: Passed")
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Template-Based Transpilation Examples
    
    /**
     * Example 3: Using templates for payment processing
     */
    func templateBasedTranspilation() {
        if #available(iOS 13.0, *) {
            // First, get available templates
            client.cobol.getTemplates(bankingSystem: .fiservDNA, category: "payment")
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Templates retrieved successfully")
                        case .failure(let error):
                            print("❌ Failed to retrieve templates: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { templates in
                        print("📋 Available Payment Templates:")
                        for template in templates {
                            print("   - \(template.name): \(template.description)")
                        }
                        
                        // Use the first template if available
                        if let firstTemplate = templates.first {
                            self.usePaymentTemplate(templateId: firstTemplate.id)
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    private func usePaymentTemplate(templateId: String) {
        let variables = [
            "fromAccount": "ACC123456789",
            "toAccount": "ACC987654321",
            "amount": 50000.00,
            "currency": "USD",
            "reference": "PAY123456"
        ]
        
        if #available(iOS 13.0, *) {
            client.cobol.transpileWithTemplate(
                templateId: templateId,
                variables: variables,
                targetLanguage: .solidity,
                blockchainNetwork: .ethereum
            )
            .sink(
                receiveCompletion: { completion in
                    switch completion {
                    case .finished:
                        print("✅ Template-based transpilation completed")
                    case .failure(let error):
                        print("❌ Template transpilation failed: \(error.localizedDescription)")
                    }
                },
                receiveValue: { result in
                    print("🎉 Template Transpilation Result:")
                    print("   Transaction ID: \(result.transactionId)")
                    print("   Contract Address: \(result.contractAddress ?? "N/A")")
                }
            )
            .store(in: &cancellables)
        }
    }
    
    // MARK: - Batch Processing Examples
    
    /**
     * Example 4: Batch processing multiple COBOL files
     */
    func batchProcessing() {
        let files = [
            BatchFile(
                name: "payment-processor.cbl",
                content: """
                IDENTIFICATION DIVISION.
                PROGRAM-ID. PAYMENT-PROCESSOR.
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 PAYMENT-AMOUNT PIC 9(8)V99 COMP-3.
                PROCEDURE DIVISION.
                PROCESS-PAYMENT.
                    MOVE 1000.00 TO PAYMENT-AMOUNT.
                    DISPLAY "Processing payment: " PAYMENT-AMOUNT.
                """,
                targetLanguage: .solidity,
                blockchainNetwork: .ethereum
            ),
            BatchFile(
                name: "account-validator.cbl",
                content: """
                IDENTIFICATION DIVISION.
                PROGRAM-ID. ACCOUNT-VALIDATOR.
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 ACCOUNT-NUMBER PIC X(10).
                01 VALID-FLAG PIC X.
                PROCEDURE DIVISION.
                VALIDATE-ACCOUNT.
                    MOVE "1234567890" TO ACCOUNT-NUMBER.
                    MOVE "Y" TO VALID-FLAG.
                    DISPLAY "Account valid: " ACCOUNT-NUMBER.
                """,
                targetLanguage: .solidity,
                blockchainNetwork: .ethereum
            )
        ]
        
        if #available(iOS 13.0, *) {
            client.cobol.batchTranspile(
                files: files,
                bankingSystem: .fiservDNA,
                options: [
                    "optimizeForGas": true,
                    "includeComments": true,
                    "parallel": true
                ]
            )
            .sink(
                receiveCompletion: { completion in
                    switch completion {
                    case .finished:
                        print("✅ Batch transpilation completed")
                    case .failure(let error):
                        print("❌ Batch transpilation failed: \(error.localizedDescription)")
                    }
                },
                receiveValue: { results in
                    print("🎉 Batch Transpilation Results:")
                    for (index, result) in results.enumerated() {
                        print("   File \(index + 1): \(result.success ? "Success" : "Failed")")
                        print("   Transaction ID: \(result.transactionId)")
                    }
                }
            )
            .store(in: &cancellables)
        }
    }
    
    // MARK: - Status Monitoring Examples
    
    /**
     * Example 5: Real-time status monitoring
     */
    func monitorTranspilationStatus(jobId: String) {
        if #available(iOS 13.0, *) {
            client.cobol.monitorStatus(jobId: jobId)
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Status monitoring completed")
                        case .failure(let error):
                            print("❌ Status monitoring failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { status in
                        print("📊 Status Update:")
                        print("   Job ID: \(status.jobId)")
                        print("   Progress: \(status.progress)%")
                        print("   Stage: \(status.stage)")
                        print("   Message: \(status.message)")
                        print("   Timestamp: \(status.timestamp)")
                        
                        if status.progress >= 100 {
                            print("🎉 Job completed successfully!")
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Analytics and Reporting Examples
    
    /**
     * Example 6: Get transpilation analytics
     */
    func getTranspilationAnalytics() {
        if #available(iOS 13.0, *) {
            client.cobol.getAnalytics(period: "7d")
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Analytics retrieved successfully")
                        case .failure(let error):
                            print("❌ Failed to retrieve analytics: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { analytics in
                        print("📊 Transpilation Analytics (7 days):")
                        print("   Total Transpilations: \(analytics.totalTranspilations)")
                        print("   Success Rate: \(analytics.successRate * 100)%")
                        print("   Average Time: \(analytics.averageTime) seconds")
                        
                        print("   By Banking System:")
                        for (system, count) in analytics.byBankingSystem {
                            print("     \(system): \(count)")
                        }
                        
                        print("   By Network:")
                        for (network, count) in analytics.byNetwork {
                            print("     \(network): \(count)")
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    /**
     * Example 7: Check quota and usage
     */
    func checkQuotaAndUsage() {
        if #available(iOS 13.0, *) {
            client.cobol.getQuota()
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Quota information retrieved")
                        case .failure(let error):
                            print("❌ Failed to retrieve quota: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { quota in
                        print("💰 Current Quota Status:")
                        print("   Current Usage: \(quota.current)")
                        print("   Limit: \(quota.limit)")
                        print("   Remaining: \(quota.remaining)")
                        print("   Reset Date: \(quota.resetDate)")
                        
                        if quota.overage > 0 {
                            print("   Overage: \(quota.overage) (Cost: $\(quota.overageCost))")
                        }
                        
                        // Warning if approaching limit
                        let usagePercent = Double(quota.current) / Double(quota.limit) * 100
                        if usagePercent > 80 {
                            print("⚠️  Warning: You've used \(usagePercent)% of your quota")
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Network Optimization Examples
    
    /**
     * Example 8: Select optimal blockchain network
     */
    func selectOptimalNetwork() {
        let criteria = NetworkSelectionCriteria(
            contractComplexity: "medium",
            transactionVolume: "high",
            latencyRequirement: "low",
            costSensitivity: "medium"
        )
        
        if #available(iOS 13.0, *) {
            client.cobol.selectOptimalNetwork(criteria: criteria)
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Network selection completed")
                        case .failure(let error):
                            print("❌ Network selection failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { selection in
                        print("🌐 Optimal Network Selection:")
                        print("   Network: \(selection.network)")
                        print("   Language: \(selection.language)")
                        print("   Estimated Cost: $\(selection.estimatedCost)")
                        print("   Estimated Time: \(selection.estimatedTime) seconds")
                        print("   Reasons:")
                        for reason in selection.reasons {
                            print("     - \(reason)")
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Cost Estimation Examples
    
    /**
     * Example 9: Estimate transpilation cost
     */
    func estimateTranspilationCost() {
        let request = CostEstimateRequest(
            sourceCodeLength: 2500,
            targetLanguage: "solidity",
            blockchainNetwork: "ethereum",
            complexity: "medium"
        )
        
        if #available(iOS 13.0, *) {
            client.cobol.estimateCost(request: request)
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Cost estimation completed")
                        case .failure(let error):
                            print("❌ Cost estimation failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { estimate in
                        print("💰 Cost Estimation:")
                        print("   Total Cost: \(estimate.estimatedCost) \(estimate.currency)")
                        print("   Breakdown:")
                        print("     Base Cost: \(estimate.breakdown.baseCost)")
                        print("     Complexity Multiplier: \(estimate.breakdown.complexityMultiplier)")
                        print("     Network Fee: \(estimate.breakdown.networkFee)")
                        print("     Deployment Cost: \(estimate.breakdown.deploymentCost)")
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Validation Examples
    
    /**
     * Example 10: Validate COBOL code without transpilation
     */
    func validateCobolCode() {
        let cobolCode = """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. VALIDATION-TEST.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 TEST-VAR PIC X(10).
        
        PROCEDURE DIVISION.
        MAIN-LOGIC.
            MOVE "TEST" TO TEST-VAR.
            DISPLAY TEST-VAR.
            STOP RUN.
        """
        
        if #available(iOS 13.0, *) {
            client.cobol.validate(sourceCode: cobolCode, bankingSystem: .fiservDNA)
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("✅ Validation completed")
                        case .failure(let error):
                            print("❌ Validation failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { result in
                        print("🔍 Validation Result:")
                        print("   Valid: \(result.valid)")
                        
                        if !result.errors.isEmpty {
                            print("   Errors:")
                            for error in result.errors {
                                print("     Line \(error.line), Column \(error.column): \(error.message)")
                            }
                        }
                        
                        if !result.warnings.isEmpty {
                            print("   Warnings:")
                            for warning in result.warnings {
                                print("     - \(warning)")
                            }
                        }
                        
                        if !result.suggestions.isEmpty {
                            print("   Suggestions:")
                            for suggestion in result.suggestions {
                                print("     - \(suggestion)")
                            }
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Complete Workflow Example
    
    /**
     * Example 11: Complete end-to-end workflow
     */
    func completeWorkflow() {
        print("🚀 Starting complete COBOL transpilation workflow...")
        
        // Step 1: Validate COBOL code
        let cobolCode = """
        IDENTIFICATION DIVISION.
        PROGRAM-ID. COMPLETE-WORKFLOW.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01 ACCOUNT-BALANCE PIC 9(8)V99 COMP-3.
        01 TRANSACTION-AMT PIC 9(8)V99 COMP-3.
        
        PROCEDURE DIVISION.
        MAIN-LOGIC.
            MOVE 10000.00 TO ACCOUNT-BALANCE.
            MOVE 500.00 TO TRANSACTION-AMT.
            PERFORM PROCESS-TRANSACTION.
            STOP RUN.
        
        PROCESS-TRANSACTION.
            SUBTRACT TRANSACTION-AMT FROM ACCOUNT-BALANCE.
            DISPLAY "New balance: " ACCOUNT-BALANCE.
        """
        
        if #available(iOS 13.0, *) {
            client.cobol.validate(sourceCode: cobolCode, bankingSystem: .tcsBancs)
                .flatMap { validationResult -> AnyPublisher<CobolTranspilerService.CobolTranspileResult, Error> in
                    if validationResult.valid {
                        print("✅ Step 1: Code validation passed")
                        
                        // Step 2: Estimate cost
                        let costRequest = CostEstimateRequest(
                            sourceCodeLength: cobolCode.count,
                            targetLanguage: "solidity",
                            blockchainNetwork: "ethereum",
                            complexity: "low"
                        )
                        
                        return self.client.cobol.estimateCost(request: costRequest)
                            .flatMap { costEstimate -> AnyPublisher<CobolTranspilerService.CobolTranspileResult, Error> in
                                print("💰 Step 2: Cost estimated at \(costEstimate.estimatedCost) \(costEstimate.currency)")
                                
                                // Step 3: Perform transpilation
                                let transpileRequest = CobolTranspilerService.CobolTranspileRequest(
                                    sourceCode: cobolCode,
                                    targetLanguage: .solidity,
                                    bankingSystem: .tcsBancs,
                                    blockchainNetwork: .ethereum,
                                    options: CobolTranspilerService.TranspileOptions(
                                        optimizeForGas: true,
                                        includeComments: true,
                                        monitorPerformance: true
                                    )
                                )
                                
                                return self.client.cobol.transpile(transpileRequest)
                            }
                            .eraseToAnyPublisher()
                    } else {
                        print("❌ Step 1: Code validation failed")
                        return Fail(error: NSError(domain: "ValidationError", code: 400, userInfo: [NSLocalizedDescriptionKey: "COBOL code validation failed"]))
                            .eraseToAnyPublisher()
                    }
                }
                .sink(
                    receiveCompletion: { completion in
                        switch completion {
                        case .finished:
                            print("🎉 Complete workflow finished successfully!")
                        case .failure(let error):
                            print("❌ Workflow failed: \(error.localizedDescription)")
                        }
                    },
                    receiveValue: { result in
                        print("✅ Step 3: Transpilation completed")
                        print("   Transaction ID: \(result.transactionId)")
                        print("   Contract Address: \(result.contractAddress ?? "N/A")")
                        
                        if let jobId = result.jobId {
                            print("📊 Step 4: Monitoring job progress...")
                            self.monitorTranspilationStatus(jobId: jobId)
                        }
                    }
                )
                .store(in: &cancellables)
        }
    }
    
    // MARK: - Cleanup
    
    func cleanup() {
        cancellables.removeAll()
    }
}