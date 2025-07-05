package com.legacybaas.android.examples

import android.util.Log
import com.legacybaas.android.sdk.LegacyBaaSClient
import com.legacybaas.android.sdk.services.*
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.collect

/**
 * Comprehensive examples for COBOL Transpiler Service on Android
 * Demonstrates all major features including transpilation, templates, monitoring, and analytics
 */
class CobolTranspilerExamples(private val apiKey: String) {
    private val client = LegacyBaaSClient(apiKey)
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Main)
    
    companion object {
        private const val TAG = "CobolExamples"
    }
    
    // MARK: - Basic Transpilation Examples
    
    /**
     * Example 1: Basic COBOL to Solidity transpilation
     */
    fun basicCobolToSolidity() {
        val cobolCode = """
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
        """.trimIndent()
        
        val request = CobolTranspileRequest(
            sourceCode = cobolCode,
            targetLanguage = CobolTranspilerService.TargetLanguage.SOLIDITY,
            bankingSystem = CobolTranspilerService.BankingSystem.FIS_SYSTEMATICS,
            blockchainNetwork = CobolTranspilerService.BlockchainNetwork.ETHEREUM,
            options = TranspileOptions(
                optimizeForGas = true,
                includeComments = true,
                generateInterface = true
            )
        )
        
        scope.launch {
            try {
                val result = client.cobol.transpile(request)
                Log.d(TAG, "✅ Transpilation completed successfully")
                Log.d(TAG, "🎉 Transpilation Result:")
                Log.d(TAG, "   Transaction ID: ${result.transactionId}")
                Log.d(TAG, "   Success: ${result.success}")
                result.contractAddress?.let { address ->
                    Log.d(TAG, "   Contract Address: $address")
                }
                result.gasEstimate?.let { gas ->
                    Log.d(TAG, "   Estimated Gas: $gas")
                }
                result.sourceCode?.let { code ->
                    Log.d(TAG, "   Generated Solidity Code:")
                    Log.d(TAG, "   $code")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Transpilation failed: ${e.message}")
            }
        }
    }
    
    /**
     * Example 2: COBOL to Corda transpilation with compliance
     */
    fun cobolToCordaWithCompliance() {
        val cobolCode = """
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
        """.trimIndent()
        
        val request = CobolTranspileRequest(
            sourceCode = cobolCode,
            targetLanguage = CobolTranspilerService.TargetLanguage.CORDA,
            bankingSystem = CobolTranspilerService.BankingSystem.TCS_BANCS,
            blockchainNetwork = CobolTranspilerService.BlockchainNetwork.CORDA,
            options = TranspileOptions(
                optimizeForGas = false,
                includeComments = true,
                generateInterface = true,
                strictTypeChecking = true,
                generateValidators = true,
                includeAuditTrail = true
            ),
            compliance = ComplianceConfig(
                riskThreshold = CobolTranspilerService.RiskLevel.MEDIUM,
                requireApproval = true,
                generateReport = true
            )
        )
        
        scope.launch {
            try {
                val result = client.cobol.transpile(request)
                Log.d(TAG, "✅ Corda transpilation completed with compliance")
                Log.d(TAG, "🎉 Corda Transpilation Result:")
                Log.d(TAG, "   Transaction ID: ${result.transactionId}")
                Log.d(TAG, "   Success: ${result.success}")
                result.compliance?.let { compliance ->
                    Log.d(TAG, "   Risk Score: ${compliance.riskScore}")
                    Log.d(TAG, "   Compliance Check: Passed")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Corda transpilation failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Template-Based Transpilation Examples
    
    /**
     * Example 3: Using templates for payment processing
     */
    fun templateBasedTranspilation() {
        scope.launch {
            try {
                // First, get available templates
                val templates = client.cobol.getTemplates(
                    bankingSystem = CobolTranspilerService.BankingSystem.FISERV_DNA,
                    category = "payment"
                )
                
                Log.d(TAG, "✅ Templates retrieved successfully")
                Log.d(TAG, "📋 Available Payment Templates:")
                templates.forEach { template ->
                    Log.d(TAG, "   - ${template.name}: ${template.description}")
                }
                
                // Use the first template if available
                templates.firstOrNull()?.let { firstTemplate ->
                    usePaymentTemplate(firstTemplate.id)
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Failed to retrieve templates: ${e.message}")
            }
        }
    }
    
    private suspend fun usePaymentTemplate(templateId: String) {
        val variables = mapOf(
            "fromAccount" to "ACC123456789",
            "toAccount" to "ACC987654321",
            "amount" to 50000.00,
            "currency" to "USD",
            "reference" to "PAY123456"
        )
        
        try {
            val result = client.cobol.transpileWithTemplate(
                templateId = templateId,
                variables = variables,
                targetLanguage = CobolTranspilerService.TargetLanguage.SOLIDITY,
                blockchainNetwork = CobolTranspilerService.BlockchainNetwork.ETHEREUM
            )
            
            Log.d(TAG, "✅ Template-based transpilation completed")
            Log.d(TAG, "🎉 Template Transpilation Result:")
            Log.d(TAG, "   Transaction ID: ${result.transactionId}")
            Log.d(TAG, "   Contract Address: ${result.contractAddress ?: "N/A"}")
        } catch (e: Exception) {
            Log.e(TAG, "❌ Template transpilation failed: ${e.message}")
        }
    }
    
    // MARK: - Batch Processing Examples
    
    /**
     * Example 4: Batch processing multiple COBOL files
     */
    fun batchProcessing() {
        val files = listOf(
            BatchFile(
                name = "payment-processor.cbl",
                content = """
                IDENTIFICATION DIVISION.
                PROGRAM-ID. PAYMENT-PROCESSOR.
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 PAYMENT-AMOUNT PIC 9(8)V99 COMP-3.
                PROCEDURE DIVISION.
                PROCESS-PAYMENT.
                    MOVE 1000.00 TO PAYMENT-AMOUNT.
                    DISPLAY "Processing payment: " PAYMENT-AMOUNT.
                """.trimIndent(),
                targetLanguage = CobolTranspilerService.TargetLanguage.SOLIDITY,
                blockchainNetwork = CobolTranspilerService.BlockchainNetwork.ETHEREUM
            ),
            BatchFile(
                name = "account-validator.cbl",
                content = """
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
                """.trimIndent(),
                targetLanguage = CobolTranspilerService.TargetLanguage.SOLIDITY,
                blockchainNetwork = CobolTranspilerService.BlockchainNetwork.ETHEREUM
            )
        )
        
        scope.launch {
            try {
                val results = client.cobol.batchTranspile(
                    files = files,
                    bankingSystem = CobolTranspilerService.BankingSystem.FISERV_DNA,
                    options = mapOf(
                        "optimizeForGas" to true,
                        "includeComments" to true,
                        "parallel" to true
                    )
                )
                
                Log.d(TAG, "✅ Batch transpilation completed")
                Log.d(TAG, "🎉 Batch Transpilation Results:")
                results.forEachIndexed { index, result ->
                    Log.d(TAG, "   File ${index + 1}: ${if (result.success) "Success" else "Failed"}")
                    Log.d(TAG, "   Transaction ID: ${result.transactionId}")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Batch transpilation failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Status Monitoring Examples
    
    /**
     * Example 5: Real-time status monitoring
     */
    fun monitorTranspilationStatus(jobId: String) {
        scope.launch {
            try {
                client.cobol.monitorStatus(jobId).collect { status ->
                    Log.d(TAG, "📊 Status Update:")
                    Log.d(TAG, "   Job ID: ${status.jobId}")
                    Log.d(TAG, "   Progress: ${status.progress}%")
                    Log.d(TAG, "   Stage: ${status.stage}")
                    Log.d(TAG, "   Message: ${status.message}")
                    Log.d(TAG, "   Timestamp: ${status.timestamp}")
                    
                    if (status.progress >= 100) {
                        Log.d(TAG, "🎉 Job completed successfully!")
                    }
                }
                Log.d(TAG, "✅ Status monitoring completed")
            } catch (e: Exception) {
                Log.e(TAG, "❌ Status monitoring failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Analytics and Reporting Examples
    
    /**
     * Example 6: Get transpilation analytics
     */
    fun getTranspilationAnalytics() {
        scope.launch {
            try {
                val analytics = client.cobol.getAnalytics(period = "7d")
                Log.d(TAG, "✅ Analytics retrieved successfully")
                Log.d(TAG, "📊 Transpilation Analytics (7 days):")
                Log.d(TAG, "   Total Transpilations: ${analytics.totalTranspilations}")
                Log.d(TAG, "   Success Rate: ${analytics.successRate * 100}%")
                Log.d(TAG, "   Average Time: ${analytics.averageTime} seconds")
                
                Log.d(TAG, "   By Banking System:")
                analytics.byBankingSystem.forEach { (system, count) ->
                    Log.d(TAG, "     $system: $count")
                }
                
                Log.d(TAG, "   By Network:")
                analytics.byNetwork.forEach { (network, count) ->
                    Log.d(TAG, "     $network: $count")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Failed to retrieve analytics: ${e.message}")
            }
        }
    }
    
    /**
     * Example 7: Check quota and usage
     */
    fun checkQuotaAndUsage() {
        scope.launch {
            try {
                val quota = client.cobol.getQuota()
                Log.d(TAG, "✅ Quota information retrieved")
                Log.d(TAG, "💰 Current Quota Status:")
                Log.d(TAG, "   Current Usage: ${quota.current}")
                Log.d(TAG, "   Limit: ${quota.limit}")
                Log.d(TAG, "   Remaining: ${quota.remaining}")
                Log.d(TAG, "   Reset Date: ${quota.resetDate}")
                
                if (quota.overage > 0) {
                    Log.d(TAG, "   Overage: ${quota.overage} (Cost: $${quota.overageCost})")
                }
                
                // Warning if approaching limit
                val usagePercent = quota.current.toDouble() / quota.limit.toDouble() * 100
                if (usagePercent > 80) {
                    Log.w(TAG, "⚠️  Warning: You've used ${usagePercent}% of your quota")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Failed to retrieve quota: ${e.message}")
            }
        }
    }
    
    // MARK: - Network Optimization Examples
    
    /**
     * Example 8: Select optimal blockchain network
     */
    fun selectOptimalNetwork() {
        val criteria = NetworkSelectionCriteria(
            contractComplexity = "medium",
            transactionVolume = "high",
            latencyRequirement = "low",
            costSensitivity = "medium"
        )
        
        scope.launch {
            try {
                val selection = client.cobol.selectOptimalNetwork(criteria)
                Log.d(TAG, "✅ Network selection completed")
                Log.d(TAG, "🌐 Optimal Network Selection:")
                Log.d(TAG, "   Network: ${selection.network}")
                Log.d(TAG, "   Language: ${selection.language}")
                Log.d(TAG, "   Estimated Cost: $${selection.estimatedCost}")
                Log.d(TAG, "   Estimated Time: ${selection.estimatedTime} seconds")
                Log.d(TAG, "   Reasons:")
                selection.reasons.forEach { reason ->
                    Log.d(TAG, "     - $reason")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Network selection failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Cost Estimation Examples
    
    /**
     * Example 9: Estimate transpilation cost
     */
    fun estimateTranspilationCost() {
        val request = CostEstimateRequest(
            sourceCodeLength = 2500,
            targetLanguage = "solidity",
            blockchainNetwork = "ethereum",
            complexity = "medium"
        )
        
        scope.launch {
            try {
                val estimate = client.cobol.estimateCost(request)
                Log.d(TAG, "✅ Cost estimation completed")
                Log.d(TAG, "💰 Cost Estimation:")
                Log.d(TAG, "   Total Cost: ${estimate.estimatedCost} ${estimate.currency}")
                Log.d(TAG, "   Breakdown:")
                Log.d(TAG, "     Base Cost: ${estimate.breakdown.baseCost}")
                Log.d(TAG, "     Complexity Multiplier: ${estimate.breakdown.complexityMultiplier}")
                Log.d(TAG, "     Network Fee: ${estimate.breakdown.networkFee}")
                Log.d(TAG, "     Deployment Cost: ${estimate.breakdown.deploymentCost}")
            } catch (e: Exception) {
                Log.e(TAG, "❌ Cost estimation failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Validation Examples
    
    /**
     * Example 10: Validate COBOL code without transpilation
     */
    fun validateCobolCode() {
        val cobolCode = """
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
        """.trimIndent()
        
        scope.launch {
            try {
                val result = client.cobol.validate(
                    sourceCode = cobolCode,
                    bankingSystem = CobolTranspilerService.BankingSystem.FISERV_DNA
                )
                
                Log.d(TAG, "✅ Validation completed")
                Log.d(TAG, "🔍 Validation Result:")
                Log.d(TAG, "   Valid: ${result.valid}")
                
                if (result.errors.isNotEmpty()) {
                    Log.d(TAG, "   Errors:")
                    result.errors.forEach { error ->
                        Log.d(TAG, "     Line ${error.line}, Column ${error.column}: ${error.message}")
                    }
                }
                
                if (result.warnings.isNotEmpty()) {
                    Log.d(TAG, "   Warnings:")
                    result.warnings.forEach { warning ->
                        Log.d(TAG, "     - $warning")
                    }
                }
                
                if (result.suggestions.isNotEmpty()) {
                    Log.d(TAG, "   Suggestions:")
                    result.suggestions.forEach { suggestion ->
                        Log.d(TAG, "     - $suggestion")
                    }
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Validation failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Complete Workflow Example
    
    /**
     * Example 11: Complete end-to-end workflow
     */
    fun completeWorkflow() {
        Log.d(TAG, "🚀 Starting complete COBOL transpilation workflow...")
        
        // Step 1: Validate COBOL code
        val cobolCode = """
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
        """.trimIndent()
        
        scope.launch {
            try {
                // Step 1: Validate code
                val validationResult = client.cobol.validate(
                    sourceCode = cobolCode,
                    bankingSystem = CobolTranspilerService.BankingSystem.TCS_BANCS
                )
                
                if (validationResult.valid) {
                    Log.d(TAG, "✅ Step 1: Code validation passed")
                    
                    // Step 2: Estimate cost
                    val costRequest = CostEstimateRequest(
                        sourceCodeLength = cobolCode.length,
                        targetLanguage = "solidity",
                        blockchainNetwork = "ethereum",
                        complexity = "low"
                    )
                    
                    val costEstimate = client.cobol.estimateCost(costRequest)
                    Log.d(TAG, "💰 Step 2: Cost estimated at ${costEstimate.estimatedCost} ${costEstimate.currency}")
                    
                    // Step 3: Perform transpilation
                    val transpileRequest = CobolTranspileRequest(
                        sourceCode = cobolCode,
                        targetLanguage = CobolTranspilerService.TargetLanguage.SOLIDITY,
                        bankingSystem = CobolTranspilerService.BankingSystem.TCS_BANCS,
                        blockchainNetwork = CobolTranspilerService.BlockchainNetwork.ETHEREUM,
                        options = TranspileOptions(
                            optimizeForGas = true,
                            includeComments = true,
                            monitorPerformance = true
                        )
                    )
                    
                    val result = client.cobol.transpile(transpileRequest)
                    Log.d(TAG, "✅ Step 3: Transpilation completed")
                    Log.d(TAG, "   Transaction ID: ${result.transactionId}")
                    Log.d(TAG, "   Contract Address: ${result.contractAddress ?: "N/A"}")
                    
                    result.jobId?.let { jobId ->
                        Log.d(TAG, "📊 Step 4: Monitoring job progress...")
                        monitorTranspilationStatus(jobId)
                    }
                    
                    Log.d(TAG, "🎉 Complete workflow finished successfully!")
                } else {
                    Log.e(TAG, "❌ Step 1: Code validation failed")
                }
            } catch (e: Exception) {
                Log.e(TAG, "❌ Workflow failed: ${e.message}")
            }
        }
    }
    
    // MARK: - Cleanup
    
    fun cleanup() {
        scope.cancel()
        client.close()
    }
}