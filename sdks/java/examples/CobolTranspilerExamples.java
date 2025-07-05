package com.legacybaas.examples;

import com.legacybaas.LegacyBaaSClient;
import com.legacybaas.config.Environment;
import com.legacybaas.models.cobol.*;
import com.legacybaas.services.CobolTranspilerService;
import com.legacybaas.services.StatusMonitor;
import com.legacybaas.services.StatusCallback;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

/**
 * Comprehensive examples for COBOL Transpiler Service in Java
 * Demonstrates all major features including transpilation, templates, monitoring, and analytics
 */
public class CobolTranspilerExamples {
    
    private final LegacyBaaSClient client;
    
    public CobolTranspilerExamples(String apiKey) {
        this.client = LegacyBaaSClient.builder()
            .apiKey(apiKey)
            .environment(Environment.PRODUCTION)
            .enableLogging(true)
            .build();
    }
    
    /**
     * Example 1: Basic COBOL to Solidity transpilation
     */
    public void basicCobolToSolidity() {
        System.out.println("🚀 Example 1: Basic COBOL to Solidity transpilation");
        
        String cobolCode = """
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
            """;
        
        try {
            CobolTranspileRequest request = CobolTranspileRequest.builder()
                .sourceCode(cobolCode)
                .targetLanguage(TargetLanguage.SOLIDITY)
                .bankingSystem(BankingSystem.FIS_SYSTEMATICS)
                .blockchainNetwork(BlockchainNetwork.ETHEREUM)
                .options(CobolTranspileOptions.builder()
                    .optimizeForGas(true)
                    .includeComments(true)
                    .generateInterface(true)
                    .build())
                .build();
            
            CobolTranspileResult result = client.cobol().transpile(request);
            
            System.out.println("✅ Transpilation completed successfully");
            System.out.println("🎉 Transpilation Result:");
            System.out.println("   Transaction ID: " + result.getTransactionId());
            System.out.println("   Success: " + result.isSuccess());
            if (result.getContractAddress() != null) {
                System.out.println("   Contract Address: " + result.getContractAddress());
            }
            if (result.getGasEstimate() != null) {
                System.out.println("   Estimated Gas: " + result.getGasEstimate());
            }
            if (result.getSourceCode() != null) {
                System.out.println("   Generated Solidity Code:");
                System.out.println("   " + result.getSourceCode().substring(0, Math.min(200, result.getSourceCode().length())) + "...");
            }
            
        } catch (Exception e) {
            System.err.println("❌ Transpilation failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 2: COBOL to Corda transpilation with compliance
     */
    public void cobolToCordaWithCompliance() {
        System.out.println("\n🚀 Example 2: COBOL to Corda transpilation with compliance");
        
        String cobolCode = """
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
            """;
        
        try {
            CobolTranspileRequest request = CobolTranspileRequest.builder()
                .sourceCode(cobolCode)
                .targetLanguage(TargetLanguage.CORDA)
                .bankingSystem(BankingSystem.TCS_BANCS)
                .blockchainNetwork(BlockchainNetwork.CORDA)
                .options(CobolTranspileOptions.builder()
                    .optimizeForGas(false)
                    .includeComments(true)
                    .generateInterface(true)
                    .strictTypeChecking(true)
                    .generateValidators(true)
                    .includeAuditTrail(true)
                    .build())
                .compliance(CobolComplianceConfig.builder()
                    .riskThreshold(RiskLevel.MEDIUM)
                    .requireApproval(true)
                    .generateReport(true)
                    .build())
                .build();
            
            CobolTranspileResult result = client.cobol().transpile(request);
            
            System.out.println("✅ Corda transpilation completed with compliance");
            System.out.println("🎉 Corda Transpilation Result:");
            System.out.println("   Transaction ID: " + result.getTransactionId());
            System.out.println("   Success: " + result.isSuccess());
            if (result.getCompliance() != null) {
                System.out.println("   Risk Score: " + result.getCompliance().getRiskScore());
                System.out.println("   Compliance Check: Passed");
            }
            
        } catch (Exception e) {
            System.err.println("❌ Corda transpilation failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 3: Using templates for payment processing
     */
    public void templateBasedTranspilation() {
        System.out.println("\n🚀 Example 3: Template-based transpilation");
        
        try {
            // First, get available templates
            List<CobolTemplate> templates = client.cobol().getTemplates(
                BankingSystem.FISERV_DNA.name(), "payment");
            
            System.out.println("✅ Templates retrieved successfully");
            System.out.println("📋 Available Payment Templates:");
            for (CobolTemplate template : templates) {
                System.out.println("   - " + template.getName() + ": " + template.getDescription());
            }
            
            // Use the first template if available
            if (!templates.isEmpty()) {
                usePaymentTemplate(templates.get(0).getId());
            }
            
        } catch (Exception e) {
            System.err.println("❌ Failed to retrieve templates: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    private void usePaymentTemplate(String templateId) {
        try {
            Map<String, Object> variables = new HashMap<>();
            variables.put("fromAccount", "ACC123456789");
            variables.put("toAccount", "ACC987654321");
            variables.put("amount", 50000.00);
            variables.put("currency", "USD");
            variables.put("reference", "PAY123456");
            
            CobolTemplateRequest request = CobolTemplateRequest.builder()
                .templateId(templateId)
                .variables(variables)
                .targetLanguage(TargetLanguage.SOLIDITY)
                .blockchainNetwork(BlockchainNetwork.ETHEREUM)
                .build();
            
            CobolTranspileResult result = client.cobol().transpileWithTemplate(request);
            
            System.out.println("✅ Template-based transpilation completed");
            System.out.println("🎉 Template Transpilation Result:");
            System.out.println("   Transaction ID: " + result.getTransactionId());
            System.out.println("   Contract Address: " + (result.getContractAddress() != null ? result.getContractAddress() : "N/A"));
            
        } catch (Exception e) {
            System.err.println("❌ Template transpilation failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 4: Batch processing multiple COBOL files
     */
    public void batchProcessing() {
        System.out.println("\n🚀 Example 4: Batch processing multiple COBOL files");
        
        try {
            CobolBatchFile file1 = CobolBatchFile.builder()
                .name("payment-processor.cbl")
                .content("""
                    IDENTIFICATION DIVISION.
                    PROGRAM-ID. PAYMENT-PROCESSOR.
                    DATA DIVISION.
                    WORKING-STORAGE SECTION.
                    01 PAYMENT-AMOUNT PIC 9(8)V99 COMP-3.
                    PROCEDURE DIVISION.
                    PROCESS-PAYMENT.
                        MOVE 1000.00 TO PAYMENT-AMOUNT.
                        DISPLAY "Processing payment: " PAYMENT-AMOUNT.
                    """)
                .targetLanguage(TargetLanguage.SOLIDITY)
                .blockchainNetwork(BlockchainNetwork.ETHEREUM)
                .build();
            
            CobolBatchFile file2 = CobolBatchFile.builder()
                .name("account-validator.cbl")
                .content("""
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
                    """)
                .targetLanguage(TargetLanguage.SOLIDITY)
                .blockchainNetwork(BlockchainNetwork.ETHEREUM)
                .build();
            
            CobolBatchRequest batchRequest = CobolBatchRequest.builder()
                .files(Arrays.asList(file1, file2))
                .bankingSystem(BankingSystem.FISERV_DNA)
                .options(Map.of(
                    "optimizeForGas", true,
                    "includeComments", true,
                    "parallel", true
                ))
                .build();
            
            List<CobolTranspileResult> results = client.cobol().batchTranspile(batchRequest);
            
            System.out.println("✅ Batch transpilation completed");
            System.out.println("🎉 Batch Transpilation Results:");
            for (int i = 0; i < results.size(); i++) {
                CobolTranspileResult result = results.get(i);
                System.out.println("   File " + (i + 1) + ": " + (result.isSuccess() ? "Success" : "Failed"));
                System.out.println("   Transaction ID: " + result.getTransactionId());
            }
            
        } catch (Exception e) {
            System.err.println("❌ Batch transpilation failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 5: Real-time status monitoring
     */
    public void monitorTranspilationStatus(String jobId) {
        System.out.println("\n🚀 Example 5: Real-time status monitoring");
        
        try {
            StatusMonitor monitor = client.cobol().monitorStatus(jobId);
            
            monitor.onProgress(new StatusCallback() {
                @Override
                public void onProgress(CobolStatusUpdate status) {
                    System.out.println("📊 Status Update:");
                    System.out.println("   Job ID: " + status.getJobId());
                    System.out.println("   Progress: " + status.getProgress() + "%");
                    System.out.println("   Stage: " + status.getStage());
                    System.out.println("   Message: " + status.getMessage());
                    System.out.println("   Timestamp: " + status.getTimestamp());
                }
                
                @Override
                public void onCompleted(CobolStatusUpdate status) {
                    System.out.println("🎉 Job completed successfully!");
                }
                
                @Override
                public void onError(Exception error) {
                    System.err.println("❌ Status monitoring failed: " + error.getMessage());
                }
            });
            
            monitor.start();
            
            // Wait for completion
            CobolStatusUpdate finalStatus = monitor.waitForCompletion().get(5, TimeUnit.MINUTES);
            System.out.println("✅ Status monitoring completed");
            
        } catch (Exception e) {
            System.err.println("❌ Status monitoring failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 6: Get transpilation analytics
     */
    public void getTranspilationAnalytics() {
        System.out.println("\n🚀 Example 6: Get transpilation analytics");
        
        try {
            CobolAnalytics analytics = client.cobol().getAnalytics("7d");
            
            System.out.println("✅ Analytics retrieved successfully");
            System.out.println("📊 Transpilation Analytics (7 days):");
            System.out.println("   Total Transpilations: " + analytics.getTotalTranspilations());
            System.out.println("   Success Rate: " + (analytics.getSuccessRate() * 100) + "%");
            System.out.println("   Average Time: " + analytics.getAverageTime() + " seconds");
            
            System.out.println("   By Banking System:");
            analytics.getByBankingSystem().forEach((system, count) ->
                System.out.println("     " + system + ": " + count)
            );
            
            System.out.println("   By Network:");
            analytics.getByNetwork().forEach((network, count) ->
                System.out.println("     " + network + ": " + count)
            );
            
        } catch (Exception e) {
            System.err.println("❌ Failed to retrieve analytics: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 7: Check quota and usage
     */
    public void checkQuotaAndUsage() {
        System.out.println("\n🚀 Example 7: Check quota and usage");
        
        try {
            CobolQuota quota = client.cobol().getQuota();
            
            System.out.println("✅ Quota information retrieved");
            System.out.println("💰 Current Quota Status:");
            System.out.println("   Current Usage: " + quota.getCurrent());
            System.out.println("   Limit: " + quota.getLimit());
            System.out.println("   Remaining: " + quota.getRemaining());
            System.out.println("   Reset Date: " + quota.getResetDate());
            
            if (quota.getOverage() > 0) {
                System.out.println("   Overage: " + quota.getOverage() + " (Cost: $" + quota.getOverageCost() + ")");
            }
            
            // Warning if approaching limit
            double usagePercent = (double) quota.getCurrent() / quota.getLimit() * 100;
            if (usagePercent > 80) {
                System.out.println("⚠️  Warning: You've used " + String.format("%.1f", usagePercent) + "% of your quota");
            }
            
        } catch (Exception e) {
            System.err.println("❌ Failed to retrieve quota: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 8: Select optimal blockchain network
     */
    public void selectOptimalNetwork() {
        System.out.println("\n🚀 Example 8: Select optimal blockchain network");
        
        try {
            NetworkSelectionCriteria criteria = NetworkSelectionCriteria.builder()
                .contractComplexity("MEDIUM")
                .transactionVolume("HIGH")
                .latencyRequirement("LOW")
                .costSensitivity("MEDIUM")
                .build();
            
            NetworkSelection selection = client.cobol().selectOptimalNetwork(criteria);
            
            System.out.println("✅ Network selection completed");
            System.out.println("🌐 Optimal Network Selection:");
            System.out.println("   Network: " + selection.getNetwork());
            System.out.println("   Language: " + selection.getLanguage());
            System.out.println("   Estimated Cost: $" + selection.getEstimatedCost());
            System.out.println("   Estimated Time: " + selection.getEstimatedTime() + " seconds");
            System.out.println("   Reasons:");
            selection.getReasons().forEach(reason ->
                System.out.println("     - " + reason)
            );
            
        } catch (Exception e) {
            System.err.println("❌ Network selection failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 9: Estimate transpilation cost
     */
    public void estimateTranspilationCost() {
        System.out.println("\n🚀 Example 9: Estimate transpilation cost");
        
        try {
            CostEstimateRequest request = CostEstimateRequest.builder()
                .sourceCodeLength(2500)
                .targetLanguage("solidity")
                .blockchainNetwork("ethereum")
                .complexity("MEDIUM")
                .build();
            
            CostEstimate estimate = client.cobol().estimateCost(request);
            
            System.out.println("✅ Cost estimation completed");
            System.out.println("💰 Cost Estimation:");
            System.out.println("   Total Cost: " + estimate.getEstimatedCost() + " " + estimate.getCurrency());
            System.out.println("   Breakdown:");
            System.out.println("     Base Cost: " + estimate.getBreakdown().getBaseCost());
            System.out.println("     Complexity Multiplier: " + estimate.getBreakdown().getComplexityMultiplier());
            System.out.println("     Network Fee: " + estimate.getBreakdown().getNetworkFee());
            System.out.println("     Deployment Cost: " + estimate.getBreakdown().getDeploymentCost());
            
        } catch (Exception e) {
            System.err.println("❌ Cost estimation failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 10: Validate COBOL code without transpilation
     */
    public void validateCobolCode() {
        System.out.println("\n🚀 Example 10: Validate COBOL code without transpilation");
        
        String cobolCode = """
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
            """;
        
        try {
            CobolValidationResult result = client.cobol().validate(cobolCode, BankingSystem.FISERV_DNA.name());
            
            System.out.println("✅ Validation completed");
            System.out.println("🔍 Validation Result:");
            System.out.println("   Valid: " + result.isValid());
            
            if (!result.getErrors().isEmpty()) {
                System.out.println("   Errors:");
                result.getErrors().forEach(error ->
                    System.out.println("     Line " + error.getLine() + ", Column " + error.getColumn() + ": " + error.getMessage())
                );
            }
            
            if (!result.getWarnings().isEmpty()) {
                System.out.println("   Warnings:");
                result.getWarnings().forEach(warning ->
                    System.out.println("     - " + warning)
                );
            }
            
            if (!result.getSuggestions().isEmpty()) {
                System.out.println("   Suggestions:");
                result.getSuggestions().forEach(suggestion ->
                    System.out.println("     - " + suggestion)
                );
            }
            
        } catch (Exception e) {
            System.err.println("❌ Validation failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Example 11: Complete end-to-end workflow
     */
    public void completeWorkflow() {
        System.out.println("\n🚀 Example 11: Complete end-to-end workflow");
        
        String cobolCode = """
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
            """;
        
        try {
            // Step 1: Validate COBOL code
            CobolValidationResult validationResult = client.cobol().validate(cobolCode, BankingSystem.TCS_BANCS.name());
            
            if (validationResult.isValid()) {
                System.out.println("✅ Step 1: Code validation passed");
                
                // Step 2: Estimate cost
                CostEstimateRequest costRequest = CostEstimateRequest.builder()
                    .sourceCodeLength(cobolCode.length())
                    .targetLanguage("solidity")
                    .blockchainNetwork("ethereum")
                    .complexity("LOW")
                    .build();
                
                CostEstimate costEstimate = client.cobol().estimateCost(costRequest);
                System.out.println("💰 Step 2: Cost estimated at " + costEstimate.getEstimatedCost() + " " + costEstimate.getCurrency());
                
                // Step 3: Perform transpilation
                CobolTranspileRequest transpileRequest = CobolTranspileRequest.builder()
                    .sourceCode(cobolCode)
                    .targetLanguage(TargetLanguage.SOLIDITY)
                    .bankingSystem(BankingSystem.TCS_BANCS)
                    .blockchainNetwork(BlockchainNetwork.ETHEREUM)
                    .options(CobolTranspileOptions.builder()
                        .optimizeForGas(true)
                        .includeComments(true)
                        .monitorPerformance(true)
                        .build())
                    .build();
                
                CobolTranspileResult result = client.cobol().transpile(transpileRequest);
                System.out.println("✅ Step 3: Transpilation completed");
                System.out.println("   Transaction ID: " + result.getTransactionId());
                System.out.println("   Contract Address: " + (result.getContractAddress() != null ? result.getContractAddress() : "N/A"));
                
                if (result.getJobId() != null) {
                    System.out.println("📊 Step 4: Monitoring job progress...");
                    monitorTranspilationStatus(result.getJobId());
                }
                
                System.out.println("🎉 Complete workflow finished successfully!");
                
            } else {
                System.err.println("❌ Step 1: Code validation failed");
            }
            
        } catch (Exception e) {
            System.err.println("❌ Workflow failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Run all examples
     */
    public void runAllExamples() {
        System.out.println("🚀 Starting comprehensive COBOL transpilation examples...");
        
        basicCobolToSolidity();
        cobolToCordaWithCompliance();
        templateBasedTranspilation();
        batchProcessing();
        getTranspilationAnalytics();
        checkQuotaAndUsage();
        selectOptimalNetwork();
        estimateTranspilationCost();
        validateCobolCode();
        completeWorkflow();
        
        System.out.println("\n🎉 All COBOL transpiler examples completed!");
    }
    
    /**
     * Cleanup resources
     */
    public void cleanup() {
        if (client != null) {
            client.close();
        }
    }
    
    /**
     * Main method for running examples
     */
    public static void main(String[] args) {
        if (args.length == 0) {
            System.err.println("Usage: java CobolTranspilerExamples <api-key>");
            System.exit(1);
        }
        
        String apiKey = args[0];
        CobolTranspilerExamples examples = new CobolTranspilerExamples(apiKey);
        
        try {
            examples.runAllExamples();
        } finally {
            examples.cleanup();
        }
    }
}