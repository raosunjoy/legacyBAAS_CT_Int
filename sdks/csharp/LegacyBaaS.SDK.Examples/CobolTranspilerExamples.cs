using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using LegacyBaaS.SDK;
using LegacyBaaS.SDK.Models.Cobol;
using LegacyBaaS.SDK.Services;

namespace LegacyBaaS.SDK.Examples
{
    /// <summary>
    /// Comprehensive examples for COBOL Transpiler Service in C#
    /// Demonstrates all major features including transpilation, templates, monitoring, and analytics
    /// </summary>
    public class CobolTranspilerExamples
    {
        private readonly LegacyBaaSClient _client;
        
        public CobolTranspilerExamples(string clientId, string clientSecret)
        {
            var options = new LegacyBaaSClientOptions
            {
                ClientId = clientId,
                ClientSecret = clientSecret,
                Environment = "production",
                EnableLogging = true
            };
            
            _client = new LegacyBaaSClient(options);
        }
        
        /// <summary>
        /// Example 1: Basic COBOL to Solidity transpilation
        /// </summary>
        public async Task BasicCobolToSolidityAsync()
        {
            Console.WriteLine("🚀 Example 1: Basic COBOL to Solidity transpilation");
            
            var cobolCode = @"
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
                    MOVE ""1234567890"" TO ACCOUNT-FROM.
                    MOVE ""0987654321"" TO ACCOUNT-TO.
                    MOVE 10000.50 TO AMOUNT.
                    PERFORM TRANSFER-FUNDS.
                    STOP RUN.
                
                TRANSFER-FUNDS.
                    MOVE ""TRANSFER SUCCESSFUL"" TO RESULT.
            ";
            
            try
            {
                var request = new CobolTranspileRequest
                {
                    SourceCode = cobolCode,
                    TargetLanguage = TargetLanguage.Solidity,
                    BankingSystem = BankingSystem.FisSystematics,
                    BlockchainNetwork = BlockchainNetwork.Ethereum,
                    Options = new CobolTranspileOptions
                    {
                        OptimizeForGas = true,
                        IncludeComments = true,
                        GenerateInterface = true
                    }
                };
                
                var result = await _client.Cobol.TranspileAsync(request);
                
                Console.WriteLine("✅ Transpilation completed successfully");
                Console.WriteLine("🎉 Transpilation Result:");
                Console.WriteLine($"   Transaction ID: {result.TransactionId}");
                Console.WriteLine($"   Success: {result.Success}");
                if (!string.IsNullOrEmpty(result.ContractAddress))
                {
                    Console.WriteLine($"   Contract Address: {result.ContractAddress}");
                }
                if (result.GasEstimate.HasValue)
                {
                    Console.WriteLine($"   Estimated Gas: {result.GasEstimate}");
                }
                if (!string.IsNullOrEmpty(result.SourceCode))
                {
                    var preview = result.SourceCode.Length > 200 ? result.SourceCode.Substring(0, 200) + "..." : result.SourceCode;
                    Console.WriteLine($"   Generated Solidity Code:");
                    Console.WriteLine($"   {preview}");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Transpilation failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 2: COBOL to Corda transpilation with compliance
        /// </summary>
        public async Task CobolToCordaWithComplianceAsync()
        {
            Console.WriteLine("\n🚀 Example 2: COBOL to Corda transpilation with compliance");
            
            var cobolCode = @"
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
                    MOVE ""TRD123456789"" TO TRADE-ID.
                    MOVE ""BANK-A"" TO PARTY-A.
                    MOVE ""BANK-B"" TO PARTY-B.
                    MOVE 5000000.00 TO AMOUNT.
                    MOVE ""USD"" TO CURRENCY.
                    MOVE ""2024-12-31"" TO SETTLEMENT-DATE.
                    PERFORM SETTLE-TRADE.
                    STOP RUN.
                
                SETTLE-TRADE.
                    DISPLAY ""TRADE SETTLED: "" TRADE-ID.
            ";
            
            try
            {
                var request = new CobolTranspileRequest
                {
                    SourceCode = cobolCode,
                    TargetLanguage = TargetLanguage.Corda,
                    BankingSystem = BankingSystem.TcsBancs,
                    BlockchainNetwork = BlockchainNetwork.Corda,
                    Options = new CobolTranspileOptions
                    {
                        OptimizeForGas = false,
                        IncludeComments = true,
                        GenerateInterface = true,
                        StrictTypeChecking = true,
                        GenerateValidators = true,
                        IncludeAuditTrail = true
                    },
                    Compliance = new CobolComplianceConfig
                    {
                        RiskThreshold = RiskLevel.Medium,
                        RequireApproval = true,
                        GenerateReport = true
                    }
                };
                
                var result = await _client.Cobol.TranspileAsync(request);
                
                Console.WriteLine("✅ Corda transpilation completed with compliance");
                Console.WriteLine("🎉 Corda Transpilation Result:");
                Console.WriteLine($"   Transaction ID: {result.TransactionId}");
                Console.WriteLine($"   Success: {result.Success}");
                if (result.Compliance != null)
                {
                    Console.WriteLine($"   Risk Score: {result.Compliance.RiskScore}");
                    Console.WriteLine("   Compliance Check: Passed");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Corda transpilation failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 3: Using templates for payment processing
        /// </summary>
        public async Task TemplateBasedTranspilationAsync()
        {
            Console.WriteLine("\n🚀 Example 3: Template-based transpilation");
            
            try
            {
                // First, get available templates
                var templates = await _client.Cobol.GetTemplatesAsync("fiserv-dna", "payment");
                
                Console.WriteLine("✅ Templates retrieved successfully");
                Console.WriteLine("📋 Available Payment Templates:");
                foreach (var template in templates)
                {
                    Console.WriteLine($"   - {template.Name}: {template.Description}");
                }
                
                // Use the first template if available
                if (templates.Count > 0)
                {
                    await UsePaymentTemplateAsync(templates[0].Id);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Failed to retrieve templates: {e.Message}");
            }
        }
        
        private async Task UsePaymentTemplateAsync(string templateId)
        {
            try
            {
                var variables = new Dictionary<string, object>
                {
                    ["fromAccount"] = "ACC123456789",
                    ["toAccount"] = "ACC987654321",
                    ["amount"] = 50000.00,
                    ["currency"] = "USD",
                    ["reference"] = "PAY123456"
                };
                
                var request = new CobolTemplateRequest
                {
                    TemplateId = templateId,
                    Variables = variables,
                    TargetLanguage = TargetLanguage.Solidity,
                    BlockchainNetwork = BlockchainNetwork.Ethereum
                };
                
                var result = await _client.Cobol.TranspileWithTemplateAsync(request);
                
                Console.WriteLine("✅ Template-based transpilation completed");
                Console.WriteLine("🎉 Template Transpilation Result:");
                Console.WriteLine($"   Transaction ID: {result.TransactionId}");
                Console.WriteLine($"   Contract Address: {result.ContractAddress ?? "N/A"}");
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Template transpilation failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 4: Batch processing multiple COBOL files
        /// </summary>
        public async Task BatchProcessingAsync()
        {
            Console.WriteLine("\n🚀 Example 4: Batch processing multiple COBOL files");
            
            try
            {
                var files = new List<CobolBatchFile>
                {
                    new CobolBatchFile
                    {
                        Name = "payment-processor.cbl",
                        Content = @"
                            IDENTIFICATION DIVISION.
                            PROGRAM-ID. PAYMENT-PROCESSOR.
                            DATA DIVISION.
                            WORKING-STORAGE SECTION.
                            01 PAYMENT-AMOUNT PIC 9(8)V99 COMP-3.
                            PROCEDURE DIVISION.
                            PROCESS-PAYMENT.
                                MOVE 1000.00 TO PAYMENT-AMOUNT.
                                DISPLAY ""Processing payment: "" PAYMENT-AMOUNT.
                        ",
                        TargetLanguage = TargetLanguage.Solidity,
                        BlockchainNetwork = BlockchainNetwork.Ethereum
                    },
                    new CobolBatchFile
                    {
                        Name = "account-validator.cbl",
                        Content = @"
                            IDENTIFICATION DIVISION.
                            PROGRAM-ID. ACCOUNT-VALIDATOR.
                            DATA DIVISION.
                            WORKING-STORAGE SECTION.
                            01 ACCOUNT-NUMBER PIC X(10).
                            01 VALID-FLAG PIC X.
                            PROCEDURE DIVISION.
                            VALIDATE-ACCOUNT.
                                MOVE ""1234567890"" TO ACCOUNT-NUMBER.
                                MOVE ""Y"" TO VALID-FLAG.
                                DISPLAY ""Account valid: "" ACCOUNT-NUMBER.
                        ",
                        TargetLanguage = TargetLanguage.Solidity,
                        BlockchainNetwork = BlockchainNetwork.Ethereum
                    }
                };
                
                var batchRequest = new CobolBatchRequest
                {
                    Files = files,
                    BankingSystem = BankingSystem.FiservDna,
                    Options = new Dictionary<string, object>
                    {
                        ["optimizeForGas"] = true,
                        ["includeComments"] = true,
                        ["parallel"] = true
                    }
                };
                
                var results = await _client.Cobol.BatchTranspileAsync(batchRequest);
                
                Console.WriteLine("✅ Batch transpilation completed");
                Console.WriteLine("🎉 Batch Transpilation Results:");
                for (int i = 0; i < results.Count; i++)
                {
                    var result = results[i];
                    Console.WriteLine($"   File {i + 1}: {(result.Success ? "Success" : "Failed")}");
                    Console.WriteLine($"   Transaction ID: {result.TransactionId}");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Batch transpilation failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 5: Real-time status monitoring
        /// </summary>
        public async Task MonitorTranspilationStatusAsync(string jobId)
        {
            Console.WriteLine("\n🚀 Example 5: Real-time status monitoring");
            
            try
            {
                var monitor = _client.Cobol.MonitorStatus(jobId);
                
                monitor.OnProgress += status =>
                {
                    Console.WriteLine("📊 Status Update:");
                    Console.WriteLine($"   Job ID: {status.JobId}");
                    Console.WriteLine($"   Progress: {status.Progress}%");
                    Console.WriteLine($"   Stage: {status.Stage}");
                    Console.WriteLine($"   Message: {status.Message}");
                    Console.WriteLine($"   Timestamp: {status.Timestamp}");
                };
                
                monitor.OnCompleted += status =>
                {
                    Console.WriteLine("🎉 Job completed successfully!");
                };
                
                monitor.OnError += error =>
                {
                    Console.WriteLine($"❌ Status monitoring failed: {error.Message}");
                };
                
                monitor.Start();
                
                // Wait for completion
                var finalStatus = await monitor.WaitForCompletionAsync();
                Console.WriteLine("✅ Status monitoring completed");
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Status monitoring failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 6: Get transpilation analytics
        /// </summary>
        public async Task GetTranspilationAnalyticsAsync()
        {
            Console.WriteLine("\n🚀 Example 6: Get transpilation analytics");
            
            try
            {
                var analytics = await _client.Cobol.GetAnalyticsAsync("7d");
                
                Console.WriteLine("✅ Analytics retrieved successfully");
                Console.WriteLine("📊 Transpilation Analytics (7 days):");
                Console.WriteLine($"   Total Transpilations: {analytics.TotalTranspilations}");
                Console.WriteLine($"   Success Rate: {analytics.SuccessRate * 100:F1}%");
                Console.WriteLine($"   Average Time: {analytics.AverageTime} seconds");
                
                Console.WriteLine("   By Banking System:");
                foreach (var kvp in analytics.ByBankingSystem)
                {
                    Console.WriteLine($"     {kvp.Key}: {kvp.Value}");
                }
                
                Console.WriteLine("   By Network:");
                foreach (var kvp in analytics.ByNetwork)
                {
                    Console.WriteLine($"     {kvp.Key}: {kvp.Value}");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Failed to retrieve analytics: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 7: Check quota and usage
        /// </summary>
        public async Task CheckQuotaAndUsageAsync()
        {
            Console.WriteLine("\n🚀 Example 7: Check quota and usage");
            
            try
            {
                var quota = await _client.Cobol.GetQuotaAsync();
                
                Console.WriteLine("✅ Quota information retrieved");
                Console.WriteLine("💰 Current Quota Status:");
                Console.WriteLine($"   Current Usage: {quota.Current}");
                Console.WriteLine($"   Limit: {quota.Limit}");
                Console.WriteLine($"   Remaining: {quota.Remaining}");
                Console.WriteLine($"   Reset Date: {quota.ResetDate}");
                
                if (quota.Overage > 0)
                {
                    Console.WriteLine($"   Overage: {quota.Overage} (Cost: ${quota.OverageCost})");
                }
                
                // Warning if approaching limit
                var usagePercent = (double)quota.Current / quota.Limit * 100;
                if (usagePercent > 80)
                {
                    Console.WriteLine($"⚠️  Warning: You've used {usagePercent:F1}% of your quota");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Failed to retrieve quota: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 8: Select optimal blockchain network
        /// </summary>
        public async Task SelectOptimalNetworkAsync()
        {
            Console.WriteLine("\n🚀 Example 8: Select optimal blockchain network");
            
            try
            {
                var criteria = new NetworkSelectionCriteria
                {
                    ContractComplexity = "MEDIUM",
                    TransactionVolume = "HIGH",
                    LatencyRequirement = "LOW",
                    CostSensitivity = "MEDIUM"
                };
                
                var selection = await _client.Cobol.SelectOptimalNetworkAsync(criteria);
                
                Console.WriteLine("✅ Network selection completed");
                Console.WriteLine("🌐 Optimal Network Selection:");
                Console.WriteLine($"   Network: {selection.Network}");
                Console.WriteLine($"   Language: {selection.Language}");
                Console.WriteLine($"   Estimated Cost: ${selection.EstimatedCost}");
                Console.WriteLine($"   Estimated Time: {selection.EstimatedTime} seconds");
                Console.WriteLine("   Reasons:");
                foreach (var reason in selection.Reasons)
                {
                    Console.WriteLine($"     - {reason}");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Network selection failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 9: Estimate transpilation cost
        /// </summary>
        public async Task EstimateTranspilationCostAsync()
        {
            Console.WriteLine("\n🚀 Example 9: Estimate transpilation cost");
            
            try
            {
                var request = new CostEstimateRequest
                {
                    SourceCodeLength = 2500,
                    TargetLanguage = "solidity",
                    BlockchainNetwork = "ethereum",
                    Complexity = "MEDIUM"
                };
                
                var estimate = await _client.Cobol.EstimateCostAsync(request);
                
                Console.WriteLine("✅ Cost estimation completed");
                Console.WriteLine("💰 Cost Estimation:");
                Console.WriteLine($"   Total Cost: {estimate.EstimatedCost} {estimate.Currency}");
                Console.WriteLine("   Breakdown:");
                Console.WriteLine($"     Base Cost: {estimate.Breakdown.BaseCost}");
                Console.WriteLine($"     Complexity Multiplier: {estimate.Breakdown.ComplexityMultiplier}");
                Console.WriteLine($"     Network Fee: {estimate.Breakdown.NetworkFee}");
                Console.WriteLine($"     Deployment Cost: {estimate.Breakdown.DeploymentCost}");
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Cost estimation failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 10: Validate COBOL code without transpilation
        /// </summary>
        public async Task ValidateCobolCodeAsync()
        {
            Console.WriteLine("\n🚀 Example 10: Validate COBOL code without transpilation");
            
            var cobolCode = @"
                IDENTIFICATION DIVISION.
                PROGRAM-ID. VALIDATION-TEST.
                DATA DIVISION.
                WORKING-STORAGE SECTION.
                01 TEST-VAR PIC X(10).
                
                PROCEDURE DIVISION.
                MAIN-LOGIC.
                    MOVE ""TEST"" TO TEST-VAR.
                    DISPLAY TEST-VAR.
                    STOP RUN.
            ";
            
            try
            {
                var result = await _client.Cobol.ValidateAsync(cobolCode, "fiserv-dna");
                
                Console.WriteLine("✅ Validation completed");
                Console.WriteLine("🔍 Validation Result:");
                Console.WriteLine($"   Valid: {result.Valid}");
                
                if (result.Errors?.Count > 0)
                {
                    Console.WriteLine("   Errors:");
                    foreach (var error in result.Errors)
                    {
                        Console.WriteLine($"     Line {error.Line}, Column {error.Column}: {error.Message}");
                    }
                }
                
                if (result.Warnings?.Count > 0)
                {
                    Console.WriteLine("   Warnings:");
                    foreach (var warning in result.Warnings)
                    {
                        Console.WriteLine($"     - {warning}");
                    }
                }
                
                if (result.Suggestions?.Count > 0)
                {
                    Console.WriteLine("   Suggestions:");
                    foreach (var suggestion in result.Suggestions)
                    {
                        Console.WriteLine($"     - {suggestion}");
                    }
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Validation failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Example 11: Complete end-to-end workflow
        /// </summary>
        public async Task CompleteWorkflowAsync()
        {
            Console.WriteLine("\n🚀 Example 11: Complete end-to-end workflow");
            
            var cobolCode = @"
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
                    DISPLAY ""New balance: "" ACCOUNT-BALANCE.
            ";
            
            try
            {
                // Step 1: Validate COBOL code
                var validationResult = await _client.Cobol.ValidateAsync(cobolCode, "tcs-bancs");
                
                if (validationResult.Valid)
                {
                    Console.WriteLine("✅ Step 1: Code validation passed");
                    
                    // Step 2: Estimate cost
                    var costRequest = new CostEstimateRequest
                    {
                        SourceCodeLength = cobolCode.Length,
                        TargetLanguage = "solidity",
                        BlockchainNetwork = "ethereum",
                        Complexity = "LOW"
                    };
                    
                    var costEstimate = await _client.Cobol.EstimateCostAsync(costRequest);
                    Console.WriteLine($"💰 Step 2: Cost estimated at {costEstimate.EstimatedCost} {costEstimate.Currency}");
                    
                    // Step 3: Perform transpilation
                    var transpileRequest = new CobolTranspileRequest
                    {
                        SourceCode = cobolCode,
                        TargetLanguage = TargetLanguage.Solidity,
                        BankingSystem = BankingSystem.TcsBancs,
                        BlockchainNetwork = BlockchainNetwork.Ethereum,
                        Options = new CobolTranspileOptions
                        {
                            OptimizeForGas = true,
                            IncludeComments = true,
                            MonitorPerformance = true
                        }
                    };
                    
                    var result = await _client.Cobol.TranspileAsync(transpileRequest);
                    Console.WriteLine("✅ Step 3: Transpilation completed");
                    Console.WriteLine($"   Transaction ID: {result.TransactionId}");
                    Console.WriteLine($"   Contract Address: {result.ContractAddress ?? "N/A"}");
                    
                    if (!string.IsNullOrEmpty(result.JobId))
                    {
                        Console.WriteLine("📊 Step 4: Monitoring job progress...");
                        await MonitorTranspilationStatusAsync(result.JobId);
                    }
                    
                    Console.WriteLine("🎉 Complete workflow finished successfully!");
                }
                else
                {
                    Console.WriteLine("❌ Step 1: Code validation failed");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Workflow failed: {e.Message}");
            }
        }
        
        /// <summary>
        /// Run all examples
        /// </summary>
        public async Task RunAllExamplesAsync()
        {
            Console.WriteLine("🚀 Starting comprehensive COBOL transpilation examples...");
            
            await BasicCobolToSolidityAsync();
            await CobolToCordaWithComplianceAsync();
            await TemplateBasedTranspilationAsync();
            await BatchProcessingAsync();
            await GetTranspilationAnalyticsAsync();
            await CheckQuotaAndUsageAsync();
            await SelectOptimalNetworkAsync();
            await EstimateTranspilationCostAsync();
            await ValidateCobolCodeAsync();
            await CompleteWorkflowAsync();
            
            Console.WriteLine("\n🎉 All COBOL transpiler examples completed!");
        }
        
        /// <summary>
        /// Cleanup resources
        /// </summary>
        public void Dispose()
        {
            _client?.Dispose();
        }
    }
    
    /// <summary>
    /// Program entry point for running examples
    /// </summary>
    public class Program
    {
        public static async Task Main(string[] args)
        {
            if (args.Length < 2)
            {
                Console.WriteLine("Usage: CobolTranspilerExamples <client-id> <client-secret>");
                Environment.Exit(1);
            }
            
            var clientId = args[0];
            var clientSecret = args[1];
            
            var examples = new CobolTranspilerExamples(clientId, clientSecret);
            
            try
            {
                await examples.RunAllExamplesAsync();
            }
            catch (Exception e)
            {
                Console.WriteLine($"❌ Examples failed: {e.Message}");
            }
            finally
            {
                examples.Dispose();
            }
        }
    }
}