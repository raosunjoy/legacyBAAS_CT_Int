"""
COBOL Transpiler Examples for Python SDK
LegacyBaaS Platform Integration
"""

import os
import time
import asyncio
from typing import Dict, List, Any

from legacybaas import LegacyBaaSClient
from legacybaas.services.cobol_service import (
    CobolTranspileRequest,
    CobolTranspileOptions,
    ComplianceConfig,
    TargetLanguage,
    BankingSystem,
    BlockchainNetwork
)
from legacybaas.exceptions import LegacyBaaSException


def setup_client():
    """Setup LegacyBaaS client with credentials"""
    return LegacyBaaSClient(
        client_id=os.getenv("LEGACYBAAS_CLIENT_ID"),
        client_secret=os.getenv("LEGACYBAAS_CLIENT_SECRET"),
        environment="production",  # or "sandbox"
        enable_logging=True,
        log_level="INFO"
    )


def example_1_basic_cobol_transpilation():
    """
    Example 1: Basic COBOL to Solidity Transpilation
    """
    print("=== Example 1: Basic COBOL Transpilation ===")
    
    client = setup_client()
    
    # COBOL code for a simple payment processor
    cobol_code = """
    IDENTIFICATION DIVISION.
    PROGRAM-ID. PAYMENT-PROCESSOR.
    
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 PAYMENT-RECORD.
       05 PAYER-ACCOUNT     PIC X(20).
       05 PAYEE-ACCOUNT     PIC X(20).
       05 AMOUNT            PIC 9(10)V99.
       05 CURRENCY          PIC X(3).
       
    PROCEDURE DIVISION.
    PROCESS-PAYMENT.
        MOVE "ACC123456789012345" TO PAYER-ACCOUNT
        MOVE "ACC987654321098765" TO PAYEE-ACCOUNT
        MOVE 1000.50 TO AMOUNT
        MOVE "USD" TO CURRENCY
        PERFORM VALIDATE-PAYMENT
        PERFORM EXECUTE-TRANSFER
        STOP RUN.
        
    VALIDATE-PAYMENT.
        IF AMOUNT > 0
           DISPLAY "Payment validation successful"
        ELSE
           DISPLAY "Invalid payment amount"
        END-IF.
        
    EXECUTE-TRANSFER.
        DISPLAY "Processing payment from " PAYER-ACCOUNT
        DISPLAY "To " PAYEE-ACCOUNT
        DISPLAY "Amount: " AMOUNT " " CURRENCY.
    """
    
    try:
        # Create transpilation request
        request = CobolTranspileRequest(
            source_code=cobol_code,
            target_language=TargetLanguage.SOLIDITY,
            banking_system=BankingSystem.FIS_SYSTEMATICS,
            blockchain_network=BlockchainNetwork.ETHEREUM,
            options=CobolTranspileOptions(
                optimize_for_gas=True,
                include_comments=True,
                generate_interface=True
            )
        )
        
        # Transpile COBOL code
        result = client.cobol.transpile(request)
        
        if result.success:
            print(f"✓ Transpilation successful!")
            print(f"  Contract Address: {result.contract_address}")
            print(f"  Transaction Hash: {result.transaction_hash}")
            print(f"  Gas Estimate: {result.gas_estimate}")
            print(f"  Generated Solidity Code Preview:")
            print(f"  {result.source_code[:200]}...")
        else:
            print(f"✗ Transpilation failed: {result.error}")
            
        return result
        
    except LegacyBaaSException as e:
        print(f"✗ Error: {e}")
        return None


def example_2_batch_cobol_processing():
    """
    Example 2: Batch COBOL File Processing
    """
    print("\n=== Example 2: Batch COBOL Processing ===")
    
    client = setup_client()
    
    # Multiple COBOL files to process
    cobol_files = [
        {
            "name": "payment-processor.cbl",
            "content": "/* COBOL code for payment processing */",
            "targetLanguage": "solidity",
            "blockchainNetwork": "ethereum"
        },
        {
            "name": "account-manager.cbl",
            "content": "/* COBOL code for account management */",
            "targetLanguage": "corda",
            "blockchainNetwork": "corda"
        },
        {
            "name": "balance-checker.cbl",
            "content": "/* COBOL code for balance verification */",
            "targetLanguage": "algorand",
            "blockchainNetwork": "algorand"
        }
    ]
    
    try:
        # Process files in batch
        results = client.cobol.batch_transpile(
            files=cobol_files,
            banking_system="fiserv-dna",
            options={
                "parallel": True,
                "maxConcurrency": 3,
                "timeout": 300000  # 5 minutes
            }
        )
        
        print(f"Batch processing completed: {len(results)} files processed")
        
        for i, result in enumerate(results):
            file_name = cobol_files[i]["name"]
            if result.success:
                print(f"  ✓ {file_name}: {result.contract_address}")
            else:
                print(f"  ✗ {file_name}: {result.error}")
        
        return results
        
    except LegacyBaaSException as e:
        print(f"✗ Batch processing error: {e}")
        return None


def example_3_template_based_transpilation():
    """
    Example 3: Template-Based Transpilation
    """
    print("\n=== Example 3: Template-Based Transpilation ===")
    
    client = setup_client()
    
    try:
        # Get available templates
        templates = client.cobol.get_templates(
            banking_system="tcs-bancs",
            category="payments"
        )
        
        print(f"Available templates: {[t.name for t in templates]}")
        
        if templates:
            # Use the first available template
            template = templates[0]
            print(f"Using template: {template.name}")
            
            # Transpile with template
            result = client.cobol.transpile_with_template(
                template_id=template.id,
                variables={
                    "contractName": "TCSPaymentProcessor",
                    "maxAmount": 100000,
                    "currency": "USD",
                    "complianceLevel": "HIGH"
                },
                target_language="solidity",
                blockchain_network="ethereum"
            )
            
            if result.success:
                print(f"✓ Template-based transpilation successful!")
                print(f"  Contract Address: {result.contract_address}")
            else:
                print(f"✗ Template transpilation failed: {result.error}")
            
            return result
        else:
            print("No templates available for the specified criteria")
            return None
            
    except LegacyBaaSException as e:
        print(f"✗ Template transpilation error: {e}")
        return None


def example_4_status_monitoring():
    """
    Example 4: Real-time Transpilation Status Monitoring
    """
    print("\n=== Example 4: Status Monitoring ===")
    
    client = setup_client()
    
    try:
        # Start a long-running transpilation
        request = CobolTranspileRequest(
            source_code="/* Large COBOL program */",
            target_language=TargetLanguage.CORDA,
            banking_system=BankingSystem.TEMENOS_TRANSACT,
            blockchain_network=BlockchainNetwork.CORDA,
            options=CobolTranspileOptions(async_processing=True)
        )
        
        result = client.cobol.transpile(request)
        
        if result.job_id:
            print(f"Transpilation job started: {result.job_id}")
            print("Monitoring progress...")
            
            # Monitor status with callback
            def status_callback(status):
                progress = status.get("progress", 0)
                stage = status.get("stage", "Unknown")
                print(f"  Progress: {progress}% - {stage}")
            
            # Monitor until completion
            for status in client.cobol.monitor_status(result.job_id, callback=status_callback):
                if status.get("progress", 0) >= 100:
                    print("✓ Transpilation completed successfully!")
                    if "contractAddress" in status:
                        print(f"  Contract Address: {status['contractAddress']}")
                    break
                elif "error" in status:
                    print(f"✗ Transpilation failed: {status['error']}")
                    break
        else:
            print("Job ID not available for monitoring")
            
    except LegacyBaaSException as e:
        print(f"✗ Status monitoring error: {e}")


def example_5_advanced_banking_configuration():
    """
    Example 5: Advanced Configuration with Custom Banking System
    """
    print("\n=== Example 5: Advanced Banking Configuration ===")
    
    client = setup_client()
    
    try:
        # Create custom banking system configuration
        custom_config_data = {
            "name": "CustomBank-CoreBanking",
            "type": "custom",
            "settings": {
                "dataTypes": {
                    "ACCOUNT-NUMBER": {"type": "string", "length": 20},
                    "AMOUNT": {"type": "decimal", "precision": 10, "scale": 2},
                    "CURRENCY": {"type": "enum", "values": ["USD", "EUR", "GBP"]}
                },
                "validationRules": [
                    {"field": "AMOUNT", "rule": "min", "value": 0.01},
                    {"field": "AMOUNT", "rule": "max", "value": 999999.99},
                    {"field": "ACCOUNT-NUMBER", "rule": "pattern", "value": "^[A-Z0-9]{20}$"}
                ],
                "complianceRequirements": ["AML", "KYC", "FATF"],
                "auditLevel": "COMPREHENSIVE"
            }
        }
        
        custom_config = client.cobol.create_banking_config(custom_config_data)
        print(f"Custom banking configuration created: {custom_config.id}")
        
        # Use custom configuration for transpilation
        request = CobolTranspileRequest(
            source_code="/* COBOL code with custom data types */",
            target_language=TargetLanguage.SOLIDITY,
            banking_system=custom_config.id,
            blockchain_network=BlockchainNetwork.ETHEREUM,
            options=CobolTranspileOptions(
                strict_type_checking=True,
                generate_validators=True,
                include_audit_trail=True
            )
        )
        
        result = client.cobol.transpile(request)
        
        if result.success:
            print("✓ Advanced transpilation with custom config successful!")
            print(f"  Contract Address: {result.contract_address}")
        else:
            print(f"✗ Advanced transpilation failed: {result.error}")
            
        return result
        
    except LegacyBaaSException as e:
        print(f"✗ Advanced configuration error: {e}")
        return None


def example_6_blockchain_integration():
    """
    Example 6: Integration with Existing Blockchain Infrastructure
    """
    print("\n=== Example 6: Blockchain Integration ===")
    
    client = setup_client()
    
    try:
        # Select optimal network based on criteria
        optimal_network = client.cobol.select_optimal_network({
            "contractComplexity": "HIGH",
            "transactionVolume": "MEDIUM",
            "latencyRequirement": "LOW",
            "costSensitivity": "MEDIUM"
        })
        
        print(f"Optimal network selected: {optimal_network.network}")
        print(f"Reasons: {', '.join(optimal_network.reasons)}")
        print(f"Estimated cost: ${optimal_network.estimated_cost}")
        print(f"Estimated time: {optimal_network.estimated_time}s")
        
        # Transpile and deploy to optimal network
        request = CobolTranspileRequest(
            source_code="/* COBOL payment processing code */",
            target_language=optimal_network.language,
            banking_system=BankingSystem.FIS_SYSTEMATICS,
            blockchain_network=optimal_network.network,
            options=CobolTranspileOptions(
                auto_optimize=True,
                deploy_immediately=True,
                monitor_performance=True
            )
        )
        
        result = client.cobol.transpile(request)
        
        if result.success:
            print("✓ Blockchain integration successful!")
            print(f"  Contract Address: {result.contract_address}")
            if result.performance:
                print(f"  Performance Metrics:")
                print(f"    Gas Usage: {result.performance.get('gasUsage')}")
                print(f"    Execution Time: {result.performance.get('executionTime')}ms")
        else:
            print(f"✗ Blockchain integration failed: {result.error}")
            
        return result
        
    except LegacyBaaSException as e:
        print(f"✗ Blockchain integration error: {e}")
        return None


def example_7_compliance_integration():
    """
    Example 7: Compliance and Audit Integration
    """
    print("\n=== Example 7: Compliance Integration ===")
    
    client = setup_client()
    
    try:
        # Transpile with comprehensive compliance monitoring
        request = CobolTranspileRequest(
            source_code="/* COBOL code for international payments */",
            target_language=TargetLanguage.SOLIDITY,
            banking_system=BankingSystem.FISERV_DNA,
            blockchain_network=BlockchainNetwork.ETHEREUM,
            compliance=ComplianceConfig(
                risk_threshold="MEDIUM",
                require_approval=True,
                generate_report=True
            )
        )
        
        result = client.cobol.transpile(request)
        
        if result.success:
            print("✓ Compliance integration successful!")
            print(f"  Contract Address: {result.contract_address}")
            
            if result.compliance:
                print(f"  Compliance Report:")
                print(f"    Risk Score: {result.compliance.get('riskScore')}")
                print(f"    Screening Results: {result.compliance.get('screeningResults')}")
                
        else:
            print(f"✗ Compliance integration failed: {result.error}")
            
        return result
        
    except LegacyBaaSException as e:
        print(f"✗ Compliance integration error: {e}")
        return None


def example_8_analytics_and_reporting():
    """
    Example 8: Analytics and Reporting
    """
    print("\n=== Example 8: Analytics and Reporting ===")
    
    client = setup_client()
    
    try:
        # Get transpilation analytics
        analytics = client.cobol.get_analytics(period="7d")
        
        print("Transpilation Analytics (Last 7 days):")
        print(f"  Total Transpilations: {analytics['totalTranspilations']}")
        print(f"  Success Rate: {analytics['successRate']:.1%}")
        print(f"  Average Time: {analytics['averageTime']:.2f}s")
        
        print("\n  By Banking System:")
        for system, count in analytics['byBankingSystem'].items():
            print(f"    {system}: {count}")
        
        print("\n  By Network:")
        for network, count in analytics['byNetwork'].items():
            print(f"    {network}: {count}")
        
        # Get quota information
        quota = client.cobol.get_quota()
        print(f"\nQuota Usage:")
        print(f"  Current: {quota['current']}")
        print(f"  Limit: {quota['limit']}")
        print(f"  Remaining: {quota['remaining']}")
        print(f"  Reset Date: {quota['resetDate']}")
        
        # Get transpilation history
        history = client.cobol.get_history(limit=10)
        print(f"\nRecent Transpilations: {history['total']} total")
        for item in history['items'][:5]:  # Show first 5
            status_icon = "✓" if item['status'] == 'completed' else "✗"
            print(f"  {status_icon} {item['id']}: {item['bankingSystem']} -> {item['targetLanguage']}")
        
        return analytics
        
    except LegacyBaaSException as e:
        print(f"✗ Analytics error: {e}")
        return None


def example_9_cost_estimation():
    """
    Example 9: Cost Estimation
    """
    print("\n=== Example 9: Cost Estimation ===")
    
    client = setup_client()
    
    try:
        # Estimate cost before transpilation
        cost_estimate = client.cobol.estimate_cost({
            "sourceCodeLength": 5000,  # 5KB of COBOL code
            "targetLanguage": "solidity",
            "blockchainNetwork": "ethereum",
            "complexity": "HIGH"
        })
        
        print("Cost Estimation:")
        print(f"  Estimated Cost: {cost_estimate['estimatedCost']} {cost_estimate['currency']}")
        print(f"  Breakdown:")
        breakdown = cost_estimate['breakdown']
        print(f"    Base Cost: {breakdown['baseCost']}")
        print(f"    Complexity Multiplier: {breakdown['complexityMultiplier']}")
        print(f"    Network Fee: {breakdown['networkFee']}")
        print(f"    Deployment Cost: {breakdown['deploymentCost']}")
        
        return cost_estimate
        
    except LegacyBaaSException as e:
        print(f"✗ Cost estimation error: {e}")
        return None


def example_10_export_results():
    """
    Example 10: Export Transpilation Results
    """
    print("\n=== Example 10: Export Results ===")
    
    client = setup_client()
    
    try:
        # Export results as Excel
        export_result = client.cobol.export_results(
            format="excel",
            filters={
                "startDate": "2025-07-01",
                "endDate": "2025-07-05",
                "bankingSystem": "fis-systematics"
            }
        )
        
        print("Export Results:")
        print(f"  Download URL: {export_result['downloadUrl']}")
        print(f"  Expires At: {export_result['expiresAt']}")
        print("  Note: Download the file from the URL before expiration")
        
        return export_result
        
    except LegacyBaaSException as e:
        print(f"✗ Export error: {e}")
        return None


def run_all_examples():
    """Run all COBOL transpiler examples"""
    print("🚀 Running COBOL Transpiler Python SDK Examples")
    print("=" * 60)
    
    try:
        # Run examples
        example_1_basic_cobol_transpilation()
        example_2_batch_cobol_processing()
        example_3_template_based_transpilation()
        example_4_status_monitoring()
        example_5_advanced_banking_configuration()
        example_6_blockchain_integration()
        example_7_compliance_integration()
        example_8_analytics_and_reporting()
        example_9_cost_estimation()
        example_10_export_results()
        
        print("\n" + "=" * 60)
        print("🎉 All examples completed successfully!")
        
    except Exception as e:
        print(f"\n❌ Example execution failed: {e}")


async def async_example():
    """
    Async example using the async COBOL service
    """
    print("\n=== Async Example ===")
    
    # Note: This would require implementing AsyncLegacyBaaSClient
    # which is not shown here but would follow similar patterns
    pass


def error_handling_example():
    """
    Example of comprehensive error handling
    """
    print("\n=== Error Handling Example ===")
    
    client = setup_client()
    
    try:
        # Intentionally cause an error with invalid COBOL
        request = CobolTranspileRequest(
            source_code="INVALID COBOL SYNTAX",
            target_language=TargetLanguage.SOLIDITY,
            banking_system=BankingSystem.FIS_SYSTEMATICS,
            blockchain_network=BlockchainNetwork.ETHEREUM
        )
        
        result = client.cobol.transpile(request)
        
    except LegacyBaaSException as e:
        print(f"Caught LegacyBaaS Exception: {e}")
        print(f"Error Type: {type(e).__name__}")
        
        # Handle specific error types
        if "rate limit" in str(e).lower():
            print("Rate limit exceeded. Implementing backoff...")
            time.sleep(5)  # Simple backoff
        elif "authentication" in str(e).lower():
            print("Authentication failed. Check credentials.")
        elif "validation" in str(e).lower():
            print("Validation error. Check input data.")
        else:
            print("Unexpected error occurred.")


if __name__ == "__main__":
    # Check if credentials are set
    if not os.getenv("LEGACYBAAS_CLIENT_ID") or not os.getenv("LEGACYBAAS_CLIENT_SECRET"):
        print("❌ Please set LEGACYBAAS_CLIENT_ID and LEGACYBAAS_CLIENT_SECRET environment variables")
        exit(1)
    
    # Run examples
    run_all_examples()
    
    # Demonstrate error handling
    error_handling_example()