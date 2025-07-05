/**
 * COBOL Transpiler Examples for JavaScript/Node.js SDK
 * LegacyBaaS Platform Integration
 */

const { LegacyBaaSClient } = require('../src/client');

// Example 1: Basic COBOL to Solidity Transpilation
async function basicCobolTranspilation() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production', // or 'sandbox'
    baseUrl: 'https://api.legacybaas.com'
  });

  try {
    // COBOL code for a simple payment processor
    const cobolCode = `
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
    `;

    const transpilationResult = await client.cobol.transpile({
      sourceCode: cobolCode,
      targetLanguage: 'solidity',
      bankingSystem: 'fis-systematics',
      blockchainNetwork: 'ethereum',
      options: {
        optimizeForGas: true,
        includeComments: true,
        generateInterface: true
      }
    });

    console.log('Transpilation successful!');
    console.log('Contract Address:', transpilationResult.contractAddress);
    console.log('Transaction Hash:', transpilationResult.transactionHash);
    console.log('Generated Solidity Code:', transpilationResult.sourceCode);

    return transpilationResult;
  } catch (error) {
    console.error('Transpilation failed:', error.message);
    throw error;
  }
}

// Example 2: Batch COBOL File Processing
async function batchCobolProcessing() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production'
  });

  const cobolFiles = [
    {
      name: 'payment-processor.cbl',
      content: '/* COBOL code for payment processing */',
      targetLanguage: 'solidity',
      blockchainNetwork: 'ethereum'
    },
    {
      name: 'account-manager.cbl',
      content: '/* COBOL code for account management */',
      targetLanguage: 'corda',
      blockchainNetwork: 'corda'
    },
    {
      name: 'balance-checker.cbl',
      content: '/* COBOL code for balance verification */',
      targetLanguage: 'algorand',
      blockchainNetwork: 'algorand'
    }
  ];

  try {
    const batchResults = await client.cobol.batchTranspile({
      files: cobolFiles,
      bankingSystem: 'fiserv-dna',
      options: {
        parallel: true,
        maxConcurrency: 3,
        timeout: 300000 // 5 minutes
      }
    });

    console.log(`Batch processing completed: ${batchResults.length} files processed`);
    
    batchResults.forEach((result, index) => {
      if (result.success) {
        console.log(`✓ ${cobolFiles[index].name}: ${result.contractAddress}`);
      } else {
        console.log(`✗ ${cobolFiles[index].name}: ${result.error}`);
      }
    });

    return batchResults;
  } catch (error) {
    console.error('Batch processing failed:', error.message);
    throw error;
  }
}

// Example 3: Template-Based Transpilation
async function templateBasedTranspilation() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production'
  });

  try {
    // Get available templates
    const templates = await client.cobol.getTemplates({
      bankingSystem: 'tcs-bancs',
      category: 'payments'
    });

    console.log('Available templates:', templates.map(t => t.name));

    // Use a specific template
    const templateResult = await client.cobol.transpileWithTemplate({
      templateId: templates.find(t => t.name === 'TCS-Payment-Template').id,
      variables: {
        contractName: 'TCSPaymentProcessor',
        maxAmount: 100000,
        currency: 'USD',
        complianceLevel: 'HIGH'
      },
      targetLanguage: 'solidity',
      blockchainNetwork: 'ethereum'
    });

    console.log('Template-based transpilation successful!');
    console.log('Generated contract:', templateResult.contractAddress);

    return templateResult;
  } catch (error) {
    console.error('Template transpilation failed:', error.message);
    throw error;
  }
}

// Example 4: Real-time Transpilation Status Monitoring
async function monitorTranspilationStatus() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production'
  });

  try {
    // Start a long-running transpilation
    const transpilationJob = await client.cobol.transpile({
      sourceCode: '/* Large COBOL program */',
      targetLanguage: 'corda',
      bankingSystem: 'temenos-transact',
      blockchainNetwork: 'corda',
      options: {
        async: true // Enable async processing for large files
      }
    });

    console.log('Transpilation job started:', transpilationJob.jobId);

    // Monitor status with real-time updates
    const statusMonitor = client.cobol.monitorStatus(transpilationJob.jobId);

    statusMonitor.on('progress', (status) => {
      console.log(`Progress: ${status.progress}% - ${status.stage}`);
    });

    statusMonitor.on('completed', (result) => {
      console.log('Transpilation completed successfully!');
      console.log('Contract Address:', result.contractAddress);
    });

    statusMonitor.on('error', (error) => {
      console.error('Transpilation failed:', error.message);
    });

    // Wait for completion
    const finalResult = await statusMonitor.waitForCompletion();
    return finalResult;
  } catch (error) {
    console.error('Status monitoring failed:', error.message);
    throw error;
  }
}

// Example 5: Advanced Configuration with Custom Banking System
async function advancedBankingSystemConfiguration() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production'
  });

  try {
    // Create custom banking system configuration
    const customConfig = await client.cobol.createBankingConfig({
      name: 'CustomBank-CoreBanking',
      type: 'custom',
      settings: {
        dataTypes: {
          'ACCOUNT-NUMBER': { type: 'string', length: 20 },
          'AMOUNT': { type: 'decimal', precision: 10, scale: 2 },
          'CURRENCY': { type: 'enum', values: ['USD', 'EUR', 'GBP'] }
        },
        validationRules: [
          { field: 'AMOUNT', rule: 'min', value: 0.01 },
          { field: 'AMOUNT', rule: 'max', value: 999999.99 },
          { field: 'ACCOUNT-NUMBER', rule: 'pattern', value: '^[A-Z0-9]{20}$' }
        ],
        complianceRequirements: ['AML', 'KYC', 'FATF'],
        auditLevel: 'COMPREHENSIVE'
      }
    });

    console.log('Custom banking configuration created:', customConfig.id);

    // Use custom configuration for transpilation
    const result = await client.cobol.transpile({
      sourceCode: '/* COBOL code with custom data types */',
      targetLanguage: 'solidity',
      bankingSystem: customConfig.id,
      blockchainNetwork: 'ethereum',
      options: {
        strictTypeChecking: true,
        generateValidators: true,
        includeAuditTrail: true
      }
    });

    console.log('Advanced transpilation with custom config successful!');
    return result;
  } catch (error) {
    console.error('Advanced configuration failed:', error.message);
    throw error;
  }
}

// Example 6: Integration with Existing Blockchain Infrastructure
async function blockchainIntegration() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production'
  });

  try {
    // Get blockchain network status
    const networkStatus = await client.blockchain.getNetworkStatus(['ethereum', 'corda', 'algorand']);
    console.log('Network Status:', networkStatus);

    // Select optimal network based on current conditions
    const optimalNetwork = await client.cobol.selectOptimalNetwork({
      contractComplexity: 'HIGH',
      transactionVolume: 'MEDIUM',
      latencyRequirement: 'LOW',
      costSensitivity: 'MEDIUM'
    });

    console.log('Optimal network selected:', optimalNetwork.network);

    // Transpile and deploy to optimal network
    const result = await client.cobol.transpile({
      sourceCode: '/* COBOL payment processing code */',
      targetLanguage: optimalNetwork.language,
      bankingSystem: 'fis-systematics',
      blockchainNetwork: optimalNetwork.network,
      options: {
        autoOptimize: true,
        deployImmediately: true,
        monitorPerformance: true
      }
    });

    // Monitor contract performance
    const performanceMonitor = client.blockchain.monitorContract(result.contractAddress);
    
    performanceMonitor.on('performance', (metrics) => {
      console.log('Contract Performance:', {
        gasUsage: metrics.gasUsage,
        executionTime: metrics.executionTime,
        throughput: metrics.throughput
      });
    });

    return result;
  } catch (error) {
    console.error('Blockchain integration failed:', error.message);
    throw error;
  }
}

// Example 7: Compliance and Audit Integration
async function complianceIntegration() {
  const client = new LegacyBaaSClient({
    apiKey: process.env.LEGACYBAAS_API_KEY,
    environment: 'production'
  });

  try {
    // Enable comprehensive compliance monitoring
    const complianceConfig = await client.compliance.configureMonitoring({
      amlScreening: true,
      sanctionsScreening: true,
      fatfTravelRule: true,
      riskScoring: true,
      auditLevel: 'COMPREHENSIVE'
    });

    console.log('Compliance monitoring configured:', complianceConfig.id);

    // Transpile with compliance integration
    const result = await client.cobol.transpile({
      sourceCode: '/* COBOL code for international payments */',
      targetLanguage: 'solidity',
      bankingSystem: 'fiserv-dna',
      blockchainNetwork: 'ethereum',
      compliance: {
        configId: complianceConfig.id,
        riskThreshold: 'MEDIUM',
        requireApproval: true,
        generateReport: true
      }
    });

    // Get compliance report
    const complianceReport = await client.compliance.getTranspilationReport(result.transactionId);
    
    console.log('Compliance Report:', {
      riskScore: complianceReport.riskScore,
      screeningResults: complianceReport.screeningResults,
      auditTrail: complianceReport.auditTrail
    });

    return result;
  } catch (error) {
    console.error('Compliance integration failed:', error.message);
    throw error;
  }
}

// Example Usage and Testing
async function runExamples() {
  console.log('=== COBOL Transpiler JavaScript SDK Examples ===\n');

  try {
    console.log('1. Basic COBOL Transpilation...');
    await basicCobolTranspilation();
    console.log('✓ Completed\n');

    console.log('2. Batch Processing...');
    await batchCobolProcessing();
    console.log('✓ Completed\n');

    console.log('3. Template-Based Transpilation...');
    await templateBasedTranspilation();
    console.log('✓ Completed\n');

    console.log('4. Status Monitoring...');
    await monitorTranspilationStatus();
    console.log('✓ Completed\n');

    console.log('5. Advanced Banking Configuration...');
    await advancedBankingSystemConfiguration();
    console.log('✓ Completed\n');

    console.log('6. Blockchain Integration...');
    await blockchainIntegration();
    console.log('✓ Completed\n');

    console.log('7. Compliance Integration...');
    await complianceIntegration();
    console.log('✓ Completed\n');

    console.log('🎉 All examples completed successfully!');
  } catch (error) {
    console.error('❌ Example execution failed:', error.message);
  }
}

// Error Handling Example
function setupErrorHandling(client) {
  client.on('error', (error) => {
    console.error('SDK Error:', error);
    
    switch (error.code) {
      case 'RATE_LIMIT_EXCEEDED':
        console.log('Rate limit exceeded. Retrying after delay...');
        // Implement exponential backoff
        break;
      case 'AUTHENTICATION_FAILED':
        console.log('Authentication failed. Check API key.');
        break;
      case 'TRANSPILATION_FAILED':
        console.log('Transpilation failed:', error.details);
        break;
      default:
        console.log('Unexpected error:', error.message);
    }
  });
}

module.exports = {
  basicCobolTranspilation,
  batchCobolProcessing,
  templateBasedTranspilation,
  monitorTranspilationStatus,
  advancedBankingSystemConfiguration,
  blockchainIntegration,
  complianceIntegration,
  runExamples,
  setupErrorHandling
};

// Run examples if this file is executed directly
if (require.main === module) {
  runExamples().catch(console.error);
}