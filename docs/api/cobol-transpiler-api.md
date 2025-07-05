# COBOL Transpiler API Documentation

## Overview

The COBOL Transpiler API enables seamless modernization of legacy COBOL applications into blockchain-compatible smart contracts. This comprehensive API integrates with the existing LegacyBAAS platform to provide enterprise-grade COBOL transpilation capabilities across multiple banking systems and blockchain networks.

## Table of Contents

1. [Authentication](#authentication)
2. [Core Transpiler Endpoints](#core-transpiler-endpoints)
3. [Banking System Integration](#banking-system-integration)
4. [Analytics & Monitoring](#analytics--monitoring)
5. [Quota Management](#quota-management)
6. [Error Handling](#error-handling)
7. [SDK Examples](#sdk-examples)
8. [Integration Guides](#integration-guides)

## Authentication

The COBOL Transpiler API uses the existing LegacyBAAS OAuth2 authentication system with role-based access control (RBAC).

### Authentication Methods

1. **OAuth2 Bearer Token** (Recommended)
2. **API Key** (For programmatic access)

### Required Headers

```http
Authorization: Bearer <your_access_token>
Content-Type: application/json
X-API-Version: 2.1
```

### User Roles and Permissions

| Role | Permissions | Description |
|------|------------|-------------|
| `super_admin` | All permissions | Full system access |
| `bank_admin` | Banking system management | Full access to assigned banking system |
| `si_developer` | Limited client access | System integrator development access |
| `bank_user` | Read-only operations | Basic banking operations |
| `reseller_partner` | Demo access | Limited demo capabilities |

## Core Transpiler Endpoints

### 1. Parse COBOL Program

Parse and analyze COBOL source code to generate an Abstract Syntax Tree (AST).

```http
POST /api/v2/transpiler/parse
```

**Request Body:**
```json
{
  "cobolCode": "IDENTIFICATION DIVISION.\nPROGRAM-ID. PAYMENT-PROCESSOR.\n...",
  "bankingSystem": "fis",
  "parseOptions": {
    "validateSyntax": true,
    "includeComments": false,
    "optimizationLevel": "standard"
  }
}
```

**Response:**
```json
{
  "success": true,
  "requestId": "req_1234567890",
  "parseResult": {
    "ast": {
      "programId": "PAYMENT-PROCESSOR",
      "divisions": ["identification", "data", "procedure"],
      "dataItems": 15,
      "procedures": 8
    },
    "complexity": "medium",
    "variables": [
      {
        "name": "ACCOUNT-NUMBER",
        "type": "PIC 9(12)",
        "level": "01",
        "usage": "display"
      }
    ],
    "procedures": [
      {
        "name": "VALIDATE-ACCOUNT",
        "type": "perform",
        "lineNumber": 25
      }
    ],
    "statistics": {
      "totalLines": 150,
      "codeLines": 120,
      "commentLines": 30,
      "cyclomaticComplexity": 8
    }
  },
  "warnings": [],
  "timestamp": "2025-07-05T10:30:00Z"
}
```

### 2. Generate Smart Contract

Convert parsed COBOL AST into blockchain-compatible smart contract code.

```http
POST /api/v2/transpiler/generate
```

**Request Body:**
```json
{
  "ast": { /* AST from parse endpoint */ },
  "targetBlockchain": "ethereum",
  "bankingSystem": "fis",
  "contractOptions": {
    "optimizeForGas": true,
    "includeDocumentation": true,
    "securityLevel": "high",
    "templateVersion": "2.1.0"
  }
}
```

**Response:**
```json
{
  "success": true,
  "requestId": "req_1234567891",
  "contractResult": {
    "language": "solidity",
    "version": "0.8.19",
    "code": "// SPDX-License-Identifier: MIT\npragma solidity ^0.8.19;\n\ncontract PaymentProcessor {\n...",
    "abi": [
      {
        "inputs": [],
        "name": "validateAccount",
        "outputs": [{"internalType": "bool", "name": "", "type": "bool"}],
        "stateMutability": "view",
        "type": "function"
      }
    ],
    "bytecode": "0x608060405234801561001057600080fd5b50...",
    "gasEstimate": 1250000,
    "functions": [
      {
        "name": "validateAccount",
        "visibility": "public",
        "parameters": ["uint256 accountNumber"],
        "returns": ["bool"]
      }
    ],
    "events": [
      {
        "name": "AccountValidated",
        "parameters": ["uint256 indexed accountNumber", "bool isValid"]
      }
    ],
    "deploymentCost": "0.045 ETH"
  },
  "mappings": {
    "cobolToSolidity": {
      "ACCOUNT-NUMBER": "accountNumber",
      "VALIDATE-ACCOUNT": "validateAccount"
    }
  },
  "timestamp": "2025-07-05T10:35:00Z"
}
```

### 3. Complete Transpilation Workflow

End-to-end transpilation from COBOL to deployed smart contract.

```http
POST /api/v2/transpiler/transpile
```

**Request Body:**
```json
{
  "cobolCode": "IDENTIFICATION DIVISION.\n...",
  "bankingSystem": "fis",
  "targetBlockchain": "ethereum",
  "deploymentConfig": {
    "network": "mainnet",
    "gasLimit": 5000000,
    "gasPrice": "20",
    "confirmations": 3,
    "constructorArgs": []
  },
  "options": {
    "validateOnly": false,
    "optimizeForGas": true,
    "generateTests": true
  }
}
```

**Response:**
```json
{
  "success": true,
  "requestId": "req_1234567892",
  "workflowId": "wf_abc123def456",
  "status": "completed",
  "parseResult": { /* Parse results */ },
  "contractResult": { /* Contract generation results */ },
  "deploymentResult": {
    "contractAddress": "0x742d35Cc6634C0532925a3b8D3C48e7aB8c8F4A1",
    "transactionHash": "0x1234567890abcdef...",
    "blockNumber": 18750000,
    "gasUsed": 1180000,
    "deploymentCost": "0.0236 ETH",
    "networkConfirmations": 3,
    "verificationStatus": "verified"
  },
  "quotaUsage": {
    "currentUsage": 251,
    "monthlyQuota": 1000,
    "overageUsage": 0,
    "costIncurred": 0
  },
  "processingTime": 25000,
  "timestamp": "2025-07-05T10:40:00Z"
}
```

### 4. Validate COBOL Syntax

Validate COBOL syntax without full transpilation.

```http
POST /api/v2/transpiler/validate
```

**Request Body:**
```json
{
  "cobolCode": "IDENTIFICATION DIVISION.\n...",
  "bankingSystem": "fis",
  "validationLevel": "strict"
}
```

**Response:**
```json
{
  "success": true,
  "valid": true,
  "errors": [],
  "warnings": [
    {
      "type": "deprecated_syntax",
      "line": 45,
      "message": "COMP-3 usage is deprecated, consider using BINARY",
      "severity": "warning"
    }
  ],
  "suggestions": [
    {
      "type": "optimization",
      "description": "Consider using indexed tables for better performance",
      "affectedLines": [52, 53, 54]
    }
  ]
}
```

### 5. Get Transpilation Status

Check the status of an ongoing transpilation process.

```http
GET /api/v2/transpiler/status/{workflowId}
```

**Response:**
```json
{
  "workflowId": "wf_abc123def456",
  "status": "processing",
  "currentStep": "contract_generation",
  "progress": 65,
  "steps": [
    {
      "name": "cobol_parsing",
      "status": "completed",
      "startTime": "2025-07-05T10:30:00Z",
      "endTime": "2025-07-05T10:30:15Z",
      "duration": 15000
    },
    {
      "name": "ast_optimization",
      "status": "completed",
      "startTime": "2025-07-05T10:30:15Z",
      "endTime": "2025-07-05T10:30:18Z",
      "duration": 3000
    },
    {
      "name": "contract_generation",
      "status": "processing",
      "startTime": "2025-07-05T10:30:18Z",
      "progress": 65
    },
    {
      "name": "blockchain_deployment",
      "status": "pending"
    }
  ],
  "estimatedCompletion": "2025-07-05T10:32:00Z"
}
```

## Banking System Integration

### 1. Get Available Banking Systems

```http
GET /api/v2/banking-systems
```

**Response:**
```json
{
  "systems": [
    {
      "id": "fis",
      "name": "FIS Systematics",
      "version": "2.1.0",
      "supported_features": ["payments", "accounts", "loans"],
      "data_types": ["COMP-3", "PIC 9(n)", "PIC X(n)"],
      "templates": ["payment_processing", "account_management", "loan_origination"]
    },
    {
      "id": "fiserv",
      "name": "Fiserv DNA",
      "version": "2.0.5",
      "supported_features": ["core_banking", "cards", "digital"],
      "data_types": ["BINARY", "PACKED-DECIMAL", "DISPLAY"],
      "templates": ["card_processing", "digital_banking", "core_operations"]
    }
  ]
}
```

### 2. Get Banking System Templates

```http
GET /api/v2/banking-systems/{systemId}/templates
```

**Response:**
```json
{
  "bankingSystem": "fis",
  "templates": [
    {
      "id": "payment_processing",
      "name": "Payment Processing Template",
      "description": "Standard payment processing logic for FIS",
      "version": "2.1.0",
      "targetBlockchains": ["ethereum", "polygon", "arbitrum"],
      "estimatedGas": 1200000,
      "complexity": "medium",
      "features": ["validation", "routing", "settlement"],
      "sampleCode": "IDENTIFICATION DIVISION.\nPROGRAM-ID. FIS-PAYMENT.\n..."
    }
  ]
}
```

## Analytics & Monitoring

### 1. Get Usage Analytics

```http
GET /api/v2/analytics/usage?period=30d&granularity=daily
```

**Response:**
```json
{
  "summary": {
    "totalTranspilations": 1247,
    "successRate": 98.7,
    "averageProcessingTime": 23000,
    "totalCustomers": 45,
    "revenueGenerated": 15750.00
  },
  "trends": [
    {
      "date": "2025-07-01",
      "transpilations": 42,
      "averageTime": 21000,
      "successRate": 100.0,
      "revenue": 525.00
    }
  ],
  "topBankingSystems": [
    {"system": "fis", "usage": 45.2, "transpilations": 563},
    {"system": "fiserv", "usage": 28.7, "transpilations": 358}
  ],
  "topBlockchains": [
    {"blockchain": "ethereum", "usage": 52.1, "deployments": 650},
    {"blockchain": "polygon", "usage": 24.3, "deployments": 303}
  ]
}
```

### 2. Get Performance Metrics

```http
GET /api/v2/analytics/performance?metric=response_time&period=7d
```

**Response:**
```json
{
  "metric": "response_time",
  "period": "7d",
  "statistics": {
    "average": 23500,
    "median": 21000,
    "p95": 45000,
    "p99": 68000,
    "min": 8500,
    "max": 125000
  },
  "timeSeries": [
    {
      "timestamp": "2025-07-05T00:00:00Z",
      "value": 22500,
      "requests": 156
    }
  ],
  "breakdown": {
    "parsing": 15.2,
    "generation": 65.8,
    "deployment": 19.0
  }
}
```

## Quota Management

### 1. Get Quota Status

```http
GET /api/v2/quota/status
```

**Response:**
```json
{
  "customerId": "customer_123",
  "subscription": {
    "tier": "enterprise",
    "monthlyQuota": 1000,
    "currentUsage": 247,
    "overageUsage": 0,
    "quotaRemaining": 753,
    "resetDate": "2025-08-01T00:00:00Z"
  },
  "usage": {
    "thisMonth": 247,
    "lastMonth": 892,
    "projectedUsage": 741,
    "averageDailyUsage": 8.2
  },
  "alerts": {
    "enabled": true,
    "thresholds": [80, 90, 95, 100],
    "nextAlert": 80
  },
  "billing": {
    "basePrice": 2999.00,
    "overageRate": 4.00,
    "currentCost": 2999.00,
    "projectedCost": 2999.00
  }
}
```

### 2. Get Usage History

```http
GET /api/v2/quota/history?period=3m&granularity=weekly
```

**Response:**
```json
{
  "period": "3m",
  "granularity": "weekly",
  "history": [
    {
      "week": "2025-W27",
      "usage": 67,
      "quota": 1000,
      "overage": 0,
      "cost": 0,
      "averageComplexity": "medium"
    }
  ],
  "trends": {
    "usageGrowth": 12.5,
    "complexityTrend": "increasing",
    "costEfficiency": 15.2
  }
}
```

## Error Handling

### Standard Error Response Format

```json
{
  "success": false,
  "error": {
    "code": "COBOL_PARSE_ERROR",
    "message": "Invalid COBOL syntax on line 45",
    "details": {
      "line": 45,
      "column": 12,
      "expected": "PROCEDURE DIVISION",
      "found": "INVALID TOKEN"
    },
    "requestId": "req_1234567890",
    "timestamp": "2025-07-05T10:30:00Z",
    "documentation": "https://docs.legacybaas.com/errors/COBOL_PARSE_ERROR"
  }
}
```

### Common Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `INVALID_COBOL_SYNTAX` | 400 | COBOL syntax validation failed |
| `UNSUPPORTED_BANKING_SYSTEM` | 400 | Banking system not supported |
| `QUOTA_EXCEEDED` | 429 | Monthly quota limit reached |
| `INSUFFICIENT_PERMISSIONS` | 403 | User lacks required permissions |
| `TRANSPILATION_TIMEOUT` | 408 | Processing exceeded time limit |
| `BLOCKCHAIN_DEPLOYMENT_FAILED` | 500 | Smart contract deployment failed |
| `INTERNAL_PROCESSING_ERROR` | 500 | Unexpected system error |

## SDK Examples

### JavaScript/TypeScript SDK

#### Installation
```bash
npm install @legacybaas/cobol-transpiler-sdk
```

#### Basic Usage
```typescript
import { CobolTranspilerClient } from '@legacybaas/cobol-transpiler-sdk';

const client = new CobolTranspilerClient({
  apiKey: 'your-api-key',
  baseURL: 'https://api.legacybaas.com/v2',
  timeout: 30000
});

// Parse COBOL code
const parseResult = await client.parse({
  cobolCode: cobolSourceCode,
  bankingSystem: 'fis',
  parseOptions: {
    validateSyntax: true,
    optimizationLevel: 'high'
  }
});

// Generate smart contract
const contractResult = await client.generate({
  ast: parseResult.ast,
  targetBlockchain: 'ethereum',
  bankingSystem: 'fis'
});

// Complete transpilation workflow
const workflowResult = await client.transpile({
  cobolCode: cobolSourceCode,
  bankingSystem: 'fis',
  targetBlockchain: 'ethereum',
  deploymentConfig: {
    network: 'mainnet',
    gasLimit: 5000000
  }
});

console.log('Contract deployed at:', workflowResult.deploymentResult.contractAddress);
```

#### Advanced Features
```typescript
// Batch processing
const batchResults = await client.batchTranspile([
  {
    id: 'job1',
    cobolCode: cobolCode1,
    bankingSystem: 'fis',
    targetBlockchain: 'ethereum'
  },
  {
    id: 'job2',
    cobolCode: cobolCode2,
    bankingSystem: 'fiserv',
    targetBlockchain: 'polygon'
  }
]);

// Monitor workflow progress
const statusUpdates = client.watchWorkflow('wf_abc123def456');
statusUpdates.on('progress', (status) => {
  console.log(`Progress: ${status.progress}% - ${status.currentStep}`);
});
statusUpdates.on('completed', (result) => {
  console.log('Workflow completed:', result);
});
statusUpdates.on('error', (error) => {
  console.error('Workflow failed:', error);
});

// Analytics and monitoring
const analytics = await client.getAnalytics({
  period: '30d',
  metrics: ['usage', 'performance', 'costs']
});
```

### Python SDK

#### Installation
```bash
pip install legacybaas-cobol-transpiler
```

#### Basic Usage
```python
from legacybaas_cobol_transpiler import CobolTranspilerClient
from legacybaas_cobol_transpiler.models import TranspileRequest, DeploymentConfig

client = CobolTranspilerClient(
    api_key='your-api-key',
    base_url='https://api.legacybaas.com/v2',
    timeout=30
)

# Parse COBOL code
parse_result = client.parse(
    cobol_code=cobol_source_code,
    banking_system='fis',
    parse_options={
        'validate_syntax': True,
        'optimization_level': 'high'
    }
)

# Complete transpilation
transpile_request = TranspileRequest(
    cobol_code=cobol_source_code,
    banking_system='fis',
    target_blockchain='ethereum',
    deployment_config=DeploymentConfig(
        network='mainnet',
        gas_limit=5000000,
        gas_price='20'
    )
)

result = client.transpile(transpile_request)
print(f"Contract deployed at: {result.deployment_result.contract_address}")
```

#### Async Support
```python
import asyncio
from legacybaas_cobol_transpiler import AsyncCobolTranspilerClient

async def main():
    async with AsyncCobolTranspilerClient(api_key='your-api-key') as client:
        # Concurrent transpilations
        tasks = [
            client.transpile(request1),
            client.transpile(request2),
            client.transpile(request3)
        ]
        
        results = await asyncio.gather(*tasks)
        
        for i, result in enumerate(results):
            print(f"Job {i+1}: {result.deployment_result.contract_address}")

asyncio.run(main())
```

### Java SDK

#### Maven Dependency
```xml
<dependency>
    <groupId>com.legacybaas</groupId>
    <artifactId>cobol-transpiler-sdk</artifactId>
    <version>2.1.0</version>
</dependency>
```

#### Basic Usage
```java
import com.legacybaas.cobol.CobolTranspilerClient;
import com.legacybaas.cobol.models.*;

public class CobolTranspilerExample {
    public static void main(String[] args) {
        CobolTranspilerClient client = CobolTranspilerClient.builder()
            .apiKey("your-api-key")
            .baseUrl("https://api.legacybaas.com/v2")
            .timeout(Duration.ofSeconds(30))
            .build();
        
        // Parse COBOL
        ParseRequest parseRequest = ParseRequest.builder()
            .cobolCode(cobolSourceCode)
            .bankingSystem("fis")
            .parseOptions(ParseOptions.builder()
                .validateSyntax(true)
                .optimizationLevel("high")
                .build())
            .build();
        
        ParseResult parseResult = client.parse(parseRequest);
        
        // Transpile to smart contract
        TranspileRequest transpileRequest = TranspileRequest.builder()
            .cobolCode(cobolSourceCode)
            .bankingSystem("fis")
            .targetBlockchain("ethereum")
            .deploymentConfig(DeploymentConfig.builder()
                .network("mainnet")
                .gasLimit(5000000L)
                .gasPrice("20")
                .build())
            .build();
        
        TranspileResult result = client.transpile(transpileRequest);
        System.out.println("Contract deployed at: " + 
            result.getDeploymentResult().getContractAddress());
    }
}
```

### C# (.NET) SDK

#### NuGet Package
```bash
dotnet add package LegacyBaaS.CobolTranspiler.SDK
```

#### Basic Usage
```csharp
using LegacyBaaS.CobolTranspiler;
using LegacyBaaS.CobolTranspiler.Models;

class Program
{
    static async Task Main(string[] args)
    {
        var client = new CobolTranspilerClient(new CobolTranspilerOptions
        {
            ApiKey = "your-api-key",
            BaseUrl = "https://api.legacybaas.com/v2",
            Timeout = TimeSpan.FromSeconds(30)
        });

        // Parse COBOL code
        var parseRequest = new ParseRequest
        {
            CobolCode = cobolSourceCode,
            BankingSystem = "fis",
            ParseOptions = new ParseOptions
            {
                ValidateSyntax = true,
                OptimizationLevel = "high"
            }
        };

        var parseResult = await client.ParseAsync(parseRequest);

        // Complete transpilation
        var transpileRequest = new TranspileRequest
        {
            CobolCode = cobolSourceCode,
            BankingSystem = "fis",
            TargetBlockchain = "ethereum",
            DeploymentConfig = new DeploymentConfig
            {
                Network = "mainnet",
                GasLimit = 5000000,
                GasPrice = "20"
            }
        };

        var result = await client.TranspileAsync(transpileRequest);
        Console.WriteLine($"Contract deployed at: {result.DeploymentResult.ContractAddress}");
    }
}
```

## Integration Guides

### FIS Systematics Integration

#### Overview
FIS Systematics is a comprehensive banking platform requiring specific data type mappings and processing patterns.

#### Key Considerations
- **Data Types**: COMP-3 fields require special handling
- **File Structures**: Support for VSAM and sequential files
- **Business Logic**: Payment processing and account management patterns

#### Sample Integration
```javascript
const fisConfig = {
  bankingSystem: 'fis',
  dataTypeMappings: {
    'COMP-3': 'uint128',
    'PIC 9(n)': 'uint256',
    'PIC X(n)': 'string'
  },
  templates: ['payment_processing', 'account_management'],
  optimizations: {
    gasOptimization: true,
    securityLevel: 'high'
  }
};

const result = await client.transpile({
  cobolCode: fisCobolCode,
  ...fisConfig,
  targetBlockchain: 'ethereum'
});
```

### Fiserv DNA Integration

#### Overview
Fiserv DNA focuses on digital banking and modern core banking operations.

#### Key Features
- Enhanced digital banking support
- Real-time processing capabilities
- Modern API integrations

#### Sample Integration
```python
fiserv_config = {
    'banking_system': 'fiserv',
    'features': ['digital_banking', 'real_time_processing'],
    'target_blockchain': 'polygon',  # Optimized for fast transactions
    'deployment_config': {
        'network': 'polygon-mainnet',
        'gas_price': '30'  # Gwei, optimized for Polygon
    }
}

result = client.transpile(
    cobol_code=fiserv_cobol_code,
    **fiserv_config
)
```

### Temenos T24 Integration

#### Overview
Temenos T24 specializes in comprehensive banking solutions with complex business logic.

#### Key Features
- Loan origination and management
- Complex financial calculations
- Multi-currency support

#### Sample Integration
```java
TempenosConfig temenosConfig = TempenosConfig.builder()
    .bankingSystem("temenos")
    .features(Arrays.asList("loan_processing", "multi_currency"))
    .targetBlockchain("ethereum")
    .complexityHandling("advanced")
    .build();

TranspileResult result = client.transpile(
    TranspileRequest.builder()
        .cobolCode(temenosCobolCode)
        .config(temenosConfig)
        .build()
);
```

### TCS BaNCS Integration

#### Overview
TCS BaNCS provides enterprise-grade banking solutions with focus on trade finance and international banking.

#### Key Features
- Trade finance processing
- International payment systems
- Compliance and regulatory support

#### Sample Integration
```csharp
var tcsConfig = new TcsConfig
{
    BankingSystem = "tcs",
    Features = new[] { "trade_finance", "international_payments" },
    TargetBlockchain = "corda",  // Optimized for enterprise use
    ComplianceLevel = "strict",
    DeploymentConfig = new DeploymentConfig
    {
        Network = "corda-mainnet",
        Notary = "O=Notary,L=London,C=GB"
    }
};

var result = await client.TranspileAsync(new TranspileRequest
{
    CobolCode = tcsCobolCode,
    Config = tcsConfig
});
```

## Rate Limits

| Endpoint | Rate Limit | Quota Impact |
|----------|------------|--------------|
| `/parse` | 100/minute | No |
| `/generate` | 50/minute | No |
| `/transpile` | 20/minute | Yes (1 per transpilation) |
| `/validate` | 200/minute | No |
| `/analytics/*` | 1000/minute | No |

## Webhooks

### Transpilation Completion
```json
{
  "event": "transpilation.completed",
  "workflowId": "wf_abc123def456",
  "customerId": "customer_123",
  "result": {
    "success": true,
    "contractAddress": "0x742d35Cc6634C0532925a3b8D3C48e7aB8c8F4A1",
    "processingTime": 25000
  },
  "timestamp": "2025-07-05T10:40:00Z"
}
```

### Quota Alert
```json
{
  "event": "quota.threshold_reached",
  "customerId": "customer_123",
  "quota": {
    "currentUsage": 800,
    "monthlyQuota": 1000,
    "threshold": 80,
    "percentUsed": 80.0
  },
  "timestamp": "2025-07-05T10:40:00Z"
}
```

## Support and Resources

- **API Documentation**: https://docs.legacybaas.com/cobol-transpiler
- **Developer Portal**: https://developer.legacybaas.com
- **Status Page**: https://status.legacybaas.com
- **Support**: support@legacybaas.com
- **Community Forum**: https://community.legacybaas.com

## Changelog

### v2.1.0 (2025-07-05)
- Added enterprise RBAC support
- Implemented SOC2 compliant audit logging
- Enhanced analytics dashboard
- Added quota management system
- Improved performance and security

### v2.0.0 (2025-06-15)
- Initial COBOL transpiler integration
- Support for 4 banking systems (FIS, Fiserv, Temenos, TCS)
- Multi-blockchain deployment support
- Comprehensive template system