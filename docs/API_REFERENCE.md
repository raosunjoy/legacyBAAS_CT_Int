# 🔌 **LegacyBAAS API Reference Guide**

**Version:** 1.0  
**Base URL:** `https://api.legacybaas.com/v1/`  
**Authentication:** OAuth2 / API Keys  
**Format:** JSON  

---

## **📋 Table of Contents**

1. [Authentication](#authentication)
2. [Banking API](#banking-api)
3. [Blockchain API](#blockchain-api)
4. [Smart Router API](#smart-router-api)
5. [Compliance API](#compliance-api)
6. [Monitoring API](#monitoring-api)
7. [Webhook API](#webhook-api)
8. [Error Handling](#error-handling)
9. [Rate Limits](#rate-limits)
10. [SDK Examples](#sdk-examples)

---

## **🔐 Authentication**

### **OAuth2 Client Credentials Flow**

```http
POST /auth/token
Content-Type: application/x-www-form-urlencoded

grant_type=client_credentials&
client_id=your_client_id&
client_secret=your_client_secret&
scope=banking:read banking:write blockchain:execute
```

**Response:**
```json
{
  "access_token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "token_type": "Bearer",
  "expires_in": 3600,
  "scope": "banking:read banking:write blockchain:execute"
}
```

### **Using Bearer Token**
```http
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

---

## **🏦 Banking API**

### **Get Account Balance**

```http
GET /banking/accounts/{accountNumber}/balance
Authorization: Bearer {token}
```

**Parameters:**
- `accountNumber` (string, required): Bank account number
- `currency` (string, optional): Currency code (default: USD)

**Response:**
```json
{
  "accountNumber": "123456789",
  "currency": "USD",
  "currentBalance": 50000.00,
  "availableBalance": 48500.00,
  "holdAmount": 1500.00,
  "lastUpdated": "2025-07-04T10:30:00Z",
  "status": "ACTIVE"
}
```

### **Process Payment**

```http
POST /banking/payments
Authorization: Bearer {token}
Content-Type: application/json
```

**Request Body:**
```json
{
  "amount": 1000.00,
  "currency": "USD",
  "fromAccount": "123456789",
  "toAccount": "987654321",
  "purpose": "Cross-border payment",
  "routing": {
    "preferredNetwork": "xrp",
    "priority": "fast",
    "fallbackNetworks": ["algorand", "corda"]
  },
  "compliance": {
    "skipScreening": false,
    "riskTolerance": "medium"
  }
}
```

**Response:**
```json
{
  "transactionId": "txn_abc123def456",
  "status": "PROCESSING",
  "amount": 1000.00,
  "currency": "USD",
  "fromAccount": "123456789",
  "toAccount": "987654321",
  "routing": {
    "selectedNetwork": "xrp",
    "reason": "Optimal for cross-border payments"
  },
  "estimatedCompletion": "2025-07-04T10:35:00Z",
  "fees": {
    "bankingFee": 2.50,
    "networkFee": 0.000012,
    "totalFee": 2.500012
  },
  "createdAt": "2025-07-04T10:30:00Z"
}
```

### **Get Transaction Status**

```http
GET /banking/transactions/{transactionId}
Authorization: Bearer {token}
```

**Response:**
```json
{
  "transactionId": "txn_abc123def456",
  "status": "COMPLETED",
  "currentPhase": "SETTLEMENT",
  "phases": {
    "banking": {
      "status": "COMPLETED",
      "timestamp": "2025-07-04T10:30:15Z",
      "details": "Debit processed successfully"
    },
    "compliance": {
      "status": "COMPLETED", 
      "timestamp": "2025-07-04T10:30:18Z",
      "details": "All compliance checks passed"
    },
    "blockchain": {
      "status": "COMPLETED",
      "timestamp": "2025-07-04T10:30:23Z",
      "details": "Transaction confirmed on XRP Ledger",
      "hash": "E3F85B...A7D2C9"
    },
    "settlement": {
      "status": "COMPLETED",
      "timestamp": "2025-07-04T10:30:25Z",
      "details": "Settlement completed"
    }
  },
  "blockchain": {
    "network": "xrp",
    "hash": "E3F85B...A7D2C9",
    "confirmations": 1,
    "explorerUrl": "https://xrpscan.com/tx/E3F85B...A7D2C9"
  }
}
```

### **Get Transaction History**

```http
GET /banking/accounts/{accountNumber}/transactions
Authorization: Bearer {token}
```

**Query Parameters:**
- `limit` (integer, optional): Number of transactions (default: 50, max: 200)
- `offset` (integer, optional): Pagination offset (default: 0)
- `from` (string, optional): Start date (ISO 8601)
- `to` (string, optional): End date (ISO 8601)
- `status` (string, optional): Filter by status

**Response:**
```json
{
  "transactions": [
    {
      "transactionId": "txn_abc123",
      "amount": 1000.00,
      "currency": "USD",
      "type": "PAYMENT",
      "status": "COMPLETED",
      "description": "Cross-border payment",
      "timestamp": "2025-07-04T10:30:00Z"
    }
  ],
  "pagination": {
    "total": 150,
    "limit": 50,
    "offset": 0,
    "hasMore": true
  }
}
```

---

## **⛓️ Blockchain API**

### **Get Network Status**

```http
GET /blockchain/networks/status
Authorization: Bearer {token}
```

**Response:**
```json
{
  "networks": {
    "xrp": {
      "status": "HEALTHY",
      "latency": 234,
      "throughput": 1547,
      "lastBlock": 85234567,
      "estimatedFee": 0.000012,
      "currency": "XRP"
    },
    "corda": {
      "status": "HEALTHY",
      "latency": 1250,
      "throughput": 342,
      "nodeCount": 12,
      "estimatedFee": "variable"
    },
    "ethereum-l2": {
      "status": "HEALTHY",
      "latency": 456,
      "throughput": 7000,
      "gasPrice": "30 gwei",
      "estimatedFee": 0.002
    },
    "algorand": {
      "status": "HEALTHY",
      "latency": 189,
      "throughput": 1000,
      "lastRound": 28945673,
      "estimatedFee": 0.001
    }
  },
  "lastUpdated": "2025-07-04T10:30:00Z"
}
```

### **Submit Blockchain Transaction**

```http
POST /blockchain/networks/{network}/transactions
Authorization: Bearer {token}
Content-Type: application/json
```

**Request Body:**
```json
{
  "sender": {
    "address": "rN7n7otQDd6FczFgLdSqtcsAUxDkw6fzRH",
    "tag": 12345
  },
  "receiver": {
    "address": "rPT1Sjq2YGrBMTttX4GZHjKu9dyfzbpAYe",
    "tag": 67890
  },
  "amount": 1000.00,
  "currency": "USD",
  "memo": "Cross-border payment via LegacyBaaS"
}
```

**Response:**
```json
{
  "transactionHash": "E3F85B...A7D2C9",
  "status": "PENDING",
  "network": "xrp",
  "estimatedConfirmation": "2025-07-04T10:30:30Z",
  "explorerUrl": "https://xrpscan.com/tx/E3F85B...A7D2C9"
}
```

### **Get Blockchain Transaction Status**

```http
GET /blockchain/transactions/{hash}
Authorization: Bearer {token}
```

**Response:**
```json
{
  "hash": "E3F85B...A7D2C9",
  "status": "CONFIRMED",
  "network": "xrp",
  "confirmations": 1,
  "blockNumber": 85234567,
  "timestamp": "2025-07-04T10:30:23Z",
  "fee": 0.000012,
  "explorerUrl": "https://xrpscan.com/tx/E3F85B...A7D2C9"
}
```

---

## **🧠 Smart Router API**

### **Get Routing Recommendation**

```http
POST /routing/recommend
Authorization: Bearer {token}
Content-Type: application/json
```

**Request Body:**
```json
{
  "amount": 1000.00,
  "currency": "USD",
  "sourceCountry": "US",
  "destinationCountry": "GB",
  "priority": "fast",
  "complianceLevel": "high"
}
```

**Response:**
```json
{
  "recommendations": [
    {
      "network": "xrp",
      "score": 0.92,
      "estimatedTime": "3-5 seconds",
      "estimatedCost": 0.000012,
      "reason": "Optimal for cross-border payments",
      "pros": ["Fast settlement", "Low cost", "High liquidity"],
      "cons": ["Requires destination tag"]
    },
    {
      "network": "corda",
      "score": 0.85,
      "estimatedTime": "10-30 seconds", 
      "estimatedCost": "Variable",
      "reason": "High compliance and privacy",
      "pros": ["Privacy", "Regulatory compliance"],
      "cons": ["Higher latency", "Variable fees"]
    }
  ],
  "selectedNetwork": "xrp",
  "fallbackNetworks": ["algorand", "corda"]
}
```

### **Get Routing Rules**

```http
GET /routing/rules
Authorization: Bearer {token}
```

**Response:**
```json
{
  "rules": [
    {
      "id": "high-value",
      "condition": "amount > 100000",
      "networks": ["corda", "algorand"],
      "priority": 1
    },
    {
      "id": "cross-border",
      "condition": "sourceCountry != destinationCountry",
      "networks": ["xrp", "corda"],
      "priority": 2
    }
  ]
}
```

---

## **✅ Compliance API**

### **Screen Transaction**

```http
POST /compliance/screen
Authorization: Bearer {token}
Content-Type: application/json
```

**Request Body:**
```json
{
  "transactionId": "txn_abc123",
  "amount": 15000.00,
  "currency": "USD",
  "sender": {
    "name": "John Doe",
    "country": "US",
    "accountNumber": "123456789"
  },
  "receiver": {
    "name": "Jane Smith", 
    "country": "GB",
    "accountNumber": "987654321"
  },
  "purpose": "Business payment"
}
```

**Response:**
```json
{
  "screeningId": "scr_def456ghi789",
  "transactionId": "txn_abc123", 
  "status": "COMPLETED",
  "riskScore": "MEDIUM",
  "approved": true,
  "checks": {
    "aml": {
      "status": "PASSED",
      "score": 0.3,
      "flags": []
    },
    "sanctions": {
      "status": "PASSED",
      "lists": ["OFAC", "EU", "UN"],
      "matches": []
    },
    "ctr": {
      "required": true,
      "status": "FILED",
      "reportId": "CTR_789123"
    },
    "travelRule": {
      "required": true,
      "status": "COMPLETED",
      "beneficiaryData": "Transmitted"
    }
  },
  "completedAt": "2025-07-04T10:30:20Z"
}
```

### **Check Sanctions**

```http
POST /compliance/sanctions/check
Authorization: Bearer {token}
Content-Type: application/json
```

**Request Body:**
```json
{
  "entities": [
    {
      "type": "person",
      "name": "John Doe",
      "dateOfBirth": "1980-01-15",
      "nationality": "US"
    },
    {
      "type": "organization", 
      "name": "ABC Corp",
      "country": "US"
    }
  ]
}
```

**Response:**
```json
{
  "results": [
    {
      "entity": {
        "type": "person",
        "name": "John Doe"
      },
      "matches": [],
      "status": "CLEAR",
      "listsChecked": ["OFAC SDN", "EU Sanctions", "UN Sanctions"]
    }
  ],
  "checkedAt": "2025-07-04T10:30:25Z"
}
```

---

## **📊 Monitoring API**

### **Get System Metrics**

```http
GET /monitoring/metrics
Authorization: Bearer {token}
```

**Response:**
```json
{
  "system": {
    "uptime": 86400,
    "cpuUsage": 45.2,
    "memoryUsage": 62.8,
    "diskUsage": 23.4
  },
  "api": {
    "requestsPerSecond": 150,
    "averageResponseTime": 180,
    "errorRate": 0.05,
    "activeConnections": 1250
  },
  "banking": {
    "connectorsOnline": 4,
    "averageProcessingTime": 2500,
    "successRate": 99.8
  },
  "blockchain": {
    "networksHealthy": 8,
    "averageConfirmationTime": 192,
    "successRate": 99.9
  },
  "timestamp": "2025-07-04T10:30:00Z"
}
```

### **Get Health Check**

```http
GET /monitoring/health
Authorization: Bearer {token}
```

**Response:**
```json
{
  "status": "HEALTHY",
  "version": "1.0.0",
  "timestamp": "2025-07-04T10:30:00Z",
  "services": {
    "database": "HEALTHY",
    "redis": "HEALTHY",
    "kafka": "HEALTHY",
    "banking_connectors": "HEALTHY",
    "blockchain_gateways": "HEALTHY"
  },
  "dependencies": {
    "fis_systematics": "CONNECTED",
    "fiserv_dna": "CONNECTED", 
    "temenos_transact": "CONNECTED",
    "tcs_bancs": "CONNECTED",
    "xrp_ledger": "CONNECTED",
    "corda_network": "CONNECTED"
  }
}
```

---

## **🔔 Webhook API**

### **Register Webhook**

```http
POST /webhooks/register
Authorization: Bearer {token}
Content-Type: application/json
```

**Request Body:**
```json
{
  "url": "https://your-system.com/webhooks/legacybaas",
  "events": [
    "payment.completed",
    "payment.failed",
    "compliance.flagged",
    "blockchain.confirmed"
  ],
  "secret": "your-webhook-secret"
}
```

**Response:**
```json
{
  "webhookId": "wh_abc123def456",
  "url": "https://your-system.com/webhooks/legacybaas",
  "events": [
    "payment.completed",
    "payment.failed", 
    "compliance.flagged",
    "blockchain.confirmed"
  ],
  "status": "ACTIVE",
  "createdAt": "2025-07-04T10:30:00Z"
}
```

### **Webhook Payload Example**

```json
{
  "event": "payment.completed",
  "timestamp": "2025-07-04T10:35:22Z",
  "data": {
    "transactionId": "txn_abc123",
    "status": "COMPLETED",
    "amount": 1000.00,
    "currency": "USD",
    "fromAccount": "123456789",
    "toAccount": "987654321",
    "blockchain": {
      "network": "xrp",
      "hash": "E3F85B...A7D2C9",
      "confirmations": 1
    },
    "fees": {
      "total": 2.500012
    },
    "completedAt": "2025-07-04T10:35:22Z"
  },
  "signature": "sha256=5d41402abc4b2a76b9719d911017c592"
}
```

### **List Webhooks**

```http
GET /webhooks
Authorization: Bearer {token}
```

**Response:**
```json
{
  "webhooks": [
    {
      "webhookId": "wh_abc123def456",
      "url": "https://your-system.com/webhooks/legacybaas",
      "events": ["payment.completed", "payment.failed"],
      "status": "ACTIVE",
      "createdAt": "2025-07-04T10:30:00Z"
    }
  ]
}
```

---

## **❌ Error Handling**

### **Error Response Format**

```json
{
  "error": {
    "code": "INSUFFICIENT_FUNDS",
    "message": "Account balance insufficient for transaction",
    "details": {
      "accountNumber": "123456789",
      "requestedAmount": 1000.00,
      "availableBalance": 500.00
    },
    "timestamp": "2025-07-04T10:30:00Z",
    "requestId": "req_abc123def456"
  }
}
```

### **Common Error Codes**

| **Code** | **HTTP Status** | **Description** |
|----------|----------------|------------------|
| `AUTHENTICATION_FAILED` | 401 | Invalid credentials |
| `AUTHORIZATION_FAILED` | 403 | Insufficient permissions |
| `INSUFFICIENT_FUNDS` | 400 | Account balance too low |
| `INVALID_ACCOUNT` | 400 | Account number not found |
| `ACCOUNT_INACTIVE` | 400 | Account is closed or frozen |
| `COMPLIANCE_FAILURE` | 400 | Transaction failed compliance checks |
| `NETWORK_ERROR` | 503 | Blockchain network unavailable |
| `RATE_LIMIT_EXCEEDED` | 429 | Too many requests |
| `INVALID_TRANSACTION` | 400 | Transaction validation failed |

---

## **⏱️ Rate Limits**

### **Default Limits**

| **Endpoint Category** | **Requests per Second** | **Requests per Hour** |
|----------------------|------------------------|----------------------|
| **Authentication** | 10 | 100 |
| **Banking Operations** | 100 | 10,000 |
| **Blockchain Operations** | 50 | 5,000 |
| **Compliance Screening** | 20 | 2,000 |
| **Monitoring** | 200 | 20,000 |

### **Rate Limit Headers**

```http
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1625097600
```

---

## **🛠️ SDK Examples**

### **JavaScript/TypeScript**

```typescript
import { LegacyBaaSClient } from '@legacybaas/sdk';

const client = new LegacyBaaSClient({
  clientId: process.env.LEGACYBAAS_CLIENT_ID,
  clientSecret: process.env.LEGACYBAAS_CLIENT_SECRET,
  environment: 'production'
});

// Process payment
const payment = await client.payments.create({
  amount: 1000,
  currency: 'USD',
  fromAccount: '123456789',
  toAccount: '987654321'
});

// Track payment status
payment.on('status', (status) => {
  console.log(`Payment status: ${status.phase}`);
});
```

### **Python**

```python
from legacybaas import LegacyBaaSClient

client = LegacyBaaSClient(
    client_id=os.environ['LEGACYBAAS_CLIENT_ID'],
    client_secret=os.environ['LEGACYBAAS_CLIENT_SECRET'],
    environment='production'
)

# Process payment
payment = client.payments.create(
    amount=1000.00,
    currency='USD',
    from_account='123456789',
    to_account='987654321'
)

print(f"Payment ID: {payment.transaction_id}")
```

### **Java**

```java
import com.legacybaas.LegacyBaaSClient;
import com.legacybaas.models.PaymentRequest;

LegacyBaaSClient client = new LegacyBaaSClient.Builder()
    .clientId(System.getenv("LEGACYBAAS_CLIENT_ID"))
    .clientSecret(System.getenv("LEGACYBAAS_CLIENT_SECRET"))
    .environment("production")
    .build();

PaymentRequest request = new PaymentRequest.Builder()
    .amount(1000.00)
    .currency("USD")
    .fromAccount("123456789")
    .toAccount("987654321")
    .build();

PaymentResponse payment = client.payments().create(request);
System.out.println("Payment ID: " + payment.getTransactionId());
```

---

**Document Version:** 1.0  
**Last Updated:** July 4, 2025  
**Support:** api-support@legacybaas.com