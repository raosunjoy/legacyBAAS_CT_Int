# Banking Legacy-to-Blockchain B2BaaS Platform API Documentation

## Overview

Welcome to the Banking Legacy-to-Blockchain B2BaaS Platform API documentation. This platform enables banks to seamlessly integrate their legacy systems with multiple blockchain networks without requiring system replacement.

## 🚀 Quick Start

### Base URLs

- **Production**: `https://api.legacybaas.com/v1`
- **Sandbox**: `https://sandbox.legacybaas.com/v1`
- **Development**: `http://localhost:3000/v1`

### Authentication

All API endpoints require OAuth2 authentication using the client credentials flow.

```bash
# Get access token
curl -X POST https://api.legacybaas.com/v1/oauth/token \
  -H "Content-Type: application/x-www-form-urlencoded" \
  -d "grant_type=client_credentials" \
  -d "client_id=YOUR_CLIENT_ID" \
  -d "client_secret=YOUR_CLIENT_SECRET"
```

Use the returned access token in the Authorization header:
```
Authorization: Bearer YOUR_ACCESS_TOKEN
```

## 📚 API Reference

### Core Resources

#### Transactions
- `POST /transactions` - Submit a new cross-border payment
- `GET /transactions/{transactionId}` - Get transaction status

#### SWIFT Processing
- `POST /swift/parse` - Parse and validate SWIFT messages

#### Smart Routing
- `POST /routing/calculate` - Calculate optimal blockchain route

#### Banking Integration
- `GET /banking/accounts/{accountNumber}` - Get account details
- `GET /banking/accounts/{accountNumber}/balance` - Check account balance

#### Analytics
- `GET /analytics/dashboard` - Get dashboard data
- `POST /analytics/transactions/search` - Search transactions

#### Webhooks
- `POST /webhooks` - Register webhook endpoint
- `DELETE /webhooks/{webhookId}` - Remove webhook

### OpenAPI Specification

The complete OpenAPI 3.0 specification is available at:
- [openapi.yaml](./openapi.yaml) - Machine-readable specification
- [API Explorer](#) - Interactive documentation (coming soon)

## 🔧 Integration Examples

### Submit a Cross-Border Payment

```javascript
const response = await fetch('https://api.legacybaas.com/v1/transactions', {
  method: 'POST',
  headers: {
    'Authorization': 'Bearer YOUR_ACCESS_TOKEN',
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({
    messageType: 'MT103',
    amount: 50000,
    currency: 'USD',
    sender: {
      account: '1234567890',
      name: 'ACME Corporation',
      bic: 'CHASUS33'
    },
    receiver: {
      account: '0987654321',
      name: 'Beta Industries',
      bic: 'DEUTDEFF'
    },
    remittanceInfo: 'Invoice payment INV-2023-001',
    priority: 'normal'
  })
});

const transaction = await response.json();
console.log(`Transaction ID: ${transaction.transactionId}`);
console.log(`Routed to: ${transaction.routingDecision.network}`);
```

### Parse SWIFT MT103 Message

```javascript
const swiftMessage = `{1:F01CHASUS33AXXX0000000000}
{2:I103DEUTDEFFXXXXN}
{4:
:20:REFERENCE123
:23B:CRED
:32A:230615USD50000,00
:50K:/1234567890
ACME CORPORATION
123 MAIN STREET
NEW YORK NY 10001
:59:/0987654321
BETA INDUSTRIES
456 INDUSTRIAL AVE
FRANKFURT 60311
:71A:OUR
-}`;

const response = await fetch('https://api.legacybaas.com/v1/swift/parse', {
  method: 'POST',
  headers: {
    'Authorization': 'Bearer YOUR_ACCESS_TOKEN',
    'Content-Type': 'text/plain'
  },
  body: swiftMessage
});

const parsed = await response.json();
```

### Register Webhook for Notifications

```javascript
const response = await fetch('https://api.legacybaas.com/v1/webhooks', {
  method: 'POST',
  headers: {
    'Authorization': 'Bearer YOUR_ACCESS_TOKEN',
    'Content-Type': 'application/json'
  },
  body: JSON.stringify({
    url: 'https://your-app.com/webhooks/legacybaas',
    events: [
      'transaction.created',
      'transaction.completed',
      'transaction.failed'
    ],
    secret: 'your-webhook-secret'
  })
});

const webhook = await response.json();
console.log(`Webhook registered: ${webhook.webhookId}`);
```

## 🌐 Supported Blockchain Networks

### XRP Ledger
- **Use Cases**: Cross-border payments, liquidity management
- **Settlement Time**: 3-5 seconds
- **Cost**: ~$0.0002 per transaction

### R3 Corda
- **Use Cases**: High-value B2B transactions, trade finance
- **Settlement Time**: Near-instant within network
- **Privacy**: Transaction details only visible to involved parties

### Ethereum Layer 2
- **Use Cases**: Tokenized deposits, DeFi integration
- **Networks**: Polygon, Arbitrum, Optimism
- **Smart Contracts**: ERC-20 token support

### Algorand
- **Use Cases**: Central Bank Digital Currencies (CBDC)
- **Features**: Atomic swaps, asset freeze/unfreeze
- **Compliance**: Built-in regulatory controls

## 📊 Response Codes

| Code | Description |
|------|-------------|
| 200 | Success |
| 201 | Created |
| 204 | No Content |
| 400 | Bad Request - Invalid input |
| 401 | Unauthorized - Invalid or missing token |
| 404 | Not Found - Resource doesn't exist |
| 422 | Unprocessable Entity - Validation error |
| 429 | Too Many Requests - Rate limit exceeded |
| 500 | Internal Server Error |
| 503 | Service Unavailable |

## 🔒 Security

### Authentication
- OAuth2 client credentials flow
- Token expiration: 1 hour
- Refresh tokens supported

### Webhook Signatures
All webhooks include HMAC-SHA256 signatures for verification:
```
X-Signature: sha256=HMAC_SIGNATURE
```

### Rate Limiting
- Default: 1000 requests per hour
- Bulk operations: 100 requests per hour
- Headers: `X-RateLimit-Limit`, `X-RateLimit-Remaining`, `X-RateLimit-Reset`

## 🆘 Support

### Documentation
- [Architecture Guide](../architecture/README.md)
- [Integration Guide](../guides/integration-guide.md)
- [Troubleshooting](../guides/troubleshooting.md)

### Contact
- **Email**: api-support@legacybaas.com
- **Developer Portal**: https://developers.legacybaas.com
- **Status Page**: https://status.legacybaas.com

## 📝 Changelog

### v1.0.0 (2025-07-02)
- Initial API release
- SWIFT MT103/MT202 support
- Multi-blockchain routing (XRP, Corda, Ethereum L2, Algorand)
- TCS BaNCS integration
- Real-time analytics dashboard
- Webhook notifications
- CBDC operations support

---

© 2025 LegacyBaaS Platform. All rights reserved.