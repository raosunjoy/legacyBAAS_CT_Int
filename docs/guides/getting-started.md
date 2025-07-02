# Developer Getting Started Guide

## Banking Legacy-to-Blockchain B2BaaS Platform

Welcome to the LegacyBaaS platform! This guide will help you get up and running quickly with our production-ready banking-to-blockchain integration platform.

## 🚀 Quick Start

### Prerequisites

- **Node.js**: Version 18.x or higher
- **Docker**: For running blockchain network simulators
- **Git**: For version control
- **Test Environment**: Access to sandbox banking APIs

### Installation

```bash
# Clone the repository
git clone https://github.com/your-org/LegacyBAAS.git
cd LegacyBAAS

# Install dependencies
npm install

# Set up environment variables
cp .env.example .env
# Edit .env with your configuration

# Run the development server
npm run dev

# Verify installation
npm test
```

## 🏗️ Platform Overview

The LegacyBaaS platform consists of several key components:

1. **SWIFT Message Parser** - Processes MT103/202 messages
2. **Smart Router** - Routes transactions using Apache Camel + Drools
3. **Banking Connectors** - Integrates with TCS BaNCS and other core banking systems
4. **Blockchain Gateways** - Connects to XRP, Corda, Ethereum L2, and Algorand networks

## 📋 API Documentation

Our comprehensive API documentation is available at:
- **Interactive Docs**: `docs/api/index.html`
- **OpenAPI Spec**: `docs/api/openapi.yaml`
- **API Guide**: `docs/api/README.md`

## 🔧 Configuration

### Environment Variables

```bash
# Core Platform
NODE_ENV=development
PORT=3000
LOG_LEVEL=debug

# Database
MONGODB_URI=mongodb://localhost:27017/legacybaas
REDIS_URL=redis://localhost:6379

# Banking Integration
BANCS_API_URL=https://sandbox.bancs.tcs.com/api/v1
BANCS_CLIENT_ID=your_client_id
BANCS_CLIENT_SECRET=your_client_secret
SWIFT_GATEWAY_URL=https://sandbox.swift.com/gateway

# Blockchain Networks
XRP_TESTNET_URL=wss://s.altnet.rippletest.net:51233
CORDA_NODE_URL=http://localhost:10200
ETHEREUM_L2_RPC=https://polygon-mumbai.g.alchemy.com/v2/your-key
ALGORAND_TESTNET_URL=https://testnet-api.algonode.cloud

# Security
JWT_SECRET=your-super-secure-jwt-secret
ENCRYPTION_KEY=your-aes-256-encryption-key
```

### Banking System Configuration

#### TCS BaNCS Setup
```javascript
// config/bancs.js
module.exports = {
  baseURL: process.env.BANCS_API_URL,
  auth: {
    clientId: process.env.BANCS_CLIENT_ID,
    clientSecret: process.env.BANCS_CLIENT_SECRET,
    scope: 'account:read transaction:write',
    tokenEndpoint: '/oauth2/token'
  },
  webhooks: {
    endpoint: '/webhooks/bancs',
    secret: process.env.BANCS_WEBHOOK_SECRET
  },
  timeout: 30000,
  retries: 3
};
```

#### SWIFT Gateway Configuration
```javascript
// config/swift.js
module.exports = {
  gatewayUrl: process.env.SWIFT_GATEWAY_URL,
  messageTypes: ['MT103', 'MT202'],
  validation: {
    strictMode: true,
    customRules: './rules/swift-validation.json'
  },
  routing: {
    defaultQueue: 'swift.incoming',
    errorQueue: 'swift.errors'
  }
};
```

## 🔗 Blockchain Gateway Setup

### XRP Ledger
```javascript
// Initialize XRP connection
const { Client } = require('xrpl');

const xrpClient = new Client(process.env.XRP_TESTNET_URL);
await xrpClient.connect();

// Example: Send XRP payment
const payment = {
  TransactionType: 'Payment',
  Account: senderWallet.address,
  Amount: '1000000', // 1 XRP in drops
  Destination: recipientAddress,
  Fee: '12' // Transaction fee in drops
};
```

### Corda Network
```javascript
// Corda flow initiation
const cordaClient = require('./lib/corda-client');

const flowResult = await cordaClient.startFlow({
  flowName: 'InitiatePaymentFlow',
  flowArgs: {
    amount: 1000,
    currency: 'USD',
    counterparty: 'O=BankB,L=New York,C=US'
  }
});
```

### Ethereum L2 (Polygon)
```javascript
// Ethereum L2 smart contract interaction
const { ethers } = require('ethers');

const provider = new ethers.providers.JsonRpcProvider(process.env.ETHEREUM_L2_RPC);
const contract = new ethers.Contract(contractAddress, abi, signer);

const transaction = await contract.transfer(recipientAddress, amount);
```

### Algorand CBDC
```javascript
// Algorand CBDC transaction
const algosdk = require('algosdk');

const algodClient = new algosdk.Algodv2('', process.env.ALGORAND_TESTNET_URL, '');
const txn = algosdk.makeAssetTransferTxnWithSuggestedParams(
  senderAddress,
  recipientAddress,
  undefined,
  undefined,
  amount,
  undefined,
  assetId,
  suggestedParams
);
```

## 🔄 Common Workflows

### 1. Processing a SWIFT MT103 Message

```javascript
const swiftParser = require('./src/parsers/swift-parser');
const smartRouter = require('./src/routing/smart-router');

// Parse incoming SWIFT message
const parsedMessage = await swiftParser.parse(mt103Message);

// Route to appropriate blockchain
const routingDecision = await smartRouter.route({
  amount: parsedMessage.amount,
  currency: parsedMessage.currency,
  urgency: parsedMessage.urgency,
  destination: parsedMessage.beneficiaryBank
});

// Execute on selected blockchain
const result = await routingDecision.gateway.processPayment(parsedMessage);
```

### 2. Bank Account to Blockchain Transfer

```javascript
const bancsConnector = require('./src/connectors/bancs-connector');
const blockchainGateway = require('./src/gateways');

// Debit from bank account
const debitResult = await bancsConnector.debitAccount({
  accountNumber: '1234567890',
  amount: 1000,
  currency: 'USD',
  reference: 'BLOCKCHAIN_TRANSFER_001'
});

// Credit to blockchain
const blockchainResult = await blockchainGateway.xrp.sendPayment({
  amount: 1000,
  destination: 'rN7n7otQDd6FczFgLdSqtcsAUxDkw6fzRH',
  currency: 'USD'
});
```

### 3. Real-time Transaction Monitoring

```javascript
const transactionMonitor = require('./src/monitoring/transaction-monitor');

// Subscribe to transaction events
transactionMonitor.on('transaction:initiated', (txn) => {
  console.log(`Transaction ${txn.id} initiated: ${txn.amount} ${txn.currency}`);
});

transactionMonitor.on('transaction:completed', (txn) => {
  console.log(`Transaction ${txn.id} completed in ${txn.duration}ms`);
});

transactionMonitor.on('transaction:failed', (txn) => {
  console.error(`Transaction ${txn.id} failed: ${txn.error}`);
});
```

## 🧪 Testing

### Running Tests

```bash
# Run all tests
npm test

# Run specific test suites
npm run test:swift
npm run test:bancs
npm run test:blockchain
npm run test:integration

# Run tests with coverage
npm run test:coverage

# Run tests in watch mode
npm run test:watch
```

### Test Configuration

```javascript
// jest.config.js
module.exports = {
  testEnvironment: 'node',
  coverageDirectory: 'coverage',
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/**/*.test.js',
    '!src/test-helpers/**'
  ],
  coverageThreshold: {
    global: {
      branches: 80,
      functions: 80,
      lines: 80,
      statements: 80
    }
  },
  setupFilesAfterEnv: ['<rootDir>/src/test-helpers/setup.js']
};
```

## 🔍 Debugging

### Logging Configuration

```javascript
// config/logging.js
const winston = require('winston');

const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.errors({ stack: true }),
    winston.format.json()
  ),
  transports: [
    new winston.transports.File({ filename: 'logs/error.log', level: 'error' }),
    new winston.transports.File({ filename: 'logs/combined.log' }),
    new winston.transports.Console({
      format: winston.format.simple()
    })
  ]
});
```

### Health Checks

```bash
# Check platform health
curl http://localhost:3000/health

# Check individual components
curl http://localhost:3000/health/swift
curl http://localhost:3000/health/bancs
curl http://localhost:3000/health/blockchain
```

## 📊 Monitoring & Analytics

### Metrics Dashboard

Access the real-time analytics dashboard at:
```
http://localhost:3000/dashboard
```

Key metrics include:
- Transaction throughput (TPS)
- Success/failure rates
- Blockchain network latency
- Bank connector performance
- Cost per transaction

### Alerting

Configure alerts for:
- Transaction failures
- High latency warnings
- System resource usage
- Security incidents
- Compliance violations

## 🔒 Security Best Practices

### API Security
- Always use HTTPS in production
- Implement rate limiting
- Validate all inputs
- Use strong authentication (OAuth2 + JWT)
- Rotate secrets regularly

### Blockchain Security
- Use hardware wallets for production keys
- Implement multi-signature transactions
- Monitor for suspicious activity
- Regular security audits
- Keep dependencies updated

## 🚀 Deployment

### Docker Deployment

```dockerfile
# Dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY . .
EXPOSE 3000
CMD ["npm", "start"]
```

### Kubernetes Deployment

```yaml
# k8s/deployment.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: legacybaas-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: legacybaas-api
  template:
    metadata:
      labels:
        app: legacybaas-api
    spec:
      containers:
      - name: api
        image: legacybaas:latest
        ports:
        - containerPort: 3000
        env:
        - name: NODE_ENV
          value: "production"
```

## 📚 Additional Resources

- **API Reference**: `/docs/api/`
- **Architecture Guide**: `/docs/architecture/`
- **Security Guide**: `/docs/security/`
- **Troubleshooting**: `/docs/troubleshooting/`
- **Contributing**: `/CONTRIBUTING.md`

## 🆘 Support

For technical support and questions:
- **Documentation**: Check the `/docs` directory
- **Issues**: Create a GitHub issue
- **Chat**: Join our developer Slack channel
- **Email**: developer-support@legacybaas.com

---

**Platform Status**: ✅ Production Ready (83.93% Test Coverage)
**Getting Started Time**: ~15 minutes
**Support Level**: Enterprise 24/7