# LegacyBAAS TypeScript SDK

Official TypeScript/JavaScript SDK for LegacyBAAS - Banking Legacy-to-Blockchain B2BaaS Platform.

## Features

- **Complete API Coverage**: All LegacyBAAS APIs including SWIFT processing, blockchain routing, banking operations, compliance, analytics, and webhooks
- **TypeScript Support**: Full type definitions for enhanced developer experience
- **Smart Routing**: AI-powered routing across 11+ blockchain networks
- **Banking Integration**: Support for FIS, Fiserv, Temenos, TCS BaNCS, and other core banking systems
- **Compliance**: Built-in AML/KYC screening and regulatory compliance
- **Real-time Events**: WebSocket support for real-time transaction updates
- **Error Handling**: Comprehensive error handling with retry logic
- **Analytics**: Advanced analytics and reporting capabilities

## Installation

```bash
npm install @legacybaas/sdk
# or
yarn add @legacybaas/sdk
```

## Quick Start

```typescript
import { LegacyBaaSClient } from '@legacybaas/sdk';

// Initialize the client
const client = new LegacyBaaSClient({
  clientId: 'your-client-id',
  clientSecret: 'your-client-secret',
  environment: 'production', // or 'sandbox', 'development'
  enableRealTime: true,
  enableLogging: true
});

// Authenticate
await client.authenticate();

// Process SWIFT message
const swiftResult = await client.swift.processMT103({
  transactionReference: 'FT21001234567',
  orderingCustomer: 'ACME Corp',
  beneficiaryCustomer: 'Global Trading Ltd',
  amount: '100000.00',
  currency: 'USD',
  valueDate: '20231201'
});

// Route blockchain transaction
const blockchainResult = await client.blockchain.routeTransaction({
  transactionId: 'tx-12345',
  amount: '100000.00',
  currency: 'USD',
  fromAddress: '0x1234....',
  toAddress: '0x5678....',
  network: 'ethereum'
});

// Get account information
const account = await client.banking.getAccount('account-123');

// Screen for compliance
const complianceResult = await client.compliance.screenSubject({
  type: 'individual',
  data: {
    firstName: 'John',
    lastName: 'Doe',
    dateOfBirth: '1990-01-01',
    nationality: 'US'
  }
});
```

## Advanced Usage

### Real-time Events

```typescript
// Listen for real-time events
client.on('transaction.completed', (data) => {
  console.log('Transaction completed:', data);
});

client.on('swift.message.processed', (data) => {
  console.log('SWIFT message processed:', data);
});

client.on('compliance.alert', (data) => {
  console.log('Compliance alert:', data);
});
```

### Error Handling

```typescript
import { 
  LegacyBaaSError, 
  AuthenticationError, 
  ValidationError,
  ComplianceError 
} from '@legacybaas/sdk';

try {
  const result = await client.swift.processMessage(message);
} catch (error) {
  if (error instanceof AuthenticationError) {
    console.error('Authentication failed:', error.message);
    // Re-authenticate
    await client.authenticate();
  } else if (error instanceof ValidationError) {
    console.error('Validation error:', error.details);
  } else if (error instanceof ComplianceError) {
    console.error('Compliance violation:', error.message);
  } else if (error instanceof LegacyBaaSError) {
    console.error('API error:', error.message, error.statusCode);
  }
}
```

### Analytics and Reporting

```typescript
// Get system metrics
const metrics = await client.analytics.getMetrics('banking', {
  start: '2023-01-01',
  end: '2023-12-31'
});

// Create custom dashboard
const dashboard = await client.analytics.createDashboard({
  dashboardId: 'my-dashboard',
  name: 'Transaction Analytics',
  widgets: [
    {
      widgetId: 'transactions-chart',
      type: 'chart',
      title: 'Daily Transactions',
      query: {
        metrics: ['transaction_count', 'transaction_volume'],
        timeRange: { start: '2023-01-01', end: '2023-12-31' }
      }
    }
  ]
});
```

### Webhook Management

```typescript
// Create webhook
const webhook = await client.webhooks.createWebhook({
  url: 'https://your-app.com/webhooks/legacybaas',
  events: ['transaction.completed', 'swift.message.processed'],
  secret: 'your-webhook-secret'
});

// Verify webhook signature (in your webhook handler)
import { WebhookHandler } from '@legacybaas/sdk';

const isValid = WebhookHandler.verifySignature(
  requestBody,
  signatureHeader,
  webhookSecret
);

if (isValid) {
  const event = WebhookHandler.parseWebhookPayload(requestBody);
  // Process webhook event
}
```

## API Reference

### Client Configuration

```typescript
interface LegacyBaaSClientConfig {
  clientId: string;
  clientSecret: string;
  baseUrl?: string;
  environment?: 'production' | 'sandbox' | 'development';
  timeout?: number;
  maxRetries?: number;
  retryDelay?: number;
  enableRealTime?: boolean;
  enableLogging?: boolean;
  logLevel?: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';
}
```

### Services

- **`client.swift`**: SWIFT message processing and routing
- **`client.blockchain`**: Blockchain transaction routing and management
- **`client.banking`**: Banking operations and account management
- **`client.analytics`**: Analytics, metrics, and reporting
- **`client.webhooks`**: Webhook management and real-time events
- **`client.compliance`**: AML/KYC screening and compliance

## Testing

```bash
npm test
```

## Building

```bash
npm run build
```

## Contributing

Please read our [Contributing Guide](CONTRIBUTING.md) for details on our code of conduct and the process for submitting pull requests.

## Support

- Documentation: https://docs.legacybaas.com
- Support: support@legacybaas.com
- Issues: https://github.com/legacybaas/sdk-typescript/issues

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.