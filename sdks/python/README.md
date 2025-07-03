# Legacy B2BaaS Python SDK

Banking Legacy-to-Blockchain B2BaaS Platform SDK for Python

## Installation

```bash
pip install legacybaas-python-sdk
```

## Quick Start

```python
from legacybaas import LegacyBaaSClient

client = LegacyBaaSClient(api_key="your-api-key")

# Process SWIFT MT103 message
mt103 = {
    "transaction_reference": "TXN123456",
    "currency": "USD",
    "amount": 10000.00,
    "ordering_customer": {
        "account": "123456789",
        "name": "John Doe"
    },
    "beneficiary_customer": {
        "account": "987654321",
        "name": "Jane Smith"
    }
}

result = client.swift.process_mt103(mt103)
print(f"Transaction ID: {result.transaction_id}")
```

## Features

- SWIFT message processing (MT103, MT202, MT700)
- Multi-blockchain routing and transactions
- BaNCS integration for legacy banking systems
- Real-time analytics and monitoring
- Webhook support for event notifications
- Async/await support for high-performance applications

## Documentation

For complete documentation, visit [https://docs.legacybaas.com/sdks/python](https://docs.legacybaas.com/sdks/python)