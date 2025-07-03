"""
Banking Legacy-to-Blockchain B2BaaS Platform SDK for Python

A comprehensive Python client library for integrating with the Legacy B2BaaS Platform.
Supports SWIFT message processing, multi-blockchain routing, BaNCS integration,
and real-time analytics.

Example:
    Basic usage:
    
    >>> from legacybaas import LegacyBaaSClient
    >>> client = LegacyBaaSClient(api_key="your-api-key")
    >>> 
    >>> # Process SWIFT MT103 message
    >>> mt103 = {
    ...     "transaction_reference": "TXN123456",
    ...     "value_date": "2025-07-03",
    ...     "currency": "USD",
    ...     "amount": 10000.00,
    ...     "ordering_customer": {
    ...         "account": "123456789",
    ...         "name": "John Doe"
    ...     },
    ...     "beneficiary_customer": {
    ...         "account": "987654321", 
    ...         "name": "Jane Smith"
    ...     }
    ... }
    >>> result = client.swift.process_mt103(mt103)
    >>> print(f"Transaction ID: {result.transaction_id}")
    
    Async usage:
    
    >>> import asyncio
    >>> from legacybaas import AsyncLegacyBaaSClient
    >>> 
    >>> async def main():
    ...     async with AsyncLegacyBaaSClient(api_key="your-api-key") as client:
    ...         result = await client.swift.process_mt103(mt103)
    ...         print(f"Transaction ID: {result.transaction_id}")
    >>> 
    >>> asyncio.run(main())
"""

__version__ = "1.0.0"
__author__ = "Legacy B2BaaS Platform"
__email__ = "sdk@legacybaas.com"
__license__ = "MIT"

# Core client classes
from .client import LegacyBaaSClient
from .async_client import AsyncLegacyBaaSClient

# Service modules
from .services.swift import SwiftProcessor
from .services.blockchain import BlockchainRouter  
from .services.bancs import BancsIntegration
from .services.analytics import AnalyticsService
from .services.webhooks import WebhookHandler

# Exception classes
from .exceptions import (
    LegacyBaaSError,
    AuthenticationError,
    ValidationError,
    NetworkError,
    RateLimitError,
    ServiceUnavailableError,
)

# Type definitions
from .types.swift import SwiftMessage, MT103Message, MT202Message, MT700Message
from .types.blockchain import BlockchainTransaction, TransactionResult, NetworkInfo
from .types.bancs import BancsTransaction, BancsAccount, BancsResponse
from .types.common import ApiResponse, HealthStatus, PaginationOptions

# Configuration
from .config import Config, Environment

# Utilities
from .utils import format_currency, validate_bic, validate_iban

# Version info
VERSION = __version__

# Public API
__all__ = [
    # Core clients
    "LegacyBaaSClient",
    "AsyncLegacyBaaSClient",
    
    # Services
    "SwiftProcessor",
    "BlockchainRouter", 
    "BancsIntegration",
    "AnalyticsService",
    "WebhookHandler",
    
    # Exceptions
    "LegacyBaaSError",
    "AuthenticationError",
    "ValidationError", 
    "NetworkError",
    "RateLimitError",
    "ServiceUnavailableError",
    
    # Types
    "SwiftMessage",
    "MT103Message", 
    "MT202Message",
    "MT700Message",
    "BlockchainTransaction",
    "TransactionResult",
    "NetworkInfo",
    "BancsTransaction",
    "BancsAccount",
    "BancsResponse",
    "ApiResponse",
    "HealthStatus",
    "PaginationOptions",
    
    # Configuration
    "Config",
    "Environment",
    
    # Utilities
    "format_currency",
    "validate_bic",
    "validate_iban",
    
    # Version
    "VERSION",
]

# SDK configuration
default_config = Config(
    base_url="https://api.legacybaas.com",
    version="v1",
    timeout=30.0,
    max_retries=3,
    retry_delay=1.0,
    enable_logging=True,
    log_level="INFO",
)