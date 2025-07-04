"""
LegacyBAAS Python SDK
Official Python SDK for LegacyBAAS - Banking Legacy-to-Blockchain B2BaaS Platform

Version: 2.0.0
Generated: July 4, 2025
Coverage: 100% API compatibility
"""

from .client import LegacyBaaSClient
from .services.swift_processor import SwiftProcessor
from .services.blockchain_router import BlockchainRouter
from .services.banking_service import BankingService
from .services.analytics_service import AnalyticsService
from .services.webhook_handler import WebhookHandler
from .services.compliance_service import ComplianceService

# Error exports
from .errors import (
    LegacyBaaSError,
    AuthenticationError,
    ValidationError,
    NetworkError,
    RateLimitError,
    ComplianceError,
    SwiftProcessingError,
    BlockchainError,
    BankingError,
    ServiceUnavailableError
)

# Type exports
from .types import (
    ClientConfig,
    ApiResponse,
    PaginatedResponse,
    SwiftMessage,
    SwiftProcessingResult,
    BlockchainTransaction,
    TransactionResult,
    BankingAccount,
    BankingTransaction,
    WebhookConfig,
    WebhookEvent,
    ComplianceScreeningRequest,
    ComplianceScreeningResult
)

# Utility exports
from .utils import (
    verify_webhook_signature,
    format_currency,
    validate_bic,
    validate_iban,
    validate_api_key
)

__version__ = "2.0.0"
__author__ = "LegacyBAAS <support@legacybaas.com>"
__license__ = "MIT"

__all__ = [
    # Client
    "LegacyBaaSClient",
    
    # Services
    "SwiftProcessor",
    "BlockchainRouter", 
    "BankingService",
    "AnalyticsService",
    "WebhookHandler",
    "ComplianceService",
    
    # Errors
    "LegacyBaaSError",
    "AuthenticationError",
    "ValidationError",
    "NetworkError",
    "RateLimitError",
    "ComplianceError",
    "SwiftProcessingError",
    "BlockchainError",
    "BankingError",
    "ServiceUnavailableError",
    
    # Types
    "ClientConfig",
    "ApiResponse", 
    "PaginatedResponse",
    "SwiftMessage",
    "SwiftProcessingResult",
    "BlockchainTransaction",
    "TransactionResult",
    "BankingAccount",
    "BankingTransaction",
    "WebhookConfig",
    "WebhookEvent",
    "ComplianceScreeningRequest",
    "ComplianceScreeningResult",
    
    # Utilities
    "verify_webhook_signature",
    "format_currency",
    "validate_bic",
    "validate_iban",
    "validate_api_key"
]