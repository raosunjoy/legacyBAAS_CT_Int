"""
Type definitions for LegacyBAAS Python SDK
"""

from typing import Dict, Any, List, Optional, Union
from dataclasses import dataclass
from datetime import datetime


@dataclass
class ClientConfig:
    """Client configuration"""
    client_id: str
    client_secret: str
    base_url: str = "https://api.legacybaas.com/v1"
    environment: str = "production"
    timeout: float = 30.0
    max_retries: int = 3
    retry_delay: float = 1.0
    enable_logging: bool = True
    log_level: str = "INFO"


@dataclass
class ApiResponse:
    """API response structure"""
    success: bool
    data: Any
    message: Optional[str] = None
    error: Optional[Dict[str, Any]] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class PaginatedResponse:
    """Paginated response structure"""
    data: List[Any]
    pagination: Dict[str, Any]


@dataclass
class SwiftMessage:
    """SWIFT message structure"""
    message_id: str
    message_type: str
    content: str
    sender: str
    receiver: str
    timestamp: str
    priority: Optional[str] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class SwiftProcessingResult:
    """SWIFT processing result"""
    message_id: str
    status: str
    blockchain_tx_id: Optional[str] = None
    routing_decision: Optional[Dict[str, Any]] = None
    validation_result: Optional[Dict[str, Any]] = None
    processing_time: Optional[float] = None
    timestamp: Optional[str] = None


@dataclass
class BlockchainTransaction:
    """Blockchain transaction structure"""
    transaction_id: str
    amount: str
    currency: str
    from_address: str
    to_address: str
    network: str
    metadata: Optional[Dict[str, Any]] = None
    priority: Optional[str] = None
    swift_reference: Optional[str] = None


@dataclass
class TransactionResult:
    """Transaction result structure"""
    transaction_id: str
    blockchain_tx_id: str
    network: str
    status: str
    fees: Dict[str, Any]
    timestamps: Dict[str, Any]
    confirmations: int
    block_number: Optional[int] = None
    estimated_time: Optional[str] = None


@dataclass
class BankingAccount:
    """Banking account structure"""
    account_id: str
    account_number: str
    account_type: str
    currency: str
    balance: Dict[str, Any]
    status: str
    holder: Dict[str, Any]
    bank_info: Dict[str, Any]
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class BankingTransaction:
    """Banking transaction structure"""
    transaction_id: str
    account_id: str
    type: str
    amount: float
    currency: str
    description: str
    reference_number: Optional[str] = None
    counterparty: Optional[Dict[str, Any]] = None
    status: str = "pending"
    timestamps: Optional[Dict[str, Any]] = None
    fees: Optional[Dict[str, Any]] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class WebhookConfig:
    """Webhook configuration"""
    url: str
    events: List[str]
    webhook_id: Optional[str] = None
    secret: Optional[str] = None
    description: Optional[str] = None
    is_active: bool = True
    retry_config: Optional[Dict[str, Any]] = None
    filters: Optional[Dict[str, Any]] = None
    headers: Optional[Dict[str, str]] = None


@dataclass
class WebhookEvent:
    """Webhook event structure"""
    event_id: str
    event_type: str
    timestamp: str
    data: Dict[str, Any]
    metadata: Dict[str, Any]


@dataclass
class ComplianceScreeningRequest:
    """Compliance screening request"""
    type: str  # 'individual' | 'entity' | 'transaction'
    data: Dict[str, Any]
    screening_lists: Optional[List[str]] = None
    risk_tolerance: str = "medium"
    additional_data: Optional[Dict[str, Any]] = None


@dataclass
class ComplianceScreeningResult:
    """Compliance screening result"""
    screening_id: str
    status: str
    risk_score: float
    risk_level: str
    matches: List[Dict[str, Any]]
    recommendations: List[Dict[str, Any]]
    processing_time: float
    timestamp: str