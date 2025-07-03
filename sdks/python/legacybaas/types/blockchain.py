"""
Blockchain-related type definitions
"""

from typing import Optional, Dict, Any, List
from dataclasses import dataclass


@dataclass
class BlockchainTransaction:
    """Blockchain transaction"""
    transaction_id: str
    blockchain_hash: str
    network: str
    status: str
    amount: str
    currency: str


@dataclass
class TransactionResult:
    """Transaction processing result"""
    transaction_id: str
    status: str
    blockchain_hash: Optional[str] = None


@dataclass
class NetworkInfo:
    """Blockchain network information"""
    network: str
    name: str
    native_currency: str
    chain_id: Optional[int] = None
    status: str = "active"