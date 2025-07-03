"""
BaNCS-related type definitions
"""

from typing import Optional, Dict, Any
from dataclasses import dataclass


@dataclass
class BancsTransaction:
    """BaNCS transaction"""
    transaction_id: str
    account_number: str
    transaction_type: str
    amount: float
    currency: str
    status: str


@dataclass
class BancsAccount:
    """BaNCS account information"""
    account_number: str
    account_name: str
    balance: float
    currency: str
    status: str


@dataclass
class BancsResponse:
    """BaNCS API response"""
    status: str
    data: Any
    message: Optional[str] = None