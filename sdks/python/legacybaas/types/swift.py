"""
SWIFT-related type definitions
"""

from typing import Optional, Dict, Any, List
from dataclasses import dataclass


@dataclass
class SwiftMessage:
    """Base SWIFT message"""
    message_type: str
    data: Dict[str, Any]


@dataclass
class MT103Message:
    """MT103 Customer Credit Transfer"""
    transaction_reference: str
    currency: str
    amount: float
    ordering_customer: Dict[str, str]
    beneficiary_customer: Dict[str, str]
    value_date: Optional[str] = None
    remittance_information: Optional[str] = None


@dataclass
class MT202Message:
    """MT202 General Financial Institution Transfer"""
    transaction_reference: str
    currency: str
    amount: float
    ordering_institution: Dict[str, str]
    beneficiary_institution: Dict[str, str]
    value_date: Optional[str] = None


@dataclass
class MT700Message:
    """MT700 Issue of a Documentary Credit"""
    documentary_credit_number: str
    date_of_issue: str
    currency: str
    amount: float
    applicant: Dict[str, str]
    beneficiary: Dict[str, str]