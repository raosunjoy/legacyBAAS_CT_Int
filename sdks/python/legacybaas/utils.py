"""
Utility functions for Legacy B2BaaS Python SDK
"""

import re
from typing import Optional


def validate_api_key(api_key: str) -> bool:
    """Validate API key format"""
    if not api_key or len(api_key) < 20:
        return False
    return True


def format_currency(amount: float, currency: str) -> str:
    """Format currency amount"""
    return f"{amount:.2f} {currency}"


def validate_bic(bic: str) -> bool:
    """Validate BIC/SWIFT code format"""
    pattern = r'^[A-Z]{6}[A-Z0-9]{2}([A-Z0-9]{3})?$'
    return bool(re.match(pattern, bic))


def validate_iban(iban: str) -> bool:
    """Validate IBAN format"""
    iban = iban.replace(' ', '').upper()
    if len(iban) < 15 or len(iban) > 34:
        return False
    return iban[:2].isalpha() and iban[2:4].isdigit()