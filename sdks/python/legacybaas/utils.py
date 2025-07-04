"""
Utility functions and classes for LegacyBAAS Python SDK
"""

import logging
import time
import hashlib
import hmac
import re
from typing import Any, Dict, Optional
from datetime import datetime


class Logger:
    """Simple logger for SDK operations"""
    
    def __init__(self, level: str = "INFO"):
        self.logger = logging.getLogger("LegacyBAAS")
        self.logger.setLevel(getattr(logging, level.upper()))
        
        if not self.logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
            handler.setFormatter(formatter)
            self.logger.addHandler(handler)
    
    def debug(self, message: str, *args, **kwargs):
        self.logger.debug(message, *args, **kwargs)
    
    def info(self, message: str, *args, **kwargs):
        self.logger.info(message, *args, **kwargs)
    
    def warning(self, message: str, *args, **kwargs):
        self.logger.warning(message, *args, **kwargs)
    
    def error(self, message: str, *args, **kwargs):
        self.logger.error(message, *args, **kwargs)


class RetryHandler:
    """Retry logic for API requests"""
    
    def __init__(self, max_retries: int = 3, retry_delay: float = 1.0, exponential_backoff: bool = True):
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        self.exponential_backoff = exponential_backoff
    
    def should_retry(self, exception: Exception, attempt: int) -> bool:
        """Determine if request should be retried"""
        if attempt >= self.max_retries:
            return False
        
        # Define retryable conditions
        retryable_errors = [
            "timeout",
            "connection",
            "network",
            "server error",
            "503",
            "502",
            "504"
        ]
        
        error_message = str(exception).lower()
        return any(error in error_message for error in retryable_errors)
    
    def get_delay(self, attempt: int) -> float:
        """Calculate retry delay"""
        if self.exponential_backoff:
            return self.retry_delay * (2 ** attempt)
        return self.retry_delay
    
    def sleep(self, duration: float):
        """Sleep for specified duration"""
        time.sleep(duration)


def verify_webhook_signature(payload: str, signature: str, secret: str) -> bool:
    """
    Verify webhook signature
    
    Args:
        payload: Raw webhook payload
        signature: Signature from webhook headers
        secret: Webhook secret
        
    Returns:
        True if signature is valid
    """
    try:
        expected_signature = hmac.new(
            secret.encode('utf-8'),
            payload.encode('utf-8'),
            hashlib.sha256
        ).hexdigest()
        
        # Remove 'sha256=' prefix if present
        if signature.startswith('sha256='):
            signature = signature[7:]
        
        return hmac.compare_digest(expected_signature, signature)
    except Exception:
        return False


def format_currency(amount: float, currency: str) -> str:
    """
    Format currency amount
    
    Args:
        amount: Amount to format
        currency: Currency code
        
    Returns:
        Formatted currency string
    """
    return f"{amount:,.2f} {currency.upper()}"


def validate_bic(bic: str) -> bool:
    """
    Validate SWIFT BIC code
    
    Args:
        bic: BIC code to validate
        
    Returns:
        True if valid BIC format
    """
    # BIC format: 4 letters (bank) + 2 letters (country) + 2 alphanumeric (location) + optional 3 alphanumeric (branch)
    pattern = r'^[A-Z]{4}[A-Z]{2}[A-Z0-9]{2}([A-Z0-9]{3})?$'
    return bool(re.match(pattern, bic.upper())) if bic else False


def validate_iban(iban: str) -> bool:
    """
    Validate IBAN
    
    Args:
        iban: IBAN to validate
        
    Returns:
        True if valid IBAN format
    """
    if not iban:
        return False
    
    # Remove spaces and convert to uppercase
    iban = iban.replace(' ', '').upper()
    
    # Basic length check (15-34 characters)
    if not (15 <= len(iban) <= 34):
        return False
    
    # Basic format check (2 letters + 2 digits + alphanumeric)
    pattern = r'^[A-Z]{2}[0-9]{2}[A-Z0-9]+$'
    if not re.match(pattern, iban):
        return False
    
    # Move first 4 characters to end
    rearranged = iban[4:] + iban[:4]
    
    # Replace letters with numbers (A=10, B=11, ..., Z=35)
    numeric_string = ''
    for char in rearranged:
        if char.isalpha():
            numeric_string += str(ord(char) - ord('A') + 10)
        else:
            numeric_string += char
    
    # Check mod 97
    try:
        return int(numeric_string) % 97 == 1
    except ValueError:
        return False


def parse_iso_date(date_string: str) -> Optional[datetime]:
    """
    Parse ISO date string
    
    Args:
        date_string: ISO date string
        
    Returns:
        Datetime object or None if invalid
    """
    try:
        return datetime.fromisoformat(date_string.replace('Z', '+00:00'))
    except (ValueError, AttributeError):
        return None


def generate_request_id() -> str:
    """Generate unique request ID"""
    timestamp = str(int(time.time() * 1000))
    return f"req_{timestamp}"


def sanitize_log_data(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Sanitize sensitive data for logging
    
    Args:
        data: Dictionary to sanitize
        
    Returns:
        Sanitized dictionary
    """
    sensitive_keys = {
        'password', 'secret', 'token', 'api_key', 'client_secret',
        'authorization', 'auth', 'credential', 'key', 'private'
    }
    
    def sanitize_value(key: str, value: Any) -> Any:
        if isinstance(key, str) and any(sensitive in key.lower() for sensitive in sensitive_keys):
            return "***REDACTED***"
        
        if isinstance(value, dict):
            return {k: sanitize_value(k, v) for k, v in value.items()}
        elif isinstance(value, list):
            return [sanitize_value("", item) for item in value]
        
        return value
    
    return {k: sanitize_value(k, v) for k, v in data.items()}


def validate_api_key(api_key: str) -> bool:
    """
    Validate API key format
    
    Args:
        api_key: API key to validate
        
    Returns:
        True if valid format
    """
    if not api_key or not isinstance(api_key, str):
        return False
    
    # Basic validation - should be alphanumeric with certain length
    return len(api_key) >= 16 and api_key.replace('-', '').replace('_', '').isalnum()


def chunk_list(lst: list, chunk_size: int):
    """
    Split list into chunks
    
    Args:
        lst: List to chunk
        chunk_size: Size of each chunk
        
    Yields:
        List chunks
    """
    for i in range(0, len(lst), chunk_size):
        yield lst[i:i + chunk_size]