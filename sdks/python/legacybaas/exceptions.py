"""
Exception classes for Legacy B2BaaS Python SDK
"""

from typing import Optional, Dict, Any


class LegacyBaaSError(Exception):
    """Base exception for Legacy B2BaaS SDK"""
    
    def __init__(
        self,
        message: str,
        status_code: Optional[int] = None,
        error_code: Optional[str] = None,
        details: Optional[Dict[str, Any]] = None
    ):
        super().__init__(message)
        self.message = message
        self.status_code = status_code
        self.error_code = error_code
        self.details = details or {}
    
    def __str__(self) -> str:
        return self.message


class AuthenticationError(LegacyBaaSError):
    """Authentication related errors"""
    pass


class ValidationError(LegacyBaaSError):
    """Validation related errors"""
    pass


class NetworkError(LegacyBaaSError):
    """Network related errors"""
    pass


class RateLimitError(LegacyBaaSError):
    """Rate limiting errors"""
    pass


class ServiceUnavailableError(LegacyBaaSError):
    """Service unavailable errors"""
    pass