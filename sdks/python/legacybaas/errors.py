"""
Error classes for LegacyBAAS Python SDK
"""

from typing import Any, Optional, Dict


class LegacyBaaSError(Exception):
    """Base exception for LegacyBAAS SDK"""
    
    def __init__(
        self,
        message: str,
        status_code: Optional[int] = None,
        error_code: Optional[str] = None,
        details: Optional[Any] = None
    ):
        super().__init__(message)
        self.message = message
        self.status_code = status_code
        self.error_code = error_code
        self.details = details
    
    def __str__(self) -> str:
        return self.message
    
    def __repr__(self) -> str:
        return f"{self.__class__.__name__}('{self.message}', status_code={self.status_code}, error_code='{self.error_code}')"
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert error to dictionary"""
        return {
            "type": self.__class__.__name__,
            "message": self.message,
            "status_code": self.status_code,
            "error_code": self.error_code,
            "details": self.details
        }


class AuthenticationError(LegacyBaaSError):
    """Authentication related errors"""
    
    def __init__(self, message: str = "Authentication failed", status_code: int = 401, error_code: str = "AUTHENTICATION_FAILED"):
        super().__init__(message, status_code, error_code)


class ValidationError(LegacyBaaSError):
    """Request validation errors"""
    
    def __init__(self, message: str = "Validation error", status_code: int = 400, error_code: str = "VALIDATION_ERROR", details: Optional[Any] = None):
        super().__init__(message, status_code, error_code, details)


class NetworkError(LegacyBaaSError):
    """Network and connection errors"""
    
    def __init__(self, message: str = "Network error", status_code: int = 500, error_code: str = "NETWORK_ERROR"):
        super().__init__(message, status_code, error_code)


class RateLimitError(LegacyBaaSError):
    """Rate limiting errors"""
    
    def __init__(self, message: str = "Rate limit exceeded", retry_after: Optional[int] = None):
        super().__init__(message, 429, "RATE_LIMIT_EXCEEDED")
        self.retry_after = retry_after


class ComplianceError(LegacyBaaSError):
    """Compliance and regulatory errors"""
    
    def __init__(self, message: str = "Compliance violation", status_code: int = 403, error_code: str = "COMPLIANCE_VIOLATION"):
        super().__init__(message, status_code, error_code)


class SwiftProcessingError(LegacyBaaSError):
    """SWIFT message processing errors"""
    
    def __init__(self, message: str = "SWIFT processing error", status_code: int = 422, error_code: str = "SWIFT_PROCESSING_ERROR"):
        super().__init__(message, status_code, error_code)


class BlockchainError(LegacyBaaSError):
    """Blockchain operation errors"""
    
    def __init__(self, message: str = "Blockchain error", status_code: int = 422, error_code: str = "BLOCKCHAIN_ERROR"):
        super().__init__(message, status_code, error_code)


class BankingError(LegacyBaaSError):
    """Banking operation errors"""
    
    def __init__(self, message: str = "Banking error", status_code: int = 422, error_code: str = "BANKING_ERROR"):
        super().__init__(message, status_code, error_code)


class ServiceUnavailableError(LegacyBaaSError):
    """Service unavailable errors"""
    
    def __init__(self, message: str = "Service unavailable", status_code: int = 503, error_code: str = "SERVICE_UNAVAILABLE"):
        super().__init__(message, status_code, error_code)