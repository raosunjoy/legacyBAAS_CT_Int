"""
Common type definitions for Legacy B2BaaS Python SDK
"""

from typing import Optional, Dict, Any, List
from dataclasses import dataclass


@dataclass
class ApiResponse:
    """Standard API response"""
    data: Any
    status: str
    message: Optional[str] = None


@dataclass
class HealthStatus:
    """Health status response"""
    status: str
    version: str
    timestamp: str
    components: Optional[Dict[str, str]] = None


@dataclass
class PaginationOptions:
    """Pagination options"""
    page: int = 1
    per_page: int = 10
    total: Optional[int] = None
    total_pages: Optional[int] = None