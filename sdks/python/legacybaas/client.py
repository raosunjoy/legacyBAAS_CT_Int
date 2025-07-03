"""
Main Legacy B2BaaS Client for Python
Synchronous client implementation
"""

import logging
import time
from typing import Optional, Dict, Any, Union
from urllib.parse import urljoin

import requests
from requests.adapters import HTTPAdapter
from requests.packages.urllib3.util.retry import Retry

from .config import Config, Environment
from .exceptions import (
    LegacyBaaSError,
    AuthenticationError,
    ValidationError,
    NetworkError,
    RateLimitError,
    ServiceUnavailableError,
)
from .services.swift import SwiftProcessor
from .services.blockchain import BlockchainRouter
from .services.bancs import BancsIntegration
from .services.analytics import AnalyticsService
from .services.webhooks import WebhookHandler
from .types.common import ApiResponse, HealthStatus
from .utils import validate_api_key


class LegacyBaaSClient:
    """
    Main Legacy B2BaaS Platform Client
    
    Provides access to all platform services including SWIFT processing,
    blockchain routing, BaNCS integration, and analytics.
    
    Args:
        api_key: Your Legacy B2BaaS API key
        config: Optional configuration override
        environment: Target environment (production, staging, sandbox)
        
    Example:
        >>> client = LegacyBaaSClient(api_key="your-api-key")
        >>> health = client.health()
        >>> print(f"Platform status: {health.status}")
    """
    
    def __init__(
        self,
        api_key: str,
        config: Optional[Config] = None,
        environment: Environment = Environment.PRODUCTION,
        **kwargs
    ):
        # Validate API key
        if not validate_api_key(api_key):
            raise AuthenticationError("Invalid API key format")
            
        self.api_key = api_key
        self.config = config or Config.for_environment(environment)
        
        # Override config with kwargs
        for key, value in kwargs.items():
            if hasattr(self.config, key):
                setattr(self.config, key, value)
        
        # Set up logging
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        if self.config.enable_logging:
            logging.basicConfig(level=getattr(logging, self.config.log_level))
        
        # Initialize HTTP session
        self.session = self._create_session()
        
        # Initialize services
        self.swift = SwiftProcessor(self)
        self.blockchain = BlockchainRouter(self)
        self.bancs = BancsIntegration(self)
        self.analytics = AnalyticsService(self)
        self.webhooks = WebhookHandler(self)
        
        self.logger.info(f"Legacy B2BaaS Client initialized for {environment}")
    
    def _create_session(self) -> requests.Session:
        """Create configured HTTP session with retries and headers"""
        session = requests.Session()
        
        # Set default headers
        session.headers.update({
            'Authorization': f'Bearer {self.api_key}',
            'Content-Type': 'application/json',
            'Accept': 'application/json',
            'User-Agent': f'LegacyBaaS-Python-SDK/1.0.0',
            'X-SDK-Version': '1.0.0',
            'X-SDK-Language': 'python',
        })
        
        # Configure retries
        retry_strategy = Retry(
            total=self.config.max_retries,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["HEAD", "GET", "OPTIONS"],
            backoff_factor=self.config.retry_delay,
        )
        
        adapter = HTTPAdapter(max_retries=retry_strategy)
        session.mount("http://", adapter)
        session.mount("https://", adapter)
        
        return session
    
    def _build_url(self, endpoint: str) -> str:
        """Build full API URL"""
        base_api_url = f"{self.config.base_url}/api/{self.config.version}"
        return urljoin(base_api_url + "/", endpoint.lstrip("/"))
    
    def _handle_response(self, response: requests.Response) -> Dict[str, Any]:
        """Handle HTTP response and extract data"""
        try:
            # Log request/response if debug enabled
            if self.config.log_level == "DEBUG":
                self.logger.debug(f"Request: {response.request.method} {response.request.url}")
                self.logger.debug(f"Response: {response.status_code} {response.text[:500]}")
            
            # Handle rate limiting
            if response.status_code == 429:
                retry_after = int(response.headers.get('Retry-After', 60))
                raise RateLimitError(f"Rate limit exceeded. Retry after {retry_after} seconds")
            
            # Handle authentication errors
            if response.status_code == 401:
                raise AuthenticationError("Invalid API key or authentication failed")
            
            # Handle validation errors
            if response.status_code == 400:
                error_data = response.json() if response.content else {}
                raise ValidationError(
                    error_data.get('message', 'Validation error'),
                    details=error_data.get('details')
                )
            
            # Handle service unavailable
            if response.status_code == 503:
                raise ServiceUnavailableError("Service temporarily unavailable")
            
            # Handle other client/server errors
            if response.status_code >= 400:
                error_data = response.json() if response.content else {}
                raise LegacyBaaSError(
                    error_data.get('message', f'HTTP {response.status_code}'),
                    status_code=response.status_code,
                    error_code=error_data.get('code')
                )
            
            # Parse successful response
            if response.content:
                data = response.json()
                if isinstance(data, dict) and 'data' in data:
                    return data['data']
                return data
            
            return {}
            
        except requests.exceptions.JSONDecodeError:
            raise LegacyBaaSError(f"Invalid JSON response: {response.text}")
        except requests.exceptions.RequestException as e:
            raise NetworkError(f"Network error: {str(e)}")
    
    def request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None,
        timeout: Optional[float] = None,
    ) -> Dict[str, Any]:
        """
        Make HTTP request to API endpoint
        
        Args:
            method: HTTP method (GET, POST, PUT, DELETE)
            endpoint: API endpoint path
            data: Request body data
            params: Query parameters
            headers: Additional headers
            timeout: Request timeout override
            
        Returns:
            Response data
            
        Raises:
            LegacyBaaSError: API or network error
        """
        url = self._build_url(endpoint)
        timeout = timeout or self.config.timeout
        
        request_headers = {}
        if headers:
            request_headers.update(headers)
        
        try:
            response = self.session.request(
                method=method,
                url=url,
                json=data,
                params=params,
                headers=request_headers,
                timeout=timeout,
            )
            
            return self._handle_response(response)
            
        except requests.exceptions.Timeout:
            raise NetworkError(f"Request timeout after {timeout} seconds")
        except requests.exceptions.ConnectionError:
            raise NetworkError("Connection error - check network connectivity")
        except requests.exceptions.RequestException as e:
            raise NetworkError(f"Request error: {str(e)}")
    
    def get(self, endpoint: str, params: Optional[Dict[str, Any]] = None, **kwargs) -> Dict[str, Any]:
        """Make GET request"""
        return self.request("GET", endpoint, params=params, **kwargs)
    
    def post(self, endpoint: str, data: Optional[Dict[str, Any]] = None, **kwargs) -> Dict[str, Any]:
        """Make POST request"""
        return self.request("POST", endpoint, data=data, **kwargs)
    
    def put(self, endpoint: str, data: Optional[Dict[str, Any]] = None, **kwargs) -> Dict[str, Any]:
        """Make PUT request"""
        return self.request("PUT", endpoint, data=data, **kwargs)
    
    def delete(self, endpoint: str, **kwargs) -> Dict[str, Any]:
        """Make DELETE request"""
        return self.request("DELETE", endpoint, **kwargs)
    
    def health(self) -> HealthStatus:
        """
        Check platform health status
        
        Returns:
            Health status information
        """
        data = self.get("/health")
        return HealthStatus(**data)
    
    def ping(self) -> Dict[str, Any]:
        """
        Test API connectivity
        
        Returns:
            Ping response with timestamp
        """
        return self.get("/ping")
    
    def get_account(self) -> Dict[str, Any]:
        """
        Get account information
        
        Returns:
            Account details
        """
        return self.get("/account")
    
    def get_usage(self, period: str = "day") -> Dict[str, Any]:
        """
        Get API usage statistics
        
        Args:
            period: Time period ('day', 'week', 'month')
            
        Returns:
            Usage statistics
        """
        return self.get("/usage", params={"period": period})
    
    def close(self):
        """Close HTTP session and cleanup resources"""
        if self.session:
            self.session.close()
            self.session = None
        self.logger.info("Legacy B2BaaS Client closed")
    
    def __enter__(self):
        """Context manager entry"""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.close()
    
    def __repr__(self):
        return f"LegacyBaaSClient(config={self.config})"