"""
LegacyBAAS Python Client
Main client for all SDK operations
"""

import time
import json
from typing import Dict, Any, Optional, Union
from urllib.parse import urljoin
import requests
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

from .errors import (
    LegacyBaaSError,
    AuthenticationError,
    ValidationError,
    NetworkError,
    RateLimitError
)
from .services.swift_processor import SwiftProcessor
from .services.blockchain_router import BlockchainRouter
from .services.banking_service import BankingService
from .services.analytics_service import AnalyticsService
from .services.webhook_handler import WebhookHandler
from .services.compliance_service import ComplianceService
from .services.cobol_service import CobolTranspilerService
from .utils import Logger


class LegacyBaaSClient:
    """
    Main LegacyBAAS client for Python SDK
    
    Args:
        client_id: OAuth2 client ID
        client_secret: OAuth2 client secret
        base_url: API base URL
        environment: Environment (production, sandbox, development)
        timeout: Request timeout in seconds
        max_retries: Maximum retry attempts
        retry_delay: Delay between retries in seconds
        enable_logging: Enable request logging
        log_level: Logging level (DEBUG, INFO, WARN, ERROR)
    """
    
    def __init__(
        self,
        client_id: str,
        client_secret: str,
        base_url: Optional[str] = None,
        environment: str = "production",
        timeout: float = 30.0,
        max_retries: int = 3,
        retry_delay: float = 1.0,
        enable_logging: bool = True,
        log_level: str = "INFO"
    ):
        self.client_id = client_id
        self.client_secret = client_secret
        self.environment = environment
        self.timeout = timeout
        self.max_retries = max_retries
        self.retry_delay = retry_delay
        
        # Set base URL based on environment
        if base_url:
            self.base_url = base_url
        else:
            self.base_url = self._get_base_url(environment)
        
        # Initialize logger
        self.logger = Logger(log_level) if enable_logging else None
        
        # Authentication state
        self.access_token: Optional[str] = None
        self.token_expiry: Optional[float] = None
        
        # Initialize HTTP session with retry strategy
        self.session = requests.Session()
        retry_strategy = Retry(
            total=max_retries,
            backoff_factor=retry_delay,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["HEAD", "GET", "PUT", "DELETE", "OPTIONS", "TRACE", "POST"]
        )
        adapter = HTTPAdapter(max_retries=retry_strategy)
        self.session.mount("http://", adapter)
        self.session.mount("https://", adapter)
        
        # Initialize services
        self.swift = SwiftProcessor(self)
        self.blockchain = BlockchainRouter(self)
        self.banking = BankingService(self)
        self.analytics = AnalyticsService(self)
        self.webhooks = WebhookHandler(self)
        self.compliance = ComplianceService(self)
        self.cobol = CobolTranspilerService(self)
        
        if self.logger:
            self.logger.info(f"LegacyBAAS Client initialized - Environment: {environment}, Base URL: {self.base_url}")
    
    def _get_base_url(self, environment: str) -> str:
        """Get base URL for environment"""
        urls = {
            "production": "https://api.legacybaas.com/v1",
            "sandbox": "https://sandbox.legacybaas.com/v1", 
            "development": "http://localhost:3000/v1"
        }
        return urls.get(environment, urls["production"])
    
    def authenticate(self) -> Dict[str, Any]:
        """
        Authenticate with OAuth2 client credentials flow
        
        Returns:
            Authentication response with token info
        """
        if not self.client_id or not self.client_secret:
            raise AuthenticationError("Client ID and Client Secret are required for authentication")
        
        try:
            response = self._make_request(
                "POST",
                "/auth/token",
                data={
                    "grant_type": "client_credentials",
                    "client_id": self.client_id,
                    "client_secret": self.client_secret,
                    "scope": "banking:read banking:write blockchain:execute compliance:screen"
                },
                skip_auth=True
            )
            
            self.access_token = response["access_token"]
            self.token_expiry = time.time() + response["expires_in"]
            
            if self.logger:
                self.logger.info("Authentication successful")
            
            return response
            
        except Exception as e:
            if self.logger:
                self.logger.error(f"Authentication failed: {e}")
            raise AuthenticationError("Failed to authenticate with LegacyBAAS API")
    
    def is_authenticated(self) -> bool:
        """Check if client has valid authentication token"""
        return bool(
            self.access_token and 
            self.token_expiry and 
            time.time() < self.token_expiry
        )
    
    def ensure_authenticated(self) -> None:
        """Ensure client is authenticated, refresh token if necessary"""
        if not self.is_authenticated():
            self.authenticate()
    
    def _make_request(
        self,
        method: str,
        endpoint: str,
        data: Optional[Dict[str, Any]] = None,
        params: Optional[Dict[str, Any]] = None,
        headers: Optional[Dict[str, str]] = None,
        skip_auth: bool = False
    ) -> Dict[str, Any]:
        """
        Make HTTP request to API
        
        Args:
            method: HTTP method
            endpoint: API endpoint
            data: Request body data
            params: Query parameters
            headers: Additional headers
            skip_auth: Skip authentication
            
        Returns:
            API response data
        """
        if not skip_auth:
            self.ensure_authenticated()
        
        url = urljoin(self.base_url, endpoint)
        
        # Prepare headers
        request_headers = {
            "Content-Type": "application/json",
            "User-Agent": "LegacyBAAS-Python-SDK/2.0.0",
            "Accept": "application/json"
        }
        
        if self.access_token and not skip_auth:
            request_headers["Authorization"] = f"Bearer {self.access_token}"
        
        if headers:
            request_headers.update(headers)
        
        # Prepare request data
        request_kwargs = {
            "timeout": self.timeout,
            "headers": request_headers
        }
        
        if params:
            request_kwargs["params"] = params
        
        if data and method in ["POST", "PUT", "PATCH"]:
            request_kwargs["json"] = data
        
        try:
            if self.logger:
                self.logger.debug(f"Making {method} request to {endpoint}")
            
            response = self.session.request(method, url, **request_kwargs)
            
            # Handle HTTP errors
            if not response.ok:
                try:
                    error_data = response.json()
                except:
                    error_data = {"message": response.text or f"HTTP {response.status_code}"}
                
                if response.status_code == 401:
                    raise AuthenticationError(error_data.get("message", "Authentication failed"))
                elif response.status_code == 400:
                    raise ValidationError(
                        error_data.get("message", "Validation error"),
                        details=error_data.get("details")
                    )
                elif response.status_code == 429:
                    retry_after = response.headers.get("Retry-After")
                    raise RateLimitError(
                        error_data.get("message", "Rate limit exceeded"),
                        retry_after=int(retry_after) if retry_after else None
                    )
                elif response.status_code >= 500:
                    raise NetworkError(error_data.get("message", "Server error"))
                else:
                    raise LegacyBaaSError(
                        error_data.get("message", f"HTTP {response.status_code}"),
                        status_code=response.status_code,
                        error_code=error_data.get("code")
                    )
            
            # Parse response
            try:
                response_data = response.json()
            except:
                response_data = {"data": response.text}
            
            if self.logger:
                self.logger.debug(f"Request completed: {method} {endpoint}")
            
            return response_data.get("data", response_data)
            
        except requests.exceptions.Timeout:
            raise NetworkError("Request timeout")
        except requests.exceptions.ConnectionError:
            raise NetworkError("Connection error")
        except requests.exceptions.RequestException as e:
            raise NetworkError(f"Request failed: {e}")
    
    def get_health(self) -> Dict[str, Any]:
        """
        Get API health status
        
        Returns:
            Health status information
        """
        return self._make_request("GET", "/health")
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get platform metrics
        
        Returns:
            Platform metrics
        """
        return self._make_request("GET", "/admin/metrics")
    
    def close(self) -> None:
        """Close client and cleanup resources"""
        if self.session:
            self.session.close()
        self.access_token = None
        self.token_expiry = None
        
        if self.logger:
            self.logger.info("LegacyBAAS Client closed")
    
    def __enter__(self):
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()