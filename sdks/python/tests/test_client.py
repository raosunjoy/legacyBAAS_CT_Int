"""
Test suite for Legacy B2BaaS Python SDK client
"""

import pytest
import requests
import responses
from unittest.mock import Mock, patch
from legacybaas import LegacyBaaSClient, AsyncLegacyBaaSClient
from legacybaas.config import Environment
from legacybaas.exceptions import (
    AuthenticationError,
    ValidationError,
    NetworkError,
    RateLimitError,
    ServiceUnavailableError,
)


class TestLegacyBaaSClient:
    """Test cases for synchronous client"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.api_key = "test_api_key_12345678901234567890"  # Valid length
        self.base_url = "https://api.legacybaas.com"
    
    def test_client_initialization_success(self):
        """Test successful client initialization"""
        client = LegacyBaaSClient(self.api_key)
        
        assert client.api_key == self.api_key
        assert client.config.base_url == self.base_url
        assert hasattr(client, 'swift')
        assert hasattr(client, 'blockchain')
        assert hasattr(client, 'bancs')
        assert hasattr(client, 'analytics')
        assert hasattr(client, 'webhooks')
    
    def test_client_initialization_invalid_api_key(self):
        """Test client initialization with invalid API key"""
        with pytest.raises(AuthenticationError):
            LegacyBaaSClient("invalid_key")
    
    def test_client_initialization_empty_api_key(self):
        """Test client initialization with empty API key"""
        with pytest.raises(AuthenticationError):
            LegacyBaaSClient("")
    
    def test_client_with_custom_config(self):
        """Test client with custom configuration"""
        client = LegacyBaaSClient(
            self.api_key,
            environment=Environment.SANDBOX,
            timeout=60,
            max_retries=5
        )
        
        assert client.config.timeout == 60
        assert client.config.max_retries == 5
    
    @responses.activate
    def test_health_endpoint_success(self):
        """Test health endpoint success"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/health",
            json={
                "status": "healthy",
                "version": "1.0.0",
                "timestamp": "2025-07-03T12:00:00Z"
            },
            status=200
        )
        
        client = LegacyBaaSClient(self.api_key)
        health = client.health()
        
        assert health.status == "healthy"
        assert health.version == "1.0.0"
    
    @responses.activate
    def test_ping_endpoint_success(self):
        """Test ping endpoint success"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/ping",
            json={
                "message": "pong",
                "timestamp": "2025-07-03T12:00:00Z"
            },
            status=200
        )
        
        client = LegacyBaaSClient(self.api_key)
        result = client.ping()
        
        assert result["message"] == "pong"
    
    @responses.activate
    def test_authentication_error_handling(self):
        """Test authentication error handling"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/ping",
            json={"error": "Unauthorized"},
            status=401
        )
        
        client = LegacyBaaSClient(self.api_key)
        
        with pytest.raises(AuthenticationError):
            client.ping()
    
    @responses.activate
    def test_validation_error_handling(self):
        """Test validation error handling"""
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/mt103",
            json={
                "error": "Validation failed",
                "details": {"amount": ["Amount must be positive"]}
            },
            status=400
        )
        
        client = LegacyBaaSClient(self.api_key)
        
        with pytest.raises(ValidationError):
            client.post("/swift/mt103", {"amount": -100})
    
    @responses.activate
    def test_rate_limit_error_handling(self):
        """Test rate limit error handling"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/ping",
            headers={"Retry-After": "60"},
            status=429
        )
        
        client = LegacyBaaSClient(self.api_key, max_retries=0)  # Disable retries
        
        with pytest.raises(RateLimitError):
            client.ping()
    
    @responses.activate
    def test_service_unavailable_error_handling(self):
        """Test service unavailable error handling"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/health",
            json={"error": "Service unavailable"},
            status=503
        )
        
        client = LegacyBaaSClient(self.api_key, max_retries=0)  # Disable retries
        
        with pytest.raises(ServiceUnavailableError):
            client.health()
    
    def test_context_manager(self):
        """Test client as context manager"""
        with LegacyBaaSClient(self.api_key) as client:
            assert client.api_key == self.api_key
        
        # Session should be closed after context exit
        assert client.session is None or len(client.session.adapters) == 0
    
    @responses.activate
    def test_get_account_endpoint(self):
        """Test get account endpoint"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/account",
            json={
                "id": "123456",
                "name": "Test Account",
                "plan": "enterprise"
            },
            status=200
        )
        
        client = LegacyBaaSClient(self.api_key)
        account = client.get_account()
        
        assert account["id"] == "123456"
        assert account["name"] == "Test Account"
        assert account["plan"] == "enterprise"
    
    @responses.activate
    def test_get_usage_endpoint(self):
        """Test get usage endpoint"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/usage",
            json={
                "period": "day",
                "requests": 1250,
                "quota": 10000
            },
            status=200
        )
        
        client = LegacyBaaSClient(self.api_key)
        usage = client.get_usage()
        
        assert usage["period"] == "day"
        assert usage["requests"] == 1250
        assert usage["quota"] == 10000
    
    @responses.activate
    def test_get_usage_with_period(self):
        """Test get usage with custom period"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/usage",
            json={
                "period": "month",
                "requests": 45000,
                "quota": 100000
            },
            status=200
        )
        
        client = LegacyBaaSClient(self.api_key)
        usage = client.get_usage("month")
        
        assert usage["period"] == "month"
        assert usage["requests"] == 45000
    
    def test_request_timeout_error(self):
        """Test request timeout error handling"""
        with patch('requests.Session.request') as mock_request:
            mock_request.side_effect = requests.exceptions.Timeout()
            
            client = LegacyBaaSClient(self.api_key)
            
            with pytest.raises(NetworkError, match="Request timeout"):
                client.ping()
    
    def test_connection_error_handling(self):
        """Test connection error handling"""
        with patch('requests.Session.request') as mock_request:
            mock_request.side_effect = requests.exceptions.ConnectionError()
            
            client = LegacyBaaSClient(self.api_key)
            
            with pytest.raises(NetworkError, match="Connection error"):
                client.ping()
    
    def test_retry_configuration(self):
        """Test retry mechanism configuration"""
        client = LegacyBaaSClient(self.api_key, max_retries=5)
        
        # Check that retry adapter is configured
        adapter = client.session.get_adapter("https://")
        assert adapter.max_retries.total == 5
    
    @responses.activate
    def test_custom_headers(self):
        """Test custom headers in requests"""
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/ping",
            json={"message": "pong"},
            status=200
        )
        
        client = LegacyBaaSClient(self.api_key)
        client.get("/ping", headers={"X-Custom-Header": "test-value"})
        
        # Verify custom header was sent
        request = responses.calls[0].request
        assert request.headers["X-Custom-Header"] == "test-value"
    
    def test_url_building(self):
        """Test URL building functionality"""
        client = LegacyBaaSClient(self.api_key)
        
        # Test basic endpoint
        url = client._build_url("/health")
        assert url == f"{self.base_url}/api/v1/health"
        
        # Test endpoint with leading slash
        url = client._build_url("health")
        assert url == f"{self.base_url}/api/v1/health"
        
        # Test nested endpoint
        url = client._build_url("/swift/mt103")
        assert url == f"{self.base_url}/api/v1/swift/mt103"


@pytest.mark.asyncio
class TestAsyncLegacyBaaSClient:
    """Test cases for asynchronous client"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.api_key = "test_api_key_12345"
    
    async def test_async_client_initialization(self):
        """Test async client initialization"""
        async with AsyncLegacyBaaSClient(self.api_key) as client:
            assert client.api_key == self.api_key
            assert hasattr(client, 'swift')
            assert hasattr(client, 'blockchain')
    
    async def test_async_context_manager(self):
        """Test async client as context manager"""
        client = AsyncLegacyBaaSClient(self.api_key)
        
        async with client:
            assert client.api_key == self.api_key
        
        # Client should be closed after context exit


class TestClientConfiguration:
    """Test client configuration options"""
    
    def test_environment_configuration(self):
        """Test different environment configurations"""
        valid_key = "test_api_key_12345678901234567890"
        
        # Production environment
        client = LegacyBaaSClient(valid_key, environment=Environment.PRODUCTION)
        assert "api.legacybaas.com" in client.config.base_url
        
        # Staging environment
        client = LegacyBaaSClient(valid_key, environment=Environment.STAGING)
        assert "staging-api.legacybaas.com" in client.config.base_url
        
        # Sandbox environment
        client = LegacyBaaSClient(valid_key, environment=Environment.SANDBOX)
        assert "sandbox-api.legacybaas.com" in client.config.base_url
    
    def test_custom_timeout_configuration(self):
        """Test custom timeout configuration"""
        valid_key = "test_api_key_12345678901234567890"
        client = LegacyBaaSClient(valid_key, timeout=60)
        assert client.config.timeout == 60
    
    def test_logging_configuration(self):
        """Test logging configuration"""
        valid_key = "test_api_key_12345678901234567890"
        client = LegacyBaaSClient(valid_key, enable_logging=False)
        assert client.config.enable_logging is False
    
    def test_retry_configuration(self):
        """Test retry configuration"""
        valid_key = "test_api_key_12345678901234567890"
        client = LegacyBaaSClient(valid_key, max_retries=5, retry_delay=2.0)
        assert client.config.max_retries == 5
        assert client.config.retry_delay == 2.0


if __name__ == "__main__":
    pytest.main([__file__, "-v"])