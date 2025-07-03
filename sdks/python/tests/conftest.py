"""
Pytest configuration and shared fixtures for Legacy B2BaaS Python SDK tests
"""

import pytest
import os
from unittest.mock import Mock, patch
from legacybaas import LegacyBaaSClient, AsyncLegacyBaaSClient
from legacybaas.config import Config, Environment


@pytest.fixture
def mock_api_key():
    """Mock API key for testing"""
    return "test_api_key_12345678901234567890"


@pytest.fixture
def mock_base_url():
    """Mock base URL for testing"""
    return "https://api.legacybaas.com"


@pytest.fixture
def test_config(mock_base_url):
    """Test configuration"""
    return Config(
        base_url=mock_base_url,
        version="v1",
        timeout=30.0,
        max_retries=3,
        retry_delay=1.0,
        enable_logging=False,  # Disable logging in tests
        log_level="ERROR"
    )


@pytest.fixture
def client(mock_api_key, test_config):
    """Legacy B2BaaS client instance for testing"""
    return LegacyBaaSClient(mock_api_key, config=test_config)


@pytest.fixture
async def async_client(mock_api_key, test_config):
    """Async Legacy B2BaaS client instance for testing"""
    client = AsyncLegacyBaaSClient(mock_api_key, config=test_config)
    yield client
    await client.close()


@pytest.fixture
def mock_responses():
    """Mock HTTP responses using responses library"""
    import responses
    with responses.RequestsMock() as rsps:
        yield rsps


@pytest.fixture
def sample_mt103():
    """Sample MT103 message data"""
    return {
        "transaction_reference": "TXN123456789",
        "value_date": "2025-07-03",
        "currency": "USD",
        "amount": 10000.00,
        "ordering_customer": {
            "account": "123456789",
            "name": "John Doe",
            "address": "123 Main Street, New York, NY 10001"
        },
        "beneficiary_customer": {
            "account": "987654321",
            "name": "Jane Smith",
            "address": "456 Oak Avenue, Los Angeles, CA 90210"
        },
        "ordering_institution": {
            "bic": "ABCDUS33XXX",
            "name": "ABC Bank"
        },
        "beneficiary_institution": {
            "bic": "XYZGB22XXX",
            "name": "XYZ Bank"
        },
        "remittance_information": "Payment for consulting services"
    }


@pytest.fixture
def sample_mt202():
    """Sample MT202 cover payment data"""
    return {
        "transaction_reference": "COV123456789",
        "value_date": "2025-07-03",
        "currency": "USD",
        "amount": 50000.00,
        "ordering_institution": {
            "bic": "ABCDUS33XXX",
            "name": "ABC Bank",
            "account": "123456789"
        },
        "beneficiary_institution": {
            "bic": "XYZGB22XXX",
            "name": "XYZ Bank",
            "account": "987654321"
        },
        "related_reference": "TXN123456789",
        "intermediary_institution": {
            "bic": "INTMDE33XXX",
            "name": "Intermediary Bank"
        }
    }


@pytest.fixture
def sample_mt700():
    """Sample MT700 documentary credit data"""
    return {
        "documentary_credit_number": "LC123456789",
        "date_of_issue": "2025-07-03",
        "date_of_expiry": "2025-12-31",
        "place_of_expiry": "NEW YORK",
        "applicant": {
            "name": "ABC Corporation",
            "address": "123 Business Avenue, New York, NY 10001"
        },
        "beneficiary": {
            "name": "XYZ Limited",
            "address": "456 Trade Street, London, UK EC1A 1BB"
        },
        "currency": "USD",
        "amount": 100000.00,
        "percentage_credit_amount_tolerance": {"plus": 10, "minus": 5},
        "latest_shipment_date": "2025-11-30",
        "description_of_goods": "High-quality electronic components and accessories",
        "documents_required": [
            "Commercial Invoice in triplicate",
            "Packing List",
            "Clean On Board Bill of Lading",
            "Certificate of Origin",
            "Insurance Certificate"
        ],
        "additional_conditions": "Partial shipments allowed. Transshipment prohibited.",
        "charges": "all charges outside beneficiary country for account of applicant"
    }


@pytest.fixture
def sample_blockchain_transaction():
    """Sample blockchain transaction data"""
    return {
        "amount": 1000.00,
        "currency": "USD",
        "from_address": "0x1234567890abcdef1234567890abcdef12345678",
        "to_address": "0xabcdef1234567890abcdef1234567890abcdef12",
        "network": "ethereum",
        "priority": "standard",
        "memo": "Test blockchain transaction"
    }


@pytest.fixture
def sample_bancs_transaction():
    """Sample BaNCS transaction data"""
    return {
        "account_number": "1234567890",
        "transaction_type": "credit",
        "amount": 5000.00,
        "currency": "USD",
        "description": "Salary payment",
        "reference": "SAL202507030001",
        "beneficiary_account": "0987654321",
        "value_date": "2025-07-03"
    }


@pytest.fixture(autouse=True)
def clean_environment():
    """Clean environment variables before each test"""
    env_vars_to_clean = [
        "LEGACYBAAS_API_KEY",
        "LEGACYBAAS_BASE_URL",
        "LEGACYBAAS_ENVIRONMENT",
        "LEGACYBAAS_TIMEOUT",
        "LEGACYBAAS_MAX_RETRIES"
    ]
    
    original_values = {}
    for var in env_vars_to_clean:
        original_values[var] = os.environ.get(var)
        if var in os.environ:
            del os.environ[var]
    
    yield
    
    # Restore original values
    for var, value in original_values.items():
        if value is not None:
            os.environ[var] = value


@pytest.fixture
def mock_session():
    """Mock requests session"""
    with patch('requests.Session') as mock_session_class:
        mock_session = Mock()
        mock_session_class.return_value = mock_session
        yield mock_session


@pytest.fixture
def mock_aiohttp_session():
    """Mock aiohttp session for async tests"""
    with patch('aiohttp.ClientSession') as mock_session_class:
        mock_session = Mock()
        mock_session_class.return_value = mock_session
        yield mock_session


@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    import asyncio
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


# Test data constants
TEST_TRANSACTION_ID = "TXN_123456789012345"
TEST_SWIFT_REFERENCE = "FT25193123456"
TEST_BLOCKCHAIN_HASH = "0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef"
TEST_WALLET_ADDRESS = "0x1234567890abcdef1234567890abcdef12345678"

# Mock response templates
MOCK_HEALTH_RESPONSE = {
    "status": "healthy",
    "version": "1.0.0",
    "timestamp": "2025-07-03T14:30:00Z",
    "components": {
        "database": "healthy",
        "swift": "healthy",
        "blockchain": "healthy",
        "bancs": "healthy"
    }
}

MOCK_PING_RESPONSE = {
    "message": "pong",
    "timestamp": "2025-07-03T14:30:00Z",
    "latency": 45
}

MOCK_ACCOUNT_RESPONSE = {
    "id": "ACC_123456789",
    "name": "Test Account",
    "plan": "enterprise",
    "status": "active",
    "created_at": "2025-01-01T00:00:00Z",
    "features": [
        "swift_processing",
        "blockchain_routing",
        "bancs_integration",
        "real_time_analytics"
    ]
}

# Test environment configurations
TEST_ENVIRONMENTS = {
    Environment.PRODUCTION: "https://api.legacybaas.com",
    Environment.STAGING: "https://staging-api.legacybaas.com",
    Environment.SANDBOX: "https://sandbox-api.legacybaas.com"
}


def pytest_configure(config):
    """Configure pytest with custom markers"""
    config.addinivalue_line(
        "markers", "integration: mark test as integration test"
    )
    config.addinivalue_line(
        "markers", "unit: mark test as unit test"
    )
    config.addinivalue_line(
        "markers", "async_test: mark test as async test"
    )
    config.addinivalue_line(
        "markers", "slow: mark test as slow running"
    )


# Skip integration tests by default
def pytest_collection_modifyitems(config, items):
    """Modify test collection to skip integration tests by default"""
    if config.getoption("--integration"):
        return
    
    skip_integration = pytest.mark.skip(reason="need --integration option to run")
    for item in items:
        if "integration" in item.keywords:
            item.add_marker(skip_integration)


def pytest_addoption(parser):
    """Add custom command line options"""
    parser.addoption(
        "--integration",
        action="store_true",
        default=False,
        help="run integration tests"
    )