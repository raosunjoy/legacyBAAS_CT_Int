"""
Test suite for SWIFT service in Legacy B2BaaS Python SDK
"""

import pytest
import responses
from datetime import date, datetime
from decimal import Decimal
from legacybaas import LegacyBaaSClient
from legacybaas.exceptions import ValidationError


class TestSwiftProcessor:
    """Test cases for SWIFT message processing"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.api_key = "test_api_key_12345"
        self.base_url = "https://api.legacybaas.com"
        self.client = LegacyBaaSClient(self.api_key)
    
    @responses.activate
    def test_process_mt103_success(self):
        """Test successful MT103 processing"""
        mt103_data = {
            "transaction_reference": "TXN123456",
            "value_date": "2025-07-03",
            "currency": "USD",
            "amount": 10000.00,
            "ordering_customer": {
                "account": "123456789",
                "name": "John Doe",
                "address": "123 Main St"
            },
            "beneficiary_customer": {
                "account": "987654321",
                "name": "Jane Smith",
                "address": "456 Oak Ave"
            },
            "remittance_information": "Payment for services"
        }
        
        expected_response = {
            "transaction_id": "TXN_789012345",
            "status": "processed",
            "swift_reference": "FT25193123456",
            "processing_time": "2025-07-03T14:30:00Z",
            "fees": {
                "amount": 25.00,
                "currency": "USD"
            }
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/mt103",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.process_mt103(mt103_data)
        
        assert result.transaction_id == "TXN_789012345"
        assert result.status == "processed"
        assert result.swift_reference == "FT25193123456"
        assert result.fees["amount"] == 25.00
    
    @responses.activate
    def test_process_mt103_with_date_objects(self):
        """Test MT103 processing with date objects"""
        mt103_data = {
            "transaction_reference": "TXN123456",
            "value_date": date(2025, 7, 3),
            "currency": "EUR",
            "amount": Decimal("5000.50"),
            "ordering_customer": {
                "account": "123456789",
                "name": "John Doe"
            },
            "beneficiary_customer": {
                "account": "987654321",
                "name": "Jane Smith"
            }
        }
        
        expected_response = {
            "transaction_id": "TXN_789012346",
            "status": "processed",
            "swift_reference": "FT25193123457"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/mt103",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.process_mt103(mt103_data)
        assert result.transaction_id == "TXN_789012346"
    
    @responses.activate
    def test_process_mt103_validation_error(self):
        """Test MT103 validation error"""
        invalid_mt103 = {
            "transaction_reference": "",  # Invalid empty reference
            "currency": "USD",
            "amount": -100  # Invalid negative amount
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/mt103",
            json={
                "error": "Validation failed",
                "details": {
                    "transaction_reference": ["Transaction reference cannot be empty"],
                    "amount": ["Amount must be positive"]
                }
            },
            status=400
        )
        
        with pytest.raises(ValidationError) as exc_info:
            self.client.swift.process_mt103(invalid_mt103)
        
        assert "Validation failed" in str(exc_info.value)
    
    @responses.activate
    def test_process_mt202_success(self):
        """Test successful MT202 processing"""
        mt202_data = {
            "transaction_reference": "COV123456",
            "value_date": "2025-07-03",
            "currency": "USD",
            "amount": 50000.00,
            "ordering_institution": {
                "bic": "ABCDUS33XXX",
                "name": "ABC Bank"
            },
            "beneficiary_institution": {
                "bic": "XYZGB22XXX",
                "name": "XYZ Bank"
            },
            "related_reference": "TXN123456"
        }
        
        expected_response = {
            "transaction_id": "COV_789012345",
            "status": "processed",
            "swift_reference": "FT25193234567",
            "cover_payment": True
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/mt202",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.process_mt202(mt202_data)
        
        assert result.transaction_id == "COV_789012345"
        assert result.status == "processed"
        assert result.cover_payment is True
    
    @responses.activate
    def test_process_mt700_success(self):
        """Test successful MT700 processing"""
        mt700_data = {
            "documentary_credit_number": "LC123456789",
            "date_of_issue": "2025-07-03",
            "date_of_expiry": "2025-12-31",
            "place_of_expiry": "NEW YORK",
            "applicant": {
                "name": "ABC Corporation",
                "address": "123 Business Ave, New York, NY"
            },
            "beneficiary": {
                "name": "XYZ Limited",
                "address": "456 Trade St, London, UK"
            },
            "currency": "USD",
            "amount": 100000.00,
            "description_of_goods": "Electronic components",
            "documents_required": [
                "Commercial Invoice",
                "Packing List",
                "Bill of Lading"
            ]
        }
        
        expected_response = {
            "lc_id": "LC_789012345",
            "status": "issued",
            "swift_reference": "LC25193123456",
            "advisory_bank": "ADVBANK22XXX"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/mt700",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.process_mt700(mt700_data)
        
        assert result.lc_id == "LC_789012345"
        assert result.status == "issued"
        assert result.swift_reference == "LC25193123456"
    
    @responses.activate
    def test_get_transaction_status(self):
        """Test get transaction status"""
        transaction_id = "TXN_789012345"
        
        expected_response = {
            "transaction_id": transaction_id,
            "status": "completed",
            "swift_reference": "FT25193123456",
            "created_at": "2025-07-03T14:30:00Z",
            "completed_at": "2025-07-03T14:32:15Z",
            "settlement_status": "settled"
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/swift/transactions/{transaction_id}",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.get_transaction_status(transaction_id)
        
        assert result.transaction_id == transaction_id
        assert result.status == "completed"
        assert result.settlement_status == "settled"
    
    @responses.activate
    def test_list_transactions(self):
        """Test list transactions"""
        expected_response = {
            "transactions": [
                {
                    "transaction_id": "TXN_789012345",
                    "type": "MT103",
                    "status": "completed",
                    "amount": 10000.00,
                    "currency": "USD",
                    "created_at": "2025-07-03T14:30:00Z"
                },
                {
                    "transaction_id": "TXN_789012346",
                    "type": "MT202",
                    "status": "processing",
                    "amount": 50000.00,
                    "currency": "EUR",
                    "created_at": "2025-07-03T15:00:00Z"
                }
            ],
            "pagination": {
                "page": 1,
                "per_page": 10,
                "total": 2,
                "total_pages": 1
            }
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/swift/transactions",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.list_transactions()
        
        assert len(result.transactions) == 2
        assert result.transactions[0].transaction_id == "TXN_789012345"
        assert result.transactions[0].type == "MT103"
        assert result.pagination.total == 2
    
    @responses.activate
    def test_list_transactions_with_filters(self):
        """Test list transactions with filters"""
        filters = {
            "status": "completed",
            "type": "MT103",
            "from_date": "2025-07-01",
            "to_date": "2025-07-03",
            "page": 1,
            "per_page": 20
        }
        
        expected_response = {
            "transactions": [
                {
                    "transaction_id": "TXN_789012345",
                    "type": "MT103",
                    "status": "completed",
                    "amount": 10000.00,
                    "currency": "USD"
                }
            ],
            "pagination": {
                "page": 1,
                "per_page": 20,
                "total": 1,
                "total_pages": 1
            }
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/swift/transactions",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.list_transactions(**filters)
        
        assert len(result.transactions) == 1
        assert result.transactions[0].type == "MT103"
        assert result.transactions[0].status == "completed"
    
    @responses.activate
    def test_validate_swift_message(self):
        """Test SWIFT message validation"""
        mt103_data = {
            "transaction_reference": "TXN123456",
            "currency": "USD",
            "amount": 10000.00,
            "ordering_customer": {
                "account": "123456789",
                "name": "John Doe"
            },
            "beneficiary_customer": {
                "account": "987654321",
                "name": "Jane Smith"
            }
        }
        
        expected_response = {
            "valid": True,
            "message_type": "MT103",
            "compliance_status": "passed",
            "warnings": [],
            "validation_time": "2025-07-03T14:30:00Z"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/validate",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.validate_message("MT103", mt103_data)
        
        assert result.valid is True
        assert result.message_type == "MT103"
        assert result.compliance_status == "passed"
        assert len(result.warnings) == 0
    
    @responses.activate
    def test_validate_swift_message_with_warnings(self):
        """Test SWIFT message validation with warnings"""
        mt103_data = {
            "transaction_reference": "TXN123456",
            "currency": "USD",
            "amount": 10000.00,
            "ordering_customer": {
                "account": "123456789",
                "name": "John Doe"
            },
            "beneficiary_customer": {
                "account": "987654321",
                "name": "J"  # Very short name
            }
        }
        
        expected_response = {
            "valid": True,
            "message_type": "MT103",
            "compliance_status": "passed",
            "warnings": [
                "Beneficiary name is very short and may require additional verification"
            ],
            "validation_time": "2025-07-03T14:30:00Z"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/swift/validate",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.validate_message("MT103", mt103_data)
        
        assert result.valid is True
        assert len(result.warnings) == 1
        assert "short" in result.warnings[0]
    
    @responses.activate
    def test_get_swift_rates(self):
        """Test get SWIFT exchange rates"""
        expected_response = {
            "base_currency": "USD",
            "rates": {
                "EUR": 0.85,
                "GBP": 0.73,
                "JPY": 110.50,
                "CHF": 0.91
            },
            "timestamp": "2025-07-03T14:30:00Z",
            "provider": "ECB"
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/swift/rates",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.get_rates()
        
        assert result.base_currency == "USD"
        assert result.rates["EUR"] == 0.85
        assert result.rates["GBP"] == 0.73
        assert result.provider == "ECB"
    
    @responses.activate
    def test_get_swift_rates_with_currency(self):
        """Test get SWIFT rates for specific currency"""
        currency_pair = "EUR/USD"
        
        expected_response = {
            "from_currency": "EUR",
            "to_currency": "USD",
            "rate": 1.18,
            "timestamp": "2025-07-03T14:30:00Z",
            "bid": 1.179,
            "ask": 1.181
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/swift/rates/{currency_pair}",
            json=expected_response,
            status=200
        )
        
        result = self.client.swift.get_rate(currency_pair)
        
        assert result.from_currency == "EUR"
        assert result.to_currency == "USD"
        assert result.rate == 1.18
        assert result.bid == 1.179
        assert result.ask == 1.181


if __name__ == "__main__":
    pytest.main([__file__, "-v"])