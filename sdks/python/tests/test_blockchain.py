"""
Test suite for Blockchain service in Legacy B2BaaS Python SDK
"""

import pytest
import responses
from legacybaas import LegacyBaaSClient
from legacybaas.exceptions import ValidationError


class TestBlockchainRouter:
    """Test cases for blockchain routing and transactions"""
    
    def setup_method(self):
        """Setup test fixtures"""
        self.api_key = "test_api_key_12345"
        self.base_url = "https://api.legacybaas.com"
        self.client = LegacyBaaSClient(self.api_key)
    
    @responses.activate
    def test_route_transaction_success(self):
        """Test successful transaction routing"""
        transaction_data = {
            "amount": 1000.00,
            "currency": "USD",
            "from_address": "0x123...abc",
            "to_address": "0x456...def",
            "network": "ethereum",
            "priority": "high"
        }
        
        expected_response = {
            "transaction_id": "TXN_BL_789012345",
            "blockchain_hash": "0x789...012",
            "network": "ethereum",
            "status": "pending",
            "gas_fee": 0.001,
            "estimated_confirmation": "2-5 minutes"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/blockchain/route",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.route_transaction(transaction_data)
        
        assert result.transaction_id == "TXN_BL_789012345"
        assert result.blockchain_hash == "0x789...012"
        assert result.network == "ethereum"
        assert result.status == "pending"
    
    @responses.activate
    def test_route_transaction_validation_error(self):
        """Test transaction routing validation error"""
        invalid_transaction = {
            "amount": -100,  # Invalid negative amount
            "currency": "INVALID",  # Invalid currency
            "from_address": "",  # Empty address
            "to_address": "invalid_address"  # Invalid format
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/blockchain/route",
            json={
                "error": "Validation failed",
                "details": {
                    "amount": ["Amount must be positive"],
                    "currency": ["Invalid currency code"],
                    "from_address": ["Address cannot be empty"],
                    "to_address": ["Invalid address format"]
                }
            },
            status=400
        )
        
        with pytest.raises(ValidationError):
            self.client.blockchain.route_transaction(invalid_transaction)
    
    @responses.activate
    def test_get_transaction_status(self):
        """Test get blockchain transaction status"""
        transaction_id = "TXN_BL_789012345"
        
        expected_response = {
            "transaction_id": transaction_id,
            "blockchain_hash": "0x789...012",
            "network": "ethereum",
            "status": "confirmed",
            "confirmations": 12,
            "block_number": 18234567,
            "gas_used": 21000,
            "gas_price": "0.000000020",
            "created_at": "2025-07-03T14:30:00Z",
            "confirmed_at": "2025-07-03T14:32:15Z"
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/blockchain/transactions/{transaction_id}",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.get_transaction_status(transaction_id)
        
        assert result.transaction_id == transaction_id
        assert result.status == "confirmed"
        assert result.confirmations == 12
        assert result.block_number == 18234567
    
    @responses.activate
    def test_list_supported_networks(self):
        """Test list supported blockchain networks"""
        expected_response = {
            "networks": [
                {
                    "network": "ethereum",
                    "name": "Ethereum Mainnet",
                    "native_currency": "ETH",
                    "chain_id": 1,
                    "block_time": 12,
                    "status": "active"
                },
                {
                    "network": "polygon",
                    "name": "Polygon",
                    "native_currency": "MATIC",
                    "chain_id": 137,
                    "block_time": 2,
                    "status": "active"
                },
                {
                    "network": "xrp",
                    "name": "XRP Ledger",
                    "native_currency": "XRP",
                    "chain_id": null,
                    "block_time": 3,
                    "status": "active"
                }
            ]
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/blockchain/networks",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.get_supported_networks()
        
        assert len(result.networks) == 3
        assert result.networks[0].network == "ethereum"
        assert result.networks[1].network == "polygon"
        assert result.networks[2].network == "xrp"
    
    @responses.activate
    def test_get_network_info(self):
        """Test get specific network information"""
        network = "ethereum"
        
        expected_response = {
            "network": "ethereum",
            "name": "Ethereum Mainnet",
            "native_currency": "ETH",
            "chain_id": 1,
            "block_time": 12,
            "latest_block": 18234567,
            "gas_price": {
                "slow": "0.000000015",
                "standard": "0.000000020",
                "fast": "0.000000025"
            },
            "status": "active",
            "health": {
                "rpc_status": "healthy",
                "block_delay": 0,
                "last_update": "2025-07-03T14:30:00Z"
            }
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/blockchain/networks/{network}",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.get_network_info(network)
        
        assert result.network == "ethereum"
        assert result.chain_id == 1
        assert result.latest_block == 18234567
        assert result.gas_price["standard"] == "0.000000020"
        assert result.health["rpc_status"] == "healthy"
    
    @responses.activate
    def test_estimate_gas_fees(self):
        """Test estimate gas fees for transaction"""
        transaction_data = {
            "network": "ethereum",
            "from_address": "0x123...abc",
            "to_address": "0x456...def",
            "amount": 1.0,
            "currency": "ETH"
        }
        
        expected_response = {
            "network": "ethereum",
            "gas_estimates": {
                "slow": {
                    "gas_price": "0.000000015",
                    "gas_limit": 21000,
                    "total_fee": "0.000315",
                    "confirmation_time": "5-10 minutes"
                },
                "standard": {
                    "gas_price": "0.000000020",
                    "gas_limit": 21000,
                    "total_fee": "0.000420",
                    "confirmation_time": "2-5 minutes"
                },
                "fast": {
                    "gas_price": "0.000000025",
                    "gas_limit": 21000,
                    "total_fee": "0.000525",
                    "confirmation_time": "1-2 minutes"
                }
            },
            "estimated_at": "2025-07-03T14:30:00Z"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/blockchain/estimate-gas",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.estimate_gas(transaction_data)
        
        assert result.network == "ethereum"
        assert "slow" in result.gas_estimates
        assert "standard" in result.gas_estimates
        assert "fast" in result.gas_estimates
        assert result.gas_estimates["standard"]["total_fee"] == "0.000420"
    
    @responses.activate
    def test_get_wallet_balance(self):
        """Test get wallet balance"""
        address = "0x123...abc"
        network = "ethereum"
        
        expected_response = {
            "address": address,
            "network": network,
            "balances": [
                {
                    "currency": "ETH",
                    "balance": "5.247832",
                    "usd_value": 10495.66
                },
                {
                    "currency": "USDC",
                    "balance": "1000.000000",
                    "usd_value": 1000.00
                },
                {
                    "currency": "USDT",
                    "balance": "500.000000",
                    "usd_value": 500.00
                }
            ],
            "total_usd_value": 11995.66,
            "last_updated": "2025-07-03T14:30:00Z"
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/blockchain/balance",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.get_balance(address, network)
        
        assert result.address == address
        assert result.network == network
        assert len(result.balances) == 3
        assert result.balances[0].currency == "ETH"
        assert result.total_usd_value == 11995.66
    
    @responses.activate
    def test_create_wallet(self):
        """Test create new wallet"""
        wallet_data = {
            "network": "ethereum",
            "name": "Test Wallet",
            "description": "Wallet for testing"
        }
        
        expected_response = {
            "wallet_id": "WALLET_123456789",
            "address": "0x789...def",
            "network": "ethereum",
            "name": "Test Wallet",
            "description": "Wallet for testing",
            "status": "active",
            "created_at": "2025-07-03T14:30:00Z"
        }
        
        responses.add(
            responses.POST,
            f"{self.base_url}/api/v1/blockchain/wallets",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.create_wallet(wallet_data)
        
        assert result.wallet_id == "WALLET_123456789"
        assert result.address == "0x789...def"
        assert result.network == "ethereum"
        assert result.name == "Test Wallet"
        assert result.status == "active"
    
    @responses.activate
    def test_list_wallets(self):
        """Test list wallets"""
        expected_response = {
            "wallets": [
                {
                    "wallet_id": "WALLET_123456789",
                    "address": "0x789...def",
                    "network": "ethereum",
                    "name": "Test Wallet",
                    "status": "active",
                    "balance_usd": 11995.66
                },
                {
                    "wallet_id": "WALLET_987654321",
                    "address": "0xabc...123",
                    "network": "polygon",
                    "name": "Polygon Wallet",
                    "status": "active",
                    "balance_usd": 2500.00
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
            f"{self.base_url}/api/v1/blockchain/wallets",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.list_wallets()
        
        assert len(result.wallets) == 2
        assert result.wallets[0].wallet_id == "WALLET_123456789"
        assert result.wallets[1].network == "polygon"
        assert result.pagination.total == 2
    
    @responses.activate
    def test_track_transaction(self):
        """Test track transaction across networks"""
        blockchain_hash = "0x789...012"
        
        expected_response = {
            "blockchain_hash": blockchain_hash,
            "network": "ethereum",
            "status": "confirmed",
            "confirmations": 15,
            "block_number": 18234567,
            "from_address": "0x123...abc",
            "to_address": "0x456...def",
            "amount": "1.000000000000000000",
            "currency": "ETH",
            "gas_used": 21000,
            "gas_price": "0.000000020",
            "transaction_fee": "0.000420",
            "timestamp": "2025-07-03T14:30:00Z"
        }
        
        responses.add(
            responses.GET,
            f"{self.base_url}/api/v1/blockchain/track/{blockchain_hash}",
            json=expected_response,
            status=200
        )
        
        result = self.client.blockchain.track_transaction(blockchain_hash)
        
        assert result.blockchain_hash == blockchain_hash
        assert result.network == "ethereum"
        assert result.status == "confirmed"
        assert result.confirmations == 15
        assert result.amount == "1.000000000000000000"


if __name__ == "__main__":
    pytest.main([__file__, "-v"])