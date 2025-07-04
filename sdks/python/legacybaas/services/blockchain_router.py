"""
Blockchain Router Service
Smart routing across 11+ blockchain networks
"""

from typing import Dict, Any, List, Optional
from ..errors import LegacyBaaSError, BlockchainError


class BlockchainRouter:
    """Blockchain Router for Python SDK"""
    
    def __init__(self, client):
        self.client = client
    
    def route_transaction(
        self,
        transaction: Dict[str, Any],
        preferences: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Route transaction using AI-powered smart routing
        
        Args:
            transaction: Blockchain transaction data
            preferences: Routing preferences
            
        Returns:
            Transaction result
        """
        default_preferences = {
            "preferred_networks": [],
            "max_fee": None,
            "max_time": None,
            "prioritize_speed": False,
            "prioritize_cost": False,
            "prioritize_security": True
        }
        
        if preferences:
            default_preferences.update(preferences)
        
        try:
            return self.client._make_request(
                "POST",
                "/blockchain/route",
                data={
                    "transaction": transaction,
                    "preferences": default_preferences
                }
            )
        except Exception as e:
            raise BlockchainError(f"Transaction routing failed: {e}")
    
    def get_routing_recommendation(
        self,
        transaction: Dict[str, Any],
        preferences: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Get optimal routing recommendation
        
        Args:
            transaction: Transaction to analyze
            preferences: Routing preferences
            
        Returns:
            Routing recommendations with AI insights
        """
        return self.client._make_request(
            "POST",
            "/blockchain/route/recommend",
            data={
                "transaction": transaction,
                "preferences": preferences or {}
            }
        )
    
    def get_transaction_status(self, transaction_id: str) -> Dict[str, Any]:
        """
        Get transaction status
        
        Args:
            transaction_id: Transaction ID
            
        Returns:
            Transaction status
        """
        return self.client._make_request(
            "GET",
            f"/blockchain/transactions/{transaction_id}"
        )
    
    def track_transaction(self, blockchain_tx_id: str) -> Dict[str, Any]:
        """
        Track transaction by blockchain hash
        
        Args:
            blockchain_tx_id: Blockchain transaction ID
            
        Returns:
            Transaction tracking info
        """
        return self.client._make_request(
            "GET",
            f"/blockchain/track/{blockchain_tx_id}"
        )
    
    def get_supported_networks(self) -> Dict[str, Any]:
        """
        Get supported networks
        
        Returns:
            Supported blockchain networks
        """
        return self.client._make_request(
            "GET",
            "/blockchain/networks"
        )
    
    def get_network_info(self, network_id: str) -> Dict[str, Any]:
        """
        Get specific network information
        
        Args:
            network_id: Network identifier
            
        Returns:
            Network information
        """
        return self.client._make_request(
            "GET",
            f"/blockchain/networks/{network_id}"
        )
    
    def get_network_health(self) -> Dict[str, Any]:
        """
        Get network health metrics
        
        Returns:
            Network health status
        """
        return self.client._make_request(
            "GET",
            "/blockchain/health"
        )
    
    def estimate_gas(
        self,
        transaction: Dict[str, Any],
        network_id: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """
        Estimate gas fees for transaction
        
        Args:
            transaction: Transaction data
            network_id: Specific network to estimate for
            
        Returns:
            Gas estimates
        """
        return self.client._make_request(
            "POST",
            "/blockchain/estimate-gas",
            data={
                "transaction": transaction,
                "network_id": network_id
            }
        )
    
    def create_wallet(self, wallet_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Create new wallet
        
        Args:
            wallet_data: Wallet creation data
            
        Returns:
            Wallet information
        """
        return self.client._make_request(
            "POST",
            "/blockchain/wallets",
            data=wallet_data
        )
    
    def get_wallet_balance(self, address: str, network: str) -> Dict[str, Any]:
        """
        Get wallet balance
        
        Args:
            address: Wallet address
            network: Network identifier
            
        Returns:
            Wallet balance information
        """
        return self.client._make_request(
            "GET",
            f"/blockchain/balance/{address}",
            headers={"X-Network": network}
        )
    
    def list_wallets(self, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """
        List wallets
        
        Args:
            filters: Optional filters
            
        Returns:
            Wallet list with pagination
        """
        return self.client._make_request(
            "GET",
            "/blockchain/wallets",
            params=filters or {}
        )
    
    def get_cross_chain_bridge_status(self) -> Dict[str, Any]:
        """
        Get cross-chain bridge status
        
        Returns:
            Bridge status information
        """
        return self.client._make_request(
            "GET",
            "/blockchain/bridges"
        )
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get blockchain metrics
        
        Returns:
            Blockchain operation metrics
        """
        return self.client._make_request(
            "GET",
            "/blockchain/metrics"
        )