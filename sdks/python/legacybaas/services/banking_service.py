"""
Banking Service - Integration with core banking systems
"""

from typing import Dict, Any, Optional
from ..errors import BankingError


class BankingService:
    """Banking Service for Python SDK"""
    
    def __init__(self, client):
        self.client = client
    
    def get_account(self, account_id: str) -> Dict[str, Any]:
        """Get account information"""
        return self.client._make_request("GET", f"/banking/accounts/{account_id}")
    
    def list_accounts(self, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """List accounts"""
        return self.client._make_request("GET", "/banking/accounts", params=filters or {})
    
    def get_account_balance(self, account_id: str) -> Dict[str, Any]:
        """Get account balance"""
        return self.client._make_request("GET", f"/banking/accounts/{account_id}/balance")
    
    def get_transaction_history(self, account_id: str, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Get transaction history"""
        return self.client._make_request("GET", f"/banking/accounts/{account_id}/transactions", params=filters or {})
    
    def transfer_funds(self, transfer_request: Dict[str, Any]) -> Dict[str, Any]:
        """Transfer funds between accounts"""
        return self.client._make_request("POST", "/banking/transfers", data=transfer_request)
    
    def make_payment(self, payment_request: Dict[str, Any]) -> Dict[str, Any]:
        """Make a payment"""
        return self.client._make_request("POST", "/banking/payments", data=payment_request)
    
    def get_payment_status(self, payment_id: str) -> Dict[str, Any]:
        """Get payment status"""
        return self.client._make_request("GET", f"/banking/payments/{payment_id}/status")
    
    def generate_statement(self, statement_request: Dict[str, Any]) -> Dict[str, Any]:
        """Generate account statement"""
        return self.client._make_request("POST", "/banking/statements", data=statement_request)
    
    def get_supported_systems(self) -> Dict[str, Any]:
        """Get supported banking systems"""
        return self.client._make_request("GET", "/banking/systems")
    
    def validate_account(self, account_number: str, routing_number: str) -> Dict[str, Any]:
        """Validate account"""
        return self.client._make_request("POST", "/banking/validate-account", data={
            "account_number": account_number,
            "routing_number": routing_number
        })
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get banking metrics"""
        return self.client._make_request("GET", "/banking/metrics")