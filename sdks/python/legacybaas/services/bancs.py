"""
BaNCS integration service
"""

from typing import Dict, Any


class BancsIntegration:
    """BaNCS banking system integration service"""
    
    def __init__(self, client):
        self.client = client
    
    def process_transaction(self, transaction_data: Dict[str, Any]):
        """Process BaNCS transaction"""
        response = self.client.post("/bancs/transactions", transaction_data)
        return type('BancsResult', (), response)()
    
    def get_account(self, account_number: str):
        """Get account information"""
        response = self.client.get(f"/bancs/accounts/{account_number}")
        return type('Account', (), response)()
    
    def get_balance(self, account_number: str):
        """Get account balance"""
        response = self.client.get(f"/bancs/accounts/{account_number}/balance")
        return type('Balance', (), response)()