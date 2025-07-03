"""
SWIFT processing service
"""

from typing import Dict, Any
from ..types.swift import MT103Message, MT202Message, MT700Message


class SwiftProcessor:
    """SWIFT message processing service"""
    
    def __init__(self, client):
        self.client = client
    
    def process_mt103(self, mt103_data: Dict[str, Any]):
        """Process MT103 message"""
        response = self.client.post("/swift/mt103", mt103_data)
        return type('MT103Result', (), response)()
    
    def process_mt202(self, mt202_data: Dict[str, Any]):
        """Process MT202 message"""
        response = self.client.post("/swift/mt202", mt202_data)
        return type('MT202Result', (), response)()
    
    def process_mt700(self, mt700_data: Dict[str, Any]):
        """Process MT700 message"""
        response = self.client.post("/swift/mt700", mt700_data)
        return type('MT700Result', (), response)()
    
    def get_transaction_status(self, transaction_id: str):
        """Get transaction status"""
        response = self.client.get(f"/swift/transactions/{transaction_id}")
        return type('TransactionStatus', (), response)()
    
    def list_transactions(self, **filters):
        """List transactions"""
        response = self.client.get("/swift/transactions", params=filters)
        transactions = [type('Transaction', (), tx)() for tx in response.get('transactions', [])]
        pagination = type('Pagination', (), response.get('pagination', {}))()
        return type('TransactionList', (), {'transactions': transactions, 'pagination': pagination})()
    
    def validate_message(self, message_type: str, message_data: Dict[str, Any]):
        """Validate SWIFT message"""
        response = self.client.post("/swift/validate", {
            "message_type": message_type,
            "data": message_data
        })
        return type('ValidationResult', (), response)()
    
    def get_rates(self):
        """Get exchange rates"""
        response = self.client.get("/swift/rates")
        return type('ExchangeRates', (), response)()
    
    def get_rate(self, currency_pair: str):
        """Get specific currency rate"""
        response = self.client.get(f"/swift/rates/{currency_pair}")
        return type('CurrencyRate', (), response)()