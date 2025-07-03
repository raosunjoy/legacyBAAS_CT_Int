"""
Webhook handling service
"""

from typing import Dict, Any


class WebhookHandler:
    """Webhook management service"""
    
    def __init__(self, client):
        self.client = client
    
    def create_webhook(self, webhook_data: Dict[str, Any]):
        """Create webhook"""
        response = self.client.post("/webhooks", webhook_data)
        return type('Webhook', (), response)()
    
    def list_webhooks(self):
        """List webhooks"""
        response = self.client.get("/webhooks")
        webhooks = [type('Webhook', (), wh)() for wh in response.get('webhooks', [])]
        return type('WebhookList', (), {'webhooks': webhooks})()