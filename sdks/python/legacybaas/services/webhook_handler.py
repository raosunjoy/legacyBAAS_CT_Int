"""
Webhook Handler Service - Real-time event notifications
"""

from typing import Dict, Any, Optional
from ..errors import LegacyBaaSError
from ..utils import verify_webhook_signature


class WebhookHandler:
    """Webhook Handler for Python SDK"""
    
    def __init__(self, client):
        self.client = client
    
    def create_webhook(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """Create webhook"""
        return self.client._make_request("POST", "/webhooks", data=config)
    
    def get_webhook(self, webhook_id: str) -> Dict[str, Any]:
        """Get webhook"""
        return self.client._make_request("GET", f"/webhooks/{webhook_id}")
    
    def list_webhooks(self, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """List webhooks"""
        return self.client._make_request("GET", "/webhooks", params=filters or {})
    
    def update_webhook(self, webhook_id: str, updates: Dict[str, Any]) -> Dict[str, Any]:
        """Update webhook"""
        return self.client._make_request("PUT", f"/webhooks/{webhook_id}", data=updates)
    
    def delete_webhook(self, webhook_id: str) -> Dict[str, Any]:
        """Delete webhook"""
        return self.client._make_request("DELETE", f"/webhooks/{webhook_id}")
    
    def test_webhook(self, webhook_id: str, test_event: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Test webhook"""
        data = {"event": test_event} if test_event else {}
        return self.client._make_request("POST", f"/webhooks/{webhook_id}/test", data=data)
    
    def get_webhook_deliveries(self, webhook_id: str, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Get webhook deliveries"""
        return self.client._make_request("GET", f"/webhooks/{webhook_id}/deliveries", params=filters or {})
    
    def get_delivery_details(self, delivery_id: str) -> Dict[str, Any]:
        """Get delivery details"""
        return self.client._make_request("GET", f"/webhooks/deliveries/{delivery_id}")
    
    def retry_delivery(self, delivery_id: str) -> Dict[str, Any]:
        """Retry webhook delivery"""
        return self.client._make_request("POST", f"/webhooks/deliveries/{delivery_id}/retry")
    
    def get_webhook_stats(self, webhook_id: str, time_range: Dict[str, str]) -> Dict[str, Any]:
        """Get webhook statistics"""
        return self.client._make_request(
            "GET", 
            f"/webhooks/{webhook_id}/stats",
            headers={"X-Time-Range": str(time_range)}
        )
    
    def get_available_events(self) -> Dict[str, Any]:
        """Get available events"""
        return self.client._make_request("GET", "/webhooks/events")
    
    def get_webhook_health(self) -> Dict[str, Any]:
        """Get webhook health"""
        return self.client._make_request("GET", "/webhooks/health")
    
    @staticmethod
    def verify_signature(payload: str, signature: str, secret: str) -> bool:
        """Verify webhook signature"""
        return verify_webhook_signature(payload, signature, secret)
    
    @staticmethod
    def parse_webhook_payload(payload: str) -> Dict[str, Any]:
        """Parse webhook payload"""
        import json
        try:
            return json.loads(payload)
        except json.JSONDecodeError:
            raise ValueError("Invalid webhook payload format")