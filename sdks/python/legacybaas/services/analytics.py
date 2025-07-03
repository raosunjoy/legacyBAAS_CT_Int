"""
Analytics service
"""

from typing import Dict, Any


class AnalyticsService:
    """Real-time analytics service"""
    
    def __init__(self, client):
        self.client = client
    
    def get_metrics(self, **filters):
        """Get analytics metrics"""
        response = self.client.get("/analytics/metrics", params=filters)
        return type('Metrics', (), response)()
    
    def get_transaction_volume(self, period: str = "day"):
        """Get transaction volume"""
        response = self.client.get("/analytics/volume", params={"period": period})
        return type('VolumeData', (), response)()