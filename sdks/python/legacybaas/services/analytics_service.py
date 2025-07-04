"""
Analytics Service - Advanced analytics and reporting
"""

from typing import Dict, Any, Optional
from ..errors import LegacyBaaSError


class AnalyticsService:
    """Analytics Service for Python SDK"""
    
    def __init__(self, client):
        self.client = client
    
    def execute_query(self, query: Dict[str, Any]) -> Dict[str, Any]:
        """Execute analytics query"""
        return self.client._make_request("POST", "/analytics/query", data=query)
    
    def get_metrics(self, category: str, time_range: Dict[str, str], filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Get pre-built metrics"""
        return self.client._make_request(
            "GET", 
            f"/analytics/metrics/{category}",
            headers={
                "X-Time-Range": str(time_range),
                "X-Filters": str(filters or {})
            }
        )
    
    def get_system_metrics(self) -> Dict[str, Any]:
        """Get system performance metrics"""
        return self.client._make_request("GET", "/analytics/system")
    
    def get_real_time_metrics(self) -> Dict[str, Any]:
        """Get real-time analytics"""
        return self.client._make_request("GET", "/analytics/real-time")
    
    def create_dashboard(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """Create custom dashboard"""
        return self.client._make_request("POST", "/analytics/dashboards", data=config)
    
    def get_dashboard(self, dashboard_id: str) -> Dict[str, Any]:
        """Get dashboard"""
        return self.client._make_request("GET", f"/analytics/dashboards/{dashboard_id}")
    
    def list_dashboards(self) -> Dict[str, Any]:
        """List dashboards"""
        return self.client._make_request("GET", "/analytics/dashboards")
    
    def create_report(self, config: Dict[str, Any]) -> Dict[str, Any]:
        """Create report"""
        return self.client._make_request("POST", "/analytics/reports", data=config)
    
    def get_report_status(self, report_id: str) -> Dict[str, Any]:
        """Get report status"""
        return self.client._make_request("GET", f"/analytics/reports/{report_id}/status")
    
    def get_fraud_metrics(self) -> Dict[str, Any]:
        """Get fraud detection metrics"""
        return self.client._make_request("GET", "/analytics/fraud")
    
    def get_predictive_analytics(self, model: str, time_horizon: str) -> Dict[str, Any]:
        """Get predictive analytics"""
        return self.client._make_request(
            "GET", 
            f"/analytics/predictive/{model}",
            headers={"X-Time-Horizon": time_horizon}
        )