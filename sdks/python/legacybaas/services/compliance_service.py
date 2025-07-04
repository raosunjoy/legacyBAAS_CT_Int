"""
Compliance Service - AML/KYC screening and regulatory compliance
"""

from typing import Dict, Any, Optional, List
from ..errors import ComplianceError


class ComplianceService:
    """Compliance Service for Python SDK"""
    
    def __init__(self, client):
        self.client = client
    
    def screen_subject(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Screen individual, entity or transaction"""
        return self.client._make_request("POST", "/compliance/screen", data=request)
    
    def get_screening_result(self, screening_id: str) -> Dict[str, Any]:
        """Get screening result"""
        return self.client._make_request("GET", f"/compliance/screenings/{screening_id}")
    
    def bulk_screen(self, requests: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Bulk screening"""
        return self.client._make_request("POST", "/compliance/screen/bulk", data={"requests": requests})
    
    def get_bulk_screening_status(self, batch_id: str) -> Dict[str, Any]:
        """Get bulk screening status"""
        return self.client._make_request("GET", f"/compliance/screen/bulk/{batch_id}")
    
    def upload_document(self, customer_id: str, document_type: str, file_data: bytes, file_name: str, metadata: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Upload KYC document"""
        import base64
        return self.client._make_request("POST", "/compliance/documents", data={
            "customer_id": customer_id,
            "document_type": document_type,
            "file_name": file_name,
            "metadata": metadata or {},
            "file_size": len(file_data),
            "file_data": base64.b64encode(file_data).decode('utf-8')
        })
    
    def get_document_status(self, document_id: str) -> Dict[str, Any]:
        """Get document verification status"""
        return self.client._make_request("GET", f"/compliance/documents/{document_id}")
    
    def list_documents(self, customer_id: str, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """List KYC documents"""
        return self.client._make_request("GET", f"/compliance/customers/{customer_id}/documents", params=filters or {})
    
    def create_case(self, case_data: Dict[str, Any]) -> Dict[str, Any]:
        """Create compliance case"""
        return self.client._make_request("POST", "/compliance/cases", data=case_data)
    
    def get_case(self, case_id: str) -> Dict[str, Any]:
        """Get compliance case"""
        return self.client._make_request("GET", f"/compliance/cases/{case_id}")
    
    def update_case(self, case_id: str, updates: Dict[str, Any]) -> Dict[str, Any]:
        """Update compliance case"""
        return self.client._make_request("PUT", f"/compliance/cases/{case_id}", data=updates)
    
    def list_cases(self, filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """List compliance cases"""
        return self.client._make_request("GET", "/compliance/cases", params=filters or {})
    
    def get_policies(self) -> Dict[str, Any]:
        """Get compliance policies"""
        return self.client._make_request("GET", "/compliance/policies")
    
    def create_policy(self, policy: Dict[str, Any]) -> Dict[str, Any]:
        """Create compliance policy"""
        return self.client._make_request("POST", "/compliance/policies", data=policy)
    
    def get_compliance_metrics(self) -> Dict[str, Any]:
        """Get compliance metrics"""
        return self.client._make_request("GET", "/compliance/metrics")
    
    def get_screening_lists(self) -> Dict[str, Any]:
        """Get available screening lists"""
        return self.client._make_request("GET", "/compliance/lists")
    
    def generate_report(self, report_type: str, time_range: Dict[str, str], filters: Optional[Dict[str, Any]] = None) -> Dict[str, Any]:
        """Generate compliance report"""
        return self.client._make_request("POST", "/compliance/reports", data={
            "report_type": report_type,
            "time_range": time_range,
            "filters": filters or {}
        })