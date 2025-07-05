"""
COBOL Transpiler Service for Python SDK
Handles all COBOL transpilation operations
"""

import asyncio
import time
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass
from enum import Enum

import requests
import aiohttp
from ..exceptions import LegacyBaaSException
from ..types.common import ApiResponse


class TargetLanguage(Enum):
    SOLIDITY = "solidity"
    CORDA = "corda"
    ALGORAND = "algorand"


class BankingSystem(Enum):
    FIS_SYSTEMATICS = "fis-systematics"
    FISERV_DNA = "fiserv-dna"
    TCS_BANCS = "tcs-bancs"
    TEMENOS_TRANSACT = "temenos-transact"


class BlockchainNetwork(Enum):
    ETHEREUM = "ethereum"
    CORDA = "corda"
    ALGORAND = "algorand"
    XRP = "xrp"


@dataclass
class CobolTranspileOptions:
    optimize_for_gas: bool = True
    include_comments: bool = True
    generate_interface: bool = True
    strict_type_checking: bool = False
    generate_validators: bool = False
    include_audit_trail: bool = False
    auto_optimize: bool = False
    deploy_immediately: bool = False
    monitor_performance: bool = False
    async_processing: bool = False


@dataclass
class ComplianceConfig:
    config_id: Optional[str] = None
    risk_threshold: str = "MEDIUM"  # LOW, MEDIUM, HIGH, CRITICAL
    require_approval: bool = False
    generate_report: bool = False


@dataclass
class CobolTranspileRequest:
    source_code: str
    target_language: Union[TargetLanguage, str]
    banking_system: Union[BankingSystem, str]
    blockchain_network: Union[BlockchainNetwork, str]
    options: Optional[CobolTranspileOptions] = None
    compliance: Optional[ComplianceConfig] = None


@dataclass
class CobolTranspileResult:
    success: bool
    transaction_id: str
    job_id: Optional[str] = None
    contract_address: Optional[str] = None
    transaction_hash: Optional[str] = None
    source_code: Optional[str] = None
    gas_estimate: Optional[int] = None
    deployment_cost: Optional[float] = None
    performance: Optional[Dict[str, Any]] = None
    compliance: Optional[Dict[str, Any]] = None
    error: Optional[str] = None


@dataclass
class CobolTemplate:
    id: str
    name: str
    description: str
    banking_system: str
    category: str
    variables: List[Dict[str, Any]]
    version: str
    created: str
    updated: str


@dataclass
class CobolBankingConfig:
    id: str
    name: str
    type: str
    settings: Dict[str, Any]


@dataclass
class NetworkSelection:
    network: str
    language: str
    reasons: List[str]
    estimated_cost: float
    estimated_time: int


class CobolTranspilerService:
    """COBOL Transpiler Service Implementation"""
    
    def __init__(self, http_client):
        self.http = http_client
        self.base_url = "/banking"
    
    def transpile(self, request: CobolTranspileRequest) -> CobolTranspileResult:
        """
        Transpile COBOL code to target blockchain language
        
        Args:
            request: CobolTranspileRequest with source code and configuration
            
        Returns:
            CobolTranspileResult with transpilation results
        """
        try:
            data = {
                "sourceCode": request.source_code,
                "targetLanguage": request.target_language.value if isinstance(request.target_language, TargetLanguage) else request.target_language,
                "bankingSystem": request.banking_system.value if isinstance(request.banking_system, BankingSystem) else request.banking_system,
                "blockchainNetwork": request.blockchain_network.value if isinstance(request.blockchain_network, BlockchainNetwork) else request.blockchain_network
            }
            
            if request.options:
                data["options"] = {
                    "optimizeForGas": request.options.optimize_for_gas,
                    "includeComments": request.options.include_comments,
                    "generateInterface": request.options.generate_interface,
                    "strictTypeChecking": request.options.strict_type_checking,
                    "generateValidators": request.options.generate_validators,
                    "includeAuditTrail": request.options.include_audit_trail,
                    "autoOptimize": request.options.auto_optimize,
                    "deployImmediately": request.options.deploy_immediately,
                    "monitorPerformance": request.options.monitor_performance,
                    "async": request.options.async_processing
                }
            
            if request.compliance:
                data["compliance"] = {
                    "configId": request.compliance.config_id,
                    "riskThreshold": request.compliance.risk_threshold,
                    "requireApproval": request.compliance.require_approval,
                    "generateReport": request.compliance.generate_report
                }
            
            response = self.http.post(f"{self.base_url}/transpile", json=data)
            result_data = response.json()["data"]
            
            return CobolTranspileResult(
                success=result_data.get("success", False),
                transaction_id=result_data.get("transactionId"),
                job_id=result_data.get("jobId"),
                contract_address=result_data.get("contractAddress"),
                transaction_hash=result_data.get("transactionHash"),
                source_code=result_data.get("sourceCode"),
                gas_estimate=result_data.get("gasEstimate"),
                deployment_cost=result_data.get("deploymentCost"),
                performance=result_data.get("performance"),
                compliance=result_data.get("compliance"),
                error=result_data.get("error")
            )
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"COBOL transpilation failed: {str(e)}")
    
    def batch_transpile(self, files: List[Dict[str, Any]], banking_system: str, 
                       options: Optional[Dict[str, Any]] = None) -> List[CobolTranspileResult]:
        """Alias for batchTranspile"""
        return self.batchTranspile(files, banking_system, options)
    
    def batchTranspile(self, files: List[Dict[str, Any]], banking_system: str, 
                       options: Optional[Dict[str, Any]] = None) -> List[CobolTranspileResult]:
        """
        Process multiple COBOL files in batch
        
        Args:
            files: List of file dictionaries with name, content, and configuration
            banking_system: Banking system to use for all files
            options: Batch processing options
            
        Returns:
            List of CobolTranspileResult objects
        """
        try:
            data = {
                "files": files,
                "bankingSystem": banking_system,
                "options": options or {}
            }
            
            response = self.http.post(f"{self.base_url}/transpile/batch", json=data)
            results_data = response.json()["data"]
            
            return [
                CobolTranspileResult(
                    success=result.get("success", False),
                    transaction_id=result.get("transactionId"),
                    job_id=result.get("jobId"),
                    contract_address=result.get("contractAddress"),
                    transaction_hash=result.get("transactionHash"),
                    source_code=result.get("sourceCode"),
                    error=result.get("error")
                )
                for result in results_data
            ]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Batch COBOL transpilation failed: {str(e)}")
    
    def validate(self, source_code: str, banking_system: str) -> Dict[str, Any]:
        """
        Validate COBOL code without transpilation
        
        Args:
            source_code: COBOL source code to validate
            banking_system: Banking system for validation context
            
        Returns:
            Validation results with errors, warnings, and suggestions
        """
        try:
            data = {
                "sourceCode": source_code,
                "bankingSystem": banking_system
            }
            
            response = self.http.post(f"{self.base_url}/transpile/validate", json=data)
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"COBOL validation failed: {str(e)}")
    
    def getStatus(self, job_id: str) -> Dict[str, Any]:
        """Get transpilation job status (camelCase alias)"""
        return self.get_status(job_id)
    
    def get_status(self, job_id: str) -> Dict[str, Any]:
        """
        Get transpilation job status
        
        Args:
            job_id: Job identifier
            
        Returns:
            Job status information
        """
        try:
            response = self.http.get(f"{self.base_url}/transpile/status/{job_id}")
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get transpilation status: {str(e)}")
    
    def monitorStatus(self, job_id: str, callback=None, poll_interval: int = 2):
        """Monitor transpilation status with polling (camelCase alias)"""
        return self.monitor_status(job_id, callback, poll_interval)
    
    def monitor_status(self, job_id: str, callback=None, poll_interval: int = 2):
        """
        Monitor transpilation status with polling
        
        Args:
            job_id: Job identifier
            callback: Optional callback function for status updates
            poll_interval: Polling interval in seconds
            
        Yields:
            Status updates until completion
        """
        while True:
            try:
                status = self.get_status(job_id)
                
                if callback:
                    callback(status)
                
                yield status
                
                if status.get("progress", 0) >= 100:
                    break
                    
                time.sleep(poll_interval)
                
            except Exception as e:
                if callback:
                    callback({"error": str(e)})
                raise
    
    def getTemplates(self, banking_system: Optional[str] = None, 
                    category: Optional[str] = None) -> List[CobolTemplate]:
        """Get available COBOL templates (camelCase alias)"""
        return self.get_templates(banking_system, category)
    
    def get_templates(self, banking_system: Optional[str] = None, 
                     category: Optional[str] = None) -> List[CobolTemplate]:
        """
        Get available COBOL templates
        
        Args:
            banking_system: Filter by banking system
            category: Filter by category
            
        Returns:
            List of available templates
        """
        try:
            params = {}
            if banking_system:
                params["bankingSystem"] = banking_system
            if category:
                params["category"] = category
            
            response = self.http.get(f"{self.base_url}/transpile/templates", params=params)
            templates_data = response.json()["data"]
            
            return [
                CobolTemplate(
                    id=template["id"],
                    name=template["name"],
                    description=template["description"],
                    banking_system=template["bankingSystem"],
                    category=template["category"],
                    variables=template["variables"],
                    version=template["version"],
                    created=template["created"],
                    updated=template["updated"]
                )
                for template in templates_data
            ]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get templates: {str(e)}")
    
    def getTemplate(self, template_id: str) -> CobolTemplate:
        """Get specific template by ID (camelCase alias)"""
        return self.get_template(template_id)
    
    def get_template(self, template_id: str) -> CobolTemplate:
        """
        Get specific template by ID
        
        Args:
            template_id: Template identifier
            
        Returns:
            Template information
        """
        try:
            response = self.http.get(f"{self.base_url}/transpile/templates/{template_id}")
            template_data = response.json()["data"]
            
            return CobolTemplate(
                id=template_data["id"],
                name=template_data["name"],
                description=template_data["description"],
                banking_system=template_data["bankingSystem"],
                category=template_data["category"],
                variables=template_data["variables"],
                version=template_data["version"],
                created=template_data["created"],
                updated=template_data["updated"]
            )
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get template: {str(e)}")
    
    def transpileWithTemplate(self, template_id: str, variables: Dict[str, Any],
                             target_language: str, blockchain_network: str,
                             options: Optional[Dict[str, Any]] = None) -> CobolTranspileResult:
        """Transpile using a template (camelCase alias)"""
        return self.transpile_with_template(template_id, variables, target_language, blockchain_network, options)
    
    def transpile_with_template(self, template_id: str, variables: Dict[str, Any],
                               target_language: str, blockchain_network: str,
                               options: Optional[Dict[str, Any]] = None) -> CobolTranspileResult:
        """
        Transpile using a template
        
        Args:
            template_id: Template identifier
            variables: Template variables
            target_language: Target blockchain language
            blockchain_network: Target blockchain network
            options: Additional options
            
        Returns:
            Transpilation result
        """
        try:
            data = {
                "variables": variables,
                "targetLanguage": target_language,
                "blockchainNetwork": blockchain_network,
                "options": options or {}
            }
            
            response = self.http.post(f"{self.base_url}/transpile/templates/{template_id}", json=data)
            result_data = response.json()["data"]
            
            return CobolTranspileResult(
                success=result_data.get("success", False),
                transaction_id=result_data.get("transactionId"),
                job_id=result_data.get("jobId"),
                contract_address=result_data.get("contractAddress"),
                transaction_hash=result_data.get("transactionHash"),
                source_code=result_data.get("sourceCode"),
                error=result_data.get("error")
            )
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Template transpilation failed: {str(e)}")
    
    def create_banking_config(self, config: Dict[str, Any]) -> CobolBankingConfig:
        """
        Create custom banking system configuration
        
        Args:
            config: Banking configuration dictionary
            
        Returns:
            Created configuration
        """
        try:
            response = self.http.post(f"{self.base_url}/transpile/configs", json=config)
            config_data = response.json()["data"]
            
            return CobolBankingConfig(
                id=config_data["id"],
                name=config_data["name"],
                type=config_data["type"],
                settings=config_data["settings"]
            )
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to create banking config: {str(e)}")
    
    def get_banking_configs(self) -> List[CobolBankingConfig]:
        """
        Get banking system configurations
        
        Returns:
            List of banking configurations
        """
        try:
            response = self.http.get(f"{self.base_url}/transpile/configs")
            configs_data = response.json()["data"]
            
            return [
                CobolBankingConfig(
                    id=config["id"],
                    name=config["name"],
                    type=config["type"],
                    settings=config["settings"]
                )
                for config in configs_data
            ]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get banking configs: {str(e)}")
    
    def selectOptimalNetwork(self, criteria: Dict[str, str]) -> NetworkSelection:
        """Select optimal blockchain network (camelCase alias)"""
        return self.select_optimal_network(criteria)
    
    def select_optimal_network(self, criteria: Dict[str, str]) -> NetworkSelection:
        """
        Select optimal blockchain network based on criteria
        
        Args:
            criteria: Network selection criteria
            
        Returns:
            Optimal network selection
        """
        try:
            response = self.http.post(f"{self.base_url}/transpile/network-selection", json=criteria)
            selection_data = response.json()["data"]
            
            return NetworkSelection(
                network=selection_data["network"],
                language=selection_data["language"],
                reasons=selection_data["reasons"],
                estimated_cost=selection_data["estimatedCost"],
                estimated_time=selection_data["estimatedTime"]
            )
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Network selection failed: {str(e)}")
    
    def get_history(self, **filters) -> Dict[str, Any]:
        """
        Get transpilation history
        
        Args:
            **filters: History filter parameters
            
        Returns:
            Transpilation history
        """
        try:
            response = self.http.get(f"{self.base_url}/transpile/history", params=filters)
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get transpilation history: {str(e)}")
    
    def getAnalytics(self, period: str = "24h") -> Dict[str, Any]:
        """Get transpilation analytics (camelCase alias)"""
        return self.get_analytics(period)
    
    def get_analytics(self, period: str = "24h") -> Dict[str, Any]:
        """
        Get transpilation analytics
        
        Args:
            period: Analytics period (24h, 7d, 30d, 90d)
            
        Returns:
            Analytics data
        """
        try:
            response = self.http.get(f"{self.base_url}/transpile/analytics", 
                                   params={"period": period})
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get analytics: {str(e)}")
    
    def export_results(self, format: str = "json", filters: Optional[Dict[str, Any]] = None) -> Dict[str, str]:
        """
        Export transpilation results
        
        Args:
            format: Export format (json, csv, excel)
            filters: Export filters
            
        Returns:
            Download URL and expiration
        """
        try:
            data = filters or {}
            response = self.http.post(f"{self.base_url}/transpile/export", 
                                    json=data, params={"format": format})
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Export failed: {str(e)}")
    
    def getQuota(self) -> Dict[str, Any]:
        """Get quota information (camelCase alias)"""
        return self.get_quota()
    
    def get_quota(self) -> Dict[str, Any]:
        """
        Get quota information
        
        Returns:
            Quota usage information
        """
        try:
            response = self.http.get(f"{self.base_url}/transpile/quota")
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Failed to get quota information: {str(e)}")
    
    def estimateCost(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """Estimate transpilation cost (camelCase alias)"""
        return self.estimate_cost(request)
    
    def estimate_cost(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Estimate transpilation cost
        
        Args:
            request: Cost estimation request
            
        Returns:
            Cost estimation
        """
        try:
            response = self.http.post(f"{self.base_url}/transpile/cost-estimate", json=request)
            return response.json()["data"]
            
        except requests.RequestException as e:
            raise LegacyBaaSException(f"Cost estimation failed: {str(e)}")


# Async version of the service
class AsyncCobolTranspilerService:
    """Async COBOL Transpiler Service Implementation"""
    
    def __init__(self, http_client):
        self.http = http_client
        self.base_url = "/banking"
    
    async def transpile(self, request: CobolTranspileRequest) -> CobolTranspileResult:
        """Async version of transpile method"""
        # Implementation similar to sync version but using aiohttp
        pass
    
    async def batch_transpile(self, files: List[Dict[str, Any]], banking_system: str, 
                             options: Optional[Dict[str, Any]] = None) -> List[CobolTranspileResult]:
        """Async version of batch_transpile method"""
        pass
    
    async def monitor_status_async(self, job_id: str, poll_interval: int = 2):
        """Async version of status monitoring"""
        while True:
            try:
                status = await self.get_status(job_id)
                yield status
                
                if status.get("progress", 0) >= 100:
                    break
                    
                await asyncio.sleep(poll_interval)
                
            except Exception as e:
                yield {"error": str(e)}
                break