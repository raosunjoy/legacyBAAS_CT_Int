"""
SWIFT Message Processing Service
Enhanced SwiftParser integration with blockchain routing
"""

from typing import Dict, Any, List, Optional
from ..errors import LegacyBaaSError, SwiftProcessingError


class SwiftProcessor:
    """SWIFT Message Processor for Python SDK"""
    
    def __init__(self, client):
        self.client = client
    
    def process_message(
        self,
        message: Dict[str, Any],
        options: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Process SWIFT message with enhanced routing
        
        Args:
            message: SWIFT message data
            options: Processing options
            
        Returns:
            Processing result
        """
        default_options = {
            "validate_only": False,
            "async_processing": False,
            "priority_level": "normal",
            "enable_blockchain": True,
            "routing_preference": "fastest"
        }
        
        if options:
            default_options.update(options)
        
        try:
            return self.client._make_request(
                "POST",
                "/swift/process",
                data={
                    "message": message,
                    "options": default_options
                }
            )
        except Exception as e:
            raise SwiftProcessingError(f"SWIFT processing failed: {e}")
    
    def process_mt103(
        self,
        mt103_data: Dict[str, Any],
        options: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Process MT103 Customer Credit Transfer
        
        Args:
            mt103_data: MT103 message data
            options: Processing options
            
        Returns:
            Processing result
        """
        message = {
            "message_id": mt103_data["transaction_reference"],
            "message_type": "MT103",
            "content": self._format_mt103(mt103_data),
            "sender": mt103_data["ordering_customer"],
            "receiver": mt103_data["beneficiary_customer"],
            "timestamp": "",
            "priority": options.get("priority_level", "high") if options else "high"
        }
        
        processing_options = {
            "enable_blockchain": True,
            "priority_level": "high"
        }
        if options:
            processing_options.update(options)
        
        return self.process_message(message, processing_options)
    
    def process_mt202(
        self,
        mt202_data: Dict[str, Any],
        options: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Process MT202 Financial Institution Transfer
        
        Args:
            mt202_data: MT202 message data
            options: Processing options
            
        Returns:
            Processing result
        """
        message = {
            "message_id": mt202_data["transaction_reference"],
            "message_type": "MT202",
            "content": self._format_mt202(mt202_data),
            "sender": mt202_data["account_with_institution"],
            "receiver": mt202_data["beneficiary_institution"],
            "timestamp": "",
            "priority": options.get("priority_level", "high") if options else "high"
        }
        
        processing_options = {
            "enable_blockchain": True,
            "priority_level": "high"
        }
        if options:
            processing_options.update(options)
        
        return self.process_message(message, processing_options)
    
    def parse_message(self, raw_message: str) -> Dict[str, Any]:
        """
        Parse raw SWIFT message
        
        Args:
            raw_message: Raw SWIFT message string
            
        Returns:
            Parsed message
        """
        return self.client._make_request(
            "POST",
            "/swift/parse",
            data={"raw_message": raw_message}
        )
    
    def validate_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """
        Validate SWIFT message format
        
        Args:
            message: SWIFT message to validate
            
        Returns:
            Validation result
        """
        return self.client._make_request(
            "POST",
            "/swift/validate",
            data={"message": message}
        )
    
    def get_message_status(self, message_id: str) -> Dict[str, Any]:
        """
        Get message processing status
        
        Args:
            message_id: Message ID to check
            
        Returns:
            Message status
        """
        return self.client._make_request(
            "GET",
            f"/swift/status/{message_id}"
        )
    
    def get_processing_history(
        self,
        filters: Optional[Dict[str, Any]] = None
    ) -> Dict[str, Any]:
        """
        Get processing history
        
        Args:
            filters: Optional filters
            
        Returns:
            Processing history
        """
        return self.client._make_request(
            "GET",
            "/swift/history",
            params=filters or {}
        )
    
    def get_supported_message_types(self) -> Dict[str, Any]:
        """
        Get supported message types
        
        Returns:
            Supported message types and capabilities
        """
        return self.client._make_request(
            "GET",
            "/swift/message-types"
        )
    
    def get_metrics(self) -> Dict[str, Any]:
        """
        Get SWIFT network metrics
        
        Returns:
            Network metrics
        """
        return self.client._make_request(
            "GET",
            "/swift/metrics"
        )
    
    def _format_mt103(self, data: Dict[str, Any]) -> str:
        """Format MT103 message"""
        lines = [
            f"{{1:{data['transaction_reference']}}}",
            f"{{2:{data['amount']}{data['currency']}}}",
            f"{{3:{data['value_date']}}}",
            "{4:",
            f":20:{data['transaction_reference']}",
        ]
        
        if data.get("bank_operation_code"):
            lines.append(f":23B:{data['bank_operation_code']}")
        
        if data.get("instruction_code"):
            lines.append(f":23E:{data['instruction_code']}")
        
        lines.extend([
            f":32A:{data['value_date']}{data['currency']}{data['amount']}",
            f":50K:{data['ordering_customer']}",
        ])
        
        if data.get("ordering_institution"):
            lines.append(f":52A:{data['ordering_institution']}")
        
        if data.get("senders_correspondent"):
            lines.append(f":53A:{data['senders_correspondent']}")
        
        if data.get("receivers_correspondent"):
            lines.append(f":54A:{data['receivers_correspondent']}")
        
        if data.get("intermediary_institution"):
            lines.append(f":56A:{data['intermediary_institution']}")
        
        if data.get("account_with_institution"):
            lines.append(f":57A:{data['account_with_institution']}")
        
        lines.append(f":59:{data['beneficiary_customer']}")
        
        if data.get("remittance_information"):
            lines.append(f":70:{data['remittance_information']}")
        
        if data.get("sender_to_receiver_info"):
            lines.append(f":72:{data['sender_to_receiver_info']}")
        
        lines.append("-}")
        
        return "\n".join(lines)
    
    def _format_mt202(self, data: Dict[str, Any]) -> str:
        """Format MT202 message"""
        lines = [
            f"{{1:{data['transaction_reference']}}}",
            f"{{2:{data['amount']}{data['currency']}}}",
            f"{{3:{data['value_date']}}}",
            "{4:",
            f":20:{data['transaction_reference']}",
        ]
        
        if data.get("related_reference"):
            lines.append(f":21:{data['related_reference']}")
        
        if data.get("time_indication"):
            lines.append(f":13C:{data['time_indication']}")
        
        lines.extend([
            f":32A:{data['value_date']}{data['currency']}{data['amount']}",
        ])
        
        if data.get("senders_correspondent"):
            lines.append(f":53A:{data['senders_correspondent']}")
        
        if data.get("receivers_correspondent"):
            lines.append(f":54A:{data['receivers_correspondent']}")
        
        if data.get("intermediary_institution"):
            lines.append(f":56A:{data['intermediary_institution']}")
        
        lines.extend([
            f":57A:{data['account_with_institution']}",
            f":58A:{data['beneficiary_institution']}",
            "-}"
        ])
        
        return "\n".join(lines)