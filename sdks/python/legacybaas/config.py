"""
Configuration module for Legacy B2BaaS Python SDK
"""

from enum import Enum
from typing import Optional
from dataclasses import dataclass


class Environment(Enum):
    """Supported environments"""
    PRODUCTION = "production"
    STAGING = "staging"
    SANDBOX = "sandbox"
    
    def get_base_url(self) -> str:
        """Get base URL for environment"""
        urls = {
            Environment.PRODUCTION: "https://api.legacybaas.com",
            Environment.STAGING: "https://staging-api.legacybaas.com", 
            Environment.SANDBOX: "https://sandbox-api.legacybaas.com"
        }
        return urls[self]


@dataclass
class Config:
    """SDK configuration"""
    base_url: str = "https://api.legacybaas.com"
    version: str = "v1"
    timeout: float = 30.0
    max_retries: int = 3
    retry_delay: float = 1.0
    enable_logging: bool = True
    log_level: str = "INFO"
    
    @classmethod
    def for_environment(cls, environment: Environment) -> "Config":
        """Create config for specific environment"""
        return cls(base_url=environment.get_base_url())