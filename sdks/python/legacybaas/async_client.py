"""
Async Legacy B2BaaS Client for Python
"""

import asyncio
import logging
from typing import Optional, Dict, Any
import aiohttp


class AsyncLegacyBaaSClient:
    """Async Legacy B2BaaS Platform Client"""
    
    def __init__(self, api_key: str, **kwargs):
        self.api_key = api_key
        self.session = None
    
    async def __aenter__(self):
        self.session = aiohttp.ClientSession()
        return self
    
    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()
    
    async def close(self):
        if self.session:
            await self.session.close()