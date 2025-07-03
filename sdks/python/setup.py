"""
Banking Legacy-to-Blockchain B2BaaS Platform SDK for Python
Setup configuration for pip installation
"""

from setuptools import setup, find_packages
import os

# Read README file
with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

# Read requirements
with open("requirements.txt", "r", encoding="utf-8") as fh:
    requirements = [line.strip() for line in fh if line.strip() and not line.startswith("#")]

setup(
    name="legacybaas-python-sdk",
    version="1.0.0",
    author="Legacy B2BaaS Platform",
    author_email="sdk@legacybaas.com",
    description="Banking Legacy-to-Blockchain B2BaaS Platform SDK for Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/legacybaas/python-sdk",
    project_urls={
        "Bug Tracker": "https://github.com/legacybaas/python-sdk/issues",
        "Documentation": "https://docs.legacybaas.com/sdks/python",
        "Homepage": "https://legacybaas.com",
    },
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Developers",
        "Intended Audience :: Financial and Insurance Industry",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Programming Language :: Python :: 3.12",
        "Topic :: Office/Business :: Financial",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Internet :: WWW/HTTP :: Dynamic Content",
    ],
    packages=find_packages(exclude=["tests*"]),
    python_requires=">=3.8",
    install_requires=requirements,
    extras_require={
        "dev": [
            "pytest>=7.0.0",
            "pytest-cov>=4.0.0",
            "pytest-asyncio>=0.21.0",
            "responses>=0.23.0",
            "black>=23.0.0",
            "isort>=5.12.0",
            "flake8>=6.0.0",
            "mypy>=1.0.0",
            "sphinx>=6.0.0",
            "sphinx-rtd-theme>=1.2.0",
        ],
        "async": [
            "aiohttp>=3.8.0",
            "asyncio-mqtt>=0.11.0",
        ],
        "django": [
            "django>=4.0.0",
            "djangorestframework>=3.14.0",
        ],
        "flask": [
            "flask>=2.2.0",
            "flask-restful>=0.3.9",
        ],
    },
    keywords=[
        "banking",
        "blockchain",
        "b2baas",
        "swift",
        "payments",
        "legacy-banking",
        "multi-blockchain",
        "financial-services",
        "sdk",
        "api-client",
        "fintech",
    ],
    include_package_data=True,
    package_data={
        "legacybaas": ["py.typed"],
    },
    entry_points={
        "console_scripts": [
            "legacybaas-cli=legacybaas.cli:main",
        ],
    },
)