-- ===========================================================================
-- COBOL Transpiler Database Migration Script
-- File: add-transpiler-tables.sql
-- Description: Database schema extensions for COBOL transpiler integration
-- Task: 2.5 - Database Schema Extensions
-- Date: 2025-07-05
-- ===========================================================================

-- Create customers table if it doesn't exist (for standalone testing)
-- This ensures the migration can run independently if needed
CREATE TABLE IF NOT EXISTS customers (
    id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    industry TEXT,
    tier TEXT CHECK (tier IN ('basic', 'professional', 'enterprise', 'fortune100')),
    contact_info TEXT, -- JSON blob for contact information
    billing_config TEXT DEFAULT '{}', -- JSON blob for billing configuration
    feature_flags TEXT DEFAULT '{}', -- JSON blob for feature flags
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at TEXT NOT NULL DEFAULT (datetime('now')),
    status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active', 'suspended', 'inactive')),
    cobol_features TEXT DEFAULT '{}' CHECK (json_valid(cobol_features))
);

-- Create transpilation_projects table
-- Stores COBOL transpilation project details and metadata
CREATE TABLE IF NOT EXISTS transpilation_projects (
    id TEXT PRIMARY KEY,
    customer_id TEXT NOT NULL,
    project_name TEXT NOT NULL,
    banking_system TEXT NOT NULL CHECK (banking_system IN ('FIS', 'Fiserv', 'Temenos', 'TCS_BaNCS')),
    target_blockchain TEXT NOT NULL CHECK (target_blockchain IN ('Ethereum', 'Corda', 'Algorand', 'XRP')),
    cobol_files_count INTEGER DEFAULT 0,
    generated_contracts_count INTEGER DEFAULT 0,
    project_status TEXT NOT NULL DEFAULT 'created' CHECK (project_status IN ('created', 'processing', 'completed', 'failed', 'deployed')),
    configuration_data TEXT, -- JSON blob for project-specific configuration
    created_at TEXT NOT NULL DEFAULT (datetime('now')),
    updated_at TEXT NOT NULL DEFAULT (datetime('now')),
    completed_at TEXT,
    deployment_transaction_hash TEXT,
    error_message TEXT,
    template_version TEXT DEFAULT 'v1.0',
    estimated_cost REAL DEFAULT 0.0,
    actual_cost REAL DEFAULT 0.0,
    
    -- Indexes for performance
    FOREIGN KEY (customer_id) REFERENCES customers(id) ON DELETE CASCADE
);

-- Create transpilation_usage table
-- Tracks usage metrics for billing and analytics
CREATE TABLE IF NOT EXISTS transpilation_usage (
    id TEXT PRIMARY KEY,
    customer_id TEXT NOT NULL,
    project_id TEXT NOT NULL,
    usage_type TEXT NOT NULL CHECK (usage_type IN ('cobol_file_processed', 'contract_generated', 'contract_deployed', 'validation_run')),
    usage_count INTEGER DEFAULT 1,
    usage_date TEXT NOT NULL DEFAULT (datetime('now')),
    billing_period TEXT NOT NULL, -- Format: YYYY-MM for monthly billing
    unit_cost REAL DEFAULT 0.0,
    total_cost REAL DEFAULT 0.0,
    metadata TEXT, -- JSON blob for additional usage metadata
    processed_at TEXT NOT NULL DEFAULT (datetime('now')),
    
    -- Indexes for performance
    FOREIGN KEY (customer_id) REFERENCES customers(id) ON DELETE CASCADE,
    FOREIGN KEY (project_id) REFERENCES transpilation_projects(id) ON DELETE CASCADE
);

-- Create performance indexes for transpilation_projects
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_customer_id 
    ON transpilation_projects(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_status 
    ON transpilation_projects(project_status);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_banking_system 
    ON transpilation_projects(banking_system);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_created_at 
    ON transpilation_projects(created_at);

-- Create performance indexes for transpilation_usage
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_customer_id 
    ON transpilation_usage(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_project_id 
    ON transpilation_usage(project_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_billing_period 
    ON transpilation_usage(billing_period);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_usage_date 
    ON transpilation_usage(usage_date);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_type 
    ON transpilation_usage(usage_type);

-- Create performance indexes for customers table
CREATE INDEX IF NOT EXISTS idx_customers_email ON customers(email);
CREATE INDEX IF NOT EXISTS idx_customers_status ON customers(status);
CREATE INDEX IF NOT EXISTS idx_customers_tier ON customers(tier);
CREATE INDEX IF NOT EXISTS idx_customers_industry ON customers(industry);


-- ===========================================================================
-- Sample Data for Testing (Optional - Remove in production)
-- ===========================================================================

-- Insert sample customer data for testing
INSERT OR IGNORE INTO customers (
    id, name, email, industry, tier, 
    contact_info, billing_config, feature_flags, 
    cobol_features
) VALUES (
    'cust_001_fis_bank',
    'First National Bank (FIS)',
    'integration@firstnational.com',
    'banking',
    'enterprise',
    '{"primary_contact": "John Smith", "phone": "+1-555-0123", "address": "123 Banking Ave, New York, NY"}',
    '{"billing_cycle": "monthly", "payment_method": "invoice", "currency": "USD"}',
    '{"swift_parser": true, "blockchain_routing": true, "compliance_screening": true}',
    '{"cobol_transpiler": true, "max_projects": 50, "banking_systems": ["FIS"], "target_blockchains": ["Ethereum", "Corda"]}'
);

INSERT OR IGNORE INTO customers (
    id, name, email, industry, tier, 
    contact_info, billing_config, feature_flags, 
    cobol_features
) VALUES (
    'cust_002_fiserv_bank',
    'Metro Credit Union (Fiserv)',
    'tech@metrocreditunion.com',
    'banking',
    'professional',
    '{"primary_contact": "Sarah Johnson", "phone": "+1-555-0456", "address": "456 Credit Union Blvd, Chicago, IL"}',
    '{"billing_cycle": "monthly", "payment_method": "credit_card", "currency": "USD"}',
    '{"swift_parser": true, "blockchain_routing": true}',
    '{"cobol_transpiler": true, "max_projects": 25, "banking_systems": ["Fiserv"], "target_blockchains": ["Algorand", "XRP"]}'
);

-- Insert sample transpilation project
INSERT OR IGNORE INTO transpilation_projects (
    id, customer_id, project_name, banking_system, target_blockchain,
    cobol_files_count, generated_contracts_count, project_status,
    configuration_data, template_version, estimated_cost, actual_cost
) VALUES (
    'proj_001_fis_mortgage',
    'cust_001_fis_bank',
    'Mortgage Processing Modernization',
    'FIS',
    'Ethereum',
    12,
    8,
    'completed',
    '{"cobol_version": "COBOL-85", "optimization_level": "high", "contract_type": "payment_processing", "gas_optimization": true}',
    'v1.0',
    5000.00,
    4750.00
);

-- Insert sample usage data
INSERT OR IGNORE INTO transpilation_usage (
    id, customer_id, project_id, usage_type, usage_count,
    billing_period, unit_cost, total_cost, metadata
) VALUES (
    'usage_001_cobol_process',
    'cust_001_fis_bank',
    'proj_001_fis_mortgage',
    'cobol_file_processed',
    12,
    '2025-07',
    100.00,
    1200.00,
    '{"file_sizes": [1024, 2048, 1536], "processing_time_ms": [2500, 3200, 2100], "complexity_score": "medium"}'
);

-- ===========================================================================
-- Migration Verification Queries
-- ===========================================================================

-- Verify tables were created
SELECT name FROM sqlite_master WHERE type='table' AND name IN ('transpilation_projects', 'transpilation_usage', 'customers');

-- Verify indexes were created
SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_transpilation_%';

-- Verify customer table has cobol_features column
PRAGMA table_info(customers);

-- ===========================================================================
-- Migration Complete
-- ===========================================================================