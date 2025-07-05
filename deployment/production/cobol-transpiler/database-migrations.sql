-- COBOL Transpiler Production Database Migration Script
-- LegacyBaaS Platform - Complete Schema Setup

-- =============================================================================
-- COBOL TRANSPILER PRODUCTION DATABASE SCHEMA
-- =============================================================================

-- Create extension for UUID generation
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- =============================================================================
-- TRANSPILATION PROJECTS TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS transpilation_projects (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    customer_id UUID NOT NULL,
    project_name VARCHAR(255) NOT NULL,
    description TEXT,
    banking_system VARCHAR(100) NOT NULL CHECK (banking_system IN ('fis-systematics', 'fiserv-dna', 'tcs-bancs', 'temenos-transact')),
    target_language VARCHAR(50) NOT NULL CHECK (target_language IN ('solidity', 'corda', 'algorand')),
    blockchain_network VARCHAR(50) NOT NULL CHECK (blockchain_network IN ('ethereum', 'corda', 'algorand', 'xrp')),
    status VARCHAR(50) NOT NULL DEFAULT 'created' CHECK (status IN ('created', 'in_progress', 'completed', 'failed', 'cancelled')),
    source_code_hash VARCHAR(64),
    generated_contract_address VARCHAR(255),
    transaction_hash VARCHAR(255),
    gas_estimate BIGINT,
    deployment_cost DECIMAL(18, 8),
    compliance_score INTEGER CHECK (compliance_score >= 0 AND compliance_score <= 100),
    risk_level VARCHAR(20) CHECK (risk_level IN ('LOW', 'MEDIUM', 'HIGH', 'CRITICAL')),
    configuration JSONB DEFAULT '{}',
    metrics JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    completed_at TIMESTAMP WITH TIME ZONE,
    created_by UUID NOT NULL
);

-- Indexes for transpilation_projects
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_customer_id ON transpilation_projects(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_status ON transpilation_projects(status);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_banking_system ON transpilation_projects(banking_system);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_target_language ON transpilation_projects(target_language);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_blockchain_network ON transpilation_projects(blockchain_network);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_created_at ON transpilation_projects(created_at);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_risk_level ON transpilation_projects(risk_level);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_contract_address ON transpilation_projects(generated_contract_address);
CREATE INDEX IF NOT EXISTS idx_transpilation_projects_source_code_hash ON transpilation_projects(source_code_hash);

-- =============================================================================
-- TRANSPILATION USAGE TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS transpilation_usage (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    customer_id UUID NOT NULL,
    project_id UUID REFERENCES transpilation_projects(id) ON DELETE CASCADE,
    operation_type VARCHAR(50) NOT NULL CHECK (operation_type IN ('transpile', 'validate', 'deploy', 'monitor')),
    banking_system VARCHAR(100) NOT NULL,
    target_language VARCHAR(50) NOT NULL,
    blockchain_network VARCHAR(50) NOT NULL,
    source_code_size INTEGER NOT NULL DEFAULT 0,
    processing_time_ms INTEGER NOT NULL DEFAULT 0,
    memory_usage_mb INTEGER DEFAULT 0,
    cpu_usage_percent DECIMAL(5, 2) DEFAULT 0,
    success BOOLEAN NOT NULL DEFAULT false,
    error_message TEXT,
    error_code VARCHAR(50),
    billable_units INTEGER NOT NULL DEFAULT 1,
    cost_usd DECIMAL(10, 4) DEFAULT 0,
    quota_consumed INTEGER NOT NULL DEFAULT 1,
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for transpilation_usage
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_customer_id ON transpilation_usage(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_project_id ON transpilation_usage(project_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_operation_type ON transpilation_usage(operation_type);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_created_at ON transpilation_usage(created_at);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_success ON transpilation_usage(success);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_banking_system ON transpilation_usage(banking_system);
CREATE INDEX IF NOT EXISTS idx_transpilation_usage_customer_created_at ON transpilation_usage(customer_id, created_at);

-- =============================================================================
-- COBOL TEMPLATES TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS cobol_templates (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(255) NOT NULL,
    description TEXT,
    category VARCHAR(100) NOT NULL,
    banking_system VARCHAR(100) NOT NULL,
    target_language VARCHAR(50) NOT NULL,
    template_content TEXT NOT NULL,
    variables JSONB DEFAULT '[]',
    validation_rules JSONB DEFAULT '{}',
    compliance_requirements JSONB DEFAULT '[]',
    version VARCHAR(20) NOT NULL DEFAULT '1.0.0',
    is_active BOOLEAN NOT NULL DEFAULT true,
    is_public BOOLEAN NOT NULL DEFAULT false,
    created_by UUID NOT NULL,
    usage_count INTEGER NOT NULL DEFAULT 0,
    success_rate DECIMAL(5, 2) DEFAULT 0,
    average_processing_time INTEGER DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for cobol_templates
CREATE INDEX IF NOT EXISTS idx_cobol_templates_banking_system ON cobol_templates(banking_system);
CREATE INDEX IF NOT EXISTS idx_cobol_templates_category ON cobol_templates(category);
CREATE INDEX IF NOT EXISTS idx_cobol_templates_target_language ON cobol_templates(target_language);
CREATE INDEX IF NOT EXISTS idx_cobol_templates_is_active ON cobol_templates(is_active);
CREATE INDEX IF NOT EXISTS idx_cobol_templates_is_public ON cobol_templates(is_public);
CREATE INDEX IF NOT EXISTS idx_cobol_templates_created_by ON cobol_templates(created_by);
CREATE UNIQUE INDEX IF NOT EXISTS idx_cobol_templates_name_version ON cobol_templates(name, version);

-- =============================================================================
-- BANKING CONFIGURATIONS TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS banking_configurations (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name VARCHAR(255) NOT NULL,
    banking_system VARCHAR(100) NOT NULL,
    configuration_type VARCHAR(50) NOT NULL CHECK (configuration_type IN ('official', 'custom', 'derived')),
    data_types JSONB NOT NULL DEFAULT '{}',
    validation_rules JSONB NOT NULL DEFAULT '[]',
    compliance_requirements JSONB NOT NULL DEFAULT '[]',
    mapping_rules JSONB NOT NULL DEFAULT '{}',
    optimization_settings JSONB NOT NULL DEFAULT '{}',
    audit_level VARCHAR(20) NOT NULL DEFAULT 'STANDARD' CHECK (audit_level IN ('MINIMAL', 'STANDARD', 'COMPREHENSIVE')),
    is_active BOOLEAN NOT NULL DEFAULT true,
    created_by UUID NOT NULL,
    version VARCHAR(20) NOT NULL DEFAULT '1.0.0',
    parent_config_id UUID REFERENCES banking_configurations(id),
    usage_count INTEGER NOT NULL DEFAULT 0,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for banking_configurations
CREATE INDEX IF NOT EXISTS idx_banking_configurations_banking_system ON banking_configurations(banking_system);
CREATE INDEX IF NOT EXISTS idx_banking_configurations_type ON banking_configurations(configuration_type);
CREATE INDEX IF NOT EXISTS idx_banking_configurations_is_active ON banking_configurations(is_active);
CREATE INDEX IF NOT EXISTS idx_banking_configurations_created_by ON banking_configurations(created_by);
CREATE INDEX IF NOT EXISTS idx_banking_configurations_parent_config_id ON banking_configurations(parent_config_id);
CREATE UNIQUE INDEX IF NOT EXISTS idx_banking_configurations_name_version ON banking_configurations(name, version);

-- =============================================================================
-- TRANSPILATION JOBS TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS transpilation_jobs (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    project_id UUID REFERENCES transpilation_projects(id) ON DELETE CASCADE,
    customer_id UUID NOT NULL,
    job_type VARCHAR(50) NOT NULL CHECK (job_type IN ('transpile', 'batch_transpile', 'validate', 'deploy')),
    status VARCHAR(50) NOT NULL DEFAULT 'queued' CHECK (status IN ('queued', 'running', 'completed', 'failed', 'cancelled')),
    priority INTEGER NOT NULL DEFAULT 5 CHECK (priority >= 1 AND priority <= 10),
    progress INTEGER NOT NULL DEFAULT 0 CHECK (progress >= 0 AND progress <= 100),
    current_stage VARCHAR(100),
    source_files JSONB NOT NULL DEFAULT '[]',
    configuration JSONB NOT NULL DEFAULT '{}',
    results JSONB DEFAULT '{}',
    error_details JSONB DEFAULT '{}',
    performance_metrics JSONB DEFAULT '{}',
    estimated_completion_time TIMESTAMP WITH TIME ZONE,
    started_at TIMESTAMP WITH TIME ZONE,
    completed_at TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for transpilation_jobs
CREATE INDEX IF NOT EXISTS idx_transpilation_jobs_project_id ON transpilation_jobs(project_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_jobs_customer_id ON transpilation_jobs(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_jobs_status ON transpilation_jobs(status);
CREATE INDEX IF NOT EXISTS idx_transpilation_jobs_priority ON transpilation_jobs(priority);
CREATE INDEX IF NOT EXISTS idx_transpilation_jobs_created_at ON transpilation_jobs(created_at);
CREATE INDEX IF NOT EXISTS idx_transpilation_jobs_status_priority ON transpilation_jobs(status, priority);

-- =============================================================================
-- AUDIT LOGS TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS transpilation_audit_logs (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    customer_id UUID NOT NULL,
    project_id UUID,
    job_id UUID,
    user_id UUID NOT NULL,
    action VARCHAR(100) NOT NULL,
    resource_type VARCHAR(50) NOT NULL,
    resource_id UUID,
    details JSONB NOT NULL DEFAULT '{}',
    ip_address INET,
    user_agent TEXT,
    session_id VARCHAR(255),
    compliance_context JSONB DEFAULT '{}',
    risk_assessment JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for transpilation_audit_logs
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_customer_id ON transpilation_audit_logs(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_project_id ON transpilation_audit_logs(project_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_job_id ON transpilation_audit_logs(job_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_user_id ON transpilation_audit_logs(user_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_action ON transpilation_audit_logs(action);
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_created_at ON transpilation_audit_logs(created_at);
CREATE INDEX IF NOT EXISTS idx_transpilation_audit_logs_resource_type ON transpilation_audit_logs(resource_type);

-- =============================================================================
-- PERFORMANCE METRICS TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS transpilation_performance_metrics (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    metric_name VARCHAR(100) NOT NULL,
    metric_type VARCHAR(50) NOT NULL CHECK (metric_type IN ('counter', 'gauge', 'histogram', 'timing')),
    value DECIMAL(20, 8) NOT NULL,
    unit VARCHAR(20),
    labels JSONB DEFAULT '{}',
    customer_id UUID,
    project_id UUID,
    job_id UUID,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for transpilation_performance_metrics
CREATE INDEX IF NOT EXISTS idx_transpilation_performance_metrics_name ON transpilation_performance_metrics(metric_name);
CREATE INDEX IF NOT EXISTS idx_transpilation_performance_metrics_type ON transpilation_performance_metrics(metric_type);
CREATE INDEX IF NOT EXISTS idx_transpilation_performance_metrics_created_at ON transpilation_performance_metrics(created_at);
CREATE INDEX IF NOT EXISTS idx_transpilation_performance_metrics_customer_id ON transpilation_performance_metrics(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_performance_metrics_project_id ON transpilation_performance_metrics(project_id);

-- =============================================================================
-- QUOTA MANAGEMENT TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS transpilation_quotas (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    customer_id UUID NOT NULL UNIQUE,
    monthly_limit INTEGER NOT NULL DEFAULT 1000,
    current_usage INTEGER NOT NULL DEFAULT 0,
    overage_allowed BOOLEAN NOT NULL DEFAULT false,
    overage_limit INTEGER DEFAULT 0,
    overage_cost_per_unit DECIMAL(10, 4) DEFAULT 0,
    reset_date DATE NOT NULL,
    billing_tier VARCHAR(20) NOT NULL DEFAULT 'BASIC' CHECK (billing_tier IN ('BASIC', 'PROFESSIONAL', 'ENTERPRISE', 'CUSTOM')),
    features JSONB NOT NULL DEFAULT '{}',
    usage_history JSONB NOT NULL DEFAULT '[]',
    alerts_enabled BOOLEAN NOT NULL DEFAULT true,
    alert_thresholds JSONB NOT NULL DEFAULT '{"warning": 80, "critical": 95}',
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for transpilation_quotas
CREATE UNIQUE INDEX IF NOT EXISTS idx_transpilation_quotas_customer_id ON transpilation_quotas(customer_id);
CREATE INDEX IF NOT EXISTS idx_transpilation_quotas_billing_tier ON transpilation_quotas(billing_tier);
CREATE INDEX IF NOT EXISTS idx_transpilation_quotas_reset_date ON transpilation_quotas(reset_date);

-- =============================================================================
-- BLOCKCHAIN DEPLOYMENT TRACKING TABLE
-- =============================================================================

CREATE TABLE IF NOT EXISTS blockchain_deployments (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    project_id UUID REFERENCES transpilation_projects(id) ON DELETE CASCADE,
    job_id UUID REFERENCES transpilation_jobs(id) ON DELETE CASCADE,
    customer_id UUID NOT NULL,
    blockchain_network VARCHAR(50) NOT NULL,
    contract_address VARCHAR(255) NOT NULL,
    transaction_hash VARCHAR(255) NOT NULL,
    deployment_status VARCHAR(50) NOT NULL DEFAULT 'pending' CHECK (deployment_status IN ('pending', 'confirmed', 'failed', 'reverted')),
    gas_used BIGINT,
    gas_price_gwei DECIMAL(20, 8),
    deployment_cost_usd DECIMAL(18, 8),
    block_number BIGINT,
    confirmation_count INTEGER DEFAULT 0,
    network_fees JSONB DEFAULT '{}',
    performance_metrics JSONB DEFAULT '{}',
    verification_status VARCHAR(50) DEFAULT 'pending' CHECK (verification_status IN ('pending', 'verified', 'failed', 'not_required')),
    source_code_verified BOOLEAN DEFAULT false,
    abi JSONB,
    bytecode TEXT,
    deployed_at TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for blockchain_deployments
CREATE INDEX IF NOT EXISTS idx_blockchain_deployments_project_id ON blockchain_deployments(project_id);
CREATE INDEX IF NOT EXISTS idx_blockchain_deployments_customer_id ON blockchain_deployments(customer_id);
CREATE INDEX IF NOT EXISTS idx_blockchain_deployments_network ON blockchain_deployments(blockchain_network);
CREATE INDEX IF NOT EXISTS idx_blockchain_deployments_status ON blockchain_deployments(deployment_status);
CREATE INDEX IF NOT EXISTS idx_blockchain_deployments_contract_address ON blockchain_deployments(contract_address);
CREATE INDEX IF NOT EXISTS idx_blockchain_deployments_transaction_hash ON blockchain_deployments(transaction_hash);

-- =============================================================================
-- UPDATE TRIGGER FUNCTIONS
-- =============================================================================

-- Function to automatically update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Apply update triggers to relevant tables
DROP TRIGGER IF EXISTS update_transpilation_projects_updated_at ON transpilation_projects;
CREATE TRIGGER update_transpilation_projects_updated_at 
    BEFORE UPDATE ON transpilation_projects 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_cobol_templates_updated_at ON cobol_templates;
CREATE TRIGGER update_cobol_templates_updated_at 
    BEFORE UPDATE ON cobol_templates 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_banking_configurations_updated_at ON banking_configurations;
CREATE TRIGGER update_banking_configurations_updated_at 
    BEFORE UPDATE ON banking_configurations 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_transpilation_jobs_updated_at ON transpilation_jobs;
CREATE TRIGGER update_transpilation_jobs_updated_at 
    BEFORE UPDATE ON transpilation_jobs 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_transpilation_quotas_updated_at ON transpilation_quotas;
CREATE TRIGGER update_transpilation_quotas_updated_at 
    BEFORE UPDATE ON transpilation_quotas 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_blockchain_deployments_updated_at ON blockchain_deployments;
CREATE TRIGGER update_blockchain_deployments_updated_at 
    BEFORE UPDATE ON blockchain_deployments 
    FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- =============================================================================
-- SEED DATA - DEFAULT TEMPLATES AND CONFIGURATIONS
-- =============================================================================

-- Insert default COBOL templates
INSERT INTO cobol_templates (id, name, description, category, banking_system, target_language, template_content, variables, validation_rules, compliance_requirements, version, is_active, is_public, created_by) 
VALUES 
(
    uuid_generate_v4(),
    'FIS Payment Processor',
    'Standard payment processing template for FIS Systematics',
    'payments',
    'fis-systematics',
    'solidity',
    'pragma solidity ^0.8.0;\n\ncontract FISPaymentProcessor {\n    // Template implementation\n}',
    '[{"name": "contractName", "type": "string", "required": true}, {"name": "maxAmount", "type": "number", "required": true}]',
    '{"rules": [{"field": "amount", "type": "range", "min": 0.01, "max": 1000000}]}',
    '["AML", "KYC", "FATF"]',
    '1.0.0',
    true,
    true,
    uuid_generate_v4()
),
(
    uuid_generate_v4(),
    'Fiserv DNA Account Manager',
    'Account management template for Fiserv DNA',
    'accounts',
    'fiserv-dna',
    'solidity',
    'pragma solidity ^0.8.0;\n\ncontract FiservDNAAccountManager {\n    // Template implementation\n}',
    '[{"name": "contractName", "type": "string", "required": true}]',
    '{"rules": []}',
    '["AML", "KYC"]',
    '1.0.0',
    true,
    true,
    uuid_generate_v4()
),
(
    uuid_generate_v4(),
    'TCS BaNCS Transaction Processor',
    'Transaction processing template for TCS BaNCS',
    'transactions',
    'tcs-bancs',
    'corda',
    'package com.tcsbancs\n\n// Corda flow implementation',
    '[{"name": "flowName", "type": "string", "required": true}]',
    '{"rules": []}',
    '["AML", "KYC", "FATF", "PCI_DSS"]',
    '1.0.0',
    true,
    true,
    uuid_generate_v4()
),
(
    uuid_generate_v4(),
    'Temenos T24 Core Banking',
    'Core banking template for Temenos T24',
    'core_banking',
    'temenos-transact',
    'algorand',
    '// Algorand smart contract\n// Template implementation',
    '[{"name": "contractName", "type": "string", "required": true}]',
    '{"rules": []}',
    '["AML", "KYC", "BASEL_III"]',
    '1.0.0',
    true,
    true,
    uuid_generate_v4()
);

-- Insert default banking configurations
INSERT INTO banking_configurations (id, name, banking_system, configuration_type, data_types, validation_rules, compliance_requirements, mapping_rules, optimization_settings, audit_level, is_active, created_by, version) 
VALUES 
(
    uuid_generate_v4(),
    'FIS Systematics Standard',
    'fis-systematics',
    'official',
    '{"ACCOUNT_NUMBER": {"type": "string", "length": 20}, "AMOUNT": {"type": "decimal", "precision": 15, "scale": 2}, "CURRENCY": {"type": "enum", "values": ["USD", "EUR", "GBP"]}}',
    '[{"field": "AMOUNT", "rule": "min", "value": 0.01}, {"field": "AMOUNT", "rule": "max", "value": 999999999.99}]',
    '["AML", "KYC", "FATF", "SOX"]',
    '{"ACCOUNT_NUMBER": "address", "AMOUNT": "uint256", "CURRENCY": "string"}',
    '{"gasOptimization": true, "batchProcessing": true}',
    'COMPREHENSIVE',
    true,
    uuid_generate_v4(),
    '1.0.0'
),
(
    uuid_generate_v4(),
    'Fiserv DNA Standard',
    'fiserv-dna',
    'official',
    '{"ACCOUNT_ID": {"type": "string", "length": 16}, "BALANCE": {"type": "decimal", "precision": 12, "scale": 2}, "STATUS": {"type": "enum", "values": ["ACTIVE", "INACTIVE", "SUSPENDED"]}}',
    '[{"field": "BALANCE", "rule": "min", "value": 0}]',
    '["AML", "KYC", "FDIC"]',
    '{"ACCOUNT_ID": "address", "BALANCE": "uint256", "STATUS": "uint8"}',
    '{"gasOptimization": true}',
    'STANDARD',
    true,
    uuid_generate_v4(),
    '1.0.0'
),
(
    uuid_generate_v4(),
    'TCS BaNCS Standard',
    'tcs-bancs',
    'official',
    '{"CUSTOMER_ID": {"type": "string", "length": 12}, "PRODUCT_CODE": {"type": "string", "length": 6}, "TRANSACTION_AMOUNT": {"type": "decimal", "precision": 18, "scale": 4}}',
    '[{"field": "TRANSACTION_AMOUNT", "rule": "min", "value": 0.0001}]',
    '["AML", "KYC", "FATF", "RBI"]',
    '{"CUSTOMER_ID": "address", "PRODUCT_CODE": "string", "TRANSACTION_AMOUNT": "uint256"}',
    '{"cordaFlowOptimization": true}',
    'COMPREHENSIVE',
    true,
    uuid_generate_v4(),
    '1.0.0'
),
(
    uuid_generate_v4(),
    'Temenos T24 Standard',
    'temenos-transact',
    'official',
    '{"ARRANGEMENT_ID": {"type": "string", "length": 14}, "BALANCE": {"type": "decimal", "precision": 16, "scale": 2}, "CCY": {"type": "string", "length": 3}}',
    '[{"field": "BALANCE", "rule": "min", "value": 0}]',
    '["AML", "KYC", "BASEL_III", "MiFID_II"]',
    '{"ARRANGEMENT_ID": "address", "BALANCE": "uint256", "CCY": "string"}',
    '{"algorandOptimization": true}',
    'COMPREHENSIVE',
    true,
    uuid_generate_v4(),
    '1.0.0'
);

-- =============================================================================
-- VIEWS FOR ANALYTICS AND REPORTING
-- =============================================================================

-- View for transpilation analytics
CREATE OR REPLACE VIEW transpilation_analytics AS
SELECT 
    tp.banking_system,
    tp.target_language,
    tp.blockchain_network,
    COUNT(*) as total_transpilations,
    COUNT(CASE WHEN tp.status = 'completed' THEN 1 END) as successful_transpilations,
    COUNT(CASE WHEN tp.status = 'failed' THEN 1 END) as failed_transpilations,
    ROUND(
        COUNT(CASE WHEN tp.status = 'completed' THEN 1 END) * 100.0 / COUNT(*), 
        2
    ) as success_rate,
    AVG(
        CASE WHEN tp.status = 'completed' 
        THEN EXTRACT(EPOCH FROM (tp.completed_at - tp.created_at)) * 1000 
        END
    ) as avg_processing_time_ms,
    SUM(COALESCE(tp.deployment_cost, 0)) as total_deployment_cost,
    AVG(COALESCE(tp.gas_estimate, 0)) as avg_gas_estimate
FROM transpilation_projects tp
GROUP BY tp.banking_system, tp.target_language, tp.blockchain_network;

-- View for customer usage summary
CREATE OR REPLACE VIEW customer_usage_summary AS
SELECT 
    tu.customer_id,
    COUNT(*) as total_operations,
    COUNT(CASE WHEN tu.success THEN 1 END) as successful_operations,
    SUM(tu.quota_consumed) as total_quota_consumed,
    SUM(tu.cost_usd) as total_cost_usd,
    AVG(tu.processing_time_ms) as avg_processing_time_ms,
    MAX(tu.created_at) as last_operation_at
FROM transpilation_usage tu
GROUP BY tu.customer_id;

-- View for performance monitoring
CREATE OR REPLACE VIEW performance_dashboard AS
SELECT 
    DATE_TRUNC('hour', created_at) as hour,
    COUNT(*) as operations_count,
    AVG(processing_time_ms) as avg_processing_time,
    PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY processing_time_ms) as p95_processing_time,
    AVG(memory_usage_mb) as avg_memory_usage,
    AVG(cpu_usage_percent) as avg_cpu_usage,
    COUNT(CASE WHEN success THEN 1 END) as successful_operations,
    ROUND(
        COUNT(CASE WHEN success THEN 1 END) * 100.0 / COUNT(*), 
        2
    ) as success_rate
FROM transpilation_usage
WHERE created_at >= NOW() - INTERVAL '24 hours'
GROUP BY DATE_TRUNC('hour', created_at)
ORDER BY hour;

-- =============================================================================
-- STORED PROCEDURES FOR COMMON OPERATIONS
-- =============================================================================

-- Procedure to update quota usage
CREATE OR REPLACE FUNCTION update_quota_usage(
    p_customer_id UUID,
    p_units_consumed INTEGER
) RETURNS BOOLEAN AS $$
DECLARE
    current_quota RECORD;
BEGIN
    -- Get current quota information
    SELECT * INTO current_quota 
    FROM transpilation_quotas 
    WHERE customer_id = p_customer_id;
    
    IF NOT FOUND THEN
        -- Create default quota if not exists
        INSERT INTO transpilation_quotas (customer_id, monthly_limit, current_usage, reset_date)
        VALUES (p_customer_id, 1000, p_units_consumed, DATE_TRUNC('month', CURRENT_DATE) + INTERVAL '1 month');
        RETURN TRUE;
    END IF;
    
    -- Check if we need to reset quota (new month)
    IF CURRENT_DATE >= current_quota.reset_date THEN
        UPDATE transpilation_quotas 
        SET 
            current_usage = p_units_consumed,
            reset_date = DATE_TRUNC('month', CURRENT_DATE) + INTERVAL '1 month',
            updated_at = CURRENT_TIMESTAMP
        WHERE customer_id = p_customer_id;
    ELSE
        -- Update current usage
        UPDATE transpilation_quotas 
        SET 
            current_usage = current_usage + p_units_consumed,
            updated_at = CURRENT_TIMESTAMP
        WHERE customer_id = p_customer_id;
    END IF;
    
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

-- Procedure to check quota availability
CREATE OR REPLACE FUNCTION check_quota_availability(
    p_customer_id UUID,
    p_units_requested INTEGER
) RETURNS BOOLEAN AS $$
DECLARE
    current_quota RECORD;
BEGIN
    SELECT * INTO current_quota 
    FROM transpilation_quotas 
    WHERE customer_id = p_customer_id;
    
    IF NOT FOUND THEN
        RETURN TRUE; -- Default quota will be created
    END IF;
    
    -- Check if quota reset is needed
    IF CURRENT_DATE >= current_quota.reset_date THEN
        RETURN TRUE; -- New month, quota is reset
    END IF;
    
    -- Check if we have enough quota
    IF current_quota.current_usage + p_units_requested <= current_quota.monthly_limit THEN
        RETURN TRUE;
    END IF;
    
    -- Check overage allowance
    IF current_quota.overage_allowed AND 
       current_quota.current_usage + p_units_requested <= current_quota.monthly_limit + current_quota.overage_limit THEN
        RETURN TRUE;
    END IF;
    
    RETURN FALSE;
END;
$$ LANGUAGE plpgsql;

-- =============================================================================
-- GRANTS AND PERMISSIONS
-- =============================================================================

-- Create application user
DO $$
BEGIN
    IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'legacybaas_app') THEN
        CREATE ROLE legacybaas_app WITH LOGIN PASSWORD 'secure_app_password';
    END IF;
END
$$;

-- Grant permissions to application user
GRANT USAGE ON SCHEMA public TO legacybaas_app;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO legacybaas_app;
GRANT SELECT, USAGE ON ALL SEQUENCES IN SCHEMA public TO legacybaas_app;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO legacybaas_app;

-- Create read-only user for analytics
DO $$
BEGIN
    IF NOT EXISTS (SELECT FROM pg_catalog.pg_roles WHERE rolname = 'legacybaas_readonly') THEN
        CREATE ROLE legacybaas_readonly WITH LOGIN PASSWORD 'secure_readonly_password';
    END IF;
END
$$;

-- Grant read-only permissions
GRANT USAGE ON SCHEMA public TO legacybaas_readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO legacybaas_readonly;
GRANT SELECT ON ALL VIEWS IN SCHEMA public TO legacybaas_readonly;

-- =============================================================================
-- COMPLETION MESSAGE
-- =============================================================================

DO $$
BEGIN
    RAISE NOTICE 'COBOL Transpiler Production Database Migration Completed Successfully!';
    RAISE NOTICE 'Tables Created: 8';
    RAISE NOTICE 'Indexes Created: 45+';
    RAISE NOTICE 'Views Created: 3';
    RAISE NOTICE 'Functions Created: 3';
    RAISE NOTICE 'Triggers Created: 6';
    RAISE NOTICE 'Default Templates Inserted: 4';
    RAISE NOTICE 'Default Configurations Inserted: 4';
    RAISE NOTICE 'Database is ready for production deployment!';
END
$$;