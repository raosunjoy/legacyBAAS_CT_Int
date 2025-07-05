/**
 * Transpiler Schema Database Tests
 * Test suite for COBOL transpiler database schema extensions
 * 
 * Task 2.5 - Database Schema Extensions
 * Banking Legacy-to-Blockchain B2BaaS Platform
 */

const sqlite3 = require('sqlite3').verbose();
const path = require('path');
const fs = require('fs');

describe('Transpiler Database Schema Tests', () => {
  let db;
  let migrationSql;
  const testDbPath = path.join(__dirname, '../../temp/test_transpiler_schema.db');

  beforeAll(async () => {
    // Read the migration SQL file
    const migrationPath = path.join(__dirname, '../../database/migrations/add-transpiler-tables.sql');
    const fullMigrationSql = fs.readFileSync(migrationPath, 'utf8');
    
    // Remove sample data and verification queries for clean testing
    const lines = fullMigrationSql.split('\n');
    const cleanLines = [];
    let inSampleSection = false;
    
    for (const line of lines) {
      if (line.includes('-- Sample Data for Testing')) {
        inSampleSection = true;
        continue;
      }
      if (line.includes('-- Migration Complete')) {
        break;
      }
      if (!inSampleSection) {
        cleanLines.push(line);
      }
    }
    
    migrationSql = cleanLines.join('\n');
    
    // Ensure temp directory exists
    const tempDir = path.dirname(testDbPath);
    if (!fs.existsSync(tempDir)) {
      fs.mkdirSync(tempDir, { recursive: true });
    }
  });

  beforeEach(async () => {
    // Create fresh database for each test
    db = new sqlite3.Database(testDbPath);
    
    // Execute migration
    await new Promise((resolve, reject) => {
      db.exec(migrationSql, (err) => {
        if (err) reject(err);
        else resolve();
      });
    });
  });

  afterEach(async () => {
    // Close database connection
    if (db) {
      await new Promise((resolve) => {
        db.close((err) => {
          if (err) console.error('Error closing database:', err);
          resolve();
        });
      });
    }
    
    // Delete the test database file to ensure clean state
    if (fs.existsSync(testDbPath)) {
      fs.unlinkSync(testDbPath);
    }
  });

  afterAll(() => {
    // Clean up test database file
    if (fs.existsSync(testDbPath)) {
      fs.unlinkSync(testDbPath);
    }
  });

  /**
   * Test 1: Schema Creation and Structure Validation
   */
  describe('Schema Creation and Structure', () => {
    test('should create transpilation_projects table with correct schema', async () => {
      const tableInfo = await new Promise((resolve, reject) => {
        db.all("PRAGMA table_info(transpilation_projects)", (err, rows) => {
          if (err) reject(err);
          else resolve(rows);
        });
      });

      expect(tableInfo).toHaveLength(17);
      
      const expectedColumns = [
        'id', 'customer_id', 'project_name', 'banking_system', 'target_blockchain',
        'cobol_files_count', 'generated_contracts_count', 'project_status',
        'configuration_data', 'created_at', 'updated_at', 'completed_at',
        'deployment_transaction_hash', 'error_message', 'template_version',
        'estimated_cost', 'actual_cost'
      ];

      const actualColumns = tableInfo.map(col => col.name);
      expectedColumns.forEach(col => {
        expect(actualColumns).toContain(col);
      });

      // Verify primary key
      const primaryKey = tableInfo.find(col => col.pk === 1);
      expect(primaryKey.name).toBe('id');
    });

    test('should create transpilation_usage table with correct schema', async () => {
      const tableInfo = await new Promise((resolve, reject) => {
        db.all("PRAGMA table_info(transpilation_usage)", (err, rows) => {
          if (err) reject(err);
          else resolve(rows);
        });
      });

      expect(tableInfo).toHaveLength(11);
      
      const expectedColumns = [
        'id', 'customer_id', 'project_id', 'usage_type', 'usage_count',
        'usage_date', 'billing_period', 'unit_cost', 'total_cost',
        'metadata', 'processed_at'
      ];

      const actualColumns = tableInfo.map(col => col.name);
      expectedColumns.forEach(col => {
        expect(actualColumns).toContain(col);
      });
    });

    test('should create customers table with cobol_features column', async () => {
      const tableInfo = await new Promise((resolve, reject) => {
        db.all("PRAGMA table_info(customers)", (err, rows) => {
          if (err) reject(err);
          else resolve(rows);
        });
      });

      const cobolFeaturesColumn = tableInfo.find(col => col.name === 'cobol_features');
      expect(cobolFeaturesColumn).toBeDefined();
      expect(cobolFeaturesColumn.type).toBe('TEXT');
      expect(cobolFeaturesColumn.dflt_value).toBe("'{}'");
    });

    test('should create all required indexes', async () => {
      const indexes = await new Promise((resolve, reject) => {
        db.all("SELECT name FROM sqlite_master WHERE type='index' AND name LIKE 'idx_transpilation_%'", (err, rows) => {
          if (err) reject(err);
          else resolve(rows);
        });
      });

      const expectedIndexes = [
        'idx_transpilation_projects_customer_id',
        'idx_transpilation_projects_status',
        'idx_transpilation_projects_banking_system',
        'idx_transpilation_projects_created_at',
        'idx_transpilation_usage_customer_id',
        'idx_transpilation_usage_project_id',
        'idx_transpilation_usage_billing_period',
        'idx_transpilation_usage_usage_date',
        'idx_transpilation_usage_type'
      ];

      const actualIndexes = indexes.map(idx => idx.name);
      expectedIndexes.forEach(idx => {
        expect(actualIndexes).toContain(idx);
      });
    });
  });

  /**
   * Test 2: Data Operations and Constraints
   */
  describe('Data Operations and Constraints', () => {
    test('should insert and retrieve transpilation project data', async () => {
      const projectData = {
        id: 'proj_test_001',
        customer_id: 'cust_test_001',
        project_name: 'Test FIS Modernization',
        banking_system: 'FIS',
        target_blockchain: 'Ethereum',
        cobol_files_count: 5,
        generated_contracts_count: 3,
        project_status: 'completed',
        configuration_data: '{"optimization": "high"}',
        template_version: 'v1.0',
        estimated_cost: 1000.00,
        actual_cost: 950.00
      };

      // Insert project
      await new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO transpilation_projects (
            id, customer_id, project_name, banking_system, target_blockchain,
            cobol_files_count, generated_contracts_count, project_status,
            configuration_data, template_version, estimated_cost, actual_cost
          ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        `, [
          projectData.id, projectData.customer_id, projectData.project_name,
          projectData.banking_system, projectData.target_blockchain,
          projectData.cobol_files_count, projectData.generated_contracts_count,
          projectData.project_status, projectData.configuration_data,
          projectData.template_version, projectData.estimated_cost, projectData.actual_cost
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      });

      // Retrieve and verify
      const retrievedProject = await new Promise((resolve, reject) => {
        db.get("SELECT * FROM transpilation_projects WHERE id = ?", [projectData.id], (err, row) => {
          if (err) reject(err);
          else resolve(row);
        });
      });

      expect(retrievedProject).toBeDefined();
      expect(retrievedProject.id).toBe(projectData.id);
      expect(retrievedProject.customer_id).toBe(projectData.customer_id);
      expect(retrievedProject.banking_system).toBe(projectData.banking_system);
      expect(retrievedProject.target_blockchain).toBe(projectData.target_blockchain);
      expect(retrievedProject.cobol_files_count).toBe(projectData.cobol_files_count);
      expect(retrievedProject.actual_cost).toBe(projectData.actual_cost);
    });

    test('should enforce banking_system constraints', async () => {
      const invalidProject = {
        id: 'proj_invalid_001',
        customer_id: 'cust_test_001',
        project_name: 'Invalid Bank Test',
        banking_system: 'INVALID_BANK',
        target_blockchain: 'Ethereum'
      };

      await expect(new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO transpilation_projects (
            id, customer_id, project_name, banking_system, target_blockchain
          ) VALUES (?, ?, ?, ?, ?)
        `, [
          invalidProject.id, invalidProject.customer_id, invalidProject.project_name,
          invalidProject.banking_system, invalidProject.target_blockchain
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      })).rejects.toThrow();
    });

    test('should enforce target_blockchain constraints', async () => {
      const invalidProject = {
        id: 'proj_invalid_002',
        customer_id: 'cust_test_001',
        project_name: 'Invalid Blockchain Test',
        banking_system: 'FIS',
        target_blockchain: 'INVALID_BLOCKCHAIN'
      };

      await expect(new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO transpilation_projects (
            id, customer_id, project_name, banking_system, target_blockchain
          ) VALUES (?, ?, ?, ?, ?)
        `, [
          invalidProject.id, invalidProject.customer_id, invalidProject.project_name,
          invalidProject.banking_system, invalidProject.target_blockchain
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      })).rejects.toThrow();
    });

    test('should enforce usage_type constraints', async () => {
      const invalidUsage = {
        id: 'usage_invalid_001',
        customer_id: 'cust_test_001',
        project_id: 'proj_test_001',
        usage_type: 'INVALID_TYPE',
        billing_period: '2025-07'
      };

      await expect(new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO transpilation_usage (
            id, customer_id, project_id, usage_type, billing_period
          ) VALUES (?, ?, ?, ?, ?)
        `, [
          invalidUsage.id, invalidUsage.customer_id, invalidUsage.project_id,
          invalidUsage.usage_type, invalidUsage.billing_period
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      })).rejects.toThrow();
    });
  });

  /**
   * Test 3: Usage Tracking and Billing Data
   */
  describe('Usage Tracking and Billing', () => {
    beforeEach(async () => {
      // Insert test customer and project
      await new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO customers (id, name, email, cobol_features)
          VALUES ('cust_billing_unique', 'Test Bank', 'billing-test@bank.com', '{"cobol_transpiler": true}')
        `, (err) => {
          if (err) reject(err);
          else resolve();
        });
      });

      await new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO transpilation_projects (
            id, customer_id, project_name, banking_system, target_blockchain, project_status
          ) VALUES ('proj_billing_001', 'cust_billing_unique', 'Billing Test', 'FIS', 'Ethereum', 'completed')
        `, (err) => {
          if (err) reject(err);
          else resolve();
        });
      });
    });

    test('should track usage metrics correctly', async () => {
      const usageEntries = [
        {
          id: 'usage_001',
          customer_id: 'cust_billing_unique',
          project_id: 'proj_billing_001',
          usage_type: 'cobol_file_processed',
          usage_count: 10,
          billing_period: '2025-07',
          unit_cost: 50.00,
          total_cost: 500.00,
          metadata: '{"complexity": "high"}'
        },
        {
          id: 'usage_002',
          customer_id: 'cust_billing_unique',
          project_id: 'proj_billing_001',
          usage_type: 'contract_generated',
          usage_count: 8,
          billing_period: '2025-07',
          unit_cost: 25.00,
          total_cost: 200.00,
          metadata: '{"blockchain": "ethereum"}'
        }
      ];

      // Insert usage entries
      for (const usage of usageEntries) {
        await new Promise((resolve, reject) => {
          db.run(`
            INSERT INTO transpilation_usage (
              id, customer_id, project_id, usage_type, usage_count,
              billing_period, unit_cost, total_cost, metadata
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
          `, [
            usage.id, usage.customer_id, usage.project_id, usage.usage_type,
            usage.usage_count, usage.billing_period, usage.unit_cost,
            usage.total_cost, usage.metadata
          ], (err) => {
            if (err) reject(err);
            else resolve();
          });
        });
      }

      // Verify billing summary
      const billingSummary = await new Promise((resolve, reject) => {
        db.get(`
          SELECT 
            customer_id,
            billing_period,
            SUM(usage_count) as total_usage,
            SUM(total_cost) as total_billing
          FROM transpilation_usage
          WHERE customer_id = ? AND billing_period = ?
          GROUP BY customer_id, billing_period
        `, ['cust_billing_unique', '2025-07'], (err, row) => {
          if (err) reject(err);
          else resolve(row);
        });
      });

      expect(billingSummary).toBeDefined();
      expect(billingSummary.customer_id).toBe('cust_billing_unique');
      expect(billingSummary.billing_period).toBe('2025-07');
      expect(billingSummary.total_usage).toBe(18);
      expect(billingSummary.total_billing).toBe(700.00);
    });

    test('should handle different usage types properly', async () => {
      const usageTypes = ['cobol_file_processed', 'contract_generated', 'contract_deployed', 'validation_run'];
      
      for (let i = 0; i < usageTypes.length; i++) {
        await new Promise((resolve, reject) => {
          db.run(`
            INSERT INTO transpilation_usage (
              id, customer_id, project_id, usage_type, usage_count, billing_period
            ) VALUES (?, ?, ?, ?, ?, ?)
          `, [
            `usage_type_test_${i}`, 'cust_billing_unique', 'proj_billing_001',
            usageTypes[i], 1, '2025-07'
          ], (err) => {
            if (err) reject(err);
            else resolve();
          });
        });
      }

      // Verify all usage types were recorded
      const usageByType = await new Promise((resolve, reject) => {
        db.all(`
          SELECT usage_type, COUNT(*) as count
          FROM transpilation_usage
          WHERE customer_id = ?
          GROUP BY usage_type
        `, ['cust_billing_unique'], (err, rows) => {
          if (err) reject(err);
          else resolve(rows);
        });
      });

      expect(usageByType).toHaveLength(4);
      usageTypes.forEach(type => {
        const typeRecord = usageByType.find(r => r.usage_type === type);
        expect(typeRecord).toBeDefined();
        expect(typeRecord.count).toBe(1);
      });
    });
  });

  /**
   * Test 4: JSON Features and Customer Extensions
   */
  describe('JSON Features and Customer Extensions', () => {
    test('should handle cobol_features JSON column correctly', async () => {
      const customerData = {
        id: 'cust_json_001',
        name: 'Advanced Bank',
        email: 'json-test@bank.com',
        cobol_features: JSON.stringify({
          cobol_transpiler: true,
          max_projects: 100,
          banking_systems: ['FIS', 'Fiserv'],
          target_blockchains: ['Ethereum', 'Corda', 'Algorand'],
          advanced_features: {
            gas_optimization: true,
            custom_templates: true,
            performance_monitoring: true
          }
        })
      };

      // Insert customer with JSON features
      await new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO customers (id, name, email, cobol_features)
          VALUES (?, ?, ?, ?)
        `, [
          customerData.id, customerData.name, customerData.email, customerData.cobol_features
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      });

      // Retrieve and verify JSON parsing
      const retrievedCustomer = await new Promise((resolve, reject) => {
        db.get("SELECT * FROM customers WHERE id = ?", [customerData.id], (err, row) => {
          if (err) reject(err);
          else resolve(row);
        });
      });

      expect(retrievedCustomer).toBeDefined();
      expect(retrievedCustomer.cobol_features).toBe(customerData.cobol_features);
      
      const parsedFeatures = JSON.parse(retrievedCustomer.cobol_features);
      expect(parsedFeatures.cobol_transpiler).toBe(true);
      expect(parsedFeatures.max_projects).toBe(100);
      expect(parsedFeatures.banking_systems).toContain('FIS');
      expect(parsedFeatures.banking_systems).toContain('Fiserv');
      expect(parsedFeatures.advanced_features.gas_optimization).toBe(true);
    });

    test('should reject invalid JSON in cobol_features', async () => {
      await expect(new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO customers (id, name, email, cobol_features)
          VALUES (?, ?, ?, ?)
        `, [
          'cust_invalid_json',
          'Invalid JSON Bank',
          'invalid@bank.com',
          'invalid json string'
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      })).rejects.toThrow();
    });

    test('should handle complex configuration_data JSON in projects', async () => {
      const complexConfig = {
        cobol_version: 'COBOL-85',
        optimization_settings: {
          level: 'high',
          gas_optimization: true,
          memory_optimization: true
        },
        banking_specific: {
          fis_version: '2.1',
          api_endpoints: ['https://api.fis.com/v1', 'https://api.fis.com/v2'],
          security_settings: {
            encryption: 'AES-256',
            authentication: 'OAuth2'
          }
        },
        blockchain_settings: {
          ethereum: {
            network: 'mainnet',
            gas_price_strategy: 'medium'
          }
        }
      };

      await new Promise((resolve, reject) => {
        db.run(`
          INSERT INTO transpilation_projects (
            id, customer_id, project_name, banking_system, target_blockchain,
            configuration_data
          ) VALUES (?, ?, ?, ?, ?, ?)
        `, [
          'proj_complex_config',
          'cust_json_001',
          'Complex Config Test',
          'FIS',
          'Ethereum',
          JSON.stringify(complexConfig)
        ], (err) => {
          if (err) reject(err);
          else resolve();
        });
      });

      const retrievedProject = await new Promise((resolve, reject) => {
        db.get("SELECT * FROM transpilation_projects WHERE id = ?", ['proj_complex_config'], (err, row) => {
          if (err) reject(err);
          else resolve(row);
        });
      });

      expect(retrievedProject).toBeDefined();
      const parsedConfig = JSON.parse(retrievedProject.configuration_data);
      expect(parsedConfig.optimization_settings.level).toBe('high');
      expect(parsedConfig.banking_specific.fis_version).toBe('2.1');
      expect(parsedConfig.blockchain_settings.ethereum.network).toBe('mainnet');
    });
  });
});