/**
 * COBOL API Routes Test Suite
 * LegacyBAAS Platform Integration Tests
 * 
 * Tests all COBOL transpiler API endpoints with authentication and validation
 */

const request = require('supertest');
const app = require('../../src/index');
const { AuthManager, USER_ROLES } = require('../../src/auth/auth-middleware');

describe('COBOL API Routes Tests', () => {
  let authManager;
  let validToken;
  let adminToken;

  beforeAll(async () => {
    // Initialize auth manager
    authManager = new AuthManager({
      jwtSecret: 'test-secret',
      enableRateLimit: false,
      enableAuditLog: false
    });

    // Generate test tokens
    validToken = authManager.generateJWT({
      sub: 'test-user',
      customerId: 'test-customer',
      role: USER_ROLES.BANK_ADMIN,
      features: { 
        cobol_transpiler: { 
          enabled: true,
          quota: { transpilations_per_month: 100 }
        }
      }
    });

    adminToken = authManager.generateJWT({
      sub: 'admin-user',
      customerId: 'admin-customer',
      role: USER_ROLES.SYSTEM_ADMIN,
      features: { 
        cobol_transpiler: { 
          enabled: true,
          quota: { transpilations_per_month: 1000 }
        }
      }
    });
  });

  const sampleCobolCode = `
    IDENTIFICATION DIVISION.
    PROGRAM-ID. TEST-PAYMENT.
    AUTHOR. TEST-BANK.
    
    DATA DIVISION.
    WORKING-STORAGE SECTION.
    01 PAYMENT-AMOUNT PIC 9(8)V99 COMP-3.
    01 FEE PIC 9(4)V99 COMP-3.
    01 TOTAL PIC 9(8)V99 COMP-3.
    
    PROCEDURE DIVISION.
    MAIN-PARAGRAPH.
        MOVE 1000.00 TO PAYMENT-AMOUNT.
        COMPUTE FEE = PAYMENT-AMOUNT * 0.025.
        COMPUTE TOTAL = PAYMENT-AMOUNT + FEE.
        IF TOTAL > 500
            DISPLAY "HIGH AMOUNT TRANSACTION".
        STOP RUN.
  `;

  describe('POST /api/v1/cobol/transpile', () => {
    test('should successfully transpile COBOL to smart contract', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          cobolCode: sampleCobolCode,
          bankingSystem: 'FIS_SYSTEMATICS',
          targetBlockchain: 'ethereum',
          optimizeGas: true
        })
        .expect(200);

      expect(response.body).toHaveProperty('success', true);
      expect(response.body).toHaveProperty('requestId');
      expect(response.body).toHaveProperty('transpilation');
      expect(response.body.transpilation).toHaveProperty('ast');
      expect(response.body.transpilation).toHaveProperty('smartContract');
      expect(response.body.transpilation.ast.programId).toBe('TEST-PAYMENT');
      expect(response.body.transpilation.smartContract.blockchain).toBe('ethereum');
      expect(response.body.transpilation.smartContract.contractCode).toContain('pragma solidity');
    });

    test('should reject request without authentication', async () => {
      await request(app)
        .post('/api/v1/cobol/transpile')
        .send({ cobolCode: sampleCobolCode })
        .expect(401);
    });

    test('should reject request without COBOL code', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          bankingSystem: 'FIS_SYSTEMATICS'
        })
        .expect(400);

      expect(response.body.error).toBe('VALIDATION_ERROR');
      expect(response.body.message).toContain('COBOL code is required');
    });

    test('should reject unsupported banking system', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          cobolCode: sampleCobolCode,
          bankingSystem: 'UNSUPPORTED_SYSTEM'
        })
        .expect(400);

      expect(response.body.error).toBe('VALIDATION_ERROR');
      expect(response.body.message).toContain('Unsupported banking system');
    });

    test('should reject unsupported blockchain', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          cobolCode: sampleCobolCode,
          targetBlockchain: 'unsupported-chain'
        })
        .expect(400);

      expect(response.body.error).toBe('VALIDATION_ERROR');
      expect(response.body.message).toContain('Unsupported blockchain platform');
    });

    test('should handle different blockchain targets', async () => {
      const blockchains = ['ethereum', 'corda', 'algorand'];
      
      for (const blockchain of blockchains) {
        const response = await request(app)
          .post('/api/v1/cobol/transpile')
          .set('Authorization', `Bearer ${validToken}`)
          .send({
            cobolCode: sampleCobolCode,
            targetBlockchain: blockchain
          })
          .expect(200);

        expect(response.body.transpilation.smartContract.blockchain).toBe(blockchain);
      }
    });

    test('should include request metadata and links', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: sampleCobolCode })
        .expect(200);

      expect(response.body).toHaveProperty('requestId');
      expect(response.body).toHaveProperty('links');
      expect(response.body.links).toHaveProperty('status');
      expect(response.body.links).toHaveProperty('download');
      expect(response.body.links).toHaveProperty('deploy');
      expect(response.body).toHaveProperty('timestamp');
    });
  });

  describe('POST /api/v1/cobol/parse', () => {
    test('should successfully parse COBOL to AST', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/parse')
        .set('Authorization', `Bearer ${validToken}`)
        .send({
          cobolCode: sampleCobolCode,
          bankingSystem: 'FIS_SYSTEMATICS'
        })
        .expect(200);

      expect(response.body).toHaveProperty('success', true);
      expect(response.body).toHaveProperty('parsing');
      expect(response.body.parsing).toHaveProperty('ast');
      expect(response.body.parsing).toHaveProperty('statistics');
      expect(response.body.parsing.ast.program.programId).toBe('TEST-PAYMENT');
      expect(response.body.parsing.statistics.variableCount).toBeGreaterThan(0);
    });

    test('should reject malformed COBOL', async () => {
      const malformedCobol = 'INVALID COBOL CODE';
      
      await request(app)
        .post('/api/v1/cobol/parse')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: malformedCobol })
        .expect(500);
    });

    test('should handle different banking systems in parsing', async () => {
      const systems = ['FIS_SYSTEMATICS', 'FISERV_DNA', 'TCS_BANCS', 'TEMENOS_TRANSACT'];
      
      for (const system of systems) {
        const response = await request(app)
          .post('/api/v1/cobol/parse')
          .set('Authorization', `Bearer ${validToken}`)
          .send({
            cobolCode: sampleCobolCode,
            bankingSystem: system
          })
          .expect(200);

        expect(response.body.parsing.ast.metadata.bankingSystem).toBe(system);
      }
    });
  });

  describe('GET /api/v1/cobol/templates', () => {
    test('should return available templates', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/templates')
        .set('Authorization', `Bearer ${validToken}`)
        .expect(200);

      expect(response.body).toHaveProperty('success', true);
      expect(response.body).toHaveProperty('templates');
      expect(response.body.templates).toHaveProperty('blockchain');
      expect(response.body.templates).toHaveProperty('bankingSystems');
      expect(response.body.templates).toHaveProperty('useCases');
      
      // Check blockchain templates
      expect(response.body.templates.blockchain).toHaveProperty('ethereum');
      expect(response.body.templates.blockchain).toHaveProperty('corda');
      expect(response.body.templates.blockchain).toHaveProperty('algorand');
    });

    test('should filter templates by blockchain', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/templates?blockchain=ethereum')
        .set('Authorization', `Bearer ${validToken}`)
        .expect(200);

      expect(response.body.templates.blockchain).toHaveProperty('ethereum');
      expect(response.body.templates.blockchain).not.toHaveProperty('corda');
      expect(response.body.templates.blockchain).not.toHaveProperty('algorand');
    });

    test('should filter templates by banking system', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/templates?bankingSystem=FIS_SYSTEMATICS')
        .set('Authorization', `Bearer ${validToken}`)
        .expect(200);

      const fisSystems = response.body.templates.bankingSystems.filter(
        s => s.id === 'FIS_SYSTEMATICS'
      );
      expect(fisSystems).toHaveLength(1);
    });
  });

  describe('POST /api/v1/cobol/validate', () => {
    test('should validate correct COBOL syntax', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/validate')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: sampleCobolCode })
        .expect(200);

      expect(response.body).toHaveProperty('success', true);
      expect(response.body).toHaveProperty('validation');
      expect(response.body.validation.valid).toBe(true);
      expect(response.body.validation.programId).toBe('TEST-PAYMENT');
    });

    test('should reject invalid COBOL syntax', async () => {
      const invalidCobol = `
        IDENTIFICATION DIVISION.
        INVALID SYNTAX HERE
      `;

      const response = await request(app)
        .post('/api/v1/cobol/validate')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: invalidCobol })
        .expect(422);

      expect(response.body.success).toBe(false);
      expect(response.body.validation.valid).toBe(false);
    });

    test('should provide validation warnings', async () => {
      const noVarsCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. NO-VARS.
        
        PROCEDURE DIVISION.
        DISPLAY "HELLO".
      `;

      const response = await request(app)
        .post('/api/v1/cobol/validate')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: noVarsCobol })
        .expect(200);

      expect(response.body.validation.warnings).toContain('No variables found in COBOL program');
    });
  });

  describe('GET /api/v1/cobol/systems', () => {
    test('should return supported banking systems', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/systems')
        .set('Authorization', `Bearer ${validToken}`)
        .expect(200);

      expect(response.body).toHaveProperty('success', true);
      expect(response.body).toHaveProperty('systems');
      expect(response.body).toHaveProperty('totalCount');
      expect(Array.isArray(response.body.systems)).toBe(true);
      expect(response.body.systems.length).toBeGreaterThan(0);

      // Check system structure
      const system = response.body.systems[0];
      expect(system).toHaveProperty('id');
      expect(system).toHaveProperty('name');
      expect(system).toHaveProperty('preferredBlockchains');
      expect(system).toHaveProperty('complianceRequirements');
    });

    test('should include system details', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/systems')
        .set('Authorization', `Bearer ${validToken}`)
        .expect(200);

      const fisSystem = response.body.systems.find(s => s.id === 'FIS_SYSTEMATICS');
      expect(fisSystem).toBeDefined();
      expect(fisSystem.name).toBe('FIS Systematics IBS');
      expect(fisSystem.architecture).toBe('mainframe');
      expect(fisSystem.preferredBlockchains).toContain('corda');
    });
  });

  describe('GET /api/v1/cobol/metrics', () => {
    test('should return transpiler metrics for admin users', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/metrics')
        .set('Authorization', `Bearer ${adminToken}`)
        .expect(200);

      expect(response.body).toHaveProperty('success', true);
      expect(response.body).toHaveProperty('metrics');
      expect(response.body.metrics).toHaveProperty('transpiler');
      expect(response.body.metrics).toHaveProperty('authentication');
      expect(response.body.metrics).toHaveProperty('configuration');
      expect(response.body.metrics).toHaveProperty('platform');
    });

    test('should reject metrics access for non-admin users', async () => {
      const userToken = authManager.generateJWT({
        sub: 'regular-user',
        customerId: 'user-customer',
        role: USER_ROLES.BANK_USER,
        features: { cobol_transpiler: { enabled: true } }
      });

      await request(app)
        .get('/api/v1/cobol/metrics')
        .set('Authorization', `Bearer ${userToken}`)
        .expect(403);
    });
  });

  describe('GET /api/v1/cobol/health', () => {
    test('should return health status', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/health')
        .expect(200);

      expect(response.body).toHaveProperty('status', 'healthy');
      expect(response.body).toHaveProperty('components');
      expect(response.body).toHaveProperty('capabilities');
      expect(response.body.components.transpiler).toBe('operational');
      expect(response.body.capabilities.parsing).toBe(true);
      expect(response.body.capabilities.transpilation).toBe(true);
    });

    test('should not require authentication for health check', async () => {
      await request(app)
        .get('/api/v1/cobol/health')
        .expect(200);
    });
  });

  describe('Authentication and Authorization', () => {
    test('should reject requests with invalid tokens', async () => {
      await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', 'Bearer invalid-token')
        .send({ cobolCode: sampleCobolCode })
        .expect(401);
    });

    test('should reject requests without required permissions', async () => {
      const limitedToken = authManager.generateJWT({
        sub: 'limited-user',
        customerId: 'limited-customer',
        role: USER_ROLES.RESELLER_PARTNER,
        features: { cobol_transpiler: { enabled: true } }
      });

      await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${limitedToken}`)
        .send({ cobolCode: sampleCobolCode })
        .expect(403);
    });

    test('should reject requests for disabled COBOL feature', async () => {
      const disabledToken = authManager.generateJWT({
        sub: 'disabled-user',
        customerId: 'disabled-customer',
        role: USER_ROLES.BANK_ADMIN,
        features: { cobol_transpiler: { enabled: false } }
      });

      await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${disabledToken}`)
        .send({ cobolCode: sampleCobolCode })
        .expect(403);
    });
  });

  describe('Input Validation', () => {
    test('should reject requests with wrong content type', async () => {
      await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .set('Content-Type', 'text/plain')
        .send('invalid content')
        .expect(415);
    });

    test('should reject oversized COBOL code', async () => {
      const oversizedCode = 'A'.repeat(100001); // > 100KB
      
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: oversizedCode })
        .expect(413);

      expect(response.body.error).toBe('PAYLOAD_TOO_LARGE');
    });

    test('should validate COBOL structure requirements', async () => {
      const invalidStructure = 'NO IDENTIFICATION DIVISION HERE';
      
      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: invalidStructure })
        .expect(422);

      expect(response.body.error).toBe('INVALID_COBOL');
    });
  });

  describe('Response Format', () => {
    test('should include standard response fields', async () => {
      const response = await request(app)
        .get('/api/v1/cobol/health')
        .expect(200);

      expect(response.body).toHaveProperty('timestamp');
      expect(response.headers).toHaveProperty('x-request-id');
    });

    test('should include quota information in responses', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/parse')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: sampleCobolCode })
        .expect(200);

      expect(response.body).toHaveProperty('quota');
      expect(response.body.quota).toHaveProperty('limit');
      expect(response.body.quota).toHaveProperty('used');
      expect(response.body.quota).toHaveProperty('remaining');
    });

    test('should include processing time in successful responses', async () => {
      const response = await request(app)
        .post('/api/v1/cobol/parse')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: sampleCobolCode })
        .expect(200);

      expect(response.body).toHaveProperty('processingTime');
      expect(typeof response.body.processingTime).toBe('number');
    });
  });

  describe('Error Handling', () => {
    test('should handle parsing errors gracefully', async () => {
      const brokenCobol = `
        IDENTIFICATION DIVISION.
        PROGRAM-ID. BROKEN.
        
        DATA DIVISION.
        INVALID SYNTAX
      `;

      const response = await request(app)
        .post('/api/v1/cobol/transpile')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: brokenCobol })
        .expect(500);

      expect(response.body).toHaveProperty('error');
      expect(response.body).toHaveProperty('message');
      expect(response.body).toHaveProperty('requestId');
      expect(response.body).toHaveProperty('timestamp');
    });

    test('should provide detailed error information in development', async () => {
      // This would need environment variable manipulation in a real test
      // For now, just verify error structure
      const response = await request(app)
        .post('/api/v1/cobol/validate')
        .set('Authorization', `Bearer ${validToken}`)
        .send({ cobolCode: 'INVALID' })
        .expect(422);

      expect(response.body.validation.valid).toBe(false);
      expect(response.body.validation).toHaveProperty('error');
    });
  });

  describe('Integration with Existing Platform', () => {
    test('should integrate with platform status endpoint', async () => {
      const response = await request(app)
        .get('/api/v1/status')
        .expect(200);

      expect(response.body.components.cobolTranspiler).toBe('operational');
      expect(response.body.supportedBankingSystems).toContain('FIS Systematics');
      expect(response.body.useCases).toContain('COBOL Modernization');
      expect(response.body.endpoints.cobol).toBe('/api/v1/cobol');
    });

    test('should integrate with platform health endpoint', async () => {
      const response = await request(app)
        .get('/health')
        .expect(200);

      expect(response.body).toHaveProperty('status', 'healthy');
      expect(response.body).toHaveProperty('version');
    });
  });
});