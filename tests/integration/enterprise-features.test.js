const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const request = require('supertest');
const express = require('express');

// Import enterprise feature modules
const { cobolRBAC } = require('../../src/auth/cobol-rbac');
const { transpilerAudit, AUDIT_EVENT_TYPES } = require('../../src/audit/transpiler-audit');
const { quotaManager, SUBSCRIPTION_TIERS } = require('../../src/billing/quota-manager');

// Mock dependencies
jest.mock('../../src/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
    debug: jest.fn()
  }
}));

jest.mock('../../src/database/connection', () => ({
  DatabaseManager: jest.fn().mockImplementation(() => ({
    connect: jest.fn().mockResolvedValue(true),
    disconnect: jest.fn().mockResolvedValue(true),
    query: jest.fn().mockResolvedValue([])
  }))
}));

jest.mock('../../src/utils/cache', () => ({
  CacheManager: jest.fn().mockImplementation(() => ({
    get: jest.fn(),
    set: jest.fn(),
    delete: jest.fn(),
    clear: jest.fn()
  }))
}));

describe('Enterprise Features Integration Tests', () => {
  let app;
  let mockDb;

  beforeAll(async () => {
    // Create Express app for testing
    app = express();
    app.use(express.json());

    // Initialize enterprise features
    await cobolRBAC.initialize();
    await transpilerAudit.initialize();
    await quotaManager.initialize();

    // Mock database for all modules
    mockDb = cobolRBAC.db;
    mockDb.query.mockResolvedValue([]);

    // Setup test routes
    setupTestRoutes(app);
  });

  afterAll(async () => {
    await cobolRBAC.shutdown();
    await transpilerAudit.shutdown();
    await quotaManager.shutdown();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Reset state
    cobolRBAC.permissions.clear();
    cobolRBAC.roleHierarchy.clear();
    quotaManager.quotaCache.clear();
    quotaManager.usageCounters.clear();
  });

  function setupTestRoutes(app) {
    // COBOL transpiler route with enterprise features
    app.post('/api/transpiler/cobol', 
      cobolRBAC.requirePermission('transpiler.execute'),
      async (req, res) => {
        try {
          const { userId, customerId } = req.user;
          const { projectId, fileName, cobolCode } = req.body;

          // Check quota
          const quotaCheck = await quotaManager.canPerformTranspilation(customerId, req.id);
          if (!quotaCheck.allowed) {
            return res.status(429).json({
              error: 'Quota exceeded',
              reason: quotaCheck.reason,
              details: quotaCheck.details
            });
          }

          // Simulate transpilation process
          const startTime = Date.now();
          
          // Log transpilation start
          await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_START, {
            projectId,
            fileName,
            codeLength: cobolCode.length
          }, {
            userId,
            customerId,
            requestId: req.id,
            action: 'transpile_cobol'
          });

          // Simulate processing
          await new Promise(resolve => setTimeout(resolve, 100));

          // Record usage
          await quotaManager.recordUsage(customerId, req.id, {
            projectId,
            fileName,
            processingTime: Date.now() - startTime
          });

          // Log successful completion
          await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_COMPLETE, {
            projectId,
            fileName,
            success: true,
            processingTime: Date.now() - startTime,
            outputSize: cobolCode.length * 1.2
          }, {
            userId,
            customerId,
            requestId: req.id,
            action: 'transpile_cobol'
          });

          res.json({
            success: true,
            projectId,
            fileName,
            generatedCode: `// Generated smart contract from ${fileName}`,
            processingTime: Date.now() - startTime,
            quotaRemaining: quotaCheck.remaining
          });

        } catch (error) {
          await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.TRANSPILE_FAILED, {
            projectId: req.body.projectId,
            error: error.message
          }, {
            userId: req.user?.userId,
            customerId: req.user?.customerId,
            requestId: req.id,
            result: 'failed',
            error: error.message
          });

          res.status(500).json({
            error: 'Transpilation failed',
            message: error.message
          });
        }
      }
    );

    // Banking system access route
    app.get('/api/banking-systems/:system',
      cobolRBAC.requireBankingSystemAccess(),
      async (req, res) => {
        const { system } = req.params;
        const { userId, customerId, bankingSystem } = req.user;

        await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.SENSITIVE_DATA_ACCESS, {
          bankingSystem: system,
          accessType: 'system_configuration'
        }, {
          userId,
          customerId,
          requestId: req.id,
          resourceType: 'banking_system',
          resourceId: system
        });

        res.json({
          system,
          accessGranted: true,
          userSystem: bankingSystem,
          configuration: {
            templates: ['payment_processing', 'account_management'],
            features: ['real_time_processing', 'compliance_monitoring']
          }
        });
      }
    );

    // Admin-only route for user management
    app.post('/api/admin/users/:userId/role',
      cobolRBAC.requirePermission('admin.user_management'),
      async (req, res) => {
        const { userId: targetUserId } = req.params;
        const { role, bankingSystem } = req.body;
        const { userId: adminUserId, customerId } = req.user;

        await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.USER_ROLE_CHANGE, {
          targetUserId,
          newRole: role,
          newBankingSystem: bankingSystem,
          adminUserId
        }, {
          userId: adminUserId,
          customerId,
          requestId: req.id,
          action: 'change_user_role'
        });

        res.json({
          success: true,
          targetUserId,
          newRole: role,
          newBankingSystem: bankingSystem
        });
      }
    );

    // Quota management route
    app.get('/api/quota/status',
      cobolRBAC.requirePermission('quota.view'),
      async (req, res) => {
        const { customerId } = req.user;
        
        const quota = await quotaManager.getCustomerQuota(customerId);
        if (!quota) {
          return res.status(404).json({ error: 'Quota not found' });
        }

        const status = quotaManager.evaluateQuotaStatus(quota);

        res.json({
          quota: {
            tier: quota.tier,
            monthlyQuota: quota.monthlyQuota,
            currentUsage: quota.currentUsage,
            overageUsage: quota.overageUsage,
            totalUsage: quota.currentUsage + quota.overageUsage
          },
          status,
          alerts: quota.alertThresholds
        });
      }
    );

    // Analytics route (enterprise tier only)
    app.get('/api/analytics/usage',
      cobolRBAC.requirePermission('analytics.view'),
      async (req, res) => {
        const { customerId } = req.user;
        const { startDate, endDate } = req.query;

        const analytics = await quotaManager.getUsageAnalytics(
          customerId,
          startDate || '2025-07-01',
          endDate || '2025-07-31'
        );

        await transpilerAudit.logEvent(AUDIT_EVENT_TYPES.DATA_EXPORT, {
          dataType: 'usage_analytics',
          period: { startDate, endDate },
          recordCount: analytics.dailyData.length
        }, {
          userId: req.user.userId,
          customerId,
          requestId: req.id,
          action: 'export_analytics'
        });

        res.json(analytics);
      }
    );

    // Authentication middleware (mock)
    app.use((req, res, next) => {
      req.id = `req-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
      next();
    });
  }

  describe('Role-Based Access Control Integration', () => {
    test('should enforce permissions for transpiler access', async () => {
      // Setup user with transpiler permissions
      const userId = 'user-123';
      const customerId = 'customer-abc';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));

      // Setup quota
      const quota = {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 100,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      // Mock request with user context
      const response = await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-123',
          fileName: 'payment.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. PAYMENT.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.quotaRemaining).toBe(150);
    });

    test('should deny access without proper permissions', async () => {
      // Setup user without transpiler permissions
      const userId = 'user-456';
      const customerId = 'customer-def';

      cobolRBAC.permissions.set(userId, new Set(['basic.read']));

      const response = await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-123',
          fileName: 'payment.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. PAYMENT.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(403);
      expect(response.body.error).toBe('Insufficient permissions');
    });

    test('should enforce banking system access controls', async () => {
      // Setup user with FIS access
      const userId = 'user-789';
      const customerId = 'customer-ghi';

      cobolRBAC.permissions.set(userId, new Set(['banking.fis.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_user',
        bankingSystem: 'fis',
        permissions: ['banking.fis.access']
      });

      const response = await request(app)
        .get('/api/banking-systems/fis')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(200);
      expect(response.body.accessGranted).toBe(true);
      expect(response.body.userSystem).toBe('fis');
    });

    test('should block access to unauthorized banking system', async () => {
      // Setup user with FIS access trying to access Fiserv
      const userId = 'user-789';
      const customerId = 'customer-ghi';

      cobolRBAC.permissions.set(userId, new Set(['banking.fis.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_user',
        bankingSystem: 'fis',
        permissions: ['banking.fis.access']
      });

      const response = await request(app)
        .get('/api/banking-systems/fiserv')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(403);
    });

    test('should allow admin operations for super_admin role', async () => {
      // Setup super admin user
      const adminUserId = 'admin-123';
      const customerId = 'customer-admin';

      cobolRBAC.permissions.set(adminUserId, new Set(['admin.user_management']));
      cobolRBAC.userProfiles.set(adminUserId, {
        role: 'super_admin',
        permissions: ['admin.user_management']
      });

      const response = await request(app)
        .post('/api/admin/users/user-456/role')
        .send({
          role: 'bank_admin',
          bankingSystem: 'temenos'
        })
        .set('x-user-id', adminUserId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);
      expect(response.body.newRole).toBe('bank_admin');
    });
  });

  describe('Quota Management Integration', () => {
    test('should block transpilation when quota is exceeded', async () => {
      // Setup user with permissions but exceeded quota
      const userId = 'user-quota-test';
      const customerId = 'customer-quota-test';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));

      // Setup exceeded quota
      const quota = {
        customerId,
        tier: 'starter',
        monthlyQuota: 50,
        currentUsage: 50,
        overageUsage: 25, // Exceeded max overage (50% = 25)
        maxOveragePercent: 50,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      const response = await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-quota-test',
          fileName: 'test.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. TEST.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(429);
      expect(response.body.error).toBe('Quota exceeded');
      expect(response.body.reason).toBe('max_overage_exceeded');
    });

    test('should track usage correctly after transpilation', async () => {
      // Setup user and quota
      const userId = 'user-usage-test';
      const customerId = 'customer-usage-test';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));

      const quota = {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 100,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      // Mock database update for usage tracking
      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      const response = await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-usage-test',
          fileName: 'usage.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. USAGE.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(200);
      expect(response.body.success).toBe(true);

      // Verify usage was recorded
      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('UPDATE customer_quotas'),
        [101, 0, customerId] // Usage should increment to 101
      );
    });

    test('should provide quota status for authorized users', async () => {
      // Setup user with quota view permissions
      const userId = 'user-quota-view';
      const customerId = 'customer-quota-view';

      cobolRBAC.permissions.set(userId, new Set(['quota.view']));

      const quota = {
        customerId,
        tier: 'enterprise',
        monthlyQuota: 1000,
        currentUsage: 750,
        overageUsage: 50,
        maxOveragePercent: 200,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000),
        alertThresholds: [85, 95, 100, 110, 125]
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      const response = await request(app)
        .get('/api/quota/status')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(200);
      expect(response.body.quota).toEqual({
        tier: 'enterprise',
        monthlyQuota: 1000,
        currentUsage: 750,
        overageUsage: 50,
        totalUsage: 800
      });
      expect(response.body.status.allowed).toBe(true);
      expect(response.body.status.reason).toBe('overage_allowed');
    });
  });

  describe('Audit Logging Integration', () => {
    test('should log all transpilation events', async () => {
      // Setup user and quota
      const userId = 'user-audit-test';
      const customerId = 'customer-audit-test';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));

      const quota = {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 50,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      // Mock audit logging
      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');

      await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-audit-test',
          fileName: 'audit.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. AUDIT.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      // Verify audit events were logged
      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.TRANSPILE_START,
        expect.objectContaining({
          projectId: 'proj-audit-test',
          fileName: 'audit.cbl'
        }),
        expect.objectContaining({
          userId,
          customerId,
          action: 'transpile_cobol'
        })
      );

      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.TRANSPILE_COMPLETE,
        expect.objectContaining({
          projectId: 'proj-audit-test',
          success: true
        }),
        expect.objectContaining({
          userId,
          customerId,
          action: 'transpile_cobol'
        })
      );

      auditSpy.mockRestore();
    });

    test('should log admin operations with proper audit trail', async () => {
      // Setup admin user
      const adminUserId = 'admin-audit-test';
      const customerId = 'customer-admin-audit';

      cobolRBAC.permissions.set(adminUserId, new Set(['admin.user_management']));
      cobolRBAC.userProfiles.set(adminUserId, {
        role: 'super_admin',
        permissions: ['admin.user_management']
      });

      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');

      await request(app)
        .post('/api/admin/users/user-target/role')
        .send({
          role: 'bank_admin',
          bankingSystem: 'fis'
        })
        .set('x-user-id', adminUserId)
        .set('x-customer-id', customerId);

      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.USER_ROLE_CHANGE,
        expect.objectContaining({
          targetUserId: 'user-target',
          newRole: 'bank_admin',
          newBankingSystem: 'fis',
          adminUserId
        }),
        expect.objectContaining({
          userId: adminUserId,
          customerId,
          action: 'change_user_role'
        })
      );

      auditSpy.mockRestore();
    });

    test('should log sensitive data access events', async () => {
      // Setup user with banking system access
      const userId = 'user-sensitive-test';
      const customerId = 'customer-sensitive-test';

      cobolRBAC.permissions.set(userId, new Set(['banking.temenos.access']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_user',
        bankingSystem: 'temenos',
        permissions: ['banking.temenos.access']
      });

      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');

      await request(app)
        .get('/api/banking-systems/temenos')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.SENSITIVE_DATA_ACCESS,
        expect.objectContaining({
          bankingSystem: 'temenos',
          accessType: 'system_configuration'
        }),
        expect.objectContaining({
          userId,
          customerId,
          resourceType: 'banking_system',
          resourceId: 'temenos'
        })
      );

      auditSpy.mockRestore();
    });
  });

  describe('Analytics and Reporting Integration', () => {
    test('should provide usage analytics for enterprise customers', async () => {
      // Setup enterprise user with analytics permissions
      const userId = 'user-analytics-test';
      const customerId = 'customer-analytics-test';

      cobolRBAC.permissions.set(userId, new Set(['analytics.view']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_admin',
        tier: 'enterprise',
        permissions: ['analytics.view']
      });

      // Mock analytics data
      const mockAnalyticsData = [
        {
          usage_date: '2025-07-01',
          daily_usage: 45,
          cumulative_usage: 200,
          quota_remaining: 800,
          overage_amount: 0,
          cost_incurred: 2999.00
        },
        {
          usage_date: '2025-07-02',
          daily_usage: 55,
          cumulative_usage: 255,
          quota_remaining: 745,
          overage_amount: 0,
          cost_incurred: 2999.00
        }
      ];

      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue(mockAnalyticsData);

      const response = await request(app)
        .get('/api/analytics/usage?startDate=2025-07-01&endDate=2025-07-31')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(200);
      expect(response.body.summary).toEqual({
        totalUsage: 100,
        averageDailyUsage: 50,
        totalOverage: 0,
        totalCost: 5998.00,
        peakDailyUsage: 55,
        daysWithOverage: 0
      });
      expect(response.body.dailyData).toHaveLength(2);
    });

    test('should log analytics data export events', async () => {
      // Setup user with analytics permissions
      const userId = 'user-export-test';
      const customerId = 'customer-export-test';

      cobolRBAC.permissions.set(userId, new Set(['analytics.view']));

      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue([]);

      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');

      await request(app)
        .get('/api/analytics/usage?startDate=2025-07-01&endDate=2025-07-31')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.DATA_EXPORT,
        expect.objectContaining({
          dataType: 'usage_analytics',
          period: {
            startDate: '2025-07-01',
            endDate: '2025-07-31'
          }
        }),
        expect.objectContaining({
          userId,
          customerId,
          action: 'export_analytics'
        })
      );

      auditSpy.mockRestore();
    });
  });

  describe('Cross-Module Integration Scenarios', () => {
    test('should handle complete user journey with all enterprise features', async () => {
      // Setup enterprise customer with full permissions
      const userId = 'user-journey-test';
      const customerId = 'customer-journey-test';

      // Setup RBAC
      cobolRBAC.permissions.set(userId, new Set([
        'transpiler.execute',
        'banking.fis.access',
        'quota.view',
        'analytics.view'
      ]));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_admin',
        bankingSystem: 'fis',
        tier: 'enterprise',
        permissions: ['transpiler.execute', 'banking.fis.access', 'quota.view', 'analytics.view']
      });

      // Setup quota
      const quota = {
        customerId,
        tier: 'enterprise',
        monthlyQuota: 1000,
        currentUsage: 500,
        overageUsage: 0,
        maxOveragePercent: 200,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000),
        alertThresholds: [85, 95, 100, 110, 125]
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      // Mock database responses
      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      // 1. Check quota status
      const quotaResponse = await request(app)
        .get('/api/quota/status')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(quotaResponse.status).toBe(200);
      expect(quotaResponse.body.quota.currentUsage).toBe(500);

      // 2. Access banking system
      const bankingResponse = await request(app)
        .get('/api/banking-systems/fis')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(bankingResponse.status).toBe(200);
      expect(bankingResponse.body.accessGranted).toBe(true);

      // 3. Perform transpilation
      const transpileResponse = await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-journey-test',
          fileName: 'journey.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. JOURNEY.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(transpileResponse.status).toBe(200);
      expect(transpileResponse.body.success).toBe(true);

      // 4. Verify usage was tracked
      expect(mockDb.query).toHaveBeenCalledWith(
        expect.stringContaining('UPDATE customer_quotas'),
        [501, 0, customerId] // Usage should increment
      );

      // 5. Get analytics (mock data for this test)
      mockDb.query.mockResolvedValue([]);
      
      const analyticsResponse = await request(app)
        .get('/api/analytics/usage')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(analyticsResponse.status).toBe(200);
    });

    test('should handle permission escalation attempts', async () => {
      // Setup regular user trying to access admin functions
      const userId = 'user-escalation-test';
      const customerId = 'customer-escalation-test';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));
      cobolRBAC.userProfiles.set(userId, {
        role: 'bank_user',
        bankingSystem: 'fis',
        permissions: ['transpiler.execute']
      });

      // Attempt to access admin function
      const response = await request(app)
        .post('/api/admin/users/target-user/role')
        .send({
          role: 'super_admin',
          bankingSystem: 'all'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(403);
      expect(response.body.error).toBe('Insufficient permissions');
    });

    test('should handle quota exhaustion with proper error handling', async () => {
      // Setup user with permissions but no quota
      const userId = 'user-no-quota';
      const customerId = 'customer-no-quota';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));

      // No quota setup - will trigger quota check failure

      const response = await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-no-quota',
          fileName: 'test.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. TEST.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      expect(response.status).toBe(429);
      expect(response.body.error).toBe('Quota exceeded');
      expect(response.body.reason).toBe('quota_check_failed');
    });

    test('should maintain audit trail across all operations', async () => {
      // Setup user and test complete workflow
      const userId = 'user-audit-workflow';
      const customerId = 'customer-audit-workflow';

      cobolRBAC.permissions.set(userId, new Set([
        'transpiler.execute',
        'quota.view',
        'analytics.view'
      ]));

      const quota = {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 100,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      const auditSpy = jest.spyOn(transpilerAudit, 'logEvent');

      // Perform multiple operations
      await request(app)
        .get('/api/quota/status')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      await request(app)
        .post('/api/transpiler/cobol')
        .send({
          projectId: 'proj-workflow',
          fileName: 'workflow.cbl',
          cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. WORKFLOW.'
        })
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      // Mock analytics data
      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue([]);

      await request(app)
        .get('/api/analytics/usage')
        .set('x-user-id', userId)
        .set('x-customer-id', customerId);

      // Verify comprehensive audit trail
      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.TRANSPILE_START,
        expect.any(Object),
        expect.objectContaining({ userId, customerId })
      );

      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.TRANSPILE_COMPLETE,
        expect.any(Object),
        expect.objectContaining({ userId, customerId })
      );

      expect(auditSpy).toHaveBeenCalledWith(
        AUDIT_EVENT_TYPES.DATA_EXPORT,
        expect.any(Object),
        expect.objectContaining({ userId, customerId })
      );

      auditSpy.mockRestore();
    });
  });

  describe('Performance and Scalability Tests', () => {
    test('should handle concurrent transpilation requests', async () => {
      // Setup multiple users
      const users = Array.from({ length: 5 }, (_, i) => ({
        userId: `user-concurrent-${i}`,
        customerId: `customer-concurrent-${i}`
      }));

      // Setup permissions and quotas for all users
      users.forEach(({ userId, customerId }) => {
        cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));
        
        const quota = {
          customerId,
          tier: 'enterprise',
          monthlyQuota: 1000,
          currentUsage: 100,
          overageUsage: 0,
          maxOveragePercent: 200,
          status: 'active',
          periodEnd: new Date(Date.now() + 86400000)
        };
        quotaManager.quotaCache.set(`quota:${customerId}`, quota);
      });

      // Mock database
      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      // Perform concurrent requests
      const requests = users.map(({ userId, customerId }) =>
        request(app)
          .post('/api/transpiler/cobol')
          .send({
            projectId: `proj-concurrent-${userId}`,
            fileName: 'concurrent.cbl',
            cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. CONCURRENT.'
          })
          .set('x-user-id', userId)
          .set('x-customer-id', customerId)
      );

      const responses = await Promise.all(requests);

      // Verify all requests succeeded
      responses.forEach(response => {
        expect(response.status).toBe(200);
        expect(response.body.success).toBe(true);
      });
    });

    test('should handle quota checking performance with caching', async () => {
      const userId = 'user-perf-test';
      const customerId = 'customer-perf-test';

      cobolRBAC.permissions.set(userId, new Set(['transpiler.execute']));

      const quota = {
        customerId,
        tier: 'professional',
        monthlyQuota: 250,
        currentUsage: 100,
        overageUsage: 0,
        maxOveragePercent: 100,
        status: 'active',
        periodEnd: new Date(Date.now() + 86400000)
      };
      quotaManager.quotaCache.set(`quota:${customerId}`, quota);

      // Set rate limiting timestamp
      quotaManager.lastQuotaCheck.set(customerId, Date.now());

      // Mock database
      const mockDb = quotaManager.db;
      mockDb.query.mockResolvedValue({ rowCount: 1 });

      // Perform rapid consecutive requests
      const requests = Array.from({ length: 3 }, () =>
        request(app)
          .post('/api/transpiler/cobol')
          .send({
            projectId: 'proj-perf-test',
            fileName: 'perf.cbl',
            cobolCode: 'IDENTIFICATION DIVISION. PROGRAM-ID. PERF.'
          })
          .set('x-user-id', userId)
          .set('x-customer-id', customerId)
      );

      const responses = await Promise.all(requests);

      // Verify all succeeded and rate limiting worked
      responses.forEach(response => {
        expect(response.status).toBe(200);
      });

      // Should use cached quota checks, not hit database repeatedly
      expect(mockDb.query).toHaveBeenCalledTimes(3); // Only for usage updates
    });
  });
});