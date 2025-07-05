const { describe, test, expect, beforeAll, afterAll, beforeEach, jest } = require('@jest/globals');
const { 
  CobolRBACManager,
  COBOL_USER_ROLES,
  COBOL_PERMISSIONS,
  BANKING_SYSTEM_ACCESS 
} = require('../../src/auth/cobol-rbac');

// Mock dependencies
jest.mock('../../src/utils/logger', () => ({
  logger: {
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn()
  }
}));

jest.mock('../../src/database/connection', () => ({
  DatabaseManager: jest.fn().mockImplementation(() => ({
    connect: jest.fn().mockResolvedValue(true),
    disconnect: jest.fn().mockResolvedValue(true),
    query: jest.fn()
  }))
}));

jest.mock('../../src/utils/cache', () => ({
  CacheManager: jest.fn().mockImplementation(() => ({
    get: jest.fn(),
    set: jest.fn(),
    delete: jest.fn()
  }))
}));

describe('COBOL RBAC Manager', () => {
  let rbacManager;
  let mockDb;

  beforeAll(async () => {
    rbacManager = new CobolRBACManager();
    mockDb = rbacManager.db;
  });

  afterAll(async () => {
    await rbacManager.shutdown();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    rbacManager.permissionCache.clear();
  });

  describe('Initialization', () => {
    test('should initialize RBAC manager successfully', async () => {
      mockDb.connect.mockResolvedValue(true);
      mockDb.query.mockResolvedValue([]);

      await rbacManager.initialize();

      expect(mockDb.connect).toHaveBeenCalled();
    });

    test('should handle initialization errors gracefully', async () => {
      const error = new Error('Database connection failed');
      mockDb.connect.mockRejectedValue(error);

      await expect(rbacManager.initialize()).rejects.toThrow('Database connection failed');
    });
  });

  describe('Permission Calculation', () => {
    test('should calculate correct permissions for bank admin', async () => {
      const userRecord = {
        user_id: '1',
        role: COBOL_USER_ROLES.BANK_ADMIN,
        cobol_features: { enabled: true },
        banking_systems: ['FIS_SYSTEMATICS', 'FISERV_DNA'],
        subscription_tier: 'enterprise'
      };

      const permissions = await rbacManager.calculateUserPermissions(userRecord);

      expect(permissions).toContain(COBOL_PERMISSIONS.COBOL_TRANSPILE);
      expect(permissions).toContain(COBOL_PERMISSIONS.TEMPLATE_CREATE);
      expect(permissions).toContain(COBOL_PERMISSIONS.PROJECT_DEPLOY);
      expect(permissions).toContain(COBOL_PERMISSIONS.USER_MANAGE);
      expect(permissions).toContain(BANKING_SYSTEM_ACCESS.FIS_SYSTEMATICS);
      expect(permissions).toContain(BANKING_SYSTEM_ACCESS.FISERV_DNA);
    });

    test('should calculate limited permissions for SI developer', async () => {
      const userRecord = {
        user_id: '2',
        role: COBOL_USER_ROLES.SI_DEVELOPER,
        cobol_features: { enabled: true },
        banking_systems: ['TCS_BANCS'],
        subscription_tier: 'professional'
      };

      const permissions = await rbacManager.calculateUserPermissions(userRecord);

      expect(permissions).toContain(COBOL_PERMISSIONS.COBOL_TRANSPILE);
      expect(permissions).toContain(COBOL_PERMISSIONS.TEMPLATE_CREATE);
      expect(permissions).not.toContain(COBOL_PERMISSIONS.USER_MANAGE);
      expect(permissions).toContain(BANKING_SYSTEM_ACCESS.TCS_BANCS);
      expect(permissions).not.toContain(BANKING_SYSTEM_ACCESS.FIS_SYSTEMATICS);
    });

    test('should return no permissions when COBOL feature disabled', async () => {
      const userRecord = {
        user_id: '3',
        role: COBOL_USER_ROLES.BANK_ADMIN,
        cobol_features: { enabled: false },
        banking_systems: ['FIS_SYSTEMATICS'],
        subscription_tier: 'enterprise'
      };

      const permissions = await rbacManager.calculateUserPermissions(userRecord);

      expect(permissions).toEqual([]);
    });

    test('should apply starter tier restrictions', async () => {
      const userRecord = {
        user_id: '4',
        role: COBOL_USER_ROLES.BANK_ADMIN,
        cobol_features: { enabled: true },
        banking_systems: ['FISERV_DNA'],
        subscription_tier: 'starter'
      };

      const permissions = await rbacManager.calculateUserPermissions(userRecord);

      expect(permissions).not.toContain(COBOL_PERMISSIONS.TEMPLATE_PUBLISH);
      expect(permissions).not.toContain(COBOL_PERMISSIONS.ANALYTICS_EXPORT);
      expect(permissions).toContain(COBOL_PERMISSIONS.COBOL_TRANSPILE);
    });

    test('should handle read-only bank user permissions', async () => {
      const userRecord = {
        user_id: '5',
        role: COBOL_USER_ROLES.BANK_USER,
        cobol_features: { enabled: true },
        banking_systems: ['TEMENOS_TRANSACT'],
        subscription_tier: 'professional'
      };

      const permissions = await rbacManager.calculateUserPermissions(userRecord);

      expect(permissions).toContain(COBOL_PERMISSIONS.COBOL_VIEW);
      expect(permissions).toContain(COBOL_PERMISSIONS.COBOL_DOWNLOAD);
      expect(permissions).not.toContain(COBOL_PERMISSIONS.COBOL_TRANSPILE);
      expect(permissions).not.toContain(COBOL_PERMISSIONS.TEMPLATE_DELETE);
      expect(permissions).toContain(BANKING_SYSTEM_ACCESS.TEMENOS_TRANSACT);
    });

    test('should provide demo access for reseller partner', async () => {
      const userRecord = {
        user_id: '6',
        role: COBOL_USER_ROLES.RESELLER_PARTNER,
        cobol_features: { enabled: true },
        banking_systems: [],
        subscription_tier: 'demo'
      };

      const permissions = await rbacManager.calculateUserPermissions(userRecord);

      expect(permissions).toContain(COBOL_PERMISSIONS.COBOL_VIEW);
      expect(permissions).toContain(COBOL_PERMISSIONS.ANALYTICS_VIEW);
      expect(permissions).toContain(COBOL_PERMISSIONS.BILLING_VIEW);
      expect(permissions).not.toContain(COBOL_PERMISSIONS.COBOL_TRANSPILE);
      expect(permissions).not.toContain(COBOL_PERMISSIONS.PROJECT_CREATE);
    });
  });

  describe('Permission Checking', () => {
    beforeEach(() => {
      // Setup mock user permissions in cache
      rbacManager.permissionCache.set('permissions:1', {
        permissions: [
          COBOL_PERMISSIONS.COBOL_TRANSPILE,
          COBOL_PERMISSIONS.TEMPLATE_CREATE,
          BANKING_SYSTEM_ACCESS.FIS_SYSTEMATICS
        ],
        bankingSystems: ['FIS_SYSTEMATICS'],
        subscriptionTier: 'professional',
        cachedAt: Date.now()
      });
    });

    test('should correctly check basic permissions', async () => {
      const hasPermission = await rbacManager.hasPermission('1', COBOL_PERMISSIONS.COBOL_TRANSPILE);
      expect(hasPermission).toBe(true);

      const lacksPermission = await rbacManager.hasPermission('1', COBOL_PERMISSIONS.USER_MANAGE);
      expect(lacksPermission).toBe(false);
    });

    test('should check banking system context permissions', async () => {
      const context = { bankingSystem: 'FIS_SYSTEMATICS' };
      const hasAccess = await rbacManager.hasPermission('1', COBOL_PERMISSIONS.COBOL_TRANSPILE, context);
      expect(hasAccess).toBe(true);

      const noAccess = await rbacManager.hasPermission('1', COBOL_PERMISSIONS.COBOL_TRANSPILE, 
        { bankingSystem: 'TCS_BANCS' });
      expect(noAccess).toBe(false);
    });

    test('should return false for non-existent user', async () => {
      const hasPermission = await rbacManager.hasPermission('999', COBOL_PERMISSIONS.COBOL_VIEW);
      expect(hasPermission).toBe(false);
    });
  });

  describe('Banking System Access', () => {
    beforeEach(() => {
      rbacManager.permissionCache.set('permissions:2', {
        permissions: [
          COBOL_PERMISSIONS.COBOL_TRANSPILE,
          BANKING_SYSTEM_ACCESS.FIS_SYSTEMATICS,
          BANKING_SYSTEM_ACCESS.TCS_BANCS
        ],
        bankingSystems: ['FIS_SYSTEMATICS', 'TCS_BANCS'],
        subscriptionTier: 'enterprise',
        cachedAt: Date.now()
      });
    });

    test('should correctly identify accessible banking systems', async () => {
      const canAccessFIS = await rbacManager.canAccessBankingSystem('2', 'FIS_SYSTEMATICS');
      expect(canAccessFIS).toBe(true);

      const canAccessTCS = await rbacManager.canAccessBankingSystem('2', 'TCS_BANCS');
      expect(canAccessTCS).toBe(true);

      const cannotAccessFiserv = await rbacManager.canAccessBankingSystem('2', 'FISERV_DNA');
      expect(cannotAccessFiserv).toBe(false);
    });

    test('should return list of allowed banking systems', async () => {
      const allowedSystems = await rbacManager.getAllowedBankingSystems('2');
      
      expect(allowedSystems).toContain('FIS_SYSTEMATICS');
      expect(allowedSystems).toContain('TCS_BANCS');
      expect(allowedSystems).not.toContain('FISERV_DNA');
      expect(allowedSystems).not.toContain('TEMENOS_TRANSACT');
    });
  });

  describe('Middleware Functions', () => {
    let req, res, next;

    beforeEach(() => {
      req = {
        user: { userId: '1', customerId: 'customer1' },
        body: {},
        params: {}
      };
      res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };
      next = jest.fn();

      rbacManager.permissionCache.set('permissions:1', {
        permissions: [COBOL_PERMISSIONS.COBOL_TRANSPILE, BANKING_SYSTEM_ACCESS.FIS_SYSTEMATICS],
        bankingSystems: ['FIS_SYSTEMATICS'],
        subscriptionTier: 'professional',
        cachedAt: Date.now()
      });
    });

    test('should allow access with correct permission', async () => {
      const middleware = rbacManager.requirePermission(COBOL_PERMISSIONS.COBOL_TRANSPILE);
      
      await middleware(req, res, next);
      
      expect(next).toHaveBeenCalled();
      expect(res.status).not.toHaveBeenCalled();
    });

    test('should deny access without required permission', async () => {
      const middleware = rbacManager.requirePermission(COBOL_PERMISSIONS.USER_MANAGE);
      
      await middleware(req, res, next);
      
      expect(next).not.toHaveBeenCalled();
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Insufficient permissions',
        code: 'PERMISSION_DENIED',
        required: COBOL_PERMISSIONS.USER_MANAGE
      });
    });

    test('should deny access without authentication', async () => {
      req.user = null;
      const middleware = rbacManager.requirePermission(COBOL_PERMISSIONS.COBOL_VIEW);
      
      await middleware(req, res, next);
      
      expect(next).not.toHaveBeenCalled();
      expect(res.status).toHaveBeenCalledWith(401);
      expect(res.json).toHaveBeenCalledWith({
        error: 'Authentication required',
        code: 'AUTH_REQUIRED'
      });
    });

    test('should validate banking system context in middleware', async () => {
      req.body.bankingSystem = 'FIS_SYSTEMATICS';
      const middleware = rbacManager.requirePermission(COBOL_PERMISSIONS.COBOL_TRANSPILE);
      
      await middleware(req, res, next);
      
      expect(next).toHaveBeenCalled();
      expect(req.userPermissions).toBeDefined();
      expect(req.allowedBankingSystems).toBeDefined();
    });
  });

  describe('Admin Functions', () => {
    beforeEach(() => {
      rbacManager.permissionCache.set('permissions:admin', {
        permissions: [COBOL_PERMISSIONS.USER_MANAGE, COBOL_PERMISSIONS.AUDIT_VIEW],
        bankingSystems: ['FIS_SYSTEMATICS', 'FISERV_DNA'],
        subscriptionTier: 'enterprise',
        cachedAt: Date.now()
      });
    });

    test('should allow admin to grant permissions', async () => {
      mockDb.query.mockResolvedValue([{
        user_id: 'target',
        role: COBOL_USER_ROLES.BANK_USER,
        cobol_features: { enabled: true },
        banking_systems: ['FIS_SYSTEMATICS'],
        subscription_tier: 'professional'
      }]);

      const result = await rbacManager.grantPermission('admin', 'target', COBOL_PERMISSIONS.COBOL_TRANSPILE);
      
      expect(result).toBe(true);
    });

    test('should prevent non-admin from granting permissions', async () => {
      rbacManager.permissionCache.set('permissions:user', {
        permissions: [COBOL_PERMISSIONS.COBOL_VIEW],
        bankingSystems: ['FIS_SYSTEMATICS'],
        subscriptionTier: 'professional',
        cachedAt: Date.now()
      });

      await expect(
        rbacManager.grantPermission('user', 'target', COBOL_PERMISSIONS.COBOL_TRANSPILE)
      ).rejects.toThrow('Insufficient permissions to manage users');
    });

    test('should generate permission summary for admin', async () => {
      const summary = await rbacManager.getPermissionSummary('admin');
      
      expect(summary).toHaveProperty('totalUsers');
      expect(summary).toHaveProperty('roleDistribution');
      expect(summary).toHaveProperty('bankingSystemAccess');
      expect(summary).toHaveProperty('cacheMetrics');
    });
  });

  describe('Cache Management', () => {
    test('should refresh user permissions when cache is stale', async () => {
      // Set stale cache entry (older than 15 minutes)
      const staleTime = Date.now() - (16 * 60 * 1000);
      rbacManager.permissionCache.set('permissions:1', {
        permissions: [COBOL_PERMISSIONS.COBOL_VIEW],
        bankingSystems: ['FIS_SYSTEMATICS'],
        subscriptionTier: 'starter',
        cachedAt: staleTime
      });

      mockDb.query.mockResolvedValue([{
        user_id: '1',
        role: COBOL_USER_ROLES.BANK_ADMIN,
        cobol_features: { enabled: true },
        banking_systems: ['FIS_SYSTEMATICS', 'FISERV_DNA'],
        subscription_tier: 'enterprise'
      }]);

      const hasPermission = await rbacManager.hasPermission('1', COBOL_PERMISSIONS.COBOL_TRANSPILE);
      
      expect(mockDb.query).toHaveBeenCalled();
      expect(hasPermission).toBe(true);
    });

    test('should use cached permissions when fresh', async () => {
      rbacManager.permissionCache.set('permissions:1', {
        permissions: [COBOL_PERMISSIONS.COBOL_TRANSPILE],
        bankingSystems: ['FIS_SYSTEMATICS'],
        subscriptionTier: 'professional',
        cachedAt: Date.now()
      });

      const hasPermission = await rbacManager.hasPermission('1', COBOL_PERMISSIONS.COBOL_TRANSPILE);
      
      expect(mockDb.query).not.toHaveBeenCalled();
      expect(hasPermission).toBe(true);
    });
  });
});