/**
 * COBOL Transpiler Role-Based Access Control (RBAC) Implementation
 * 
 * This module implements granular access control for COBOL transpiler features
 * within the existing LegacyBAAS authentication framework.
 * 
 * Integrates with:
 * - Existing src/auth/ OAuth2 system
 * - Customer feature flags from database
 * - Banking system specific permissions
 * - Partner-specific access levels
 */

const { logger } = require('../utils/logger');
const { DatabaseManager } = require('../database/connection');
const { CacheManager } = require('../utils/cache');

// COBOL-specific user roles
const COBOL_USER_ROLES = {
  BANK_ADMIN: 'bank_admin',
  SI_DEVELOPER: 'si_developer', 
  BANK_USER: 'bank_user',
  RESELLER_PARTNER: 'reseller_partner',
  SUPER_ADMIN: 'super_admin'
};

// COBOL transpiler permissions
const COBOL_PERMISSIONS = {
  // Core transpiler operations
  COBOL_TRANSPILE: 'cobol:transpile',
  COBOL_VIEW: 'cobol:view',
  COBOL_DOWNLOAD: 'cobol:download',
  COBOL_VALIDATE: 'cobol:validate',
  
  // Template management
  TEMPLATE_CREATE: 'template:create',
  TEMPLATE_EDIT: 'template:edit',
  TEMPLATE_DELETE: 'template:delete',
  TEMPLATE_PUBLISH: 'template:publish',
  
  // Project management
  PROJECT_CREATE: 'project:create',
  PROJECT_EDIT: 'project:edit',
  PROJECT_DELETE: 'project:delete',
  PROJECT_DEPLOY: 'project:deploy',
  
  // Analytics and reporting
  ANALYTICS_VIEW: 'analytics:view',
  ANALYTICS_EXPORT: 'analytics:export',
  USAGE_VIEW: 'usage:view',
  BILLING_VIEW: 'billing:view',
  
  // Administrative functions
  USER_MANAGE: 'user:manage',
  QUOTA_MANAGE: 'quota:manage',
  AUDIT_VIEW: 'audit:view',
  SYSTEM_CONFIG: 'system:config'
};

// Banking system specific permissions
const BANKING_SYSTEM_ACCESS = {
  FIS_SYSTEMATICS: 'banking:fis_systematics',
  FISERV_DNA: 'banking:fiserv_dna',
  TCS_BANCS: 'banking:tcs_bancs',
  TEMENOS_TRANSACT: 'banking:temenos_transact'
};

// Role-permission mappings
const ROLE_PERMISSIONS = {
  [COBOL_USER_ROLES.BANK_ADMIN]: [
    COBOL_PERMISSIONS.COBOL_TRANSPILE,
    COBOL_PERMISSIONS.COBOL_VIEW,
    COBOL_PERMISSIONS.COBOL_DOWNLOAD,
    COBOL_PERMISSIONS.COBOL_VALIDATE,
    COBOL_PERMISSIONS.TEMPLATE_CREATE,
    COBOL_PERMISSIONS.TEMPLATE_EDIT,
    COBOL_PERMISSIONS.TEMPLATE_DELETE,
    COBOL_PERMISSIONS.PROJECT_CREATE,
    COBOL_PERMISSIONS.PROJECT_EDIT,
    COBOL_PERMISSIONS.PROJECT_DELETE,
    COBOL_PERMISSIONS.PROJECT_DEPLOY,
    COBOL_PERMISSIONS.ANALYTICS_VIEW,
    COBOL_PERMISSIONS.ANALYTICS_EXPORT,
    COBOL_PERMISSIONS.USAGE_VIEW,
    COBOL_PERMISSIONS.BILLING_VIEW,
    COBOL_PERMISSIONS.USER_MANAGE,
    COBOL_PERMISSIONS.QUOTA_MANAGE,
    COBOL_PERMISSIONS.AUDIT_VIEW,
    // All banking systems for bank admin
    ...Object.values(BANKING_SYSTEM_ACCESS)
  ],
  
  [COBOL_USER_ROLES.SI_DEVELOPER]: [
    COBOL_PERMISSIONS.COBOL_TRANSPILE,
    COBOL_PERMISSIONS.COBOL_VIEW,
    COBOL_PERMISSIONS.COBOL_DOWNLOAD,
    COBOL_PERMISSIONS.COBOL_VALIDATE,
    COBOL_PERMISSIONS.TEMPLATE_CREATE,
    COBOL_PERMISSIONS.TEMPLATE_EDIT,
    COBOL_PERMISSIONS.PROJECT_CREATE,
    COBOL_PERMISSIONS.PROJECT_EDIT,
    COBOL_PERMISSIONS.PROJECT_DEPLOY,
    COBOL_PERMISSIONS.ANALYTICS_VIEW,
    COBOL_PERMISSIONS.USAGE_VIEW,
    // Limited to customer's banking systems only
  ],
  
  [COBOL_USER_ROLES.BANK_USER]: [
    COBOL_PERMISSIONS.COBOL_VIEW,
    COBOL_PERMISSIONS.COBOL_DOWNLOAD,
    COBOL_PERMISSIONS.PROJECT_CREATE,
    COBOL_PERMISSIONS.ANALYTICS_VIEW,
    COBOL_PERMISSIONS.USAGE_VIEW,
    // Read-only access to customer's banking systems
  ],
  
  [COBOL_USER_ROLES.RESELLER_PARTNER]: [
    COBOL_PERMISSIONS.COBOL_VIEW,
    COBOL_PERMISSIONS.ANALYTICS_VIEW,
    COBOL_PERMISSIONS.USAGE_VIEW,
    COBOL_PERMISSIONS.BILLING_VIEW,
    // Demo/limited access only
  ],
  
  [COBOL_USER_ROLES.SUPER_ADMIN]: [
    // All permissions for super admin
    ...Object.values(COBOL_PERMISSIONS),
    ...Object.values(BANKING_SYSTEM_ACCESS),
    COBOL_PERMISSIONS.SYSTEM_CONFIG
  ]
};

class CobolRBACManager {
  constructor() {
    this.db = new DatabaseManager();
    this.cache = new CacheManager();
    this.permissionCache = new Map();
  }

  /**
   * Initialize RBAC system with database connections
   */
  async initialize() {
    try {
      await this.db.connect();
      await this.loadPermissionCache();
      logger.info('COBOL RBAC Manager initialized successfully');
    } catch (error) {
      logger.error('Failed to initialize COBOL RBAC Manager:', error);
      throw error;
    }
  }

  /**
   * Load user permissions into cache for faster access
   */
  async loadPermissionCache() {
    try {
      const query = `
        SELECT 
          u.user_id,
          u.customer_id,
          u.role,
          c.cobol_features,
          c.banking_systems,
          c.subscription_tier
        FROM users u
        JOIN customers c ON u.customer_id = c.customer_id
        WHERE c.cobol_features->>'enabled' = 'true'
      `;
      
      const results = await this.db.query(query);
      
      for (const row of results) {
        const cacheKey = `permissions:${row.user_id}`;
        const permissions = await this.calculateUserPermissions(row);
        this.permissionCache.set(cacheKey, {
          permissions,
          bankingSystems: row.banking_systems,
          subscriptionTier: row.subscription_tier,
          cachedAt: Date.now()
        });
      }
      
      logger.info(`Loaded permissions for ${results.length} COBOL users into cache`);
    } catch (error) {
      logger.error('Failed to load permission cache:', error);
      throw error;
    }
  }

  /**
   * Calculate effective permissions for a user based on role and customer features
   */
  async calculateUserPermissions(userRecord) {
    const basePermissions = ROLE_PERMISSIONS[userRecord.role] || [];
    const cobolFeatures = userRecord.cobol_features || {};
    const bankingSystems = userRecord.banking_systems || [];
    
    let effectivePermissions = [...basePermissions];
    
    // Apply feature flag restrictions
    if (!cobolFeatures.enabled) {
      return []; // No COBOL access if feature not enabled
    }
    
    // Apply banking system restrictions for non-admin roles
    if (userRecord.role !== COBOL_USER_ROLES.BANK_ADMIN && 
        userRecord.role !== COBOL_USER_ROLES.SUPER_ADMIN) {
      
      // Filter banking system permissions based on customer's allowed systems
      const allowedBankingPermissions = bankingSystems.map(system => 
        BANKING_SYSTEM_ACCESS[system]
      ).filter(Boolean);
      
      effectivePermissions = effectivePermissions.filter(permission => {
        if (Object.values(BANKING_SYSTEM_ACCESS).includes(permission)) {
          return allowedBankingPermissions.includes(permission);
        }
        return true;
      });
      
      // Add allowed banking system permissions
      effectivePermissions.push(...allowedBankingPermissions);
    }
    
    // Apply subscription tier restrictions
    if (userRecord.subscription_tier === 'starter') {
      // Remove advanced permissions for starter tier
      effectivePermissions = effectivePermissions.filter(permission => 
        ![
          COBOL_PERMISSIONS.TEMPLATE_PUBLISH,
          COBOL_PERMISSIONS.ANALYTICS_EXPORT,
          COBOL_PERMISSIONS.SYSTEM_CONFIG
        ].includes(permission)
      );
    }
    
    return [...new Set(effectivePermissions)]; // Remove duplicates
  }

  /**
   * Check if user has specific permission
   */
  async hasPermission(userId, permission, context = {}) {
    try {
      const cacheKey = `permissions:${userId}`;
      let userPermissions = this.permissionCache.get(cacheKey);
      
      // Refresh cache if older than 15 minutes
      if (!userPermissions || (Date.now() - userPermissions.cachedAt) > 900000) {
        await this.refreshUserPermissions(userId);
        userPermissions = this.permissionCache.get(cacheKey);
      }
      
      if (!userPermissions) {
        logger.warn(`No permissions found for user ${userId}`);
        return false;
      }
      
      // Check basic permission
      const hasBasicPermission = userPermissions.permissions.includes(permission);
      
      // Apply context-specific checks
      if (hasBasicPermission && context.bankingSystem) {
        const requiredBankingPermission = BANKING_SYSTEM_ACCESS[context.bankingSystem];
        if (requiredBankingPermission) {
          return userPermissions.permissions.includes(requiredBankingPermission);
        }
      }
      
      return hasBasicPermission;
    } catch (error) {
      logger.error(`Error checking permission ${permission} for user ${userId}:`, error);
      return false;
    }
  }

  /**
   * Refresh permissions for a specific user
   */
  async refreshUserPermissions(userId) {
    try {
      const query = `
        SELECT 
          u.user_id,
          u.customer_id,
          u.role,
          c.cobol_features,
          c.banking_systems,
          c.subscription_tier
        FROM users u
        JOIN customers c ON u.customer_id = c.customer_id
        WHERE u.user_id = $1
      `;
      
      const results = await this.db.query(query, [userId]);
      
      if (results.length === 0) {
        this.permissionCache.delete(`permissions:${userId}`);
        return;
      }
      
      const userRecord = results[0];
      const permissions = await this.calculateUserPermissions(userRecord);
      
      this.permissionCache.set(`permissions:${userId}`, {
        permissions,
        bankingSystems: userRecord.banking_systems,
        subscriptionTier: userRecord.subscription_tier,
        cachedAt: Date.now()
      });
      
    } catch (error) {
      logger.error(`Failed to refresh permissions for user ${userId}:`, error);
      throw error;
    }
  }

  /**
   * Get all permissions for a user
   */
  async getUserPermissions(userId) {
    const cacheKey = `permissions:${userId}`;
    let userPermissions = this.permissionCache.get(cacheKey);
    
    if (!userPermissions || (Date.now() - userPermissions.cachedAt) > 900000) {
      await this.refreshUserPermissions(userId);
      userPermissions = this.permissionCache.get(cacheKey);
    }
    
    return userPermissions?.permissions || [];
  }

  /**
   * Check if user can access specific banking system
   */
  async canAccessBankingSystem(userId, bankingSystem) {
    const requiredPermission = BANKING_SYSTEM_ACCESS[bankingSystem];
    if (!requiredPermission) {
      return false;
    }
    
    return await this.hasPermission(userId, requiredPermission);
  }

  /**
   * Get allowed banking systems for user
   */
  async getAllowedBankingSystems(userId) {
    const permissions = await this.getUserPermissions(userId);
    const allowedSystems = [];
    
    for (const [system, permission] of Object.entries(BANKING_SYSTEM_ACCESS)) {
      if (permissions.includes(permission)) {
        allowedSystems.push(system);
      }
    }
    
    return allowedSystems;
  }

  /**
   * Middleware function for Express.js route protection
   */
  requirePermission(permission, options = {}) {
    return async (req, res, next) => {
      try {
        const userId = req.user?.userId;
        if (!userId) {
          return res.status(401).json({
            error: 'Authentication required',
            code: 'AUTH_REQUIRED'
          });
        }
        
        const context = {
          bankingSystem: options.bankingSystem || req.body?.bankingSystem || req.params?.bankingSystem,
          customerId: req.user?.customerId
        };
        
        const hasPermission = await this.hasPermission(userId, permission, context);
        
        if (!hasPermission) {
          logger.warn(`Access denied: User ${userId} lacks permission ${permission}`, { context });
          return res.status(403).json({
            error: 'Insufficient permissions',
            code: 'PERMISSION_DENIED',
            required: permission
          });
        }
        
        // Add permissions to request for downstream use
        req.userPermissions = await this.getUserPermissions(userId);
        req.allowedBankingSystems = await this.getAllowedBankingSystems(userId);
        
        next();
      } catch (error) {
        logger.error('RBAC middleware error:', error);
        res.status(500).json({
          error: 'Authorization check failed',
          code: 'RBAC_ERROR'
        });
      }
    };
  }

  /**
   * Middleware to require multiple permissions (all must be present)
   */
  requireAllPermissions(permissions, options = {}) {
    return async (req, res, next) => {
      try {
        const userId = req.user?.userId;
        if (!userId) {
          return res.status(401).json({
            error: 'Authentication required',
            code: 'AUTH_REQUIRED'
          });
        }
        
        const context = {
          bankingSystem: options.bankingSystem || req.body?.bankingSystem || req.params?.bankingSystem,
          customerId: req.user?.customerId
        };
        
        for (const permission of permissions) {
          const hasPermission = await this.hasPermission(userId, permission, context);
          if (!hasPermission) {
            logger.warn(`Access denied: User ${userId} lacks permission ${permission}`, { context });
            return res.status(403).json({
              error: 'Insufficient permissions',
              code: 'PERMISSION_DENIED',
              required: permissions
            });
          }
        }
        
        req.userPermissions = await this.getUserPermissions(userId);
        req.allowedBankingSystems = await this.getAllowedBankingSystems(userId);
        
        next();
      } catch (error) {
        logger.error('RBAC middleware error:', error);
        res.status(500).json({
          error: 'Authorization check failed',
          code: 'RBAC_ERROR'
        });
      }
    };
  }

  /**
   * Middleware to require any of the specified permissions
   */
  requireAnyPermission(permissions, options = {}) {
    return async (req, res, next) => {
      try {
        const userId = req.user?.userId;
        if (!userId) {
          return res.status(401).json({
            error: 'Authentication required',
            code: 'AUTH_REQUIRED'
          });
        }
        
        const context = {
          bankingSystem: options.bankingSystem || req.body?.bankingSystem || req.params?.bankingSystem,
          customerId: req.user?.customerId
        };
        
        let hasAnyPermission = false;
        for (const permission of permissions) {
          if (await this.hasPermission(userId, permission, context)) {
            hasAnyPermission = true;
            break;
          }
        }
        
        if (!hasAnyPermission) {
          logger.warn(`Access denied: User ${userId} lacks any of permissions`, { permissions, context });
          return res.status(403).json({
            error: 'Insufficient permissions',
            code: 'PERMISSION_DENIED',
            required: permissions
          });
        }
        
        req.userPermissions = await this.getUserPermissions(userId);
        req.allowedBankingSystems = await this.getAllowedBankingSystems(userId);
        
        next();
      } catch (error) {
        logger.error('RBAC middleware error:', error);
        res.status(500).json({
          error: 'Authorization check failed',
          code: 'RBAC_ERROR'
        });
      }
    };
  }

  /**
   * Admin function to grant permission to user
   */
  async grantPermission(adminUserId, targetUserId, permission) {
    try {
      // Check if admin has permission to manage users
      const canManage = await this.hasPermission(adminUserId, COBOL_PERMISSIONS.USER_MANAGE);
      if (!canManage) {
        throw new Error('Insufficient permissions to manage users');
      }
      
      // Implementation would update database and refresh cache
      // For now, we'll log the action
      logger.info(`Admin ${adminUserId} granted permission ${permission} to user ${targetUserId}`);
      
      // Refresh target user's permissions
      await this.refreshUserPermissions(targetUserId);
      
      return true;
    } catch (error) {
      logger.error('Failed to grant permission:', error);
      throw error;
    }
  }

  /**
   * Get permission summary for admin dashboard
   */
  async getPermissionSummary(adminUserId) {
    try {
      const canViewAudit = await this.hasPermission(adminUserId, COBOL_PERMISSIONS.AUDIT_VIEW);
      if (!canViewAudit) {
        throw new Error('Insufficient permissions to view permission summary');
      }
      
      const summary = {
        totalUsers: this.permissionCache.size,
        roleDistribution: {},
        bankingSystemAccess: {},
        cacheMetrics: {
          size: this.permissionCache.size,
          lastUpdated: new Date().toISOString()
        }
      };
      
      // Calculate role distribution from cache
      for (const [userId, userData] of this.permissionCache) {
        // This would be implemented with actual role data
        // For now, return mock summary
      }
      
      return summary;
    } catch (error) {
      logger.error('Failed to get permission summary:', error);
      throw error;
    }
  }

  /**
   * Clean up resources
   */
  async shutdown() {
    try {
      this.permissionCache.clear();
      await this.db.disconnect();
      logger.info('COBOL RBAC Manager shut down successfully');
    } catch (error) {
      logger.error('Error during RBAC shutdown:', error);
    }
  }
}

// Export singleton instance
const cobolRBAC = new CobolRBACManager();

module.exports = {
  CobolRBACManager,
  cobolRBAC,
  COBOL_USER_ROLES,
  COBOL_PERMISSIONS,
  BANKING_SYSTEM_ACCESS,
  ROLE_PERMISSIONS
};