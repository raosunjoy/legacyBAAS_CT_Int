/**
 * Authentication Middleware for LegacyBAAS Platform
 * COBOL Transpiler Integration
 * 
 * Provides OAuth2, JWT, and API key authentication for COBOL transpiler endpoints
 * Integrates with existing LegacyBAAS authentication infrastructure
 */

const jwt = require('jsonwebtoken');
const crypto = require('crypto');
const winston = require('winston');
const axios = require('axios');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'auth-middleware' }
});

/**
 * Authentication Types
 */
const AUTH_TYPES = {
  OAUTH2: 'oauth2',
  JWT: 'jwt',
  API_KEY: 'api_key',
  MUTUAL_TLS: 'mutual_tls'
};

/**
 * User Roles for COBOL Transpiler
 */
const USER_ROLES = {
  BANK_ADMIN: 'bank_admin',
  SI_DEVELOPER: 'si_developer', 
  BANK_USER: 'bank_user',
  RESELLER_PARTNER: 'reseller_partner',
  SYSTEM_ADMIN: 'system_admin'
};

/**
 * Permission Sets
 */
const PERMISSIONS = {
  COBOL_UPLOAD: 'cobol:upload',
  COBOL_TRANSPILE: 'cobol:transpile',
  COBOL_DEPLOY: 'cobol:deploy',
  COBOL_MANAGE: 'cobol:manage',
  COBOL_VIEW: 'cobol:view',
  COBOL_DOWNLOAD: 'cobol:download',
  BANKING_READ: 'banking:read',
  BANKING_WRITE: 'banking:write',
  BLOCKCHAIN_EXECUTE: 'blockchain:execute',
  COMPLIANCE_SCREEN: 'compliance:screen',
  ANALYTICS_VIEW: 'analytics:view',
  ADMIN_MANAGE: 'admin:manage'
};

/**
 * Role-Permission Mapping
 */
const ROLE_PERMISSIONS = {
  [USER_ROLES.BANK_ADMIN]: [
    PERMISSIONS.COBOL_UPLOAD,
    PERMISSIONS.COBOL_TRANSPILE,
    PERMISSIONS.COBOL_DEPLOY,
    PERMISSIONS.COBOL_MANAGE,
    PERMISSIONS.COBOL_VIEW,
    PERMISSIONS.COBOL_DOWNLOAD,
    PERMISSIONS.BANKING_READ,
    PERMISSIONS.BANKING_WRITE,
    PERMISSIONS.BLOCKCHAIN_EXECUTE,
    PERMISSIONS.COMPLIANCE_SCREEN,
    PERMISSIONS.ANALYTICS_VIEW
  ],
  
  [USER_ROLES.SI_DEVELOPER]: [
    PERMISSIONS.COBOL_UPLOAD,
    PERMISSIONS.COBOL_TRANSPILE,
    PERMISSIONS.COBOL_VIEW,
    PERMISSIONS.COBOL_DOWNLOAD,
    PERMISSIONS.BANKING_READ,
    PERMISSIONS.BLOCKCHAIN_EXECUTE,
    PERMISSIONS.ANALYTICS_VIEW
  ],
  
  [USER_ROLES.BANK_USER]: [
    PERMISSIONS.COBOL_VIEW,
    PERMISSIONS.COBOL_DOWNLOAD,
    PERMISSIONS.BANKING_READ,
    PERMISSIONS.ANALYTICS_VIEW
  ],
  
  [USER_ROLES.RESELLER_PARTNER]: [
    PERMISSIONS.COBOL_VIEW,
    PERMISSIONS.ANALYTICS_VIEW
  ],
  
  [USER_ROLES.SYSTEM_ADMIN]: Object.values(PERMISSIONS)
};

/**
 * Authentication Manager Class
 */
class AuthManager {
  constructor(config = {}) {
    this.config = {
      jwtSecret: config.jwtSecret || process.env.JWT_SECRET || 'legacy-baas-default-secret',
      jwtExpiry: config.jwtExpiry || process.env.JWT_EXPIRY || '1h',
      apiKeyHeader: config.apiKeyHeader || 'X-API-Key',
      oauth2Endpoint: config.oauth2Endpoint || process.env.OAUTH2_ENDPOINT,
      enableRateLimit: config.enableRateLimit !== false,
      enableAuditLog: config.enableAuditLog !== false,
      ...config
    };
    
    this.tokenCache = new Map();
    this.rateLimitCache = new Map();
    
    logger.info('AuthManager initialized', {
      jwtExpiry: this.config.jwtExpiry,
      enableRateLimit: this.config.enableRateLimit,
      enableAuditLog: this.config.enableAuditLog
    });
  }

  /**
   * Main authentication middleware
   */
  authenticate(options = {}) {
    return async (req, res, next) => {
      try {
        const startTime = Date.now();
        
        // Rate limiting check
        if (this.config.enableRateLimit) {
          const rateLimitResult = this.checkRateLimit(req);
          if (!rateLimitResult.allowed) {
            return this._sendErrorResponse(res, 429, 'RATE_LIMIT_EXCEEDED', 
              `Rate limit exceeded. Try again in ${rateLimitResult.retryAfter} seconds`);
          }
        }

        // Extract authentication credentials
        const authResult = await this._extractAuthentication(req);
        
        if (!authResult.success) {
          return this._sendErrorResponse(res, 401, 'AUTHENTICATION_REQUIRED', authResult.error);
        }

        // Validate and decode authentication
        const userContext = await this._validateAuthentication(authResult.type, authResult.credentials);
        
        if (!userContext) {
          return this._sendErrorResponse(res, 401, 'AUTHENTICATION_FAILED', 'Invalid credentials');
        }

        // Check permissions if required
        if (options.permissions && options.permissions.length > 0) {
          const hasPermission = this._checkPermissions(userContext, options.permissions);
          if (!hasPermission) {
            return this._sendErrorResponse(res, 403, 'AUTHORIZATION_FAILED', 
              `Required permissions: ${options.permissions.join(', ')}`);
          }
        }

        // Check COBOL transpiler specific quotas
        if (options.checkQuota) {
          const quotaCheck = await this._checkCobolQuota(userContext);
          if (!quotaCheck.allowed) {
            return this._sendErrorResponse(res, 429, 'QUOTA_EXCEEDED', quotaCheck.message);
          }
        }

        // Add user context to request
        req.user = userContext;
        req.authTime = Date.now() - startTime;

        // Audit logging
        if (this.config.enableAuditLog) {
          this._logAuthEvent(req, userContext, 'AUTHENTICATION_SUCCESS');
        }

        next();

      } catch (error) {
        logger.error('Authentication middleware error', {
          error: error.message,
          path: req.path,
          method: req.method
        });

        return this._sendErrorResponse(res, 500, 'AUTHENTICATION_ERROR', 'Internal authentication error');
      }
    };
  }

  /**
   * Extract authentication from request
   */
  async _extractAuthentication(req) {
    // Check for Bearer token (JWT or OAuth2)
    const authHeader = req.headers.authorization;
    if (authHeader && authHeader.startsWith('Bearer ')) {
      const token = authHeader.substring(7);
      
      // Determine if it's JWT or OAuth2 token
      if (this._isJWT(token)) {
        return { success: true, type: AUTH_TYPES.JWT, credentials: token };
      } else {
        return { success: true, type: AUTH_TYPES.OAUTH2, credentials: token };
      }
    }

    // Check for API key
    const apiKey = req.headers[this.config.apiKeyHeader.toLowerCase()];
    if (apiKey) {
      return { success: true, type: AUTH_TYPES.API_KEY, credentials: apiKey };
    }

    // Check for mutual TLS (certificate-based)
    if (req.client && req.client.authorized) {
      const cert = req.client.getPeerCertificate();
      return { success: true, type: AUTH_TYPES.MUTUAL_TLS, credentials: cert };
    }

    return { success: false, error: 'No authentication credentials provided' };
  }

  /**
   * Validate authentication credentials
   */
  async _validateAuthentication(type, credentials) {
    switch (type) {
      case AUTH_TYPES.JWT:
        return this._validateJWT(credentials);
      
      case AUTH_TYPES.OAUTH2:
        return this._validateOAuth2Token(credentials);
      
      case AUTH_TYPES.API_KEY:
        return this._validateAPIKey(credentials);
      
      case AUTH_TYPES.MUTUAL_TLS:
        return this._validateMutualTLS(credentials);
      
      default:
        return null;
    }
  }

  /**
   * Validate JWT token
   */
  _validateJWT(token) {
    try {
      const decoded = jwt.verify(token, this.config.jwtSecret);
      
      return {
        userId: decoded.sub,
        customerId: decoded.customerId,
        role: decoded.role,
        permissions: ROLE_PERMISSIONS[decoded.role] || [],
        authType: AUTH_TYPES.JWT,
        exp: decoded.exp,
        iat: decoded.iat,
        features: decoded.features || {}
      };
    } catch (error) {
      logger.warn('JWT validation failed', { error: error.message });
      return null;
    }
  }

  /**
   * Validate OAuth2 token
   */
  async _validateOAuth2Token(token) {
    try {
      // Check cache first
      if (this.tokenCache.has(token)) {
        const cached = this.tokenCache.get(token);
        if (cached.expiry > Date.now()) {
          return cached.userContext;
        }
        this.tokenCache.delete(token);
      }

      // Validate with OAuth2 provider
      if (!this.config.oauth2Endpoint) {
        throw new Error('OAuth2 endpoint not configured');
      }

      const response = await axios.get(`${this.config.oauth2Endpoint}/validate`, {
        headers: { Authorization: `Bearer ${token}` },
        timeout: 5000
      });

      const userContext = {
        userId: response.data.sub,
        customerId: response.data.customerId,
        role: response.data.role,
        permissions: ROLE_PERMISSIONS[response.data.role] || [],
        authType: AUTH_TYPES.OAUTH2,
        scopes: response.data.scope ? response.data.scope.split(' ') : [],
        features: response.data.features || {}
      };

      // Cache the result
      this.tokenCache.set(token, {
        userContext,
        expiry: Date.now() + (30 * 60 * 1000) // 30 minutes
      });

      return userContext;

    } catch (error) {
      logger.warn('OAuth2 token validation failed', { error: error.message });
      return null;
    }
  }

  /**
   * Validate API key
   */
  async _validateAPIKey(apiKey) {
    try {
      // Hash the API key for security
      const hashedKey = crypto.createHash('sha256').update(apiKey).digest('hex');
      
      // In production, this would query a secure database
      // For now, return a mock user context
      if (this._isValidAPIKey(hashedKey)) {
        return {
          userId: 'api-user',
          customerId: 'api-customer',
          role: USER_ROLES.SI_DEVELOPER,
          permissions: ROLE_PERMISSIONS[USER_ROLES.SI_DEVELOPER],
          authType: AUTH_TYPES.API_KEY,
          features: { cobol_transpiler: { enabled: true } }
        };
      }

      return null;
    } catch (error) {
      logger.warn('API key validation failed', { error: error.message });
      return null;
    }
  }

  /**
   * Validate mutual TLS certificate
   */
  _validateMutualTLS(cert) {
    try {
      // Validate certificate properties
      if (!cert.subject || !cert.issuer) {
        return null;
      }

      // Extract client information from certificate
      const clientId = cert.subject.CN;
      const orgUnit = cert.subject.OU;

      return {
        userId: clientId,
        customerId: orgUnit,
        role: USER_ROLES.BANK_ADMIN,
        permissions: ROLE_PERMISSIONS[USER_ROLES.BANK_ADMIN],
        authType: AUTH_TYPES.MUTUAL_TLS,
        certificate: {
          subject: cert.subject,
          issuer: cert.issuer,
          valid_from: cert.valid_from,
          valid_to: cert.valid_to
        },
        features: { cobol_transpiler: { enabled: true } }
      };
    } catch (error) {
      logger.warn('Mutual TLS validation failed', { error: error.message });
      return null;
    }
  }

  /**
   * Check user permissions
   */
  _checkPermissions(userContext, requiredPermissions) {
    if (!userContext.permissions || !Array.isArray(requiredPermissions)) {
      return false;
    }

    return requiredPermissions.every(permission => 
      userContext.permissions.includes(permission)
    );
  }

  /**
   * Check COBOL transpiler quota
   */
  async _checkCobolQuota(userContext) {
    try {
      // Get customer configuration
      const features = userContext.features?.cobol_transpiler;
      if (!features?.enabled) {
        return {
          allowed: false,
          message: 'COBOL transpiler not enabled for this customer'
        };
      }

      // Check monthly quota (would query database in production)
      const currentUsage = await this._getCurrentUsage(userContext.customerId);
      const quota = features.quota || { transpilations_per_month: 100 };

      if (currentUsage.transpilations >= quota.transpilations_per_month) {
        return {
          allowed: false,
          message: `Monthly transpilation quota exceeded (${quota.transpilations_per_month})`
        };
      }

      return { allowed: true };
    } catch (error) {
      logger.error('Quota check failed', { error: error.message });
      return { allowed: true }; // Fail open for availability
    }
  }

  /**
   * Rate limiting check
   */
  checkRateLimit(req) {
    const key = this._getRateLimitKey(req);
    const now = Date.now();
    const windowMs = 60 * 1000; // 1 minute window
    const maxRequests = 100; // requests per window

    if (!this.rateLimitCache.has(key)) {
      this.rateLimitCache.set(key, { count: 1, windowStart: now });
      return { allowed: true };
    }

    const record = this.rateLimitCache.get(key);
    
    if (now - record.windowStart > windowMs) {
      // New window
      record.count = 1;
      record.windowStart = now;
      return { allowed: true };
    }

    if (record.count >= maxRequests) {
      const retryAfter = Math.ceil((windowMs - (now - record.windowStart)) / 1000);
      return { allowed: false, retryAfter };
    }

    record.count++;
    return { allowed: true };
  }

  /**
   * Helper methods
   */
  _isJWT(token) {
    return token.split('.').length === 3;
  }

  _isValidAPIKey(hashedKey) {
    // In production, this would check against a secure database
    // For demo purposes, accept any non-empty hash
    return hashedKey && hashedKey.length === 64;
  }

  _getRateLimitKey(req) {
    const ip = req.ip || req.connection.remoteAddress;
    const userAgent = req.get('User-Agent') || 'unknown';
    return crypto.createHash('md5').update(ip + userAgent).digest('hex');
  }

  async _getCurrentUsage(customerId) {
    // In production, this would query the usage database
    return {
      transpilations: 25,
      deployments: 5
    };
  }

  _logAuthEvent(req, userContext, event) {
    logger.info('Authentication event', {
      event,
      userId: userContext.userId,
      customerId: userContext.customerId,
      role: userContext.role,
      authType: userContext.authType,
      path: req.path,
      method: req.method,
      ip: req.ip,
      userAgent: req.get('User-Agent'),
      timestamp: new Date().toISOString()
    });
  }

  _sendErrorResponse(res, status, code, message) {
    res.status(status).json({
      error: {
        code,
        message,
        timestamp: new Date().toISOString()
      }
    });
  }

  /**
   * Generate JWT token (for testing/development)
   */
  generateJWT(payload) {
    return jwt.sign(payload, this.config.jwtSecret, {
      expiresIn: this.config.jwtExpiry
    });
  }

  /**
   * Middleware factory for specific permissions
   */
  requirePermissions(...permissions) {
    return this.authenticate({ permissions });
  }

  /**
   * Middleware factory for COBOL operations with quota check
   */
  requireCobolAccess(...permissions) {
    return this.authenticate({ 
      permissions: [PERMISSIONS.COBOL_TRANSPILE, ...permissions],
      checkQuota: true 
    });
  }
}

module.exports = {
  AuthManager,
  AUTH_TYPES,
  USER_ROLES,
  PERMISSIONS,
  ROLE_PERMISSIONS
};