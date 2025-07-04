/**
 * COBOL Transpiler Middleware
 * LegacyBAAS Platform Integration
 * 
 * Custom middleware for COBOL transpiler specific operations
 * Handles request validation, quota management, and response formatting
 */

const winston = require('winston');
const rateLimit = require('express-rate-limit');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'cobol-middleware' }
});

/**
 * Request ID middleware - adds unique identifier to each request
 */
const requestId = (req, res, next) => {
  req.requestId = uuidv4();
  res.setHeader('X-Request-ID', req.requestId);
  
  logger.debug('Request received', {
    requestId: req.requestId,
    method: req.method,
    path: req.path,
    userAgent: req.get('User-Agent'),
    ip: req.ip
  });
  
  next();
};

/**
 * COBOL-specific rate limiting
 */
const cobolRateLimit = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: (req) => {
    // Different limits based on user role
    const role = req.user?.role;
    switch (role) {
      case 'system_admin': return 1000;
      case 'bank_admin': return 100;
      case 'si_developer': return 50;
      case 'bank_user': return 20;
      case 'reseller_partner': return 10;
      default: return 5;
    }
  },
  message: (req) => ({
    error: 'RATE_LIMIT_EXCEEDED',
    message: `Rate limit exceeded for ${req.user?.role || 'anonymous'}. Try again later.`,
    retryAfter: Math.ceil(15 * 60), // 15 minutes in seconds
    requestId: req.requestId,
    timestamp: new Date().toISOString()
  }),
  standardHeaders: true,
  legacyHeaders: false,
  keyGenerator: (req) => {
    // Rate limit by user ID if authenticated, otherwise by IP
    return req.user?.userId || req.ip;
  },
  skip: (req) => {
    // Skip rate limiting for health checks
    return req.path === '/api/v1/cobol/health';
  }
});

/**
 * COBOL code validation middleware
 */
const validateCobolCode = (req, res, next) => {
  const { cobolCode } = req.body;
  
  if (!cobolCode) {
    return res.status(400).json({
      error: 'VALIDATION_ERROR',
      message: 'COBOL code is required',
      field: 'cobolCode',
      requestId: req.requestId,
      timestamp: new Date().toISOString()
    });
  }
  
  if (typeof cobolCode !== 'string') {
    return res.status(400).json({
      error: 'VALIDATION_ERROR',
      message: 'COBOL code must be a string',
      field: 'cobolCode',
      requestId: req.requestId,
      timestamp: new Date().toISOString()
    });
  }
  
  // Check code length limits
  const maxLength = 100000; // 100KB
  if (cobolCode.length > maxLength) {
    return res.status(413).json({
      error: 'PAYLOAD_TOO_LARGE',
      message: `COBOL code exceeds maximum length of ${maxLength} characters`,
      actualLength: cobolCode.length,
      maxLength,
      requestId: req.requestId,
      timestamp: new Date().toISOString()
    });
  }
  
  // Basic COBOL syntax validation
  if (!cobolCode.toUpperCase().includes('IDENTIFICATION DIVISION')) {
    return res.status(422).json({
      error: 'INVALID_COBOL',
      message: 'COBOL code must include IDENTIFICATION DIVISION',
      requestId: req.requestId,
      timestamp: new Date().toISOString()
    });
  }
  
  logger.debug('COBOL code validation passed', {
    requestId: req.requestId,
    codeLength: cobolCode.length,
    userId: req.user?.userId
  });
  
  next();
};

/**
 * Banking system validation middleware
 */
const validateBankingSystem = (req, res, next) => {
  const { bankingSystem } = req.body;
  
  if (bankingSystem) {
    const supportedSystems = [
      'FIS_SYSTEMATICS',
      'FISERV_DNA', 
      'TEMENOS_TRANSACT',
      'TCS_BANCS'
    ];
    
    if (!supportedSystems.includes(bankingSystem)) {
      return res.status(400).json({
        error: 'VALIDATION_ERROR',
        message: 'Unsupported banking system',
        field: 'bankingSystem',
        providedValue: bankingSystem,
        supportedValues: supportedSystems,
        requestId: req.requestId,
        timestamp: new Date().toISOString()
      });
    }
  }
  
  next();
};

/**
 * Blockchain validation middleware
 */
const validateBlockchain = (req, res, next) => {
  const { targetBlockchain } = req.body;
  
  if (targetBlockchain) {
    const supportedBlockchains = ['ethereum', 'corda', 'algorand'];
    
    if (!supportedBlockchains.includes(targetBlockchain)) {
      return res.status(400).json({
        error: 'VALIDATION_ERROR',
        message: 'Unsupported blockchain platform',
        field: 'targetBlockchain',
        providedValue: targetBlockchain,
        supportedValues: supportedBlockchains,
        requestId: req.requestId,
        timestamp: new Date().toISOString()
      });
    }
  }
  
  next();
};

/**
 * Content type validation middleware
 */
const validateContentType = (req, res, next) => {
  if (req.method === 'POST' || req.method === 'PUT') {
    const contentType = req.get('Content-Type');
    
    if (!contentType || !contentType.includes('application/json')) {
      return res.status(415).json({
        error: 'UNSUPPORTED_MEDIA_TYPE',
        message: 'Content-Type must be application/json',
        providedContentType: contentType,
        requestId: req.requestId,
        timestamp: new Date().toISOString()
      });
    }
  }
  
  next();
};

/**
 * Quota validation middleware
 */
const validateQuota = async (req, res, next) => {
  try {
    const userContext = req.user;
    if (!userContext) {
      return next(); // Authentication middleware will handle this
    }
    
    // Check COBOL transpiler feature enablement
    const features = userContext.features?.cobol_transpiler;
    if (!features?.enabled) {
      return res.status(403).json({
        error: 'FEATURE_NOT_ENABLED',
        message: 'COBOL transpiler feature is not enabled for this customer',
        customerId: userContext.customerId,
        requestId: req.requestId,
        timestamp: new Date().toISOString()
      });
    }
    
    // Check monthly quota (mock implementation)
    const quota = features.quota || { transpilations_per_month: 100 };
    const currentUsage = await getCurrentUsage(userContext.customerId);
    
    if (currentUsage.transpilations >= quota.transpilations_per_month) {
      return res.status(429).json({
        error: 'QUOTA_EXCEEDED',
        message: 'Monthly transpilation quota exceeded',
        quota: quota.transpilations_per_month,
        currentUsage: currentUsage.transpilations,
        resetDate: getNextMonthReset(),
        requestId: req.requestId,
        timestamp: new Date().toISOString()
      });
    }
    
    // Add quota info to request
    req.quota = {
      limit: quota.transpilations_per_month,
      used: currentUsage.transpilations,
      remaining: quota.transpilations_per_month - currentUsage.transpilations
    };
    
    next();
    
  } catch (error) {
    logger.error('Quota validation error', {
      error: error.message,
      requestId: req.requestId,
      userId: req.user?.userId
    });
    
    // Fail open for availability
    next();
  }
};

/**
 * Response formatting middleware
 */
const formatResponse = (req, res, next) => {
  // Store original json method
  const originalJson = res.json;
  
  // Override json method to add standard fields
  res.json = function(data) {
    // Add standard response fields
    const response = {
      ...data,
      requestId: req.requestId,
      timestamp: data.timestamp || new Date().toISOString()
    };
    
    // Add quota info to successful responses
    if (req.quota && res.statusCode < 400) {
      response.quota = req.quota;
    }
    
    // Add processing time for successful operations
    if (req.startTime && res.statusCode < 400) {
      response.processingTime = Date.now() - req.startTime;
    }
    
    return originalJson.call(this, response);
  };
  
  // Record start time
  req.startTime = Date.now();
  
  next();
};

/**
 * Error handling middleware for COBOL operations
 */
const handleCobolErrors = (err, req, res, next) => {
  logger.error('COBOL operation error', {
    error: err.message,
    stack: err.stack,
    requestId: req.requestId,
    userId: req.user?.userId,
    path: req.path,
    method: req.method
  });
  
  // Determine error type and appropriate response
  let statusCode = 500;
  let errorCode = 'INTERNAL_ERROR';
  let message = 'An internal error occurred';
  
  if (err.message.includes('COBOL parsing failed')) {
    statusCode = 422;
    errorCode = 'PARSING_ERROR';
    message = 'COBOL code could not be parsed';
  } else if (err.message.includes('Authentication failed')) {
    statusCode = 401;
    errorCode = 'AUTHENTICATION_ERROR';
    message = 'Authentication failed';
  } else if (err.message.includes('Unsupported')) {
    statusCode = 400;
    errorCode = 'UNSUPPORTED_OPERATION';
    message = err.message;
  } else if (err.message.includes('Quota exceeded')) {
    statusCode = 429;
    errorCode = 'QUOTA_EXCEEDED';
    message = err.message;
  } else if (err.message.includes('Validation failed')) {
    statusCode = 422;
    errorCode = 'VALIDATION_ERROR';
    message = err.message;
  }
  
  res.status(statusCode).json({
    error: errorCode,
    message,
    requestId: req.requestId,
    timestamp: new Date().toISOString(),
    ...(process.env.NODE_ENV === 'development' && { details: err.message })
  });
};

/**
 * Logging middleware for COBOL operations
 */
const logCobolOperation = (req, res, next) => {
  const startTime = Date.now();
  
  // Log request
  logger.info('COBOL operation started', {
    requestId: req.requestId,
    method: req.method,
    path: req.path,
    userId: req.user?.userId,
    customerId: req.user?.customerId,
    userAgent: req.get('User-Agent'),
    ip: req.ip
  });
  
  // Override res.end to log completion
  const originalEnd = res.end;
  res.end = function(...args) {
    const duration = Date.now() - startTime;
    
    logger.info('COBOL operation completed', {
      requestId: req.requestId,
      statusCode: res.statusCode,
      duration,
      userId: req.user?.userId,
      success: res.statusCode < 400
    });
    
    originalEnd.apply(this, args);
  };
  
  next();
};

/**
 * Helper function to get current usage (mock implementation)
 */
async function getCurrentUsage(customerId) {
  // In production, this would query the usage database
  return {
    transpilations: Math.floor(Math.random() * 50), // Mock current usage
    deployments: Math.floor(Math.random() * 10)
  };
}

/**
 * Helper function to get next month reset date
 */
function getNextMonthReset() {
  const now = new Date();
  const nextMonth = new Date(now.getFullYear(), now.getMonth() + 1, 1);
  return nextMonth.toISOString();
}

module.exports = {
  requestId,
  cobolRateLimit,
  validateCobolCode,
  validateBankingSystem,
  validateBlockchain,
  validateContentType,
  validateQuota,
  formatResponse,
  handleCobolErrors,
  logCobolOperation
};