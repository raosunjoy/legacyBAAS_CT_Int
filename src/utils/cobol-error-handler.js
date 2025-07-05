const winston = require('winston');
const { v4: uuidv4 } = require('uuid');
const { transpilerAudit } = require('../audit/transpiler-audit');
const { TranspilerMetrics } = require('../monitoring/transpiler-metrics');

class CobolErrorHandler {
  constructor() {
    this.logger = winston.createLogger({
      level: 'info',
      format: winston.format.combine(
        winston.format.timestamp(),
        winston.format.errors({ stack: true }),
        winston.format.json()
      ),
      defaultMeta: { service: 'cobol-transpiler' },
      transports: [
        new winston.transports.File({ filename: 'logs/cobol-error.log', level: 'error' }),
        new winston.transports.File({ filename: 'logs/cobol-combined.log' }),
        new winston.transports.Console({
          format: winston.format.simple()
        })
      ]
    });

    this.metrics = new TranspilerMetrics();
    this.errorClassifications = {
      SYNTAX_ERROR: 'SYNTAX_ERROR',
      VALIDATION_ERROR: 'VALIDATION_ERROR',
      BUSINESS_LOGIC_ERROR: 'BUSINESS_LOGIC_ERROR',
      SYSTEM_ERROR: 'SYSTEM_ERROR',
      NETWORK_ERROR: 'NETWORK_ERROR',
      SECURITY_ERROR: 'SECURITY_ERROR',
      RESOURCE_ERROR: 'RESOURCE_ERROR',
      UNKNOWN_ERROR: 'UNKNOWN_ERROR'
    };

    this.severityLevels = {
      LOW: 'LOW',
      MEDIUM: 'MEDIUM',
      HIGH: 'HIGH',
      CRITICAL: 'CRITICAL'
    };

    this.userFriendlyMessages = {
      SYNTAX_ERROR: 'There was an issue with the COBOL syntax. Please check your code and try again.',
      VALIDATION_ERROR: 'The input data failed validation. Please verify your data and retry.',
      BUSINESS_LOGIC_ERROR: 'A business rule validation failed. Please review the requirements.',
      SYSTEM_ERROR: 'An internal system error occurred. Our team has been notified.',
      NETWORK_ERROR: 'A network connectivity issue occurred. Please try again later.',
      SECURITY_ERROR: 'A security validation failed. Please verify your credentials.',
      RESOURCE_ERROR: 'System resources are temporarily unavailable. Please try again later.',
      UNKNOWN_ERROR: 'An unexpected error occurred. Our team has been notified.'
    };

    this.recoveryStrategies = {
      SYNTAX_ERROR: this.recoverFromSyntaxError.bind(this),
      VALIDATION_ERROR: this.recoverFromValidationError.bind(this),
      BUSINESS_LOGIC_ERROR: this.recoverFromBusinessLogicError.bind(this),
      SYSTEM_ERROR: this.recoverFromSystemError.bind(this),
      NETWORK_ERROR: this.recoverFromNetworkError.bind(this),
      SECURITY_ERROR: this.recoverFromSecurityError.bind(this),
      RESOURCE_ERROR: this.recoverFromResourceError.bind(this),
      UNKNOWN_ERROR: this.recoverFromUnknownError.bind(this)
    };

    this.errorHistory = [];
    this.errorThresholds = {
      CRITICAL: 5,
      HIGH: 10,
      MEDIUM: 50,
      LOW: 100
    };

    this.setupErrorHandling();
  }

  setupErrorHandling() {
    process.on('uncaughtException', (error) => {
      this.handleCriticalError(error, 'UNCAUGHT_EXCEPTION');
      process.exit(1);
    });

    process.on('unhandledRejection', (reason, promise) => {
      this.handleCriticalError(reason, 'UNHANDLED_REJECTION', { promise });
    });
  }

  handleError(error, context = {}) {
    const errorId = uuidv4();
    const classification = this.classifyError(error);
    const severity = this.determineSeverity(error, classification);
    const userMessage = this.getUserFriendlyMessage(classification);
    
    const errorDetails = {
      id: errorId,
      timestamp: new Date().toISOString(),
      classification,
      severity,
      message: error.message,
      stack: error.stack,
      context,
      userMessage,
      recoverable: this.isRecoverable(classification)
    };

    this.logError(errorDetails);
    this.recordMetrics(errorDetails);
    this.auditError(errorDetails);
    this.checkErrorThresholds(classification, severity);

    if (errorDetails.recoverable) {
      return this.attemptRecovery(errorDetails);
    }

    return {
      success: false,
      errorId,
      userMessage,
      severity,
      recoverable: false
    };
  }

  classifyError(error) {
    if (error.name === 'SyntaxError' || error.message.includes('syntax')) {
      return this.errorClassifications.SYNTAX_ERROR;
    }
    if (error.name === 'ValidationError' || error.message.includes('validation')) {
      return this.errorClassifications.VALIDATION_ERROR;
    }
    if (error.message.includes('business') || error.message.includes('rule')) {
      return this.errorClassifications.BUSINESS_LOGIC_ERROR;
    }
    if (error.name === 'SystemError' || error.code === 'ENOSYS') {
      return this.errorClassifications.SYSTEM_ERROR;
    }
    if (error.code === 'ENOTFOUND' || error.code === 'ECONNREFUSED') {
      return this.errorClassifications.NETWORK_ERROR;
    }
    if (error.message.includes('auth') || error.message.includes('permission')) {
      return this.errorClassifications.SECURITY_ERROR;
    }
    if (error.code === 'ENOMEM' || error.code === 'EMFILE') {
      return this.errorClassifications.RESOURCE_ERROR;
    }
    return this.errorClassifications.UNKNOWN_ERROR;
  }

  determineSeverity(error, classification) {
    if (classification === this.errorClassifications.SECURITY_ERROR) {
      return this.severityLevels.CRITICAL;
    }
    if (classification === this.errorClassifications.SYSTEM_ERROR) {
      return this.severityLevels.HIGH;
    }
    if (classification === this.errorClassifications.NETWORK_ERROR || 
        classification === this.errorClassifications.RESOURCE_ERROR) {
      return this.severityLevels.MEDIUM;
    }
    return this.severityLevels.LOW;
  }

  getUserFriendlyMessage(classification) {
    return this.userFriendlyMessages[classification] || this.userFriendlyMessages.UNKNOWN_ERROR;
  }

  isRecoverable(classification) {
    const recoverableErrors = [
      this.errorClassifications.SYNTAX_ERROR,
      this.errorClassifications.VALIDATION_ERROR,
      this.errorClassifications.NETWORK_ERROR,
      this.errorClassifications.RESOURCE_ERROR
    ];
    return recoverableErrors.includes(classification);
  }

  logError(errorDetails) {
    this.logger.error({
      ...errorDetails,
      component: 'cobol-transpiler',
      version: process.env.npm_package_version || '1.0.0'
    });

    this.errorHistory.push(errorDetails);
    if (this.errorHistory.length > 1000) {
      this.errorHistory = this.errorHistory.slice(-500);
    }
  }

  recordMetrics(errorDetails) {
    this.metrics.recordError(new Error(errorDetails.message), {
      classification: errorDetails.classification,
      severity: errorDetails.severity,
      context: errorDetails.context
    });
  }

  auditError(errorDetails) {
    transpilerAudit.logEvent('error', {
      errorId: errorDetails.id,
      classification: errorDetails.classification,
      severity: errorDetails.severity,
      message: errorDetails.message,
      context: errorDetails.context
    });
  }

  checkErrorThresholds(classification, severity) {
    const recentErrors = this.errorHistory.filter(error => {
      const errorTime = new Date(error.timestamp);
      const oneHourAgo = new Date(Date.now() - 60 * 60 * 1000);
      return errorTime > oneHourAgo && error.severity === severity;
    });

    const threshold = this.errorThresholds[severity];
    if (recentErrors.length >= threshold) {
      this.handleThresholdExceeded(severity, recentErrors.length);
    }
  }

  handleThresholdExceeded(severity, errorCount) {
    this.logger.warn({
      message: 'Error threshold exceeded',
      severity,
      errorCount,
      threshold: this.errorThresholds[severity]
    });

    if (severity === this.severityLevels.CRITICAL) {
      this.triggerEmergencyProtocol();
    }
  }

  triggerEmergencyProtocol() {
    this.logger.error({
      message: 'Emergency protocol triggered - critical error threshold exceeded',
      timestamp: new Date().toISOString()
    });
  }

  attemptRecovery(errorDetails) {
    const recoveryStrategy = this.recoveryStrategies[errorDetails.classification];
    if (recoveryStrategy) {
      try {
        const recoveryResult = recoveryStrategy(errorDetails);
        if (recoveryResult.success) {
          this.logger.info({
            message: 'Error recovery successful',
            errorId: errorDetails.id,
            strategy: errorDetails.classification
          });
          return recoveryResult;
        }
      } catch (recoveryError) {
        this.logger.error({
          message: 'Error recovery failed',
          errorId: errorDetails.id,
          recoveryError: recoveryError.message
        });
      }
    }

    return {
      success: false,
      errorId: errorDetails.id,
      userMessage: errorDetails.userMessage,
      severity: errorDetails.severity,
      recoverable: false
    };
  }

  recoverFromSyntaxError(errorDetails) {
    return {
      success: false,
      userMessage: 'Please check your COBOL syntax and try again.',
      suggestions: [
        'Verify COBOL keywords are spelled correctly',
        'Check for missing periods at end of statements',
        'Ensure proper DATA DIVISION structure'
      ]
    };
  }

  recoverFromValidationError(errorDetails) {
    return {
      success: false,
      userMessage: 'Please verify your input data and try again.',
      suggestions: [
        'Check required fields are provided',
        'Verify data types match expected formats',
        'Ensure all constraints are met'
      ]
    };
  }

  recoverFromBusinessLogicError(errorDetails) {
    return {
      success: false,
      userMessage: 'A business rule validation failed.',
      suggestions: [
        'Review business requirements',
        'Check data constraints',
        'Verify calculation logic'
      ]
    };
  }

  recoverFromSystemError(errorDetails) {
    return {
      success: false,
      userMessage: 'A system error occurred. Please try again later.',
      suggestions: [
        'Try again in a few minutes',
        'Contact support if the issue persists'
      ]
    };
  }

  recoverFromNetworkError(errorDetails) {
    return {
      success: false,
      userMessage: 'Network connectivity issue. Please try again.',
      suggestions: [
        'Check your internet connection',
        'Try again in a few moments',
        'Verify service endpoints are accessible'
      ]
    };
  }

  recoverFromSecurityError(errorDetails) {
    return {
      success: false,
      userMessage: 'Authentication or authorization failed.',
      suggestions: [
        'Verify your credentials',
        'Check API key validity',
        'Ensure proper permissions'
      ]
    };
  }

  recoverFromResourceError(errorDetails) {
    return {
      success: false,
      userMessage: 'System resources temporarily unavailable.',
      suggestions: [
        'Try again in a few minutes',
        'Consider reducing request size',
        'Contact support for resource limits'
      ]
    };
  }

  recoverFromUnknownError(errorDetails) {
    return {
      success: false,
      userMessage: 'An unexpected error occurred.',
      suggestions: [
        'Try again later',
        'Contact support with error ID: ' + errorDetails.id
      ]
    };
  }

  handleCriticalError(error, type, context = {}) {
    const errorId = uuidv4();
    const criticalErrorDetails = {
      id: errorId,
      type,
      timestamp: new Date().toISOString(),
      severity: this.severityLevels.CRITICAL,
      message: error.message || error.toString(),
      stack: error.stack,
      context,
      pid: process.pid,
      memory: process.memoryUsage(),
      uptime: process.uptime()
    };

    this.logger.error({
      ...criticalErrorDetails,
      component: 'cobol-transpiler-critical'
    });

    this.auditError(criticalErrorDetails);
    this.recordMetrics(criticalErrorDetails);
    this.triggerEmergencyProtocol();
  }

  getErrorStatistics() {
    const now = new Date();
    const last24Hours = new Date(now.getTime() - 24 * 60 * 60 * 1000);
    const recentErrors = this.errorHistory.filter(error => 
      new Date(error.timestamp) > last24Hours
    );

    const statistics = {
      total: recentErrors.length,
      byClassification: {},
      bySeverity: {},
      byHour: {}
    };

    recentErrors.forEach(error => {
      statistics.byClassification[error.classification] = 
        (statistics.byClassification[error.classification] || 0) + 1;
      statistics.bySeverity[error.severity] = 
        (statistics.bySeverity[error.severity] || 0) + 1;
      
      const hour = new Date(error.timestamp).getHours();
      statistics.byHour[hour] = (statistics.byHour[hour] || 0) + 1;
    });

    return statistics;
  }

  createErrorReport(timeRange = '24h') {
    const statistics = this.getErrorStatistics();
    const topErrors = this.getTopErrors(10);
    const trends = this.getErrorTrends();

    return {
      timestamp: new Date().toISOString(),
      timeRange,
      statistics,
      topErrors,
      trends,
      recommendations: this.getRecommendations(statistics)
    };
  }

  getTopErrors(limit = 10) {
    const errorCounts = {};
    this.errorHistory.forEach(error => {
      const key = `${error.classification}:${error.message}`;
      errorCounts[key] = (errorCounts[key] || 0) + 1;
    });

    return Object.entries(errorCounts)
      .sort(([,a], [,b]) => b - a)
      .slice(0, limit)
      .map(([key, count]) => ({ error: key, count }));
  }

  getErrorTrends() {
    const hourlyTrends = {};
    this.errorHistory.forEach(error => {
      const hour = new Date(error.timestamp).getHours();
      hourlyTrends[hour] = (hourlyTrends[hour] || 0) + 1;
    });

    return {
      hourly: hourlyTrends,
      peak: Object.entries(hourlyTrends).reduce((a, b) => 
        hourlyTrends[a[0]] > hourlyTrends[b[0]] ? a : b
      )
    };
  }

  getRecommendations(statistics) {
    const recommendations = [];

    if (statistics.bySeverity.CRITICAL > 0) {
      recommendations.push('Critical errors detected - immediate attention required');
    }

    if (statistics.byClassification.SYNTAX_ERROR > 10) {
      recommendations.push('High syntax error rate - consider input validation improvements');
    }

    if (statistics.byClassification.NETWORK_ERROR > 5) {
      recommendations.push('Network connectivity issues - check service dependencies');
    }

    if (statistics.total > 100) {
      recommendations.push('High error rate - consider system health check');
    }

    return recommendations;
  }

  cleanup() {
    if (this.metrics) {
      this.metrics.cleanup();
    }
  }
}

module.exports = { CobolErrorHandler };