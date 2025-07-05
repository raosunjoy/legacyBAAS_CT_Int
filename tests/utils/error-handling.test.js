const { describe, test, expect, beforeEach, afterEach, jest } = require('@jest/globals');
const { CobolErrorHandler } = require('../../src/utils/cobol-error-handler');

// Mock external dependencies
jest.mock('winston', () => ({
  createLogger: jest.fn(() => ({
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn()
  })),
  format: {
    combine: jest.fn(),
    timestamp: jest.fn(),
    errors: jest.fn(),
    json: jest.fn(),
    simple: jest.fn()
  },
  transports: {
    File: jest.fn(),
    Console: jest.fn()
  }
}));

jest.mock('../../src/audit/transpiler-audit', () => ({
  transpilerAudit: {
    logEvent: jest.fn()
  }
}));

jest.mock('../../src/monitoring/transpiler-metrics', () => ({
  TranspilerMetrics: jest.fn(() => ({
    recordError: jest.fn(),
    cleanup: jest.fn()
  }))
}));

describe('CobolErrorHandler', () => {
  let errorHandler;
  let originalProcessOn;
  let originalProcessExit;

  beforeEach(() => {
    errorHandler = new CobolErrorHandler();
    originalProcessOn = process.on;
    originalProcessExit = process.exit;
    process.on = jest.fn();
    process.exit = jest.fn();
  });

  afterEach(() => {
    process.on = originalProcessOn;
    process.exit = originalProcessExit;
    if (errorHandler) {
      errorHandler.cleanup();
    }
  });

  describe('Constructor and Initialization', () => {
    test('should initialize with default configurations', () => {
      expect(errorHandler.errorClassifications).toBeDefined();
      expect(errorHandler.severityLevels).toBeDefined();
      expect(errorHandler.userFriendlyMessages).toBeDefined();
      expect(errorHandler.recoveryStrategies).toBeDefined();
      expect(errorHandler.errorHistory).toEqual([]);
    });

    test('should set up error thresholds', () => {
      expect(errorHandler.errorThresholds).toEqual({
        CRITICAL: 5,
        HIGH: 10,
        MEDIUM: 50,
        LOW: 100
      });
    });

    test('should set up process error handlers', () => {
      expect(process.on).toHaveBeenCalledWith('uncaughtException', expect.any(Function));
      expect(process.on).toHaveBeenCalledWith('unhandledRejection', expect.any(Function));
    });
  });

  describe('Error Classification', () => {
    test('should classify syntax errors correctly', () => {
      const syntaxError = new SyntaxError('Invalid COBOL syntax');
      const classification = errorHandler.classifyError(syntaxError);
      expect(classification).toBe(errorHandler.errorClassifications.SYNTAX_ERROR);
    });

    test('should classify validation errors correctly', () => {
      const validationError = new Error('validation failed');
      const classification = errorHandler.classifyError(validationError);
      expect(classification).toBe(errorHandler.errorClassifications.VALIDATION_ERROR);
    });

    test('should classify business logic errors correctly', () => {
      const businessError = new Error('business rule violation');
      const classification = errorHandler.classifyError(businessError);
      expect(classification).toBe(errorHandler.errorClassifications.BUSINESS_LOGIC_ERROR);
    });

    test('should classify system errors correctly', () => {
      const systemError = new Error('system failure');
      systemError.name = 'SystemError';
      const classification = errorHandler.classifyError(systemError);
      expect(classification).toBe(errorHandler.errorClassifications.SYSTEM_ERROR);
    });

    test('should classify network errors correctly', () => {
      const networkError = new Error('connection failed');
      networkError.code = 'ECONNREFUSED';
      const classification = errorHandler.classifyError(networkError);
      expect(classification).toBe(errorHandler.errorClassifications.NETWORK_ERROR);
    });

    test('should classify security errors correctly', () => {
      const securityError = new Error('authentication failed');
      const classification = errorHandler.classifyError(securityError);
      expect(classification).toBe(errorHandler.errorClassifications.SECURITY_ERROR);
    });

    test('should classify resource errors correctly', () => {
      const resourceError = new Error('out of memory');
      resourceError.code = 'ENOMEM';
      const classification = errorHandler.classifyError(resourceError);
      expect(classification).toBe(errorHandler.errorClassifications.RESOURCE_ERROR);
    });

    test('should classify unknown errors correctly', () => {
      const unknownError = new Error('some random error');
      const classification = errorHandler.classifyError(unknownError);
      expect(classification).toBe(errorHandler.errorClassifications.UNKNOWN_ERROR);
    });
  });

  describe('Severity Determination', () => {
    test('should assign CRITICAL severity to security errors', () => {
      const severity = errorHandler.determineSeverity(
        new Error('test'), 
        errorHandler.errorClassifications.SECURITY_ERROR
      );
      expect(severity).toBe(errorHandler.severityLevels.CRITICAL);
    });

    test('should assign HIGH severity to system errors', () => {
      const severity = errorHandler.determineSeverity(
        new Error('test'), 
        errorHandler.errorClassifications.SYSTEM_ERROR
      );
      expect(severity).toBe(errorHandler.severityLevels.HIGH);
    });

    test('should assign MEDIUM severity to network errors', () => {
      const severity = errorHandler.determineSeverity(
        new Error('test'), 
        errorHandler.errorClassifications.NETWORK_ERROR
      );
      expect(severity).toBe(errorHandler.severityLevels.MEDIUM);
    });

    test('should assign LOW severity to syntax errors', () => {
      const severity = errorHandler.determineSeverity(
        new Error('test'), 
        errorHandler.errorClassifications.SYNTAX_ERROR
      );
      expect(severity).toBe(errorHandler.severityLevels.LOW);
    });
  });

  describe('User-Friendly Messages', () => {
    test('should provide user-friendly message for syntax errors', () => {
      const message = errorHandler.getUserFriendlyMessage(
        errorHandler.errorClassifications.SYNTAX_ERROR
      );
      expect(message).toBe('There was an issue with the COBOL syntax. Please check your code and try again.');
    });

    test('should provide user-friendly message for validation errors', () => {
      const message = errorHandler.getUserFriendlyMessage(
        errorHandler.errorClassifications.VALIDATION_ERROR
      );
      expect(message).toBe('The input data failed validation. Please verify your data and retry.');
    });

    test('should provide default message for unknown classifications', () => {
      const message = errorHandler.getUserFriendlyMessage('UNKNOWN_CLASSIFICATION');
      expect(message).toBe('An unexpected error occurred. Our team has been notified.');
    });
  });

  describe('Error Recovery', () => {
    test('should identify recoverable errors', () => {
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.SYNTAX_ERROR)).toBe(true);
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.VALIDATION_ERROR)).toBe(true);
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.NETWORK_ERROR)).toBe(true);
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.RESOURCE_ERROR)).toBe(true);
    });

    test('should identify non-recoverable errors', () => {
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.SECURITY_ERROR)).toBe(false);
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.SYSTEM_ERROR)).toBe(false);
      expect(errorHandler.isRecoverable(errorHandler.errorClassifications.BUSINESS_LOGIC_ERROR)).toBe(false);
    });

    test('should attempt recovery for recoverable errors', () => {
      const syntaxError = new SyntaxError('Invalid COBOL syntax');
      const result = errorHandler.handleError(syntaxError);
      
      expect(result.success).toBe(false);
      expect(result.suggestions).toBeDefined();
      expect(result.suggestions).toContain('Verify COBOL keywords are spelled correctly');
    });

    test('should provide specific recovery suggestions for validation errors', () => {
      const validationError = new Error('validation failed');
      const result = errorHandler.handleError(validationError);
      
      expect(result.suggestions).toContain('Check required fields are provided');
      expect(result.suggestions).toContain('Verify data types match expected formats');
    });

    test('should provide network error recovery suggestions', () => {
      const networkError = new Error('connection failed');
      networkError.code = 'ECONNREFUSED';
      const result = errorHandler.handleError(networkError);
      
      expect(result.suggestions).toContain('Check your internet connection');
      expect(result.suggestions).toContain('Try again in a few moments');
    });
  });

  describe('Error Handling Workflow', () => {
    test('should handle complete error workflow', () => {
      const testError = new Error('test error');
      const context = { sourceFile: 'test.cbl', operation: 'transpile' };
      
      const result = errorHandler.handleError(testError, context);
      
      expect(result.errorId).toBeDefined();
      expect(result.userMessage).toBeDefined();
      expect(result.severity).toBeDefined();
      expect(result.recoverable).toBeDefined();
      expect(errorHandler.errorHistory).toHaveLength(1);
    });

    test('should log error details correctly', () => {
      const testError = new Error('test error');
      const context = { sourceFile: 'test.cbl' };
      
      errorHandler.handleError(testError, context);
      
      expect(errorHandler.logger.error).toHaveBeenCalledWith(
        expect.objectContaining({
          message: 'test error',
          context,
          component: 'cobol-transpiler'
        })
      );
    });

    test('should record metrics for errors', () => {
      const testError = new Error('test error');
      errorHandler.handleError(testError);
      
      expect(errorHandler.metrics.recordError).toHaveBeenCalledWith(
        expect.any(Error),
        expect.objectContaining({
          classification: expect.any(String),
          severity: expect.any(String)
        })
      );
    });

    test('should audit error events', () => {
      const { transpilerAudit } = require('../../src/audit/transpiler-audit');
      const testError = new Error('test error');
      
      errorHandler.handleError(testError);
      
      expect(transpilerAudit.logEvent).toHaveBeenCalledWith(
        'error',
        expect.objectContaining({
          errorId: expect.any(String),
          classification: expect.any(String),
          severity: expect.any(String),
          message: 'test error'
        })
      );
    });
  });

  describe('Error Thresholds and Monitoring', () => {
    test('should track error history', () => {
      const testError = new Error('test error');
      
      errorHandler.handleError(testError);
      errorHandler.handleError(testError);
      errorHandler.handleError(testError);
      
      expect(errorHandler.errorHistory).toHaveLength(3);
    });

    test('should limit error history size', () => {
      const testError = new Error('test error');
      
      for (let i = 0; i < 1200; i++) {
        errorHandler.handleError(testError);
      }
      
      expect(errorHandler.errorHistory).toHaveLength(500);
    });

    test('should check error thresholds', () => {
      const criticalError = new Error('security breach');
      const spy = jest.spyOn(errorHandler, 'handleThresholdExceeded');
      
      // Create enough critical errors to exceed threshold
      for (let i = 0; i < 6; i++) {
        errorHandler.handleError(criticalError);
      }
      
      expect(spy).toHaveBeenCalledWith('LOW', expect.any(Number));
    });

    test('should trigger emergency protocol for critical threshold', () => {
      const spy = jest.spyOn(errorHandler, 'triggerEmergencyProtocol');
      
      errorHandler.handleThresholdExceeded('CRITICAL', 10);
      
      expect(spy).toHaveBeenCalled();
    });
  });

  describe('Critical Error Handling', () => {
    test('should handle critical errors with full context', () => {
      const criticalError = new Error('critical system failure');
      const context = { operation: 'deploy', target: 'ethereum' };
      
      errorHandler.handleCriticalError(criticalError, 'SYSTEM_FAILURE', context);
      
      expect(errorHandler.logger.error).toHaveBeenCalledWith(
        expect.objectContaining({
          type: 'SYSTEM_FAILURE',
          severity: 'CRITICAL',
          context,
          pid: expect.any(Number),
          memory: expect.any(Object),
          uptime: expect.any(Number)
        })
      );
    });

    test('should handle uncaught exceptions', () => {
      const uncaughtError = new Error('uncaught exception');
      const spy = jest.spyOn(errorHandler, 'handleCriticalError');
      
      // Simulate uncaught exception
      const uncaughtHandler = process.on.mock.calls.find(call => call[0] === 'uncaughtException')[1];
      uncaughtHandler(uncaughtError);
      
      expect(spy).toHaveBeenCalledWith(uncaughtError, 'UNCAUGHT_EXCEPTION');
      expect(process.exit).toHaveBeenCalledWith(1);
    });

    test('should handle unhandled rejections', () => {
      const rejectionReason = new Error('unhandled rejection');
      const promise = Promise.reject(rejectionReason);
      const spy = jest.spyOn(errorHandler, 'handleCriticalError');
      
      // Simulate unhandled rejection
      const rejectionHandler = process.on.mock.calls.find(call => call[0] === 'unhandledRejection')[1];
      rejectionHandler(rejectionReason, promise);
      
      expect(spy).toHaveBeenCalledWith(rejectionReason, 'UNHANDLED_REJECTION', { promise });
    });
  });

  describe('Error Statistics and Reporting', () => {
    beforeEach(() => {
      // Clear error history for clean statistics
      errorHandler.errorHistory = [];
    });

    test('should generate error statistics', () => {
      const syntaxError = new SyntaxError('syntax error');
      const validationError = new Error('validation failed');
      const networkError = new Error('network error');
      networkError.code = 'ECONNREFUSED';
      
      errorHandler.handleError(syntaxError);
      errorHandler.handleError(validationError);
      errorHandler.handleError(networkError);
      
      const stats = errorHandler.getErrorStatistics();
      
      expect(stats.total).toBe(3);
      expect(stats.byClassification.SYNTAX_ERROR).toBe(1);
      expect(stats.byClassification.VALIDATION_ERROR).toBe(1);
      expect(stats.byClassification.NETWORK_ERROR).toBe(1);
    });

    test('should create comprehensive error report', () => {
      const testError = new Error('test error');
      errorHandler.handleError(testError);
      
      const report = errorHandler.createErrorReport();
      
      expect(report.timestamp).toBeDefined();
      expect(report.timeRange).toBe('24h');
      expect(report.statistics).toBeDefined();
      expect(report.topErrors).toBeDefined();
      expect(report.trends).toBeDefined();
      expect(report.recommendations).toBeDefined();
    });

    test('should identify top errors', () => {
      const commonError = new Error('common error');
      const rareError = new Error('rare error');
      
      // Generate more common errors
      for (let i = 0; i < 5; i++) {
        errorHandler.handleError(commonError);
      }
      errorHandler.handleError(rareError);
      
      const topErrors = errorHandler.getTopErrors(2);
      
      expect(topErrors).toHaveLength(2);
      expect(topErrors[0].count).toBe(5);
      expect(topErrors[1].count).toBe(1);
    });

    test('should analyze error trends', () => {
      const testError = new Error('test error');
      errorHandler.handleError(testError);
      
      const trends = errorHandler.getErrorTrends();
      
      expect(trends.hourly).toBeDefined();
      expect(trends.peak).toBeDefined();
    });

    test('should provide recommendations based on error patterns', () => {
      const securityError = new Error('authentication failed');
      const syntaxError = new SyntaxError('syntax error');
      
      errorHandler.handleError(securityError);
      
      // Generate many syntax errors
      for (let i = 0; i < 12; i++) {
        errorHandler.handleError(syntaxError);
      }
      
      const stats = errorHandler.getErrorStatistics();
      const recommendations = errorHandler.getRecommendations(stats);
      
      expect(recommendations).toContain('Critical errors detected - immediate attention required');
      expect(recommendations).toContain('High syntax error rate - consider input validation improvements');
    });
  });

  describe('Recovery Strategies', () => {
    test('should provide syntax error recovery strategy', () => {
      const errorDetails = {
        classification: 'SYNTAX_ERROR',
        message: 'Invalid COBOL syntax'
      };
      
      const recovery = errorHandler.recoverFromSyntaxError(errorDetails);
      
      expect(recovery.success).toBe(false);
      expect(recovery.suggestions).toContain('Verify COBOL keywords are spelled correctly');
      expect(recovery.suggestions).toContain('Check for missing periods at end of statements');
    });

    test('should provide validation error recovery strategy', () => {
      const errorDetails = {
        classification: 'VALIDATION_ERROR',
        message: 'Invalid input data'
      };
      
      const recovery = errorHandler.recoverFromValidationError(errorDetails);
      
      expect(recovery.success).toBe(false);
      expect(recovery.suggestions).toContain('Check required fields are provided');
      expect(recovery.suggestions).toContain('Verify data types match expected formats');
    });

    test('should provide business logic error recovery strategy', () => {
      const errorDetails = {
        classification: 'BUSINESS_LOGIC_ERROR',
        message: 'Business rule violation'
      };
      
      const recovery = errorHandler.recoverFromBusinessLogicError(errorDetails);
      
      expect(recovery.success).toBe(false);
      expect(recovery.suggestions).toContain('Review business requirements');
      expect(recovery.suggestions).toContain('Check data constraints');
    });

    test('should provide system error recovery strategy', () => {
      const errorDetails = {
        classification: 'SYSTEM_ERROR',
        message: 'System failure'
      };
      
      const recovery = errorHandler.recoverFromSystemError(errorDetails);
      
      expect(recovery.success).toBe(false);
      expect(recovery.suggestions).toContain('Try again in a few minutes');
      expect(recovery.suggestions).toContain('Contact support if the issue persists');
    });
  });

  describe('Memory Management and Cleanup', () => {
    test('should cleanup resources', () => {
      const cleanupSpy = jest.spyOn(errorHandler.metrics, 'cleanup');
      
      errorHandler.cleanup();
      
      expect(cleanupSpy).toHaveBeenCalled();
    });

    test('should handle cleanup when metrics is null', () => {
      errorHandler.metrics = null;
      
      expect(() => errorHandler.cleanup()).not.toThrow();
    });
  });

  describe('Edge Cases and Error Handling', () => {
    test('should handle null error objects', () => {
      expect(() => errorHandler.handleError(null)).not.toThrow();
    });

    test('should handle errors without messages', () => {
      const errorWithoutMessage = new Error();
      delete errorWithoutMessage.message;
      
      const result = errorHandler.handleError(errorWithoutMessage);
      
      expect(result.errorId).toBeDefined();
      expect(result.userMessage).toBeDefined();
    });

    test('should handle errors without stack traces', () => {
      const errorWithoutStack = new Error('test error');
      delete errorWithoutStack.stack;
      
      const result = errorHandler.handleError(errorWithoutStack);
      
      expect(result.errorId).toBeDefined();
    });

    test('should handle recovery strategy failures', () => {
      const originalStrategy = errorHandler.recoverFromSyntaxError;
      errorHandler.recoverFromSyntaxError = jest.fn(() => {
        throw new Error('Recovery failed');
      });
      
      const syntaxError = new SyntaxError('test syntax error');
      const result = errorHandler.handleError(syntaxError);
      
      expect(result.success).toBe(false);
      expect(result.recoverable).toBe(false);
      
      // Restore original strategy
      errorHandler.recoverFromSyntaxError = originalStrategy;
    });
  });
});