package com.legacybaas.exceptions;

/**
 * Validation related exceptions
 */
public class ValidationException extends LegacyBaaSException {
    
    public ValidationException(String message) {
        super(message, "VALIDATION_ERROR", 400);
    }
    
    public ValidationException(String message, Throwable cause) {
        super(message, cause, "VALIDATION_ERROR", 400);
    }
}