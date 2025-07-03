package com.legacybaas.exceptions;

/**
 * Base exception for Legacy B2BaaS SDK
 */
public class LegacyBaaSException extends RuntimeException {
    
    private final String errorCode;
    private final int statusCode;
    
    public LegacyBaaSException(String message) {
        this(message, null, 0);
    }
    
    public LegacyBaaSException(String message, String errorCode) {
        this(message, errorCode, 0);
    }
    
    public LegacyBaaSException(String message, String errorCode, int statusCode) {
        super(message);
        this.errorCode = errorCode;
        this.statusCode = statusCode;
    }
    
    public LegacyBaaSException(String message, Throwable cause) {
        this(message, cause, null, 0);
    }
    
    public LegacyBaaSException(String message, Throwable cause, String errorCode, int statusCode) {
        super(message, cause);
        this.errorCode = errorCode;
        this.statusCode = statusCode;
    }
    
    public String getErrorCode() {
        return errorCode;
    }
    
    public int getStatusCode() {
        return statusCode;
    }
}