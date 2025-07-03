package com.legacybaas.exceptions;

/**
 * Rate limiting exceptions
 */
public class RateLimitException extends LegacyBaaSException {
    
    public RateLimitException(String message) {
        super(message, "RATE_LIMIT_ERROR", 429);
    }
    
    public RateLimitException(String message, Throwable cause) {
        super(message, cause, "RATE_LIMIT_ERROR", 429);
    }
}