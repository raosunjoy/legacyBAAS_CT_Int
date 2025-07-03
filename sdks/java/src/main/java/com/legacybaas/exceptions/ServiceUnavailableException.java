package com.legacybaas.exceptions;

/**
 * Service unavailable exceptions
 */
public class ServiceUnavailableException extends LegacyBaaSException {
    
    public ServiceUnavailableException(String message) {
        super(message, "SERVICE_UNAVAILABLE", 503);
    }
    
    public ServiceUnavailableException(String message, Throwable cause) {
        super(message, cause, "SERVICE_UNAVAILABLE", 503);
    }
}