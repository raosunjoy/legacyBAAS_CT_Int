package com.legacybaas.exceptions;

/**
 * Authentication related exceptions
 */
public class AuthenticationException extends LegacyBaaSException {
    
    public AuthenticationException(String message) {
        super(message, "AUTHENTICATION_ERROR", 401);
    }
    
    public AuthenticationException(String message, Throwable cause) {
        super(message, cause, "AUTHENTICATION_ERROR", 401);
    }
}