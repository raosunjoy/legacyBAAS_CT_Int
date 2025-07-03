package com.legacybaas.exceptions;

/**
 * Network related exceptions
 */
public class NetworkException extends LegacyBaaSException {
    
    public NetworkException(String message) {
        super(message, "NETWORK_ERROR");
    }
    
    public NetworkException(String message, Throwable cause) {
        super(message, cause, "NETWORK_ERROR", 0);
    }
}