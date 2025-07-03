package com.legacybaas.utils;

/**
 * API key validation utilities
 */
public class ApiKeyValidator {
    
    private static final int MIN_API_KEY_LENGTH = 20;
    
    /**
     * Validate API key format
     * 
     * @param apiKey The API key to validate
     * @return true if valid, false otherwise
     */
    public static boolean isValid(String apiKey) {
        if (apiKey == null || apiKey.trim().isEmpty()) {
            return false;
        }
        
        return apiKey.length() >= MIN_API_KEY_LENGTH;
    }
}