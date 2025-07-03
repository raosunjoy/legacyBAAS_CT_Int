package com.legacybaas.config;

/**
 * Supported environments for Legacy B2BaaS Platform
 */
public enum Environment {
    PRODUCTION("https://api.legacybaas.com"),
    STAGING("https://staging-api.legacybaas.com"),
    SANDBOX("https://sandbox-api.legacybaas.com");

    private final String baseUrl;

    Environment(String baseUrl) {
        this.baseUrl = baseUrl;
    }

    public String getBaseUrl() {
        return baseUrl;
    }

    @Override
    public String toString() {
        return name().toLowerCase();
    }
}