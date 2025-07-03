package com.legacybaas.config;

import java.time.Duration;

/**
 * Configuration for Legacy B2BaaS Client
 */
public class ClientConfig {
    
    private final Environment environment;
    private final String baseUrl;
    private final Duration timeout;
    private final int maxRetries;
    private final Duration retryDelay;
    private final boolean enableLogging;
    
    private ClientConfig(Builder builder) {
        this.environment = builder.environment;
        this.baseUrl = builder.baseUrl != null ? builder.baseUrl : builder.environment.getBaseUrl();
        this.timeout = builder.timeout;
        this.maxRetries = builder.maxRetries;
        this.retryDelay = builder.retryDelay;
        this.enableLogging = builder.enableLogging;
    }
    
    public static Builder builder() {
        return new Builder();
    }
    
    // Getters
    public Environment getEnvironment() { return environment; }
    public String getBaseUrl() { return baseUrl; }
    public Duration getTimeout() { return timeout; }
    public int getMaxRetries() { return maxRetries; }
    public Duration getRetryDelay() { return retryDelay; }
    public boolean isEnableLogging() { return enableLogging; }
    
    public static class Builder {
        private Environment environment = Environment.PRODUCTION;
        private String baseUrl;
        private Duration timeout = Duration.ofSeconds(30);
        private int maxRetries = 3;
        private Duration retryDelay = Duration.ofSeconds(1);
        private boolean enableLogging = true;
        
        public Builder environment(Environment environment) {
            this.environment = environment;
            return this;
        }
        
        public Builder baseUrl(String baseUrl) {
            this.baseUrl = baseUrl;
            return this;
        }
        
        public Builder timeout(Duration timeout) {
            this.timeout = timeout;
            return this;
        }
        
        public Builder maxRetries(int maxRetries) {
            this.maxRetries = maxRetries;
            return this;
        }
        
        public Builder retryDelay(Duration retryDelay) {
            this.retryDelay = retryDelay;
            return this;
        }
        
        public Builder enableLogging(boolean enableLogging) {
            this.enableLogging = enableLogging;
            return this;
        }
        
        public ClientConfig build() {
            return new ClientConfig(this);
        }
    }
    
    @Override
    public String toString() {
        return String.format("ClientConfig{environment=%s, baseUrl='%s', timeout=%s}", 
            environment, baseUrl, timeout);
    }
}