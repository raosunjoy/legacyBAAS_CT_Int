/**
 * LegacyBAAS Java Client
 * Main client for all SDK operations
 */

package com.legacybaas.sdk;

import com.legacybaas.sdk.errors.*;
import com.legacybaas.sdk.services.*;
import com.legacybaas.sdk.utils.Logger;
import com.legacybaas.sdk.utils.RetryHandler;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.HashMap;
import java.util.concurrent.CompletableFuture;

/**
 * Main LegacyBAAS client for Java SDK
 */
public class LegacyBaaSClient {
    private final String clientId;
    private final String clientSecret;
    private final String baseUrl;
    private final String environment;
    private final Duration timeout;
    private final int maxRetries;
    private final Duration retryDelay;
    private final Logger logger;
    
    private String accessToken;
    private Instant tokenExpiry;
    
    private final HttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final RetryHandler retryHandler;
    
    // Services
    private final SwiftProcessor swiftProcessor;
    private final BlockchainRouter blockchainRouter;
    private final BankingService bankingService;
    private final AnalyticsService analyticsService;
    private final WebhookHandler webhookHandler;
    private final ComplianceService complianceService;
    
    /**
     * Constructor
     */
    public LegacyBaaSClient(Builder builder) {
        this.clientId = builder.clientId;
        this.clientSecret = builder.clientSecret;
        this.baseUrl = builder.baseUrl != null ? builder.baseUrl : getBaseUrl(builder.environment);
        this.environment = builder.environment;
        this.timeout = builder.timeout;
        this.maxRetries = builder.maxRetries;
        this.retryDelay = builder.retryDelay;
        this.logger = builder.enableLogging ? new Logger(builder.logLevel) : null;
        
        // Initialize HTTP client
        this.httpClient = HttpClient.newBuilder()
            .connectTimeout(timeout)
            .build();
        
        this.objectMapper = new ObjectMapper();
        this.retryHandler = new RetryHandler(maxRetries, retryDelay);
        
        // Initialize services
        this.swiftProcessor = new SwiftProcessor(this);
        this.blockchainRouter = new BlockchainRouter(this);
        this.bankingService = new BankingService(this);
        this.analyticsService = new AnalyticsService(this);
        this.webhookHandler = new WebhookHandler(this);
        this.complianceService = new ComplianceService(this);
        
        if (logger != null) {
            logger.info("LegacyBAAS Client initialized - Environment: " + environment + ", Base URL: " + baseUrl);
        }
    }
    
    /**
     * Builder class for LegacyBaaSClient
     */
    public static class Builder {
        private String clientId;
        private String clientSecret;
        private String baseUrl;
        private String environment = "production";
        private Duration timeout = Duration.ofSeconds(30);
        private int maxRetries = 3;
        private Duration retryDelay = Duration.ofSeconds(1);
        private boolean enableLogging = true;
        private String logLevel = "INFO";
        
        public Builder clientId(String clientId) {
            this.clientId = clientId;
            return this;
        }
        
        public Builder clientSecret(String clientSecret) {
            this.clientSecret = clientSecret;
            return this;
        }
        
        public Builder baseUrl(String baseUrl) {
            this.baseUrl = baseUrl;
            return this;
        }
        
        public Builder environment(String environment) {
            this.environment = environment;
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
        
        public Builder logLevel(String logLevel) {
            this.logLevel = logLevel;
            return this;
        }
        
        public LegacyBaaSClient build() {
            if (clientId == null || clientSecret == null) {
                throw new IllegalArgumentException("Client ID and Client Secret are required");
            }
            return new LegacyBaaSClient(this);
        }
    }
    
    /**
     * Get base URL for environment
     */
    private String getBaseUrl(String environment) {
        switch (environment.toLowerCase()) {
            case "production":
                return "https://api.legacybaas.com/v1";
            case "sandbox":
                return "https://sandbox.legacybaas.com/v1";
            case "development":
                return "http://localhost:3000/v1";
            default:
                return "https://api.legacybaas.com/v1";
        }
    }
    
    /**
     * Authenticate with OAuth2 client credentials flow
     */
    public CompletableFuture<Map<String, Object>> authenticate() {
        if (clientId == null || clientSecret == null) {
            return CompletableFuture.failedFuture(
                new AuthenticationError("Client ID and Client Secret are required for authentication")
            );
        }
        
        Map<String, Object> data = new HashMap<>();
        data.put("grant_type", "client_credentials");
        data.put("client_id", clientId);
        data.put("client_secret", clientSecret);
        data.put("scope", "banking:read banking:write blockchain:execute compliance:screen");
        
        return makeRequest("POST", "/auth/token", data, true)
            .thenApply(response -> {
                Map<String, Object> tokenData = (Map<String, Object>) response;
                this.accessToken = (String) tokenData.get("access_token");
                Integer expiresIn = (Integer) tokenData.get("expires_in");
                this.tokenExpiry = Instant.now().plusSeconds(expiresIn);
                
                if (logger != null) {
                    logger.info("Authentication successful");
                }
                
                return tokenData;
            })
            .exceptionally(throwable -> {
                if (logger != null) {
                    logger.error("Authentication failed: " + throwable.getMessage());
                }
                throw new AuthenticationError("Failed to authenticate with LegacyBAAS API");
            });
    }
    
    /**
     * Check if client has valid authentication token
     */
    public boolean isAuthenticated() {
        return accessToken != null && tokenExpiry != null && Instant.now().isBefore(tokenExpiry);
    }
    
    /**
     * Ensure client is authenticated
     */
    public CompletableFuture<Void> ensureAuthenticated() {
        if (!isAuthenticated()) {
            return authenticate().thenAccept(response -> {});
        }
        return CompletableFuture.completedFuture(null);
    }
    
    /**
     * Make HTTP request to API
     */
    public CompletableFuture<Object> makeRequest(String method, String endpoint, Object data, boolean skipAuth) {
        CompletableFuture<Void> authFuture = skipAuth ? 
            CompletableFuture.completedFuture(null) : 
            ensureAuthenticated();
            
        return authFuture.thenCompose(v -> {
            try {
                String url = baseUrl + endpoint;
                
                HttpRequest.Builder requestBuilder = HttpRequest.newBuilder()
                    .uri(URI.create(url))
                    .timeout(timeout)
                    .header("Content-Type", "application/json")
                    .header("User-Agent", "LegacyBAAS-Java-SDK/2.0.0")
                    .header("Accept", "application/json");
                
                if (accessToken != null && !skipAuth) {
                    requestBuilder.header("Authorization", "Bearer " + accessToken);
                }
                
                HttpRequest request;
                switch (method.toUpperCase()) {
                    case "GET":
                        request = requestBuilder.GET().build();
                        break;
                    case "POST":
                        String jsonData = data != null ? objectMapper.writeValueAsString(data) : "";
                        request = requestBuilder.POST(HttpRequest.BodyPublishers.ofString(jsonData)).build();
                        break;
                    case "PUT":
                        String putData = data != null ? objectMapper.writeValueAsString(data) : "";
                        request = requestBuilder.PUT(HttpRequest.BodyPublishers.ofString(putData)).build();
                        break;
                    case "DELETE":
                        request = requestBuilder.DELETE().build();
                        break;
                    default:
                        throw new IllegalArgumentException("Unsupported HTTP method: " + method);
                }
                
                if (logger != null) {
                    logger.debug("Making " + method + " request to " + endpoint);
                }
                
                return httpClient.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                    .thenApply(this::handleResponse);
                    
            } catch (Exception e) {
                return CompletableFuture.failedFuture(new NetworkError("Request failed: " + e.getMessage()));
            }
        });
    }
    
    /**
     * Handle HTTP response
     */
    private Object handleResponse(HttpResponse<String> response) {
        try {
            if (response.statusCode() >= 400) {
                JsonNode errorData;
                try {
                    errorData = objectMapper.readTree(response.body());
                } catch (Exception e) {
                    errorData = objectMapper.createObjectNode().put("message", response.body());
                }
                
                String message = errorData.has("message") ? errorData.get("message").asText() : "HTTP " + response.statusCode();
                
                switch (response.statusCode()) {
                    case 401:
                        throw new AuthenticationError(message);
                    case 400:
                        throw new ValidationError(message);
                    case 429:
                        throw new RateLimitError(message);
                    case 500:
                    case 502:
                    case 503:
                    case 504:
                        throw new NetworkError(message);
                    default:
                        throw new LegacyBaaSError(message, response.statusCode());
                }
            }
            
            if (logger != null) {
                logger.debug("Request completed: " + response.request().method() + " " + response.request().uri());
            }
            
            // Parse response
            JsonNode responseData = objectMapper.readTree(response.body());
            if (responseData.has("data")) {
                return objectMapper.convertValue(responseData.get("data"), Object.class);
            }
            return objectMapper.convertValue(responseData, Object.class);
            
        } catch (IOException e) {
            throw new NetworkError("Failed to parse response: " + e.getMessage());
        }
    }
    
    // Service getters
    public SwiftProcessor getSwiftProcessor() { return swiftProcessor; }
    public BlockchainRouter getBlockchainRouter() { return blockchainRouter; }
    public BankingService getBankingService() { return bankingService; }
    public AnalyticsService getAnalyticsService() { return analyticsService; }
    public WebhookHandler getWebhookHandler() { return webhookHandler; }
    public ComplianceService getComplianceService() { return complianceService; }
    
    /**
     * Get API health status
     */
    public CompletableFuture<Object> getHealth() {
        return makeRequest("GET", "/health", null, false);
    }
    
    /**
     * Get platform metrics
     */
    public CompletableFuture<Object> getMetrics() {
        return makeRequest("GET", "/admin/metrics", null, false);
    }
    
    /**
     * Close client and cleanup resources
     */
    public void close() {
        this.accessToken = null;
        this.tokenExpiry = null;
        
        if (logger != null) {
            logger.info("LegacyBAAS Client closed");
        }
    }
}