package com.legacybaas;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import com.legacybaas.config.ClientConfig;
import com.legacybaas.config.Environment;
import com.legacybaas.exceptions.LegacyBaaSException;
import com.legacybaas.exceptions.AuthenticationException;
import com.legacybaas.exceptions.ValidationException;
import com.legacybaas.exceptions.NetworkException;
import com.legacybaas.exceptions.RateLimitException;
import com.legacybaas.exceptions.ServiceUnavailableException;
import com.legacybaas.http.HttpClient;
import com.legacybaas.models.common.ApiResponse;
import com.legacybaas.models.common.HealthStatus;
import com.legacybaas.services.SwiftProcessor;
import com.legacybaas.services.BlockchainRouter;
import com.legacybaas.services.BancsIntegration;
import com.legacybaas.services.AnalyticsService;
import com.legacybaas.services.WebhookHandler;
import com.legacybaas.utils.ApiKeyValidator;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.Closeable;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Main Legacy B2BaaS Platform Client for Java
 * 
 * <p>Provides access to all platform services including SWIFT processing,
 * blockchain routing, BaNCS integration, and analytics.</p>
 * 
 * <p>Example usage:</p>
 * <pre>{@code
 * LegacyBaaSClient client = LegacyBaaSClient.builder()
 *     .apiKey("your-api-key")
 *     .environment(Environment.PRODUCTION)
 *     .build();
 * 
 * // Check platform health
 * HealthStatus health = client.health().get();
 * System.out.println("Platform status: " + health.getStatus());
 * 
 * // Process SWIFT MT103 message
 * MT103Message mt103 = MT103Message.builder()
 *     .transactionReference("TXN123456")
 *     .valueDate(LocalDate.now())
 *     .currency("USD")
 *     .amount(BigDecimal.valueOf(10000))
 *     .orderingCustomer(Customer.builder()
 *         .account("123456789")
 *         .name("John Doe")
 *         .build())
 *     .beneficiaryCustomer(Customer.builder()
 *         .account("987654321")
 *         .name("Jane Smith")
 *         .build())
 *     .build();
 * 
 * SwiftProcessingResult result = client.swift().processMT103(mt103).get();
 * System.out.println("Transaction ID: " + result.getTransactionId());
 * 
 * // Close client when done
 * client.close();
 * }</pre>
 * 
 * @author Legacy B2BaaS Platform
 * @version 1.0.0
 * @since 1.0.0
 */
public class LegacyBaaSClient implements Closeable {
    
    private static final Logger logger = LoggerFactory.getLogger(LegacyBaaSClient.class);
    private static final String SDK_VERSION = "1.0.0";
    
    private final String apiKey;
    private final ClientConfig config;
    private final HttpClient httpClient;
    private final ObjectMapper objectMapper;
    
    // Service instances
    private final SwiftProcessor swiftProcessor;
    private final BlockchainRouter blockchainRouter;
    private final BancsIntegration bancsIntegration;
    private final AnalyticsService analyticsService;
    private final WebhookHandler webhookHandler;
    
    /**
     * Constructs a new Legacy B2BaaS client
     * 
     * @param apiKey Your Legacy B2BaaS API key
     * @param config Client configuration
     * @throws AuthenticationException if API key is invalid
     * @throws IllegalArgumentException if required parameters are null
     */
    public LegacyBaaSClient(@NotNull String apiKey, @NotNull ClientConfig config) {
        if (apiKey == null || apiKey.trim().isEmpty()) {
            throw new IllegalArgumentException("API key cannot be null or empty");
        }
        if (config == null) {
            throw new IllegalArgumentException("Client config cannot be null");
        }
        
        // Validate API key format
        if (!ApiKeyValidator.isValid(apiKey)) {
            throw new AuthenticationException("Invalid API key format");
        }
        
        this.apiKey = apiKey;
        this.config = config;
        
        // Initialize object mapper
        this.objectMapper = new ObjectMapper()
            .registerModule(new JavaTimeModule())
            .findAndRegisterModules();
        
        // Initialize HTTP client
        this.httpClient = new HttpClient(apiKey, config, objectMapper);
        
        // Initialize services
        this.swiftProcessor = new SwiftProcessor(httpClient);
        this.blockchainRouter = new BlockchainRouter(httpClient);
        this.bancsIntegration = new BancsIntegration(httpClient);
        this.analyticsService = new AnalyticsService(httpClient);
        this.webhookHandler = new WebhookHandler(httpClient);
        
        logger.info("Legacy B2BaaS Client initialized for environment: {}", config.getEnvironment());
    }
    
    /**
     * Creates a new builder for configuring the client
     * 
     * @return A new client builder
     */
    public static Builder builder() {
        return new Builder();
    }
    
    /**
     * Get SWIFT processing service
     * 
     * @return SWIFT processor instance
     */
    public SwiftProcessor swift() {
        return swiftProcessor;
    }
    
    /**
     * Get blockchain routing service
     * 
     * @return Blockchain router instance
     */
    public BlockchainRouter blockchain() {
        return blockchainRouter;
    }
    
    /**
     * Get BaNCS integration service
     * 
     * @return BaNCS integration instance
     */
    public BancsIntegration bancs() {
        return bancsIntegration;
    }
    
    /**
     * Get analytics service
     * 
     * @return Analytics service instance
     */
    public AnalyticsService analytics() {
        return analyticsService;
    }
    
    /**
     * Get webhook handler service
     * 
     * @return Webhook handler instance
     */
    public WebhookHandler webhooks() {
        return webhookHandler;
    }
    
    /**
     * Check platform health status
     * 
     * @return Future containing health status
     */
    public CompletableFuture<HealthStatus> health() {
        return httpClient.get("/health", HealthStatus.class);
    }
    
    /**
     * Test API connectivity
     * 
     * @return Future containing ping response
     */
    public CompletableFuture<Map<String, Object>> ping() {
        return httpClient.get("/ping", Map.class);
    }
    
    /**
     * Get account information
     * 
     * @return Future containing account details
     */
    public CompletableFuture<Map<String, Object>> getAccount() {
        return httpClient.get("/account", Map.class);
    }
    
    /**
     * Get API usage statistics
     * 
     * @param period Time period ("day", "week", "month")
     * @return Future containing usage statistics
     */
    public CompletableFuture<Map<String, Object>> getUsage(@NotNull String period) {
        return httpClient.get("/usage?period=" + period, Map.class);
    }
    
    /**
     * Get API usage statistics for the current day
     * 
     * @return Future containing usage statistics
     */
    public CompletableFuture<Map<String, Object>> getUsage() {
        return getUsage("day");
    }
    
    /**
     * Get client configuration
     * 
     * @return Client configuration
     */
    public ClientConfig getConfig() {
        return config;
    }
    
    /**
     * Get SDK version
     * 
     * @return SDK version string
     */
    public static String getSdkVersion() {
        return SDK_VERSION;
    }
    
    /**
     * Close the client and free resources
     */
    @Override
    public void close() {
        try {
            if (httpClient != null) {
                httpClient.close();
            }
            logger.info("Legacy B2BaaS Client closed");
        } catch (Exception e) {
            logger.warn("Error closing client: {}", e.getMessage());
        }
    }
    
    /**
     * Builder for creating LegacyBaaSClient instances
     */
    public static class Builder {
        private String apiKey;
        private Environment environment = Environment.PRODUCTION;
        private String baseUrl;
        private Duration timeout = Duration.ofSeconds(30);
        private int maxRetries = 3;
        private Duration retryDelay = Duration.ofSeconds(1);
        private boolean enableLogging = true;
        
        public Builder apiKey(@NotNull String apiKey) {
            this.apiKey = apiKey;
            return this;
        }
        
        public Builder environment(@NotNull Environment environment) {
            this.environment = environment;
            return this;
        }
        
        public Builder baseUrl(@NotNull String baseUrl) {
            this.baseUrl = baseUrl;
            return this;
        }
        
        public Builder timeout(@NotNull Duration timeout) {
            this.timeout = timeout;
            return this;
        }
        
        public Builder maxRetries(int maxRetries) {
            this.maxRetries = maxRetries;
            return this;
        }
        
        public Builder retryDelay(@NotNull Duration retryDelay) {
            this.retryDelay = retryDelay;
            return this;
        }
        
        public Builder enableLogging(boolean enableLogging) {
            this.enableLogging = enableLogging;
            return this;
        }
        
        public LegacyBaaSClient build() {
            if (apiKey == null || apiKey.trim().isEmpty()) {
                throw new IllegalArgumentException("API key is required");
            }
            
            ClientConfig config = ClientConfig.builder()
                .environment(environment)
                .baseUrl(baseUrl != null ? baseUrl : environment.getBaseUrl())
                .timeout(timeout)
                .maxRetries(maxRetries)
                .retryDelay(retryDelay)
                .enableLogging(enableLogging)
                .build();
                
            return new LegacyBaaSClient(apiKey, config);
        }
    }
}