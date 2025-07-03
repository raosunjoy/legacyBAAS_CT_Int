package com.legacybaas;

import com.legacybaas.config.ClientConfig;
import com.legacybaas.config.Environment;
import com.legacybaas.exceptions.AuthenticationException;
import com.legacybaas.models.common.HealthStatus;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.io.IOException;
import java.time.Duration;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

import static org.junit.jupiter.api.Assertions.*;

@DisplayName("Legacy B2BaaS Client Tests")
class LegacyBaaSClientTest {

    private MockWebServer mockWebServer;
    private String baseUrl;
    private final String validApiKey = "test_api_key_12345678901234567890";

    @BeforeEach
    void setUp() throws IOException {
        mockWebServer = new MockWebServer();
        mockWebServer.start();
        baseUrl = mockWebServer.url("/").toString().replaceAll("/$", "");
    }

    @AfterEach
    void tearDown() throws IOException {
        mockWebServer.shutdown();
    }

    @Test
    @DisplayName("Should initialize client successfully with valid API key")
    void testClientInitializationSuccess() {
        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        assertNotNull(client);
        assertNotNull(client.swift());
        assertNotNull(client.blockchain());
        assertNotNull(client.bancs());
        assertNotNull(client.analytics());
        assertNotNull(client.webhooks());
        assertEquals("1.0.0", LegacyBaaSClient.getSdkVersion());
        
        client.close();
    }

    @Test
    @DisplayName("Should throw AuthenticationException for invalid API key")
    void testClientInitializationInvalidApiKey() {
        assertThrows(AuthenticationException.class, () -> {
            LegacyBaaSClient.builder()
                .apiKey("invalid_key")
                .build();
        });
    }

    @Test
    @DisplayName("Should throw IllegalArgumentException for null API key")
    void testClientInitializationNullApiKey() {
        assertThrows(IllegalArgumentException.class, () -> {
            LegacyBaaSClient.builder()
                .apiKey(null)
                .build();
        });
    }

    @Test
    @DisplayName("Should throw IllegalArgumentException for empty API key")
    void testClientInitializationEmptyApiKey() {
        assertThrows(IllegalArgumentException.class, () -> {
            LegacyBaaSClient.builder()
                .apiKey("")
                .build();
        });
    }

    @Test
    @DisplayName("Should configure client with custom settings")
    void testClientWithCustomConfig() {
        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .environment(Environment.SANDBOX)
            .timeout(Duration.ofSeconds(60))
            .maxRetries(5)
            .retryDelay(Duration.ofSeconds(2))
            .enableLogging(false)
            .build();

        ClientConfig config = client.getConfig();
        assertEquals(Duration.ofSeconds(60), config.getTimeout());
        assertEquals(5, config.getMaxRetries());
        assertEquals(Duration.ofSeconds(2), config.getRetryDelay());
        assertFalse(config.isEnableLogging());
        
        client.close();
    }

    @Test
    @DisplayName("Should successfully call health endpoint")
    void testHealthEndpointSuccess() throws ExecutionException, InterruptedException {
        String healthResponse = """
            {
                "status": "healthy",
                "version": "1.0.0",
                "timestamp": "2025-07-03T14:30:00Z",
                "components": {
                    "database": "healthy",
                    "swift": "healthy",
                    "blockchain": "healthy"
                }
            }
            """;

        mockWebServer.enqueue(new MockResponse()
            .setBody(healthResponse)
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        CompletableFuture<HealthStatus> healthFuture = client.health();
        HealthStatus health = healthFuture.get();

        assertNotNull(health);
        assertEquals("healthy", health.getStatus());
        assertEquals("1.0.0", health.getVersion());
        
        client.close();
    }

    @Test
    @DisplayName("Should successfully call ping endpoint")
    void testPingEndpointSuccess() throws ExecutionException, InterruptedException {
        String pingResponse = """
            {
                "message": "pong",
                "timestamp": "2025-07-03T14:30:00Z",
                "latency": 45
            }
            """;

        mockWebServer.enqueue(new MockResponse()
            .setBody(pingResponse)
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        CompletableFuture<Map<String, Object>> pingFuture = client.ping();
        Map<String, Object> result = pingFuture.get();

        assertNotNull(result);
        assertEquals("pong", result.get("message"));
        
        client.close();
    }

    @Test
    @DisplayName("Should successfully get account information")
    void testGetAccountEndpoint() throws ExecutionException, InterruptedException {
        String accountResponse = """
            {
                "id": "ACC_123456789",
                "name": "Test Account",
                "plan": "enterprise",
                "status": "active",
                "features": ["swift_processing", "blockchain_routing"]
            }
            """;

        mockWebServer.enqueue(new MockResponse()
            .setBody(accountResponse)
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        CompletableFuture<Map<String, Object>> accountFuture = client.getAccount();
        Map<String, Object> account = accountFuture.get();

        assertNotNull(account);
        assertEquals("ACC_123456789", account.get("id"));
        assertEquals("Test Account", account.get("name"));
        assertEquals("enterprise", account.get("plan"));
        
        client.close();
    }

    @Test
    @DisplayName("Should successfully get usage statistics")
    void testGetUsageEndpoint() throws ExecutionException, InterruptedException {
        String usageResponse = """
            {
                "period": "day",
                "requests": 1250,
                "quota": 10000,
                "remaining": 8750
            }
            """;

        mockWebServer.enqueue(new MockResponse()
            .setBody(usageResponse)
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        CompletableFuture<Map<String, Object>> usageFuture = client.getUsage();
        Map<String, Object> usage = usageFuture.get();

        assertNotNull(usage);
        assertEquals("day", usage.get("period"));
        assertEquals(1250, ((Number) usage.get("requests")).intValue());
        assertEquals(10000, ((Number) usage.get("quota")).intValue());
        
        client.close();
    }

    @Test
    @DisplayName("Should handle authentication errors")
    void testAuthenticationErrorHandling() {
        mockWebServer.enqueue(new MockResponse()
            .setResponseCode(401)
            .setBody("{\"error\": \"Unauthorized\"}")
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        CompletableFuture<Map<String, Object>> pingFuture = client.ping();
        
        assertThrows(ExecutionException.class, () -> {
            pingFuture.get();
        });
        
        client.close();
    }

    @Test
    @DisplayName("Should handle rate limit errors")
    void testRateLimitErrorHandling() {
        mockWebServer.enqueue(new MockResponse()
            .setResponseCode(429)
            .setHeader("Retry-After", "60")
            .setBody("{\"error\": \"Too Many Requests\"}")
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .maxRetries(0)  // Disable retries for testing
            .build();

        CompletableFuture<Map<String, Object>> pingFuture = client.ping();
        
        assertThrows(ExecutionException.class, () -> {
            pingFuture.get();
        });
        
        client.close();
    }

    @Test
    @DisplayName("Should handle service unavailable errors")
    void testServiceUnavailableErrorHandling() {
        mockWebServer.enqueue(new MockResponse()
            .setResponseCode(503)
            .setBody("{\"error\": \"Service Unavailable\"}")
            .setHeader("Content-Type", "application/json"));

        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .maxRetries(0)  // Disable retries for testing
            .build();

        CompletableFuture<HealthStatus> healthFuture = client.health();
        
        assertThrows(ExecutionException.class, () -> {
            healthFuture.get();
        });
        
        client.close();
    }

    @Test
    @DisplayName("Should configure different environments correctly")
    void testEnvironmentConfiguration() {
        // Production environment
        LegacyBaaSClient prodClient = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .environment(Environment.PRODUCTION)
            .build();
        assertTrue(prodClient.getConfig().getBaseUrl().contains("api.legacybaas.com"));
        prodClient.close();

        // Staging environment
        LegacyBaaSClient stagingClient = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .environment(Environment.STAGING)
            .build();
        assertTrue(stagingClient.getConfig().getBaseUrl().contains("staging-api.legacybaas.com"));
        stagingClient.close();

        // Sandbox environment
        LegacyBaaSClient sandboxClient = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .environment(Environment.SANDBOX)
            .build();
        assertTrue(sandboxClient.getConfig().getBaseUrl().contains("sandbox-api.legacybaas.com"));
        sandboxClient.close();
    }

    @Test
    @DisplayName("Should properly close client resources")
    void testClientResourceCleanup() {
        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .baseUrl(baseUrl)
            .build();

        assertDoesNotThrow(() -> {
            client.close();
        });
    }

    @Test
    @DisplayName("Should configure retry mechanism properly")
    void testRetryConfiguration() {
        LegacyBaaSClient client = LegacyBaaSClient.builder()
            .apiKey(validApiKey)
            .maxRetries(5)
            .retryDelay(Duration.ofSeconds(2))
            .build();

        assertEquals(5, client.getConfig().getMaxRetries());
        assertEquals(Duration.ofSeconds(2), client.getConfig().getRetryDelay());
        
        client.close();
    }
}