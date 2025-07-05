package com.legacybaas.services;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.legacybaas.exceptions.LegacyBaaSException;
import com.legacybaas.models.cobol.*;
import com.legacybaas.models.common.ApiResponse;
import okhttp3.*;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * COBOL Transpiler Service for Java SDK
 * Handles all COBOL transpilation operations
 */
public class CobolTranspilerService {
    
    private final OkHttpClient httpClient;
    private final String baseUrl;
    private final ObjectMapper objectMapper;
    private final ScheduledExecutorService scheduler;
    
    public CobolTranspilerService(OkHttpClient httpClient, String baseUrl) {
        this.httpClient = httpClient;
        this.baseUrl = baseUrl + "/banking";
        this.objectMapper = new ObjectMapper();
        this.scheduler = Executors.newScheduledThreadPool(4);
    }
    
    /**
     * Transpile COBOL code to target blockchain language
     */
    public CobolTranspileResult transpile(CobolTranspileRequest request) throws LegacyBaaSException {
        try {
            String json = objectMapper.writeValueAsString(request);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile")
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("COBOL transpilation failed: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolTranspileResult> apiResponse = objectMapper.readValue(
                    responseBody, 
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolTranspileResult.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("COBOL transpilation failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * Transpile COBOL code asynchronously
     */
    public CompletableFuture<CobolTranspileResult> transpileAsync(CobolTranspileRequest request) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return transpile(request);
            } catch (LegacyBaaSException e) {
                throw new RuntimeException(e);
            }
        });
    }
    
    /**
     * Process multiple COBOL files in batch
     */
    public List<CobolTranspileResult> batchTranspile(CobolBatchRequest request) throws LegacyBaaSException {
        try {
            String json = objectMapper.writeValueAsString(request);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile/batch")
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Batch COBOL transpilation failed: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<List<CobolTranspileResult>> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(
                        ApiResponse.class,
                        objectMapper.getTypeFactory().constructCollectionType(List.class, CobolTranspileResult.class)
                    )
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Batch COBOL transpilation failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * Validate COBOL code without transpilation
     */
    public CobolValidationResult validate(String sourceCode, String bankingSystem) throws LegacyBaaSException {
        try {
            CobolValidationRequest request = new CobolValidationRequest(sourceCode, bankingSystem);
            String json = objectMapper.writeValueAsString(request);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile/validate")
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("COBOL validation failed: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolValidationResult> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolValidationResult.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("COBOL validation failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * Get transpilation job status
     */
    public CobolStatusUpdate getStatus(String jobId) throws LegacyBaaSException {
        try {
            Request request = new Request.Builder()
                    .url(baseUrl + "/transpile/status/" + jobId)
                    .get()
                    .build();
            
            try (Response response = httpClient.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to get transpilation status: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolStatusUpdate> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolStatusUpdate.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to get transpilation status: " + e.getMessage(), e);
        }
    }
    
    /**
     * Monitor transpilation status with callbacks
     */
    public StatusMonitor monitorStatus(String jobId) {
        return new StatusMonitor(jobId, this, scheduler);
    }
    
    /**
     * Get available COBOL templates
     */
    public List<CobolTemplate> getTemplates(String bankingSystem, String category) throws LegacyBaaSException {
        try {
            HttpUrl.Builder urlBuilder = HttpUrl.parse(baseUrl + "/transpile/templates").newBuilder();
            if (bankingSystem != null) {
                urlBuilder.addQueryParameter("bankingSystem", bankingSystem);
            }
            if (category != null) {
                urlBuilder.addQueryParameter("category", category);
            }
            
            Request request = new Request.Builder()
                    .url(urlBuilder.build())
                    .get()
                    .build();
            
            try (Response response = httpClient.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to get templates: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<List<CobolTemplate>> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(
                        ApiResponse.class,
                        objectMapper.getTypeFactory().constructCollectionType(List.class, CobolTemplate.class)
                    )
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to get templates: " + e.getMessage(), e);
        }
    }
    
    /**
     * Get specific template by ID
     */
    public CobolTemplate getTemplate(String templateId) throws LegacyBaaSException {
        try {
            Request request = new Request.Builder()
                    .url(baseUrl + "/transpile/templates/" + templateId)
                    .get()
                    .build();
            
            try (Response response = httpClient.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to get template: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolTemplate> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolTemplate.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to get template: " + e.getMessage(), e);
        }
    }
    
    /**
     * Transpile using a template
     */
    public CobolTranspileResult transpileWithTemplate(CobolTemplateRequest request) throws LegacyBaaSException {
        try {
            String json = objectMapper.writeValueAsString(request);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile/templates/" + request.getTemplateId())
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Template transpilation failed: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolTranspileResult> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolTranspileResult.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Template transpilation failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * Create custom banking system configuration
     */
    public CobolBankingConfig createBankingConfig(CobolBankingConfigRequest request) throws LegacyBaaSException {
        try {
            String json = objectMapper.writeValueAsString(request);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile/configs")
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to create banking config: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolBankingConfig> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolBankingConfig.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to create banking config: " + e.getMessage(), e);
        }
    }
    
    /**
     * Get banking system configurations
     */
    public List<CobolBankingConfig> getBankingConfigs() throws LegacyBaaSException {
        try {
            Request request = new Request.Builder()
                    .url(baseUrl + "/transpile/configs")
                    .get()
                    .build();
            
            try (Response response = httpClient.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to get banking configs: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<List<CobolBankingConfig>> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(
                        ApiResponse.class,
                        objectMapper.getTypeFactory().constructCollectionType(List.class, CobolBankingConfig.class)
                    )
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to get banking configs: " + e.getMessage(), e);
        }
    }
    
    /**
     * Select optimal blockchain network
     */
    public NetworkSelection selectOptimalNetwork(NetworkSelectionCriteria criteria) throws LegacyBaaSException {
        try {
            String json = objectMapper.writeValueAsString(criteria);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile/network-selection")
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Network selection failed: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<NetworkSelection> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, NetworkSelection.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Network selection failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * Get transpilation analytics
     */
    public CobolAnalytics getAnalytics(String period) throws LegacyBaaSException {
        try {
            HttpUrl url = HttpUrl.parse(baseUrl + "/transpile/analytics")
                    .newBuilder()
                    .addQueryParameter("period", period)
                    .build();
            
            Request request = new Request.Builder()
                    .url(url)
                    .get()
                    .build();
            
            try (Response response = httpClient.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to get analytics: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolAnalytics> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolAnalytics.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to get analytics: " + e.getMessage(), e);
        }
    }
    
    /**
     * Get quota information
     */
    public CobolQuota getQuota() throws LegacyBaaSException {
        try {
            Request request = new Request.Builder()
                    .url(baseUrl + "/transpile/quota")
                    .get()
                    .build();
            
            try (Response response = httpClient.newCall(request).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Failed to get quota information: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CobolQuota> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CobolQuota.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Failed to get quota information: " + e.getMessage(), e);
        }
    }
    
    /**
     * Estimate transpilation cost
     */
    public CostEstimate estimateCost(CostEstimateRequest request) throws LegacyBaaSException {
        try {
            String json = objectMapper.writeValueAsString(request);
            RequestBody body = RequestBody.create(json, MediaType.get("application/json"));
            
            Request httpRequest = new Request.Builder()
                    .url(baseUrl + "/transpile/cost-estimate")
                    .post(body)
                    .build();
            
            try (Response response = httpClient.newCall(httpRequest).execute()) {
                if (!response.isSuccessful()) {
                    throw new LegacyBaaSException("Cost estimation failed: " + response.message());
                }
                
                String responseBody = response.body().string();
                ApiResponse<CostEstimate> apiResponse = objectMapper.readValue(
                    responseBody,
                    objectMapper.getTypeFactory().constructParametricType(ApiResponse.class, CostEstimate.class)
                );
                
                return apiResponse.getData();
            }
            
        } catch (IOException e) {
            throw new LegacyBaaSException("Cost estimation failed: " + e.getMessage(), e);
        }
    }
    
    /**
     * Close the service and cleanup resources
     */
    public void close() {
        scheduler.shutdown();
        try {
            if (!scheduler.awaitTermination(5, TimeUnit.SECONDS)) {
                scheduler.shutdownNow();
            }
        } catch (InterruptedException e) {
            scheduler.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}

/**
 * Status Monitor for tracking transpilation progress
 */
class StatusMonitor {
    private final String jobId;
    private final CobolTranspilerService service;
    private final ScheduledExecutorService scheduler;
    private volatile boolean monitoring = false;
    private StatusCallback callback;
    
    public StatusMonitor(String jobId, CobolTranspilerService service, ScheduledExecutorService scheduler) {
        this.jobId = jobId;
        this.service = service;
        this.scheduler = scheduler;
    }
    
    public void onProgress(StatusCallback callback) {
        this.callback = callback;
    }
    
    public void start() {
        if (monitoring) return;
        
        monitoring = true;
        scheduler.scheduleWithFixedDelay(() -> {
            try {
                CobolStatusUpdate status = service.getStatus(jobId);
                
                if (callback != null) {
                    callback.onProgress(status);
                }
                
                if (status.getProgress() >= 100) {
                    monitoring = false;
                    if (callback != null) {
                        callback.onCompleted(status);
                    }
                }
                
            } catch (Exception e) {
                monitoring = false;
                if (callback != null) {
                    callback.onError(e);
                }
            }
        }, 0, 2, TimeUnit.SECONDS);
    }
    
    public void stop() {
        monitoring = false;
    }
    
    public CompletableFuture<CobolStatusUpdate> waitForCompletion() {
        CompletableFuture<CobolStatusUpdate> future = new CompletableFuture<>();
        
        onProgress(new StatusCallback() {
            @Override
            public void onProgress(CobolStatusUpdate status) {
                // Progress updates
            }
            
            @Override
            public void onCompleted(CobolStatusUpdate status) {
                future.complete(status);
            }
            
            @Override
            public void onError(Exception error) {
                future.completeExceptionally(error);
            }
        });
        
        start();
        return future;
    }
}

/**
 * Callback interface for status monitoring
 */
interface StatusCallback {
    void onProgress(CobolStatusUpdate status);
    void onCompleted(CobolStatusUpdate status);
    void onError(Exception error);
}