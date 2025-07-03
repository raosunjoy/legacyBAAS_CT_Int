package com.legacybaas.models.common;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;

/**
 * Health status response model
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class HealthStatus {
    
    @JsonProperty("status")
    private String status;
    
    @JsonProperty("version")
    private String version;
    
    @JsonProperty("timestamp")
    private String timestamp;
    
    @JsonProperty("components")
    private Map<String, String> components;
    
    // Default constructor for Jackson
    public HealthStatus() {}
    
    public HealthStatus(String status, String version, String timestamp, Map<String, String> components) {
        this.status = status;
        this.version = version;
        this.timestamp = timestamp;
        this.components = components;
    }
    
    // Getters
    public String getStatus() { return status; }
    public String getVersion() { return version; }
    public String getTimestamp() { return timestamp; }
    public Map<String, String> getComponents() { return components; }
    
    // Setters for Jackson
    public void setStatus(String status) { this.status = status; }
    public void setVersion(String version) { this.version = version; }
    public void setTimestamp(String timestamp) { this.timestamp = timestamp; }
    public void setComponents(Map<String, String> components) { this.components = components; }
    
    @Override
    public String toString() {
        return String.format("HealthStatus{status='%s', version='%s', timestamp='%s'}", 
            status, version, timestamp);
    }
}