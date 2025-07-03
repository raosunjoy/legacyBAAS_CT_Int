package com.legacybaas.models.common;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * Standard API response wrapper
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ApiResponse<T> {
    
    @JsonProperty("data")
    private T data;
    
    @JsonProperty("status")
    private String status;
    
    @JsonProperty("message")
    private String message;
    
    @JsonProperty("error")
    private String error;
    
    // Default constructor for Jackson
    public ApiResponse() {}
    
    public ApiResponse(T data, String status, String message) {
        this.data = data;
        this.status = status;
        this.message = message;
    }
    
    // Getters
    public T getData() { return data; }
    public String getStatus() { return status; }
    public String getMessage() { return message; }
    public String getError() { return error; }
    
    // Setters for Jackson
    public void setData(T data) { this.data = data; }
    public void setStatus(String status) { this.status = status; }
    public void setMessage(String message) { this.message = message; }
    public void setError(String error) { this.error = error; }
    
    public boolean isSuccess() {
        return error == null && "success".equalsIgnoreCase(status);
    }
    
    @Override
    public String toString() {
        return String.format("ApiResponse{status='%s', message='%s'}", status, message);
    }
}