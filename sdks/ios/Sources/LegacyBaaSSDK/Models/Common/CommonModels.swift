import Foundation

/// Health status response
public struct HealthStatus: Codable {
    /// Overall status
    public let status: String
    
    /// Platform version
    public let version: String
    
    /// Timestamp
    public let timestamp: Date
    
    /// Component health status
    public let components: [String: String]?
    
    /// Whether the platform is healthy
    public var isHealthy: Bool {
        return status.lowercased() == "healthy"
    }
}

/// Ping response
public struct PingResponse: Codable {
    /// Response message
    public let message: String
    
    /// Timestamp
    public let timestamp: Date
    
    /// Latency in milliseconds
    public let latency: Int?
}

/// Account information
public struct AccountInfo: Codable {
    /// Account ID
    public let id: String
    
    /// Account name
    public let name: String
    
    /// Account plan
    public let plan: String
    
    /// Account status
    public let status: String
    
    /// Creation timestamp
    public let createdAt: Date?
    
    /// Available features
    public let features: [String]?
}

/// Usage information
public struct UsageInfo: Codable {
    /// Time period
    public let period: String
    
    /// Number of requests
    public let requests: Int
    
    /// Request quota
    public let quota: Int
    
    /// Remaining requests
    public let remaining: Int?
}

/// Standard API response wrapper
public struct APIResponse<T: Codable>: Codable {
    /// Response data
    public let data: T?
    
    /// Response status
    public let status: String
    
    /// Response message
    public let message: String?
    
    /// Error message
    public let error: String?
    
    /// Whether the response indicates success
    public var isSuccess: Bool {
        return error == nil && status.lowercased() == "success"
    }
}

/// Pagination information
public struct PaginationInfo: Codable {
    /// Current page number
    public let page: Int
    
    /// Items per page
    public let perPage: Int
    
    /// Total number of items
    public let total: Int
    
    /// Total number of pages
    public let totalPages: Int
    
    /// Whether there is a next page
    public var hasNextPage: Bool {
        return page < totalPages
    }
    
    /// Whether there is a previous page
    public var hasPreviousPage: Bool {
        return page > 1
    }
}

/// Paginated response
public struct PagedResponse<T: Codable>: Codable {
    /// Items
    public let items: [T]
    
    /// Pagination information
    public let pagination: PaginationInfo
}