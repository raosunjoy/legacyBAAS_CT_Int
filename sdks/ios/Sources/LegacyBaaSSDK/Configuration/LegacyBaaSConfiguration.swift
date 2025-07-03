import Foundation

/// Configuration for Legacy B2BaaS SDK
public struct LegacyBaaSConfiguration {
    
    /// Target environment
    public let environment: Environment
    
    /// Custom base URL (overrides environment default)
    public let baseURL: String?
    
    /// Request timeout in seconds
    public let timeout: TimeInterval
    
    /// Maximum number of retry attempts
    public let maxRetries: Int
    
    /// Delay between retry attempts
    public let retryDelay: TimeInterval
    
    /// Enable SDK logging
    public let enableLogging: Bool
    
    /// Initialize configuration
    /// - Parameters:
    ///   - environment: Target environment
    ///   - baseURL: Custom base URL (optional)
    ///   - timeout: Request timeout in seconds
    ///   - maxRetries: Maximum retry attempts
    ///   - retryDelay: Delay between retries
    ///   - enableLogging: Enable logging
    public init(
        environment: Environment = .production,
        baseURL: String? = nil,
        timeout: TimeInterval = 30.0,
        maxRetries: Int = 3,
        retryDelay: TimeInterval = 1.0,
        enableLogging: Bool = true
    ) {
        self.environment = environment
        self.baseURL = baseURL
        self.timeout = timeout
        self.maxRetries = maxRetries
        self.retryDelay = retryDelay
        self.enableLogging = enableLogging
    }
    
    /// Default configuration
    public static let `default` = LegacyBaaSConfiguration()
}

/// Supported environments
public enum Environment: String, CaseIterable {
    case production = "production"
    case staging = "staging"
    case sandbox = "sandbox"
    
    /// Base URL for the environment
    public var baseURL: String {
        switch self {
        case .production:
            return "https://api.legacybaas.com"
        case .staging:
            return "https://staging-api.legacybaas.com"
        case .sandbox:
            return "https://sandbox-api.legacybaas.com"
        }
    }
    
    /// Display name for the environment
    public var displayName: String {
        switch self {
        case .production:
            return "Production"
        case .staging:
            return "Staging"
        case .sandbox:
            return "Sandbox"
        }
    }
}