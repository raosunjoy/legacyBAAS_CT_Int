import Foundation
import Network
import Security

/// Main Legacy B2BaaS Platform Client for iOS
///
/// Provides access to all platform services including SWIFT processing,
/// blockchain routing, BaNCS integration, and analytics.
///
/// Example usage:
/// ```swift
/// let client = LegacyBaaSClient(apiKey: "your-api-key")
///
/// // Check platform health
/// client.getHealth { result in
///     switch result {
///     case .success(let health):
///         print("Platform status: \(health.status)")
///     case .failure(let error):
///         print("Error: \(error)")
///     }
/// }
///
/// // Process SWIFT MT103 message
/// let mt103 = MT103Message(
///     transactionReference: "TXN123456",
///     currency: "USD",
///     amount: 10000,
///     orderingCustomer: Customer(account: "123456789", name: "John Doe"),
///     beneficiaryCustomer: Customer(account: "987654321", name: "Jane Smith")
/// )
///
/// client.swift.processMT103(mt103) { result in
///     switch result {
///     case .success(let response):
///         print("Transaction ID: \(response.transactionId)")
///     case .failure(let error):
///         print("Error: \(error)")
///     }
/// }
/// ```
@objc public class LegacyBaaSClient: NSObject {
    
    // MARK: - Properties
    
    /// SDK version
    public static let sdkVersion = "1.0.0"
    
    /// Configuration options
    public let configuration: LegacyBaaSConfiguration
    
    /// SWIFT processing service
    public lazy var swift: SwiftService = SwiftService(client: self)
    
    /// Blockchain routing service
    public lazy var blockchain: BlockchainService = BlockchainService(client: self)
    
    /// BaNCS integration service
    public lazy var bancs: BancsService = BancsService(client: self)
    
    /// Analytics service
    public lazy var analytics: AnalyticsService = AnalyticsService(client: self)
    
    /// Webhook handler service
    public lazy var webhooks: WebhookService = WebhookService(client: self)
    
    private let session: URLSession
    private let apiKeyManager: APIKeyManager
    private let monitor: NWPathMonitor
    private let monitorQueue = DispatchQueue(label: "network-monitor")
    
    // MARK: - Initialization
    
    /// Initialize Legacy B2BaaS client
    /// - Parameters:
    ///   - apiKey: Your Legacy B2BaaS API key
    ///   - configuration: Optional configuration (uses default if not provided)
    public init(apiKey: String, configuration: LegacyBaaSConfiguration = .default) {
        guard APIKeyValidator.isValid(apiKey) else {
            fatalError("Invalid API key format")
        }
        
        self.configuration = configuration
        self.apiKeyManager = APIKeyManager(apiKey: apiKey)
        
        // Configure URL session
        let config = URLSessionConfiguration.default
        config.timeoutIntervalForRequest = configuration.timeout
        config.timeoutIntervalForResource = configuration.timeout * 2
        config.requestCachePolicy = .reloadIgnoringLocalCacheData
        
        // Set default headers
        config.httpAdditionalHeaders = [
            "Accept": "application/json",
            "Content-Type": "application/json",
            "User-Agent": "LegacyBaaS-iOS-SDK/\(Self.sdkVersion)",
            "X-SDK-Version": Self.sdkVersion,
            "X-SDK-Language": "swift"
        ]
        
        self.session = URLSession(configuration: config)
        self.monitor = NWPathMonitor()
        
        super.init()
        
        // Start network monitoring
        monitor.start(queue: monitorQueue)
        
        if configuration.enableLogging {
            print("LegacyBaaS iOS SDK initialized for environment: \(configuration.environment)")
        }
    }
    
    deinit {
        monitor.cancel()
        session.invalidateAndCancel()
    }
    
    // MARK: - Public API
    
    /// Check platform health status
    /// - Parameter completion: Completion handler with health status result
    public func getHealth(completion: @escaping (Result<HealthStatus, LegacyBaaSError>) -> Void) {
        performRequest(endpoint: "/health", method: .GET, completion: completion)
    }
    
    /// Test API connectivity
    /// - Parameter completion: Completion handler with ping result
    public func ping(completion: @escaping (Result<PingResponse, LegacyBaaSError>) -> Void) {
        performRequest(endpoint: "/ping", method: .GET, completion: completion)
    }
    
    /// Get account information
    /// - Parameter completion: Completion handler with account details
    public func getAccount(completion: @escaping (Result<AccountInfo, LegacyBaaSError>) -> Void) {
        performRequest(endpoint: "/account", method: .GET, completion: completion)
    }
    
    /// Get API usage statistics
    /// - Parameters:
    ///   - period: Time period (day, week, month)
    ///   - completion: Completion handler with usage statistics
    public func getUsage(period: String = "day", completion: @escaping (Result<UsageInfo, LegacyBaaSError>) -> Void) {
        let endpoint = "/usage?period=\(period)"
        performRequest(endpoint: endpoint, method: .GET, completion: completion)
    }
    
    // MARK: - Internal Methods
    
    internal func performRequest<T: Codable>(
        endpoint: String,
        method: HTTPMethod,
        body: Codable? = nil,
        completion: @escaping (Result<T, LegacyBaaSError>) -> Void
    ) {
        guard let url = buildURL(endpoint: endpoint) else {
            completion(.failure(.invalidURL))
            return
        }
        
        var request = URLRequest(url: url)
        request.httpMethod = method.rawValue
        
        // Add authorization header
        do {
            let apiKey = try apiKeyManager.getAPIKey()
            request.setValue("Bearer \(apiKey)", forHTTPHeaderField: "Authorization")
        } catch {
            completion(.failure(.authenticationError("Failed to retrieve API key")))
            return
        }
        
        // Add body if provided
        if let body = body {
            do {
                request.httpBody = try JSONEncoder().encode(body)
            } catch {
                completion(.failure(.encodingError(error)))
                return
            }
        }
        
        // Perform request with retry logic
        performRequestWithRetry(request: request, retryCount: 0, completion: completion)
    }
    
    private func performRequestWithRetry<T: Codable>(
        request: URLRequest,
        retryCount: Int,
        completion: @escaping (Result<T, LegacyBaaSError>) -> Void
    ) {
        session.dataTask(with: request) { [weak self] data, response, error in
            guard let self = self else { return }
            
            if let error = error {
                if retryCount < self.configuration.maxRetries && self.shouldRetry(error: error) {
                    DispatchQueue.global().asyncAfter(deadline: .now() + self.configuration.retryDelay) {
                        self.performRequestWithRetry(request: request, retryCount: retryCount + 1, completion: completion)
                    }
                    return
                }
                completion(.failure(.networkError(error)))
                return
            }
            
            guard let httpResponse = response as? HTTPURLResponse else {
                completion(.failure(.invalidResponse))
                return
            }
            
            guard let data = data else {
                completion(.failure(.noData))
                return
            }
            
            // Handle HTTP errors
            if let apiError = self.handleHTTPError(statusCode: httpResponse.statusCode, data: data) {
                if self.shouldRetry(statusCode: httpResponse.statusCode) && retryCount < self.configuration.maxRetries {
                    DispatchQueue.global().asyncAfter(deadline: .now() + self.configuration.retryDelay) {
                        self.performRequestWithRetry(request: request, retryCount: retryCount + 1, completion: completion)
                    }
                    return
                }
                completion(.failure(apiError))
                return
            }
            
            // Decode successful response
            do {
                let decoder = JSONDecoder()
                decoder.dateDecodingStrategy = .iso8601
                let result = try decoder.decode(T.self, from: data)
                completion(.success(result))
            } catch {
                completion(.failure(.decodingError(error)))
            }
        }.resume()
    }
    
    private func buildURL(endpoint: String) -> URL? {
        let baseURL = configuration.baseURL ?? configuration.environment.baseURL
        let fullURL = "\(baseURL)/api/v1\(endpoint)"
        return URL(string: fullURL)
    }
    
    private func shouldRetry(error: Error) -> Bool {
        if let urlError = error as? URLError {
            switch urlError.code {
            case .timedOut, .networkConnectionLost, .notConnectedToInternet:
                return true
            default:
                return false
            }
        }
        return false
    }
    
    private func shouldRetry(statusCode: Int) -> Bool {
        return [408, 429, 500, 502, 503, 504].contains(statusCode)
    }
    
    private func handleHTTPError(statusCode: Int, data: Data) -> LegacyBaaSError? {
        switch statusCode {
        case 200...299:
            return nil
        case 400:
            return .validationError(parseErrorMessage(data))
        case 401:
            return .authenticationError(parseErrorMessage(data))
        case 429:
            return .rateLimitError(parseErrorMessage(data))
        case 503:
            return .serviceUnavailableError(parseErrorMessage(data))
        default:
            return .httpError(statusCode, parseErrorMessage(data))
        }
    }
    
    private func parseErrorMessage(_ data: Data) -> String {
        if let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
           let message = json["message"] as? String {
            return message
        }
        return "Unknown error"
    }
}

// MARK: - HTTP Method

internal enum HTTPMethod: String {
    case GET = "GET"
    case POST = "POST"
    case PUT = "PUT"
    case DELETE = "DELETE"
}