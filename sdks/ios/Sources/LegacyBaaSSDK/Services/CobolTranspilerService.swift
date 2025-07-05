import Foundation
import Combine

/// COBOL Transpiler Service for iOS SDK
/// Handles all COBOL transpilation operations
@available(iOS 13.0, *)
public class CobolTranspilerService: ObservableObject {
    
    // MARK: - Types
    
    public enum TargetLanguage: String, CaseIterable {
        case solidity = "solidity"
        case corda = "corda"
        case algorand = "algorand"
    }
    
    public enum BankingSystem: String, CaseIterable {
        case fisSystematics = "fis-systematics"
        case fiservDNA = "fiserv-dna"
        case tcsBancs = "tcs-bancs"
        case temenosTransact = "temenos-transact"
    }
    
    public enum BlockchainNetwork: String, CaseIterable {
        case ethereum = "ethereum"
        case corda = "corda"
        case algorand = "algorand"
        case xrp = "xrp"
    }
    
    public enum RiskLevel: String, CaseIterable {
        case low = "LOW"
        case medium = "MEDIUM"
        case high = "HIGH"
        case critical = "CRITICAL"
    }
    
    // MARK: - Models
    
    public struct CobolTranspileRequest {
        public let sourceCode: String
        public let targetLanguage: TargetLanguage
        public let bankingSystem: BankingSystem
        public let blockchainNetwork: BlockchainNetwork
        public let options: TranspileOptions?
        public let compliance: ComplianceConfig?
        
        public init(
            sourceCode: String,
            targetLanguage: TargetLanguage,
            bankingSystem: BankingSystem,
            blockchainNetwork: BlockchainNetwork,
            options: TranspileOptions? = nil,
            compliance: ComplianceConfig? = nil
        ) {
            self.sourceCode = sourceCode
            self.targetLanguage = targetLanguage
            self.bankingSystem = bankingSystem
            self.blockchainNetwork = blockchainNetwork
            self.options = options
            self.compliance = compliance
        }
    }
    
    public struct TranspileOptions {
        public let optimizeForGas: Bool
        public let includeComments: Bool
        public let generateInterface: Bool
        public let strictTypeChecking: Bool
        public let generateValidators: Bool
        public let includeAuditTrail: Bool
        public let autoOptimize: Bool
        public let deployImmediately: Bool
        public let monitorPerformance: Bool
        public let asyncProcessing: Bool
        
        public init(
            optimizeForGas: Bool = true,
            includeComments: Bool = true,
            generateInterface: Bool = true,
            strictTypeChecking: Bool = false,
            generateValidators: Bool = false,
            includeAuditTrail: Bool = false,
            autoOptimize: Bool = false,
            deployImmediately: Bool = false,
            monitorPerformance: Bool = false,
            asyncProcessing: Bool = false
        ) {
            self.optimizeForGas = optimizeForGas
            self.includeComments = includeComments
            self.generateInterface = generateInterface
            self.strictTypeChecking = strictTypeChecking
            self.generateValidators = generateValidators
            self.includeAuditTrail = includeAuditTrail
            self.autoOptimize = autoOptimize
            self.deployImmediately = deployImmediately
            self.monitorPerformance = monitorPerformance
            self.asyncProcessing = asyncProcessing
        }
    }
    
    public struct ComplianceConfig {
        public let configId: String?
        public let riskThreshold: RiskLevel
        public let requireApproval: Bool
        public let generateReport: Bool
        
        public init(
            configId: String? = nil,
            riskThreshold: RiskLevel = .medium,
            requireApproval: Bool = false,
            generateReport: Bool = false
        ) {
            self.configId = configId
            self.riskThreshold = riskThreshold
            self.requireApproval = requireApproval
            self.generateReport = generateReport
        }
    }
    
    public struct CobolTranspileResult {
        public let success: Bool
        public let transactionId: String
        public let jobId: String?
        public let contractAddress: String?
        public let transactionHash: String?
        public let sourceCode: String?
        public let gasEstimate: Int?
        public let deploymentCost: Double?
        public let performance: PerformanceMetrics?
        public let compliance: ComplianceResult?
        public let error: String?
    }
    
    public struct PerformanceMetrics {
        public let compilationTime: Double
        public let deploymentTime: Double
        public let gasUsage: Int
        public let executionTime: Double
        public let throughput: Double
    }
    
    public struct ComplianceResult {
        public let riskScore: Int
        public let screeningResults: [String: Any]
        public let auditTrail: [String: Any]
    }
    
    public struct CobolTemplate {
        public let id: String
        public let name: String
        public let description: String
        public let bankingSystem: BankingSystem
        public let category: String
        public let variables: [TemplateVariable]
        public let version: String
        public let created: Date
        public let updated: Date
    }
    
    public struct TemplateVariable {
        public let name: String
        public let type: String
        public let required: Bool
        public let description: String
    }
    
    public struct CobolStatusUpdate {
        public let jobId: String
        public let progress: Int
        public let stage: String
        public let message: String
        public let timestamp: Date
    }
    
    public struct NetworkSelection {
        public let network: BlockchainNetwork
        public let language: TargetLanguage
        public let reasons: [String]
        public let estimatedCost: Double
        public let estimatedTime: TimeInterval
    }
    
    public struct CobolAnalytics {
        public let totalTranspilations: Int
        public let successRate: Double
        public let averageTime: TimeInterval
        public let byBankingSystem: [String: Int]
        public let byNetwork: [String: Int]
        public let errorTypes: [String: Int]
        public let performanceMetrics: PerformanceMetrics
    }
    
    public struct CobolQuota {
        public let current: Int
        public let limit: Int
        public let remaining: Int
        public let resetDate: Date
        public let overage: Int
        public let overageCost: Double
    }
    
    public struct ValidationResult {
        public let valid: Bool
        public let errors: [ValidationError]
        public let warnings: [String]
        public let suggestions: [String]
    }
    
    public struct ValidationError {
        public let line: Int
        public let column: Int
        public let message: String
        public let severity: ErrorSeverity
    }
    
    public enum ErrorSeverity: String {
        case error = "error"
        case warning = "warning"
        case info = "info"
    }
    
    // MARK: - Properties
    
    private let httpClient: HTTPClient
    private let baseURL: URL
    private var cancellables = Set<AnyCancellable>()
    
    @Published public var isLoading = false
    @Published public var lastError: Error?
    
    // MARK: - Initialization
    
    init(httpClient: HTTPClient, baseURL: URL) {
        self.httpClient = httpClient
        self.baseURL = baseURL.appendingPathComponent("banking")
    }
    
    // MARK: - Public Methods
    
    /// Transpile COBOL code to target blockchain language
    public func transpile(_ request: CobolTranspileRequest) -> AnyPublisher<CobolTranspileResult, Error> {
        isLoading = true
        
        let endpoint = baseURL.appendingPathComponent("transpile")
        
        let requestBody: [String: Any] = [
            "sourceCode": request.sourceCode,
            "targetLanguage": request.targetLanguage.rawValue,
            "bankingSystem": request.bankingSystem.rawValue,
            "blockchainNetwork": request.blockchainNetwork.rawValue,
            "options": request.options?.toDictionary() ?? [:],
            "compliance": request.compliance?.toDictionary() ?? [:]
        ]
        
        return httpClient.post(endpoint: endpoint, body: requestBody)
            .map { (response: APIResponse<CobolTranspileResult>) in
                self.isLoading = false
                return response.data
            }
            .catch { error -> AnyPublisher<CobolTranspileResult, Error> in
                self.isLoading = false
                self.lastError = error
                return Fail(error: error).eraseToAnyPublisher()
            }
            .eraseToAnyPublisher()
    }
    
    /// Process multiple COBOL files in batch
    public func batchTranspile(files: [BatchFile], bankingSystem: BankingSystem, options: [String: Any]? = nil) -> AnyPublisher<[CobolTranspileResult], Error> {
        isLoading = true
        
        let endpoint = baseURL.appendingPathComponent("transpile/batch")
        
        let requestBody: [String: Any] = [
            "files": files.map { $0.toDictionary() },
            "bankingSystem": bankingSystem.rawValue,
            "options": options ?? [:]
        ]
        
        return httpClient.post(endpoint: endpoint, body: requestBody)
            .map { (response: APIResponse<[CobolTranspileResult]>) in
                self.isLoading = false
                return response.data
            }
            .catch { error -> AnyPublisher<[CobolTranspileResult], Error> in
                self.isLoading = false
                self.lastError = error
                return Fail(error: error).eraseToAnyPublisher()
            }
            .eraseToAnyPublisher()
    }
    
    /// Validate COBOL code without transpilation
    public func validate(sourceCode: String, bankingSystem: BankingSystem) -> AnyPublisher<ValidationResult, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/validate")
        
        let requestBody: [String: Any] = [
            "sourceCode": sourceCode,
            "bankingSystem": bankingSystem.rawValue
        ]
        
        return httpClient.post(endpoint: endpoint, body: requestBody)
            .map { (response: APIResponse<ValidationResult>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Get transpilation job status
    public func getStatus(jobId: String) -> AnyPublisher<CobolStatusUpdate, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/status/\(jobId)")
        
        return httpClient.get(endpoint: endpoint)
            .map { (response: APIResponse<CobolStatusUpdate>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Monitor transpilation status with real-time updates
    public func monitorStatus(jobId: String) -> AnyPublisher<CobolStatusUpdate, Error> {
        Timer.publish(every: 2.0, on: .main, in: .common)
            .autoconnect()
            .flatMap { _ in
                self.getStatus(jobId: jobId)
            }
            .takeWhile { status in
                status.progress < 100
            }
            .eraseToAnyPublisher()
    }
    
    /// Get available COBOL templates
    public func getTemplates(bankingSystem: BankingSystem? = nil, category: String? = nil) -> AnyPublisher<[CobolTemplate], Error> {
        var endpoint = baseURL.appendingPathComponent("transpile/templates")
        
        var queryItems: [URLQueryItem] = []
        if let bankingSystem = bankingSystem {
            queryItems.append(URLQueryItem(name: "bankingSystem", value: bankingSystem.rawValue))
        }
        if let category = category {
            queryItems.append(URLQueryItem(name: "category", value: category))
        }
        
        if !queryItems.isEmpty {
            var components = URLComponents(url: endpoint, resolvingAgainstBaseURL: false)!
            components.queryItems = queryItems
            endpoint = components.url!
        }
        
        return httpClient.get(endpoint: endpoint)
            .map { (response: APIResponse<[CobolTemplate]>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Get specific template by ID
    public func getTemplate(templateId: String) -> AnyPublisher<CobolTemplate, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/templates/\(templateId)")
        
        return httpClient.get(endpoint: endpoint)
            .map { (response: APIResponse<CobolTemplate>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Transpile using a template
    public func transpileWithTemplate(templateId: String, variables: [String: Any], targetLanguage: TargetLanguage, blockchainNetwork: BlockchainNetwork, options: [String: Any]? = nil) -> AnyPublisher<CobolTranspileResult, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/templates/\(templateId)")
        
        let requestBody: [String: Any] = [
            "variables": variables,
            "targetLanguage": targetLanguage.rawValue,
            "blockchainNetwork": blockchainNetwork.rawValue,
            "options": options ?? [:]
        ]
        
        return httpClient.post(endpoint: endpoint, body: requestBody)
            .map { (response: APIResponse<CobolTranspileResult>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Select optimal blockchain network
    public func selectOptimalNetwork(criteria: NetworkSelectionCriteria) -> AnyPublisher<NetworkSelection, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/network-selection")
        
        return httpClient.post(endpoint: endpoint, body: criteria.toDictionary())
            .map { (response: APIResponse<NetworkSelection>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Get transpilation analytics
    public func getAnalytics(period: String = "24h") -> AnyPublisher<CobolAnalytics, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/analytics")
        
        var components = URLComponents(url: endpoint, resolvingAgainstBaseURL: false)!
        components.queryItems = [URLQueryItem(name: "period", value: period)]
        
        return httpClient.get(endpoint: components.url!)
            .map { (response: APIResponse<CobolAnalytics>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Get quota information
    public func getQuota() -> AnyPublisher<CobolQuota, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/quota")
        
        return httpClient.get(endpoint: endpoint)
            .map { (response: APIResponse<CobolQuota>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
    
    /// Estimate transpilation cost
    public func estimateCost(request: CostEstimateRequest) -> AnyPublisher<CostEstimate, Error> {
        let endpoint = baseURL.appendingPathComponent("transpile/cost-estimate")
        
        return httpClient.post(endpoint: endpoint, body: request.toDictionary())
            .map { (response: APIResponse<CostEstimate>) in
                response.data
            }
            .eraseToAnyPublisher()
    }
}

// MARK: - Supporting Types

public struct BatchFile {
    public let name: String
    public let content: String
    public let targetLanguage: CobolTranspilerService.TargetLanguage
    public let blockchainNetwork: CobolTranspilerService.BlockchainNetwork
    public let options: [String: Any]?
    
    public init(name: String, content: String, targetLanguage: CobolTranspilerService.TargetLanguage, blockchainNetwork: CobolTranspilerService.BlockchainNetwork, options: [String: Any]? = nil) {
        self.name = name
        self.content = content
        self.targetLanguage = targetLanguage
        self.blockchainNetwork = blockchainNetwork
        self.options = options
    }
    
    func toDictionary() -> [String: Any] {
        return [
            "name": name,
            "content": content,
            "targetLanguage": targetLanguage.rawValue,
            "blockchainNetwork": blockchainNetwork.rawValue,
            "options": options ?? [:]
        ]
    }
}

public struct NetworkSelectionCriteria {
    public let contractComplexity: String
    public let transactionVolume: String
    public let latencyRequirement: String
    public let costSensitivity: String
    
    public init(contractComplexity: String, transactionVolume: String, latencyRequirement: String, costSensitivity: String) {
        self.contractComplexity = contractComplexity
        self.transactionVolume = transactionVolume
        self.latencyRequirement = latencyRequirement
        self.costSensitivity = costSensitivity
    }
    
    func toDictionary() -> [String: Any] {
        return [
            "contractComplexity": contractComplexity,
            "transactionVolume": transactionVolume,
            "latencyRequirement": latencyRequirement,
            "costSensitivity": costSensitivity
        ]
    }
}

public struct CostEstimateRequest {
    public let sourceCodeLength: Int
    public let targetLanguage: String
    public let blockchainNetwork: String
    public let complexity: String?
    
    public init(sourceCodeLength: Int, targetLanguage: String, blockchainNetwork: String, complexity: String? = nil) {
        self.sourceCodeLength = sourceCodeLength
        self.targetLanguage = targetLanguage
        self.blockchainNetwork = blockchainNetwork
        self.complexity = complexity
    }
    
    func toDictionary() -> [String: Any] {
        var dict: [String: Any] = [
            "sourceCodeLength": sourceCodeLength,
            "targetLanguage": targetLanguage,
            "blockchainNetwork": blockchainNetwork
        ]
        if let complexity = complexity {
            dict["complexity"] = complexity
        }
        return dict
    }
}

public struct CostEstimate {
    public let estimatedCost: Double
    public let breakdown: CostBreakdown
    public let currency: String
}

public struct CostBreakdown {
    public let baseCost: Double
    public let complexityMultiplier: Double
    public let networkFee: Double
    public let deploymentCost: Double
}

// MARK: - Extensions

extension CobolTranspilerService.TranspileOptions {
    func toDictionary() -> [String: Any] {
        return [
            "optimizeForGas": optimizeForGas,
            "includeComments": includeComments,
            "generateInterface": generateInterface,
            "strictTypeChecking": strictTypeChecking,
            "generateValidators": generateValidators,
            "includeAuditTrail": includeAuditTrail,
            "autoOptimize": autoOptimize,
            "deployImmediately": deployImmediately,
            "monitorPerformance": monitorPerformance,
            "async": asyncProcessing
        ]
    }
}

extension CobolTranspilerService.ComplianceConfig {
    func toDictionary() -> [String: Any] {
        var dict: [String: Any] = [
            "riskThreshold": riskThreshold.rawValue,
            "requireApproval": requireApproval,
            "generateReport": generateReport
        ]
        if let configId = configId {
            dict["configId"] = configId
        }
        return dict
    }
}

// MARK: - Codable Extensions

extension CobolTranspilerService.CobolTranspileResult: Codable {
    enum CodingKeys: String, CodingKey {
        case success, transactionId, jobId, contractAddress, transactionHash, sourceCode, gasEstimate, deploymentCost, performance, compliance, error
    }
}

extension CobolTranspilerService.PerformanceMetrics: Codable {}
extension CobolTranspilerService.ComplianceResult: Codable {}
extension CobolTranspilerService.CobolTemplate: Codable {}
extension CobolTranspilerService.TemplateVariable: Codable {}
extension CobolTranspilerService.CobolStatusUpdate: Codable {}
extension CobolTranspilerService.NetworkSelection: Codable {}
extension CobolTranspilerService.CobolAnalytics: Codable {}
extension CobolTranspilerService.CobolQuota: Codable {}
extension CobolTranspilerService.ValidationResult: Codable {}
extension CobolTranspilerService.ValidationError: Codable {}
extension CostEstimate: Codable {}
extension CostBreakdown: Codable {}

extension CobolTranspilerService.TargetLanguage: Codable {}
extension CobolTranspilerService.BankingSystem: Codable {}
extension CobolTranspilerService.BlockchainNetwork: Codable {}
extension CobolTranspilerService.RiskLevel: Codable {}
extension CobolTranspilerService.ErrorSeverity: Codable {}