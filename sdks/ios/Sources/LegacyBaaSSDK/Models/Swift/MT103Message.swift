import Foundation

/// MT103 Single Customer Credit Transfer message
public struct MT103Message: Codable {
    /// Transaction reference number
    public let transactionReference: String
    
    /// Value date
    public let valueDate: Date?
    
    /// Currency code (ISO 4217)
    public let currency: String
    
    /// Transaction amount
    public let amount: Decimal
    
    /// Ordering customer information
    public let orderingCustomer: Customer
    
    /// Beneficiary customer information
    public let beneficiaryCustomer: Customer
    
    /// Ordering institution
    public let orderingInstitution: Institution?
    
    /// Beneficiary institution
    public let beneficiaryInstitution: Institution?
    
    /// Intermediary institution
    public let intermediaryInstitution: Institution?
    
    /// Remittance information
    public let remittanceInformation: String?
    
    /// Regulatory reporting information
    public let regulatoryReporting: String?
    
    /// Charges indication
    public let charges: ChargesIndication?
    
    public init(
        transactionReference: String,
        valueDate: Date? = nil,
        currency: String,
        amount: Decimal,
        orderingCustomer: Customer,
        beneficiaryCustomer: Customer,
        orderingInstitution: Institution? = nil,
        beneficiaryInstitution: Institution? = nil,
        intermediaryInstitution: Institution? = nil,
        remittanceInformation: String? = nil,
        regulatoryReporting: String? = nil,
        charges: ChargesIndication? = nil
    ) {
        self.transactionReference = transactionReference
        self.valueDate = valueDate
        self.currency = currency
        self.amount = amount
        self.orderingCustomer = orderingCustomer
        self.beneficiaryCustomer = beneficiaryCustomer
        self.orderingInstitution = orderingInstitution
        self.beneficiaryInstitution = beneficiaryInstitution
        self.intermediaryInstitution = intermediaryInstitution
        self.remittanceInformation = remittanceInformation
        self.regulatoryReporting = regulatoryReporting
        self.charges = charges
    }
}

/// Customer information for SWIFT messages
public struct Customer: Codable {
    /// Account number
    public let account: String
    
    /// Customer name
    public let name: String
    
    /// Customer address
    public let address: String?
    
    /// Country code
    public let country: String?
    
    /// Additional identification
    public let identification: String?
    
    public init(
        account: String,
        name: String,
        address: String? = nil,
        country: String? = nil,
        identification: String? = nil
    ) {
        self.account = account
        self.name = name
        self.address = address
        self.country = country
        self.identification = identification
    }
}

/// Financial institution information
public struct Institution: Codable {
    /// BIC (Bank Identifier Code)
    public let bic: String?
    
    /// Institution name
    public let name: String?
    
    /// Institution address
    public let address: String?
    
    /// Account number at the institution
    public let account: String?
    
    public init(
        bic: String? = nil,
        name: String? = nil,
        address: String? = nil,
        account: String? = nil
    ) {
        self.bic = bic
        self.name = name
        self.address = address
        self.account = account
    }
}

/// Charges indication for SWIFT messages
public enum ChargesIndication: String, Codable, CaseIterable {
    /// All charges for account of ordering customer
    case our = "OUR"
    
    /// All charges for account of beneficiary customer
    case ben = "BEN"
    
    /// Charges shared between ordering and beneficiary
    case sha = "SHA"
}

/// SWIFT message processing result
public struct SwiftProcessingResult: Codable {
    /// Transaction ID
    public let transactionId: String
    
    /// SWIFT reference
    public let swiftReference: String
    
    /// Processing status
    public let status: String
    
    /// Processing timestamp
    public let processingTime: Date
    
    /// Processing fees
    public let fees: Fee?
    
    /// Estimated settlement time
    public let estimatedSettlement: Date?
}

/// Fee information
public struct Fee: Codable {
    /// Fee amount
    public let amount: Decimal
    
    /// Fee currency
    public let currency: String
    
    /// Fee description
    public let description: String?
}