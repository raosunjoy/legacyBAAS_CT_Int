using System;
using System.ComponentModel.DataAnnotations;
using System.Text.Json.Serialization;

namespace LegacyBaaS.SDK.Models.Swift
{
    /// <summary>
    /// MT103 Single Customer Credit Transfer message
    /// </summary>
    public class MT103Message
    {
        /// <summary>
        /// Gets or sets the transaction reference number
        /// </summary>
        [Required]
        [JsonPropertyName("transactionReference")]
        public string TransactionReference { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the value date
        /// </summary>
        [JsonPropertyName("valueDate")]
        public DateTime? ValueDate { get; set; }

        /// <summary>
        /// Gets or sets the currency code (ISO 4217)
        /// </summary>
        [Required]
        [StringLength(3, MinimumLength = 3)]
        [JsonPropertyName("currency")]
        public string Currency { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the transaction amount
        /// </summary>
        [Required]
        [Range(0.01, double.MaxValue)]
        [JsonPropertyName("amount")]
        public decimal Amount { get; set; }

        /// <summary>
        /// Gets or sets the ordering customer information
        /// </summary>
        [Required]
        [JsonPropertyName("orderingCustomer")]
        public Customer OrderingCustomer { get; set; } = new();

        /// <summary>
        /// Gets or sets the beneficiary customer information
        /// </summary>
        [Required]
        [JsonPropertyName("beneficiaryCustomer")]
        public Customer BeneficiaryCustomer { get; set; } = new();

        /// <summary>
        /// Gets or sets the ordering institution
        /// </summary>
        [JsonPropertyName("orderingInstitution")]
        public Institution? OrderingInstitution { get; set; }

        /// <summary>
        /// Gets or sets the beneficiary institution
        /// </summary>
        [JsonPropertyName("beneficiaryInstitution")]
        public Institution? BeneficiaryInstitution { get; set; }

        /// <summary>
        /// Gets or sets the intermediary institution
        /// </summary>
        [JsonPropertyName("intermediaryInstitution")]
        public Institution? IntermediaryInstitution { get; set; }

        /// <summary>
        /// Gets or sets the remittance information
        /// </summary>
        [StringLength(4 * 35)] // Max 4 lines of 35 characters
        [JsonPropertyName("remittanceInformation")]
        public string? RemittanceInformation { get; set; }

        /// <summary>
        /// Gets or sets the regulatory reporting information
        /// </summary>
        [JsonPropertyName("regulatoryReporting")]
        public string? RegulatoryReporting { get; set; }

        /// <summary>
        /// Gets or sets the charges indication
        /// </summary>
        [JsonPropertyName("charges")]
        public ChargesIndication? Charges { get; set; }
    }

    /// <summary>
    /// Customer information for SWIFT messages
    /// </summary>
    public class Customer
    {
        /// <summary>
        /// Gets or sets the account number
        /// </summary>
        [Required]
        [JsonPropertyName("account")]
        public string Account { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the customer name
        /// </summary>
        [Required]
        [StringLength(4 * 35)] // Max 4 lines of 35 characters
        [JsonPropertyName("name")]
        public string Name { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the customer address
        /// </summary>
        [StringLength(4 * 35)] // Max 4 lines of 35 characters
        [JsonPropertyName("address")]
        public string? Address { get; set; }

        /// <summary>
        /// Gets or sets the country code
        /// </summary>
        [StringLength(2, MinimumLength = 2)]
        [JsonPropertyName("country")]
        public string? Country { get; set; }

        /// <summary>
        /// Gets or sets additional identification
        /// </summary>
        [JsonPropertyName("identification")]
        public string? Identification { get; set; }
    }

    /// <summary>
    /// Financial institution information
    /// </summary>
    public class Institution
    {
        /// <summary>
        /// Gets or sets the BIC (Bank Identifier Code)
        /// </summary>
        [StringLength(11, MinimumLength = 8)]
        [JsonPropertyName("bic")]
        public string? Bic { get; set; }

        /// <summary>
        /// Gets or sets the institution name
        /// </summary>
        [StringLength(4 * 35)] // Max 4 lines of 35 characters
        [JsonPropertyName("name")]
        public string? Name { get; set; }

        /// <summary>
        /// Gets or sets the institution address
        /// </summary>
        [StringLength(4 * 35)] // Max 4 lines of 35 characters
        [JsonPropertyName("address")]
        public string? Address { get; set; }

        /// <summary>
        /// Gets or sets the account number at the institution
        /// </summary>
        [JsonPropertyName("account")]
        public string? Account { get; set; }
    }

    /// <summary>
    /// Charges indication for SWIFT messages
    /// </summary>
    [JsonConverter(typeof(JsonStringEnumConverter))]
    public enum ChargesIndication
    {
        /// <summary>
        /// All charges for account of ordering customer
        /// </summary>
        OUR,

        /// <summary>
        /// All charges for account of beneficiary customer
        /// </summary>
        BEN,

        /// <summary>
        /// Charges shared between ordering and beneficiary
        /// </summary>
        SHA
    }

    /// <summary>
    /// SWIFT message processing result
    /// </summary>
    public class SwiftProcessingResult
    {
        /// <summary>
        /// Gets or sets the transaction ID
        /// </summary>
        [JsonPropertyName("transactionId")]
        public string TransactionId { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the SWIFT reference
        /// </summary>
        [JsonPropertyName("swiftReference")]
        public string SwiftReference { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the processing status
        /// </summary>
        [JsonPropertyName("status")]
        public string Status { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the processing timestamp
        /// </summary>
        [JsonPropertyName("processingTime")]
        public DateTime ProcessingTime { get; set; }

        /// <summary>
        /// Gets or sets any processing fees
        /// </summary>
        [JsonPropertyName("fees")]
        public Fee? Fees { get; set; }

        /// <summary>
        /// Gets or sets estimated settlement time
        /// </summary>
        [JsonPropertyName("estimatedSettlement")]
        public DateTime? EstimatedSettlement { get; set; }
    }

    /// <summary>
    /// Fee information
    /// </summary>
    public class Fee
    {
        /// <summary>
        /// Gets or sets the fee amount
        /// </summary>
        [JsonPropertyName("amount")]
        public decimal Amount { get; set; }

        /// <summary>
        /// Gets or sets the fee currency
        /// </summary>
        [JsonPropertyName("currency")]
        public string Currency { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the fee description
        /// </summary>
        [JsonPropertyName("description")]
        public string? Description { get; set; }
    }
}