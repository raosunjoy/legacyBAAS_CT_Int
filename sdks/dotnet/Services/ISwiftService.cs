using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using LegacyBaaS.SDK.Models.Swift;
using LegacyBaaS.SDK.Models.Common;

namespace LegacyBaaS.SDK.Services
{
    /// <summary>
    /// Interface for SWIFT message processing service
    /// </summary>
    public interface ISwiftService
    {
        /// <summary>
        /// Process MT103 Single Customer Credit Transfer
        /// </summary>
        /// <param name="message">MT103 message data</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Processing result</returns>
        Task<SwiftProcessingResult> ProcessMT103Async(MT103Message message, CancellationToken cancellationToken = default);

        /// <summary>
        /// Get transaction status by transaction ID
        /// </summary>
        /// <param name="transactionId">Transaction ID</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Transaction status</returns>
        Task<TransactionStatus> GetTransactionStatusAsync(string transactionId, CancellationToken cancellationToken = default);

        /// <summary>
        /// List transactions with optional filtering
        /// </summary>
        /// <param name="filters">Filter options</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Paginated list of transactions</returns>
        Task<PagedResponse<TransactionSummary>> ListTransactionsAsync(TransactionFilters? filters = null, CancellationToken cancellationToken = default);

        /// <summary>
        /// Validate SWIFT message format
        /// </summary>
        /// <param name="messageType">Message type (e.g., "MT103")</param>
        /// <param name="messageData">Message data to validate</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Validation result</returns>
        Task<ValidationResult> ValidateMessageAsync(string messageType, object messageData, CancellationToken cancellationToken = default);

        /// <summary>
        /// Get current exchange rates
        /// </summary>
        /// <param name="baseCurrency">Base currency (optional)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Exchange rates</returns>
        Task<ExchangeRates> GetRatesAsync(string? baseCurrency = null, CancellationToken cancellationToken = default);

        /// <summary>
        /// Get specific currency pair rate
        /// </summary>
        /// <param name="currencyPair">Currency pair (e.g., "USD/EUR")</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Currency rate</returns>
        Task<CurrencyRate> GetRateAsync(string currencyPair, CancellationToken cancellationToken = default);
    }

    /// <summary>
    /// Transaction status information
    /// </summary>
    public class TransactionStatus
    {
        /// <summary>
        /// Gets or sets the transaction ID
        /// </summary>
        public string TransactionId { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the current status
        /// </summary>
        public string Status { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the SWIFT reference
        /// </summary>
        public string SwiftReference { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the creation timestamp
        /// </summary>
        public DateTime CreatedAt { get; set; }

        /// <summary>
        /// Gets or sets the completion timestamp
        /// </summary>
        public DateTime? CompletedAt { get; set; }

        /// <summary>
        /// Gets or sets the settlement status
        /// </summary>
        public string? SettlementStatus { get; set; }
    }

    /// <summary>
    /// Transaction summary for listings
    /// </summary>
    public class TransactionSummary
    {
        /// <summary>
        /// Gets or sets the transaction ID
        /// </summary>
        public string TransactionId { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the message type
        /// </summary>
        public string Type { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the status
        /// </summary>
        public string Status { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the amount
        /// </summary>
        public decimal Amount { get; set; }

        /// <summary>
        /// Gets or sets the currency
        /// </summary>
        public string Currency { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the creation timestamp
        /// </summary>
        public DateTime CreatedAt { get; set; }
    }

    /// <summary>
    /// Transaction filtering options
    /// </summary>
    public class TransactionFilters
    {
        /// <summary>
        /// Gets or sets the status filter
        /// </summary>
        public string? Status { get; set; }

        /// <summary>
        /// Gets or sets the message type filter
        /// </summary>
        public string? Type { get; set; }

        /// <summary>
        /// Gets or sets the from date filter
        /// </summary>
        public DateTime? FromDate { get; set; }

        /// <summary>
        /// Gets or sets the to date filter
        /// </summary>
        public DateTime? ToDate { get; set; }

        /// <summary>
        /// Gets or sets the page number
        /// </summary>
        public int Page { get; set; } = 1;

        /// <summary>
        /// Gets or sets the page size
        /// </summary>
        public int PerPage { get; set; } = 10;
    }

    /// <summary>
    /// SWIFT message validation result
    /// </summary>
    public class ValidationResult
    {
        /// <summary>
        /// Gets or sets whether the message is valid
        /// </summary>
        public bool Valid { get; set; }

        /// <summary>
        /// Gets or sets the message type
        /// </summary>
        public string MessageType { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the compliance status
        /// </summary>
        public string ComplianceStatus { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets any validation warnings
        /// </summary>
        public List<string> Warnings { get; set; } = new();

        /// <summary>
        /// Gets or sets the validation timestamp
        /// </summary>
        public DateTime ValidationTime { get; set; }
    }

    /// <summary>
    /// Exchange rates information
    /// </summary>
    public class ExchangeRates
    {
        /// <summary>
        /// Gets or sets the base currency
        /// </summary>
        public string BaseCurrency { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the exchange rates
        /// </summary>
        public Dictionary<string, decimal> Rates { get; set; } = new();

        /// <summary>
        /// Gets or sets the timestamp
        /// </summary>
        public DateTime Timestamp { get; set; }

        /// <summary>
        /// Gets or sets the rate provider
        /// </summary>
        public string Provider { get; set; } = string.Empty;
    }

    /// <summary>
    /// Currency rate for specific pair
    /// </summary>
    public class CurrencyRate
    {
        /// <summary>
        /// Gets or sets the from currency
        /// </summary>
        public string FromCurrency { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the to currency
        /// </summary>
        public string ToCurrency { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the exchange rate
        /// </summary>
        public decimal Rate { get; set; }

        /// <summary>
        /// Gets or sets the bid rate
        /// </summary>
        public decimal? Bid { get; set; }

        /// <summary>
        /// Gets or sets the ask rate
        /// </summary>
        public decimal? Ask { get; set; }

        /// <summary>
        /// Gets or sets the timestamp
        /// </summary>
        public DateTime Timestamp { get; set; }
    }
}