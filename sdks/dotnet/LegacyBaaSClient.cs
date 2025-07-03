using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Options;
using LegacyBaaS.SDK.Configuration;
using LegacyBaaS.SDK.Exceptions;
using LegacyBaaS.SDK.Models.Common;
using LegacyBaaS.SDK.Services;

namespace LegacyBaaS.SDK
{
    /// <summary>
    /// Main Legacy B2BaaS Platform Client for .NET
    /// 
    /// Provides access to all platform services including SWIFT processing,
    /// blockchain routing, BaNCS integration, and analytics.
    /// </summary>
    /// <example>
    /// <code>
    /// var client = new LegacyBaaSClient("your-api-key");
    /// 
    /// // Check platform health
    /// var health = await client.GetHealthAsync();
    /// Console.WriteLine($"Platform status: {health.Status}");
    /// 
    /// // Process SWIFT MT103 message
    /// var mt103 = new MT103Message
    /// {
    ///     TransactionReference = "TXN123456",
    ///     Currency = "USD",
    ///     Amount = 10000m,
    ///     OrderingCustomer = new Customer 
    ///     { 
    ///         Account = "123456789", 
    ///         Name = "John Doe" 
    ///     }
    /// };
    /// 
    /// var result = await client.Swift.ProcessMT103Async(mt103);
    /// Console.WriteLine($"Transaction ID: {result.TransactionId}");
    /// </code>
    /// </example>
    public class LegacyBaaSClient : IDisposable
    {
        private readonly HttpClient _httpClient;
        private readonly LegacyBaaSOptions _options;
        private readonly ILogger<LegacyBaaSClient> _logger;
        private readonly JsonSerializerOptions _jsonOptions;
        private bool _disposed;

        /// <summary>
        /// Gets the SWIFT processing service
        /// </summary>
        public ISwiftService Swift { get; }

        /// <summary>
        /// Gets the blockchain routing service  
        /// </summary>
        public IBlockchainService Blockchain { get; }

        /// <summary>
        /// Gets the BaNCS integration service
        /// </summary>
        public IBancsService Bancs { get; }

        /// <summary>
        /// Gets the analytics service
        /// </summary>
        public IAnalyticsService Analytics { get; }

        /// <summary>
        /// Gets the webhook handler service
        /// </summary>
        public IWebhookService Webhooks { get; }

        /// <summary>
        /// Gets the SDK version
        /// </summary>
        public static string SdkVersion => "1.0.0";

        /// <summary>
        /// Initializes a new instance of the LegacyBaaSClient
        /// </summary>
        /// <param name="apiKey">Your Legacy B2BaaS API key</param>
        /// <param name="options">Optional configuration options</param>
        /// <param name="httpClient">Optional HTTP client (will create new if not provided)</param>
        /// <param name="logger">Optional logger instance</param>
        /// <exception cref="ArgumentException">Thrown when API key is null or empty</exception>
        /// <exception cref="AuthenticationException">Thrown when API key format is invalid</exception>
        public LegacyBaaSClient(
            string apiKey, 
            LegacyBaaSOptions? options = null,
            HttpClient? httpClient = null,
            ILogger<LegacyBaaSClient>? logger = null)
        {
            if (string.IsNullOrWhiteSpace(apiKey))
                throw new ArgumentException("API key cannot be null or empty", nameof(apiKey));

            if (!IsValidApiKey(apiKey))
                throw new AuthenticationException("Invalid API key format");

            _options = options ?? new LegacyBaaSOptions();
            _logger = logger ?? Microsoft.Extensions.Logging.Abstractions.NullLogger<LegacyBaaSClient>.Instance;

            // Configure JSON options
            _jsonOptions = new JsonSerializerOptions
            {
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                WriteIndented = false,
                DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull
            };

            // Configure HTTP client
            _httpClient = httpClient ?? CreateHttpClient(apiKey);

            // Initialize services
            Swift = new SwiftService(_httpClient, _options, _jsonOptions, _logger);
            Blockchain = new BlockchainService(_httpClient, _options, _jsonOptions, _logger);
            Bancs = new BancsService(_httpClient, _options, _jsonOptions, _logger);
            Analytics = new AnalyticsService(_httpClient, _options, _jsonOptions, _logger);
            Webhooks = new WebhookService(_httpClient, _options, _jsonOptions, _logger);

            _logger.LogInformation("Legacy B2BaaS Client initialized for environment: {Environment}", _options.Environment);
        }

        /// <summary>
        /// Initializes a new instance using IOptions pattern
        /// </summary>
        /// <param name="options">Configuration options</param>
        /// <param name="httpClient">HTTP client</param>
        /// <param name="logger">Logger instance</param>
        public LegacyBaaSClient(
            IOptions<LegacyBaaSOptions> options,
            HttpClient httpClient,
            ILogger<LegacyBaaSClient> logger)
            : this(options.Value.ApiKey, options.Value, httpClient, logger)
        {
        }

        /// <summary>
        /// Check platform health status
        /// </summary>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Health status information</returns>
        public async Task<HealthStatus> GetHealthAsync(CancellationToken cancellationToken = default)
        {
            var response = await _httpClient.GetAsync("/health", cancellationToken);
            response.EnsureSuccessStatusCode();
            
            var content = await response.Content.ReadAsStringAsync(cancellationToken);
            return JsonSerializer.Deserialize<HealthStatus>(content, _jsonOptions)!;
        }

        /// <summary>
        /// Test API connectivity
        /// </summary>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Ping response with timestamp</returns>
        public async Task<Dictionary<string, object>> PingAsync(CancellationToken cancellationToken = default)
        {
            var response = await _httpClient.GetAsync("/ping", cancellationToken);
            response.EnsureSuccessStatusCode();
            
            var content = await response.Content.ReadAsStringAsync(cancellationToken);
            return JsonSerializer.Deserialize<Dictionary<string, object>>(content, _jsonOptions)!;
        }

        /// <summary>
        /// Get account information
        /// </summary>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Account details</returns>
        public async Task<Dictionary<string, object>> GetAccountAsync(CancellationToken cancellationToken = default)
        {
            var response = await _httpClient.GetAsync("/account", cancellationToken);
            response.EnsureSuccessStatusCode();
            
            var content = await response.Content.ReadAsStringAsync(cancellationToken);
            return JsonSerializer.Deserialize<Dictionary<string, object>>(content, _jsonOptions)!;
        }

        /// <summary>
        /// Get API usage statistics
        /// </summary>
        /// <param name="period">Time period (day, week, month)</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Usage statistics</returns>
        public async Task<Dictionary<string, object>> GetUsageAsync(
            string period = "day", 
            CancellationToken cancellationToken = default)
        {
            var response = await _httpClient.GetAsync($"/usage?period={period}", cancellationToken);
            response.EnsureSuccessStatusCode();
            
            var content = await response.Content.ReadAsStringAsync(cancellationToken);
            return JsonSerializer.Deserialize<Dictionary<string, object>>(content, _jsonOptions)!;
        }

        /// <summary>
        /// Get client configuration
        /// </summary>
        public LegacyBaaSOptions GetConfiguration() => _options;

        private HttpClient CreateHttpClient(string apiKey)
        {
            var httpClient = new HttpClient();
            
            httpClient.BaseAddress = new Uri(_options.BaseUrl);
            httpClient.Timeout = _options.Timeout;
            
            // Set default headers
            httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {apiKey}");
            httpClient.DefaultRequestHeaders.Add("Accept", "application/json");
            httpClient.DefaultRequestHeaders.Add("User-Agent", $"LegacyBaaS-DotNet-SDK/{SdkVersion}");
            httpClient.DefaultRequestHeaders.Add("X-SDK-Version", SdkVersion);
            httpClient.DefaultRequestHeaders.Add("X-SDK-Language", "csharp");

            return httpClient;
        }

        private static bool IsValidApiKey(string apiKey)
        {
            return !string.IsNullOrWhiteSpace(apiKey) && apiKey.Length >= 20;
        }

        /// <summary>
        /// Dispose resources
        /// </summary>
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        /// <summary>
        /// Dispose resources
        /// </summary>
        /// <param name="disposing">Whether disposing</param>
        protected virtual void Dispose(bool disposing)
        {
            if (!_disposed && disposing)
            {
                _httpClient?.Dispose();
                _logger.LogInformation("Legacy B2BaaS Client disposed");
                _disposed = true;
            }
        }
    }
}