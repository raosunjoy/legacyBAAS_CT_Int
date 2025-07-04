/**
 * LegacyBAAS C# Client
 * Main client for all SDK operations
 */

using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using LegacyBaaS.SDK.Services;
using LegacyBaaS.SDK.Errors;
using LegacyBaaS.SDK.Utils;

namespace LegacyBaaS.SDK
{
    /// <summary>
    /// Main LegacyBAAS client for C# SDK
    /// </summary>
    public class LegacyBaaSClient : IDisposable
    {
        private readonly string _clientId;
        private readonly string _clientSecret;
        private readonly string _baseUrl;
        private readonly string _environment;
        private readonly TimeSpan _timeout;
        private readonly int _maxRetries;
        private readonly TimeSpan _retryDelay;
        private readonly ILogger _logger;
        
        private string _accessToken;
        private DateTime _tokenExpiry;
        
        private readonly HttpClient _httpClient;
        private readonly JsonSerializerOptions _jsonOptions;
        private readonly RetryHandler _retryHandler;
        
        // Services
        public SwiftProcessor Swift { get; }
        public BlockchainRouter Blockchain { get; }
        public BankingService Banking { get; }
        public AnalyticsService Analytics { get; }
        public WebhookHandler Webhooks { get; }
        public ComplianceService Compliance { get; }
        
        /// <summary>
        /// Constructor
        /// </summary>
        public LegacyBaaSClient(LegacyBaaSClientOptions options)
        {
            _clientId = options.ClientId ?? throw new ArgumentNullException(nameof(options.ClientId));
            _clientSecret = options.ClientSecret ?? throw new ArgumentNullException(nameof(options.ClientSecret));
            _baseUrl = options.BaseUrl ?? GetBaseUrl(options.Environment ?? "production");
            _environment = options.Environment ?? "production";
            _timeout = options.Timeout ?? TimeSpan.FromSeconds(30);
            _maxRetries = options.MaxRetries ?? 3;
            _retryDelay = options.RetryDelay ?? TimeSpan.FromSeconds(1);
            _logger = options.EnableLogging ? new Logger(options.LogLevel ?? "INFO") : null;
            
            // Initialize HTTP client
            _httpClient = new HttpClient()
            {
                Timeout = _timeout
            };
            _httpClient.DefaultRequestHeaders.Add("User-Agent", "LegacyBAAS-CSharp-SDK/2.0.0");
            _httpClient.DefaultRequestHeaders.Add("Accept", "application/json");
            
            _jsonOptions = new JsonSerializerOptions
            {
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                WriteIndented = false
            };
            
            _retryHandler = new RetryHandler(_maxRetries, _retryDelay);
            
            // Initialize services
            Swift = new SwiftProcessor(this);
            Blockchain = new BlockchainRouter(this);
            Banking = new BankingService(this);
            Analytics = new AnalyticsService(this);
            Webhooks = new WebhookHandler(this);
            Compliance = new ComplianceService(this);
            
            _logger?.Info($"LegacyBAAS Client initialized - Environment: {_environment}, Base URL: {_baseUrl}");
        }
        
        /// <summary>
        /// Get base URL for environment
        /// </summary>
        private string GetBaseUrl(string environment)
        {
            return environment.ToLower() switch
            {
                "production" => "https://api.legacybaas.com/v1",
                "sandbox" => "https://sandbox.legacybaas.com/v1",
                "development" => "http://localhost:3000/v1",
                _ => "https://api.legacybaas.com/v1"
            };
        }
        
        /// <summary>
        /// Authenticate with OAuth2 client credentials flow
        /// </summary>
        public async Task<Dictionary<string, object>> AuthenticateAsync()
        {
            if (string.IsNullOrEmpty(_clientId) || string.IsNullOrEmpty(_clientSecret))
            {
                throw new AuthenticationError("Client ID and Client Secret are required for authentication");
            }
            
            var data = new Dictionary<string, object>
            {
                ["grant_type"] = "client_credentials",
                ["client_id"] = _clientId,
                ["client_secret"] = _clientSecret,
                ["scope"] = "banking:read banking:write blockchain:execute compliance:screen"
            };
            
            try
            {
                var response = await MakeRequestAsync<Dictionary<string, object>>("POST", "/auth/token", data, skipAuth: true);
                
                _accessToken = response["access_token"].ToString();
                var expiresIn = Convert.ToInt32(response["expires_in"]);
                _tokenExpiry = DateTime.UtcNow.AddSeconds(expiresIn);
                
                _logger?.Info("Authentication successful");
                
                return response;
            }
            catch (Exception e)
            {
                _logger?.Error($"Authentication failed: {e.Message}");
                throw new AuthenticationError("Failed to authenticate with LegacyBAAS API");
            }
        }
        
        /// <summary>
        /// Check if client has valid authentication token
        /// </summary>
        public bool IsAuthenticated()
        {
            return !string.IsNullOrEmpty(_accessToken) && DateTime.UtcNow < _tokenExpiry;
        }
        
        /// <summary>
        /// Ensure client is authenticated
        /// </summary>
        public async Task EnsureAuthenticatedAsync()
        {
            if (!IsAuthenticated())
            {
                await AuthenticateAsync();
            }
        }
        
        /// <summary>
        /// Make HTTP request to API
        /// </summary>
        public async Task<T> MakeRequestAsync<T>(string method, string endpoint, object data = null, bool skipAuth = false, Dictionary<string, string> headers = null)
        {
            if (!skipAuth)
            {
                await EnsureAuthenticatedAsync();
            }
            
            var url = _baseUrl + endpoint;
            var request = new HttpRequestMessage(new HttpMethod(method), url);
            
            // Add headers
            request.Headers.Add("Content-Type", "application/json");
            if (!string.IsNullOrEmpty(_accessToken) && !skipAuth)
            {
                request.Headers.Add("Authorization", $"Bearer {_accessToken}");
            }
            
            if (headers != null)
            {
                foreach (var header in headers)
                {
                    request.Headers.Add(header.Key, header.Value);
                }
            }
            
            // Add body for POST/PUT requests
            if (data != null && (method == "POST" || method == "PUT" || method == "PATCH"))
            {
                var json = JsonSerializer.Serialize(data, _jsonOptions);
                request.Content = new StringContent(json, Encoding.UTF8, "application/json");
            }
            
            try
            {
                _logger?.Debug($"Making {method} request to {endpoint}");
                
                var response = await _httpClient.SendAsync(request);
                
                return await HandleResponseAsync<T>(response);
            }
            catch (HttpRequestException e)
            {
                throw new NetworkError($"Request failed: {e.Message}");
            }
            catch (TaskCanceledException e)
            {
                throw new NetworkError("Request timeout");
            }
        }
        
        /// <summary>
        /// Handle HTTP response
        /// </summary>
        private async Task<T> HandleResponseAsync<T>(HttpResponseMessage response)
        {
            var content = await response.Content.ReadAsStringAsync();
            
            if (!response.IsSuccessStatusCode)
            {
                Dictionary<string, object> errorData;
                try
                {
                    errorData = JsonSerializer.Deserialize<Dictionary<string, object>>(content, _jsonOptions);
                }
                catch
                {
                    errorData = new Dictionary<string, object> { ["message"] = content };
                }
                
                var message = errorData.TryGetValue("message", out var msg) ? msg.ToString() : $"HTTP {(int)response.StatusCode}";
                
                throw (int)response.StatusCode switch
                {
                    401 => new AuthenticationError(message),
                    400 => new ValidationError(message),
                    429 => new RateLimitError(message),
                    >= 500 => new NetworkError(message),
                    _ => new LegacyBaaSError(message, (int)response.StatusCode)
                };
            }
            
            _logger?.Debug($"Request completed: {response.RequestMessage.Method} {response.RequestMessage.RequestUri}");
            
            // Parse response
            var responseData = JsonSerializer.Deserialize<Dictionary<string, object>>(content, _jsonOptions);
            if (responseData.TryGetValue("data", out var data))
            {
                return JsonSerializer.Deserialize<T>(data.ToString(), _jsonOptions);
            }
            
            return JsonSerializer.Deserialize<T>(content, _jsonOptions);
        }
        
        /// <summary>
        /// Get API health status
        /// </summary>
        public async Task<object> GetHealthAsync()
        {
            return await MakeRequestAsync<object>("GET", "/health");
        }
        
        /// <summary>
        /// Get platform metrics
        /// </summary>
        public async Task<object> GetMetricsAsync()
        {
            return await MakeRequestAsync<object>("GET", "/admin/metrics");
        }
        
        /// <summary>
        /// Dispose resources
        /// </summary>
        public void Dispose()
        {
            _httpClient?.Dispose();
            _accessToken = null;
            _tokenExpiry = DateTime.MinValue;
            
            _logger?.Info("LegacyBAAS Client disposed");
        }
    }
    
    /// <summary>
    /// Configuration options for LegacyBaaSClient
    /// </summary>
    public class LegacyBaaSClientOptions
    {
        public string ClientId { get; set; }
        public string ClientSecret { get; set; }
        public string BaseUrl { get; set; }
        public string Environment { get; set; } = "production";
        public TimeSpan? Timeout { get; set; } = TimeSpan.FromSeconds(30);
        public int? MaxRetries { get; set; } = 3;
        public TimeSpan? RetryDelay { get; set; } = TimeSpan.FromSeconds(1);
        public bool EnableLogging { get; set; } = true;
        public string LogLevel { get; set; } = "INFO";
    }
}