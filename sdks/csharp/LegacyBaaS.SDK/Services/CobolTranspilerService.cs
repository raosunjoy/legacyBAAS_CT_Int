using System;
using System.Collections.Generic;
using System.Net.Http;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using LegacyBaaS.SDK.Models.Cobol;
using LegacyBaaS.SDK.Models.Common;
using LegacyBaaS.SDK.Exceptions;

namespace LegacyBaaS.SDK.Services
{
    /// <summary>
    /// COBOL Transpiler Service for C# SDK
    /// Handles all COBOL transpilation operations
    /// </summary>
    public class CobolTranspilerService : IDisposable
    {
        private readonly HttpClient _httpClient;
        private readonly string _baseUrl;
        private readonly JsonSerializerOptions _jsonOptions;
        private readonly Timer _statusTimer;
        private readonly Dictionary<string, StatusMonitor> _activeMonitors;
        
        public CobolTranspilerService(HttpClient httpClient, string baseUrl)
        {
            _httpClient = httpClient ?? throw new ArgumentNullException(nameof(httpClient));
            _baseUrl = $"{baseUrl}/banking";
            _jsonOptions = new JsonSerializerOptions
            {
                PropertyNamingPolicy = JsonNamingPolicy.CamelCase,
                WriteIndented = false
            };
            _activeMonitors = new Dictionary<string, StatusMonitor>();
        }
        
        /// <summary>
        /// Transpile COBOL code to target blockchain language
        /// </summary>
        public async Task<CobolTranspileResult> TranspileAsync(CobolTranspileRequest request, CancellationToken cancellationToken = default)
        {
            try
            {
                var json = JsonSerializer.Serialize(request, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"COBOL transpilation failed: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolTranspileResult>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"COBOL transpilation failed: {ex.Message}", ex);
            }
            catch (TaskCanceledException ex) when (ex.InnerException is TimeoutException)
            {
                throw new LegacyBaaSException("COBOL transpilation timed out", ex);
            }
        }
        
        /// <summary>
        /// Process multiple COBOL files in batch
        /// </summary>
        public async Task<List<CobolTranspileResult>> BatchTranspileAsync(CobolBatchRequest request, CancellationToken cancellationToken = default)
        {
            try
            {
                var json = JsonSerializer.Serialize(request, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile/batch", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Batch COBOL transpilation failed: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<List<CobolTranspileResult>>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Batch COBOL transpilation failed: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Validate COBOL code without transpilation
        /// </summary>
        public async Task<CobolValidationResult> ValidateAsync(string sourceCode, string bankingSystem, CancellationToken cancellationToken = default)
        {
            try
            {
                var request = new CobolValidationRequest
                {
                    SourceCode = sourceCode,
                    BankingSystem = bankingSystem
                };
                
                var json = JsonSerializer.Serialize(request, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile/validate", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"COBOL validation failed: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolValidationResult>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"COBOL validation failed: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Get transpilation job status
        /// </summary>
        public async Task<CobolStatusUpdate> GetStatusAsync(string jobId, CancellationToken cancellationToken = default)
        {
            try
            {
                var response = await _httpClient.GetAsync($"{_baseUrl}/transpile/status/{jobId}", cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to get transpilation status: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolStatusUpdate>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to get transpilation status: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Monitor transpilation status with callbacks
        /// </summary>
        public StatusMonitor MonitorStatus(string jobId)
        {
            var monitor = new StatusMonitor(jobId, this);
            _activeMonitors[jobId] = monitor;
            return monitor;
        }
        
        /// <summary>
        /// Get available COBOL templates
        /// </summary>
        public async Task<List<CobolTemplate>> GetTemplatesAsync(string bankingSystem = null, string category = null, CancellationToken cancellationToken = default)
        {
            try
            {
                var url = $"{_baseUrl}/transpile/templates";
                var queryParams = new List<string>();
                
                if (!string.IsNullOrEmpty(bankingSystem))
                    queryParams.Add($"bankingSystem={Uri.EscapeDataString(bankingSystem)}");
                
                if (!string.IsNullOrEmpty(category))
                    queryParams.Add($"category={Uri.EscapeDataString(category)}");
                
                if (queryParams.Count > 0)
                    url += "?" + string.Join("&", queryParams);
                
                var response = await _httpClient.GetAsync(url, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to get templates: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<List<CobolTemplate>>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to get templates: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Get specific template by ID
        /// </summary>
        public async Task<CobolTemplate> GetTemplateAsync(string templateId, CancellationToken cancellationToken = default)
        {
            try
            {
                var response = await _httpClient.GetAsync($"{_baseUrl}/transpile/templates/{templateId}", cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to get template: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolTemplate>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to get template: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Transpile using a template
        /// </summary>
        public async Task<CobolTranspileResult> TranspileWithTemplateAsync(CobolTemplateRequest request, CancellationToken cancellationToken = default)
        {
            try
            {
                var json = JsonSerializer.Serialize(request, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile/templates/{request.TemplateId}", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Template transpilation failed: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolTranspileResult>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Template transpilation failed: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Create custom banking system configuration
        /// </summary>
        public async Task<CobolBankingConfig> CreateBankingConfigAsync(CobolBankingConfigRequest request, CancellationToken cancellationToken = default)
        {
            try
            {
                var json = JsonSerializer.Serialize(request, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile/configs", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to create banking config: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolBankingConfig>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to create banking config: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Get banking system configurations
        /// </summary>
        public async Task<List<CobolBankingConfig>> GetBankingConfigsAsync(CancellationToken cancellationToken = default)
        {
            try
            {
                var response = await _httpClient.GetAsync($"{_baseUrl}/transpile/configs", cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to get banking configs: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<List<CobolBankingConfig>>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to get banking configs: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Select optimal blockchain network
        /// </summary>
        public async Task<NetworkSelection> SelectOptimalNetworkAsync(NetworkSelectionCriteria criteria, CancellationToken cancellationToken = default)
        {
            try
            {
                var json = JsonSerializer.Serialize(criteria, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile/network-selection", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Network selection failed: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<NetworkSelection>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Network selection failed: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Get transpilation analytics
        /// </summary>
        public async Task<CobolAnalytics> GetAnalyticsAsync(string period = "24h", CancellationToken cancellationToken = default)
        {
            try
            {
                var response = await _httpClient.GetAsync($"{_baseUrl}/transpile/analytics?period={period}", cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to get analytics: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolAnalytics>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to get analytics: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Get quota information
        /// </summary>
        public async Task<CobolQuota> GetQuotaAsync(CancellationToken cancellationToken = default)
        {
            try
            {
                var response = await _httpClient.GetAsync($"{_baseUrl}/transpile/quota", cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Failed to get quota information: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CobolQuota>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Failed to get quota information: {ex.Message}", ex);
            }
        }
        
        /// <summary>
        /// Estimate transpilation cost
        /// </summary>
        public async Task<CostEstimate> EstimateCostAsync(CostEstimateRequest request, CancellationToken cancellationToken = default)
        {
            try
            {
                var json = JsonSerializer.Serialize(request, _jsonOptions);
                var content = new StringContent(json, Encoding.UTF8, "application/json");
                
                var response = await _httpClient.PostAsync($"{_baseUrl}/transpile/cost-estimate", content, cancellationToken);
                
                if (!response.IsSuccessStatusCode)
                {
                    var errorContent = await response.Content.ReadAsStringAsync();
                    throw new LegacyBaaSException($"Cost estimation failed: {response.ReasonPhrase}. {errorContent}");
                }
                
                var responseJson = await response.Content.ReadAsStringAsync();
                var apiResponse = JsonSerializer.Deserialize<ApiResponse<CostEstimate>>(responseJson, _jsonOptions);
                
                return apiResponse.Data;
            }
            catch (HttpRequestException ex)
            {
                throw new LegacyBaaSException($"Cost estimation failed: {ex.Message}", ex);
            }
        }
        
        public void Dispose()
        {
            foreach (var monitor in _activeMonitors.Values)
            {
                monitor.Stop();
            }
            _activeMonitors.Clear();
        }
    }
    
    /// <summary>
    /// Status Monitor for tracking transpilation progress in C#
    /// </summary>
    public class StatusMonitor : IDisposable
    {
        private readonly string _jobId;
        private readonly CobolTranspilerService _service;
        private Timer _pollTimer;
        private volatile bool _isMonitoring;
        
        public event Action<CobolStatusUpdate> OnProgress;
        public event Action<CobolStatusUpdate> OnCompleted;
        public event Action<Exception> OnError;
        
        internal StatusMonitor(string jobId, CobolTranspilerService service)
        {
            _jobId = jobId;
            _service = service;
        }
        
        public void Start(TimeSpan? pollInterval = null)
        {
            if (_isMonitoring) return;
            
            _isMonitoring = true;
            var interval = pollInterval ?? TimeSpan.FromSeconds(2);
            
            _pollTimer = new Timer(async _ =>
            {
                try
                {
                    var status = await _service.GetStatusAsync(_jobId);
                    
                    OnProgress?.Invoke(status);
                    
                    if (status.Progress >= 100)
                    {
                        _isMonitoring = false;
                        OnCompleted?.Invoke(status);
                        Stop();
                    }
                }
                catch (Exception ex)
                {
                    _isMonitoring = false;
                    OnError?.Invoke(ex);
                    Stop();
                }
            }, null, TimeSpan.Zero, interval);
        }
        
        public void Stop()
        {
            _isMonitoring = false;
            _pollTimer?.Dispose();
            _pollTimer = null;
        }
        
        public async Task<CobolStatusUpdate> WaitForCompletionAsync(CancellationToken cancellationToken = default)
        {
            var tcs = new TaskCompletionSource<CobolStatusUpdate>();
            
            OnCompleted += result => tcs.TrySetResult(result);
            OnError += error => tcs.TrySetException(error);
            
            cancellationToken.Register(() => tcs.TrySetCanceled());
            
            Start();
            
            return await tcs.Task;
        }
        
        public void Dispose()
        {
            Stop();
        }
    }
}