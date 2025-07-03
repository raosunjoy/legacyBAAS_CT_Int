using System;
using System.ComponentModel.DataAnnotations;

namespace LegacyBaaS.SDK.Configuration
{
    /// <summary>
    /// Configuration options for Legacy B2BaaS Client
    /// </summary>
    public class LegacyBaaSOptions
    {
        /// <summary>
        /// Configuration section name for appsettings.json
        /// </summary>
        public const string SectionName = "LegacyBaaS";

        /// <summary>
        /// Gets or sets the API key
        /// </summary>
        [Required]
        public string ApiKey { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the environment
        /// </summary>
        public LegacyBaaSEnvironment Environment { get; set; } = LegacyBaaSEnvironment.Production;

        /// <summary>
        /// Gets or sets the base URL (overrides environment default)
        /// </summary>
        public string? BaseUrl { get; set; }

        /// <summary>
        /// Gets or sets the API version
        /// </summary>
        public string Version { get; set; } = "v1";

        /// <summary>
        /// Gets or sets the request timeout
        /// </summary>
        public TimeSpan Timeout { get; set; } = TimeSpan.FromSeconds(30);

        /// <summary>
        /// Gets or sets the maximum number of retries
        /// </summary>
        [Range(0, 10)]
        public int MaxRetries { get; set; } = 3;

        /// <summary>
        /// Gets or sets the retry delay
        /// </summary>
        public TimeSpan RetryDelay { get; set; } = TimeSpan.FromSeconds(1);

        /// <summary>
        /// Gets or sets whether logging is enabled
        /// </summary>
        public bool EnableLogging { get; set; } = true;

        /// <summary>
        /// Gets the effective base URL
        /// </summary>
        internal string EffectiveBaseUrl => BaseUrl ?? Environment.GetBaseUrl();

        /// <summary>
        /// Gets the full API URL
        /// </summary>
        internal string FullApiUrl => $"{EffectiveBaseUrl}/api/{Version}";
    }

    /// <summary>
    /// Supported environments for Legacy B2BaaS Platform
    /// </summary>
    public enum LegacyBaaSEnvironment
    {
        /// <summary>
        /// Production environment
        /// </summary>
        Production,

        /// <summary>
        /// Staging environment  
        /// </summary>
        Staging,

        /// <summary>
        /// Sandbox environment
        /// </summary>
        Sandbox
    }

    /// <summary>
    /// Extension methods for LegacyBaaSEnvironment
    /// </summary>
    public static class LegacyBaaSEnvironmentExtensions
    {
        /// <summary>
        /// Get the base URL for the environment
        /// </summary>
        /// <param name="environment">The environment</param>
        /// <returns>Base URL</returns>
        public static string GetBaseUrl(this LegacyBaaSEnvironment environment) => environment switch
        {
            LegacyBaaSEnvironment.Production => "https://api.legacybaas.com",
            LegacyBaaSEnvironment.Staging => "https://staging-api.legacybaas.com",
            LegacyBaaSEnvironment.Sandbox => "https://sandbox-api.legacybaas.com",
            _ => throw new ArgumentOutOfRangeException(nameof(environment), environment, null)
        };

        /// <summary>
        /// Get the environment name as string
        /// </summary>
        /// <param name="environment">The environment</param>
        /// <returns>Environment name</returns>
        public static string ToStringValue(this LegacyBaaSEnvironment environment) => environment switch
        {
            LegacyBaaSEnvironment.Production => "production",
            LegacyBaaSEnvironment.Staging => "staging", 
            LegacyBaaSEnvironment.Sandbox => "sandbox",
            _ => throw new ArgumentOutOfRangeException(nameof(environment), environment, null)
        };
    }
}