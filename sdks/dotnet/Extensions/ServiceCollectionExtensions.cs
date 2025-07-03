using System;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Options;
using LegacyBaaS.SDK.Configuration;

namespace LegacyBaaS.SDK.Extensions
{
    /// <summary>
    /// Extension methods for IServiceCollection to register Legacy B2BaaS services
    /// </summary>
    public static class ServiceCollectionExtensions
    {
        /// <summary>
        /// Adds Legacy B2BaaS services to the dependency injection container
        /// </summary>
        /// <param name="services">The service collection</param>
        /// <param name="apiKey">The API key</param>
        /// <param name="configureOptions">Optional configuration action</param>
        /// <returns>The service collection for chaining</returns>
        public static IServiceCollection AddLegacyBaaS(
            this IServiceCollection services,
            string apiKey,
            Action<LegacyBaaSOptions>? configureOptions = null)
        {
            return services.AddLegacyBaaS(options =>
            {
                options.ApiKey = apiKey;
                configureOptions?.Invoke(options);
            });
        }

        /// <summary>
        /// Adds Legacy B2BaaS services to the dependency injection container
        /// </summary>
        /// <param name="services">The service collection</param>
        /// <param name="configureOptions">Configuration action</param>
        /// <returns>The service collection for chaining</returns>
        public static IServiceCollection AddLegacyBaaS(
            this IServiceCollection services,
            Action<LegacyBaaSOptions> configureOptions)
        {
            services.Configure(configureOptions);
            
            services.AddHttpClient<LegacyBaaSClient>((serviceProvider, httpClient) =>
            {
                var options = serviceProvider.GetRequiredService<IOptions<LegacyBaaSOptions>>().Value;
                
                httpClient.BaseAddress = new Uri(options.FullApiUrl);
                httpClient.Timeout = options.Timeout;
                
                // Set default headers
                httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
                httpClient.DefaultRequestHeaders.Add("Accept", "application/json");
                httpClient.DefaultRequestHeaders.Add("User-Agent", $"LegacyBaaS-DotNet-SDK/{LegacyBaaSClient.SdkVersion}");
                httpClient.DefaultRequestHeaders.Add("X-SDK-Version", LegacyBaaSClient.SdkVersion);
                httpClient.DefaultRequestHeaders.Add("X-SDK-Language", "csharp");
            });

            services.AddScoped<LegacyBaaSClient>();
            
            return services;
        }

        /// <summary>
        /// Adds Legacy B2BaaS services to the dependency injection container using configuration
        /// </summary>
        /// <param name="services">The service collection</param>
        /// <param name="configuration">The configuration</param>
        /// <param name="sectionName">The configuration section name (defaults to "LegacyBaaS")</param>
        /// <returns>The service collection for chaining</returns>
        public static IServiceCollection AddLegacyBaaS(
            this IServiceCollection services,
            IConfiguration configuration,
            string sectionName = LegacyBaaSOptions.SectionName)
        {
            services.Configure<LegacyBaaSOptions>(configuration.GetSection(sectionName));
            
            services.AddHttpClient<LegacyBaaSClient>((serviceProvider, httpClient) =>
            {
                var options = serviceProvider.GetRequiredService<IOptions<LegacyBaaSOptions>>().Value;
                
                httpClient.BaseAddress = new Uri(options.FullApiUrl);
                httpClient.Timeout = options.Timeout;
                
                // Set default headers
                httpClient.DefaultRequestHeaders.Add("Authorization", $"Bearer {options.ApiKey}");
                httpClient.DefaultRequestHeaders.Add("Accept", "application/json");
                httpClient.DefaultRequestHeaders.Add("User-Agent", $"LegacyBaaS-DotNet-SDK/{LegacyBaaSClient.SdkVersion}");
                httpClient.DefaultRequestHeaders.Add("X-SDK-Version", LegacyBaaSClient.SdkVersion);
                httpClient.DefaultRequestHeaders.Add("X-SDK-Language", "csharp");
            });

            services.AddScoped<LegacyBaaSClient>();
            
            return services;
        }
    }
}