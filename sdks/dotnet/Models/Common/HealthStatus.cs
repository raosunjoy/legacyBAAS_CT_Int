using System;
using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace LegacyBaaS.SDK.Models.Common
{
    /// <summary>
    /// Health status response model
    /// </summary>
    public class HealthStatus
    {
        /// <summary>
        /// Gets or sets the overall status
        /// </summary>
        [JsonPropertyName("status")]
        public string Status { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the platform version
        /// </summary>
        [JsonPropertyName("version")]
        public string Version { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the timestamp
        /// </summary>
        [JsonPropertyName("timestamp")]
        public DateTime Timestamp { get; set; }

        /// <summary>
        /// Gets or sets the component health status
        /// </summary>
        [JsonPropertyName("components")]
        public Dictionary<string, string> Components { get; set; } = new();

        /// <summary>
        /// Gets whether the platform is healthy
        /// </summary>
        [JsonIgnore]
        public bool IsHealthy => string.Equals(Status, "healthy", StringComparison.OrdinalIgnoreCase);

        /// <summary>
        /// Returns a string representation of the health status
        /// </summary>
        public override string ToString()
        {
            return $"HealthStatus{{Status='{Status}', Version='{Version}', Timestamp='{Timestamp}'}}";
        }
    }

    /// <summary>
    /// Standard API response wrapper
    /// </summary>
    /// <typeparam name="T">Response data type</typeparam>
    public class ApiResponse<T>
    {
        /// <summary>
        /// Gets or sets the response data
        /// </summary>
        [JsonPropertyName("data")]
        public T? Data { get; set; }

        /// <summary>
        /// Gets or sets the response status
        /// </summary>
        [JsonPropertyName("status")]
        public string Status { get; set; } = string.Empty;

        /// <summary>
        /// Gets or sets the response message
        /// </summary>
        [JsonPropertyName("message")]
        public string? Message { get; set; }

        /// <summary>
        /// Gets or sets the error message
        /// </summary>
        [JsonPropertyName("error")]
        public string? Error { get; set; }

        /// <summary>
        /// Gets whether the response indicates success
        /// </summary>
        [JsonIgnore]
        public bool IsSuccess => string.IsNullOrEmpty(Error) && 
                                string.Equals(Status, "success", StringComparison.OrdinalIgnoreCase);

        /// <summary>
        /// Returns a string representation of the API response
        /// </summary>
        public override string ToString()
        {
            return $"ApiResponse{{Status='{Status}', Message='{Message}'}}";
        }
    }

    /// <summary>
    /// Pagination information
    /// </summary>
    public class PaginationInfo
    {
        /// <summary>
        /// Gets or sets the current page number
        /// </summary>
        [JsonPropertyName("page")]
        public int Page { get; set; }

        /// <summary>
        /// Gets or sets the number of items per page
        /// </summary>
        [JsonPropertyName("perPage")]
        public int PerPage { get; set; }

        /// <summary>
        /// Gets or sets the total number of items
        /// </summary>
        [JsonPropertyName("total")]
        public int Total { get; set; }

        /// <summary>
        /// Gets or sets the total number of pages
        /// </summary>
        [JsonPropertyName("totalPages")]
        public int TotalPages { get; set; }

        /// <summary>
        /// Gets whether there is a next page
        /// </summary>
        [JsonIgnore]
        public bool HasNextPage => Page < TotalPages;

        /// <summary>
        /// Gets whether there is a previous page
        /// </summary>
        [JsonIgnore]
        public bool HasPreviousPage => Page > 1;
    }

    /// <summary>
    /// Base class for paginated responses
    /// </summary>
    /// <typeparam name="T">Item type</typeparam>
    public class PagedResponse<T>
    {
        /// <summary>
        /// Gets or sets the items
        /// </summary>
        [JsonPropertyName("items")]
        public List<T> Items { get; set; } = new();

        /// <summary>
        /// Gets or sets the pagination information
        /// </summary>
        [JsonPropertyName("pagination")]
        public PaginationInfo Pagination { get; set; } = new();
    }
}