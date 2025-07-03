using System;

namespace LegacyBaaS.SDK.Exceptions
{
    /// <summary>
    /// Base exception for Legacy B2BaaS SDK
    /// </summary>
    public class LegacyBaaSException : Exception
    {
        /// <summary>
        /// Gets the error code
        /// </summary>
        public string? ErrorCode { get; }

        /// <summary>
        /// Gets the HTTP status code
        /// </summary>
        public int? StatusCode { get; }

        /// <summary>
        /// Initializes a new instance of LegacyBaaSException
        /// </summary>
        /// <param name="message">Error message</param>
        public LegacyBaaSException(string message) : base(message)
        {
        }

        /// <summary>
        /// Initializes a new instance of LegacyBaaSException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="errorCode">Error code</param>
        public LegacyBaaSException(string message, string errorCode) : base(message)
        {
            ErrorCode = errorCode;
        }

        /// <summary>
        /// Initializes a new instance of LegacyBaaSException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="errorCode">Error code</param>
        /// <param name="statusCode">HTTP status code</param>
        public LegacyBaaSException(string message, string errorCode, int statusCode) : base(message)
        {
            ErrorCode = errorCode;
            StatusCode = statusCode;
        }

        /// <summary>
        /// Initializes a new instance of LegacyBaaSException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        public LegacyBaaSException(string message, Exception innerException) : base(message, innerException)
        {
        }

        /// <summary>
        /// Initializes a new instance of LegacyBaaSException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        /// <param name="errorCode">Error code</param>
        /// <param name="statusCode">HTTP status code</param>
        public LegacyBaaSException(string message, Exception innerException, string errorCode, int statusCode) 
            : base(message, innerException)
        {
            ErrorCode = errorCode;
            StatusCode = statusCode;
        }
    }

    /// <summary>
    /// Authentication related exceptions
    /// </summary>
    public class AuthenticationException : LegacyBaaSException
    {
        /// <summary>
        /// Initializes a new instance of AuthenticationException
        /// </summary>
        /// <param name="message">Error message</param>
        public AuthenticationException(string message) : base(message, "AUTHENTICATION_ERROR", 401)
        {
        }

        /// <summary>
        /// Initializes a new instance of AuthenticationException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        public AuthenticationException(string message, Exception innerException) 
            : base(message, innerException, "AUTHENTICATION_ERROR", 401)
        {
        }
    }

    /// <summary>
    /// Validation related exceptions
    /// </summary>
    public class ValidationException : LegacyBaaSException
    {
        /// <summary>
        /// Gets validation details
        /// </summary>
        public object? Details { get; }

        /// <summary>
        /// Initializes a new instance of ValidationException
        /// </summary>
        /// <param name="message">Error message</param>
        public ValidationException(string message) : base(message, "VALIDATION_ERROR", 400)
        {
        }

        /// <summary>
        /// Initializes a new instance of ValidationException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="details">Validation details</param>
        public ValidationException(string message, object details) : base(message, "VALIDATION_ERROR", 400)
        {
            Details = details;
        }

        /// <summary>
        /// Initializes a new instance of ValidationException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        public ValidationException(string message, Exception innerException) 
            : base(message, innerException, "VALIDATION_ERROR", 400)
        {
        }
    }

    /// <summary>
    /// Network related exceptions
    /// </summary>
    public class NetworkException : LegacyBaaSException
    {
        /// <summary>
        /// Initializes a new instance of NetworkException
        /// </summary>
        /// <param name="message">Error message</param>
        public NetworkException(string message) : base(message, "NETWORK_ERROR")
        {
        }

        /// <summary>
        /// Initializes a new instance of NetworkException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        public NetworkException(string message, Exception innerException) 
            : base(message, innerException, "NETWORK_ERROR", 0)
        {
        }
    }

    /// <summary>
    /// Rate limiting exceptions
    /// </summary>
    public class RateLimitException : LegacyBaaSException
    {
        /// <summary>
        /// Gets the retry after seconds
        /// </summary>
        public int? RetryAfterSeconds { get; }

        /// <summary>
        /// Initializes a new instance of RateLimitException
        /// </summary>
        /// <param name="message">Error message</param>
        public RateLimitException(string message) : base(message, "RATE_LIMIT_ERROR", 429)
        {
        }

        /// <summary>
        /// Initializes a new instance of RateLimitException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="retryAfterSeconds">Retry after seconds</param>
        public RateLimitException(string message, int retryAfterSeconds) : base(message, "RATE_LIMIT_ERROR", 429)
        {
            RetryAfterSeconds = retryAfterSeconds;
        }

        /// <summary>
        /// Initializes a new instance of RateLimitException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        public RateLimitException(string message, Exception innerException) 
            : base(message, innerException, "RATE_LIMIT_ERROR", 429)
        {
        }
    }

    /// <summary>
    /// Service unavailable exceptions
    /// </summary>
    public class ServiceUnavailableException : LegacyBaaSException
    {
        /// <summary>
        /// Initializes a new instance of ServiceUnavailableException
        /// </summary>
        /// <param name="message">Error message</param>
        public ServiceUnavailableException(string message) : base(message, "SERVICE_UNAVAILABLE", 503)
        {
        }

        /// <summary>
        /// Initializes a new instance of ServiceUnavailableException
        /// </summary>
        /// <param name="message">Error message</param>
        /// <param name="innerException">Inner exception</param>
        public ServiceUnavailableException(string message, Exception innerException) 
            : base(message, innerException, "SERVICE_UNAVAILABLE", 503)
        {
        }
    }
}