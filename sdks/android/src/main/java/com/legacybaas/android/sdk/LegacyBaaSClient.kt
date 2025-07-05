package com.legacybaas.android.sdk

import android.content.Context
import kotlinx.coroutines.*
import kotlinx.coroutines.flow.Flow
import kotlinx.serialization.json.Json
import okhttp3.*
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.RequestBody.Companion.toRequestBody
import timber.log.Timber
import java.io.IOException
import java.util.concurrent.TimeUnit
import com.legacybaas.android.sdk.configuration.LegacyBaaSConfiguration
import com.legacybaas.android.sdk.configuration.Environment
import com.legacybaas.android.sdk.exceptions.*
import com.legacybaas.android.sdk.models.common.*
import com.legacybaas.android.sdk.services.*
import com.legacybaas.android.sdk.utils.ApiKeyValidator

/**
 * Main Legacy B2BaaS Platform Client for Android
 *
 * Provides access to all platform services including SWIFT processing,
 * blockchain routing, BaNCS integration, and analytics.
 *
 * Example usage:
 * ```kotlin
 * val client = LegacyBaaSClient("your-api-key")
 *
 * // Check platform health
 * lifecycleScope.launch {
 *     try {
 *         val health = client.getHealth()
 *         Log.d("Health", "Platform status: ${health.status}")
 *     } catch (e: Exception) {
 *         Log.e("Error", "Health check failed: ${e.message}")
 *     }
 * }
 *
 * // Process SWIFT MT103 message
 * val mt103 = MT103Message(
 *     transactionReference = "TXN123456",
 *     currency = "USD",
 *     amount = 10000.0,
 *     orderingCustomer = Customer("123456789", "John Doe"),
 *     beneficiaryCustomer = Customer("987654321", "Jane Smith")
 * )
 *
 * lifecycleScope.launch {
 *     try {
 *         val result = client.swift.processMT103(mt103)
 *         Log.d("SWIFT", "Transaction ID: ${result.transactionId}")
 *     } catch (e: Exception) {
 *         Log.e("Error", "Transaction failed: ${e.message}")
 *     }
 * }
 * ```
 */
class LegacyBaaSClient @JvmOverloads constructor(
    private val apiKey: String,
    private val configuration: LegacyBaaSConfiguration = LegacyBaaSConfiguration.default(),
    context: Context? = null
) {
    
    companion object {
        /**
         * SDK version
         */
        const val SDK_VERSION = BuildConfig.SDK_VERSION
        
        private const val MEDIA_TYPE_JSON = "application/json; charset=utf-8"
    }
    
    private val json = Json {
        ignoreUnknownKeys = true
        isLenient = true
        encodeDefaults = false
    }
    
    private val client: OkHttpClient
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    
    /**
     * SWIFT processing service
     */
    val swift: SwiftService by lazy { SwiftService(this) }
    
    /**
     * Blockchain routing service
     */
    val blockchain: BlockchainService by lazy { BlockchainService(this) }
    
    /**
     * BaNCS integration service
     */
    val bancs: BancsService by lazy { BancsService(this) }
    
    /**
     * Analytics service
     */
    val analytics: AnalyticsService by lazy { AnalyticsService(this) }
    
    /**
     * Webhook handler service
     */
    val webhooks: WebhookService by lazy { WebhookService(this) }
    
    /**
     * COBOL transpiler service
     */
    val cobol: CobolTranspilerService by lazy { CobolTranspilerService(this) }
    
    init {
        require(ApiKeyValidator.isValid(apiKey)) { "Invalid API key format" }
        
        // Configure HTTP client
        client = OkHttpClient.Builder()
            .connectTimeout(configuration.timeout.toLong(), TimeUnit.SECONDS)
            .readTimeout(configuration.timeout.toLong(), TimeUnit.SECONDS)
            .writeTimeout(configuration.timeout.toLong(), TimeUnit.SECONDS)
            .addInterceptor(AuthInterceptor(apiKey))
            .addInterceptor(UserAgentInterceptor())
            .addInterceptor(RetryInterceptor(configuration.maxRetries))
            .apply {
                if (configuration.enableLogging) {
                    addInterceptor(LoggingInterceptor())
                }
            }
            .build()
        
        if (configuration.enableLogging) {
            Timber.tag("LegacyBaaS").i("Android SDK initialized for environment: ${configuration.environment}")
        }
    }
    
    /**
     * Check platform health status
     */
    suspend fun getHealth(): HealthStatus = withContext(Dispatchers.IO) {
        performRequest<HealthStatus>("/health", HttpMethod.GET)
    }
    
    /**
     * Test API connectivity
     */
    suspend fun ping(): PingResponse = withContext(Dispatchers.IO) {
        performRequest<PingResponse>("/ping", HttpMethod.GET)
    }
    
    /**
     * Get account information
     */
    suspend fun getAccount(): AccountInfo = withContext(Dispatchers.IO) {
        performRequest<AccountInfo>("/account", HttpMethod.GET)
    }
    
    /**
     * Get API usage statistics
     */
    suspend fun getUsage(period: String = "day"): UsageInfo = withContext(Dispatchers.IO) {
        performRequest<UsageInfo>("/usage?period=$period", HttpMethod.GET)
    }
    
    /**
     * Get client configuration
     */
    fun getConfiguration(): LegacyBaaSConfiguration = configuration
    
    /**
     * Close the client and cleanup resources
     */
    fun close() {
        scope.cancel()
        client.dispatcher.executorService.shutdown()
        client.connectionPool.evictAll()
        client.cache?.close()
    }
    
    // Internal methods
    
    internal suspend inline fun <reified T> performRequest(
        endpoint: String,
        method: HttpMethod,
        body: Any? = null
    ): T = withContext(Dispatchers.IO) {
        val url = buildUrl(endpoint)
        val request = buildRequest(url, method, body)
        
        try {
            val response = client.newCall(request).await()
            handleResponse<T>(response)
        } catch (e: IOException) {
            throw NetworkException("Network error: ${e.message}", e)
        }
    }
    
    private fun buildUrl(endpoint: String): String {
        val baseUrl = configuration.baseUrl ?: configuration.environment.baseUrl
        return "$baseUrl/api/v1$endpoint"
    }
    
    private fun buildRequest(url: String, method: HttpMethod, body: Any?): Request {
        val requestBuilder = Request.Builder().url(url)
        
        when (method) {
            HttpMethod.GET -> requestBuilder.get()
            HttpMethod.POST -> {
                val requestBody = body?.let {
                    json.encodeToString(kotlinx.serialization.serializer(), it)
                        .toRequestBody(MEDIA_TYPE_JSON.toMediaType())
                } ?: ByteArray(0).toRequestBody()
                requestBuilder.post(requestBody)
            }
            HttpMethod.PUT -> {
                val requestBody = body?.let {
                    json.encodeToString(kotlinx.serialization.serializer(), it)
                        .toRequestBody(MEDIA_TYPE_JSON.toMediaType())
                } ?: ByteArray(0).toRequestBody()
                requestBuilder.put(requestBody)
            }
            HttpMethod.DELETE -> requestBuilder.delete()
        }
        
        return requestBuilder.build()
    }
    
    private suspend inline fun <reified T> handleResponse(response: Response): T {
        val body = response.body?.string() ?: ""
        
        when (response.code) {
            in 200..299 -> {
                return try {
                    json.decodeFromString<T>(body)
                } catch (e: Exception) {
                    throw SerializationException("Failed to parse response: ${e.message}", e)
                }
            }
            400 -> throw ValidationException(parseErrorMessage(body))
            401 -> throw AuthenticationException(parseErrorMessage(body))
            429 -> {
                val retryAfter = response.header("Retry-After")?.toIntOrNull()
                throw RateLimitException(parseErrorMessage(body), retryAfter)
            }
            503 -> throw ServiceUnavailableException(parseErrorMessage(body))
            else -> throw HttpException(response.code, parseErrorMessage(body))
        }
    }
    
    private fun parseErrorMessage(body: String): String {
        return try {
            val errorResponse = json.decodeFromString<ErrorResponse>(body)
            errorResponse.message ?: "Unknown error"
        } catch (e: Exception) {
            "Unknown error"
        }
    }
    
    // HTTP Method enum
    internal enum class HttpMethod {
        GET, POST, PUT, DELETE
    }
}

// Extension function for async OkHttp calls
private suspend fun Call.await(): Response {
    return suspendCancellableCoroutine { continuation ->
        continuation.invokeOnCancellation {
            cancel()
        }
        
        enqueue(object : Callback {
            override fun onResponse(call: Call, response: Response) {
                continuation.resume(response)
            }
            
            override fun onFailure(call: Call, e: IOException) {
                continuation.resumeWithException(e)
            }
        })
    }
}