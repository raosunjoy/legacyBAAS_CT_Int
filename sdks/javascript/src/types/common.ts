/**
 * Common Types and Interfaces
 */

/**
 * SDK Configuration
 */
export interface ClientConfig {
  baseUrl: string;
  version: string;
  timeout: number;
  retries?: number;
  retryDelay?: number;
  enableRealTime?: boolean;
}

/**
 * Standard API Response Wrapper
 */
export interface ApiResponse<T> {
  success: boolean;
  data: T;
  message?: string;
  timestamp: string;
  requestId?: string;
}

/**
 * Health Status Response
 */
export interface HealthStatus {
  status: 'healthy' | 'degraded' | 'unhealthy';
  components: {
    database: 'healthy' | 'degraded' | 'unhealthy';
    blockchain: 'healthy' | 'degraded' | 'unhealthy';
    swift: 'healthy' | 'degraded' | 'unhealthy';
    bancs: 'healthy' | 'degraded' | 'unhealthy';
  };
  uptime: number;
  version: string;
  timestamp: string;
}

/**
 * Pagination Options
 */
export interface PaginationOptions {
  page?: number;
  limit?: number;
  sortBy?: string;
  sortOrder?: 'asc' | 'desc';
}

/**
 * Date Range Filter
 */
export interface DateRangeFilter {
  startDate?: string;
  endDate?: string;
}

/**
 * Error Response
 */
export interface ErrorResponse {
  error: {
    code: string;
    message: string;
    details?: any;
  };
  timestamp: string;
  requestId?: string;
}