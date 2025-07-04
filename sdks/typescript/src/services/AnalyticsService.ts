/**
 * Analytics Service
 * Advanced analytics and reporting for banking and blockchain operations
 */

import { LegacyBaaSClient } from '../client/LegacyBaaSClient';
import { ApiResponse } from '../types';

export interface AnalyticsQuery {
  metrics: string[];
  dimensions?: string[];
  filters?: Record<string, any>;
  timeRange: {
    start: string;
    end: string;
  };
  groupBy?: string;
  orderBy?: {
    metric: string;
    direction: 'asc' | 'desc';
  };
  limit?: number;
}

export interface AnalyticsResult {
  queryId: string;
  data: Array<{
    dimensions: Record<string, any>;
    metrics: Record<string, number>;
    timestamp: string;
  }>;
  metadata: {
    totalRows: number;
    processedAt: string;
    processingTime: number;
    cached: boolean;
  };
  summary: {
    aggregations: Record<string, number>;
    trends: Record<string, {
      trend: 'up' | 'down' | 'stable';
      changePercent: number;
      significance: 'high' | 'medium' | 'low';
    }>;
  };
}

export interface DashboardConfig {
  dashboardId: string;
  name: string;
  widgets: Array<{
    widgetId: string;
    type: 'chart' | 'table' | 'metric' | 'gauge' | 'map';
    title: string;
    query: AnalyticsQuery;
    chartConfig?: {
      chartType: 'line' | 'bar' | 'pie' | 'area' | 'scatter';
      xAxis: string;
      yAxis: string;
      series: string[];
    };
    refreshInterval?: number;
    position: { x: number; y: number; width: number; height: number };
  }>;
  filters: Record<string, any>;
  refreshInterval: number;
}

export interface ReportConfig {
  reportId: string;
  name: string;
  type: 'scheduled' | 'ad_hoc' | 'real_time';
  query: AnalyticsQuery;
  format: 'pdf' | 'csv' | 'json' | 'xlsx';
  schedule?: {
    frequency: 'daily' | 'weekly' | 'monthly' | 'quarterly';
    time: string;
    timezone: string;
    recipients: string[];
  };
  template?: string;
}

export class AnalyticsService {
  constructor(private client: LegacyBaaSClient) {}

  /**
   * Execute analytics query
   */
  async executeQuery(query: AnalyticsQuery): Promise<AnalyticsResult> {
    const response = await this.client.makeRequest<AnalyticsResult>(
      'POST',
      '/analytics/query',
      query
    );

    return response.data;
  }

  /**
   * Get pre-built metrics
   */
  async getMetrics(
    category: 'banking' | 'blockchain' | 'swift' | 'compliance' | 'system',
    timeRange: { start: string; end: string },
    filters: Record<string, any> = {}
  ): Promise<{
    category: string;
    metrics: Record<string, {
      value: number;
      unit: string;
      trend: {
        direction: 'up' | 'down' | 'stable';
        changePercent: number;
        previousValue: number;
      };
      benchmark?: {
        value: number;
        comparison: 'above' | 'below' | 'equal';
      };
    }>;
    timestamp: string;
  }> {
    const response = await this.client.makeRequest<{
      category: string;
      metrics: Record<string, {
        value: number;
        unit: string;
        trend: {
          direction: 'up' | 'down' | 'stable';
          changePercent: number;
          previousValue: number;
        };
        benchmark?: {
          value: number;
          comparison: 'above' | 'below' | 'equal';
        };
      }>;
      timestamp: string;
    }>('GET', `/analytics/metrics/${category}`, undefined, {
      headers: { 
        'X-Time-Range': JSON.stringify(timeRange),
        'X-Filters': JSON.stringify(filters)
      }
    });

    return response.data;
  }

  /**
   * Get system performance metrics
   */
  async getSystemMetrics(): Promise<{
    uptime: number;
    responseTime: {
      avg: number;
      p95: number;
      p99: number;
    };
    throughput: {
      requestsPerSecond: number;
      transactionsPerSecond: number;
    };
    errorRate: number;
    resourceUtilization: {
      cpu: number;
      memory: number;
      disk: number;
      network: number;
    };
    serviceHealth: Record<string, {
      status: 'healthy' | 'degraded' | 'unhealthy';
      uptime: number;
      lastCheck: string;
    }>;
  }> {
    const response = await this.client.makeRequest<{
      uptime: number;
      responseTime: {
        avg: number;
        p95: number;
        p99: number;
      };
      throughput: {
        requestsPerSecond: number;
        transactionsPerSecond: number;
      };
      errorRate: number;
      resourceUtilization: {
        cpu: number;
        memory: number;
        disk: number;
        network: number;
      };
      serviceHealth: Record<string, {
        status: 'healthy' | 'degraded' | 'unhealthy';
        uptime: number;
        lastCheck: string;
      }>;
    }>('GET', '/analytics/system');

    return response.data;
  }

  /**
   * Get real-time analytics
   */
  async getRealTimeMetrics(): Promise<{
    timestamp: string;
    metrics: {
      activeTransactions: number;
      queueDepth: number;
      processingRate: number;
      errorRate: number;
      networkLatency: Record<string, number>;
      systemLoad: number;
    };
    alerts: Array<{
      alertId: string;
      severity: 'low' | 'medium' | 'high' | 'critical';
      message: string;
      timestamp: string;
      category: string;
    }>;
  }> {
    const response = await this.client.makeRequest<{
      timestamp: string;
      metrics: {
        activeTransactions: number;
        queueDepth: number;
        processingRate: number;
        errorRate: number;
        networkLatency: Record<string, number>;
        systemLoad: number;
      };
      alerts: Array<{
        alertId: string;
        severity: 'low' | 'medium' | 'high' | 'critical';
        message: string;
        timestamp: string;
        category: string;
      }>;
    }>('GET', '/analytics/real-time');

    return response.data;
  }

  /**
   * Create custom dashboard
   */
  async createDashboard(config: DashboardConfig): Promise<{
    dashboardId: string;
    url: string;
    embedUrl: string;
    createdAt: string;
  }> {
    const response = await this.client.makeRequest<{
      dashboardId: string;
      url: string;
      embedUrl: string;
      createdAt: string;
    }>('POST', '/analytics/dashboards', config);

    return response.data;
  }

  /**
   * Get dashboard
   */
  async getDashboard(dashboardId: string): Promise<DashboardConfig> {
    const response = await this.client.makeRequest<DashboardConfig>(
      'GET',
      `/analytics/dashboards/${dashboardId}`
    );

    return response.data;
  }

  /**
   * List dashboards
   */
  async listDashboards(): Promise<{
    dashboards: Array<{
      dashboardId: string;
      name: string;
      type: 'private' | 'shared' | 'public';
      createdAt: string;
      lastModified: string;
      owner: string;
      widgetCount: number;
    }>;
    totalCount: number;
  }> {
    const response = await this.client.makeRequest<{
      dashboards: Array<{
        dashboardId: string;
        name: string;
        type: 'private' | 'shared' | 'public';
        createdAt: string;
        lastModified: string;
        owner: string;
        widgetCount: number;
      }>;
      totalCount: number;
    }>('GET', '/analytics/dashboards');

    return response.data;
  }

  /**
   * Create report
   */
  async createReport(config: ReportConfig): Promise<{
    reportId: string;
    status: 'created' | 'scheduled' | 'generating' | 'completed' | 'failed';
    downloadUrl?: string;
    nextExecution?: string;
  }> {
    const response = await this.client.makeRequest<{
      reportId: string;
      status: 'created' | 'scheduled' | 'generating' | 'completed' | 'failed';
      downloadUrl?: string;
      nextExecution?: string;
    }>('POST', '/analytics/reports', config);

    return response.data;
  }

  /**
   * Get report status
   */
  async getReportStatus(reportId: string): Promise<{
    reportId: string;
    status: 'created' | 'scheduled' | 'generating' | 'completed' | 'failed';
    progress: number;
    downloadUrl?: string;
    generatedAt?: string;
    expiresAt?: string;
    error?: string;
  }> {
    const response = await this.client.makeRequest<{
      reportId: string;
      status: 'created' | 'scheduled' | 'generating' | 'completed' | 'failed';
      progress: number;
      downloadUrl?: string;
      generatedAt?: string;
      expiresAt?: string;
      error?: string;
    }>('GET', `/analytics/reports/${reportId}/status`);

    return response.data;
  }

  /**
   * Get fraud detection metrics
   */
  async getFraudMetrics(): Promise<{
    totalTransactions: number;
    flaggedTransactions: number;
    confirmedFraud: number;
    falsePositives: number;
    fraudRate: number;
    accuracyRate: number;
    riskDistribution: Record<string, number>;
    topRiskFactors: Array<{
      factor: string;
      weight: number;
      frequency: number;
    }>;
  }> {
    const response = await this.client.makeRequest<{
      totalTransactions: number;
      flaggedTransactions: number;
      confirmedFraud: number;
      falsePositives: number;
      fraudRate: number;
      accuracyRate: number;
      riskDistribution: Record<string, number>;
      topRiskFactors: Array<{
        factor: string;
        weight: number;
        frequency: number;
      }>;
    }>('GET', '/analytics/fraud');

    return response.data;
  }

  /**
   * Get predictive analytics
   */
  async getPredictiveAnalytics(
    model: 'transaction_volume' | 'fraud_risk' | 'system_performance' | 'cost_optimization',
    timeHorizon: '1h' | '24h' | '7d' | '30d'
  ): Promise<{
    model: string;
    predictions: Array<{
      timestamp: string;
      prediction: number;
      confidence: number;
      upperBound: number;
      lowerBound: number;
    }>;
    accuracy: {
      mape: number;
      rmse: number;
      r2: number;
    };
    insights: string[];
  }> {
    const response = await this.client.makeRequest<{
      model: string;
      predictions: Array<{
        timestamp: string;
        prediction: number;
        confidence: number;
        upperBound: number;
        lowerBound: number;
      }>;
      accuracy: {
        mape: number;
        rmse: number;
        r2: number;
      };
      insights: string[];
    }>('GET', `/analytics/predictive/${model}`, undefined, {
      headers: { 'X-Time-Horizon': timeHorizon }
    });

    return response.data;
  }

  /**
   * Get available metrics and dimensions
   */
  async getAvailableMetrics(): Promise<{
    metrics: Record<string, {
      name: string;
      description: string;
      unit: string;
      type: 'count' | 'sum' | 'avg' | 'min' | 'max' | 'percentage';
      category: string;
    }>;
    dimensions: Record<string, {
      name: string;
      description: string;
      type: 'string' | 'number' | 'date' | 'boolean';
      values?: string[];
    }>;
  }> {
    const response = await this.client.makeRequest<{
      metrics: Record<string, {
        name: string;
        description: string;
        unit: string;
        type: 'count' | 'sum' | 'avg' | 'min' | 'max' | 'percentage';
        category: string;
      }>;
      dimensions: Record<string, {
        name: string;
        description: string;
        type: 'string' | 'number' | 'date' | 'boolean';
        values?: string[];
      }>;
    }>('GET', '/analytics/metadata');

    return response.data;
  }
}