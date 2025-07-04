/**
 * Compliance Service
 * AML/KYC screening and regulatory compliance
 */

import { LegacyBaaSClient } from '../client/LegacyBaaSClient';
import { ApiResponse } from '../types';

export interface ComplianceScreeningRequest {
  type: 'individual' | 'entity' | 'transaction';
  data: {
    // Individual screening
    firstName?: string;
    lastName?: string;
    dateOfBirth?: string;
    nationality?: string;
    
    // Entity screening
    entityName?: string;
    registrationNumber?: string;
    jurisdiction?: string;
    
    // Transaction screening
    amount?: number;
    currency?: string;
    fromCountry?: string;
    toCountry?: string;
    transactionType?: string;
    
    // Common fields
    address?: {
      street: string;
      city: string;
      state?: string;
      postalCode: string;
      country: string;
    };
    identificationNumbers?: Array<{
      type: 'passport' | 'national_id' | 'ssn' | 'tax_id';
      number: string;
      issuingCountry?: string;
    }>;
  };
  screeningLists?: string[];
  riskTolerance?: 'low' | 'medium' | 'high';
  additionalData?: Record<string, any>;
}

export interface ComplianceScreeningResult {
  screeningId: string;
  status: 'clear' | 'potential_match' | 'confirmed_match' | 'error';
  riskScore: number;
  riskLevel: 'low' | 'medium' | 'high' | 'critical';
  matches: Array<{
    matchId: string;
    listName: string;
    matchType: 'exact' | 'fuzzy' | 'phonetic';
    confidence: number;
    matchedFields: string[];
    details: {
      name: string;
      aliases?: string[];
      dateOfBirth?: string;
      nationality?: string;
      listingReason: string;
      listingDate: string;
      source: string;
      additionalInfo?: Record<string, any>;
    };
  }>;
  recommendations: Array<{
    action: 'approve' | 'review' | 'decline' | 'escalate';
    reason: string;
    priority: 'low' | 'medium' | 'high';
  }>;
  processingTime: number;
  timestamp: string;
}

export interface KycDocument {
  documentId: string;
  type: 'passport' | 'drivers_license' | 'national_id' | 'utility_bill' | 'bank_statement' | 'other';
  status: 'pending' | 'verified' | 'rejected' | 'expired';
  uploadedAt: string;
  verifiedAt?: string;
  expiryDate?: string;
  extractedData?: Record<string, any>;
  verificationResults?: {
    authentic: boolean;
    confidence: number;
    checks: Array<{
      checkType: string;
      result: 'pass' | 'fail' | 'warning';
      details: string;
    }>;
  };
}

export interface ComplianceCase {
  caseId: string;
  type: 'aml' | 'kyc' | 'sanctions' | 'fraud' | 'other';
  status: 'open' | 'under_review' | 'closed' | 'escalated';
  priority: 'low' | 'medium' | 'high' | 'critical';
  customerId?: string;
  transactionId?: string;
  description: string;
  createdAt: string;
  updatedAt: string;
  assignedTo?: string;
  resolution?: {
    action: 'approved' | 'rejected' | 'restricted' | 'monitored';
    reason: string;
    resolvedAt: string;
    resolvedBy: string;
  };
  timeline: Array<{
    timestamp: string;
    action: string;
    description: string;
    performedBy: string;
  }>;
}

export interface CompliancePolicy {
  policyId: string;
  name: string;
  type: 'screening' | 'transaction_monitoring' | 'kyc' | 'reporting';
  isActive: boolean;
  rules: Array<{
    ruleId: string;
    name: string;
    condition: string;
    action: 'alert' | 'block' | 'review' | 'report';
    parameters: Record<string, any>;
  }>;
  createdAt: string;
  updatedAt: string;
}

export class ComplianceService {
  constructor(private client: LegacyBaaSClient) {}

  /**
   * Screen individual, entity or transaction
   */
  async screenSubject(request: ComplianceScreeningRequest): Promise<ComplianceScreeningResult> {
    const response = await this.client.makeRequest<ComplianceScreeningResult>(
      'POST',
      '/compliance/screen',
      request
    );

    return response.data;
  }

  /**
   * Get screening result
   */
  async getScreeningResult(screeningId: string): Promise<ComplianceScreeningResult> {
    const response = await this.client.makeRequest<ComplianceScreeningResult>(
      'GET',
      `/compliance/screenings/${screeningId}`
    );

    return response.data;
  }

  /**
   * Bulk screening
   */
  async bulkScreen(requests: ComplianceScreeningRequest[]): Promise<{
    batchId: string;
    totalRequests: number;
    status: 'processing' | 'completed' | 'failed';
    results?: ComplianceScreeningResult[];
    progress?: number;
  }> {
    const response = await this.client.makeRequest<{
      batchId: string;
      totalRequests: number;
      status: 'processing' | 'completed' | 'failed';
      results?: ComplianceScreeningResult[];
      progress?: number;
    }>('POST', '/compliance/screen/bulk', { requests });

    return response.data;
  }

  /**
   * Get bulk screening status
   */
  async getBulkScreeningStatus(batchId: string): Promise<{
    batchId: string;
    status: 'processing' | 'completed' | 'failed';
    totalRequests: number;
    processedRequests: number;
    failedRequests: number;
    progress: number;
    results?: ComplianceScreeningResult[];
    downloadUrl?: string;
  }> {
    const response = await this.client.makeRequest<{
      batchId: string;
      status: 'processing' | 'completed' | 'failed';
      totalRequests: number;
      processedRequests: number;
      failedRequests: number;
      progress: number;
      results?: ComplianceScreeningResult[];
      downloadUrl?: string;
    }>('GET', `/compliance/screen/bulk/${batchId}`);

    return response.data;
  }

  /**
   * Upload KYC document
   */
  async uploadDocument(
    customerId: string,
    documentType: string,
    file: Buffer,
    fileName: string,
    metadata?: Record<string, any>
  ): Promise<{
    documentId: string;
    uploadUrl: string;
    status: 'uploaded';
    processingStarted: boolean;
  }> {
    const response = await this.client.makeRequest<{
      documentId: string;
      uploadUrl: string;
      status: 'uploaded';
      processingStarted: boolean;
    }>('POST', '/compliance/documents', {
      customerId,
      documentType,
      fileName,
      metadata,
      fileSize: file.length,
      fileData: file.toString('base64')
    });

    return response.data;
  }

  /**
   * Get document verification status
   */
  async getDocumentStatus(documentId: string): Promise<KycDocument> {
    const response = await this.client.makeRequest<KycDocument>(
      'GET',
      `/compliance/documents/${documentId}`
    );

    return response.data;
  }

  /**
   * List KYC documents
   */
  async listDocuments(
    customerId: string,
    filters: {
      documentType?: string;
      status?: string;
      limit?: number;
      offset?: number;
    } = {}
  ): Promise<{
    documents: KycDocument[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
  }> {
    const response = await this.client.makeRequest<{
      documents: KycDocument[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
    }>('GET', `/compliance/customers/${customerId}/documents`, undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Create compliance case
   */
  async createCase(caseData: {
    type: 'aml' | 'kyc' | 'sanctions' | 'fraud' | 'other';
    priority: 'low' | 'medium' | 'high' | 'critical';
    customerId?: string;
    transactionId?: string;
    description: string;
    assignedTo?: string;
    metadata?: Record<string, any>;
  }): Promise<{
    caseId: string;
    status: 'open';
    createdAt: string;
  }> {
    const response = await this.client.makeRequest<{
      caseId: string;
      status: 'open';
      createdAt: string;
    }>('POST', '/compliance/cases', caseData);

    return response.data;
  }

  /**
   * Get compliance case
   */
  async getCase(caseId: string): Promise<ComplianceCase> {
    const response = await this.client.makeRequest<ComplianceCase>(
      'GET',
      `/compliance/cases/${caseId}`
    );

    return response.data;
  }

  /**
   * Update compliance case
   */
  async updateCase(
    caseId: string,
    updates: {
      status?: 'open' | 'under_review' | 'closed' | 'escalated';
      priority?: 'low' | 'medium' | 'high' | 'critical';
      assignedTo?: string;
      resolution?: {
        action: 'approved' | 'rejected' | 'restricted' | 'monitored';
        reason: string;
      };
      notes?: string;
    }
  ): Promise<{
    caseId: string;
    status: 'updated';
    updatedAt: string;
  }> {
    const response = await this.client.makeRequest<{
      caseId: string;
      status: 'updated';
      updatedAt: string;
    }>('PUT', `/compliance/cases/${caseId}`, updates);

    return response.data;
  }

  /**
   * List compliance cases
   */
  async listCases(filters: {
    type?: string;
    status?: string;
    priority?: string;
    assignedTo?: string;
    customerId?: string;
    startDate?: string;
    endDate?: string;
    limit?: number;
    offset?: number;
  } = {}): Promise<{
    cases: ComplianceCase[];
    pagination: {
      total: number;
      limit: number;
      offset: number;
      hasMore: boolean;
    };
    summary: {
      totalCases: number;
      openCases: number;
      closedCases: number;
      escalatedCases: number;
    };
  }> {
    const response = await this.client.makeRequest<{
      cases: ComplianceCase[];
      pagination: {
        total: number;
        limit: number;
        offset: number;
        hasMore: boolean;
      };
      summary: {
        totalCases: number;
        openCases: number;
        closedCases: number;
        escalatedCases: number;
      };
    }>('GET', '/compliance/cases', undefined, {
      headers: { 'X-Query-Params': JSON.stringify(filters) }
    });

    return response.data;
  }

  /**
   * Get compliance policies
   */
  async getPolicies(): Promise<{
    policies: CompliancePolicy[];
    totalPolicies: number;
    activePolicies: number;
  }> {
    const response = await this.client.makeRequest<{
      policies: CompliancePolicy[];
      totalPolicies: number;
      activePolicies: number;
    }>('GET', '/compliance/policies');

    return response.data;
  }

  /**
   * Create compliance policy
   */
  async createPolicy(policy: Omit<CompliancePolicy, 'policyId' | 'createdAt' | 'updatedAt'>): Promise<{
    policyId: string;
    status: 'created';
    createdAt: string;
  }> {
    const response = await this.client.makeRequest<{
      policyId: string;
      status: 'created';
      createdAt: string;
    }>('POST', '/compliance/policies', policy);

    return response.data;
  }

  /**
   * Get compliance metrics
   */
  async getComplianceMetrics(): Promise<{
    totalScreenings: number;
    riskDistribution: Record<string, number>;
    matchRate: number;
    falsePositiveRate: number;
    avgProcessingTime: number;
    complianceScore: number;
    caseMetrics: {
      totalCases: number;
      openCases: number;
      avgResolutionTime: number;
      caseTypes: Record<string, number>;
    };
    documentMetrics: {
      totalDocuments: number;
      verifiedDocuments: number;
      rejectedDocuments: number;
      verificationRate: number;
    };
  }> {
    const response = await this.client.makeRequest<{
      totalScreenings: number;
      riskDistribution: Record<string, number>;
      matchRate: number;
      falsePositiveRate: number;
      avgProcessingTime: number;
      complianceScore: number;
      caseMetrics: {
        totalCases: number;
        openCases: number;
        avgResolutionTime: number;
        caseTypes: Record<string, number>;
      };
      documentMetrics: {
        totalDocuments: number;
        verifiedDocuments: number;
        rejectedDocuments: number;
        verificationRate: number;
      };
    }>('GET', '/compliance/metrics');

    return response.data;
  }

  /**
   * Get available screening lists
   */
  async getScreeningLists(): Promise<{
    lists: Array<{
      listId: string;
      name: string;
      type: 'sanctions' | 'pep' | 'watchlist' | 'adverse_media';
      source: string;
      recordCount: number;
      lastUpdated: string;
      isActive: boolean;
    }>;
    totalLists: number;
    activeLists: number;
    lastGlobalUpdate: string;
  }> {
    const response = await this.client.makeRequest<{
      lists: Array<{
        listId: string;
        name: string;
        type: 'sanctions' | 'pep' | 'watchlist' | 'adverse_media';
        source: string;
        recordCount: number;
        lastUpdated: string;
        isActive: boolean;
      }>;
      totalLists: number;
      activeLists: number;
      lastGlobalUpdate: string;
    }>('GET', '/compliance/lists');

    return response.data;
  }

  /**
   * Generate compliance report
   */
  async generateReport(
    reportType: 'screening_summary' | 'case_summary' | 'policy_effectiveness' | 'risk_analysis',
    timeRange: { start: string; end: string },
    filters?: Record<string, any>
  ): Promise<{
    reportId: string;
    status: 'generating' | 'completed' | 'failed';
    downloadUrl?: string;
    estimatedCompletion?: string;
  }> {
    const response = await this.client.makeRequest<{
      reportId: string;
      status: 'generating' | 'completed' | 'failed';
      downloadUrl?: string;
      estimatedCompletion?: string;
    }>('POST', '/compliance/reports', {
      reportType,
      timeRange,
      filters: filters || {}
    });

    return response.data;
  }
}