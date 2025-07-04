/**
 * Compliance type definitions
 */

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