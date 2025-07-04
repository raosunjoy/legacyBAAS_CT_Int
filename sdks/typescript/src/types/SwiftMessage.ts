/**
 * SWIFT Message type definitions
 */

export interface SwiftMessage {
  messageId: string;
  messageType: string;
  content: string;
  sender: string;
  receiver: string;
  timestamp: string;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
  metadata?: Record<string, any>;
}

export interface SwiftProcessingOptions {
  validateOnly?: boolean;
  asyncProcessing?: boolean;
  priorityLevel?: 'low' | 'normal' | 'high' | 'urgent';
  enableBlockchain?: boolean;
  routingPreference?: 'fastest' | 'cheapest' | 'most_secure';
}

export interface SwiftProcessingResult {
  messageId: string;
  status: 'processed' | 'failed' | 'pending' | 'routing';
  blockchainTxId?: string;
  routingDecision?: {
    selectedNetwork: string;
    estimatedFees: number;
    estimatedTime: string;
    confidence: number;
  };
  validationResult: {
    isValid: boolean;
    errors: string[];
    warnings: string[];
  };
  processingTime: number;
  timestamp: string;
}

export interface MT103Data {
  transactionReference: string;
  bankOperationCode?: string;
  instructionCode?: string;
  senderToReceiverInfo?: string;
  orderingCustomer: string;
  orderingInstitution?: string;
  sendersCorrespondent?: string;
  receiversCorrespondent?: string;
  intermediaryInstitution?: string;
  accountWithInstitution?: string;
  beneficiaryCustomer: string;
  remittanceInformation?: string;
  amount: string;
  currency: string;
  valueDate: string;
}

export interface MT202Data {
  transactionReference: string;
  relatedReference?: string;
  timeIndication?: string;
  sendersCorrespondent?: string;
  receiversCorrespondent?: string;
  intermediaryInstitution?: string;
  accountWithInstitution: string;
  beneficiaryInstitution: string;
  amount: string;
  currency: string;
  valueDate: string;
}

export interface MT700Data {
  creditNumber: string;
  dateOfIssue: string;
  applicantBank: string;
  beneficiary: string;
  amount: string;
  currency: string;
  expiryDate: string;
  expiryPlace: string;
  availableWith: string;
  documentaryRequirements: string;
  additionalConditions?: string;
}