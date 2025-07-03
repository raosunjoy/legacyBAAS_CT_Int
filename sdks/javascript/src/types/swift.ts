/**
 * SWIFT Message Types and Interfaces
 */

/**
 * SWIFT Message Base Interface
 */
export interface SwiftMessage {
  type: string;
  content: any;
  messageId: string;
  timestamp: string;
  priority?: 'low' | 'normal' | 'high' | 'urgent';
}

/**
 * SWIFT Processing Options
 */
export interface SwiftProcessingOptions {
  validateOnly?: boolean;
  asyncProcessing?: boolean;
  priorityLevel?: 'low' | 'normal' | 'high' | 'urgent';
  enableBlockchain?: boolean;
  targetNetwork?: string;
  complianceCheck?: boolean;
}

/**
 * SWIFT Processing Result
 */
export interface SwiftProcessingResult {
  messageId: string;
  status: 'pending' | 'processed' | 'failed' | 'rejected';
  processedAt: string;
  blockchainTransactionId?: string;
  blockchainNetwork?: string;
  fees?: {
    processing: number;
    blockchain: number;
    total: number;
    currency: string;
  };
  compliance?: {
    amlCheck: boolean;
    kycVerified: boolean;
    sanctionsCheck: boolean;
  };
  errors?: string[];
  warnings?: string[];
}

/**
 * SWIFT Validation Result
 */
export interface SwiftValidationResult {
  isValid: boolean;
  messageType: string;
  errors: string[];
  warnings: string[];
  parsedFields: Record<string, any>;
}

/**
 * MT103 Customer Credit Transfer
 */
export interface MT103Message {
  transactionReference: string;
  bankOperationCode?: string;
  instructionCode?: string;
  valueDate: string;
  currency: string;
  amount: number;
  orderingCustomer: {
    account: string;
    name: string;
    address?: string[];
  };
  senderCorrespondent?: {
    bic: string;
    account?: string;
  };
  receiverCorrespondent?: {
    bic: string;
    account?: string;
  };
  beneficiaryCustomer: {
    account: string;
    name: string;
    address?: string[];
  };
  remittanceInformation?: string;
  detailsOfCharges: 'OUR' | 'BEN' | 'SHA';
  senderToReceiverInformation?: string;
}

/**
 * MT202 Financial Institution Transfer
 */
export interface MT202Message {
  transactionReference: string;
  relatedReference?: string;
  valueDate: string;
  currency: string;
  amount: number;
  orderingInstitution: {
    bic: string;
    account?: string;
  };
  senderCorrespondent?: {
    bic: string;
    account?: string;
  };
  receiverCorrespondent?: {
    bic: string;
    account?: string;
  };
  beneficiaryInstitution: {
    bic: string;
    account?: string;
  };
  remittanceInformation?: string;
}

/**
 * MT700 Letter of Credit
 */
export interface MT700Message {
  creditNumber: string;
  issueDate: string;
  expiryDate: string;
  expiryPlace?: string;
  applicant: {
    name: string;
    address: string[];
  };
  beneficiary: {
    name: string;
    address: string[];
  };
  currency: string;
  amount: number;
  tolerances?: {
    plus?: number;
    minus?: number;
  };
  availableWith: {
    bic: string;
    type: 'payment' | 'acceptance' | 'negotiation';
  };
  draftsAt?: string;
  draweeBank?: {
    bic: string;
    name?: string;
  };
  partialShipments: 'allowed' | 'not_allowed';
  transhipment: 'allowed' | 'not_allowed';
  shipmentFrom: string;
  shipmentTo: string;
  latestShipmentDate?: string;
  descriptionOfGoods: string;
  documentsRequired: string[];
  additionalConditions?: string;
  presentationPeriod?: number;
  confirmationInstructions?: string;
  chargesFor: 'applicant' | 'beneficiary';
}

/**
 * SWIFT Network Information
 */
export interface SwiftNetworkInfo {
  bic: string;
  institutionName: string;
  country: string;
  city: string;
  branch?: string;
  services: string[];
  connectivity: 'live' | 'test' | 'offline';
}