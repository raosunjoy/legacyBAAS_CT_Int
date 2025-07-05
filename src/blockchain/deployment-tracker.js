/**
 * Blockchain Deployment Tracker
 * Centralized tracking service for COBOL contract deployments across all blockchain networks
 * 
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Provides unified status tracking for Ethereum, Corda, and XRP deployments
 */

const winston = require('winston');
const { EventEmitter } = require('events');
const { v4: uuidv4 } = require('uuid');

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  defaultMeta: { service: 'deployment-tracker' }
});

/**
 * Deployment Status Enumeration
 */
const DEPLOYMENT_STATUS = {
  PENDING: 'pending',
  DEPLOYING: 'deploying',
  CONFIRMED: 'confirmed',
  FAILED: 'failed',
  CANCELLED: 'cancelled'
};

/**
 * Network Types for Deployment Tracking
 */
const TRACKED_NETWORKS = {
  ETHEREUM_L2: 'ethereum-polygon',
  CORDA: 'r3-corda',
  XRP: 'xrp-ledger',
  ALGORAND: 'algorand'
};

/**
 * Deployment Types
 */
const DEPLOYMENT_TYPES = {
  SMART_CONTRACT: 'smart_contract',
  CORDA_FLOW: 'corda_flow',
  PAYMENT_CONTRACT: 'payment_contract'
};

/**
 * Blockchain Deployment Tracker Class
 * Centralized tracking for all COBOL contract deployments
 */
class BlockchainDeploymentTracker extends EventEmitter {
  constructor(config = {}) {
    super();
    
    this.config = {
      // Tracking configuration
      maxHistorySize: config.maxHistorySize || 10000,
      statusUpdateInterval: config.statusUpdateInterval || 30000, // 30 seconds
      enablePersistence: config.enablePersistence !== false,
      
      // Network configurations
      networks: config.networks || {},
      
      ...config
    };
    
    // Active deployments tracking
    this.activeDeployments = new Map();
    this.deploymentHistory = new Map();
    
    // Network gateway references
    this.gateways = new Map();
    
    // Metrics
    this.metrics = {
      totalDeployments: 0,
      successfulDeployments: 0,
      failedDeployments: 0,
      activeDeployments: 0,
      networkDeployments: {}
    };
    
    // Status monitoring interval
    this.statusMonitoringInterval = null;
    
    logger.info('Blockchain Deployment Tracker initialized', {
      maxHistorySize: this.config.maxHistorySize,
      statusUpdateInterval: this.config.statusUpdateInterval
    });
    
    // Start status monitoring
    this.startStatusMonitoring();
  }
  
  /**
   * Register a blockchain gateway for tracking
   * @param {string} networkType - Network type identifier
   * @param {Object} gateway - Gateway instance
   */
  registerGateway(networkType, gateway) {
    this.gateways.set(networkType, gateway);
    this.metrics.networkDeployments[networkType] = {
      total: 0,
      successful: 0,
      failed: 0,
      active: 0
    };
    
    // Listen to gateway deployment events
    gateway.on('contractDeployed', (event) => {
      this.handleDeploymentSuccess(event.deploymentId, event);
    });
    
    gateway.on('contractDeploymentFailed', (event) => {
      this.handleDeploymentFailure(event.deploymentId, event);
    });
    
    gateway.on('flowDeployed', (event) => {
      this.handleDeploymentSuccess(event.deploymentId, event);
    });
    
    gateway.on('flowDeploymentFailed', (event) => {
      this.handleDeploymentFailure(event.deploymentId, event);
    });
    
    gateway.on('paymentContractDeployed', (event) => {
      this.handleDeploymentSuccess(event.deploymentId, event);
    });
    
    gateway.on('paymentContractDeploymentFailed', (event) => {
      this.handleDeploymentFailure(event.deploymentId, event);
    });
    
    logger.info('Gateway registered for deployment tracking', {
      networkType,
      gatewayType: gateway.constructor.name
    });
  }
  
  /**
   * Track a new deployment
   * @param {Object} deploymentInfo - Deployment information
   * @returns {string} Tracking ID
   */
  trackDeployment(deploymentInfo) {
    const trackingId = uuidv4();
    
    const deployment = {
      trackingId,
      deploymentId: deploymentInfo.deploymentId,
      networkType: deploymentInfo.networkType,
      deploymentType: deploymentInfo.deploymentType,
      contractName: deploymentInfo.contractName,
      bankingSystem: deploymentInfo.bankingSystem,
      sourceProgram: deploymentInfo.sourceProgram,
      status: DEPLOYMENT_STATUS.PENDING,
      startTime: new Date().toISOString(),
      lastUpdated: new Date().toISOString(),
      metadata: deploymentInfo.metadata || {},
      statusHistory: [{
        status: DEPLOYMENT_STATUS.PENDING,
        timestamp: new Date().toISOString(),
        message: 'Deployment initiated'
      }]
    };
    
    this.activeDeployments.set(trackingId, deployment);
    this.updateMetrics(deploymentInfo.networkType, 'initiated');
    
    logger.info('Deployment tracking initiated', {
      trackingId,
      deploymentId: deploymentInfo.deploymentId,
      networkType: deploymentInfo.networkType,
      contractName: deploymentInfo.contractName
    });
    
    // Emit tracking event
    this.emit('deploymentTracked', deployment);
    
    return trackingId;
  }
  
  /**
   * Update deployment status
   * @param {string} trackingId - Tracking ID
   * @param {string} status - New status
   * @param {Object} details - Additional details
   */
  updateDeploymentStatus(trackingId, status, details = {}) {
    const deployment = this.activeDeployments.get(trackingId);
    if (!deployment) {
      logger.warn('Attempted to update non-existent deployment', { trackingId });
      return;
    }
    
    const previousStatus = deployment.status;
    deployment.status = status;
    deployment.lastUpdated = new Date().toISOString();
    
    // Add to status history
    deployment.statusHistory.push({
      status,
      timestamp: new Date().toISOString(),
      message: details.message || `Status changed to ${status}`,
      details
    });
    
    // Update metadata if provided
    if (details.metadata) {
      deployment.metadata = { ...deployment.metadata, ...details.metadata };
    }
    
    logger.info('Deployment status updated', {
      trackingId,
      previousStatus,
      newStatus: status,
      deploymentId: deployment.deploymentId
    });
    
    // Emit status update event
    this.emit('deploymentStatusUpdated', {
      trackingId,
      deployment,
      previousStatus,
      newStatus: status
    });
    
    // Handle completion or failure
    if (status === DEPLOYMENT_STATUS.CONFIRMED || status === DEPLOYMENT_STATUS.FAILED) {
      this.completeDeployment(trackingId, status === DEPLOYMENT_STATUS.CONFIRMED);
    }
  }
  
  /**
   * Handle deployment success
   * @param {string} deploymentId - Deployment ID
   * @param {Object} event - Success event data
   */
  handleDeploymentSuccess(deploymentId, event) {
    const trackingId = this.findTrackingIdByDeploymentId(deploymentId);
    if (trackingId) {
      this.updateDeploymentStatus(trackingId, DEPLOYMENT_STATUS.CONFIRMED, {
        message: 'Deployment completed successfully',
        metadata: {
          transactionHash: event.txHash || event.transactionHash,
          contractAddress: event.address || event.contractAddress,
          deploymentTime: event.deploymentTime,
          result: event.result
        }
      });
    }
  }
  
  /**
   * Handle deployment failure
   * @param {string} deploymentId - Deployment ID
   * @param {Object} event - Failure event data
   */
  handleDeploymentFailure(deploymentId, event) {
    const trackingId = this.findTrackingIdByDeploymentId(deploymentId);
    if (trackingId) {
      this.updateDeploymentStatus(trackingId, DEPLOYMENT_STATUS.FAILED, {
        message: `Deployment failed: ${event.error}`,
        metadata: {
          error: event.error,
          deploymentTime: event.deploymentTime
        }
      });
    }
  }
  
  /**
   * Complete deployment tracking
   * @param {string} trackingId - Tracking ID
   * @param {boolean} success - Whether deployment was successful
   */
  completeDeployment(trackingId, success) {
    const deployment = this.activeDeployments.get(trackingId);
    if (!deployment) {
      return;
    }
    
    // Move to history
    deployment.completedAt = new Date().toISOString();
    deployment.duration = Date.now() - new Date(deployment.startTime).getTime();
    
    this.deploymentHistory.set(trackingId, deployment);
    this.activeDeployments.delete(trackingId);
    
    // Update metrics
    this.updateMetrics(deployment.networkType, success ? 'success' : 'failure');
    
    // Cleanup old history if needed
    this.cleanupHistory();
    
    logger.info('Deployment tracking completed', {
      trackingId,
      success,
      duration: deployment.duration,
      networkType: deployment.networkType
    });
  }
  
  /**
   * Get deployment status
   * @param {string} trackingId - Tracking ID
   * @returns {Object|null} Deployment information
   */
  getDeploymentStatus(trackingId) {
    return this.activeDeployments.get(trackingId) || this.deploymentHistory.get(trackingId) || null;
  }
  
  /**
   * Get deployment by deployment ID
   * @param {string} deploymentId - Deployment ID
   * @returns {Object|null} Deployment information
   */
  getDeploymentByDeploymentId(deploymentId) {
    // Search active deployments
    for (const deployment of this.activeDeployments.values()) {
      if (deployment.deploymentId === deploymentId) {
        return deployment;
      }
    }
    
    // Search history
    for (const deployment of this.deploymentHistory.values()) {
      if (deployment.deploymentId === deploymentId) {
        return deployment;
      }
    }
    
    return null;
  }
  
  /**
   * List active deployments
   * @param {Object} filters - Optional filters
   * @returns {Array} Active deployments
   */
  listActiveDeployments(filters = {}) {
    const deployments = Array.from(this.activeDeployments.values());
    return this.applyFilters(deployments, filters);
  }
  
  /**
   * List deployment history
   * @param {Object} filters - Optional filters
   * @returns {Array} Historical deployments
   */
  listDeploymentHistory(filters = {}) {
    const deployments = Array.from(this.deploymentHistory.values());
    return this.applyFilters(deployments, filters);
  }
  
  /**
   * Apply filters to deployment list
   * @param {Array} deployments - Deployments to filter
   * @param {Object} filters - Filters to apply
   * @returns {Array} Filtered deployments
   */
  applyFilters(deployments, filters) {
    let filtered = deployments;
    
    if (filters.networkType) {
      filtered = filtered.filter(d => d.networkType === filters.networkType);
    }
    
    if (filters.status) {
      filtered = filtered.filter(d => d.status === filters.status);
    }
    
    if (filters.bankingSystem) {
      filtered = filtered.filter(d => d.bankingSystem === filters.bankingSystem);
    }
    
    if (filters.deploymentType) {
      filtered = filtered.filter(d => d.deploymentType === filters.deploymentType);
    }
    
    if (filters.limit) {
      filtered = filtered.slice(0, filters.limit);
    }
    
    return filtered.sort((a, b) => new Date(b.startTime) - new Date(a.startTime));
  }
  
  /**
   * Start automatic status monitoring
   */
  startStatusMonitoring() {
    if (this.statusMonitoringInterval) {
      clearInterval(this.statusMonitoringInterval);
    }
    
    this.statusMonitoringInterval = setInterval(async () => {
      await this.updateActiveDeploymentStatuses();
    }, this.config.statusUpdateInterval);
    
    logger.info('Status monitoring started', {
      interval: this.config.statusUpdateInterval
    });
  }
  
  /**
   * Update statuses of all active deployments
   */
  async updateActiveDeploymentStatuses() {
    const activeDeployments = Array.from(this.activeDeployments.values());
    
    for (const deployment of activeDeployments) {
      try {
        await this.updateSingleDeploymentStatus(deployment);
      } catch (error) {
        logger.error('Failed to update deployment status', {
          trackingId: deployment.trackingId,
          error: error.message
        });
      }
    }
  }
  
  /**
   * Update status of a single deployment
   * @param {Object} deployment - Deployment to update
   */
  async updateSingleDeploymentStatus(deployment) {
    const gateway = this.gateways.get(deployment.networkType);
    if (!gateway) {
      return;
    }
    
    let statusResult;
    
    try {
      // Call appropriate status method based on deployment type
      if (deployment.deploymentType === DEPLOYMENT_TYPES.SMART_CONTRACT) {
        statusResult = await gateway.getDeploymentStatus(deployment.deploymentId);
      } else if (deployment.deploymentType === DEPLOYMENT_TYPES.CORDA_FLOW) {
        statusResult = await gateway.getFlowDeploymentStatus(deployment.deploymentId);
      } else if (deployment.deploymentType === DEPLOYMENT_TYPES.PAYMENT_CONTRACT) {
        statusResult = await gateway.getPaymentContractDeploymentStatus(deployment.deploymentId);
      }
      
      if (statusResult && statusResult.status !== deployment.status) {
        this.updateDeploymentStatus(deployment.trackingId, statusResult.status, {
          message: `Status updated from network: ${statusResult.status}`,
          metadata: statusResult
        });
      }
      
    } catch (error) {
      // Network status check failed, but don't mark deployment as failed yet
      logger.warn('Network status check failed', {
        trackingId: deployment.trackingId,
        networkType: deployment.networkType,
        error: error.message
      });
    }
  }
  
  /**
   * Find tracking ID by deployment ID
   * @param {string} deploymentId - Deployment ID
   * @returns {string|null} Tracking ID
   */
  findTrackingIdByDeploymentId(deploymentId) {
    for (const [trackingId, deployment] of this.activeDeployments) {
      if (deployment.deploymentId === deploymentId) {
        return trackingId;
      }
    }
    return null;
  }
  
  /**
   * Update tracking metrics
   * @param {string} networkType - Network type
   * @param {string} event - Event type (initiated, success, failure)
   */
  updateMetrics(networkType, event) {
    this.metrics.totalDeployments++;
    this.metrics.activeDeployments = this.activeDeployments.size;
    
    if (!this.metrics.networkDeployments[networkType]) {
      this.metrics.networkDeployments[networkType] = { total: 0, successful: 0, failed: 0, active: 0 };
    }
    
    const networkMetrics = this.metrics.networkDeployments[networkType];
    
    if (event === 'initiated') {
      networkMetrics.total++;
      networkMetrics.active++;
    } else if (event === 'success') {
      this.metrics.successfulDeployments++;
      networkMetrics.successful++;
      networkMetrics.active--;
    } else if (event === 'failure') {
      this.metrics.failedDeployments++;
      networkMetrics.failed++;
      networkMetrics.active--;
    }
  }
  
  /**
   * Cleanup old deployment history
   */
  cleanupHistory() {
    if (this.deploymentHistory.size > this.config.maxHistorySize) {
      const entries = Array.from(this.deploymentHistory.entries());
      const sorted = entries.sort((a, b) => new Date(a[1].completedAt) - new Date(b[1].completedAt));
      
      // Remove oldest entries
      const toRemove = sorted.slice(0, entries.length - this.config.maxHistorySize);
      for (const [trackingId] of toRemove) {
        this.deploymentHistory.delete(trackingId);
      }
      
      logger.info('Deployment history cleaned up', {
        removed: toRemove.length,
        remaining: this.deploymentHistory.size
      });
    }
  }
  
  /**
   * Get tracking metrics
   * @returns {Object} Tracking metrics
   */
  getMetrics() {
    return {
      ...this.metrics,
      successRate: this.metrics.totalDeployments > 0 
        ? this.metrics.successfulDeployments / this.metrics.totalDeployments 
        : 0,
      activeDeployments: this.activeDeployments.size,
      historicalDeployments: this.deploymentHistory.size
    };
  }
  
  /**
   * Stop status monitoring and cleanup
   */
  async cleanup() {
    if (this.statusMonitoringInterval) {
      clearInterval(this.statusMonitoringInterval);
      this.statusMonitoringInterval = null;
    }
    
    logger.info('Deployment tracker cleaned up');
  }
}

module.exports = {
  BlockchainDeploymentTracker,
  DEPLOYMENT_STATUS,
  TRACKED_NETWORKS,
  DEPLOYMENT_TYPES
};