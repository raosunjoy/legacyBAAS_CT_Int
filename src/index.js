/**
 * Banking Legacy-to-Blockchain B2BaaS Platform
 * Main application entry point
 */

const express = require('express');
const cors = require('cors');
const helmet = require('helmet');
const dotenv = require('dotenv');
const winston = require('winston');

// Load environment configuration
dotenv.config();

// Configure logger
const logger = winston.createLogger({
  level: process.env.LOG_LEVEL || 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.errors({ stack: true }),
    winston.format.json()
  ),
  defaultMeta: { service: 'legacy-baas' },
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' }),
    new winston.transports.Console({
      format: winston.format.simple()
    })
  ]
});

// Initialize Express app
const app = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(helmet());
app.use(cors());
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true }));

// Request logging
app.use((req, res, next) => {
  logger.info(`${req.method} ${req.path}`, {
    method: req.method,
    url: req.url,
    ip: req.ip,
    userAgent: req.get('User-Agent')
  });
  next();
});

// Health check endpoint
app.get('/health', (req, res) => {
  res.status(200).json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    version: process.env.npm_package_version || '0.1.0',
    environment: process.env.NODE_ENV || 'development'
  });
});

// Import API routes
const apiRoutes = require('./api/routes/index');

// Mount API routes
app.use('/api/v1', apiRoutes);

// API status endpoint (updated to reflect COBOL transpiler)
app.get('/api/v1/status', (req, res) => {
  res.json({
    message: 'Banking Legacy-to-Blockchain B2BaaS Platform',
    version: '1.0.0',
    components: {
      smartRouter: 'operational',
      legacyAdapters: 'configured',
      blockchainGateways: 'connected',
      complianceEngine: 'active',
      cobolTranspiler: 'operational'
    },
    supportedNetworks: [
      'XRP Ledger',
      'R3 Corda',
      'Ethereum L2 (Polygon)',
      'Algorand'
    ],
    supportedBankingSystems: [
      'FIS Systematics',
      'Fiserv DNA',
      'Temenos Transact',
      'TCS BaNCS'
    ],
    useCases: [
      'Cross-Border Payments',
      'Trade Finance',
      'Tokenized Deposits',
      'Compliance Automation',
      'CBDC Interoperability',
      'COBOL Modernization'
    ],
    endpoints: {
      cobol: '/api/v1/cobol',
      banking: '/api/v1/banking',
      routing: '/api/v1/routing',
      compliance: '/api/v1/compliance',
      analytics: '/api/v1/analytics'
    }
  });
});

// Error handling middleware
app.use((err, req, res, next) => {
  logger.error('Unhandled error:', err);
  res.status(500).json({
    error: 'Internal server error',
    message: process.env.NODE_ENV === 'development' ? err.message : 'Something went wrong'
  });
});

// 404 handler
app.use('*', (req, res) => {
  res.status(404).json({
    error: 'Not found',
    message: 'The requested resource was not found'
  });
});

// Graceful shutdown
process.on('SIGTERM', () => {
  logger.info('SIGTERM received, shutting down gracefully');
  process.exit(0);
});

process.on('SIGINT', () => {
  logger.info('SIGINT received, shutting down gracefully');
  process.exit(0);
});

// Start server only if this file is run directly (not imported)
if (require.main === module) {
  app.listen(PORT, () => {
    logger.info(`🚀 Legacy-to-Blockchain B2BaaS Platform started on port ${PORT}`);
    logger.info(`📊 Health check: http://localhost:${PORT}/health`);
    logger.info(`🔧 API status: http://localhost:${PORT}/api/v1/status`);
    logger.info(`🌐 Environment: ${process.env.NODE_ENV || 'development'}`);
  });
}

module.exports = app;