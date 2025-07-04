/**
 * API Routes Index
 * LegacyBAAS Platform
 * 
 * Central routing configuration for all API endpoints
 */

const express = require('express');
const router = express.Router();

// Import route modules
const cobolRoutes = require('./cobol-routes');

// Mount route modules
router.use('/cobol', cobolRoutes);

// API root endpoint
router.get('/', (req, res) => {
  res.json({
    message: 'LegacyBAAS Platform API',
    version: '1.0.0',
    endpoints: {
      cobol: '/api/v1/cobol',
      banking: '/api/v1/banking',
      routing: '/api/v1/routing',
      compliance: '/api/v1/compliance',
      analytics: '/api/v1/analytics'
    },
    documentation: '/docs/api',
    health: '/health',
    timestamp: new Date().toISOString()
  });
});

module.exports = router;