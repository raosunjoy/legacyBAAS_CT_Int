/**
 * Jest Configuration for Banking Legacy-to-Blockchain B2BaaS Platform
 * Enforcing 100% test coverage across all modules
 */

module.exports = {
  // Test environment
  testEnvironment: 'node',
  
  // Root directory
  rootDir: '.',
  
  // Test directories
  testMatch: [
    '<rootDir>/tests/**/*.test.js',
    '<rootDir>/src/**/*.test.js',
    '<rootDir>/tests/**/*.spec.js'
  ],
  
  // Setup files
  setupFilesAfterEnv: ['<rootDir>/tests/setup.js'],
  
  // Coverage configuration - 100% enforcement
  collectCoverage: true,
  collectCoverageFrom: [
    'src/**/*.js',
    '!src/**/*.test.js',
    '!src/**/*.spec.js',
    '!**/node_modules/**',
    '!**/coverage/**'
  ],
  
  // 100% coverage thresholds - NON-NEGOTIABLE
  coverageThreshold: {
    global: {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    },
    // Per-module enforcement
    './src/router/': {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    },
    './src/adapters/': {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    },
    './src/blockchain/': {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    },
    './src/compliance/': {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    },
    './src/api/': {
      branches: 100,
      functions: 100,
      lines: 100,
      statements: 100
    }
  },
  
  // Coverage reporters
  coverageReporters: [
    'text',
    'text-summary',
    'html',
    'lcov',
    'clover',
    'json'
  ],
  
  // Coverage directory
  coverageDirectory: 'coverage',
  
  // Transform configuration
  transform: {
    '^.+\\.js$': 'babel-jest'
  },
  
  // Module file extensions
  moduleFileExtensions: ['js', 'json', 'node'],
  
  // Test timeout (blockchain operations can be slow)
  testTimeout: 30000,
  
  // Verbose output
  verbose: true,
  
  // Clear mocks between tests
  clearMocks: true,
  
  // Restore mocks after each test
  restoreMocks: true,
  
  // Global variables for tests
  globals: {
    NODE_ENV: 'test'
  },
  
  // Error handling
  errorOnDeprecated: true,
  
  // Force exit after tests complete
  forceExit: true,
  
  // Detect open handles
  detectOpenHandles: true,
  
  // Detect leaked async operations
  detectLeaks: true,
  
  // Test name pattern
  testNamePattern: '^((?!integration).)*$', // Exclude integration tests by default
  
  // Reporters
  reporters: [
    'default',
    ['jest-junit', {
      outputDirectory: 'coverage',
      outputName: 'junit.xml'
    }]
  ]
};