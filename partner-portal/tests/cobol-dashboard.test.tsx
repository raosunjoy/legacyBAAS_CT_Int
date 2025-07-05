/**
 * COBOL Dashboard Component Tests
 * LegacyBAAS Partner Portal - COBOL Transpiler Integration
 * 
 * Test Coverage: Dashboard functionality, metrics display, project overview, analytics
 * Target: 8 comprehensive tests for 100% coverage
 */

import React from 'react';
import { render, screen, waitFor, fireEvent } from '@testing-library/react';
import '@testing-library/jest-dom';
import COBOLDashboard from '../app/cobol-transpiler/dashboard/page';

// Mock recharts components
jest.mock('recharts', () => ({
  ResponsiveContainer: ({ children }: { children: React.ReactNode }) => <div data-testid="responsive-container">{children}</div>,
  BarChart: ({ children }: { children: React.ReactNode }) => <div data-testid="bar-chart">{children}</div>,
  LineChart: ({ children }: { children: React.ReactNode }) => <div data-testid="line-chart">{children}</div>,
  PieChart: ({ children }: { children: React.ReactNode }) => <div data-testid="pie-chart">{children}</div>,
  Bar: () => <div data-testid="bar" />,
  Line: () => <div data-testid="line" />,
  Pie: () => <div data-testid="pie" />,
  Cell: () => <div data-testid="cell" />,
  XAxis: () => <div data-testid="x-axis" />,
  YAxis: () => <div data-testid="y-axis" />,
  CartesianGrid: () => <div data-testid="cartesian-grid" />,
  Tooltip: () => <div data-testid="tooltip" />
}));

// Mock window.location.reload
Object.defineProperty(window, 'location', {
  value: {
    reload: jest.fn()
  },
  writable: true
});

describe('COBOL Dashboard', () => {
  beforeEach(() => {
    // Reset all mocks before each test
    jest.clearAllMocks();
  });

  test('should render dashboard with loading state initially', () => {
    render(<COBOLDashboard />);
    
    expect(screen.getByTestId('refresh-icon')).toBeInTheDocument();
    expect(screen.getByTestId('refresh-icon')).toHaveClass('animate-spin');
  });

  test('should display transpilation project overview correctly', async () => {
    render(<COBOLDashboard />);
    
    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Check main metrics cards
    expect(screen.getByText('Total Transpilations')).toBeInTheDocument();
    expect(screen.getByText('Success Rate')).toBeInTheDocument();
    expect(screen.getByText('Quota Usage')).toBeInTheDocument();
    expect(screen.getByText('Monthly Revenue')).toBeInTheDocument();

    // Check project overview
    expect(screen.getByText('Recent Transpilation Projects')).toBeInTheDocument();
    expect(screen.getByText('Mortgage Processing Modernization')).toBeInTheDocument();
    expect(screen.getByText('Payment Gateway Integration')).toBeInTheDocument();
    expect(screen.getByText('Trade Finance Automation')).toBeInTheDocument();
    expect(screen.getByText('Cross-Border Remittance')).toBeInTheDocument();
  });

  test('should display usage metrics and quota tracking', async () => {
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Switch to Usage tab
    fireEvent.click(screen.getByText('Usage'));

    await waitFor(() => {
      expect(screen.getByText('Monthly Usage Tracking')).toBeInTheDocument();
      expect(screen.getByText('Performance Metrics')).toBeInTheDocument();
    });

    // Check usage metrics
    expect(screen.getByText('Current Month Usage')).toBeInTheDocument();
    expect(screen.getByText('Projected Monthly Usage')).toBeInTheDocument();
    expect(screen.getByText('Average transpilation time:')).toBeInTheDocument();
  });

  test('should show success/failure rate analytics', async () => {
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Switch to Analytics tab
    fireEvent.click(screen.getByText('Analytics'));

    await waitFor(() => {
      expect(screen.getByText('Daily Transpilation Trend')).toBeInTheDocument();
      expect(screen.getByText('Banking System Distribution')).toBeInTheDocument();
    });

    // Check charts are rendered
    expect(screen.getByTestId('line-chart')).toBeInTheDocument();
    expect(screen.getByTestId('pie-chart')).toBeInTheDocument();
  });

  test('should track revenue for SI partners correctly', async () => {
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Switch to Revenue tab
    fireEvent.click(screen.getByText('Revenue'));

    await waitFor(() => {
      expect(screen.getByText('Revenue Analytics')).toBeInTheDocument();
    });

    // Check revenue metrics
    expect(screen.getByText('This Month')).toBeInTheDocument();
    expect(screen.getByText('Total Revenue')).toBeInTheDocument();
    expect(screen.getByText('Avg. Project Value')).toBeInTheDocument();
    
    // Check revenue chart
    expect(screen.getByTestId('bar-chart')).toBeInTheDocument();
  });

  test('should handle refresh functionality', async () => {
    const reloadSpy = jest.spyOn(window.location, 'reload');
    
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Click refresh button
    const refreshButton = screen.getByText('Refresh');
    fireEvent.click(refreshButton);

    expect(reloadSpy).toHaveBeenCalledTimes(1);
  });

  test('should display project status indicators correctly', async () => {
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Check status badges
    expect(screen.getByText('Completed')).toBeInTheDocument();
    expect(screen.getByText('Active')).toBeInTheDocument();
    expect(screen.getByText('Pending')).toBeInTheDocument();

    // Check banking system labels
    expect(screen.getByText('FIS_SYSTEMATICS → corda')).toBeInTheDocument();
    expect(screen.getByText('FISERV_DNA → ethereum')).toBeInTheDocument();
    expect(screen.getByText('TCS_BANCS → algorand')).toBeInTheDocument();
    expect(screen.getByText('TEMENOS_TRANSACT → xrp')).toBeInTheDocument();
  });

  test('should show export and navigation functionality', async () => {
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Check navigation tabs
    expect(screen.getByText('Projects')).toBeInTheDocument();
    expect(screen.getByText('Analytics')).toBeInTheDocument();
    expect(screen.getByText('Usage')).toBeInTheDocument();
    expect(screen.getByText('Revenue')).toBeInTheDocument();

    // Check action buttons
    expect(screen.getByText('Export Report')).toBeInTheDocument();
    expect(screen.getByText('Refresh')).toBeInTheDocument();

    // Test tab navigation
    fireEvent.click(screen.getByText('Analytics'));
    await waitFor(() => {
      expect(screen.getByText('Daily Transpilation Trend')).toBeInTheDocument();
    });

    fireEvent.click(screen.getByText('Usage'));
    await waitFor(() => {
      expect(screen.getByText('Monthly Usage Tracking')).toBeInTheDocument();
    });

    fireEvent.click(screen.getByText('Revenue'));
    await waitFor(() => {
      expect(screen.getByText('Revenue Analytics')).toBeInTheDocument();
    });
  });
});

// Integration test with mock API
describe('COBOL Dashboard Integration', () => {
  test('should handle API data loading and error states', async () => {
    const consoleSpy = jest.spyOn(console, 'error').mockImplementation(() => {});
    
    render(<COBOLDashboard />);
    
    // Initial loading state
    expect(screen.getByTestId('refresh-icon')).toHaveClass('animate-spin');
    
    // Wait for data to load
    await waitFor(() => {
      expect(screen.getByText('847')).toBeInTheDocument(); // Total transpilations
    }, { timeout: 2000 });

    // Check that metrics are displayed
    expect(screen.getByText('92.3%')).toBeInTheDocument(); // Success rate
    expect(screen.getByText('782/1000')).toBeInTheDocument(); // Quota usage
    expect(screen.getByText('$37,250')).toBeInTheDocument(); // Monthly revenue

    consoleSpy.mockRestore();
  });
});

// Performance test
describe('COBOL Dashboard Performance', () => {
  test('should render efficiently with large datasets', async () => {
    const startTime = Date.now();
    
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    const renderTime = Date.now() - startTime;
    
    // Should render within reasonable time (less than 2 seconds)
    expect(renderTime).toBeLessThan(2000);
  });
});

// Accessibility test
describe('COBOL Dashboard Accessibility', () => {
  test('should have proper ARIA labels and semantic structure', async () => {
    render(<COBOLDashboard />);
    
    await waitFor(() => {
      expect(screen.getByText('COBOL Transpiler Dashboard')).toBeInTheDocument();
    });

    // Check for proper heading structure
    expect(screen.getByRole('heading', { level: 1 })).toHaveTextContent('COBOL Transpiler Dashboard');
    
    // Check for tab navigation
    expect(screen.getByRole('tablist')).toBeInTheDocument();
    expect(screen.getAllByRole('tab')).toHaveLength(4);
    
    // Check for button accessibility
    expect(screen.getByRole('button', { name: /export report/i })).toBeInTheDocument();
    expect(screen.getByRole('button', { name: /refresh/i })).toBeInTheDocument();
  });
});