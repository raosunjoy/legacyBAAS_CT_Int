import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import '@testing-library/jest-dom';
import CobolUsageAnalytics from '../../app/analytics/cobol-usage/page';

// Mock recharts components
jest.mock('recharts', () => ({
  LineChart: ({ children }: any) => <div data-testid="line-chart">{children}</div>,
  Line: () => <div data-testid="line" />,
  AreaChart: ({ children }: any) => <div data-testid="area-chart">{children}</div>,
  Area: () => <div data-testid="area" />,
  BarChart: ({ children }: any) => <div data-testid="bar-chart">{children}</div>,
  Bar: () => <div data-testid="bar" />,
  PieChart: ({ children }: any) => <div data-testid="pie-chart">{children}</div>,
  Pie: () => <div data-testid="pie" />,
  Cell: () => <div data-testid="cell" />,
  ComposedChart: ({ children }: any) => <div data-testid="composed-chart">{children}</div>,
  RadialBarChart: ({ children }: any) => <div data-testid="radial-bar-chart">{children}</div>,
  RadialBar: () => <div data-testid="radial-bar" />,
  Treemap: ({ children }: any) => <div data-testid="treemap">{children}</div>,
  XAxis: () => <div data-testid="x-axis" />,
  YAxis: () => <div data-testid="y-axis" />,
  CartesianGrid: () => <div data-testid="cartesian-grid" />,
  Tooltip: () => <div data-testid="tooltip" />,
  Legend: () => <div data-testid="legend" />,
  ResponsiveContainer: ({ children }: any) => <div data-testid="responsive-container">{children}</div>
}));

// Mock UI components
jest.mock('@/components/ui/card', () => ({
  Card: ({ children }: any) => <div data-testid="card">{children}</div>,
  CardHeader: ({ children }: any) => <div data-testid="card-header">{children}</div>,
  CardTitle: ({ children }: any) => <div data-testid="card-title">{children}</div>,
  CardDescription: ({ children }: any) => <div data-testid="card-description">{children}</div>,
  CardContent: ({ children }: any) => <div data-testid="card-content">{children}</div>
}));

jest.mock('@/components/ui/tabs', () => ({
  Tabs: ({ children, value, onValueChange }: any) => (
    <div data-testid="tabs" data-value={value}>
      {React.Children.map(children, child => 
        React.isValidElement(child) && child.props.value === value ? child : null
      )}
      {children}
    </div>
  ),
  TabsList: ({ children }: any) => <div data-testid="tabs-list">{children}</div>,
  TabsTrigger: ({ children, value, onClick }: any) => (
    <button data-testid="tabs-trigger" data-value={value} onClick={onClick}>
      {children}
    </button>
  ),
  TabsContent: ({ children, value }: any) => (
    <div data-testid="tabs-content" data-value={value}>{children}</div>
  )
}));

jest.mock('@/components/ui/button', () => ({
  Button: ({ children, onClick, variant, className }: any) => (
    <button data-testid="button" onClick={onClick} data-variant={variant} className={className}>
      {children}
    </button>
  )
}));

jest.mock('@/components/ui/badge', () => ({
  Badge: ({ children, variant, className }: any) => (
    <span data-testid="badge" data-variant={variant} className={className}>{children}</span>
  )
}));

jest.mock('@/components/ui/progress', () => ({
  Progress: ({ value, className }: any) => (
    <div data-testid="progress" data-value={value} className={className}></div>
  )
}));

jest.mock('@/components/ui/input', () => ({
  Input: ({ value, onChange, placeholder }: any) => (
    <input data-testid="input" value={value} onChange={onChange} placeholder={placeholder} />
  )
}));

jest.mock('@/components/ui/label', () => ({
  Label: ({ children }: any) => <label data-testid="label">{children}</label>
}));

jest.mock('@/components/ui/select', () => ({
  Select: ({ children, value, onValueChange }: any) => (
    <div data-testid="select" data-value={value}>
      {children}
    </div>
  ),
  SelectTrigger: ({ children }: any) => <div data-testid="select-trigger">{children}</div>,
  SelectValue: ({ placeholder }: any) => <div data-testid="select-value">{placeholder}</div>,
  SelectContent: ({ children }: any) => <div data-testid="select-content">{children}</div>,
  SelectItem: ({ children, value }: any) => <div data-testid="select-item" data-value={value}>{children}</div>
}));

jest.mock('@/components/ui/alert', () => ({
  Alert: ({ children, className }: any) => <div data-testid="alert" className={className}>{children}</div>,
  AlertDescription: ({ children }: any) => <div data-testid="alert-description">{children}</div>,
  AlertTitle: ({ children }: any) => <div data-testid="alert-title">{children}</div>
}));

describe('COBOL Usage Analytics Dashboard', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders analytics dashboard with header and summary metrics', async () => {
    render(<CobolUsageAnalytics />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check header
    expect(screen.getByText('COBOL Usage Analytics')).toBeInTheDocument();
    expect(screen.getByText('Advanced analytics and insights for enterprise customers')).toBeInTheDocument();

    // Check summary metrics cards
    expect(screen.getByText('Total Transpilations')).toBeInTheDocument();
    expect(screen.getByText('Active Customers')).toBeInTheDocument();
    expect(screen.getByText('Avg Processing Time')).toBeInTheDocument();
    expect(screen.getByText('Success Rate')).toBeInTheDocument();
  });

  it('displays real-time metrics and trending data', async () => {
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check for trending indicators
    expect(screen.getByText('+12.5% vs last period')).toBeInTheDocument();
    expect(screen.getByText('+8.3% growth')).toBeInTheDocument();
    expect(screen.getByText('-15% improvement')).toBeInTheDocument();

    // Check for formatted numbers
    expect(screen.getByText('12,847')).toBeInTheDocument(); // Total transpilations
    expect(screen.getByText('147')).toBeInTheDocument(); // Total customers
    expect(screen.getByText('2.3min')).toBeInTheDocument(); // Avg processing time
    expect(screen.getByText('98.7%')).toBeInTheDocument(); // Success rate
  });

  it('handles period selection and auto-refresh functionality', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check period selector
    expect(screen.getByTestId('select')).toBeInTheDocument();

    // Check auto-refresh button
    const autoRefreshButton = screen.getByText('Auto Refresh');
    expect(autoRefreshButton).toBeInTheDocument();
    
    await user.click(autoRefreshButton);
    expect(autoRefreshButton).toHaveClass('bg-green-50');
  });

  it('renders overview tab with usage trends and resource utilization', async () => {
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check overview content
    expect(screen.getByText('Daily Usage Trends')).toBeInTheDocument();
    expect(screen.getByText('Peak Usage Hours')).toBeInTheDocument();
    expect(screen.getByText('Resource Utilization')).toBeInTheDocument();

    // Check charts are rendered
    expect(screen.getByTestId('composed-chart')).toBeInTheDocument();
    expect(screen.getByTestId('area-chart')).toBeInTheDocument();

    // Check resource utilization metrics
    expect(screen.getByText('CPU')).toBeInTheDocument();
    expect(screen.getByText('Memory')).toBeInTheDocument();
    expect(screen.getByText('Storage')).toBeInTheDocument();
    expect(screen.getByText('Network')).toBeInTheDocument();
    expect(screen.getByText('Database')).toBeInTheDocument();
  });

  it('displays performance monitoring in performance tab', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Navigate to performance tab
    const performanceTab = screen.getByText('Performance');
    await user.click(performanceTab);

    // Check performance content
    expect(screen.getByText('Response Time Monitoring')).toBeInTheDocument();
    expect(screen.getByText('Throughput Analysis')).toBeInTheDocument();
    expect(screen.getByText('Error Rate Tracking')).toBeInTheDocument();
    
    // Check performance status indicators
    expect(screen.getByText('GOOD')).toBeInTheDocument();
    
    // Check charts are rendered
    const lineCharts = screen.getAllByTestId('line-chart');
    expect(lineCharts.length).toBeGreaterThan(0);
  });

  it('shows customer analytics and growth metrics', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Navigate to customers tab
    const customersTab = screen.getByText('Customers');
    await user.click(customersTab);

    // Check customer content
    expect(screen.getByText('Top Customers by Usage')).toBeInTheDocument();
    expect(screen.getByText('Customer Growth')).toBeInTheDocument();
    expect(screen.getByText('Churn Analysis')).toBeInTheDocument();

    // Check customer data
    expect(screen.getByText('Global Bank Corp')).toBeInTheDocument();
    expect(screen.getByText('Enterprise tier')).toBeInTheDocument();
  });

  it('displays banking systems and blockchain analytics', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Navigate to systems tab
    const systemsTab = screen.getByText('Banking Systems');
    await user.click(systemsTab);

    // Check banking systems content
    expect(screen.getByText('Banking System Usage')).toBeInTheDocument();
    expect(screen.getByText('Blockchain Distribution')).toBeInTheDocument();
    expect(screen.getByText('Template Popularity')).toBeInTheDocument();

    // Check banking systems
    expect(screen.getByText('FIS Systematics')).toBeInTheDocument();
    expect(screen.getByText('Fiserv DNA')).toBeInTheDocument();
    expect(screen.getByText('TCS BaNCS')).toBeInTheDocument();
    expect(screen.getByText('Temenos T24')).toBeInTheDocument();

    // Check blockchains
    expect(screen.getByText('Ethereum')).toBeInTheDocument();
    expect(screen.getByText('Corda')).toBeInTheDocument();
    expect(screen.getByText('XRP Ledger')).toBeInTheDocument();
    expect(screen.getByText('Algorand')).toBeInTheDocument();
  });

  it('provides forecasting and capacity planning insights', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Navigate to forecasting tab
    const forecastingTab = screen.getByText('Forecasting');
    await user.click(forecastingTab);

    // Check forecasting content
    expect(screen.getByText('Usage Prediction')).toBeInTheDocument();
    expect(screen.getByText('Capacity Planning')).toBeInTheDocument();
    expect(screen.getByText('Trend Analysis')).toBeInTheDocument();

    // Check trend indicators
    expect(screen.getByText('Growing')).toBeInTheDocument();
    expect(screen.getByText('Healthy')).toBeInTheDocument();
    expect(screen.getByText('Scale Soon')).toBeInTheDocument();

    // Check capacity planning metrics
    expect(screen.getByText('Compute Instances')).toBeInTheDocument();
    expect(screen.getByText('Database Storage')).toBeInTheDocument();
    expect(screen.getByText('API Gateway')).toBeInTheDocument();
  });

  it('shows cost optimization recommendations', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Navigate to optimization tab
    const optimizationTab = screen.getByText('Optimization');
    await user.click(optimizationTab);

    // Check optimization content
    expect(screen.getByText('Cost Optimization Opportunities')).toBeInTheDocument();
    expect(screen.getByText('Optimization Recommendations')).toBeInTheDocument();

    // Check optimization areas
    expect(screen.getByText('Instance Right-sizing')).toBeInTheDocument();
    expect(screen.getByText('Storage Optimization')).toBeInTheDocument();
    expect(screen.getByText('Network Efficiency')).toBeInTheDocument();

    // Check effort indicators
    expect(screen.getByText('low effort')).toBeInTheDocument();
    expect(screen.getByText('medium effort')).toBeInTheDocument();
    expect(screen.getByText('high effort')).toBeInTheDocument();

    // Check recommendations
    expect(screen.getByText('High Priority')).toBeInTheDocument();
    expect(screen.getByText('Medium Priority')).toBeInTheDocument();
    expect(screen.getByText('Long Term')).toBeInTheDocument();
  });

  it('handles export functionality', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Find and click export button
    const exportButton = screen.getByText('Export Report');
    expect(exportButton).toBeInTheDocument();
    
    await user.click(exportButton);
    // In a real implementation, this would trigger a download
  });

  it('displays formatted currency and percentage values correctly', async () => {
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check currency formatting
    expect(screen.getByText('$445,892')).toBeInTheDocument(); // Revenue generated
    expect(screen.getByText('$3.47')).toBeInTheDocument(); // Cost per transpilation

    // Check number formatting
    expect(screen.getByText('2,847,293')).toBeInTheDocument(); // Lines processed

    // Check percentage formatting (success rate)
    expect(screen.getByText('98.7%')).toBeInTheDocument();
  });

  it('shows loading state and handles data updates', async () => {
    render(<CobolUsageAnalytics />);

    // Check loading state
    expect(screen.getByText('Loading analytics...')).toBeInTheDocument();

    // Wait for data to load
    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check that data is displayed
    expect(screen.getByText('COBOL Usage Analytics')).toBeInTheDocument();
  });

  it('handles tab navigation correctly', async () => {
    const user = userEvent.setup();
    render(<CobolUsageAnalytics />);

    await waitFor(() => {
      expect(screen.queryByText('Loading analytics...')).not.toBeInTheDocument();
    });

    // Check all tabs are present
    expect(screen.getByText('Overview')).toBeInTheDocument();
    expect(screen.getByText('Performance')).toBeInTheDocument();
    expect(screen.getByText('Customers')).toBeInTheDocument();
    expect(screen.getByText('Banking Systems')).toBeInTheDocument();
    expect(screen.getByText('Forecasting')).toBeInTheDocument();
    expect(screen.getByText('Optimization')).toBeInTheDocument();

    // Test navigation to different tabs
    await user.click(screen.getByText('Performance'));
    await user.click(screen.getByText('Customers'));
    await user.click(screen.getByText('Overview'));
  });
});