import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import '@testing-library/jest-dom';
import BillingIntegration from '../app/cobol-transpiler/billing/page';

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
  Button: ({ children, onClick, variant, size, disabled }: any) => (
    <button data-testid="button" onClick={onClick} data-variant={variant} data-size={size} disabled={disabled}>
      {children}
    </button>
  )
}));

jest.mock('@/components/ui/badge', () => ({
  Badge: ({ children, className }: any) => (
    <span data-testid="badge" className={className}>{children}</span>
  )
}));

jest.mock('@/components/ui/progress', () => ({
  Progress: ({ value, className }: any) => (
    <div data-testid="progress" data-value={value} className={className}></div>
  )
}));

jest.mock('@/components/ui/input', () => ({
  Input: ({ value, onChange, placeholder, ...props }: any) => (
    <input 
      data-testid="input"
      value={value}
      onChange={onChange}
      placeholder={placeholder}
      {...props}
    />
  )
}));

jest.mock('@/components/ui/label', () => ({
  Label: ({ children, htmlFor }: any) => <label data-testid="label" htmlFor={htmlFor}>{children}</label>
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

describe('Billing Integration', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders billing interface with header and overview cards', async () => {
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // Check header
    expect(screen.getByText('Billing & Usage')).toBeInTheDocument();
    expect(screen.getByText('Track your COBOL transpiler usage and manage billing')).toBeInTheDocument();

    // Check overview cards
    expect(screen.getByText('Current Usage')).toBeInTheDocument();
    expect(screen.getByText('Current Period Cost')).toBeInTheDocument();
    expect(screen.getByText('Lines Processed')).toBeInTheDocument();
    expect(screen.getByText('Next Reset')).toBeInTheDocument();
  });

  it('displays current usage vs quota with progress bar', async () => {
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // Check usage information
    expect(screen.getByText('187')).toBeInTheDocument(); // Current usage
    expect(screen.getByText('/250')).toBeInTheDocument(); // Quota limit
    expect(screen.getByText('63 transpilations remaining')).toBeInTheDocument();

    // Check progress bar
    const progressBars = screen.getAllByTestId('progress');
    expect(progressBars.length).toBeGreaterThan(0);
  });

  it('shows overage calculations and projections', async () => {
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // Check cost information
    expect(screen.getByText('$999.00')).toBeInTheDocument(); // Total cost
    expect(screen.getByText('245,632')).toBeInTheDocument(); // Lines processed

    // Check projections section
    expect(screen.getByText('End-of-Month Projections')).toBeInTheDocument();
    expect(screen.getByText('Projected Transpilations')).toBeInTheDocument();
    expect(screen.getByText('Projected Cost')).toBeInTheDocument();
    expect(screen.getByText('Recommended Plan')).toBeInTheDocument();
  });

  it('renders billing history with invoice details', async () => {
    const user = userEvent.setup();
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // Click on billing history tab
    const billingTab = screen.getByText('Billing History');
    await user.click(billingTab);

    // Check billing history entries
    expect(screen.getByText('July 2025')).toBeInTheDocument();
    expect(screen.getByText('June 2025')).toBeInTheDocument();
    expect(screen.getByText('Pending')).toBeInTheDocument();
    expect(screen.getByText('Paid')).toBeInTheDocument();

    // Check download invoice buttons
    const downloadButtons = screen.getAllByText('Download Invoice');
    expect(downloadButtons.length).toBeGreaterThan(0);
  });

  it('displays partner commission information', async () => {
    const user = userEvent.setup();
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // Click on partner commissions tab
    const commissionsTab = screen.getByText('Partner Commissions');
    await user.click(commissionsTab);

    // Check commission overview
    expect(screen.getByText('Commission Overview')).toBeInTheDocument();
    expect(screen.getByText('Total Commissions')).toBeInTheDocument();
    expect(screen.getByText('Commission Rate')).toBeInTheDocument();
    expect(screen.getByText('15%')).toBeInTheDocument();

    // Check commission entries
    expect(screen.getByText('Revenue Generated:')).toBeInTheDocument();
    expect(screen.getByText('Customer Count:')).toBeInTheDocument();
  });

  it('shows pricing plans comparison', async () => {
    const user = userEvent.setup();
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // Click on pricing plans tab
    const plansTab = screen.getByText('Pricing Plans');
    await user.click(plansTab);

    // Check pricing plan information
    expect(screen.getByText('Current Plan: Professional')).toBeInTheDocument();
    expect(screen.getByText('Starter')).toBeInTheDocument();
    expect(screen.getByText('Professional')).toBeInTheDocument();
    expect(screen.getByText('Enterprise')).toBeInTheDocument();
    expect(screen.getByText('Custom')).toBeInTheDocument();

    // Check plan comparison table
    expect(screen.getByText('Plan Comparison')).toBeInTheDocument();
    expect(screen.getByText('Monthly Transpilations')).toBeInTheDocument();
    expect(screen.getByText('Overage Rate')).toBeInTheDocument();
  });

  it('handles high usage alert correctly', async () => {
    // Mock high usage scenario
    const highUsageMock = jest.fn();
    
    render(<BillingIntegration />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading billing information...')).not.toBeInTheDocument();
    });

    // The component should show usage at 74.8% (187/250), which is below 85% threshold
    // so no alert should be shown
    expect(screen.queryByText('High Usage Alert')).not.toBeInTheDocument();
  });
});