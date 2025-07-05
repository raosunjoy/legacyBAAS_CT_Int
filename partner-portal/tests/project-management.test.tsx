import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import '@testing-library/jest-dom';
import ProjectManagement from '../app/cobol-transpiler/projects/page';

// Mock react-dropzone
jest.mock('react-dropzone', () => ({
  useDropzone: jest.fn(() => ({
    getRootProps: () => ({
      'data-testid': 'dropzone'
    }),
    getInputProps: () => ({
      'data-testid': 'file-input'
    }),
    isDragActive: false
  }))
}));

// Mock UI components
jest.mock('@/components/ui/dialog', () => ({
  Dialog: ({ children, open, onOpenChange }: any) => 
    open ? <div data-testid="dialog" onClick={() => onOpenChange(false)}>{children}</div> : null,
  DialogContent: ({ children }: any) => <div data-testid="dialog-content">{children}</div>,
  DialogHeader: ({ children }: any) => <div data-testid="dialog-header">{children}</div>,
  DialogTitle: ({ children }: any) => <div data-testid="dialog-title">{children}</div>,
  DialogDescription: ({ children }: any) => <div data-testid="dialog-description">{children}</div>,
  DialogFooter: ({ children }: any) => <div data-testid="dialog-footer">{children}</div>,
  DialogTrigger: ({ children }: any) => <div data-testid="dialog-trigger">{children}</div>
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

jest.mock('@/components/ui/card', () => ({
  Card: ({ children }: any) => <div data-testid="card">{children}</div>,
  CardHeader: ({ children }: any) => <div data-testid="card-header">{children}</div>,
  CardTitle: ({ children }: any) => <div data-testid="card-title">{children}</div>,
  CardDescription: ({ children }: any) => <div data-testid="card-description">{children}</div>,
  CardContent: ({ children }: any) => <div data-testid="card-content">{children}</div>
}));

jest.mock('@/components/ui/button', () => ({
  Button: ({ children, onClick, variant, size }: any) => (
    <button data-testid="button" onClick={onClick} data-variant={variant} data-size={size}>
      {children}
    </button>
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

jest.mock('@/components/ui/select', () => ({
  Select: ({ children, value, onValueChange }: any) => (
    <div data-testid="select" data-value={value}>
      {children}
    </div>
  ),
  SelectTrigger: ({ children, onClick }: any) => (
    <div data-testid="select-trigger" onClick={onClick}>{children}</div>
  ),
  SelectValue: ({ placeholder }: any) => <div data-testid="select-value">{placeholder}</div>,
  SelectContent: ({ children }: any) => <div data-testid="select-content">{children}</div>,
  SelectItem: ({ children, value, onClick }: any) => (
    <div data-testid="select-item" data-value={value} onClick={onClick}>{children}</div>
  )
}));

jest.mock('@/components/ui/badge', () => ({
  Badge: ({ children, className }: any) => (
    <span data-testid="badge" className={className}>{children}</span>
  )
}));

jest.mock('@/components/ui/textarea', () => ({
  Textarea: ({ value, onChange, placeholder, rows, ...props }: any) => (
    <textarea 
      data-testid="textarea"
      value={value}
      onChange={onChange}
      placeholder={placeholder}
      rows={rows}
      {...props}
    />
  )
}));

jest.mock('@/components/ui/label', () => ({
  Label: ({ children, htmlFor }: any) => <label data-testid="label" htmlFor={htmlFor}>{children}</label>
}));

jest.mock('@/components/ui/alert', () => ({
  Alert: ({ children, className }: any) => <div data-testid="alert" className={className}>{children}</div>,
  AlertDescription: ({ children }: any) => <div data-testid="alert-description">{children}</div>,
  AlertTitle: ({ children }: any) => <div data-testid="alert-title">{children}</div>
}));

jest.mock('@/components/ui/progress', () => ({
  Progress: ({ value, className }: any) => (
    <div data-testid="progress" data-value={value} className={className}></div>
  )
}));

jest.mock('@/components/ui/data-table', () => ({
  DataTable: ({ data }: any) => <div data-testid="data-table">Table with {data?.length || 0} items</div>
}));

describe('Project Management', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders project management interface with header and statistics', async () => {
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Check header
    expect(screen.getByText('COBOL Transpilation Projects')).toBeInTheDocument();
    expect(screen.getByText('Manage your COBOL to blockchain transformation projects')).toBeInTheDocument();

    // Check statistics cards
    expect(screen.getByText('Total Projects')).toBeInTheDocument();
    expect(screen.getByText('Active Projects')).toBeInTheDocument();
    expect(screen.getByText('Completed')).toBeInTheDocument();
    expect(screen.getByText('Total Lines')).toBeInTheDocument();
  });

  it('displays project cards with correct information', async () => {
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Check that project cards are rendered
    expect(screen.getByText('Mortgage Processing Migration')).toBeInTheDocument();
    expect(screen.getByText('Payment Gateway Integration')).toBeInTheDocument();
    expect(screen.getByText('Trade Finance Automation')).toBeInTheDocument();
    expect(screen.getByText('Cross-Border Payments')).toBeInTheDocument();
    expect(screen.getByText('Derivatives Trading Platform')).toBeInTheDocument();

    // Check project details
    expect(screen.getByText('Converting legacy mortgage processing COBOL to smart contracts')).toBeInTheDocument();
    expect(screen.getByText('FIS_SYSTEMATICS')).toBeInTheDocument();
    expect(screen.getByText('corda')).toBeInTheDocument();
  });

  it('handles create new project dialog', async () => {
    const user = userEvent.setup();
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Click new project button
    const newProjectButton = screen.getByText('New Project');
    await user.click(newProjectButton);

    // Check that dialog opens
    expect(screen.getByTestId('dialog')).toBeInTheDocument();
    expect(screen.getByText('Create New Transpilation Project')).toBeInTheDocument();
    expect(screen.getByText('Set up a new COBOL to blockchain transformation project')).toBeInTheDocument();

    // Check form fields
    expect(screen.getByPlaceholderText('Enter project name')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Describe your project...')).toBeInTheDocument();
    expect(screen.getByText('Select banking system')).toBeInTheDocument();
    expect(screen.getByText('Select blockchain')).toBeInTheDocument();
    expect(screen.getByText('Select a template')).toBeInTheDocument();
  });

  it('handles file upload dialog', async () => {
    const user = userEvent.setup();
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Find and click upload files button for draft project
    const uploadButtons = screen.getAllByText('Upload Files');
    if (uploadButtons.length > 0) {
      await user.click(uploadButtons[0]);

      // Check that upload dialog opens
      expect(screen.getByText('Upload COBOL Files')).toBeInTheDocument();
      expect(screen.getByTestId('dropzone')).toBeInTheDocument();
      expect(screen.getByText('Drag & drop COBOL files here, or click to select')).toBeInTheDocument();
    }
  });

  it('shows project status and progress correctly', async () => {
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Check for different status badges
    expect(screen.getByText('Completed')).toBeInTheDocument();
    expect(screen.getByText('Processing')).toBeInTheDocument();
    expect(screen.getByText('Ready')).toBeInTheDocument();
    expect(screen.getByText('Failed')).toBeInTheDocument();
    expect(screen.getByText('Draft')).toBeInTheDocument();

    // Check for progress indicator on processing project
    const progressElements = screen.getAllByTestId('progress');
    expect(progressElements.length).toBeGreaterThan(0);
  });

  it('handles start transpilation action', async () => {
    const user = userEvent.setup();
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Find and click start transpilation button
    const startButtons = screen.getAllByText('Start Transpilation');
    if (startButtons.length > 0) {
      await user.click(startButtons[0]);
      
      // The project status should change (implementation detail)
      // In a real test, we'd check that the appropriate API call was made
    }

    expect(startButtons.length).toBeGreaterThan(0);
  });

  it('displays download buttons for completed projects', async () => {
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Check for generated files section in completed project
    expect(screen.getByText('Generated Files')).toBeInTheDocument();
    expect(screen.getByText('MortgageProcessor.kt')).toBeInTheDocument();
    expect(screen.getByText('MortgageProcessor.json')).toBeInTheDocument();

    // Check for download buttons
    const downloadButtons = screen.getAllByTestId('button').filter(button =>
      button.textContent?.includes('Download') || button.querySelector('svg')
    );
    expect(downloadButtons.length).toBeGreaterThan(0);
  });

  it('handles tab navigation between project statuses', async () => {
    const user = userEvent.setup();
    render(<ProjectManagement />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading projects...')).not.toBeInTheDocument();
    });

    // Check that tabs are rendered
    expect(screen.getByText(/All Projects/)).toBeInTheDocument();
    expect(screen.getByText(/Draft/)).toBeInTheDocument();
    expect(screen.getByText(/Ready/)).toBeInTheDocument();
    expect(screen.getByText(/Processing/)).toBeInTheDocument();
    expect(screen.getByText(/Completed/)).toBeInTheDocument();
    expect(screen.getByText(/Deployed/)).toBeInTheDocument();

    // Click on a different tab
    const processingTab = screen.getByText(/Processing \(1\)/);
    await user.click(processingTab);

    // Should show only processing projects
    expect(screen.getByText('Payment Gateway Integration')).toBeInTheDocument();
  });
});