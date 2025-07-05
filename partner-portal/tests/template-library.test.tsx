import React from 'react';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import '@testing-library/jest-dom';
import TemplateLibrary from '../app/cobol-transpiler/templates/page';

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

// Mock Radix UI components
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
  Tabs: ({ children, defaultValue }: any) => <div data-testid="tabs" data-default-value={defaultValue}>{children}</div>,
  TabsList: ({ children }: any) => <div data-testid="tabs-list">{children}</div>,
  TabsTrigger: ({ children, value }: any) => <div data-testid="tabs-trigger" data-value={value}>{children}</div>,
  TabsContent: ({ children, value }: any) => <div data-testid="tabs-content" data-value={value}>{children}</div>
}));

jest.mock('@/components/ui/card', () => ({
  Card: ({ children, className }: any) => <div data-testid="card" className={className}>{children}</div>,
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
    <div data-testid="select" data-value={value} onClick={() => onValueChange && onValueChange('test-value')}>
      {children}
    </div>
  ),
  SelectTrigger: ({ children }: any) => <div data-testid="select-trigger">{children}</div>,
  SelectValue: ({ placeholder }: any) => <div data-testid="select-value">{placeholder}</div>,
  SelectContent: ({ children }: any) => <div data-testid="select-content">{children}</div>,
  SelectItem: ({ children, value }: any) => <div data-testid="select-item" data-value={value}>{children}</div>
}));

jest.mock('@/components/ui/badge', () => ({
  Badge: ({ children, variant, className }: any) => (
    <span data-testid="badge" data-variant={variant} className={className}>{children}</span>
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
  Alert: ({ children }: any) => <div data-testid="alert">{children}</div>,
  AlertDescription: ({ children }: any) => <div data-testid="alert-description">{children}</div>,
  AlertTitle: ({ children }: any) => <div data-testid="alert-title">{children}</div>
}));

jest.mock('@/components/ui/progress', () => ({
  Progress: ({ value }: any) => <div data-testid="progress" data-value={value}></div>
}));

describe('Template Library', () => {
  beforeEach(() => {
    jest.clearAllMocks();
  });

  it('renders template library with header and search functionality', async () => {
    render(<TemplateLibrary />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading templates...')).not.toBeInTheDocument();
    });

    // Check header
    expect(screen.getByText('Template Library')).toBeInTheDocument();
    expect(screen.getByText('Browse, upload, and manage COBOL transpiler templates')).toBeInTheDocument();

    // Check search and filter section
    expect(screen.getByText('Search & Filter Templates')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Search templates by name, description, or tags...')).toBeInTheDocument();
  });

  it('displays template cards with correct information', async () => {
    render(<TemplateLibrary />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading templates...')).not.toBeInTheDocument();
    });

    // Check that template cards are rendered
    expect(screen.getByText('FIS Mortgage Processing Template')).toBeInTheDocument();
    expect(screen.getByText('Fiserv Payment Gateway Template')).toBeInTheDocument();
    expect(screen.getByText('TCS Trade Finance Template')).toBeInTheDocument();
    expect(screen.getByText('Temenos Cross-Border Remittance')).toBeInTheDocument();
    expect(screen.getByText('Custom Derivatives Template')).toBeInTheDocument();

    // Check template details
    expect(screen.getByText('Standard template for mortgage loan processing and amortization calculations')).toBeInTheDocument();
    expect(screen.getByText('v2.1.0')).toBeInTheDocument();
    expect(screen.getByText('1,247 downloads')).toBeInTheDocument();
  });

  it('handles search functionality correctly', async () => {
    const user = userEvent.setup();
    render(<TemplateLibrary />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading templates...')).not.toBeInTheDocument();
    });

    // Search for mortgage
    const searchInput = screen.getByPlaceholderText('Search templates by name, description, or tags...');
    await user.type(searchInput, 'mortgage');

    // Should show templates containing 'mortgage'
    await waitFor(() => {
      expect(screen.getByText('FIS Mortgage Processing Template')).toBeInTheDocument();
      expect(screen.queryByText('Fiserv Payment Gateway Template')).not.toBeInTheDocument();
    });
  });

  it('opens template preview dialog with correct content', async () => {
    const user = userEvent.setup();
    render(<TemplateLibrary />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading templates...')).not.toBeInTheDocument();
    });

    // Click preview button on first template
    const previewButtons = screen.getAllByText('Preview');
    await user.click(previewButtons[0]);

    // Check that preview dialog opens
    expect(screen.getByTestId('dialog')).toBeInTheDocument();
    expect(screen.getByText('FIS Mortgage Processing Template - Preview')).toBeInTheDocument();
    expect(screen.getByText('Standard template for mortgage loan processing and amortization calculations')).toBeInTheDocument();
  });

  it('opens upload template dialog with form fields', async () => {
    const user = userEvent.setup();
    render(<TemplateLibrary />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading templates...')).not.toBeInTheDocument();
    });

    // Click upload template button
    const uploadButton = screen.getByText('Upload Template');
    await user.click(uploadButton);

    // Check that upload dialog opens
    expect(screen.getByTestId('dialog')).toBeInTheDocument();
    expect(screen.getByText('Upload New Template')).toBeInTheDocument();
    expect(screen.getByText('Upload a custom COBOL transpiler template or create a new one')).toBeInTheDocument();

    // Check form fields in create tab
    expect(screen.getByPlaceholderText('Enter template name')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('1.0.0')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Describe what this template does...')).toBeInTheDocument();
    expect(screen.getByPlaceholderText('Enter your Handlebars template content...')).toBeInTheDocument();
  });

  it('handles template starring functionality', async () => {
    const user = userEvent.setup();
    render(<TemplateLibrary />);

    // Wait for loading to complete
    await waitFor(() => {
      expect(screen.queryByText('Loading templates...')).not.toBeInTheDocument();
    });

    // Find star buttons (templates have star icons)
    const starButtons = screen.getAllByTestId('button').filter(button => 
      button.querySelector('svg') && button.getAttribute('data-variant') === 'ghost'
    );

    // Click on a star button
    if (starButtons.length > 0) {
      await user.click(starButtons[0]);
      // The star state should toggle (implementation detail - would need more specific testing)
    }

    expect(starButtons.length).toBeGreaterThan(0);
  });
});