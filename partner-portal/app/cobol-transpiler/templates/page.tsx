'use client';

import { useEffect, useState, useCallback } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Dialog, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { Textarea } from '@/components/ui/textarea';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { 
  Upload, 
  Download, 
  Eye, 
  Edit, 
  Trash2, 
  Copy, 
  GitBranch, 
  Search, 
  Filter, 
  Plus, 
  FileCode, 
  CheckCircle, 
  XCircle, 
  AlertTriangle,
  Clock,
  Star,
  Code2,
  Layers,
  History
} from 'lucide-react';
import { useDropzone } from 'react-dropzone';

interface Template {
  id: string;
  name: string;
  description: string;
  bankingSystem: 'FIS_SYSTEMATICS' | 'FISERV_DNA' | 'TCS_BANCS' | 'TEMENOS_TRANSACT';
  targetBlockchain: 'ethereum' | 'corda' | 'xrp' | 'algorand';
  category: 'official' | 'community' | 'custom';
  version: string;
  createdAt: string;
  updatedAt: string;
  author: string;
  downloads: number;
  rating: number;
  status: 'active' | 'deprecated' | 'beta';
  templateContent: string;
  previewContract?: string;
  validationStatus: 'valid' | 'invalid' | 'pending';
  tags: string[];
  isStarred: boolean;
}

interface TemplateVersion {
  version: string;
  createdAt: string;
  author: string;
  changelog: string;
  isActive: boolean;
}

const BANKING_SYSTEMS = [
  { value: 'FIS_SYSTEMATICS', label: 'FIS Systematics IBS' },
  { value: 'FISERV_DNA', label: 'Fiserv DNA' },
  { value: 'TCS_BANCS', label: 'TCS BaNCS' },
  { value: 'TEMENOS_TRANSACT', label: 'Temenos Transact' }
];

const BLOCKCHAINS = [
  { value: 'ethereum', label: 'Ethereum' },
  { value: 'corda', label: 'Corda' },
  { value: 'xrp', label: 'XRP Ledger' },
  { value: 'algorand', label: 'Algorand' }
];

export default function TemplateLibrary() {
  const [templates, setTemplates] = useState<Template[]>([]);
  const [filteredTemplates, setFilteredTemplates] = useState<Template[]>([]);
  const [loading, setLoading] = useState(true);
  const [searchQuery, setSearchQuery] = useState('');
  const [selectedBankingSystem, setSelectedBankingSystem] = useState<string>('all');
  const [selectedBlockchain, setSelectedBlockchain] = useState<string>('all');
  const [selectedCategory, setSelectedCategory] = useState<string>('all');
  const [selectedTemplate, setSelectedTemplate] = useState<Template | null>(null);
  const [previewOpen, setPreviewOpen] = useState(false);
  const [uploadOpen, setUploadOpen] = useState(false);
  const [versionHistoryOpen, setVersionHistoryOpen] = useState(false);
  const [templateVersions, setTemplateVersions] = useState<TemplateVersion[]>([]);

  // Mock data - In production, this would fetch from the API
  useEffect(() => {
    const fetchTemplates = async () => {
      setLoading(true);
      
      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      const mockTemplates: Template[] = [
        {
          id: '1',
          name: 'FIS Mortgage Processing Template',
          description: 'Standard template for mortgage loan processing and amortization calculations',
          bankingSystem: 'FIS_SYSTEMATICS',
          targetBlockchain: 'corda',
          category: 'official',
          version: '2.1.0',
          createdAt: '2025-06-15T10:00:00Z',
          updatedAt: '2025-07-01T14:30:00Z',
          author: 'LegacyBAAS Team',
          downloads: 1247,
          rating: 4.8,
          status: 'active',
          templateContent: `// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

/**
 * {{contractName}} - Generated from COBOL Program {{programId}}
 * Mortgage Processing Contract
 */
contract {{contractName}} {
    {{#each variables}}
    {{type}} {{visibility}} {{name}};
    {{/each}}

    function calculateMortgage(uint256 principal, uint256 rate, uint256 term) 
        public pure returns (uint256) {
        // Mortgage calculation logic
        return principal * rate / term;
    }
}`,
          previewContract: 'MortgageProcessor.sol generated',
          validationStatus: 'valid',
          tags: ['mortgage', 'loan', 'calculation', 'financial'],
          isStarred: true
        },
        {
          id: '2',
          name: 'Fiserv Payment Gateway Template',
          description: 'Real-time payment processing template with fraud detection',
          bankingSystem: 'FISERV_DNA',
          targetBlockchain: 'ethereum',
          category: 'official',
          version: '1.8.2',
          createdAt: '2025-06-20T09:00:00Z',
          updatedAt: '2025-07-03T11:15:00Z',
          author: 'LegacyBAAS Team',
          downloads: 892,
          rating: 4.6,
          status: 'active',
          templateContent: `// Payment Gateway Template
contract {{contractName}} {
    mapping(address => uint256) public balances;
    
    event PaymentProcessed(address from, address to, uint256 amount);
    
    function processPayment(address to, uint256 amount) external {
        require(balances[msg.sender] >= amount, "Insufficient funds");
        balances[msg.sender] -= amount;
        balances[to] += amount;
        emit PaymentProcessed(msg.sender, to, amount);
    }
}`,
          validationStatus: 'valid',
          tags: ['payment', 'gateway', 'fraud-detection', 'real-time'],
          isStarred: false
        },
        {
          id: '3',
          name: 'TCS Trade Finance Template',
          description: 'Letter of credit and trade finance automation template',
          bankingSystem: 'TCS_BANCS',
          targetBlockchain: 'algorand',
          category: 'official',
          version: '1.5.1',
          createdAt: '2025-06-25T15:00:00Z',
          updatedAt: '2025-07-02T16:45:00Z',
          author: 'LegacyBAAS Team',
          downloads: 634,
          rating: 4.4,
          status: 'active',
          templateContent: `#pragma version 8
// Trade Finance TEAL Template

int 1
return`,
          validationStatus: 'valid',
          tags: ['trade-finance', 'letter-of-credit', 'automation'],
          isStarred: true
        },
        {
          id: '4',
          name: 'Temenos Cross-Border Remittance',
          description: 'SEPA instant payments and cross-border remittance processing',
          bankingSystem: 'TEMENOS_TRANSACT',
          targetBlockchain: 'xrp',
          category: 'official',
          version: '3.0.0-beta',
          createdAt: '2025-07-01T12:00:00Z',
          updatedAt: '2025-07-04T10:30:00Z',
          author: 'LegacyBAAS Team',
          downloads: 156,
          rating: 4.2,
          status: 'beta',
          templateContent: `// XRP Ledger Payment Template
{
  "TransactionType": "Payment",
  "Account": "{{senderAccount}}",
  "Destination": "{{recipientAccount}}",
  "Amount": "{{amount}}",
  "Fee": "12"
}`,
          validationStatus: 'valid',
          tags: ['cross-border', 'remittance', 'sepa', 'instant'],
          isStarred: false
        },
        {
          id: '5',
          name: 'Custom Derivatives Template',
          description: 'Community-contributed template for derivatives trading',
          bankingSystem: 'FIS_SYSTEMATICS',
          targetBlockchain: 'ethereum',
          category: 'community',
          version: '1.2.3',
          createdAt: '2025-06-10T08:00:00Z',
          updatedAt: '2025-06-28T13:20:00Z',
          author: 'Community Contributor',
          downloads: 89,
          rating: 3.9,
          status: 'active',
          templateContent: `// Community Derivatives Template
contract DerivativesTrading {
    // Basic derivatives logic
    function calculateOption(uint256 strike, uint256 spot) 
        external pure returns (uint256) {
        return spot > strike ? spot - strike : 0;
    }
}`,
          validationStatus: 'pending',
          tags: ['derivatives', 'trading', 'options', 'community'],
          isStarred: false
        }
      ];

      setTemplates(mockTemplates);
      setFilteredTemplates(mockTemplates);
      setLoading(false);
    };

    fetchTemplates();
  }, []);

  // Filter templates based on search and filters
  useEffect(() => {
    let filtered = templates;

    // Search filter
    if (searchQuery) {
      filtered = filtered.filter(template => 
        template.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        template.description.toLowerCase().includes(searchQuery.toLowerCase()) ||
        template.tags.some(tag => tag.toLowerCase().includes(searchQuery.toLowerCase()))
      );
    }

    // Banking system filter
    if (selectedBankingSystem !== 'all') {
      filtered = filtered.filter(template => template.bankingSystem === selectedBankingSystem);
    }

    // Blockchain filter
    if (selectedBlockchain !== 'all') {
      filtered = filtered.filter(template => template.targetBlockchain === selectedBlockchain);
    }

    // Category filter
    if (selectedCategory !== 'all') {
      filtered = filtered.filter(template => template.category === selectedCategory);
    }

    setFilteredTemplates(filtered);
  }, [templates, searchQuery, selectedBankingSystem, selectedBlockchain, selectedCategory]);

  const onDrop = useCallback((acceptedFiles: File[]) => {
    // Handle file upload
    acceptedFiles.forEach(file => {
      const reader = new FileReader();
      reader.onabort = () => console.log('file reading was aborted');
      reader.onerror = () => console.log('file reading has failed');
      reader.onload = () => {
        // Process the template file
        console.log('Template uploaded:', file.name);
      };
      reader.readAsText(file);
    });
  }, []);

  const { getRootProps, getInputProps, isDragActive } = useDropzone({
    onDrop,
    accept: {
      'text/plain': ['.hbs', '.handlebars', '.sol', '.kt', '.teal'],
    },
    multiple: true
  });

  const handleStarTemplate = (templateId: string) => {
    setTemplates(prev => prev.map(template =>
      template.id === templateId 
        ? { ...template, isStarred: !template.isStarred }
        : template
    ));
  };

  const handlePreviewTemplate = (template: Template) => {
    setSelectedTemplate(template);
    setPreviewOpen(true);
  };

  const handleVersionHistory = (template: Template) => {
    // Mock version history data
    const mockVersions: TemplateVersion[] = [
      {
        version: template.version,
        createdAt: template.updatedAt,
        author: template.author,
        changelog: 'Latest version with performance improvements',
        isActive: true
      },
      {
        version: '2.0.0',
        createdAt: '2025-06-15T10:00:00Z',
        author: template.author,
        changelog: 'Major update with enhanced validation',
        isActive: false
      },
      {
        version: '1.9.5',
        createdAt: '2025-05-20T14:30:00Z',
        author: template.author,
        changelog: 'Bug fixes and stability improvements',
        isActive: false
      }
    ];
    
    setTemplateVersions(mockVersions);
    setSelectedTemplate(template);
    setVersionHistoryOpen(true);
  };

  const getStatusBadge = (status: string) => {
    const variants = {
      active: 'bg-green-100 text-green-800',
      deprecated: 'bg-red-100 text-red-800',
      beta: 'bg-yellow-100 text-yellow-800'
    };
    
    return (
      <Badge className={variants[status as keyof typeof variants]}>
        {status.charAt(0).toUpperCase() + status.slice(1)}
      </Badge>
    );
  };

  const getCategoryIcon = (category: string) => {
    switch (category) {
      case 'official':
        return <CheckCircle className="h-4 w-4 text-blue-500" />;
      case 'community':
        return <Star className="h-4 w-4 text-purple-500" />;
      case 'custom':
        return <Edit className="h-4 w-4 text-orange-500" />;
      default:
        return <FileCode className="h-4 w-4 text-gray-500" />;
    }
  };

  const getValidationIcon = (status: string) => {
    switch (status) {
      case 'valid':
        return <CheckCircle className="h-4 w-4 text-green-500" />;
      case 'invalid':
        return <XCircle className="h-4 w-4 text-red-500" />;
      case 'pending':
        return <Clock className="h-4 w-4 text-yellow-500" />;
      default:
        return <AlertTriangle className="h-4 w-4 text-gray-500" />;
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500 mx-auto"></div>
          <p className="mt-2 text-gray-600">Loading templates...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto p-6 space-y-8">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">Template Library</h1>
          <p className="text-gray-600 mt-2">Browse, upload, and manage COBOL transpiler templates</p>
        </div>
        <div className="flex items-center gap-3">
          <Dialog open={uploadOpen} onOpenChange={setUploadOpen}>
            <DialogTrigger asChild>
              <Button>
                <Plus className="h-4 w-4 mr-2" />
                Upload Template
              </Button>
            </DialogTrigger>
            <DialogContent className="max-w-2xl">
              <DialogHeader>
                <DialogTitle>Upload New Template</DialogTitle>
                <DialogDescription>
                  Upload a custom COBOL transpiler template or create a new one
                </DialogDescription>
              </DialogHeader>
              
              <Tabs defaultValue="upload" className="space-y-4">
                <TabsList className="grid w-full grid-cols-2">
                  <TabsTrigger value="upload">Upload File</TabsTrigger>
                  <TabsTrigger value="create">Create New</TabsTrigger>
                </TabsList>
                
                <TabsContent value="upload" className="space-y-4">
                  <div
                    {...getRootProps()}
                    className={`border-2 border-dashed rounded-lg p-6 text-center cursor-pointer transition-colors ${
                      isDragActive ? 'border-blue-500 bg-blue-50' : 'border-gray-300 hover:border-gray-400'
                    }`}
                  >
                    <input {...getInputProps()} />
                    <Upload className="h-8 w-8 mx-auto text-gray-400 mb-2" />
                    {isDragActive ? (
                      <p>Drop the template files here...</p>
                    ) : (
                      <div>
                        <p>Drag & drop template files here, or click to select</p>
                        <p className="text-sm text-gray-500 mt-1">
                          Supports .hbs, .handlebars, .sol, .kt, .teal files
                        </p>
                      </div>
                    )}
                  </div>
                </TabsContent>
                
                <TabsContent value="create" className="space-y-4">
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <Label htmlFor="template-name">Template Name</Label>
                      <Input id="template-name" placeholder="Enter template name" />
                    </div>
                    <div>
                      <Label htmlFor="template-version">Version</Label>
                      <Input id="template-version" placeholder="1.0.0" />
                    </div>
                  </div>
                  
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <Label htmlFor="banking-system">Banking System</Label>
                      <Select>
                        <SelectTrigger>
                          <SelectValue placeholder="Select banking system" />
                        </SelectTrigger>
                        <SelectContent>
                          {BANKING_SYSTEMS.map(system => (
                            <SelectItem key={system.value} value={system.value}>
                              {system.label}
                            </SelectItem>
                          ))}
                        </SelectContent>
                      </Select>
                    </div>
                    <div>
                      <Label htmlFor="blockchain">Target Blockchain</Label>
                      <Select>
                        <SelectTrigger>
                          <SelectValue placeholder="Select blockchain" />
                        </SelectTrigger>
                        <SelectContent>
                          {BLOCKCHAINS.map(blockchain => (
                            <SelectItem key={blockchain.value} value={blockchain.value}>
                              {blockchain.label}
                            </SelectItem>
                          ))}
                        </SelectContent>
                      </Select>
                    </div>
                  </div>
                  
                  <div>
                    <Label htmlFor="template-description">Description</Label>
                    <Textarea 
                      id="template-description"
                      placeholder="Describe what this template does..."
                      rows={3}
                    />
                  </div>
                  
                  <div>
                    <Label htmlFor="template-content">Template Content</Label>
                    <Textarea 
                      id="template-content"
                      placeholder="Enter your Handlebars template content..."
                      rows={10}
                      className="font-mono text-sm"
                    />
                  </div>
                </TabsContent>
              </Tabs>
              
              <DialogFooter>
                <Button variant="outline" onClick={() => setUploadOpen(false)}>
                  Cancel
                </Button>
                <Button>Save Template</Button>
              </DialogFooter>
            </DialogContent>
          </Dialog>
        </div>
      </div>

      {/* Search and Filters */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center">
            <Filter className="h-5 w-5 mr-2" />
            Search & Filter Templates
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex gap-4">
            <div className="flex-1">
              <div className="relative">
                <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
                <Input
                  placeholder="Search templates by name, description, or tags..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  className="pl-10"
                />
              </div>
            </div>
            <Select value={selectedBankingSystem} onValueChange={setSelectedBankingSystem}>
              <SelectTrigger className="w-48">
                <SelectValue placeholder="Banking System" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Banking Systems</SelectItem>
                {BANKING_SYSTEMS.map(system => (
                  <SelectItem key={system.value} value={system.value}>
                    {system.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <Select value={selectedBlockchain} onValueChange={setSelectedBlockchain}>
              <SelectTrigger className="w-40">
                <SelectValue placeholder="Blockchain" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Blockchains</SelectItem>
                {BLOCKCHAINS.map(blockchain => (
                  <SelectItem key={blockchain.value} value={blockchain.value}>
                    {blockchain.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
            <Select value={selectedCategory} onValueChange={setSelectedCategory}>
              <SelectTrigger className="w-32">
                <SelectValue placeholder="Category" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Categories</SelectItem>
                <SelectItem value="official">Official</SelectItem>
                <SelectItem value="community">Community</SelectItem>
                <SelectItem value="custom">Custom</SelectItem>
              </SelectContent>
            </Select>
          </div>
          
          <div className="text-sm text-gray-600">
            Found {filteredTemplates.length} template{filteredTemplates.length !== 1 ? 's' : ''}
          </div>
        </CardContent>
      </Card>

      {/* Templates Grid */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {filteredTemplates.map((template) => (
          <Card key={template.id} className="hover:shadow-lg transition-shadow">
            <CardHeader>
              <div className="flex items-start justify-between">
                <div className="flex items-center space-x-2">
                  {getCategoryIcon(template.category)}
                  <CardTitle className="text-lg">{template.name}</CardTitle>
                </div>
                <div className="flex items-center space-x-1">
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={() => handleStarTemplate(template.id)}
                  >
                    <Star 
                      className={`h-4 w-4 ${template.isStarred ? 'fill-yellow-400 text-yellow-400' : 'text-gray-400'}`} 
                    />
                  </Button>
                  {getValidationIcon(template.validationStatus)}
                </div>
              </div>
              <CardDescription className="line-clamp-2">
                {template.description}
              </CardDescription>
            </CardHeader>
            
            <CardContent className="space-y-4">
              <div className="flex items-center justify-between text-sm">
                <span className="text-gray-600">v{template.version}</span>
                {getStatusBadge(template.status)}
              </div>
              
              <div className="flex items-center justify-between text-sm text-gray-600">
                <span>{template.bankingSystem}</span>
                <span>→ {template.targetBlockchain}</span>
              </div>
              
              <div className="flex items-center space-x-4 text-sm text-gray-600">
                <span>{template.downloads.toLocaleString()} downloads</span>
                <div className="flex items-center">
                  <Star className="h-3 w-3 fill-current text-yellow-400 mr-1" />
                  <span>{template.rating}</span>
                </div>
              </div>
              
              <div className="flex flex-wrap gap-1">
                {template.tags.slice(0, 3).map((tag) => (
                  <Badge key={tag} variant="outline" className="text-xs">
                    {tag}
                  </Badge>
                ))}
                {template.tags.length > 3 && (
                  <Badge variant="outline" className="text-xs">
                    +{template.tags.length - 3}
                  </Badge>
                )}
              </div>
              
              <div className="flex justify-between pt-2">
                <div className="flex space-x-2">
                  <Button 
                    variant="outline" 
                    size="sm"
                    onClick={() => handlePreviewTemplate(template)}
                  >
                    <Eye className="h-4 w-4 mr-1" />
                    Preview
                  </Button>
                  <Button 
                    variant="outline" 
                    size="sm"
                    onClick={() => handleVersionHistory(template)}
                  >
                    <History className="h-4 w-4 mr-1" />
                    History
                  </Button>
                </div>
                <Button size="sm">
                  <Download className="h-4 w-4 mr-1" />
                  Use
                </Button>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>

      {/* Template Preview Dialog */}
      <Dialog open={previewOpen} onOpenChange={setPreviewOpen}>
        <DialogContent className="max-w-4xl max-h-[80vh] overflow-auto">
          <DialogHeader>
            <DialogTitle className="flex items-center">
              <Code2 className="h-5 w-5 mr-2" />
              {selectedTemplate?.name} - Preview
            </DialogTitle>
            <DialogDescription>
              {selectedTemplate?.description}
            </DialogDescription>
          </DialogHeader>
          
          {selectedTemplate && (
            <Tabs defaultValue="template" className="space-y-4">
              <TabsList>
                <TabsTrigger value="template">Template Code</TabsTrigger>
                <TabsTrigger value="preview">Generated Preview</TabsTrigger>
                <TabsTrigger value="metadata">Metadata</TabsTrigger>
              </TabsList>
              
              <TabsContent value="template">
                <div className="border rounded-lg p-4 bg-gray-50">
                  <pre className="text-sm overflow-x-auto">
                    <code>{selectedTemplate.templateContent}</code>
                  </pre>
                </div>
              </TabsContent>
              
              <TabsContent value="preview">
                <div className="border rounded-lg p-4 bg-gray-50">
                  <pre className="text-sm overflow-x-auto">
                    <code>{selectedTemplate.previewContract || 'Preview not available'}</code>
                  </pre>
                </div>
              </TabsContent>
              
              <TabsContent value="metadata" className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label>Banking System</Label>
                    <p className="text-sm text-gray-600">{selectedTemplate.bankingSystem}</p>
                  </div>
                  <div>
                    <Label>Target Blockchain</Label>
                    <p className="text-sm text-gray-600">{selectedTemplate.targetBlockchain}</p>
                  </div>
                  <div>
                    <Label>Version</Label>
                    <p className="text-sm text-gray-600">{selectedTemplate.version}</p>
                  </div>
                  <div>
                    <Label>Author</Label>
                    <p className="text-sm text-gray-600">{selectedTemplate.author}</p>
                  </div>
                  <div>
                    <Label>Downloads</Label>
                    <p className="text-sm text-gray-600">{selectedTemplate.downloads.toLocaleString()}</p>
                  </div>
                  <div>
                    <Label>Rating</Label>
                    <p className="text-sm text-gray-600">{selectedTemplate.rating}/5.0</p>
                  </div>
                </div>
                
                <div>
                  <Label>Tags</Label>
                  <div className="flex flex-wrap gap-1 mt-1">
                    {selectedTemplate.tags.map((tag) => (
                      <Badge key={tag} variant="outline" className="text-xs">
                        {tag}
                      </Badge>
                    ))}
                  </div>
                </div>
              </TabsContent>
            </Tabs>
          )}
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setPreviewOpen(false)}>
              Close
            </Button>
            <Button>Use This Template</Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Version History Dialog */}
      <Dialog open={versionHistoryOpen} onOpenChange={setVersionHistoryOpen}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle className="flex items-center">
              <GitBranch className="h-5 w-5 mr-2" />
              Version History - {selectedTemplate?.name}
            </DialogTitle>
            <DialogDescription>
              View and manage template versions
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-4">
            {templateVersions.map((version) => (
              <div key={version.version} className="border rounded-lg p-4">
                <div className="flex items-center justify-between">
                  <div className="flex items-center space-x-3">
                    <Badge variant={version.isActive ? 'default' : 'outline'}>
                      v{version.version}
                    </Badge>
                    <span className="text-sm text-gray-600">
                      by {version.author}
                    </span>
                    <span className="text-sm text-gray-500">
                      {new Date(version.createdAt).toLocaleDateString()}
                    </span>
                  </div>
                  {!version.isActive && (
                    <Button variant="outline" size="sm">
                      Rollback
                    </Button>
                  )}
                </div>
                <p className="text-sm text-gray-600 mt-2">{version.changelog}</p>
              </div>
            ))}
          </div>
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setVersionHistoryOpen(false)}>
              Close
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {filteredTemplates.length === 0 && (
        <Card>
          <CardContent className="text-center py-12">
            <FileCode className="h-12 w-12 text-gray-400 mx-auto mb-4" />
            <h3 className="text-lg font-medium text-gray-900 mb-2">No templates found</h3>
            <p className="text-gray-600 mb-4">
              Try adjusting your search criteria or upload a new template.
            </p>
            <Button onClick={() => setUploadOpen(true)}>
              <Plus className="h-4 w-4 mr-2" />
              Upload First Template
            </Button>
          </CardContent>
        </Card>
      )}
    </div>
  );
}