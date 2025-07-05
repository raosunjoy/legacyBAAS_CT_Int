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
import { DataTable } from '@/components/ui/data-table';
import { 
  Upload, 
  Download, 
  Eye, 
  Play, 
  Pause,
  StopCircle,
  RefreshCw,
  Clock,
  CheckCircle,
  XCircle,
  AlertTriangle,
  FileCode,
  FileText,
  Settings,
  Trash2,
  Copy,
  ExternalLink,
  Plus,
  FolderOpen,
  Code2,
  Layers,
  Activity,
  Terminal,
  Package,
  GitBranch,
  Zap
} from 'lucide-react';
import { useDropzone } from 'react-dropzone';

interface TranspilationProject {
  id: string;
  name: string;
  description: string;
  bankingSystem: 'FIS_SYSTEMATICS' | 'FISERV_DNA' | 'TCS_BANCS' | 'TEMENOS_TRANSACT';
  targetBlockchain: 'ethereum' | 'corda' | 'xrp' | 'algorand';
  status: 'draft' | 'ready' | 'processing' | 'completed' | 'failed' | 'deployed';
  createdAt: string;
  updatedAt: string;
  owner: string;
  cobolFiles: CobolFile[];
  configFile?: ConfigFile;
  template: string;
  progress: number;
  estimatedTime?: string;
  completedAt?: string;
  outputFiles?: OutputFile[];
  deploymentInfo?: DeploymentInfo;
  errorMessage?: string;
  metrics?: ProjectMetrics;
}

interface CobolFile {
  id: string;
  name: string;
  size: number;
  uploadedAt: string;
  status: 'uploaded' | 'validated' | 'invalid';
  lineCount?: number;
  programId?: string;
  validationErrors?: string[];
}

interface ConfigFile {
  id: string;
  name: string;
  content: string;
  uploadedAt: string;
}

interface OutputFile {
  id: string;
  name: string;
  type: 'contract' | 'abi' | 'deployment' | 'documentation';
  size: number;
  generatedAt: string;
  downloadUrl: string;
}

interface DeploymentInfo {
  network: string;
  contractAddress?: string;
  transactionHash?: string;
  deployedAt?: string;
  gasUsed?: string;
  explorerUrl?: string;
}

interface ProjectMetrics {
  linesOfCode: number;
  complexity: 'low' | 'medium' | 'high';
  estimatedGas?: string;
  contractSize?: number;
  functions?: number;
  events?: number;
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

export default function ProjectManagement() {
  const [projects, setProjects] = useState<TranspilationProject[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedProject, setSelectedProject] = useState<TranspilationProject | null>(null);
  const [createProjectOpen, setCreateProjectOpen] = useState(false);
  const [viewDetailsOpen, setViewDetailsOpen] = useState(false);
  const [uploadFilesOpen, setUploadFilesOpen] = useState(false);
  const [deploymentOpen, setDeploymentOpen] = useState(false);
  const [activeTab, setActiveTab] = useState('all');

  // New project form state
  const [newProjectName, setNewProjectName] = useState('');
  const [newProjectDescription, setNewProjectDescription] = useState('');
  const [newProjectBankingSystem, setNewProjectBankingSystem] = useState('');
  const [newProjectBlockchain, setNewProjectBlockchain] = useState('');
  const [newProjectTemplate, setNewProjectTemplate] = useState('');

  // Mock data - In production, this would fetch from the API
  useEffect(() => {
    const fetchProjects = async () => {
      setLoading(true);
      
      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      const mockProjects: TranspilationProject[] = [
        {
          id: '1',
          name: 'Mortgage Processing Migration',
          description: 'Converting legacy mortgage processing COBOL to smart contracts',
          bankingSystem: 'FIS_SYSTEMATICS',
          targetBlockchain: 'corda',
          status: 'completed',
          createdAt: '2025-07-01T10:00:00Z',
          updatedAt: '2025-07-01T14:30:00Z',
          owner: 'john.doe@bank.com',
          cobolFiles: [
            {
              id: '1',
              name: 'MORTGAGE-CALC.cbl',
              size: 15234,
              uploadedAt: '2025-07-01T10:05:00Z',
              status: 'validated',
              lineCount: 450,
              programId: 'MORTCALC'
            },
            {
              id: '2',
              name: 'AMORTIZATION.cbl',
              size: 8921,
              uploadedAt: '2025-07-01T10:06:00Z',
              status: 'validated',
              lineCount: 280,
              programId: 'AMORTIZ'
            }
          ],
          configFile: {
            id: '1',
            name: 'mortgage-config.yaml',
            content: 'bankingSystem: FIS_SYSTEMATICS\ntargetBlockchain: corda',
            uploadedAt: '2025-07-01T10:07:00Z'
          },
          template: 'FIS Mortgage Processing Template',
          progress: 100,
          completedAt: '2025-07-01T14:30:00Z',
          outputFiles: [
            {
              id: '1',
              name: 'MortgageProcessor.kt',
              type: 'contract',
              size: 23456,
              generatedAt: '2025-07-01T14:25:00Z',
              downloadUrl: '/api/download/1'
            },
            {
              id: '2',
              name: 'MortgageProcessor.json',
              type: 'abi',
              size: 5678,
              generatedAt: '2025-07-01T14:25:00Z',
              downloadUrl: '/api/download/2'
            }
          ],
          deploymentInfo: {
            network: 'Corda Test Network',
            contractAddress: 'net.corda.mortgageprocessor',
            transactionHash: '0xabc123...',
            deployedAt: '2025-07-01T15:00:00Z',
            gasUsed: 'N/A'
          },
          metrics: {
            linesOfCode: 730,
            complexity: 'medium',
            contractSize: 23456,
            functions: 12,
            events: 5
          }
        },
        {
          id: '2',
          name: 'Payment Gateway Integration',
          description: 'Real-time payment processing with fraud detection',
          bankingSystem: 'FISERV_DNA',
          targetBlockchain: 'ethereum',
          status: 'processing',
          createdAt: '2025-07-05T09:00:00Z',
          updatedAt: '2025-07-05T09:15:00Z',
          owner: 'jane.smith@bank.com',
          cobolFiles: [
            {
              id: '3',
              name: 'PAYMENT-PROC.cbl',
              size: 25678,
              uploadedAt: '2025-07-05T09:05:00Z',
              status: 'validated',
              lineCount: 890,
              programId: 'PAYMPROC'
            }
          ],
          template: 'Fiserv Payment Gateway Template',
          progress: 65,
          estimatedTime: '5 minutes',
          metrics: {
            linesOfCode: 890,
            complexity: 'high',
            estimatedGas: '2,500,000'
          }
        },
        {
          id: '3',
          name: 'Trade Finance Automation',
          description: 'Letter of credit processing on blockchain',
          bankingSystem: 'TCS_BANCS',
          targetBlockchain: 'algorand',
          status: 'ready',
          createdAt: '2025-07-04T14:00:00Z',
          updatedAt: '2025-07-04T14:10:00Z',
          owner: 'mike.wilson@bank.com',
          cobolFiles: [
            {
              id: '4',
              name: 'LC-PROCESSOR.cbl',
              size: 18234,
              uploadedAt: '2025-07-04T14:05:00Z',
              status: 'validated',
              lineCount: 620,
              programId: 'LCPROC'
            },
            {
              id: '5',
              name: 'TRADE-VALIDATION.cbl',
              size: 12345,
              uploadedAt: '2025-07-04T14:06:00Z',
              status: 'validated',
              lineCount: 410,
              programId: 'TRADEVAL'
            }
          ],
          template: 'TCS Trade Finance Template',
          progress: 0,
          metrics: {
            linesOfCode: 1030,
            complexity: 'medium'
          }
        },
        {
          id: '4',
          name: 'Cross-Border Payments',
          description: 'SEPA instant payments migration',
          bankingSystem: 'TEMENOS_TRANSACT',
          targetBlockchain: 'xrp',
          status: 'failed',
          createdAt: '2025-07-03T11:00:00Z',
          updatedAt: '2025-07-03T11:45:00Z',
          owner: 'sarah.johnson@bank.com',
          cobolFiles: [
            {
              id: '6',
              name: 'SEPA-INSTANT.cbl',
              size: 21234,
              uploadedAt: '2025-07-03T11:05:00Z',
              status: 'invalid',
              validationErrors: ['Missing IDENTIFICATION DIVISION', 'Invalid PROCEDURE DIVISION syntax']
            }
          ],
          template: 'Temenos Cross-Border Remittance',
          progress: 15,
          errorMessage: 'COBOL validation failed: Invalid syntax in PROCEDURE DIVISION',
          metrics: {
            linesOfCode: 0,
            complexity: 'low'
          }
        },
        {
          id: '5',
          name: 'Derivatives Trading Platform',
          description: 'Options and futures contract automation',
          bankingSystem: 'FIS_SYSTEMATICS',
          targetBlockchain: 'ethereum',
          status: 'draft',
          createdAt: '2025-07-05T08:00:00Z',
          updatedAt: '2025-07-05T08:00:00Z',
          owner: 'alex.chen@bank.com',
          cobolFiles: [],
          template: 'Custom Derivatives Template',
          progress: 0,
          metrics: {
            linesOfCode: 0,
            complexity: 'low'
          }
        }
      ];

      setProjects(mockProjects);
      setLoading(false);
    };

    fetchProjects();
  }, []);

  const onDrop = useCallback((acceptedFiles: File[]) => {
    // Handle file upload
    acceptedFiles.forEach(file => {
      const reader = new FileReader();
      reader.onabort = () => console.log('file reading was aborted');
      reader.onerror = () => console.log('file reading has failed');
      reader.onload = () => {
        // Process the COBOL file
        console.log('COBOL file uploaded:', file.name);
      };
      reader.readAsText(file);
    });
  }, []);

  const { getRootProps, getInputProps, isDragActive } = useDropzone({
    onDrop,
    accept: {
      'text/plain': ['.cbl', '.cob', '.cobol'],
      'application/x-yaml': ['.yaml', '.yml']
    },
    multiple: true
  });

  const getStatusBadge = (status: string) => {
    const variants = {
      draft: { color: 'bg-gray-100 text-gray-800', icon: <FileText className="h-3 w-3" /> },
      ready: { color: 'bg-blue-100 text-blue-800', icon: <Clock className="h-3 w-3" /> },
      processing: { color: 'bg-yellow-100 text-yellow-800', icon: <RefreshCw className="h-3 w-3 animate-spin" /> },
      completed: { color: 'bg-green-100 text-green-800', icon: <CheckCircle className="h-3 w-3" /> },
      failed: { color: 'bg-red-100 text-red-800', icon: <XCircle className="h-3 w-3" /> },
      deployed: { color: 'bg-purple-100 text-purple-800', icon: <Zap className="h-3 w-3" /> }
    };
    
    const variant = variants[status as keyof typeof variants];
    
    return (
      <Badge className={`${variant.color} flex items-center gap-1`}>
        {variant.icon}
        {status.charAt(0).toUpperCase() + status.slice(1)}
      </Badge>
    );
  };

  const getComplexityBadge = (complexity: string) => {
    const colors = {
      low: 'bg-green-100 text-green-800',
      medium: 'bg-yellow-100 text-yellow-800',
      high: 'bg-red-100 text-red-800'
    };
    
    return (
      <Badge className={colors[complexity as keyof typeof colors]}>
        {complexity.charAt(0).toUpperCase() + complexity.slice(1)} Complexity
      </Badge>
    );
  };

  const handleCreateProject = () => {
    // Create new project
    const newProject: TranspilationProject = {
      id: (projects.length + 1).toString(),
      name: newProjectName,
      description: newProjectDescription,
      bankingSystem: newProjectBankingSystem as any,
      targetBlockchain: newProjectBlockchain as any,
      status: 'draft',
      createdAt: new Date().toISOString(),
      updatedAt: new Date().toISOString(),
      owner: 'current.user@bank.com',
      cobolFiles: [],
      template: newProjectTemplate,
      progress: 0,
      metrics: {
        linesOfCode: 0,
        complexity: 'low'
      }
    };

    setProjects([...projects, newProject]);
    setCreateProjectOpen(false);
    
    // Reset form
    setNewProjectName('');
    setNewProjectDescription('');
    setNewProjectBankingSystem('');
    setNewProjectBlockchain('');
    setNewProjectTemplate('');
  };

  const handleStartTranspilation = (project: TranspilationProject) => {
    // Start transpilation process
    setProjects(prev => prev.map(p => 
      p.id === project.id 
        ? { ...p, status: 'processing' as const, progress: 10 }
        : p
    ));
  };

  const handleDownloadOutput = (file: OutputFile) => {
    // Download output file
    console.log('Downloading:', file.name);
  };

  const handleDeployContract = (project: TranspilationProject) => {
    setSelectedProject(project);
    setDeploymentOpen(true);
  };

  const filteredProjects = projects.filter(project => {
    if (activeTab === 'all') return true;
    return project.status === activeTab;
  });

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500 mx-auto"></div>
          <p className="mt-2 text-gray-600">Loading projects...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto p-6 space-y-8">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">COBOL Transpilation Projects</h1>
          <p className="text-gray-600 mt-2">Manage your COBOL to blockchain transformation projects</p>
        </div>
        <Dialog open={createProjectOpen} onOpenChange={setCreateProjectOpen}>
          <DialogTrigger asChild>
            <Button>
              <Plus className="h-4 w-4 mr-2" />
              New Project
            </Button>
          </DialogTrigger>
          <DialogContent className="max-w-2xl">
            <DialogHeader>
              <DialogTitle>Create New Transpilation Project</DialogTitle>
              <DialogDescription>
                Set up a new COBOL to blockchain transformation project
              </DialogDescription>
            </DialogHeader>
            
            <div className="space-y-4">
              <div>
                <Label htmlFor="project-name">Project Name</Label>
                <Input 
                  id="project-name"
                  placeholder="Enter project name"
                  value={newProjectName}
                  onChange={(e) => setNewProjectName(e.target.value)}
                />
              </div>
              
              <div>
                <Label htmlFor="project-description">Description</Label>
                <Textarea 
                  id="project-description"
                  placeholder="Describe your project..."
                  rows={3}
                  value={newProjectDescription}
                  onChange={(e) => setNewProjectDescription(e.target.value)}
                />
              </div>
              
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <Label>Banking System</Label>
                  <Select value={newProjectBankingSystem} onValueChange={setNewProjectBankingSystem}>
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
                  <Label>Target Blockchain</Label>
                  <Select value={newProjectBlockchain} onValueChange={setNewProjectBlockchain}>
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
                <Label>Template</Label>
                <Select value={newProjectTemplate} onValueChange={setNewProjectTemplate}>
                  <SelectTrigger>
                    <SelectValue placeholder="Select a template" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="FIS Mortgage Processing Template">FIS Mortgage Processing Template</SelectItem>
                    <SelectItem value="Fiserv Payment Gateway Template">Fiserv Payment Gateway Template</SelectItem>
                    <SelectItem value="TCS Trade Finance Template">TCS Trade Finance Template</SelectItem>
                    <SelectItem value="Temenos Cross-Border Remittance">Temenos Cross-Border Remittance</SelectItem>
                    <SelectItem value="Custom Template">Custom Template</SelectItem>
                  </SelectContent>
                </Select>
              </div>
            </div>
            
            <DialogFooter>
              <Button variant="outline" onClick={() => setCreateProjectOpen(false)}>
                Cancel
              </Button>
              <Button onClick={handleCreateProject}>Create Project</Button>
            </DialogFooter>
          </DialogContent>
        </Dialog>
      </div>

      {/* Project Statistics */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium">Total Projects</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{projects.length}</div>
            <p className="text-xs text-gray-600 mt-1">Across all statuses</p>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium">Active Projects</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {projects.filter(p => p.status === 'processing').length}
            </div>
            <p className="text-xs text-gray-600 mt-1">Currently processing</p>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium">Completed</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {projects.filter(p => p.status === 'completed' || p.status === 'deployed').length}
            </div>
            <p className="text-xs text-gray-600 mt-1">Successfully transpiled</p>
          </CardContent>
        </Card>
        
        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium">Total Lines</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {projects.reduce((acc, p) => acc + (p.metrics?.linesOfCode || 0), 0).toLocaleString()}
            </div>
            <p className="text-xs text-gray-600 mt-1">COBOL code processed</p>
          </CardContent>
        </Card>
      </div>

      {/* Projects Tabs */}
      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList>
          <TabsTrigger value="all">All Projects ({projects.length})</TabsTrigger>
          <TabsTrigger value="draft">Draft ({projects.filter(p => p.status === 'draft').length})</TabsTrigger>
          <TabsTrigger value="ready">Ready ({projects.filter(p => p.status === 'ready').length})</TabsTrigger>
          <TabsTrigger value="processing">Processing ({projects.filter(p => p.status === 'processing').length})</TabsTrigger>
          <TabsTrigger value="completed">Completed ({projects.filter(p => p.status === 'completed').length})</TabsTrigger>
          <TabsTrigger value="deployed">Deployed ({projects.filter(p => p.status === 'deployed').length})</TabsTrigger>
        </TabsList>

        <TabsContent value={activeTab} className="space-y-4">
          {filteredProjects.map((project) => (
            <Card key={project.id}>
              <CardHeader>
                <div className="flex items-start justify-between">
                  <div>
                    <CardTitle className="text-xl">{project.name}</CardTitle>
                    <CardDescription className="mt-1">{project.description}</CardDescription>
                  </div>
                  <div className="flex items-center space-x-2">
                    {getStatusBadge(project.status)}
                    {project.metrics && getComplexityBadge(project.metrics.complexity)}
                  </div>
                </div>
              </CardHeader>
              
              <CardContent className="space-y-4">
                {/* Project Info */}
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4 text-sm">
                  <div>
                    <span className="text-gray-600">Banking System:</span>
                    <p className="font-medium">{project.bankingSystem}</p>
                  </div>
                  <div>
                    <span className="text-gray-600">Target Blockchain:</span>
                    <p className="font-medium">{project.targetBlockchain}</p>
                  </div>
                  <div>
                    <span className="text-gray-600">Template:</span>
                    <p className="font-medium">{project.template}</p>
                  </div>
                  <div>
                    <span className="text-gray-600">Owner:</span>
                    <p className="font-medium">{project.owner}</p>
                  </div>
                </div>

                {/* Files Section */}
                <div>
                  <div className="flex items-center justify-between mb-2">
                    <h4 className="text-sm font-medium flex items-center">
                      <FileCode className="h-4 w-4 mr-1" />
                      COBOL Files ({project.cobolFiles.length})
                    </h4>
                    {project.status === 'draft' && (
                      <Button 
                        variant="outline" 
                        size="sm"
                        onClick={() => {
                          setSelectedProject(project);
                          setUploadFilesOpen(true);
                        }}
                      >
                        <Upload className="h-3 w-3 mr-1" />
                        Upload Files
                      </Button>
                    )}
                  </div>
                  
                  {project.cobolFiles.length > 0 ? (
                    <div className="space-y-1">
                      {project.cobolFiles.map((file) => (
                        <div key={file.id} className="flex items-center justify-between text-sm bg-gray-50 rounded p-2">
                          <div className="flex items-center space-x-2">
                            <FileCode className="h-4 w-4 text-gray-500" />
                            <span>{file.name}</span>
                            <Badge variant="outline" className="text-xs">
                              {file.lineCount} lines
                            </Badge>
                            {file.status === 'validated' ? (
                              <CheckCircle className="h-3 w-3 text-green-500" />
                            ) : file.status === 'invalid' ? (
                              <XCircle className="h-3 w-3 text-red-500" />
                            ) : null}
                          </div>
                          <span className="text-gray-500">{(file.size / 1024).toFixed(1)} KB</span>
                        </div>
                      ))}
                    </div>
                  ) : (
                    <p className="text-sm text-gray-500">No files uploaded yet</p>
                  )}
                </div>

                {/* Progress Section */}
                {project.status === 'processing' && (
                  <div>
                    <div className="flex items-center justify-between mb-2">
                      <span className="text-sm font-medium">Transpilation Progress</span>
                      <span className="text-sm text-gray-600">{project.progress}%</span>
                    </div>
                    <Progress value={project.progress} className="h-2" />
                    {project.estimatedTime && (
                      <p className="text-xs text-gray-500 mt-1">
                        Estimated time remaining: {project.estimatedTime}
                      </p>
                    )}
                  </div>
                )}

                {/* Error Message */}
                {project.status === 'failed' && project.errorMessage && (
                  <Alert className="bg-red-50 border-red-200">
                    <AlertTriangle className="h-4 w-4 text-red-600" />
                    <AlertTitle>Transpilation Failed</AlertTitle>
                    <AlertDescription>{project.errorMessage}</AlertDescription>
                  </Alert>
                )}

                {/* Output Files */}
                {project.outputFiles && project.outputFiles.length > 0 && (
                  <div>
                    <h4 className="text-sm font-medium mb-2 flex items-center">
                      <Package className="h-4 w-4 mr-1" />
                      Generated Files
                    </h4>
                    <div className="space-y-1">
                      {project.outputFiles.map((file) => (
                        <div key={file.id} className="flex items-center justify-between text-sm bg-green-50 rounded p-2">
                          <div className="flex items-center space-x-2">
                            <Code2 className="h-4 w-4 text-green-600" />
                            <span>{file.name}</span>
                            <Badge variant="outline" className="text-xs">
                              {file.type}
                            </Badge>
                          </div>
                          <Button 
                            variant="ghost" 
                            size="sm"
                            onClick={() => handleDownloadOutput(file)}
                          >
                            <Download className="h-3 w-3" />
                          </Button>
                        </div>
                      ))}
                    </div>
                  </div>
                )}

                {/* Deployment Info */}
                {project.deploymentInfo && (
                  <div className="bg-purple-50 rounded p-3">
                    <h4 className="text-sm font-medium mb-2 flex items-center">
                      <Zap className="h-4 w-4 mr-1 text-purple-600" />
                      Deployment Information
                    </h4>
                    <div className="grid grid-cols-2 gap-2 text-sm">
                      <div>
                        <span className="text-gray-600">Network:</span>
                        <p className="font-medium">{project.deploymentInfo.network}</p>
                      </div>
                      <div>
                        <span className="text-gray-600">Contract Address:</span>
                        <p className="font-medium font-mono text-xs">
                          {project.deploymentInfo.contractAddress}
                        </p>
                      </div>
                    </div>
                    {project.deploymentInfo.explorerUrl && (
                      <Button variant="link" size="sm" className="mt-2 p-0 h-auto">
                        View on Explorer
                        <ExternalLink className="h-3 w-3 ml-1" />
                      </Button>
                    )}
                  </div>
                )}

                {/* Action Buttons */}
                <div className="flex justify-between pt-2">
                  <div className="flex space-x-2">
                    <Button 
                      variant="outline" 
                      size="sm"
                      onClick={() => {
                        setSelectedProject(project);
                        setViewDetailsOpen(true);
                      }}
                    >
                      <Eye className="h-4 w-4 mr-1" />
                      View Details
                    </Button>
                    {project.status === 'draft' && project.cobolFiles.length > 0 && (
                      <Button 
                        variant="outline" 
                        size="sm"
                        onClick={() => handleStartTranspilation(project)}
                      >
                        <Settings className="h-4 w-4 mr-1" />
                        Configure
                      </Button>
                    )}
                    {project.status === 'completed' && (
                      <Button 
                        variant="outline" 
                        size="sm"
                        onClick={() => handleDeployContract(project)}
                      >
                        <Zap className="h-4 w-4 mr-1" />
                        Deploy
                      </Button>
                    )}
                  </div>
                  <div className="flex space-x-2">
                    {project.status === 'ready' && (
                      <Button 
                        size="sm"
                        onClick={() => handleStartTranspilation(project)}
                      >
                        <Play className="h-4 w-4 mr-1" />
                        Start Transpilation
                      </Button>
                    )}
                    {project.status === 'processing' && (
                      <Button 
                        size="sm"
                        variant="destructive"
                      >
                        <StopCircle className="h-4 w-4 mr-1" />
                        Stop
                      </Button>
                    )}
                    {project.status === 'failed' && (
                      <Button 
                        size="sm"
                        onClick={() => handleStartTranspilation(project)}
                      >
                        <RefreshCw className="h-4 w-4 mr-1" />
                        Retry
                      </Button>
                    )}
                  </div>
                </div>
              </CardContent>
            </Card>
          ))}

          {filteredProjects.length === 0 && (
            <Card>
              <CardContent className="text-center py-12">
                <FolderOpen className="h-12 w-12 text-gray-400 mx-auto mb-4" />
                <h3 className="text-lg font-medium text-gray-900 mb-2">No projects found</h3>
                <p className="text-gray-600 mb-4">
                  {activeTab === 'all' 
                    ? 'Create your first transpilation project to get started.'
                    : `No projects with status "${activeTab}".`}
                </p>
                <Button onClick={() => setCreateProjectOpen(true)}>
                  <Plus className="h-4 w-4 mr-2" />
                  Create New Project
                </Button>
              </CardContent>
            </Card>
          )}
        </TabsContent>
      </Tabs>

      {/* View Details Dialog */}
      <Dialog open={viewDetailsOpen} onOpenChange={setViewDetailsOpen}>
        <DialogContent className="max-w-4xl max-h-[80vh] overflow-auto">
          <DialogHeader>
            <DialogTitle>Project Details - {selectedProject?.name}</DialogTitle>
            <DialogDescription>
              Complete information about your transpilation project
            </DialogDescription>
          </DialogHeader>
          
          {selectedProject && (
            <Tabs defaultValue="overview" className="space-y-4">
              <TabsList>
                <TabsTrigger value="overview">Overview</TabsTrigger>
                <TabsTrigger value="files">Files</TabsTrigger>
                <TabsTrigger value="configuration">Configuration</TabsTrigger>
                <TabsTrigger value="output">Output</TabsTrigger>
                <TabsTrigger value="logs">Logs</TabsTrigger>
              </TabsList>
              
              <TabsContent value="overview" className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label>Project ID</Label>
                    <p className="text-sm text-gray-600">{selectedProject.id}</p>
                  </div>
                  <div>
                    <Label>Status</Label>
                    <div className="mt-1">{getStatusBadge(selectedProject.status)}</div>
                  </div>
                  <div>
                    <Label>Created</Label>
                    <p className="text-sm text-gray-600">
                      {new Date(selectedProject.createdAt).toLocaleString()}
                    </p>
                  </div>
                  <div>
                    <Label>Last Updated</Label>
                    <p className="text-sm text-gray-600">
                      {new Date(selectedProject.updatedAt).toLocaleString()}
                    </p>
                  </div>
                </div>
                
                {selectedProject.metrics && (
                  <div>
                    <Label>Project Metrics</Label>
                    <div className="grid grid-cols-3 gap-4 mt-2">
                      <Card>
                        <CardContent className="pt-4">
                          <div className="text-2xl font-bold">
                            {selectedProject.metrics.linesOfCode.toLocaleString()}
                          </div>
                          <p className="text-xs text-gray-600">Lines of Code</p>
                        </CardContent>
                      </Card>
                      <Card>
                        <CardContent className="pt-4">
                          <div className="text-2xl font-bold">
                            {selectedProject.metrics.functions || 0}
                          </div>
                          <p className="text-xs text-gray-600">Functions</p>
                        </CardContent>
                      </Card>
                      <Card>
                        <CardContent className="pt-4">
                          <div className="text-2xl font-bold">
                            {selectedProject.metrics.events || 0}
                          </div>
                          <p className="text-xs text-gray-600">Events</p>
                        </CardContent>
                      </Card>
                    </div>
                  </div>
                )}
              </TabsContent>
              
              <TabsContent value="files" className="space-y-4">
                <div>
                  <h4 className="font-medium mb-2">COBOL Source Files</h4>
                  {selectedProject.cobolFiles.map((file) => (
                    <Card key={file.id} className="mb-2">
                      <CardContent className="pt-4">
                        <div className="flex items-center justify-between">
                          <div>
                            <p className="font-medium">{file.name}</p>
                            <p className="text-sm text-gray-600">
                              Program ID: {file.programId || 'N/A'} | 
                              {file.lineCount} lines | 
                              {(file.size / 1024).toFixed(1)} KB
                            </p>
                          </div>
                          <Button variant="outline" size="sm">
                            <Eye className="h-4 w-4 mr-1" />
                            View
                          </Button>
                        </div>
                        {file.validationErrors && (
                          <Alert className="mt-2 bg-red-50 border-red-200">
                            <AlertTriangle className="h-4 w-4 text-red-600" />
                            <AlertTitle>Validation Errors</AlertTitle>
                            <AlertDescription>
                              <ul className="list-disc list-inside">
                                {file.validationErrors.map((error, i) => (
                                  <li key={i}>{error}</li>
                                ))}
                              </ul>
                            </AlertDescription>
                          </Alert>
                        )}
                      </CardContent>
                    </Card>
                  ))}
                </div>
              </TabsContent>
              
              <TabsContent value="configuration" className="space-y-4">
                <div>
                  <Label>Banking System</Label>
                  <p className="text-sm text-gray-600 mt-1">{selectedProject.bankingSystem}</p>
                </div>
                <div>
                  <Label>Target Blockchain</Label>
                  <p className="text-sm text-gray-600 mt-1">{selectedProject.targetBlockchain}</p>
                </div>
                <div>
                  <Label>Template</Label>
                  <p className="text-sm text-gray-600 mt-1">{selectedProject.template}</p>
                </div>
                {selectedProject.configFile && (
                  <div>
                    <Label>Configuration File</Label>
                    <Card className="mt-2">
                      <CardContent className="pt-4">
                        <pre className="text-sm bg-gray-50 p-3 rounded overflow-x-auto">
                          <code>{selectedProject.configFile.content}</code>
                        </pre>
                      </CardContent>
                    </Card>
                  </div>
                )}
              </TabsContent>
              
              <TabsContent value="output" className="space-y-4">
                {selectedProject.outputFiles && selectedProject.outputFiles.length > 0 ? (
                  <div className="space-y-2">
                    {selectedProject.outputFiles.map((file) => (
                      <Card key={file.id}>
                        <CardContent className="pt-4">
                          <div className="flex items-center justify-between">
                            <div>
                              <p className="font-medium">{file.name}</p>
                              <p className="text-sm text-gray-600">
                                Type: {file.type} | Size: {(file.size / 1024).toFixed(1)} KB
                              </p>
                              <p className="text-xs text-gray-500">
                                Generated: {new Date(file.generatedAt).toLocaleString()}
                              </p>
                            </div>
                            <div className="flex space-x-2">
                              <Button variant="outline" size="sm">
                                <Eye className="h-4 w-4 mr-1" />
                                Preview
                              </Button>
                              <Button size="sm" onClick={() => handleDownloadOutput(file)}>
                                <Download className="h-4 w-4 mr-1" />
                                Download
                              </Button>
                            </div>
                          </div>
                        </CardContent>
                      </Card>
                    ))}
                  </div>
                ) : (
                  <p className="text-gray-500 text-center py-8">
                    No output files generated yet
                  </p>
                )}
              </TabsContent>
              
              <TabsContent value="logs" className="space-y-4">
                <Card>
                  <CardContent className="pt-4">
                    <div className="bg-gray-900 text-gray-100 p-4 rounded font-mono text-sm">
                      <p>[2025-07-05 09:00:00] Project created</p>
                      <p>[2025-07-05 09:05:00] COBOL files uploaded</p>
                      <p>[2025-07-05 09:06:00] File validation completed</p>
                      <p>[2025-07-05 09:10:00] Transpilation started</p>
                      <p>[2025-07-05 09:15:00] Template processing...</p>
                      <p className="text-yellow-400">[2025-07-05 09:15:30] Warning: Complex nested PERFORM statements detected</p>
                      <p>[2025-07-05 09:20:00] Smart contract generation in progress...</p>
                    </div>
                  </CardContent>
                </Card>
              </TabsContent>
            </Tabs>
          )}
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setViewDetailsOpen(false)}>
              Close
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Upload Files Dialog */}
      <Dialog open={uploadFilesOpen} onOpenChange={setUploadFilesOpen}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle>Upload COBOL Files</DialogTitle>
            <DialogDescription>
              Upload COBOL source files and configuration for {selectedProject?.name}
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-4">
            <div
              {...getRootProps()}
              className={`border-2 border-dashed rounded-lg p-8 text-center cursor-pointer transition-colors ${
                isDragActive ? 'border-blue-500 bg-blue-50' : 'border-gray-300 hover:border-gray-400'
              }`}
            >
              <input {...getInputProps()} />
              <Upload className="h-12 w-12 mx-auto text-gray-400 mb-3" />
              {isDragActive ? (
                <p>Drop the COBOL files here...</p>
              ) : (
                <div>
                  <p className="font-medium">Drag & drop COBOL files here, or click to select</p>
                  <p className="text-sm text-gray-500 mt-1">
                    Supports .cbl, .cob, .cobol files and .yaml/.yml configuration files
                  </p>
                </div>
              )}
            </div>
            
            <Alert>
              <AlertTriangle className="h-4 w-4" />
              <AlertTitle>File Requirements</AlertTitle>
              <AlertDescription>
                <ul className="list-disc list-inside text-sm mt-1">
                  <li>COBOL files must have valid IDENTIFICATION, DATA, and PROCEDURE divisions</li>
                  <li>Maximum file size: 10MB per file</li>
                  <li>Configuration files must be in YAML format</li>
                  <li>All files will be validated before processing</li>
                </ul>
              </AlertDescription>
            </Alert>
          </div>
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setUploadFilesOpen(false)}>
              Cancel
            </Button>
            <Button>Upload Files</Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>

      {/* Deployment Dialog */}
      <Dialog open={deploymentOpen} onOpenChange={setDeploymentOpen}>
        <DialogContent className="max-w-2xl">
          <DialogHeader>
            <DialogTitle>Deploy Smart Contract</DialogTitle>
            <DialogDescription>
              Deploy your transpiled smart contract to {selectedProject?.targetBlockchain}
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-4">
            <div>
              <Label>Target Network</Label>
              <Select>
                <SelectTrigger>
                  <SelectValue placeholder="Select deployment network" />
                </SelectTrigger>
                <SelectContent>
                  {selectedProject?.targetBlockchain === 'ethereum' && (
                    <>
                      <SelectItem value="mainnet">Ethereum Mainnet</SelectItem>
                      <SelectItem value="goerli">Goerli Testnet</SelectItem>
                      <SelectItem value="sepolia">Sepolia Testnet</SelectItem>
                    </>
                  )}
                  {selectedProject?.targetBlockchain === 'corda' && (
                    <>
                      <SelectItem value="corda-prod">Corda Production Network</SelectItem>
                      <SelectItem value="corda-test">Corda Test Network</SelectItem>
                    </>
                  )}
                  {selectedProject?.targetBlockchain === 'xrp' && (
                    <>
                      <SelectItem value="xrp-mainnet">XRP Ledger Mainnet</SelectItem>
                      <SelectItem value="xrp-testnet">XRP Ledger Testnet</SelectItem>
                    </>
                  )}
                  {selectedProject?.targetBlockchain === 'algorand' && (
                    <>
                      <SelectItem value="algo-mainnet">Algorand MainNet</SelectItem>
                      <SelectItem value="algo-testnet">Algorand TestNet</SelectItem>
                    </>
                  )}
                </SelectContent>
              </Select>
            </div>
            
            <div>
              <Label>Deployment Parameters</Label>
              <Textarea 
                placeholder="Enter any deployment parameters (JSON format)"
                rows={4}
                className="font-mono text-sm"
              />
            </div>
            
            <Alert>
              <Zap className="h-4 w-4" />
              <AlertTitle>Deployment Costs</AlertTitle>
              <AlertDescription>
                <div className="mt-2 space-y-1 text-sm">
                  <p>Estimated gas cost: 2,500,000 gas</p>
                  <p>Network fee: ~0.05 ETH ($125 USD)</p>
                  <p>Total deployment time: ~2-3 minutes</p>
                </div>
              </AlertDescription>
            </Alert>
          </div>
          
          <DialogFooter>
            <Button variant="outline" onClick={() => setDeploymentOpen(false)}>
              Cancel
            </Button>
            <Button>
              <Zap className="h-4 w-4 mr-2" />
              Deploy Contract
            </Button>
          </DialogFooter>
        </DialogContent>
      </Dialog>
    </div>
  );
}