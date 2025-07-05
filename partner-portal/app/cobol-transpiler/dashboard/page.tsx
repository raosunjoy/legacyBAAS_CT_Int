'use client';

import { useEffect, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Progress } from '@/components/ui/progress';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { 
  BarChart, 
  Bar, 
  XAxis, 
  YAxis, 
  CartesianGrid, 
  Tooltip, 
  ResponsiveContainer,
  LineChart,
  Line,
  PieChart,
  Pie,
  Cell
} from 'recharts';
import { 
  Code, 
  Zap, 
  TrendingUp, 
  DollarSign, 
  Clock, 
  CheckCircle, 
  XCircle, 
  AlertTriangle,
  Download,
  RefreshCw,
  Calendar,
  Target
} from 'lucide-react';

interface TranspilationProject {
  id: string;
  name: string;
  status: 'active' | 'completed' | 'failed' | 'pending';
  bankingSystem: 'FIS_SYSTEMATICS' | 'FISERV_DNA' | 'TCS_BANCS' | 'TEMENOS_TRANSACT';
  targetBlockchain: string;
  createdAt: string;
  completedAt?: string;
  cobolFiles: number;
  smartContracts: number;
  testsPassing: number;
  totalTests: number;
  gasOptimized: boolean;
  revenue: number;
}

interface UsageMetrics {
  totalTranspilations: number;
  successfulTranspilations: number;
  failedTranspilations: number;
  quotaUsed: number;
  quotaLimit: number;
  currentMonthUsage: number;
  projectedMonthlyUsage: number;
  successRate: number;
  averageTranspilationTime: number;
  totalRevenue: number;
  monthlyRevenue: number;
}

interface AnalyticsData {
  date: string;
  transpilations: number;
  success: number;
  failed: number;
  revenue: number;
}

const COLORS = ['#3b82f6', '#10b981', '#f59e0b', '#ef4444', '#8b5cf6'];

export default function COBOLDashboard() {
  const [projects, setProjects] = useState<TranspilationProject[]>([]);
  const [metrics, setMetrics] = useState<UsageMetrics | null>(null);
  const [analytics, setAnalytics] = useState<AnalyticsData[]>([]);
  const [loading, setLoading] = useState(true);
  const [selectedTimeframe, setSelectedTimeframe] = useState('7d');

  // Mock data - In production, this would fetch from the API
  useEffect(() => {
    const fetchDashboardData = async () => {
      setLoading(true);
      
      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      // Mock projects data
      const mockProjects: TranspilationProject[] = [
        {
          id: '1',
          name: 'Mortgage Processing Modernization',
          status: 'completed',
          bankingSystem: 'FIS_SYSTEMATICS',
          targetBlockchain: 'corda',
          createdAt: '2025-07-01T10:00:00Z',
          completedAt: '2025-07-03T15:30:00Z',
          cobolFiles: 25,
          smartContracts: 25,
          testsPassing: 124,
          totalTests: 124,
          gasOptimized: true,
          revenue: 12500
        },
        {
          id: '2',
          name: 'Payment Gateway Integration',
          status: 'active',
          bankingSystem: 'FISERV_DNA',
          targetBlockchain: 'ethereum',
          createdAt: '2025-07-04T09:00:00Z',
          cobolFiles: 15,
          smartContracts: 12,
          testsPassing: 89,
          totalTests: 96,
          gasOptimized: true,
          revenue: 8750
        },
        {
          id: '3',
          name: 'Trade Finance Automation',
          status: 'pending',
          bankingSystem: 'TCS_BANCS',
          targetBlockchain: 'algorand',
          createdAt: '2025-07-05T14:00:00Z',
          cobolFiles: 18,
          smartContracts: 0,
          testsPassing: 0,
          totalTests: 72,
          gasOptimized: false,
          revenue: 0
        },
        {
          id: '4',
          name: 'Cross-Border Remittance',
          status: 'completed',
          bankingSystem: 'TEMENOS_TRANSACT',
          targetBlockchain: 'xrp',
          createdAt: '2025-06-28T11:00:00Z',
          completedAt: '2025-07-02T16:45:00Z',
          cobolFiles: 32,
          smartContracts: 32,
          testsPassing: 156,
          totalTests: 156,
          gasOptimized: true,
          revenue: 16000
        }
      ];

      // Mock metrics data
      const mockMetrics: UsageMetrics = {
        totalTranspilations: 847,
        successfulTranspilations: 782,
        failedTranspilations: 65,
        quotaUsed: 782,
        quotaLimit: 1000,
        currentMonthUsage: 234,
        projectedMonthlyUsage: 312,
        successRate: 92.3,
        averageTranspilationTime: 147, // seconds
        totalRevenue: 156750,
        monthlyRevenue: 37250
      };

      // Mock analytics data
      const mockAnalytics: AnalyticsData[] = [
        { date: '2025-06-29', transpilations: 45, success: 42, failed: 3, revenue: 5250 },
        { date: '2025-06-30', transpilations: 52, success: 48, failed: 4, revenue: 6200 },
        { date: '2025-07-01', transpilations: 38, success: 36, failed: 2, revenue: 4750 },
        { date: '2025-07-02', transpilations: 61, success: 56, failed: 5, revenue: 7125 },
        { date: '2025-07-03', transpilations: 49, success: 45, failed: 4, revenue: 5875 },
        { date: '2025-07-04', transpilations: 55, success: 51, failed: 4, revenue: 6250 },
        { date: '2025-07-05', transpilations: 43, success: 40, failed: 3, revenue: 5000 }
      ];

      setProjects(mockProjects);
      setMetrics(mockMetrics);
      setAnalytics(mockAnalytics);
      setLoading(false);
    };

    fetchDashboardData();
  }, [selectedTimeframe]);

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'completed':
        return <CheckCircle className="h-4 w-4 text-green-500" />;
      case 'active':
        return <Clock className="h-4 w-4 text-blue-500" />;
      case 'failed':
        return <XCircle className="h-4 w-4 text-red-500" />;
      case 'pending':
        return <AlertTriangle className="h-4 w-4 text-yellow-500" />;
      default:
        return null;
    }
  };

  const getStatusBadge = (status: string) => {
    const variants = {
      completed: 'bg-green-100 text-green-800',
      active: 'bg-blue-100 text-blue-800',
      failed: 'bg-red-100 text-red-800',
      pending: 'bg-yellow-100 text-yellow-800'
    };
    
    return (
      <Badge className={variants[status as keyof typeof variants]}>
        {status.charAt(0).toUpperCase() + status.slice(1)}
      </Badge>
    );
  };

  const bankingSystemData = projects.reduce((acc, project) => {
    const existing = acc.find(item => item.name === project.bankingSystem);
    if (existing) {
      existing.value += 1;
    } else {
      acc.push({ name: project.bankingSystem, value: 1 });
    }
    return acc;
  }, [] as Array<{ name: string; value: number }>);

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <RefreshCw className="h-8 w-8 animate-spin text-blue-500" />
      </div>
    );
  }

  return (
    <div className="container mx-auto p-6 space-y-8">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">COBOL Transpiler Dashboard</h1>
          <p className="text-gray-600 mt-2">Monitor and manage your COBOL modernization projects</p>
        </div>
        <div className="flex items-center gap-3">
          <Button variant="outline" size="sm">
            <Download className="h-4 w-4 mr-2" />
            Export Report
          </Button>
          <Button variant="outline" size="sm" onClick={() => window.location.reload()}>
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
        </div>
      </div>

      {/* Key Metrics Overview */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Transpilations</CardTitle>
            <Code className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{metrics?.totalTranspilations}</div>
            <p className="text-xs text-muted-foreground">
              {metrics?.successfulTranspilations} successful, {metrics?.failedTranspilations} failed
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Success Rate</CardTitle>
            <TrendingUp className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{metrics?.successRate}%</div>
            <div className="w-full bg-gray-200 rounded-full h-2 mt-2">
              <div 
                className="bg-green-500 h-2 rounded-full" 
                style={{ width: `${metrics?.successRate}%` }}
              />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Quota Usage</CardTitle>
            <Target className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {metrics?.quotaUsed}/{metrics?.quotaLimit}
            </div>
            <Progress value={(metrics?.quotaUsed / metrics?.quotaLimit) * 100} className="mt-2" />
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Monthly Revenue</CardTitle>
            <DollarSign className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">${metrics?.monthlyRevenue.toLocaleString()}</div>
            <p className="text-xs text-muted-foreground">
              Total: ${metrics?.totalRevenue.toLocaleString()}
            </p>
          </CardContent>
        </Card>
      </div>

      {/* Main Content Tabs */}
      <Tabs defaultValue="projects" className="space-y-6">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="projects">Projects</TabsTrigger>
          <TabsTrigger value="analytics">Analytics</TabsTrigger>
          <TabsTrigger value="usage">Usage</TabsTrigger>
          <TabsTrigger value="revenue">Revenue</TabsTrigger>
        </TabsList>

        {/* Projects Tab */}
        <TabsContent value="projects" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle>Recent Transpilation Projects</CardTitle>
              <CardDescription>
                Monitor the status and progress of your COBOL modernization projects
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {projects.map((project) => (
                  <div key={project.id} className="flex items-center justify-between p-4 border rounded-lg">
                    <div className="flex items-center space-x-4">
                      {getStatusIcon(project.status)}
                      <div>
                        <h3 className="font-semibold">{project.name}</h3>
                        <p className="text-sm text-gray-600">
                          {project.bankingSystem} → {project.targetBlockchain}
                        </p>
                      </div>
                    </div>
                    <div className="flex items-center space-x-4">
                      <div className="text-right">
                        <p className="text-sm font-medium">{project.cobolFiles} COBOL files</p>
                        <p className="text-sm text-gray-600">{project.smartContracts} contracts generated</p>
                      </div>
                      <div className="text-right">
                        <p className="text-sm font-medium">{project.testsPassing}/{project.totalTests} tests</p>
                        <Progress value={(project.testsPassing / project.totalTests) * 100} className="w-20" />
                      </div>
                      {getStatusBadge(project.status)}
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Analytics Tab */}
        <TabsContent value="analytics" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle>Daily Transpilation Trend</CardTitle>
                <CardDescription>Success vs failure rates over time</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <LineChart data={analytics}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="date" />
                    <YAxis />
                    <Tooltip />
                    <Line type="monotone" dataKey="success" stroke="#10b981" strokeWidth={2} />
                    <Line type="monotone" dataKey="failed" stroke="#ef4444" strokeWidth={2} />
                  </LineChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Banking System Distribution</CardTitle>
                <CardDescription>Projects by core banking system</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <PieChart>
                    <Pie
                      data={bankingSystemData}
                      cx="50%"
                      cy="50%"
                      labelLine={false}
                      label={({ name, percent }) => `${name} ${(percent * 100).toFixed(0)}%`}
                      outerRadius={80}
                      fill="#8884d8"
                      dataKey="value"
                    >
                      {bankingSystemData.map((entry, index) => (
                        <Cell key={`cell-${index}`} fill={COLORS[index % COLORS.length]} />
                      ))}
                    </Pie>
                    <Tooltip />
                  </PieChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Usage Tab */}
        <TabsContent value="usage" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle>Monthly Usage Tracking</CardTitle>
                <CardDescription>Monitor your transpilation quota and usage patterns</CardDescription>
              </CardHeader>
              <CardContent className="space-y-6">
                <div className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span>Current Month Usage</span>
                    <span>{metrics?.currentMonthUsage}/{metrics?.quotaLimit}</span>
                  </div>
                  <Progress value={(metrics?.currentMonthUsage / metrics?.quotaLimit) * 100} />
                </div>
                
                <div className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span>Projected Monthly Usage</span>
                    <span className={`${metrics?.projectedMonthlyUsage > metrics?.quotaLimit ? 'text-red-600' : 'text-green-600'}`}>
                      {metrics?.projectedMonthlyUsage}
                    </span>
                  </div>
                  <Progress 
                    value={(metrics?.projectedMonthlyUsage / metrics?.quotaLimit) * 100} 
                    className={metrics?.projectedMonthlyUsage > metrics?.quotaLimit ? 'bg-red-100' : ''} 
                  />
                </div>

                <div className="pt-4 border-t">
                  <p className="text-sm text-gray-600">
                    Average transpilation time: <span className="font-medium">{metrics?.averageTranspilationTime}s</span>
                  </p>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Performance Metrics</CardTitle>
                <CardDescription>Detailed breakdown of transpilation performance</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Success Rate</span>
                    <div className="flex items-center space-x-2">
                      <div className="w-32 bg-gray-200 rounded-full h-2">
                        <div 
                          className="bg-green-500 h-2 rounded-full" 
                          style={{ width: `${metrics?.successRate}%` }}
                        />
                      </div>
                      <span className="text-sm font-medium">{metrics?.successRate}%</span>
                    </div>
                  </div>
                  
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Gas Optimization Rate</span>
                    <div className="flex items-center space-x-2">
                      <div className="w-32 bg-gray-200 rounded-full h-2">
                        <div className="bg-blue-500 h-2 rounded-full" style={{ width: '85%' }} />
                      </div>
                      <span className="text-sm font-medium">85%</span>
                    </div>
                  </div>
                  
                  <div className="flex justify-between items-center">
                    <span className="text-sm">Test Coverage</span>
                    <div className="flex items-center space-x-2">
                      <div className="w-32 bg-gray-200 rounded-full h-2">
                        <div className="bg-purple-500 h-2 rounded-full" style={{ width: '94%' }} />
                      </div>
                      <span className="text-sm font-medium">94%</span>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Revenue Tab */}
        <TabsContent value="revenue" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle>Revenue Analytics</CardTitle>
              <CardDescription>Track revenue generation from COBOL transpilation projects</CardDescription>
            </CardHeader>
            <CardContent>
              <ResponsiveContainer width="100%" height={400}>
                <BarChart data={analytics}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="date" />
                  <YAxis />
                  <Tooltip formatter={(value) => [`$${value}`, 'Revenue']} />
                  <Bar dataKey="revenue" fill="#3b82f6" />
                </BarChart>
              </ResponsiveContainer>
            </CardContent>
          </Card>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <Card>
              <CardHeader>
                <CardTitle>This Month</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">${metrics?.monthlyRevenue.toLocaleString()}</div>
                <p className="text-sm text-green-600">+23% from last month</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Total Revenue</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">${metrics?.totalRevenue.toLocaleString()}</div>
                <p className="text-sm text-gray-600">Lifetime earnings</p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Avg. Project Value</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">$10,687</div>
                <p className="text-sm text-blue-600">+12% improvement</p>
              </CardContent>
            </Card>
          </div>
        </TabsContent>
      </Tabs>
    </div>
  );
}