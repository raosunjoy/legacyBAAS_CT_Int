'use client';

import { useEffect, useState } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { 
  TrendingUp,
  TrendingDown,
  BarChart3,
  PieChart,
  Activity,
  Users,
  Clock,
  Zap,
  DollarSign,
  Target,
  AlertTriangle,
  CheckCircle,
  Download,
  RefreshCw,
  Calendar,
  Filter,
  Eye,
  Settings,
  FileText,
  Code2,
  Layers,
  GitBranch,
  Package,
  Globe,
  Server,
  Database,
  Shield
} from 'lucide-react';
import { 
  LineChart, 
  Line, 
  AreaChart, 
  Area, 
  BarChart, 
  Bar, 
  PieChart as RechartsPieChart,
  Cell,
  ComposedChart,
  XAxis, 
  YAxis, 
  CartesianGrid, 
  Tooltip, 
  ResponsiveContainer, 
  Legend,
  RadialBarChart,
  RadialBar,
  Treemap
} from 'recharts';

interface UsageAnalytics {
  summary: {
    totalTranspilations: number;
    totalCustomers: number;
    averageProcessingTime: number;
    successRate: number;
    peakConcurrentUsers: number;
    totalLinesProcessed: number;
    costPerTranspilation: number;
    revenueGenerated: number;
  };
  trends: {
    daily: DailyUsage[];
    monthly: MonthlyUsage[];
    hourly: HourlyUsage[];
  };
  performance: {
    responseTime: PerformanceMetric[];
    throughput: PerformanceMetric[];
    errorRates: PerformanceMetric[];
    resourceUtilization: ResourceMetric[];
  };
  customers: {
    topCustomers: CustomerMetric[];
    customerGrowth: GrowthMetric[];
    churnAnalysis: ChurnMetric[];
  };
  banking: {
    systemDistribution: BankingSystemMetric[];
    blockchainUsage: BlockchainMetric[];
    templatePopularity: TemplateMetric[];
  };
  forecasting: {
    usagePrediction: ForecastMetric[];
    capacityPlanning: CapacityMetric[];
    costOptimization: OptimizationMetric[];
  };
}

interface DailyUsage {
  date: string;
  transpilations: number;
  uniqueUsers: number;
  averageTime: number;
  successRate: number;
  revenue: number;
}

interface MonthlyUsage {
  month: string;
  transpilations: number;
  customers: number;
  revenue: number;
  growth: number;
}

interface HourlyUsage {
  hour: number;
  usage: number;
  peak: boolean;
}

interface PerformanceMetric {
  timestamp: string;
  value: number;
  threshold: number;
  status: 'good' | 'warning' | 'critical';
}

interface ResourceMetric {
  resource: string;
  usage: number;
  capacity: number;
  trend: 'up' | 'down' | 'stable';
}

interface CustomerMetric {
  customerId: string;
  name: string;
  transpilations: number;
  revenue: number;
  tier: string;
  growth: number;
}

interface GrowthMetric {
  period: string;
  newCustomers: number;
  totalCustomers: number;
  growthRate: number;
}

interface ChurnMetric {
  period: string;
  churnedCustomers: number;
  churnRate: number;
  reason: string;
}

interface BankingSystemMetric {
  system: string;
  usage: number;
  revenue: number;
  customers: number;
  avgComplexity: number;
}

interface BlockchainMetric {
  blockchain: string;
  transactions: number;
  gasUsed: number;
  successRate: number;
  avgCost: number;
}

interface TemplateMetric {
  templateId: string;
  name: string;
  downloads: number;
  usage: number;
  rating: number;
  category: string;
}

interface ForecastMetric {
  period: string;
  predicted: number;
  confidence: number;
  trend: 'increasing' | 'decreasing' | 'stable';
}

interface CapacityMetric {
  resource: string;
  current: number;
  predicted: number;
  recommended: number;
  timeToCapacity: number;
}

interface OptimizationMetric {
  area: string;
  currentCost: number;
  optimizedCost: number;
  savings: number;
  effort: 'low' | 'medium' | 'high';
}

export default function CobolUsageAnalytics() {
  const [loading, setLoading] = useState(true);
  const [analytics, setAnalytics] = useState<UsageAnalytics | null>(null);
  const [selectedPeriod, setSelectedPeriod] = useState('30d');
  const [selectedMetric, setSelectedMetric] = useState('transpilations');
  const [activeTab, setActiveTab] = useState('overview');
  const [autoRefresh, setAutoRefresh] = useState(true);

  // Mock data generation
  useEffect(() => {
    const fetchAnalytics = async () => {
      setLoading(true);
      
      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      const mockAnalytics: UsageAnalytics = {
        summary: {
          totalTranspilations: 12847,
          totalCustomers: 147,
          averageProcessingTime: 2.3,
          successRate: 98.7,
          peakConcurrentUsers: 89,
          totalLinesProcessed: 2847293,
          costPerTranspilation: 3.47,
          revenueGenerated: 445892
        },
        trends: {
          daily: generateDailyData(),
          monthly: generateMonthlyData(),
          hourly: generateHourlyData()
        },
        performance: {
          responseTime: generatePerformanceData('response_time'),
          throughput: generatePerformanceData('throughput'),
          errorRates: generatePerformanceData('error_rate'),
          resourceUtilization: generateResourceData()
        },
        customers: {
          topCustomers: generateCustomerData(),
          customerGrowth: generateGrowthData(),
          churnAnalysis: generateChurnData()
        },
        banking: {
          systemDistribution: generateBankingData(),
          blockchainUsage: generateBlockchainData(),
          templatePopularity: generateTemplateData()
        },
        forecasting: {
          usagePrediction: generateForecastData(),
          capacityPlanning: generateCapacityData(),
          costOptimization: generateOptimizationData()
        }
      };

      setAnalytics(mockAnalytics);
      setLoading(false);
    };

    fetchAnalytics();

    // Auto-refresh setup
    let interval: NodeJS.Timeout;
    if (autoRefresh) {
      interval = setInterval(fetchAnalytics, 30000); // Refresh every 30 seconds
    }

    return () => {
      if (interval) clearInterval(interval);
    };
  }, [selectedPeriod, autoRefresh]);

  const generateDailyData = (): DailyUsage[] => {
    const data = [];
    for (let i = 29; i >= 0; i--) {
      const date = new Date();
      date.setDate(date.getDate() - i);
      data.push({
        date: date.toISOString().split('T')[0],
        transpilations: Math.floor(Math.random() * 200) + 100,
        uniqueUsers: Math.floor(Math.random() * 50) + 25,
        averageTime: Math.random() * 2 + 1.5,
        successRate: Math.random() * 5 + 95,
        revenue: Math.floor(Math.random() * 2000) + 1000
      });
    }
    return data;
  };

  const generateMonthlyData = (): MonthlyUsage[] => {
    const months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
    return months.slice(0, 7).map((month, index) => ({
      month,
      transpilations: Math.floor(Math.random() * 2000) + (index * 200) + 1000,
      customers: Math.floor(Math.random() * 30) + (index * 5) + 50,
      revenue: Math.floor(Math.random() * 50000) + (index * 10000) + 20000,
      growth: Math.random() * 20 + 5
    }));
  };

  const generateHourlyData = (): HourlyUsage[] => {
    return Array.from({ length: 24 }, (_, hour) => ({
      hour,
      usage: Math.floor(Math.random() * 100) + (hour >= 9 && hour <= 17 ? 150 : 50),
      peak: hour >= 9 && hour <= 11 || hour >= 14 && hour <= 16
    }));
  };

  const generatePerformanceData = (type: string): PerformanceMetric[] => {
    const baseValue = type === 'response_time' ? 2.5 : type === 'throughput' ? 150 : 1.2;
    const threshold = type === 'response_time' ? 5.0 : type === 'throughput' ? 100 : 5.0;
    
    return Array.from({ length: 24 }, (_, i) => {
      const value = baseValue + (Math.random() - 0.5) * baseValue * 0.4;
      return {
        timestamp: new Date(Date.now() - (24 - i) * 3600000).toISOString(),
        value,
        threshold,
        status: value > threshold ? 'critical' : value > threshold * 0.8 ? 'warning' : 'good'
      };
    });
  };

  const generateResourceData = (): ResourceMetric[] => {
    return [
      { resource: 'CPU', usage: 67, capacity: 100, trend: 'up' },
      { resource: 'Memory', usage: 84, capacity: 100, trend: 'stable' },
      { resource: 'Storage', usage: 45, capacity: 100, trend: 'up' },
      { resource: 'Network', usage: 32, capacity: 100, trend: 'down' },
      { resource: 'Database', usage: 71, capacity: 100, trend: 'stable' }
    ];
  };

  const generateCustomerData = (): CustomerMetric[] => {
    const customers = [
      'Global Bank Corp', 'Regional Credit Union', 'Investment Partners LLC',
      'Community Bank Group', 'Financial Services Inc', 'Banking Solutions Ltd'
    ];
    
    return customers.map((name, index) => ({
      customerId: `cust-${index + 1}`,
      name,
      transpilations: Math.floor(Math.random() * 1000) + 500,
      revenue: Math.floor(Math.random() * 50000) + 25000,
      tier: ['Enterprise', 'Professional', 'Standard'][Math.floor(Math.random() * 3)],
      growth: Math.random() * 40 + 10
    }));
  };

  const generateGrowthData = (): GrowthMetric[] => {
    return Array.from({ length: 6 }, (_, i) => {
      const date = new Date();
      date.setMonth(date.getMonth() - 5 + i);
      return {
        period: date.toISOString().slice(0, 7),
        newCustomers: Math.floor(Math.random() * 15) + 5,
        totalCustomers: 120 + (i * 15) + Math.floor(Math.random() * 10),
        growthRate: Math.random() * 15 + 8
      };
    });
  };

  const generateChurnData = (): ChurnMetric[] => {
    const reasons = ['Cost concerns', 'Feature limitations', 'Support issues', 'Competition', 'Internal changes'];
    return Array.from({ length: 6 }, (_, i) => {
      const date = new Date();
      date.setMonth(date.getMonth() - 5 + i);
      return {
        period: date.toISOString().slice(0, 7),
        churnedCustomers: Math.floor(Math.random() * 5) + 1,
        churnRate: Math.random() * 3 + 1,
        reason: reasons[Math.floor(Math.random() * reasons.length)]
      };
    });
  };

  const generateBankingData = (): BankingSystemMetric[] => {
    return [
      { system: 'FIS Systematics', usage: 3247, revenue: 156890, customers: 42, avgComplexity: 7.2 },
      { system: 'Fiserv DNA', usage: 2891, revenue: 134523, customers: 38, avgComplexity: 6.8 },
      { system: 'TCS BaNCS', usage: 2156, revenue: 98765, customers: 29, avgComplexity: 8.1 },
      { system: 'Temenos T24', usage: 1847, revenue: 87234, customers: 25, avgComplexity: 7.9 }
    ];
  };

  const generateBlockchainData = (): BlockchainMetric[] => {
    return [
      { blockchain: 'Ethereum', transactions: 1247, gasUsed: 2847392, successRate: 98.2, avgCost: 0.0023 },
      { blockchain: 'Corda', transactions: 892, gasUsed: 0, successRate: 99.1, avgCost: 0.0012 },
      { blockchain: 'XRP Ledger', transactions: 634, gasUsed: 12847, successRate: 99.7, avgCost: 0.0003 },
      { blockchain: 'Algorand', transactions: 423, gasUsed: 5692, successRate: 99.4, avgCost: 0.0008 }
    ];
  };

  const generateTemplateData = (): TemplateMetric[] => {
    return [
      { templateId: '1', name: 'FIS Mortgage Processing', downloads: 1247, usage: 892, rating: 4.8, category: 'mortgage' },
      { templateId: '2', name: 'Fiserv Payment Gateway', downloads: 967, usage: 734, rating: 4.6, category: 'payments' },
      { templateId: '3', name: 'TCS Trade Finance', downloads: 723, usage: 567, rating: 4.4, category: 'trade' },
      { templateId: '4', name: 'Temenos Cross-Border', downloads: 542, usage: 398, rating: 4.2, category: 'remittance' }
    ];
  };

  const generateForecastData = (): ForecastMetric[] => {
    return Array.from({ length: 12 }, (_, i) => {
      const date = new Date();
      date.setMonth(date.getMonth() + i);
      const trend = i < 6 ? 'increasing' : i < 9 ? 'stable' : 'increasing';
      return {
        period: date.toISOString().slice(0, 7),
        predicted: Math.floor(Math.random() * 500) + 1000 + (i * 50),
        confidence: Math.random() * 20 + 80,
        trend
      };
    });
  };

  const generateCapacityData = (): CapacityMetric[] => {
    return [
      { resource: 'Compute Instances', current: 45, predicted: 67, recommended: 80, timeToCapacity: 3.2 },
      { resource: 'Database Storage', current: 2.4, predicted: 4.1, recommended: 6.0, timeToCapacity: 5.8 },
      { resource: 'API Gateway', current: 12500, predicted: 18900, recommended: 25000, timeToCapacity: 4.1 },
      { resource: 'Load Balancers', current: 8, predicted: 12, recommended: 16, timeToCapacity: 6.7 }
    ];
  };

  const generateOptimizationData = (): OptimizationMetric[] => {
    return [
      { area: 'Instance Right-sizing', currentCost: 12400, optimizedCost: 8900, savings: 3500, effort: 'low' },
      { area: 'Storage Optimization', currentCost: 8700, optimizedCost: 6200, savings: 2500, effort: 'medium' },
      { area: 'Network Efficiency', currentCost: 5600, optimizedCost: 4100, savings: 1500, effort: 'high' },
      { area: 'Database Scaling', currentCost: 15200, optimizedCost: 11800, savings: 3400, effort: 'medium' }
    ];
  };

  const formatCurrency = (amount: number) => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD'
    }).format(amount);
  };

  const formatNumber = (num: number) => {
    return new Intl.NumberFormat('en-US').format(num);
  };

  const formatPercent = (num: number) => {
    return `${num.toFixed(1)}%`;
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'good': return 'text-green-600 bg-green-100';
      case 'warning': return 'text-yellow-600 bg-yellow-100';
      case 'critical': return 'text-red-600 bg-red-100';
      default: return 'text-gray-600 bg-gray-100';
    }
  };

  const getTrendIcon = (trend: string) => {
    switch (trend) {
      case 'up':
      case 'increasing':
        return <TrendingUp className="h-4 w-4 text-green-500" />;
      case 'down':
      case 'decreasing':
        return <TrendingDown className="h-4 w-4 text-red-500" />;
      default:
        return <Activity className="h-4 w-4 text-blue-500" />;
    }
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500 mx-auto"></div>
          <p className="mt-2 text-gray-600">Loading analytics...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto p-6 space-y-8">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">COBOL Usage Analytics</h1>
          <p className="text-gray-600 mt-2">Advanced analytics and insights for enterprise customers</p>
        </div>
        <div className="flex items-center gap-3">
          <Select value={selectedPeriod} onValueChange={setSelectedPeriod}>
            <SelectTrigger className="w-32">
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="7d">Last 7 days</SelectItem>
              <SelectItem value="30d">Last 30 days</SelectItem>
              <SelectItem value="90d">Last 90 days</SelectItem>
              <SelectItem value="1y">Last year</SelectItem>
            </SelectContent>
          </Select>
          <Button
            variant="outline"
            onClick={() => setAutoRefresh(!autoRefresh)}
            className={autoRefresh ? 'bg-green-50 border-green-200' : ''}
          >
            <RefreshCw className={`h-4 w-4 mr-2 ${autoRefresh ? 'animate-spin' : ''}`} />
            Auto Refresh
          </Button>
          <Button variant="outline">
            <Download className="h-4 w-4 mr-2" />
            Export Report
          </Button>
        </div>
      </div>

      {/* Summary Dashboard */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <Zap className="h-4 w-4 mr-2 text-blue-500" />
              Total Transpilations
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{formatNumber(analytics?.summary.totalTranspilations || 0)}</div>
            <div className="flex items-center mt-2">
              <TrendingUp className="h-3 w-3 text-green-500 mr-1" />
              <span className="text-xs text-green-600">+12.5% vs last period</span>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <Users className="h-4 w-4 mr-2 text-purple-500" />
              Active Customers
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{analytics?.summary.totalCustomers || 0}</div>
            <div className="flex items-center mt-2">
              <TrendingUp className="h-3 w-3 text-green-500 mr-1" />
              <span className="text-xs text-green-600">+8.3% growth</span>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <Clock className="h-4 w-4 mr-2 text-orange-500" />
              Avg Processing Time
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{analytics?.summary.averageProcessingTime || 0}min</div>
            <div className="flex items-center mt-2">
              <TrendingDown className="h-3 w-3 text-green-500 mr-1" />
              <span className="text-xs text-green-600">-15% improvement</span>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <CheckCircle className="h-4 w-4 mr-2 text-green-500" />
              Success Rate
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{formatPercent(analytics?.summary.successRate || 0)}</div>
            <div className="flex items-center mt-2">
              <Progress value={analytics?.summary.successRate || 0} className="h-2" />
            </div>
          </CardContent>
        </Card>
      </div>

      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList className="grid w-full grid-cols-6">
          <TabsTrigger value="overview">Overview</TabsTrigger>
          <TabsTrigger value="performance">Performance</TabsTrigger>
          <TabsTrigger value="customers">Customers</TabsTrigger>
          <TabsTrigger value="systems">Banking Systems</TabsTrigger>
          <TabsTrigger value="forecasting">Forecasting</TabsTrigger>
          <TabsTrigger value="optimization">Optimization</TabsTrigger>
        </TabsList>

        <TabsContent value="overview" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Daily Usage Trends */}
            <Card>
              <CardHeader>
                <CardTitle>Daily Usage Trends</CardTitle>
                <CardDescription>Transpilations and success rates over time</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <ComposedChart data={analytics?.trends.daily}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="date" />
                    <YAxis yAxisId="left" />
                    <YAxis yAxisId="right" orientation="right" />
                    <Tooltip />
                    <Legend />
                    <Bar yAxisId="left" dataKey="transpilations" fill="#3B82F6" name="Transpilations" />
                    <Line yAxisId="right" type="monotone" dataKey="successRate" stroke="#10B981" strokeWidth={2} name="Success Rate %" />
                  </ComposedChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            {/* Peak Usage Hours */}
            <Card>
              <CardHeader>
                <CardTitle>Peak Usage Hours</CardTitle>
                <CardDescription>Usage distribution throughout the day</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <AreaChart data={analytics?.trends.hourly}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="hour" />
                    <YAxis />
                    <Tooltip />
                    <Area type="monotone" dataKey="usage" stroke="#8884d8" fill="#8884d8" fillOpacity={0.6} />
                  </AreaChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>
          </div>

          {/* Resource Utilization */}
          <Card>
            <CardHeader>
              <CardTitle>Resource Utilization</CardTitle>
              <CardDescription>Current system resource usage and trends</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-5 gap-4">
                {analytics?.performance.resourceUtilization.map((resource, index) => (
                  <div key={index} className="text-center p-4 border rounded-lg">
                    <div className="flex items-center justify-center mb-2">
                      <Server className="h-5 w-5 text-blue-500 mr-1" />
                      <span className="text-sm font-medium">{resource.resource}</span>
                      {getTrendIcon(resource.trend)}
                    </div>
                    <div className="text-2xl font-bold mb-1">{resource.usage}%</div>
                    <Progress value={resource.usage} className="h-2" />
                    <div className="text-xs text-gray-500 mt-1">
                      of {resource.capacity}% capacity
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          {/* Revenue Metrics */}
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-sm font-medium">Revenue Generated</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{formatCurrency(analytics?.summary.revenueGenerated || 0)}</div>
                <div className="text-xs text-gray-600 mt-1">This period</div>
              </CardContent>
            </Card>
            
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-sm font-medium">Cost per Transpilation</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{formatCurrency(analytics?.summary.costPerTranspilation || 0)}</div>
                <div className="text-xs text-gray-600 mt-1">Average cost</div>
              </CardContent>
            </Card>
            
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-sm font-medium">Lines Processed</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{formatNumber(analytics?.summary.totalLinesProcessed || 0)}</div>
                <div className="text-xs text-gray-600 mt-1">Total COBOL lines</div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        <TabsContent value="performance" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Response Time Monitoring */}
            <Card>
              <CardHeader>
                <CardTitle>Response Time Monitoring</CardTitle>
                <CardDescription>API response times with SLA thresholds</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <LineChart data={analytics?.performance.responseTime}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="timestamp" />
                    <YAxis />
                    <Tooltip />
                    <Line type="monotone" dataKey="value" stroke="#3B82F6" strokeWidth={2} name="Response Time (s)" />
                    <Line type="monotone" dataKey="threshold" stroke="#EF4444" strokeDasharray="5 5" name="SLA Threshold" />
                  </LineChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            {/* Throughput Analysis */}
            <Card>
              <CardHeader>
                <CardTitle>Throughput Analysis</CardTitle>
                <CardDescription>Requests processed per minute</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <AreaChart data={analytics?.performance.throughput}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="timestamp" />
                    <YAxis />
                    <Tooltip />
                    <Area type="monotone" dataKey="value" stroke="#10B981" fill="#10B981" fillOpacity={0.6} name="Requests/min" />
                  </AreaChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>
          </div>

          {/* Performance Status Grid */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {analytics?.performance.responseTime.slice(-3).map((metric, index) => (
              <Card key={index}>
                <CardContent className="pt-6">
                  <div className="flex items-center justify-between">
                    <div>
                      <div className="text-sm font-medium text-gray-600">Response Time</div>
                      <div className="text-2xl font-bold">{metric.value.toFixed(2)}s</div>
                    </div>
                    <Badge className={getStatusColor(metric.status)}>
                      {metric.status.toUpperCase()}
                    </Badge>
                  </div>
                  <div className="mt-2">
                    <Progress 
                      value={(metric.value / metric.threshold) * 100} 
                      className="h-2"
                    />
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>

          {/* Error Rate Tracking */}
          <Card>
            <CardHeader>
              <CardTitle>Error Rate Tracking</CardTitle>
              <CardDescription>System error rates and failure analysis</CardDescription>
            </CardHeader>
            <CardContent>
              <ResponsiveContainer width="100%" height={200}>
                <LineChart data={analytics?.performance.errorRates}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="timestamp" />
                  <YAxis />
                  <Tooltip />
                  <Line type="monotone" dataKey="value" stroke="#EF4444" strokeWidth={2} name="Error Rate %" />
                </LineChart>
              </ResponsiveContainer>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="customers" className="space-y-6">
          {/* Top Customers */}
          <Card>
            <CardHeader>
              <CardTitle>Top Customers by Usage</CardTitle>
              <CardDescription>Highest value customers and their growth metrics</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {analytics?.customers.topCustomers.map((customer, index) => (
                  <div key={index} className="flex items-center justify-between p-4 border rounded-lg">
                    <div>
                      <div className="font-medium">{customer.name}</div>
                      <div className="text-sm text-gray-600">
                        {formatNumber(customer.transpilations)} transpilations • {customer.tier} tier
                      </div>
                    </div>
                    <div className="text-right">
                      <div className="font-bold">{formatCurrency(customer.revenue)}</div>
                      <div className="flex items-center text-sm">
                        <TrendingUp className="h-3 w-3 text-green-500 mr-1" />
                        <span className="text-green-600">+{formatPercent(customer.growth)}</span>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Customer Growth */}
            <Card>
              <CardHeader>
                <CardTitle>Customer Growth</CardTitle>
                <CardDescription>New customer acquisition over time</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <ComposedChart data={analytics?.customers.customerGrowth}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="period" />
                    <YAxis yAxisId="left" />
                    <YAxis yAxisId="right" orientation="right" />
                    <Tooltip />
                    <Bar yAxisId="left" dataKey="newCustomers" fill="#3B82F6" name="New Customers" />
                    <Line yAxisId="right" type="monotone" dataKey="growthRate" stroke="#10B981" strokeWidth={2} name="Growth Rate %" />
                  </ComposedChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            {/* Churn Analysis */}
            <Card>
              <CardHeader>
                <CardTitle>Churn Analysis</CardTitle>
                <CardDescription>Customer retention and churn reasons</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <BarChart data={analytics?.customers.churnAnalysis}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="period" />
                    <YAxis />
                    <Tooltip />
                    <Bar dataKey="churnRate" fill="#EF4444" name="Churn Rate %" />
                  </BarChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        <TabsContent value="systems" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Banking System Distribution */}
            <Card>
              <CardHeader>
                <CardTitle>Banking System Usage</CardTitle>
                <CardDescription>Distribution across different banking platforms</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  {analytics?.banking.systemDistribution.map((system, index) => (
                    <div key={index} className="flex items-center justify-between">
                      <div>
                        <div className="font-medium">{system.system}</div>
                        <div className="text-sm text-gray-600">
                          {system.customers} customers • Complexity: {system.avgComplexity}/10
                        </div>
                      </div>
                      <div className="text-right">
                        <div className="font-bold">{formatNumber(system.usage)}</div>
                        <div className="text-sm text-gray-600">{formatCurrency(system.revenue)}</div>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>

            {/* Blockchain Usage */}
            <Card>
              <CardHeader>
                <CardTitle>Blockchain Distribution</CardTitle>
                <CardDescription>Target blockchain usage and performance</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  {analytics?.banking.blockchainUsage.map((blockchain, index) => (
                    <div key={index} className="flex items-center justify-between">
                      <div>
                        <div className="font-medium">{blockchain.blockchain}</div>
                        <div className="text-sm text-gray-600">
                          {formatPercent(blockchain.successRate)} success rate
                        </div>
                      </div>
                      <div className="text-right">
                        <div className="font-bold">{formatNumber(blockchain.transactions)}</div>
                        <div className="text-sm text-gray-600">{formatCurrency(blockchain.avgCost)} avg</div>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Template Popularity */}
          <Card>
            <CardHeader>
              <CardTitle>Template Popularity</CardTitle>
              <CardDescription>Most used COBOL transpiler templates</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                {analytics?.banking.templatePopularity.map((template, index) => (
                  <div key={index} className="p-4 border rounded-lg">
                    <div className="flex items-center justify-between mb-2">
                      <div className="font-medium">{template.name}</div>
                      <Badge variant="outline" className="text-xs">
                        {template.category}
                      </Badge>
                    </div>
                    <div className="flex items-center justify-between text-sm">
                      <span>{formatNumber(template.downloads)} downloads</span>
                      <span>{formatNumber(template.usage)} uses</span>
                      <span>{template.rating}/5.0 ⭐</span>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="forecasting" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Usage Prediction */}
            <Card>
              <CardHeader>
                <CardTitle>Usage Prediction</CardTitle>
                <CardDescription>12-month usage forecast with confidence intervals</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={300}>
                  <ComposedChart data={analytics?.forecasting.usagePrediction}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="period" />
                    <YAxis yAxisId="left" />
                    <YAxis yAxisId="right" orientation="right" />
                    <Tooltip />
                    <Bar yAxisId="left" dataKey="predicted" fill="#3B82F6" name="Predicted Usage" />
                    <Line yAxisId="right" type="monotone" dataKey="confidence" stroke="#10B981" strokeWidth={2} name="Confidence %" />
                  </ComposedChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            {/* Capacity Planning */}
            <Card>
              <CardHeader>
                <CardTitle>Capacity Planning</CardTitle>
                <CardDescription>Resource capacity recommendations</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  {analytics?.forecasting.capacityPlanning.map((capacity, index) => (
                    <div key={index} className="p-4 border rounded-lg">
                      <div className="flex items-center justify-between mb-2">
                        <div className="font-medium">{capacity.resource}</div>
                        <Badge variant="outline">
                          {capacity.timeToCapacity.toFixed(1)} months
                        </Badge>
                      </div>
                      <div className="space-y-2">
                        <div className="flex justify-between text-sm">
                          <span>Current:</span>
                          <span>{capacity.current}</span>
                        </div>
                        <div className="flex justify-between text-sm">
                          <span>Predicted:</span>
                          <span>{capacity.predicted}</span>
                        </div>
                        <div className="flex justify-between text-sm font-medium">
                          <span>Recommended:</span>
                          <span>{capacity.recommended}</span>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Trend Analysis */}
          <Card>
            <CardHeader>
              <CardTitle>Trend Analysis</CardTitle>
              <CardDescription>Long-term usage patterns and recommendations</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                <div className="text-center p-4 bg-blue-50 rounded-lg">
                  <TrendingUp className="h-8 w-8 text-blue-500 mx-auto mb-2" />
                  <div className="text-2xl font-bold text-blue-600">Growing</div>
                  <div className="text-sm text-blue-600">Usage trending up 23%</div>
                </div>
                <div className="text-center p-4 bg-green-50 rounded-lg">
                  <CheckCircle className="h-8 w-8 text-green-500 mx-auto mb-2" />
                  <div className="text-2xl font-bold text-green-600">Healthy</div>
                  <div className="text-sm text-green-600">Performance within SLA</div>
                </div>
                <div className="text-center p-4 bg-yellow-50 rounded-lg">
                  <AlertTriangle className="h-8 w-8 text-yellow-500 mx-auto mb-2" />
                  <div className="text-2xl font-bold text-yellow-600">Scale Soon</div>
                  <div className="text-sm text-yellow-600">Capacity in 4 months</div>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="optimization" className="space-y-6">
          {/* Cost Optimization Opportunities */}
          <Card>
            <CardHeader>
              <CardTitle>Cost Optimization Opportunities</CardTitle>
              <CardDescription>Identified areas for cost savings and efficiency improvements</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {analytics?.forecasting.costOptimization.map((optimization, index) => (
                  <div key={index} className="p-4 border rounded-lg">
                    <div className="flex items-center justify-between mb-2">
                      <div className="font-medium">{optimization.area}</div>
                      <div className="flex items-center gap-2">
                        <Badge 
                          variant="outline"
                          className={optimization.effort === 'low' ? 'border-green-300 text-green-700' : 
                                   optimization.effort === 'medium' ? 'border-yellow-300 text-yellow-700' : 
                                   'border-red-300 text-red-700'}
                        >
                          {optimization.effort} effort
                        </Badge>
                        <div className="font-bold text-green-600">
                          {formatCurrency(optimization.savings)} saved
                        </div>
                      </div>
                    </div>
                    <div className="grid grid-cols-3 gap-4 text-sm">
                      <div>
                        <span className="text-gray-600">Current Cost:</span>
                        <div className="font-medium">{formatCurrency(optimization.currentCost)}</div>
                      </div>
                      <div>
                        <span className="text-gray-600">Optimized Cost:</span>
                        <div className="font-medium">{formatCurrency(optimization.optimizedCost)}</div>
                      </div>
                      <div>
                        <span className="text-gray-600">Savings:</span>
                        <div className="font-medium text-green-600">
                          {formatPercent((optimization.savings / optimization.currentCost) * 100)}
                        </div>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          {/* Optimization Summary */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-sm font-medium">Total Potential Savings</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold text-green-600">
                  {formatCurrency(
                    analytics?.forecasting.costOptimization.reduce((sum, opt) => sum + opt.savings, 0) || 0
                  )}
                </div>
                <div className="text-xs text-gray-600 mt-1">Annual savings opportunity</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-sm font-medium">Quick Wins Available</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold text-blue-600">
                  {analytics?.forecasting.costOptimization.filter(opt => opt.effort === 'low').length || 0}
                </div>
                <div className="text-xs text-gray-600 mt-1">Low effort optimizations</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-3">
                <CardTitle className="text-sm font-medium">ROI Impact</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold text-purple-600">
                  {formatPercent(
                    ((analytics?.forecasting.costOptimization.reduce((sum, opt) => sum + opt.savings, 0) || 0) /
                     (analytics?.forecasting.costOptimization.reduce((sum, opt) => sum + opt.currentCost, 0) || 1)) * 100
                  )}
                </div>
                <div className="text-xs text-gray-600 mt-1">Potential cost reduction</div>
              </CardContent>
            </Card>
          </div>

          {/* Recommendations */}
          <Card>
            <CardHeader>
              <CardTitle>Optimization Recommendations</CardTitle>
              <CardDescription>Prioritized action items for maximum impact</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                <Alert className="bg-green-50 border-green-200">
                  <CheckCircle className="h-4 w-4 text-green-600" />
                  <AlertTitle>High Priority</AlertTitle>
                  <AlertDescription>
                    Right-size compute instances for 28% cost reduction with minimal effort. 
                    Estimated savings: $3,500/month.
                  </AlertDescription>
                </Alert>
                
                <Alert className="bg-blue-50 border-blue-200">
                  <Target className="h-4 w-4 text-blue-600" />
                  <AlertTitle>Medium Priority</AlertTitle>
                  <AlertDescription>
                    Implement storage optimization policies for automated lifecycle management. 
                    Estimated savings: $2,500/month.
                  </AlertDescription>
                </Alert>
                
                <Alert className="bg-yellow-50 border-yellow-200">
                  <AlertTriangle className="h-4 w-4 text-yellow-600" />
                  <AlertTitle>Long Term</AlertTitle>
                  <AlertDescription>
                    Evaluate network architecture for improved efficiency. Requires significant 
                    engineering effort but offers $1,500/month savings.
                  </AlertDescription>
                </Alert>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}