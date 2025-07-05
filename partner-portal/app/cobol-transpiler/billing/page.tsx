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
  DollarSign,
  TrendingUp,
  TrendingDown,
  Calendar,
  CreditCard,
  AlertTriangle,
  CheckCircle,
  Clock,
  Download,
  Filter,
  BarChart3,
  PieChart,
  Activity,
  Users,
  Zap,
  FileText,
  RefreshCw,
  Settings,
  Eye,
  ArrowUpRight,
  ArrowDownRight,
  Package,
  Percent
} from 'lucide-react';
import { LineChart, Line, AreaChart, Area, BarChart, Bar, PieChart as RechartsPieChart, Cell, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, Legend } from 'recharts';

interface UsageMetrics {
  currentPeriod: {
    startDate: string;
    endDate: string;
    transpilationsUsed: number;
    linesOfCodeProcessed: number;
    quotaLimit: number;
    overageAmount: number;
    totalCost: number;
  };
  quotaDetails: {
    baseQuota: number;
    usedQuota: number;
    remainingQuota: number;
    quotaResetDate: string;
    overageRate: number;
    nextTierThreshold: number;
  };
  projections: {
    projectedUsage: number;
    projectedOverage: number;
    projectedCost: number;
    recommendedPlan: string;
  };
}

interface BillingHistory {
  id: string;
  periodStart: string;
  periodEnd: string;
  transpilations: number;
  linesProcessed: number;
  baseCost: number;
  overageCost: number;
  totalCost: number;
  status: 'paid' | 'pending' | 'overdue';
  invoiceUrl?: string;
  partnerCommission?: number;
}

interface PartnerCommission {
  id: string;
  period: string;
  revenueGenerated: number;
  commissionRate: number;
  commissionAmount: number;
  customerCount: number;
  status: 'pending' | 'paid' | 'processing';
  payoutDate?: string;
}

interface PricingTier {
  name: string;
  monthlyLimit: number;
  basePrice: number;
  overageRate: number;
  features: string[];
  recommended?: boolean;
}

const PRICING_TIERS: PricingTier[] = [
  {
    name: 'Starter',
    monthlyLimit: 50,
    basePrice: 299,
    overageRate: 8.00,
    features: ['Up to 50 transpilations/month', 'Basic templates', 'Email support', 'Standard SLA']
  },
  {
    name: 'Professional', 
    monthlyLimit: 250,
    basePrice: 999,
    overageRate: 6.00,
    features: ['Up to 250 transpilations/month', 'Premium templates', 'Priority support', 'Enhanced SLA'],
    recommended: true
  },
  {
    name: 'Enterprise',
    monthlyLimit: 1000,
    basePrice: 2999,
    overageRate: 4.00,
    features: ['Up to 1000 transpilations/month', 'Custom templates', '24/7 support', 'Premium SLA', 'Dedicated success manager']
  },
  {
    name: 'Custom',
    monthlyLimit: -1,
    basePrice: 0,
    overageRate: 0,
    features: ['Unlimited transpilations', 'Custom everything', 'White-glove support', 'Custom SLA', 'On-premise deployment']
  }
];

export default function BillingIntegration() {
  const [loading, setLoading] = useState(true);
  const [usageMetrics, setUsageMetrics] = useState<UsageMetrics | null>(null);
  const [billingHistory, setBillingHistory] = useState<BillingHistory[]>([]);
  const [partnerCommissions, setPartnerCommissions] = useState<PartnerCommission[]>([]);
  const [selectedPeriod, setSelectedPeriod] = useState('current');
  const [activeTab, setActiveTab] = useState('overview');

  // Mock usage data for charts
  const usageChartData = [
    { month: 'Jan', transpilations: 45, cost: 299 },
    { month: 'Feb', transpilations: 78, cost: 523 },
    { month: 'Mar', transpilations: 125, cost: 749 },
    { month: 'Apr', transpilations: 89, cost: 533 },
    { month: 'May', transpilations: 156, cost: 935 },
    { month: 'Jun', transpilations: 203, cost: 1217 },
    { month: 'Jul', transpilations: 187, cost: 1121 }
  ];

  const dailyUsageData = [
    { day: '1', usage: 12 },
    { day: '2', usage: 8 },
    { day: '3', usage: 15 },
    { day: '4', usage: 22 },
    { day: '5', usage: 18 },
    { day: '6', usage: 25 },
    { day: '7', usage: 31 },
    { day: '8', usage: 28 },
    { day: '9', usage: 19 },
    { day: '10', usage: 24 },
    { day: '11', usage: 33 },
    { day: '12', usage: 29 },
    { day: '13', usage: 17 },
    { day: '14', usage: 21 }
  ];

  const usageByBankingSystem = [
    { name: 'FIS Systematics', value: 45, color: '#3B82F6' },
    { name: 'Fiserv DNA', value: 32, color: '#10B981' },
    { name: 'TCS BaNCS', value: 28, color: '#F59E0B' },
    { name: 'Temenos', value: 15, color: '#EF4444' }
  ];

  useEffect(() => {
    const fetchBillingData = async () => {
      setLoading(true);
      
      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 1000));
      
      const mockUsageMetrics: UsageMetrics = {
        currentPeriod: {
          startDate: '2025-07-01T00:00:00Z',
          endDate: '2025-07-31T23:59:59Z',
          transpilationsUsed: 187,
          linesOfCodeProcessed: 245632,
          quotaLimit: 250,
          overageAmount: 0,
          totalCost: 999
        },
        quotaDetails: {
          baseQuota: 250,
          usedQuota: 187,
          remainingQuota: 63,
          quotaResetDate: '2025-08-01T00:00:00Z',
          overageRate: 6.00,
          nextTierThreshold: 1000
        },
        projections: {
          projectedUsage: 242,
          projectedOverage: 0,
          projectedCost: 999,
          recommendedPlan: 'Professional'
        }
      };

      const mockBillingHistory: BillingHistory[] = [
        {
          id: '1',
          periodStart: '2025-07-01T00:00:00Z',
          periodEnd: '2025-07-31T23:59:59Z',
          transpilations: 187,
          linesProcessed: 245632,
          baseCost: 999,
          overageCost: 0,
          totalCost: 999,
          status: 'pending',
          partnerCommission: 149.85
        },
        {
          id: '2',
          periodStart: '2025-06-01T00:00:00Z',
          periodEnd: '2025-06-30T23:59:59Z',
          transpilations: 203,
          linesProcessed: 289451,
          baseCost: 999,
          overageCost: 218,
          totalCost: 1217,
          status: 'paid',
          invoiceUrl: '/invoices/2025-06.pdf',
          partnerCommission: 182.55
        },
        {
          id: '3',
          periodStart: '2025-05-01T00:00:00Z',
          periodEnd: '2025-05-31T23:59:59Z',
          transpilations: 156,
          linesProcessed: 198732,
          baseCost: 999,
          overageCost: 0,
          totalCost: 999,
          status: 'paid',
          invoiceUrl: '/invoices/2025-05.pdf',
          partnerCommission: 149.85
        },
        {
          id: '4',
          periodStart: '2025-04-01T00:00:00Z',
          periodEnd: '2025-04-30T23:59:59Z',
          transpilations: 89,
          linesProcessed: 134521,
          baseCost: 999,
          overageCost: 0,
          totalCost: 999,
          status: 'paid',
          invoiceUrl: '/invoices/2025-04.pdf',
          partnerCommission: 149.85
        }
      ];

      const mockPartnerCommissions: PartnerCommission[] = [
        {
          id: '1',
          period: '2025-07',
          revenueGenerated: 4996,
          commissionRate: 15,
          commissionAmount: 749.40,
          customerCount: 5,
          status: 'pending'
        },
        {
          id: '2',
          period: '2025-06',
          revenueGenerated: 6085,
          commissionRate: 15,
          commissionAmount: 912.75,
          customerCount: 5,
          status: 'paid',
          payoutDate: '2025-07-01T00:00:00Z'
        },
        {
          id: '3',
          period: '2025-05',
          revenueGenerated: 4995,
          commissionRate: 15,
          commissionAmount: 749.25,
          customerCount: 5,
          status: 'paid',
          payoutDate: '2025-06-01T00:00:00Z'
        }
      ];

      setUsageMetrics(mockUsageMetrics);
      setBillingHistory(mockBillingHistory);
      setPartnerCommissions(mockPartnerCommissions);
      setLoading(false);
    };

    fetchBillingData();
  }, []);

  const getStatusBadge = (status: string) => {
    const variants = {
      paid: { color: 'bg-green-100 text-green-800', icon: <CheckCircle className="h-3 w-3" /> },
      pending: { color: 'bg-yellow-100 text-yellow-800', icon: <Clock className="h-3 w-3" /> },
      overdue: { color: 'bg-red-100 text-red-800', icon: <AlertTriangle className="h-3 w-3" /> },
      processing: { color: 'bg-blue-100 text-blue-800', icon: <RefreshCw className="h-3 w-3" /> }
    };
    
    const variant = variants[status as keyof typeof variants];
    
    return (
      <Badge className={`${variant.color} flex items-center gap-1`}>
        {variant.icon}
        {status.charAt(0).toUpperCase() + status.slice(1)}
      </Badge>
    );
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

  const calculateUsagePercentage = () => {
    if (!usageMetrics) return 0;
    return (usageMetrics.quotaDetails.usedQuota / usageMetrics.quotaDetails.baseQuota) * 100;
  };

  const getUsageColor = () => {
    const percentage = calculateUsagePercentage();
    if (percentage >= 90) return 'bg-red-500';
    if (percentage >= 75) return 'bg-yellow-500';
    return 'bg-green-500';
  };

  if (loading) {
    return (
      <div className="flex items-center justify-center min-h-screen">
        <div className="text-center">
          <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-500 mx-auto"></div>
          <p className="mt-2 text-gray-600">Loading billing information...</p>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto p-6 space-y-8">
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">Billing & Usage</h1>
          <p className="text-gray-600 mt-2">Track your COBOL transpiler usage and manage billing</p>
        </div>
        <div className="flex items-center gap-3">
          <Button variant="outline">
            <Download className="h-4 w-4 mr-2" />
            Export Data
          </Button>
          <Button variant="outline">
            <Settings className="h-4 w-4 mr-2" />
            Billing Settings
          </Button>
        </div>
      </div>

      {/* Current Usage Overview */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <Zap className="h-4 w-4 mr-2 text-blue-500" />
              Current Usage
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {usageMetrics ? usageMetrics.quotaDetails.usedQuota : 0}
              <span className="text-sm font-normal text-gray-500">
                /{usageMetrics ? usageMetrics.quotaDetails.baseQuota : 0}
              </span>
            </div>
            <div className="mt-2">
              <Progress 
                value={calculateUsagePercentage()} 
                className="h-2" 
              />
            </div>
            <p className="text-xs text-gray-600 mt-2">
              {usageMetrics ? usageMetrics.quotaDetails.remainingQuota : 0} transpilations remaining
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <DollarSign className="h-4 w-4 mr-2 text-green-500" />
              Current Period Cost
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {formatCurrency(usageMetrics?.currentPeriod.totalCost || 0)}
            </div>
            <div className="flex items-center mt-2">
              <div className="text-xs text-gray-600">
                Base: {formatCurrency(999)} + Overage: {formatCurrency(usageMetrics?.currentPeriod.overageAmount || 0)}
              </div>
            </div>
            <p className="text-xs text-gray-600 mt-1">
              Period: {usageMetrics ? new Date(usageMetrics.currentPeriod.startDate).toLocaleDateString() : ''} - 
              {usageMetrics ? new Date(usageMetrics.currentPeriod.endDate).toLocaleDateString() : ''}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <FileText className="h-4 w-4 mr-2 text-purple-500" />
              Lines Processed
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {formatNumber(usageMetrics?.currentPeriod.linesOfCodeProcessed || 0)}
            </div>
            <div className="flex items-center mt-2">
              <TrendingUp className="h-3 w-3 text-green-500 mr-1" />
              <span className="text-xs text-green-600">+23% vs last month</span>
            </div>
            <p className="text-xs text-gray-600 mt-1">
              Avg: {formatNumber(Math.round((usageMetrics?.currentPeriod.linesOfCodeProcessed || 0) / (usageMetrics?.quotaDetails.usedQuota || 1)))} lines/transpilation
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-3">
            <CardTitle className="text-sm font-medium flex items-center">
              <Calendar className="h-4 w-4 mr-2 text-orange-500" />
              Next Reset
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {usageMetrics ? Math.ceil((new Date(usageMetrics.quotaDetails.quotaResetDate).getTime() - new Date().getTime()) / (1000 * 60 * 60 * 24)) : 0}
            </div>
            <p className="text-sm text-gray-600">days remaining</p>
            <p className="text-xs text-gray-600 mt-1">
              Quota resets: {usageMetrics ? new Date(usageMetrics.quotaDetails.quotaResetDate).toLocaleDateString() : ''}
            </p>
          </CardContent>
        </Card>
      </div>

      {/* Usage Alert */}
      {calculateUsagePercentage() >= 85 && (
        <Alert className="bg-yellow-50 border-yellow-200">
          <AlertTriangle className="h-4 w-4 text-yellow-600" />
          <AlertTitle>High Usage Alert</AlertTitle>
          <AlertDescription>
            You've used {calculateUsagePercentage().toFixed(1)}% of your monthly quota. 
            Overage charges of {formatCurrency(usageMetrics?.quotaDetails.overageRate || 0)} per transpilation will apply after exceeding your limit.
            <div className="mt-2">
              <Button variant="outline" size="sm">
                Upgrade Plan
              </Button>
            </div>
          </AlertDescription>
        </Alert>
      )}

      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList>
          <TabsTrigger value="overview">Overview</TabsTrigger>
          <TabsTrigger value="usage">Usage Analytics</TabsTrigger>
          <TabsTrigger value="billing">Billing History</TabsTrigger>
          <TabsTrigger value="commissions">Partner Commissions</TabsTrigger>
          <TabsTrigger value="plans">Pricing Plans</TabsTrigger>
        </TabsList>

        <TabsContent value="overview" className="space-y-6">
          {/* Usage Trends Chart */}
          <Card>
            <CardHeader>
              <CardTitle>Usage & Cost Trends</CardTitle>
              <CardDescription>Monthly transpilation usage and associated costs</CardDescription>
            </CardHeader>
            <CardContent>
              <ResponsiveContainer width="100%" height={300}>
                <LineChart data={usageChartData}>
                  <CartesianGrid strokeDasharray="3 3" />
                  <XAxis dataKey="month" />
                  <YAxis yAxisId="left" />
                  <YAxis yAxisId="right" orientation="right" />
                  <Tooltip 
                    formatter={(value: any, name: string) => [
                      name === 'cost' ? formatCurrency(value) : value,
                      name === 'transpilations' ? 'Transpilations' : 'Cost'
                    ]}
                  />
                  <Legend />
                  <Bar yAxisId="left" dataKey="transpilations" fill="#3B82F6" name="transpilations" />
                  <Line yAxisId="right" type="monotone" dataKey="cost" stroke="#10B981" strokeWidth={3} name="cost" />
                </LineChart>
              </ResponsiveContainer>
            </CardContent>
          </Card>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            {/* Daily Usage This Month */}
            <Card>
              <CardHeader>
                <CardTitle>Daily Usage This Month</CardTitle>
                <CardDescription>Transpilations per day</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={200}>
                  <AreaChart data={dailyUsageData}>
                    <CartesianGrid strokeDasharray="3 3" />
                    <XAxis dataKey="day" />
                    <YAxis />
                    <Tooltip />
                    <Area type="monotone" dataKey="usage" stroke="#8884d8" fill="#8884d8" fillOpacity={0.6} />
                  </AreaChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>

            {/* Usage by Banking System */}
            <Card>
              <CardHeader>
                <CardTitle>Usage by Banking System</CardTitle>
                <CardDescription>Distribution of transpilations</CardDescription>
              </CardHeader>
              <CardContent>
                <ResponsiveContainer width="100%" height={200}>
                  <RechartsPieChart>
                    <Pie
                      data={usageByBankingSystem}
                      cx="50%"
                      cy="50%"
                      outerRadius={80}
                      fill="#8884d8"
                      dataKey="value"
                      label={({ name, percent }) => `${name} ${(percent * 100).toFixed(0)}%`}
                    >
                      {usageByBankingSystem.map((entry, index) => (
                        <Cell key={`cell-${index}`} fill={entry.color} />
                      ))}
                    </Pie>
                    <Tooltip />
                  </RechartsPieChart>
                </ResponsiveContainer>
              </CardContent>
            </Card>
          </div>

          {/* Projections */}
          <Card>
            <CardHeader>
              <CardTitle>End-of-Month Projections</CardTitle>
              <CardDescription>Based on current usage patterns</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                <div className="text-center p-4 bg-blue-50 rounded-lg">
                  <div className="text-2xl font-bold text-blue-600">
                    {usageMetrics?.projections.projectedUsage || 0}
                  </div>
                  <p className="text-sm text-blue-600">Projected Transpilations</p>
                </div>
                <div className="text-center p-4 bg-green-50 rounded-lg">
                  <div className="text-2xl font-bold text-green-600">
                    {formatCurrency(usageMetrics?.projections.projectedCost || 0)}
                  </div>
                  <p className="text-sm text-green-600">Projected Cost</p>
                </div>
                <div className="text-center p-4 bg-purple-50 rounded-lg">
                  <div className="text-2xl font-bold text-purple-600">
                    {usageMetrics?.projections.recommendedPlan || 'Current'}
                  </div>
                  <p className="text-sm text-purple-600">Recommended Plan</p>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="usage" className="space-y-6">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle>Quota Utilization</CardTitle>
                <CardDescription>Current period usage breakdown</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="flex items-center justify-between">
                  <span className="text-sm font-medium">Used</span>
                  <span className="text-sm text-gray-600">
                    {usageMetrics?.quotaDetails.usedQuota || 0} transpilations
                  </span>
                </div>
                <Progress value={calculateUsagePercentage()} className="h-3" />
                <div className="flex items-center justify-between text-xs text-gray-600">
                  <span>0</span>
                  <span>{usageMetrics?.quotaDetails.baseQuota || 0}</span>
                </div>
                
                <div className="pt-4 space-y-2">
                  <div className="flex justify-between">
                    <span className="text-sm">Remaining this period:</span>
                    <span className="text-sm font-medium">{usageMetrics?.quotaDetails.remainingQuota || 0}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-sm">Overage rate:</span>
                    <span className="text-sm font-medium">{formatCurrency(usageMetrics?.quotaDetails.overageRate || 0)}/transpilation</span>
                  </div>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Usage Metrics</CardTitle>
                <CardDescription>Detailed usage statistics</CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <div className="text-2xl font-bold">{formatNumber(usageMetrics?.currentPeriod.linesOfCodeProcessed || 0)}</div>
                    <p className="text-xs text-gray-600">Lines of Code</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold">{usageMetrics?.currentPeriod.transpilationsUsed || 0}</div>
                    <p className="text-xs text-gray-600">Transpilations</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold">
                      {formatNumber(Math.round((usageMetrics?.currentPeriod.linesOfCodeProcessed || 0) / (usageMetrics?.currentPeriod.transpilationsUsed || 1)))}
                    </div>
                    <p className="text-xs text-gray-600">Avg Lines/Project</p>
                  </div>
                  <div>
                    <div className="text-2xl font-bold">98.5%</div>
                    <p className="text-xs text-gray-600">Success Rate</p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Usage by Time of Day */}
          <Card>
            <CardHeader>
              <CardTitle>Usage Patterns</CardTitle>
              <CardDescription>Peak usage times and patterns</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                <div className="text-center p-4 border rounded-lg">
                  <div className="text-xl font-bold">9-11 AM</div>
                  <p className="text-sm text-gray-600">Peak Usage Hours</p>
                  <div className="text-xs text-blue-600 mt-1">45% of daily activity</div>
                </div>
                <div className="text-center p-4 border rounded-lg">
                  <div className="text-xl font-bold">Tuesday</div>
                  <p className="text-sm text-gray-600">Busiest Day</p>
                  <div className="text-xs text-green-600 mt-1">22% of weekly activity</div>
                </div>
                <div className="text-center p-4 border rounded-lg">
                  <div className="text-xl font-bold">2.3 min</div>
                  <p className="text-sm text-gray-600">Avg Processing Time</p>
                  <div className="text-xs text-purple-600 mt-1">15% faster than target</div>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="billing" className="space-y-6">
          <div className="space-y-4">
            {billingHistory.map((bill) => (
              <Card key={bill.id}>
                <CardContent className="pt-6">
                  <div className="flex items-center justify-between">
                    <div>
                      <h3 className="font-semibold">
                        {new Date(bill.periodStart).toLocaleDateString('en-US', { month: 'long', year: 'numeric' })}
                      </h3>
                      <p className="text-sm text-gray-600">
                        {new Date(bill.periodStart).toLocaleDateString()} - {new Date(bill.periodEnd).toLocaleDateString()}
                      </p>
                    </div>
                    <div className="text-right">
                      <div className="text-xl font-bold">{formatCurrency(bill.totalCost)}</div>
                      {getStatusBadge(bill.status)}
                    </div>
                  </div>
                  
                  <div className="mt-4 grid grid-cols-2 md:grid-cols-4 gap-4 text-sm">
                    <div>
                      <span className="text-gray-600">Transpilations:</span>
                      <p className="font-medium">{formatNumber(bill.transpilations)}</p>
                    </div>
                    <div>
                      <span className="text-gray-600">Lines Processed:</span>
                      <p className="font-medium">{formatNumber(bill.linesProcessed)}</p>
                    </div>
                    <div>
                      <span className="text-gray-600">Base Cost:</span>
                      <p className="font-medium">{formatCurrency(bill.baseCost)}</p>
                    </div>
                    <div>
                      <span className="text-gray-600">Overage:</span>
                      <p className="font-medium">{formatCurrency(bill.overageCost)}</p>
                    </div>
                  </div>

                  {bill.partnerCommission && (
                    <div className="mt-3 p-3 bg-green-50 rounded-lg">
                      <div className="flex items-center justify-between">
                        <span className="text-sm text-green-700">Partner Commission (15%)</span>
                        <span className="font-medium text-green-700">{formatCurrency(bill.partnerCommission)}</span>
                      </div>
                    </div>
                  )}
                  
                  <div className="mt-4 flex justify-between items-center">
                    <div className="flex space-x-2">
                      {bill.invoiceUrl && (
                        <Button variant="outline" size="sm">
                          <Download className="h-3 w-3 mr-1" />
                          Download Invoice
                        </Button>
                      )}
                      <Button variant="outline" size="sm">
                        <Eye className="h-3 w-3 mr-1" />
                        View Details
                      </Button>
                    </div>
                    {bill.status === 'overdue' && (
                      <Button size="sm">
                        <CreditCard className="h-3 w-3 mr-1" />
                        Pay Now
                      </Button>
                    )}
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </TabsContent>

        <TabsContent value="commissions" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle>Commission Overview</CardTitle>
              <CardDescription>Partner revenue sharing and commission tracking</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mb-6">
                <div className="text-center p-4 bg-green-50 rounded-lg">
                  <div className="text-2xl font-bold text-green-600">
                    {formatCurrency(partnerCommissions.reduce((acc, c) => acc + c.commissionAmount, 0))}
                  </div>
                  <p className="text-sm text-green-600">Total Commissions</p>
                </div>
                <div className="text-center p-4 bg-blue-50 rounded-lg">
                  <div className="text-2xl font-bold text-blue-600">15%</div>
                  <p className="text-sm text-blue-600">Commission Rate</p>
                </div>
                <div className="text-center p-4 bg-purple-50 rounded-lg">
                  <div className="text-2xl font-bold text-purple-600">
                    {partnerCommissions[0]?.customerCount || 0}
                  </div>
                  <p className="text-sm text-purple-600">Active Customers</p>
                </div>
              </div>
            </CardContent>
          </Card>

          <div className="space-y-4">
            {partnerCommissions.map((commission) => (
              <Card key={commission.id}>
                <CardContent className="pt-6">
                  <div className="flex items-center justify-between">
                    <div>
                      <h3 className="font-semibold">
                        {new Date(commission.period + '-01').toLocaleDateString('en-US', { month: 'long', year: 'numeric' })}
                      </h3>
                      <p className="text-sm text-gray-600">
                        Commission Period
                      </p>
                    </div>
                    <div className="text-right">
                      <div className="text-xl font-bold">{formatCurrency(commission.commissionAmount)}</div>
                      {getStatusBadge(commission.status)}
                    </div>
                  </div>
                  
                  <div className="mt-4 grid grid-cols-2 md:grid-cols-4 gap-4 text-sm">
                    <div>
                      <span className="text-gray-600">Revenue Generated:</span>
                      <p className="font-medium">{formatCurrency(commission.revenueGenerated)}</p>
                    </div>
                    <div>
                      <span className="text-gray-600">Commission Rate:</span>
                      <p className="font-medium">{commission.commissionRate}%</p>
                    </div>
                    <div>
                      <span className="text-gray-600">Customer Count:</span>
                      <p className="font-medium">{commission.customerCount}</p>
                    </div>
                    <div>
                      <span className="text-gray-600">Avg Revenue/Customer:</span>
                      <p className="font-medium">{formatCurrency(commission.revenueGenerated / commission.customerCount)}</p>
                    </div>
                  </div>

                  {commission.payoutDate && (
                    <div className="mt-3 p-3 bg-green-50 rounded-lg">
                      <div className="flex items-center justify-between">
                        <span className="text-sm text-green-700">Paid on {new Date(commission.payoutDate).toLocaleDateString()}</span>
                        <CheckCircle className="h-4 w-4 text-green-500" />
                      </div>
                    </div>
                  )}
                </CardContent>
              </Card>
            ))}
          </div>
        </TabsContent>

        <TabsContent value="plans" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle>Current Plan: Professional</CardTitle>
              <CardDescription>
                You're currently on the Professional plan with 250 transpilations per month
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                {PRICING_TIERS.map((tier) => (
                  <div 
                    key={tier.name}
                    className={`border rounded-lg p-6 ${tier.recommended ? 'border-blue-500 bg-blue-50' : 'border-gray-200'}`}
                  >
                    {tier.recommended && (
                      <Badge className="mb-3 bg-blue-500 text-white">Current Plan</Badge>
                    )}
                    <h3 className="font-semibold text-lg">{tier.name}</h3>
                    <div className="mt-2">
                      {tier.name !== 'Custom' ? (
                        <>
                          <div className="text-3xl font-bold">{formatCurrency(tier.basePrice)}</div>
                          <p className="text-sm text-gray-600">per month</p>
                        </>
                      ) : (
                        <div className="text-3xl font-bold">Contact Sales</div>
                      )}
                    </div>
                    
                    <div className="mt-4">
                      <p className="text-sm font-medium">
                        {tier.monthlyLimit === -1 ? 'Unlimited' : `${tier.monthlyLimit} transpilations/month`}
                      </p>
                      {tier.name !== 'Custom' && (
                        <p className="text-xs text-gray-600">
                          Overage: {formatCurrency(tier.overageRate)}/transpilation
                        </p>
                      )}
                    </div>
                    
                    <ul className="mt-4 space-y-2">
                      {tier.features.map((feature, index) => (
                        <li key={index} className="flex items-center text-sm">
                          <CheckCircle className="h-3 w-3 text-green-500 mr-2" />
                          {feature}
                        </li>
                      ))}
                    </ul>
                    
                    <Button 
                      className="w-full mt-6" 
                      variant={tier.recommended ? "default" : "outline"}
                      disabled={tier.recommended}
                    >
                      {tier.recommended ? 'Current Plan' : 
                       tier.name === 'Custom' ? 'Contact Sales' : 'Upgrade'}
                    </Button>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Plan Comparison</CardTitle>
              <CardDescription>Compare features across all plans</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="overflow-x-auto">
                <table className="w-full text-sm">
                  <thead>
                    <tr className="border-b">
                      <th className="text-left py-2">Feature</th>
                      {PRICING_TIERS.map(tier => (
                        <th key={tier.name} className="text-center py-2">{tier.name}</th>
                      ))}
                    </tr>
                  </thead>
                  <tbody className="text-center">
                    <tr className="border-b">
                      <td className="text-left py-2">Monthly Transpilations</td>
                      {PRICING_TIERS.map(tier => (
                        <td key={tier.name} className="py-2">
                          {tier.monthlyLimit === -1 ? 'Unlimited' : tier.monthlyLimit}
                        </td>
                      ))}
                    </tr>
                    <tr className="border-b">
                      <td className="text-left py-2">Overage Rate</td>
                      {PRICING_TIERS.map(tier => (
                        <td key={tier.name} className="py-2">
                          {tier.name === 'Custom' ? 'Custom' : formatCurrency(tier.overageRate)}
                        </td>
                      ))}
                    </tr>
                    <tr className="border-b">
                      <td className="text-left py-2">Templates</td>
                      <td>Basic</td>
                      <td>Premium</td>
                      <td>Premium</td>
                      <td>Custom</td>
                    </tr>
                    <tr className="border-b">
                      <td className="text-left py-2">Support</td>
                      <td>Email</td>
                      <td>Priority</td>
                      <td>24/7</td>
                      <td>White-glove</td>
                    </tr>
                  </tbody>
                </table>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}