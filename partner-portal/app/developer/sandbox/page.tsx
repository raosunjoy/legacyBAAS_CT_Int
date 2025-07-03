'use client'

import { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { 
  Code,
  Play,
  Copy,
  CheckCircle,
  Settings,
  Book,
  Users,
  Zap,
  Shield,
  BarChart3,
  ArrowRight,
  Download,
  Terminal,
  FileText,
  Github
} from 'lucide-react'
import Link from 'next/link'

const sdkExamples = {
  javascript: {
    name: 'JavaScript/TypeScript',
    install: 'npm install @legacybaas/sdk',
    code: `import { LegacyBaaSClient } from '@legacybaas/sdk';

const client = new LegacyBaaSClient({
  apiKey: 'sb_test_12345...',
  environment: 'sandbox'
});

// Process SWIFT MT515 (Securities Trade Confirmation)
const mt515 = {
  transactionReference: 'TF515123456',
  tradeDate: '2024-03-15',
  settlementDate: '2024-03-17',
  accountIdentification: 'ACC123456789',
  financialInstrument: {
    isin: 'US0378331005',
    description: 'Apple Inc. Common Stock',
    quantity: 1000,
    unitPrice: 150.75,
    currency: 'USD'
  },
  tradeDetails: {
    buySellIndicator: 'BUY',
    grossAmount: 150750.00,
    netAmount: 150725.50,
    commission: 24.50
  },
  counterparty: {
    bic: 'BANKUS33XXX',
    name: 'Goldman Sachs International'
  }
};

try {
  const result = await client.swift.processMT515(mt515);
  console.log('Trade Confirmation ID:', result.confirmationId);
  console.log('Settlement Status:', result.settlementStatus);
  console.log('Blockchain Hash:', result.blockchainHash);
} catch (error) {
  console.error('Trade confirmation failed:', error);
}`
  },
  python: {
    name: 'Python',
    install: 'pip install legacybaas-sdk',
    code: `from legacybaas import LegacyBaaSClient

client = LegacyBaaSClient(
    api_key='sb_test_12345...',
    environment='sandbox'
)

# Process SWIFT MT515 (Securities Trade Confirmation)
mt515 = {
    'transaction_reference': 'TF515123456',
    'trade_date': '2024-03-15',
    'settlement_date': '2024-03-17',
    'account_identification': 'ACC123456789',
    'financial_instrument': {
        'isin': 'US0378331005',
        'description': 'Apple Inc. Common Stock',
        'quantity': 1000,
        'unit_price': 150.75,
        'currency': 'USD'
    },
    'trade_details': {
        'buy_sell_indicator': 'BUY',
        'gross_amount': 150750.00,
        'net_amount': 150725.50,
        'commission': 24.50
    },
    'counterparty': {
        'bic': 'BANKUS33XXX',
        'name': 'Goldman Sachs International'
    }
}

try:
    result = client.swift.process_mt515(mt515)
    print(f"Trade Confirmation ID: {result.confirmation_id}")
    print(f"Settlement Status: {result.settlement_status}")
    print(f"Blockchain Hash: {result.blockchain_hash}")
except Exception as error:
    print(f"Trade confirmation failed: {error}")`
  },
  java: {
    name: 'Java',
    install: 'implementation "com.legacybaas:sdk:1.0.0"',
    code: `import com.legacybaas.LegacyBaaSClient;
import com.legacybaas.models.swift.MT515Message;
import com.legacybaas.models.FinancialInstrument;
import com.legacybaas.models.TradeDetails;
import com.legacybaas.models.Counterparty;
import java.math.BigDecimal;
import java.time.LocalDate;

LegacyBaaSClient client = LegacyBaaSClient.builder()
    .apiKey("sb_test_12345...")
    .environment("sandbox")
    .build();

// Process SWIFT MT515 (Securities Trade Confirmation)
MT515Message mt515 = MT515Message.builder()
    .transactionReference("TF515123456")
    .tradeDate(LocalDate.of(2024, 3, 15))
    .settlementDate(LocalDate.of(2024, 3, 17))
    .accountIdentification("ACC123456789")
    .financialInstrument(
        FinancialInstrument.builder()
            .isin("US0378331005")
            .description("Apple Inc. Common Stock")
            .quantity(1000)
            .unitPrice(new BigDecimal("150.75"))
            .currency("USD")
            .build()
    )
    .tradeDetails(
        TradeDetails.builder()
            .buySellIndicator("BUY")
            .grossAmount(new BigDecimal("150750.00"))
            .netAmount(new BigDecimal("150725.50"))
            .commission(new BigDecimal("24.50"))
            .build()
    )
    .counterparty(
        Counterparty.builder()
            .bic("BANKUS33XXX")
            .name("Goldman Sachs International")
            .build()
    )
    .build();

try {
    TradeConfirmationResult result = client.swift().processMT515(mt515);
    System.out.println("Trade Confirmation ID: " + result.getConfirmationId());
    System.out.println("Settlement Status: " + result.getSettlementStatus());
    System.out.println("Blockchain Hash: " + result.getBlockchainHash());
} catch (Exception error) {
    System.err.println("Trade confirmation failed: " + error.getMessage());
}`
  },
  csharp: {
    name: '.NET/C#',
    install: 'dotnet add package LegacyBaaS.SDK',
    code: `using LegacyBaaS.SDK;
using LegacyBaaS.SDK.Models;

var client = new LegacyBaaSClient("sb_test_12345...", 
    new LegacyBaaSOptions { Environment = "sandbox" });

// Process SWIFT MT515 (Securities Trade Confirmation)
var mt515 = new MT515Message
{
    TransactionReference = "TF515123456",
    TradeDate = new DateTime(2024, 3, 15),
    SettlementDate = new DateTime(2024, 3, 17),
    AccountIdentification = "ACC123456789",
    FinancialInstrument = new FinancialInstrument
    {
        ISIN = "US0378331005",
        Description = "Apple Inc. Common Stock",
        Quantity = 1000,
        UnitPrice = 150.75m,
        Currency = "USD"
    },
    TradeDetails = new TradeDetails
    {
        BuySellIndicator = "BUY",
        GrossAmount = 150750.00m,
        NetAmount = 150725.50m,
        Commission = 24.50m
    },
    Counterparty = new Counterparty
    {
        BIC = "BANKUS33XXX",
        Name = "Goldman Sachs International"
    }
};

try
{
    var result = await client.Swift.ProcessMT515Async(mt515);
    Console.WriteLine($"Trade Confirmation ID: {result.ConfirmationId}");
    Console.WriteLine($"Settlement Status: {result.SettlementStatus}");
    Console.WriteLine($"Blockchain Hash: {result.BlockchainHash}");
}
catch (Exception error)
{
    Console.WriteLine($"Trade confirmation failed: {error.Message}");
}`
  },
  swift: {
    name: 'iOS/Swift',
    install: 'pod "LegacyBaaSSDK"',
    code: `import LegacyBaaSSDK

let client = LegacyBaaSClient(apiKey: "sb_test_12345...")

// Process SWIFT MT515 (Securities Trade Confirmation)
let mt515 = MT515Message(
    transactionReference: "TF515123456",
    tradeDate: Date.from("2024-03-15"),
    settlementDate: Date.from("2024-03-17"),
    accountIdentification: "ACC123456789",
    financialInstrument: FinancialInstrument(
        isin: "US0378331005",
        description: "Apple Inc. Common Stock",
        quantity: 1000,
        unitPrice: 150.75,
        currency: "USD"
    ),
    tradeDetails: TradeDetails(
        buySellIndicator: "BUY",
        grossAmount: 150750.00,
        netAmount: 150725.50,
        commission: 24.50
    ),
    counterparty: Counterparty(
        bic: "BANKUS33XXX",
        name: "Goldman Sachs International"
    )
)

client.swift.processMT515(mt515) { result in
    switch result {
    case .success(let response):
        print("Trade Confirmation ID: \\(response.confirmationId)")
        print("Settlement Status: \\(response.settlementStatus)")
        print("Blockchain Hash: \\(response.blockchainHash)")
    case .failure(let error):
        print("Trade confirmation failed: \\(error)")
    }
}`
  }
}

const sandboxFeatures = [
  {
    icon: Code,
    title: 'Live API Testing',
    description: 'Test all endpoints with real blockchain integration',
    features: ['Interactive API explorer', 'Real-time responses', 'Blockchain confirmations']
  },
  {
    icon: BarChart3,
    title: 'Performance Metrics',
    description: 'Monitor response times and throughput',
    features: ['Sub-50ms response tracking', 'Throughput analysis', 'Error rate monitoring']
  },
  {
    icon: Shield,
    title: 'Security Testing',
    description: 'Validate authentication and encryption',
    features: ['API key management', 'Encryption verification', 'Audit logging']
  },
  {
    icon: Zap,
    title: 'Mock Data Generation',
    description: 'Generate realistic banking test data',
    features: ['SWIFT message samples', 'Transaction scenarios', 'Error simulations']
  }
]

export default function SandboxPage() {
  const [selectedSDK, setSelectedSDK] = useState('javascript')
  const [isRunning, setIsRunning] = useState(false)

  const runExample = () => {
    setIsRunning(true)
    setTimeout(() => setIsRunning(false), 2000)
  }

  return (
    <div className="min-h-screen bg-background">
      {/* Hero Section */}
      <section className="relative overflow-hidden bg-gradient-to-br from-green-50 via-blue-50 to-purple-100 dark:from-green-950 dark:via-blue-950 dark:to-purple-950">
        <div className="absolute inset-0 bg-grid-black/[0.02] bg-[size:60px_60px]" />
        <div className="relative mx-auto max-w-7xl px-6 py-24 sm:py-32 lg:px-8">
          <div className="text-center">
            <Badge variant="secondary" className="mb-6 text-sm font-medium">
              <Terminal className="w-4 h-4 mr-2" />
              Interactive Developer Sandbox
            </Badge>
            
            <h1 className="text-4xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-6xl lg:text-7xl">
              Test Banking APIs
              <span className="block bg-gradient-to-r from-green-600 to-blue-600 bg-clip-text text-transparent">
                In Real-Time
              </span>
            </h1>
            
            <p className="mx-auto mt-6 max-w-3xl text-lg leading-8 text-gray-600 dark:text-gray-300">
              Interactive sandbox environment with live blockchain integration. Test all APIs, 
              explore 5 native SDKs, and validate your banking-to-blockchain integration with 
              real transaction processing and enterprise-grade security.
            </p>
            
            <div className="mt-10 flex flex-col items-center justify-center gap-4 sm:flex-row">
              <Button size="lg" className="h-12 px-8 bg-green-600 hover:bg-green-700">
                <Play className="mr-2 h-4 w-4" />
                Start Interactive Demo
              </Button>
              <Button asChild variant="outline" size="lg" className="h-12 px-8">
                <Link href="/developer/docs">
                  <Book className="mr-2 h-4 w-4" />
                  View Documentation
                </Link>
              </Button>
            </div>

            {/* Quick Stats */}
            <div className="mt-16 grid grid-cols-2 md:grid-cols-4 gap-8 text-center">
              <div>
                <div className="text-2xl font-bold text-green-600">5 SDKs</div>
                <div className="text-sm text-gray-500">Native Languages</div>
              </div>
              <div>
                <div className="text-2xl font-bold text-blue-600">&lt;50ms</div>
                <div className="text-sm text-gray-500">API Response</div>
              </div>
              <div>
                <div className="text-2xl font-bold text-purple-600">Live</div>
                <div className="text-sm text-gray-500">Blockchain Data</div>
              </div>
              <div>
                <div className="text-2xl font-bold text-orange-600">Free</div>
                <div className="text-sm text-gray-500">Developer Access</div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Interactive SDK Explorer */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Interactive SDK Explorer
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Test our enterprise APIs with live examples in your preferred programming language
            </p>
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
            {/* SDK Selection */}
            <div className="lg:col-span-1">
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center">
                    <Code className="h-5 w-5 mr-2" />
                    Choose SDK
                  </CardTitle>
                  <CardDescription>
                    Select your preferred programming language
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-2">
                    {Object.entries(sdkExamples).map(([key, sdk]) => (
                      <Button
                        key={key}
                        variant={selectedSDK === key ? 'default' : 'outline'}
                        className="w-full justify-start"
                        onClick={() => setSelectedSDK(key)}
                      >
                        {sdk.name}
                      </Button>
                    ))}
                  </div>
                  
                  <div className="mt-6 pt-6 border-t">
                    <h4 className="font-medium mb-3">Installation</h4>
                    <div className="bg-gray-100 dark:bg-gray-800 rounded p-3 text-sm font-mono">
                      {sdkExamples[selectedSDK as keyof typeof sdkExamples].install}
                    </div>
                    <Button variant="outline" size="sm" className="w-full mt-2">
                      <Copy className="h-3 w-3 mr-2" />
                      Copy Command
                    </Button>
                  </div>
                </CardContent>
              </Card>
            </div>

            {/* Code Editor */}
            <div className="lg:col-span-2">
              <Card>
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <CardTitle className="flex items-center">
                      <Terminal className="h-5 w-5 mr-2" />
                      SWIFT MT515 Securities Trade Example
                    </CardTitle>
                    <div className="flex gap-2">
                      <Button variant="outline" size="sm">
                        <Copy className="h-3 w-3 mr-2" />
                        Copy
                      </Button>
                      <Button 
                        size="sm" 
                        className="bg-green-600 hover:bg-green-700"
                        onClick={runExample}
                        disabled={isRunning}
                      >
                        {isRunning ? (
                          <>
                            <div className="animate-spin h-3 w-3 mr-2 border border-white border-t-transparent rounded-full" />
                            Running...
                          </>
                        ) : (
                          <>
                            <Play className="h-3 w-3 mr-2" />
                            Run Example
                          </>
                        )}
                      </Button>
                    </div>
                  </div>
                  <CardDescription>
                    Process securities trade confirmation with blockchain settlement and trade finance validation
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="bg-gray-900 text-gray-100 rounded-lg p-6 overflow-x-auto">
                    <pre className="text-sm">
                      <code>{sdkExamples[selectedSDK as keyof typeof sdkExamples].code}</code>
                    </pre>
                  </div>
                  
                  {isRunning && (
                    <div className="mt-4 p-4 bg-green-50 dark:bg-green-900/20 border border-green-200 dark:border-green-800 rounded-lg">
                      <div className="flex items-center text-green-700 dark:text-green-400">
                        <CheckCircle className="h-4 w-4 mr-2" />
                        <span className="text-sm font-medium">Trade Confirmation Successful</span>
                      </div>
                      <div className="mt-2 text-sm text-green-600 dark:text-green-300">
                        <div>Confirmation ID: MT515_sb_789012345</div>
                        <div>Trade ID: AAPL_1000_20240315</div>
                        <div>Settlement Status: T+2 Pending</div>
                        <div>Blockchain Hash: 0xdef456...abc123</div>
                        <div>Response Time: 42ms</div>
                        <div>Status: Confirmed on Corda Network</div>
                      </div>
                    </div>
                  )}
                </CardContent>
              </Card>
            </div>
          </div>
        </div>
      </section>

      {/* Sandbox Features */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Sandbox Capabilities
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Everything you need to validate your integration before going live
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 md:grid-cols-2 lg:grid-cols-4">
            {sandboxFeatures.map((feature) => {
              const Icon = feature.icon
              return (
                <Card key={feature.title} className="bg-white dark:bg-gray-900 hover:shadow-lg transition-shadow duration-300">
                  <CardContent className="p-6">
                    <div className="flex justify-center mb-4">
                      <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-blue-100 dark:bg-blue-900">
                        <Icon className="h-6 w-6 text-blue-600 dark:text-blue-400" />
                      </div>
                    </div>
                    <h3 className="text-lg font-semibold text-center mb-2">{feature.title}</h3>
                    <p className="text-sm text-gray-600 dark:text-gray-300 text-center mb-4">
                      {feature.description}
                    </p>
                    <ul className="space-y-1">
                      {feature.features.map((item, idx) => (
                        <li key={idx} className="text-xs text-gray-500 dark:text-gray-400 flex items-center">
                          <CheckCircle className="h-3 w-3 text-green-500 mr-1.5" />
                          {item}
                        </li>
                      ))}
                    </ul>
                  </CardContent>
                </Card>
              )
            })}
          </div>
        </div>
      </section>

      {/* API Testing Tabs */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Test All Enterprise APIs
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Comprehensive testing environment for all banking-to-blockchain operations
            </p>
          </div>

          <Tabs defaultValue="swift" className="space-y-8">
            <TabsList className="grid w-full grid-cols-5">
              <TabsTrigger value="swift">Cross-Border Payments</TabsTrigger>
              <TabsTrigger value="compliance">ZK-Compliance</TabsTrigger>
              <TabsTrigger value="tokenized">Tokenized Assets</TabsTrigger>
              <TabsTrigger value="cbdc">CBDC Gateway</TabsTrigger>
              <TabsTrigger value="tradefi">Trade Finance</TabsTrigger>
            </TabsList>
            
            <TabsContent value="swift" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Cross-Border Payment Processing</CardTitle>
                  <CardDescription>Real-time international payments with sub-second settlement via XRP Ledger and Ethereum L2</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    {/* Payment Messages */}
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Payment Messages</h4>
                      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center">
                          <span className="font-medium">MT103</span>
                          <span className="text-xs text-gray-500">Customer Credit Transfer</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center">
                          <span className="font-medium">MT202</span>
                          <span className="text-xs text-gray-500">Financial Institution Transfer</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center">
                          <span className="font-medium">MT101</span>
                          <span className="text-xs text-gray-500">Request for Transfer</span>
                        </Button>
                      </div>
                    </div>

                    {/* Trade Finance Messages */}
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Trade Finance Messages</h4>
                      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT700</span>
                          <span className="text-xs text-gray-500">Letter of Credit Issuance</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT710</span>
                          <span className="text-xs text-gray-500">LC Amendment</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT754</span>
                          <span className="text-xs text-gray-500">LC Payment/Acceptance</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT756</span>
                          <span className="text-xs text-gray-500">LC Reimbursement</span>
                        </Button>
                      </div>
                    </div>

                    {/* Securities Messages */}
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Securities & Investment Messages</h4>
                      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT515</span>
                          <span className="text-xs text-gray-500">Client Confirmation</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT518</span>
                          <span className="text-xs text-gray-500">Market Side Confirmation</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT535</span>
                          <span className="text-xs text-gray-500">Statement of Holdings</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT564</span>
                          <span className="text-xs text-gray-500">Corporate Action</span>
                        </Button>
                      </div>
                    </div>

                    {/* Treasury Messages */}
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Treasury & FX Messages</h4>
                      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-blue-200 hover:border-blue-400">
                          <span className="font-medium text-blue-700">MT300</span>
                          <span className="text-xs text-gray-500">FX Confirmation</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-blue-200 hover:border-blue-400">
                          <span className="font-medium text-blue-700">MT320</span>
                          <span className="text-xs text-gray-500">Fixed Loan/Deposit</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-blue-200 hover:border-blue-400">
                          <span className="font-medium text-blue-700">MT340</span>
                          <span className="text-xs text-gray-500">Forward Rate Agreement</span>
                        </Button>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
            
            <TabsContent value="compliance" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Zero-Knowledge Compliance as a Service</CardTitle>
                  <CardDescription>Privacy-preserving compliance validation with ZK-proofs for AML, KYC, and sanctions screening</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">ZK-Proof Compliance</h4>
                      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-blue-200 hover:border-blue-400">
                          <span className="font-medium text-blue-700">ZK-KYC</span>
                          <span className="text-xs text-gray-500">Identity without exposure</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-blue-200 hover:border-blue-400">
                          <span className="font-medium text-blue-700">ZK-AML</span>
                          <span className="text-xs text-gray-500">Private transaction scoring</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-blue-200 hover:border-blue-400">
                          <span className="font-medium text-blue-700">ZK-Sanctions</span>
                          <span className="text-xs text-gray-500">OFAC screening w/ privacy</span>
                        </Button>
                      </div>
                    </div>
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Regulatory Automation</h4>
                      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">Auto-SAR Filing</span>
                          <span className="text-xs text-gray-500">Suspicious activity reports</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">Real-time CTR</span>
                          <span className="text-xs text-gray-500">Currency transaction reports</span>
                        </Button>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="tokenized" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Tokenized Assets Management</CardTitle>
                  <CardDescription>Create, manage, and trade tokenized real-world assets with regulatory compliance</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Asset Tokenization</h4>
                      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">Real Estate</span>
                          <span className="text-xs text-gray-500">Property fractionalization</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">Securities</span>
                          <span className="text-xs text-gray-500">Equity & debt tokens</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">Commodities</span>
                          <span className="text-xs text-gray-500">Gold, oil, carbon credits</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">Art & Collectibles</span>
                          <span className="text-xs text-gray-500">NFT-backed assets</span>
                        </Button>
                      </div>
                    </div>
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Trading & Settlement</h4>
                      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-orange-200 hover:border-orange-400">
                          <span className="font-medium text-orange-700">Atomic Swaps</span>
                          <span className="text-xs text-gray-500">Trustless trading</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-orange-200 hover:border-orange-400">
                          <span className="font-medium text-orange-700">DvP Settlement</span>
                          <span className="text-xs text-gray-500">Delivery vs Payment</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-orange-200 hover:border-orange-400">
                          <span className="font-medium text-orange-700">Yield Distribution</span>
                          <span className="text-xs text-gray-500">Automated dividends</span>
                        </Button>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="cbdc" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Central Bank Digital Currency (CBDC) Gateway</CardTitle>
                  <CardDescription>Online and offline CBDC transactions with central bank integration and offline capability</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">CBDC Online Operations</h4>
                      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-red-200 hover:border-red-400">
                          <span className="font-medium text-red-700">Digital Yuan</span>
                          <span className="text-xs text-gray-500">PBOC integration</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-red-200 hover:border-red-400">
                          <span className="font-medium text-red-700">Digital Euro</span>
                          <span className="text-xs text-gray-500">ECB pilot program</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-red-200 hover:border-red-400">
                          <span className="font-medium text-red-700">Digital Rupee</span>
                          <span className="text-xs text-gray-500">RBI implementation</span>
                        </Button>
                      </div>
                    </div>
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">CBDC Offline Capabilities</h4>
                      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-indigo-200 hover:border-indigo-400">
                          <span className="font-medium text-indigo-700">Offline Payments</span>
                          <span className="text-xs text-gray-500">No internet required</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-indigo-200 hover:border-indigo-400">
                          <span className="font-medium text-indigo-700">NFC Transactions</span>
                          <span className="text-xs text-gray-500">Contactless payments</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-indigo-200 hover:border-indigo-400">
                          <span className="font-medium text-indigo-700">Sync & Reconcile</span>
                          <span className="text-xs text-gray-500">Batch settlement</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-indigo-200 hover:border-indigo-400">
                          <span className="font-medium text-indigo-700">Wallet Management</span>
                          <span className="text-xs text-gray-500">Balance validation</span>
                        </Button>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            <TabsContent value="tradefi" className="space-y-6">
              <Card>
                <CardHeader>
                  <CardTitle>Trade Finance & Securities Processing</CardTitle>
                  <CardDescription>End-to-end trade finance automation with letters of credit, securities trading, and smart contracts</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    {/* Trade Finance Messages */}
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Trade Finance Messages</h4>
                      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT700</span>
                          <span className="text-xs text-gray-500">Letter of Credit Issuance</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT710</span>
                          <span className="text-xs text-gray-500">LC Amendment</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT754</span>
                          <span className="text-xs text-gray-500">LC Payment/Acceptance</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-green-200 hover:border-green-400">
                          <span className="font-medium text-green-700">MT756</span>
                          <span className="text-xs text-gray-500">LC Reimbursement</span>
                        </Button>
                      </div>
                    </div>

                    {/* Securities Messages */}
                    <div>
                      <h4 className="font-medium mb-3 text-gray-900 dark:text-white">Securities & Investment Messages</h4>
                      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT515</span>
                          <span className="text-xs text-gray-500">Client Confirmation</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT518</span>
                          <span className="text-xs text-gray-500">Market Side Confirmation</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT535</span>
                          <span className="text-xs text-gray-500">Statement of Holdings</span>
                        </Button>
                        <Button variant="outline" className="h-20 flex flex-col items-center justify-center border-purple-200 hover:border-purple-400">
                          <span className="font-medium text-purple-700">MT564</span>
                          <span className="text-xs text-gray-500">Corporate Action</span>
                        </Button>
                      </div>
                    </div>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>
      </section>

      {/* Developer Resources */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Developer Resources
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Everything you need to integrate and build with our platform
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 md:grid-cols-2 lg:grid-cols-3">
            <Card className="bg-white dark:bg-gray-900">
              <CardContent className="p-6">
                <div className="flex items-center mb-4">
                  <div className="flex h-10 w-10 items-center justify-center rounded-lg bg-blue-100 dark:bg-blue-900 mr-3">
                    <FileText className="h-5 w-5 text-blue-600 dark:text-blue-400" />
                  </div>
                  <h3 className="text-lg font-semibold">API Documentation</h3>
                </div>
                <p className="text-sm text-gray-600 dark:text-gray-300 mb-4">
                  Comprehensive guides, tutorials, and reference documentation
                </p>
                <Button asChild variant="outline" className="w-full">
                  <Link href="/developer/docs">
                    View Documentation
                    <ArrowRight className="ml-2 h-4 w-4" />
                  </Link>
                </Button>
              </CardContent>
            </Card>

            <Card className="bg-white dark:bg-gray-900">
              <CardContent className="p-6">
                <div className="flex items-center mb-4">
                  <div className="flex h-10 w-10 items-center justify-center rounded-lg bg-green-100 dark:bg-green-900 mr-3">
                    <Download className="h-5 w-5 text-green-600 dark:text-green-400" />
                  </div>
                  <h3 className="text-lg font-semibold">SDK Downloads</h3>
                </div>
                <p className="text-sm text-gray-600 dark:text-gray-300 mb-4">
                  Native SDKs for JavaScript, Python, Java, .NET, and Mobile
                </p>
                <Button asChild variant="outline" className="w-full">
                  <Link href="/developer/sdks">
                    Download SDKs
                    <ArrowRight className="ml-2 h-4 w-4" />
                  </Link>
                </Button>
              </CardContent>
            </Card>

            <Card className="bg-white dark:bg-gray-900">
              <CardContent className="p-6">
                <div className="flex items-center mb-4">
                  <div className="flex h-10 w-10 items-center justify-center rounded-lg bg-purple-100 dark:bg-purple-900 mr-3">
                    <Github className="h-5 w-5 text-purple-600 dark:text-purple-400" />
                  </div>
                  <h3 className="text-lg font-semibold">Open Source</h3>
                </div>
                <p className="text-sm text-gray-600 dark:text-gray-300 mb-4">
                  SwiftParser library and community tools on GitHub
                </p>
                <Button asChild variant="outline" className="w-full">
                  <Link href="/developer/community">
                    Join Community
                    <ArrowRight className="ml-2 h-4 w-4" />
                  </Link>
                </Button>
              </CardContent>
            </Card>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 bg-gradient-to-r from-green-600 to-blue-600 text-white">
        <div className="mx-auto max-w-7xl px-6 lg:px-8 text-center">
          <h2 className="text-3xl font-bold mb-6">
            Ready to Build Your Integration?
          </h2>
          <p className="text-xl text-green-100 mb-8 max-w-2xl mx-auto">
            Start with our sandbox environment and move to production when you're ready. 
            Get enterprise support throughout your development journey.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Button asChild size="lg" className="bg-white text-gray-900 hover:bg-gray-100">
              <Link href="/developer/signup">
                Get Free API Keys
                <ArrowRight className="ml-2 h-4 w-4" />
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg" className="border-white text-white hover:bg-white/10">
              <Link href="/enterprise/contact">
                Talk to Sales
              </Link>
            </Button>
          </div>
        </div>
      </section>
    </div>
  )
}