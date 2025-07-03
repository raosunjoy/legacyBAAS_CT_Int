import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { 
  Shield,
  Zap,
  Globe,
  CheckCircle,
  ArrowRight,
  Building2,
  BarChart,
  Lock,
  HeadphonesIcon,
  FileText,
  Award,
  Users,
  Layers,
  Code,
  Server,
  TrendingUp
} from 'lucide-react'
import Link from 'next/link'

const securityFeatures = [
  {
    icon: Shield,
    title: 'SOC 2 Type II Certified',
    description: 'Comprehensive security controls audited by third-party assessors',
    details: ['Annual compliance audits', 'Continuous monitoring', 'Enterprise controls']
  },
  {
    icon: Lock,
    title: 'Banking-Grade Encryption',
    description: 'End-to-end AES-256 encryption with HSM key management',
    details: ['Hardware security modules', 'Key rotation policies', 'Zero-knowledge architecture']
  },
  {
    icon: FileText,
    title: 'Regulatory Compliance',
    description: 'Built-in compliance for global banking regulations',
    details: ['PCI DSS Level 1', 'ISO 27001 certified', 'GDPR compliant']
  }
]

const platformFeatures = [
  {
    icon: Zap,
    title: 'Real-Time Settlement',
    description: 'Sub-second transaction processing across blockchain networks',
    details: ['<50ms API response', 'Multi-chain routing', 'Atomic settlements']
  },
  {
    icon: Layers,
    title: 'Native Banking Integration',
    description: 'Direct connectivity to TCS BaNCS and core banking systems',
    details: ['SWIFT MT messaging', 'ISO 20022 support', 'Legacy system APIs']
  },
  {
    icon: Globe,
    title: 'Multi-Blockchain Support',
    description: 'Universal connectivity across enterprise blockchain networks',
    details: ['XRP Ledger', 'Ethereum L2', 'Algorand & Corda']
  }
]

const caseStudies = [
  {
    client: 'National Bank of Americas',
    industry: 'Commercial Banking',
    challenge: '48-hour international wire transfers with high fees',
    solution: 'Real-time cross-border settlements via XRP Ledger integration',
    results: ['80% faster settlements', '60% cost reduction', '$2.8B annual volume'],
    logo: 'NBA'
  },
  {
    client: 'Global Trade Finance Corp',
    industry: 'Trade Finance', 
    challenge: 'Manual letter of credit processing with fraud risks',
    solution: 'Automated smart contract workflow on Corda network',
    results: ['95% processing automation', '40% fraud reduction', '99.9% accuracy'],
    logo: 'GTF'
  },
  {
    client: 'International Investment Bank',
    industry: 'Investment Banking',
    challenge: 'Complex multi-party settlement reconciliation',
    solution: 'Algorand-based atomic swap infrastructure',
    results: ['Real-time reconciliation', '70% ops cost savings', 'Zero settlement failures'],
    logo: 'IIB'
  }
]

const certifications = [
  { name: 'SOC 2 Type II', authority: 'AICPA' },
  { name: 'ISO 27001', authority: 'ISO' },
  { name: 'PCI DSS Level 1', authority: 'PCI Council' },
  { name: 'FedRAMP Ready', authority: 'GSA' },
  { name: 'GDPR Compliant', authority: 'EU' },
  { name: 'CCPA Compliant', authority: 'California' }
]

export default function EnterprisePage() {
  return (
    <div className="min-h-screen bg-background">
      {/* Hero Section - Enterprise Banking Focus */}
      <section className="relative overflow-hidden bg-gradient-to-br from-slate-50 via-blue-50 to-indigo-100 dark:from-slate-950 dark:via-blue-950 dark:to-indigo-950">
        <div className="absolute inset-0 bg-grid-black/[0.02] bg-[size:60px_60px]" />
        <div className="relative mx-auto max-w-7xl px-6 py-24 sm:py-32 lg:px-8">
          <div className="text-center">
            <Badge variant="secondary" className="mb-6 text-sm font-medium">
              <Award className="w-4 h-4 mr-2" />
              Trusted by 120+ Fortune 500 Banks Worldwide
            </Badge>
            
            <h1 className="text-4xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-6xl lg:text-7xl">
              Enterprise Banking-to-Blockchain
              <span className="block bg-gradient-to-r from-blue-600 to-indigo-600 bg-clip-text text-transparent">
                Infrastructure Platform
              </span>
            </h1>
            
            <p className="mx-auto mt-6 max-w-4xl text-xl leading-8 text-gray-600 dark:text-gray-300">
              The world's most secure and scalable platform for seamless SWIFT-to-blockchain integration. 
              Trusted by Fortune 500 banks for real-time cross-border payments, trade finance automation, 
              and multi-party settlement with enterprise-grade security and 99.99% uptime guarantee.
            </p>
            
            <div className="mt-10 flex flex-col items-center justify-center gap-4 sm:flex-row">
              <Button asChild size="lg" className="h-14 px-10 bg-blue-600 hover:bg-blue-700 text-lg">
                <Link href="/enterprise/demo">
                  Schedule Executive Demo
                  <ArrowRight className="ml-2 h-5 w-5" />
                </Link>
              </Button>
              <Button asChild variant="outline" size="lg" className="h-14 px-10 text-lg">
                <Link href="/developer/sandbox">
                  Try Developer Sandbox
                </Link>
              </Button>
            </div>

            {/* Trust Metrics */}
            <div className="mt-20 grid grid-cols-2 md:grid-cols-4 gap-8 text-center">
              <div>
                <div className="text-3xl font-bold text-blue-600">$2.8B+</div>
                <div className="text-sm text-gray-500">Annual Transaction Volume</div>
              </div>
              <div>
                <div className="text-3xl font-bold text-green-600">99.99%</div>
                <div className="text-sm text-gray-500">Uptime SLA</div>
              </div>
              <div>
                <div className="text-3xl font-bold text-purple-600">&lt;50ms</div>
                <div className="text-sm text-gray-500">Average Response Time</div>
              </div>
              <div>
                <div className="text-3xl font-bold text-orange-600">120+</div>
                <div className="text-sm text-gray-500">Enterprise Banks</div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Security & Compliance */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Banking-Grade Security & Compliance
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Enterprise security standards with continuous compliance monitoring
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3 mb-16">
            {securityFeatures.map((feature) => {
              const Icon = feature.icon
              return (
                <Card key={feature.title} className="hover:shadow-lg transition-shadow duration-300">
                  <CardContent className="p-6">
                    <div className="flex items-center mb-4">
                      <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-blue-100 dark:bg-blue-900 mr-4">
                        <Icon className="h-6 w-6 text-blue-600 dark:text-blue-400" />
                      </div>
                      <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                        {feature.title}
                      </h3>
                    </div>
                    <p className="text-gray-600 dark:text-gray-300 text-sm mb-4">
                      {feature.description}
                    </p>
                    <ul className="space-y-1">
                      {feature.details.map((detail, idx) => (
                        <li key={idx} className="text-xs text-gray-500 dark:text-gray-400 flex items-center">
                          <CheckCircle className="h-3 w-3 text-green-500 mr-1.5" />
                          {detail}
                        </li>
                      ))}
                    </ul>
                  </CardContent>
                </Card>
              )
            })}
          </div>

          {/* Certifications */}
          <div className="bg-gray-50 dark:bg-gray-800 rounded-2xl p-8">
            <h3 className="text-xl font-semibold text-center mb-8">Industry Certifications</h3>
            <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-6">
              {certifications.map((cert) => (
                <div key={cert.name} className="text-center">
                  <div className="bg-white dark:bg-gray-700 rounded-lg p-4 shadow-sm">
                    <div className="font-semibold text-sm">{cert.name}</div>
                    <div className="text-xs text-gray-500 mt-1">{cert.authority}</div>
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>
      </section>

      {/* Platform Capabilities */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Enterprise Platform Capabilities
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Production-ready infrastructure for mission-critical banking operations
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
            {platformFeatures.map((feature) => {
              const Icon = feature.icon
              return (
                <Card key={feature.title} className="bg-white dark:bg-gray-900 hover:shadow-lg transition-shadow duration-300">
                  <CardContent className="p-6">
                    <div className="flex items-center mb-4">
                      <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-green-100 dark:bg-green-900 mr-4">
                        <Icon className="h-6 w-6 text-green-600 dark:text-green-400" />
                      </div>
                      <h3 className="text-lg font-semibold text-gray-900 dark:text-white">
                        {feature.title}
                      </h3>
                    </div>
                    <p className="text-gray-600 dark:text-gray-300 text-sm mb-4">
                      {feature.description}
                    </p>
                    <ul className="space-y-1">
                      {feature.details.map((detail, idx) => (
                        <li key={idx} className="text-xs text-gray-500 dark:text-gray-400 flex items-center">
                          <CheckCircle className="h-3 w-3 text-green-500 mr-1.5" />
                          {detail}
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

      {/* Case Studies */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Proven Results with Fortune 500 Banks
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Real implementations, measurable outcomes, transformational impact
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
            {caseStudies.map((study) => (
              <Card key={study.client} className="hover:shadow-lg transition-shadow duration-300">
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-blue-600 text-white font-bold">
                      {study.logo}
                    </div>
                    <Badge variant="outline">{study.industry}</Badge>
                  </div>
                  <CardTitle className="text-lg">{study.client}</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    <div>
                      <h4 className="font-medium text-sm text-gray-900 dark:text-white">Challenge</h4>
                      <p className="text-sm text-gray-600 dark:text-gray-300">{study.challenge}</p>
                    </div>
                    <div>
                      <h4 className="font-medium text-sm text-gray-900 dark:text-white">Solution</h4>
                      <p className="text-sm text-gray-600 dark:text-gray-300">{study.solution}</p>
                    </div>
                    <div>
                      <h4 className="font-medium text-sm text-gray-900 dark:text-white mb-2">Results</h4>
                      <ul className="space-y-1">
                        {study.results.map((result, idx) => (
                          <li key={idx} className="text-sm text-green-600 dark:text-green-400 flex items-center">
                            <TrendingUp className="h-3 w-3 mr-1.5" />
                            {result}
                          </li>
                        ))}
                      </ul>
                    </div>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* Developer Resources */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Developer-First Integration
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Enterprise APIs with comprehensive SDKs and sandbox environment
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 md:grid-cols-2 lg:grid-cols-4">
            {[
              { icon: Code, title: '5 Native SDKs', desc: 'JavaScript, Python, Java, .NET, Mobile' },
              { icon: Server, title: 'Interactive Sandbox', desc: 'Test all APIs with live blockchain data' },
              { icon: FileText, title: 'Complete Documentation', desc: 'API guides, tutorials, code examples' },
              { icon: Users, title: 'Developer Community', desc: 'Technical forums and open-source tools' }
            ].map((item) => {
              const Icon = item.icon
              return (
                <Card key={item.title} className="text-center bg-white dark:bg-gray-900">
                  <CardContent className="p-6">
                    <div className="flex justify-center mb-4">
                      <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-purple-100 dark:bg-purple-900">
                        <Icon className="h-6 w-6 text-purple-600 dark:text-purple-400" />
                      </div>
                    </div>
                    <h3 className="text-lg font-semibold mb-2">{item.title}</h3>
                    <p className="text-sm text-gray-600 dark:text-gray-300">{item.desc}</p>
                  </CardContent>
                </Card>
              )
            })}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 bg-gradient-to-r from-blue-600 to-indigo-600 text-white">
        <div className="mx-auto max-w-7xl px-6 lg:px-8 text-center">
          <h2 className="text-4xl font-bold mb-6">
            Ready to Transform Your Banking Infrastructure?
          </h2>
          <p className="text-xl text-blue-100 mb-8 max-w-3xl mx-auto">
            Join 120+ Fortune 500 banks already using our platform for secure, 
            scalable banking-to-blockchain integration. Get enterprise pricing and 
            dedicated implementation support.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center mb-12">
            <Button asChild size="lg" className="h-14 px-10 bg-white text-blue-600 hover:bg-gray-100 text-lg font-medium">
              <Link href="/enterprise/contact">
                Contact Enterprise Sales
                <ArrowRight className="ml-2 h-5 w-5" />
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg" className="h-14 px-10 border-white text-white hover:bg-white/10 text-lg">
              <Link href="/developer/sandbox">
                Explore Developer Sandbox
              </Link>
            </Button>
          </div>
          
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8 text-center">
            <div>
              <div className="text-3xl font-bold mb-2">Enterprise Pricing</div>
              <div className="text-blue-200">Custom contracts available</div>
            </div>
            <div>
              <div className="text-3xl font-bold mb-2">Dedicated Support</div>
              <div className="text-blue-200">24/7 enterprise SLA</div>
            </div>
            <div>
              <div className="text-3xl font-bold mb-2">Global Deployment</div>
              <div className="text-blue-200">Multi-region infrastructure</div>
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}