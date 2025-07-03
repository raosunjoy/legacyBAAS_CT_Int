import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { 
  TrendingUp,
  Shield,
  Zap,
  Globe,
  Clock,
  CheckCircle,
  ArrowRight,
  Building2,
  Users,
  BarChart,
  Lock,
  HeadphonesIcon,
  FileText
} from 'lucide-react'
import Link from 'next/link'

const enterpriseFeatures = [
  {
    icon: Zap,
    title: 'Instant Provisioning',
    description: 'API keys and environment setup in under 5 minutes',
    details: ['Automated onboarding', 'Self-service dashboard', 'Immediate access']
  },
  {
    icon: Shield,
    title: 'Enterprise Security',
    description: 'Banking-grade security with SOC 2 Type II compliance',
    details: ['End-to-end encryption', 'Multi-factor authentication', 'Audit trails']
  },
  {
    icon: BarChart,
    title: 'Advanced Analytics',
    description: 'Real-time usage monitoring and performance insights',
    details: ['Custom dashboards', 'Usage analytics', 'Performance metrics']
  },
  {
    icon: HeadphonesIcon,
    title: 'Priority Support',
    description: '24/7 dedicated support with SLA guarantees',
    details: ['Dedicated account manager', 'Priority ticket handling', '99.9% uptime SLA']
  },
  {
    icon: FileText,
    title: 'Compliance Reporting',
    description: 'Automated compliance reports and audit documentation',
    details: ['Regulatory reporting', 'Audit logs', 'Compliance dashboards']
  },
  {
    icon: Users,
    title: 'Team Management',
    description: 'Advanced user management and access controls',
    details: ['Role-based access', 'Team collaboration', 'Permission management']
  }
]

const pricingTiers = [
  {
    name: 'Starter',
    price: '$2,500',
    period: 'per month',
    description: 'Perfect for proof-of-concepts and initial implementations',
    features: [
      'Up to 100K transactions/month',
      'Basic API access',
      'Email support',
      'Standard SLA (99.5%)',
      'Basic analytics',
      '5 team members'
    ],
    cta: 'Start Free Trial',
    popular: false
  },
  {
    name: 'Professional',
    price: '$7,500',
    period: 'per month',
    description: 'Ideal for production deployments and growing enterprises',
    features: [
      'Up to 1M transactions/month',
      'Full API access',
      'Priority support',
      'Enhanced SLA (99.9%)',
      'Advanced analytics',
      '25 team members',
      'Custom integrations',
      'Compliance reporting'
    ],
    cta: 'Get Started',
    popular: true
  },
  {
    name: 'Enterprise',
    price: 'Custom',
    period: 'pricing',
    description: 'Tailored solutions for large-scale enterprise deployments',
    features: [
      'Unlimited transactions',
      'White-label options',
      'Dedicated support',
      'Custom SLA (99.99%)',
      'Custom analytics',
      'Unlimited team members',
      'On-premise deployment',
      'Custom compliance'
    ],
    cta: 'Contact Sales',
    popular: false
  }
]

const clientLogos = [
  'National Bank of Americas',
  'Global Financial Group',
  'Enterprise Banking Corp',
  'International Trade Bank',
  'Fortune Global Bank',
  'Digital Banking Inc'
]

export default function EnterprisePage() {
  return (
    <div className="min-h-screen bg-background">
      {/* Hero Section */}
      <section className="relative overflow-hidden bg-gradient-to-br from-purple-50 via-blue-50 to-indigo-100 dark:from-purple-950 dark:via-blue-950 dark:to-indigo-950">
        <div className="absolute inset-0 bg-grid-black/[0.02] bg-[size:60px_60px]" />
        <div className="relative mx-auto max-w-7xl px-6 py-24 sm:py-32 lg:px-8">
          <div className="text-center">
            <Badge variant="secondary" className="mb-6 text-sm font-medium">
              <Building2 className="w-4 h-4 mr-2" />
              Enterprise Self-Service Portal
            </Badge>
            
            <h1 className="text-4xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-6xl lg:text-7xl">
              Enterprise Banking
              <span className="block bg-gradient-to-r from-purple-600 to-blue-600 bg-clip-text text-transparent">
                Made Simple
              </span>
            </h1>
            
            <p className="mx-auto mt-6 max-w-3xl text-lg leading-8 text-gray-600 dark:text-gray-300">
              Self-service banking-to-blockchain integration for Fortune 500 companies. 
              Instant provisioning, enterprise security, and white-glove support with 
              banking-grade compliance built-in.
            </p>
            
            <div className="mt-10 flex flex-col items-center justify-center gap-4 sm:flex-row">
              <Button asChild size="lg" className="h-12 px-8 bg-purple-600 hover:bg-purple-700">
                <Link href="/enterprise/register">
                  Start Free Trial
                  <ArrowRight className="ml-2 h-4 w-4" />
                </Link>
              </Button>
              <Button asChild variant="outline" size="lg" className="h-12 px-8">
                <Link href="/enterprise/demo">
                  Schedule Demo
                </Link>
              </Button>
            </div>

            {/* Trust Indicators */}
            <div className="mt-16">
              <p className="text-sm text-gray-500 dark:text-gray-400 mb-8">
                Trusted by 120+ Fortune 500 companies
              </p>
              <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-8 items-center opacity-60">
                {clientLogos.map((logo) => (
                  <div key={logo} className="text-center">
                    <div className="h-8 flex items-center justify-center">
                      <span className="text-xs font-medium text-gray-400">{logo}</span>
                    </div>
                  </div>
                ))}
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* Features Section */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Enterprise-Grade Platform
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Everything your enterprise needs for successful banking-to-blockchain integration
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 md:grid-cols-2 lg:grid-cols-3">
            {enterpriseFeatures.map((feature) => {
              const Icon = feature.icon
              return (
                <Card key={feature.title} className="hover:shadow-lg transition-shadow duration-300">
                  <CardContent className="p-6">
                    <div className="flex items-start space-x-4">
                      <div className="flex-shrink-0">
                        <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-purple-100 dark:bg-purple-900">
                          <Icon className="h-6 w-6 text-purple-600 dark:text-purple-400" />
                        </div>
                      </div>
                      <div className="flex-1 min-w-0">
                        <h3 className="text-lg font-semibold text-gray-900 dark:text-white mb-2">
                          {feature.title}
                        </h3>
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
                      </div>
                    </div>
                  </CardContent>
                </Card>
              )
            })}
          </div>
        </div>
      </section>

      {/* Pricing Section */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Enterprise Pricing
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Transparent pricing designed for enterprise needs and procurement processes
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
            {pricingTiers.map((tier, index) => (
              <Card key={tier.name} className={`relative ${tier.popular ? 'border-2 border-purple-500 shadow-lg' : ''}`}>
                {tier.popular && (
                  <div className="absolute -top-4 left-1/2 transform -translate-x-1/2">
                    <Badge className="bg-purple-600">Most Popular</Badge>
                  </div>
                )}
                <CardHeader className="text-center">
                  <CardTitle className="text-2xl">{tier.name}</CardTitle>
                  <CardDescription className="text-sm">{tier.description}</CardDescription>
                  <div className="mt-4">
                    <span className="text-4xl font-bold text-gray-900 dark:text-white">{tier.price}</span>
                    <span className="text-lg text-gray-500 dark:text-gray-400">/{tier.period}</span>
                  </div>
                </CardHeader>
                <CardContent>
                  <ul className="space-y-3 mb-8">
                    {tier.features.map((feature) => (
                      <li key={feature} className="flex items-center text-sm">
                        <CheckCircle className="h-4 w-4 text-green-500 mr-3" />
                        {feature}
                      </li>
                    ))}
                  </ul>
                  <Button 
                    asChild 
                    className={`w-full ${tier.popular ? 'bg-purple-600 hover:bg-purple-700' : ''}`}
                    variant={tier.popular ? 'default' : 'outline'}
                  >
                    <Link href={`/enterprise/register?plan=${tier.name.toLowerCase()}`}>
                      {tier.cta}
                    </Link>
                  </Button>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* Implementation Timeline */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              5-Minute Implementation
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              From registration to first transaction in minutes, not months
            </p>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            {[
              {
                step: '1',
                title: 'Register & Verify',
                description: 'Complete enterprise registration and identity verification',
                time: '2 minutes'
              },
              {
                step: '2',
                title: 'Instant Provisioning',
                description: 'Receive API keys and access to sandbox environment',
                time: '30 seconds'
              },
              {
                step: '3',
                title: 'Integration Setup',
                description: 'Connect your systems using our comprehensive SDKs',
                time: '2 minutes'
              },
              {
                step: '4',
                title: 'Go Live',
                description: 'Execute your first banking-to-blockchain transaction',
                time: '30 seconds'
              }
            ].map((step) => (
              <Card key={step.step} className="text-center">
                <CardContent className="p-6">
                  <div className="flex justify-center mb-4">
                    <div className="flex h-12 w-12 items-center justify-center rounded-full bg-purple-100 dark:bg-purple-900 text-purple-600 dark:text-purple-400 font-bold text-lg">
                      {step.step}
                    </div>
                  </div>
                  <h3 className="text-lg font-semibold mb-2">{step.title}</h3>
                  <p className="text-sm text-gray-600 dark:text-gray-300 mb-2">{step.description}</p>
                  <Badge variant="outline" className="text-xs">
                    <Clock className="h-3 w-3 mr-1" />
                    {step.time}
                  </Badge>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 bg-gradient-to-r from-purple-600 to-blue-600 text-white">
        <div className="mx-auto max-w-7xl px-6 lg:px-8 text-center">
          <h2 className="text-3xl font-bold mb-6">
            Ready to Transform Your Banking Infrastructure?
          </h2>
          <p className="text-xl text-purple-100 mb-8 max-w-2xl mx-auto">
            Join 120+ Fortune 500 companies already using our platform. Start your free trial today.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Button asChild size="lg" className="bg-white text-gray-900 hover:bg-gray-100">
              <Link href="/enterprise/register">
                Start Free Trial
                <ArrowRight className="ml-2 h-4 w-4" />
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg" className="border-white text-white hover:bg-white/10">
              <Link href="/enterprise/demo">
                Schedule Demo
              </Link>
            </Button>
          </div>
          
          <div className="mt-12 grid grid-cols-1 md:grid-cols-3 gap-8 text-center">
            <div>
              <div className="text-3xl font-bold mb-2">5 min</div>
              <div className="text-purple-200">Setup Time</div>
            </div>
            <div>
              <div className="text-3xl font-bold mb-2">99.99%</div>
              <div className="text-purple-200">Uptime SLA</div>
            </div>
            <div>
              <div className="text-3xl font-bold mb-2">24/7</div>
              <div className="text-purple-200">Support</div>
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}