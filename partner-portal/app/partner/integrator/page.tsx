import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { 
  Users,
  Award,
  DollarSign,
  BookOpen,
  Code,
  Handshake,
  ArrowRight,
  CheckCircle,
  Star,
  Building2
} from 'lucide-react'
import Link from 'next/link'

const siProgram = {
  commission: '20%',
  requirements: [
    'Technical certification completion',
    'Minimum 2 successful implementations',
    'Dedicated technical resources',
    'Banking industry experience preferred'
  ],
  benefits: [
    'Implementation revenue sharing',
    'Technical certification programs',
    'Reference architecture access',
    'Developer sandbox environments',
    'Professional services support',
    'Enterprise customer introductions',
    'Priority technical support',
    'Quarterly business reviews'
  ],
  certificationLevels: [
    {
      level: 'Associate SI',
      requirements: ['Complete basic training', 'Pass technical assessment', '1 demo implementation'],
      benefits: ['20% revenue share', 'Basic sandbox access', 'Community support']
    },
    {
      level: 'Certified SI',
      requirements: ['Complete advanced training', '2 successful implementations', 'Customer references'],
      benefits: ['25% revenue share', 'Full sandbox access', 'Priority support', 'Marketing co-op']
    },
    {
      level: 'Elite SI',
      requirements: ['5+ implementations', 'Specialized expertise', 'Proven track record'],
      benefits: ['30% revenue share', 'Dedicated resources', 'Executive access', 'Custom terms']
    }
  ]
}

export default function SystemIntegratorPage() {
  return (
    <div className="min-h-screen bg-background">
      {/* Hero Section */}
      <section className="relative overflow-hidden bg-gradient-to-br from-green-50 via-blue-50 to-indigo-100 dark:from-green-950 dark:via-blue-950 dark:to-indigo-950">
        <div className="absolute inset-0 bg-grid-black/[0.02] bg-[size:60px_60px]" />
        <div className="relative mx-auto max-w-7xl px-6 py-24 sm:py-32 lg:px-8">
          <div className="text-center">
            <Badge variant="secondary" className="mb-6 text-sm font-medium">
              <Users className="w-4 h-4 mr-2" />
              System Integrator Partner Program
            </Badge>
            
            <h1 className="text-4xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-6xl lg:text-7xl">
              Enterprise Implementation
              <span className="block bg-gradient-to-r from-green-600 to-blue-600 bg-clip-text text-transparent">
                Excellence
              </span>
            </h1>
            
            <p className="mx-auto mt-6 max-w-3xl text-lg leading-8 text-gray-600 dark:text-gray-300">
              Join our elite System Integrator program and earn up to 30% revenue share on enterprise 
              banking-to-blockchain implementations. Access exclusive training, certification, and 
              enterprise customer introductions.
            </p>
            
            <div className="mt-10 flex flex-col items-center justify-center gap-4 sm:flex-row">
              <Button asChild size="lg" className="h-12 px-8 bg-green-600 hover:bg-green-700">
                <Link href="/partner/integrator/register">
                  Apply for SI Program
                  <ArrowRight className="ml-2 h-4 w-4" />
                </Link>
              </Button>
              <Button asChild variant="outline" size="lg" className="h-12 px-8">
                <Link href="/partner/integrator/certification">
                  View Certification Path
                </Link>
              </Button>
            </div>
          </div>
        </div>
      </section>

      {/* Program Overview */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              System Integrator Program Benefits
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Everything you need to successfully implement enterprise banking-to-blockchain solutions
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-2">
            <Card className="h-full">
              <CardHeader>
                <CardTitle className="flex items-center">
                  <DollarSign className="h-6 w-6 text-green-600 mr-2" />
                  Revenue Opportunities
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <div className="flex items-center justify-between p-4 bg-green-50 dark:bg-green-900/20 rounded-lg">
                    <span className="font-medium">Implementation Revenue Share</span>
                    <Badge className="bg-green-600">{siProgram.commission}+ Commission</Badge>
                  </div>
                  <ul className="space-y-2">
                    <li className="flex items-center text-sm">
                      <CheckCircle className="h-4 w-4 text-green-600 mr-2" />
                      Average project value: $75K - $2M+
                    </li>
                    <li className="flex items-center text-sm">
                      <CheckCircle className="h-4 w-4 text-green-600 mr-2" />
                      Recurring revenue on maintenance contracts
                    </li>
                    <li className="flex items-center text-sm">
                      <CheckCircle className="h-4 w-4 text-green-600 mr-2" />
                      Up to 30% for Elite SI partners
                    </li>
                  </ul>
                </div>
              </CardContent>
            </Card>

            <Card className="h-full">
              <CardHeader>
                <CardTitle className="flex items-center">
                  <Award className="h-6 w-6 text-blue-600 mr-2" />
                  Certification & Training
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <ul className="space-y-2">
                    {['Technical certification programs', 'Hands-on implementation training', 'Banking domain expertise', 'Blockchain integration mastery'].map((item) => (
                      <li key={item} className="flex items-center text-sm">
                        <CheckCircle className="h-4 w-4 text-blue-600 mr-2" />
                        {item}
                      </li>
                    ))}
                  </ul>
                  <Button asChild variant="outline" className="w-full">
                    <Link href="/partner/integrator/training">
                      View Training Curriculum
                    </Link>
                  </Button>
                </div>
              </CardContent>
            </Card>
          </div>
        </div>
      </section>

      {/* Certification Levels */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              SI Certification Levels
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Progress through our certification tiers to unlock higher revenue shares and exclusive benefits
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
            {siProgram.certificationLevels.map((cert, index) => (
              <Card key={cert.level} className={`relative ${index === 1 ? 'border-2 border-blue-500 shadow-lg' : ''}`}>
                {index === 1 && (
                  <div className="absolute -top-4 left-1/2 transform -translate-x-1/2">
                    <Badge className="bg-blue-600">Most Popular</Badge>
                  </div>
                )}
                <CardHeader className="text-center">
                  <div className="flex justify-center mb-4">
                    <div className={`flex h-16 w-16 items-center justify-center rounded-full ${
                      index === 0 ? 'bg-gray-100 dark:bg-gray-800' :
                      index === 1 ? 'bg-blue-100 dark:bg-blue-900' :
                      'bg-purple-100 dark:bg-purple-900'
                    }`}>
                      <Star className={`h-8 w-8 ${
                        index === 0 ? 'text-gray-600 dark:text-gray-400' :
                        index === 1 ? 'text-blue-600 dark:text-blue-400' :
                        'text-purple-600 dark:text-purple-400'
                      }`} />
                    </div>
                  </div>
                  <CardTitle className="text-xl">{cert.level}</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-6">
                    <div>
                      <h4 className="font-semibold mb-2">Requirements</h4>
                      <ul className="space-y-1">
                        {cert.requirements.map((req, idx) => (
                          <li key={idx} className="text-sm text-gray-600 dark:text-gray-400 flex items-start">
                            <div className="w-1.5 h-1.5 bg-blue-500 rounded-full mr-2 mt-2 flex-shrink-0" />
                            {req}
                          </li>
                        ))}
                      </ul>
                    </div>
                    
                    <div>
                      <h4 className="font-semibold mb-2">Benefits</h4>
                      <ul className="space-y-1">
                        {cert.benefits.map((benefit, idx) => (
                          <li key={idx} className="text-sm text-gray-600 dark:text-gray-400 flex items-start">
                            <div className="w-1.5 h-1.5 bg-green-500 rounded-full mr-2 mt-2 flex-shrink-0" />
                            {benefit}
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

      {/* Technical Resources */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Technical Enablement
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Comprehensive resources to ensure successful enterprise implementations
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 md:grid-cols-2 lg:grid-cols-4">
            {[
              {
                icon: Code,
                title: 'Reference Architecture',
                description: 'Enterprise-grade deployment patterns and best practices'
              },
              {
                icon: BookOpen,
                title: 'Technical Documentation',
                description: 'Comprehensive API guides, SDKs, and integration manuals'
              },
              {
                icon: Building2,
                title: 'Sandbox Environment',
                description: 'Dedicated development environment for testing and demos'
              },
              {
                icon: Handshake,
                title: 'Professional Services',
                description: 'Expert consultation and implementation support'
              }
            ].map((resource) => {
              const Icon = resource.icon
              return (
                <Card key={resource.title} className="text-center hover:shadow-md transition-shadow">
                  <CardContent className="p-6">
                    <div className="flex justify-center mb-4">
                      <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-blue-100 dark:bg-blue-900">
                        <Icon className="h-6 w-6 text-blue-600 dark:text-blue-400" />
                      </div>
                    </div>
                    <h3 className="text-lg font-semibold mb-2">{resource.title}</h3>
                    <p className="text-sm text-gray-600 dark:text-gray-300">{resource.description}</p>
                  </CardContent>
                </Card>
              )
            })}
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 bg-gradient-to-r from-green-600 to-blue-600 text-white">
        <div className="mx-auto max-w-7xl px-6 lg:px-8 text-center">
          <h2 className="text-3xl font-bold mb-6">
            Ready to Become a Certified System Integrator?
          </h2>
          <p className="text-xl text-green-100 mb-8 max-w-2xl mx-auto">
            Join our elite SI program and start earning revenue on enterprise implementations today.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Button asChild size="lg" className="bg-white text-gray-900 hover:bg-gray-100">
              <Link href="/partner/integrator/register">
                Apply Now
                <ArrowRight className="ml-2 h-4 w-4" />
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg" className="border-white text-white hover:bg-white/10">
              <Link href="/contact/si-consultation">
                Schedule Consultation
              </Link>
            </Button>
          </div>
        </div>
      </section>
    </div>
  )
}