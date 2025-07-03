'use client'

import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { 
  Building2, 
  Users, 
  TrendingUp, 
  ArrowRight, 
  DollarSign,
  Award,
  Globe,
  Handshake
} from 'lucide-react'
import Link from 'next/link'
import { motion } from 'framer-motion'

const partnerTypes = [
  {
    id: 'reseller',
    title: 'Reseller Network',
    description: 'Drive revenue through our comprehensive reseller program with industry-leading commissions and support.',
    icon: Building2,
    color: 'blue',
    commission: '25%',
    features: [
      'Recurring revenue sharing',
      'Sales enablement tools', 
      'Marketing co-op funds',
      'Dedicated partner success manager',
      'Lead sharing program',
      'Demo environment access'
    ],
    metrics: {
      partners: '150+',
      revenue: '$2.4M',
      growth: '+180%'
    }
  },
  {
    id: 'system-integrator',
    title: 'System Integrator Hub',
    description: 'Join our elite SI program with enterprise deployment expertise and premium revenue opportunities.',
    icon: Users,
    color: 'green',
    commission: '20%',
    features: [
      'Implementation revenue share',
      'Technical certification programs',
      'Reference architecture access',
      'Developer sandbox environments', 
      'Professional services support',
      'Enterprise customer introductions'
    ],
    metrics: {
      partners: '45+',
      revenue: '$1.8M',
      growth: '+240%'
    }
  },
  {
    id: 'enterprise',
    title: 'Enterprise Direct',
    description: 'Self-service enterprise portal for Fortune 500 companies with instant provisioning and compliance.',
    icon: TrendingUp,
    color: 'purple',
    commission: 'Direct',
    features: [
      'Instant API provisioning',
      'Enterprise SLA guarantees',
      'Dedicated support channels',
      'Compliance reporting tools',
      'Custom contract terms',
      'Priority feature requests'
    ],
    metrics: {
      customers: '120+',
      revenue: '$4.2M',
      growth: '+320%'
    }
  }
]

export function PartnersOverview() {
  return (
    <section className="py-24 bg-white dark:bg-gray-900">
      <div className="mx-auto max-w-7xl px-6 lg:px-8">
        <div className="text-center mb-16">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.5 }}
          >
            <Badge variant="outline" className="mb-4">
              <Globe className="w-4 h-4 mr-2" />
              Global Partner Ecosystem
            </Badge>
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Three-Tier Partner Program
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
              Choose your partnership model and unlock revenue opportunities in the world's most 
              comprehensive banking-to-blockchain integration platform.
            </p>
          </motion.div>
        </div>

        <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
          {partnerTypes.map((partner, index) => {
            const Icon = partner.icon
            return (
              <motion.div
                key={partner.id}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ duration: 0.5, delay: index * 0.1 }}
              >
                <Card className="h-full hover:shadow-lg transition-shadow duration-300 border-2 hover:border-blue-200 dark:hover:border-blue-800">
                  <CardHeader className="pb-4">
                    <div className={`inline-flex h-12 w-12 items-center justify-center rounded-lg bg-${partner.color}-100 dark:bg-${partner.color}-900 mb-4`}>
                      <Icon className={`h-6 w-6 text-${partner.color}-600 dark:text-${partner.color}-400`} />
                    </div>
                    <CardTitle className="text-xl font-semibold">
                      {partner.title}
                    </CardTitle>
                    <CardDescription className="text-sm">
                      {partner.description}
                    </CardDescription>
                    <div className="flex items-center gap-4 pt-2">
                      <Badge variant="secondary" className="font-medium">
                        <DollarSign className="w-3 h-3 mr-1" />
                        {partner.commission} Commission
                      </Badge>
                      <Badge variant="outline">
                        <Award className="w-3 h-3 mr-1" />
                        Certified Program
                      </Badge>
                    </div>
                  </CardHeader>
                  
                  <CardContent className="pt-0">
                    <div className="space-y-4">
                      <div>
                        <h4 className="font-medium text-gray-900 dark:text-white mb-2">
                          Program Benefits
                        </h4>
                        <ul className="space-y-1">
                          {partner.features.map((feature, idx) => (
                            <li key={idx} className="text-sm text-gray-600 dark:text-gray-400 flex items-center">
                              <div className="w-1.5 h-1.5 bg-blue-500 rounded-full mr-2 flex-shrink-0" />
                              {feature}
                            </li>
                          ))}
                        </ul>
                      </div>
                      
                      <div className="grid grid-cols-3 gap-4 pt-4 border-t border-gray-200 dark:border-gray-700">
                        <div className="text-center">
                          <div className="text-lg font-semibold text-gray-900 dark:text-white">
                            {partner.metrics.partners || partner.metrics.customers}
                          </div>
                          <div className="text-xs text-gray-500 dark:text-gray-400">
                            {partner.metrics.partners ? 'Partners' : 'Customers'}
                          </div>
                        </div>
                        <div className="text-center">
                          <div className="text-lg font-semibold text-gray-900 dark:text-white">
                            {partner.metrics.revenue}
                          </div>
                          <div className="text-xs text-gray-500 dark:text-gray-400">
                            Monthly Revenue
                          </div>
                        </div>
                        <div className="text-center">
                          <div className="text-lg font-semibold text-green-600 dark:text-green-400">
                            {partner.metrics.growth}
                          </div>
                          <div className="text-xs text-gray-500 dark:text-gray-400">
                            YoY Growth
                          </div>
                        </div>
                      </div>
                      
                      <Button asChild className="w-full mt-6">
                        <Link href={`/partner/${partner.id}/register`}>
                          <Handshake className="w-4 h-4 mr-2" />
                          Join {partner.title}
                          <ArrowRight className="w-4 h-4 ml-2" />
                        </Link>
                      </Button>
                    </div>
                  </CardContent>
                </Card>
              </motion.div>
            )
          })}
        </div>

        <motion.div
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5, delay: 0.4 }}
          className="mt-16 text-center"
        >
          <div className="rounded-2xl bg-gradient-to-r from-blue-50 to-indigo-50 dark:from-blue-950 dark:to-indigo-950 p-8">
            <h3 className="text-2xl font-bold text-gray-900 dark:text-white mb-4">
              Ready to Scale Your Business?
            </h3>
            <p className="text-gray-600 dark:text-gray-300 mb-6 max-w-2xl mx-auto">
              Join over 315+ partners generating $8.4M+ in monthly recurring revenue through 
              our proven partner ecosystem. Start earning today.
            </p>
            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <Button asChild size="lg">
                <Link href="/partner/compare">
                  Compare Programs
                  <ArrowRight className="w-4 h-4 ml-2" />
                </Link>
              </Button>
              <Button asChild variant="outline" size="lg">
                <Link href="/contact">
                  Talk to Partner Success
                </Link>
              </Button>
            </div>
          </div>
        </motion.div>
      </div>
    </section>
  )
}