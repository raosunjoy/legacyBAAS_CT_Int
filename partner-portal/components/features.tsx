'use client'

import { Card, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { 
  Shield,
  Zap,
  Globe,
  DollarSign,
  Users,
  BarChart,
  Clock,
  HeadphonesIcon,
  Lock,
  Layers,
  Workflow,
  Target
} from 'lucide-react'
import { motion } from 'framer-motion'

const features = [
  {
    category: 'Platform Excellence',
    items: [
      {
        icon: Shield,
        title: 'Banking-Grade Security',
        description: 'SOC 2 Type II, PCI DSS compliance with end-to-end encryption and multi-factor authentication.',
        highlight: 'Enterprise Ready'
      },
      {
        icon: Zap,
        title: 'Sub-50ms Performance',
        description: 'Lightning-fast API responses with 99.99% uptime SLA and global edge deployment.',
        highlight: 'Performance'
      },
      {
        icon: Layers,
        title: '5-Language SDK Support',
        description: 'Native SDKs for JavaScript, Python, Java, .NET, and mobile platforms with complete type safety.',
        highlight: 'Developer First'
      }
    ]
  },
  {
    category: 'Partner Success',
    items: [
      {
        icon: DollarSign,
        title: 'Automated Commission Tracking',
        description: 'Real-time revenue calculations with automated payouts and detailed commission analytics.',
        highlight: 'Revenue Focus'
      },
      {
        icon: Users,
        title: 'Dedicated Partner Success',
        description: 'Personal account managers, quarterly business reviews, and 24/7 technical support.',
        highlight: 'White Glove'
      },
      {
        icon: BarChart,
        title: 'Advanced Analytics Dashboard',
        description: 'Comprehensive insights into sales performance, customer health, and revenue forecasting.',
        highlight: 'Data Driven'
      }
    ]
  },
  {
    category: 'Enterprise Integration',
    items: [
      {
        icon: Globe,
        title: 'Multi-Blockchain Support',
        description: 'XRP, Ethereum L2, Algorand, and Corda with seamless SWIFT message processing.',
        highlight: 'Universal'
      },
      {
        icon: Workflow,
        title: 'Legacy System Integration',
        description: 'Native TCS BaNCS integration with CBDC offline gateway and real-time settlement.',
        highlight: 'Banking Native'
      },
      {
        icon: Lock,
        title: 'Enterprise Compliance',
        description: 'Built-in AML/KYC workflows, audit trails, and regulatory reporting capabilities.',
        highlight: 'Compliant'
      }
    ]
  },
  {
    category: 'Market Leadership',
    items: [
      {
        icon: Clock,
        title: '5-Minute Integration',
        description: 'Fastest time-to-value in the industry with comprehensive documentation and examples.',
        highlight: 'Rapid Deploy'
      },
      {
        icon: HeadphonesIcon,
        title: 'Premium Support Tiers',
        description: 'Multiple support levels from community forums to dedicated technical architects.',
        highlight: 'Support Excellence'
      },
      {
        icon: Target,
        title: 'Proven Revenue Model',
        description: '$8.4M+ monthly partner revenue with 240% average YoY growth across all channels.',
        highlight: 'Results Proven'
      }
    ]
  }
]

export function Features() {
  return (
    <section className="py-24 bg-gray-50 dark:bg-gray-800">
      <div className="mx-auto max-w-7xl px-6 lg:px-8">
        <div className="text-center mb-16">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.5 }}
          >
            <Badge variant="outline" className="mb-4">
              <Layers className="w-4 h-4 mr-2" />
              Enterprise Platform Features
            </Badge>
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Built for Partner Success
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
              Everything you need to build, scale, and succeed with the most comprehensive 
              banking-to-blockchain integration platform in the market.
            </p>
          </motion.div>
        </div>

        <div className="space-y-16">
          {features.map((category, categoryIndex) => (
            <motion.div
              key={category.category}
              initial={{ opacity: 0, y: 20 }}
              whileInView={{ opacity: 1, y: 0 }}
              viewport={{ once: true }}
              transition={{ duration: 0.5, delay: categoryIndex * 0.1 }}
            >
              <div className="text-center mb-8">
                <h3 className="text-2xl font-semibold text-gray-900 dark:text-white">
                  {category.category}
                </h3>
              </div>
              
              <div className="grid grid-cols-1 gap-6 lg:grid-cols-3">
                {category.items.map((feature, index) => {
                  const Icon = feature.icon
                  return (
                    <motion.div
                      key={feature.title}
                      initial={{ opacity: 0, scale: 0.95 }}
                      whileInView={{ opacity: 1, scale: 1 }}
                      viewport={{ once: true }}
                      transition={{ duration: 0.5, delay: (categoryIndex * 0.1) + (index * 0.05) }}
                    >
                      <Card className="h-full hover:shadow-md transition-all duration-300 hover:-translate-y-1">
                        <CardContent className="p-6">
                          <div className="flex items-start space-x-4">
                            <div className="flex-shrink-0">
                              <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-blue-100 dark:bg-blue-900">
                                <Icon className="h-6 w-6 text-blue-600 dark:text-blue-400" />
                              </div>
                            </div>
                            <div className="flex-1 min-w-0">
                              <div className="flex items-center justify-between mb-2">
                                <h4 className="text-lg font-semibold text-gray-900 dark:text-white">
                                  {feature.title}
                                </h4>
                                <Badge variant="secondary" className="text-xs">
                                  {feature.highlight}
                                </Badge>
                              </div>
                              <p className="text-gray-600 dark:text-gray-300 text-sm leading-relaxed">
                                {feature.description}
                              </p>
                            </div>
                          </div>
                        </CardContent>
                      </Card>
                    </motion.div>
                  )
                })}
              </div>
            </motion.div>
          ))}
        </div>

        <motion.div
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5, delay: 0.6 }}
          className="mt-16"
        >
          <Card className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white border-0">
            <CardContent className="p-8 text-center">
              <h3 className="text-2xl font-bold mb-4">
                Platform Capabilities Summary
              </h3>
              <div className="grid grid-cols-2 md:grid-cols-4 gap-6 mt-8">
                <div>
                  <div className="text-3xl font-bold mb-1">5</div>
                  <div className="text-blue-100 text-sm">Language SDKs</div>
                </div>
                <div>
                  <div className="text-3xl font-bold mb-1">99.99%</div>
                  <div className="text-blue-100 text-sm">Uptime SLA</div>
                </div>
                <div>
                  <div className="text-3xl font-bold mb-1">&lt;50ms</div>
                  <div className="text-blue-100 text-sm">API Response</div>
                </div>
                <div>
                  <div className="text-3xl font-bold mb-1">24/7</div>
                  <div className="text-blue-100 text-sm">Support</div>
                </div>
              </div>
            </CardContent>
          </Card>
        </motion.div>
      </div>
    </section>
  )
}