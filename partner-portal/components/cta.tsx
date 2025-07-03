'use client'

import { Button } from '@/components/ui/button'
import { Card, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { 
  ArrowRight,
  Rocket,
  DollarSign,
  Users,
  Clock,
  Shield,
  Zap,
  Building2
} from 'lucide-react'
import Link from 'next/link'
import { motion } from 'framer-motion'

const ctaOptions = [
  {
    id: 'reseller',
    title: 'Become a Reseller Partner',
    description: 'Start earning 25% recurring commission on all sales with comprehensive support and marketing resources.',
    icon: Building2,
    color: 'blue',
    highlights: [
      '25% recurring commission',
      'Sales enablement tools',
      'Marketing co-op funds',
      'Dedicated partner success'
    ],
    cta: 'Join Reseller Network',
    href: '/partner/reseller/register'
  },
  {
    id: 'integrator',
    title: 'Join SI Partner Program',
    description: 'Earn 20% revenue share on enterprise implementations with technical certification and professional services support.',
    icon: Users,
    color: 'green',
    highlights: [
      '20% implementation revenue',
      'Technical certification',
      'Enterprise introductions',
      'Reference architecture'
    ],
    cta: 'Become System Integrator',
    href: '/partner/integrator/register'
  },
  {
    id: 'enterprise',
    title: 'Enterprise Self-Service',
    description: 'Instant API provisioning for Fortune 500 companies with enterprise SLA and dedicated support channels.',
    icon: Rocket,
    color: 'purple',
    highlights: [
      'Instant provisioning',
      'Enterprise SLA',
      'Compliance reporting',
      'Priority support'
    ],
    cta: 'Start Enterprise Trial',
    href: '/enterprise/register'
  }
]

const urgencyFactors = [
  {
    icon: Clock,
    text: 'Limited spots available for Q1 2025 partner cohort'
  },
  {
    icon: DollarSign,
    text: 'Early partners earning $75K+ average MRR'
  },
  {
    icon: Zap,
    text: '5-minute integration, revenue in 30 days'
  },
  {
    icon: Shield,
    text: 'Banking-grade security and compliance ready'
  }
]

export function CTA() {
  return (
    <section className="py-24 bg-gradient-to-br from-blue-900 via-indigo-900 to-purple-900 text-white relative overflow-hidden">
      <div className="absolute inset-0 bg-grid-white/[0.05] bg-[size:60px_60px]" />
      <div className="relative mx-auto max-w-7xl px-6 lg:px-8">
        
        {/* Main CTA Header */}
        <div className="text-center mb-16">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.5 }}
          >
            <Badge variant="secondary" className="mb-6 bg-white/10 text-white border-white/20">
              <Rocket className="w-4 h-4 mr-2" />
              Ready to Transform Your Business?
            </Badge>
            <h2 className="text-4xl font-bold tracking-tight sm:text-5xl lg:text-6xl mb-6">
              Start Earning Today
            </h2>
            <p className="text-xl text-blue-100 max-w-3xl mx-auto mb-8">
              Join 315+ partners generating $8.4M+ monthly revenue through the world's most 
              comprehensive banking-to-blockchain integration platform.
            </p>
          </motion.div>
        </div>

        {/* Urgency Factors */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5, delay: 0.1 }}
          className="mb-16"
        >
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            {urgencyFactors.map((factor, index) => {
              const Icon = factor.icon
              return (
                <motion.div
                  key={index}
                  initial={{ opacity: 0, scale: 0.95 }}
                  whileInView={{ opacity: 1, scale: 1 }}
                  viewport={{ once: true }}
                  transition={{ duration: 0.5, delay: 0.1 + (index * 0.05) }}
                  className="flex items-center p-4 rounded-lg bg-white/10 backdrop-blur-sm border border-white/20"
                >
                  <Icon className="w-6 h-6 text-yellow-400 mr-3 flex-shrink-0" />
                  <span className="text-sm text-blue-100">{factor.text}</span>
                </motion.div>
              )
            })}
          </div>
        </motion.div>

        {/* CTA Options */}
        <div className="grid grid-cols-1 gap-8 lg:grid-cols-3 mb-16">
          {ctaOptions.map((option, index) => {
            const Icon = option.icon
            return (
              <motion.div
                key={option.id}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ duration: 0.5, delay: 0.2 + (index * 0.1) }}
              >
                <Card className="h-full bg-white/10 backdrop-blur-sm border-white/20 hover:bg-white/15 transition-all duration-300 hover:-translate-y-1">
                  <CardContent className="p-6">
                    <div className="flex items-center mb-4">
                      <div className={`flex h-12 w-12 items-center justify-center rounded-lg bg-${option.color}-500/20 mr-4`}>
                        <Icon className={`h-6 w-6 text-${option.color}-400`} />
                      </div>
                      <h3 className="text-xl font-semibold text-white">
                        {option.title}
                      </h3>
                    </div>
                    
                    <p className="text-blue-100 mb-6 leading-relaxed">
                      {option.description}
                    </p>
                    
                    <div className="space-y-2 mb-6">
                      {option.highlights.map((highlight, idx) => (
                        <div key={idx} className="flex items-center text-sm text-blue-200">
                          <div className="w-1.5 h-1.5 bg-green-400 rounded-full mr-2 flex-shrink-0" />
                          {highlight}
                        </div>
                      ))}
                    </div>
                    
                    <Button asChild className="w-full bg-white text-gray-900 hover:bg-gray-100">
                      <Link href={option.href}>
                        {option.cta}
                        <ArrowRight className="w-4 h-4 ml-2" />
                      </Link>
                    </Button>
                  </CardContent>
                </Card>
              </motion.div>
            )
          })}
        </div>

        {/* Final CTA and Contact */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5, delay: 0.5 }}
          className="text-center"
        >
          <Card className="bg-white/5 backdrop-blur-sm border-white/20">
            <CardContent className="p-8">
              <h3 className="text-2xl font-bold text-white mb-4">
                Not Sure Which Program Fits?
              </h3>
              <p className="text-blue-100 mb-8 max-w-2xl mx-auto">
                Schedule a consultation with our partner success team to explore the best 
                revenue opportunity for your business model and goals.
              </p>
              
              <div className="flex flex-col sm:flex-row gap-4 justify-center items-center">
                <Button asChild size="lg" className="bg-white text-gray-900 hover:bg-gray-100 h-12 px-8">
                  <Link href="/contact/partner-consultation">
                    Schedule Consultation
                    <ArrowRight className="w-4 h-4 ml-2" />
                  </Link>
                </Button>
                <Button asChild variant="outline" size="lg" className="border-white/30 text-white hover:bg-white/10 h-12 px-8">
                  <Link href="/partner/compare">
                    Compare All Programs
                  </Link>
                </Button>
              </div>
              
              <div className="mt-8 pt-8 border-t border-white/20">
                <div className="grid grid-cols-1 md:grid-cols-3 gap-6 text-center">
                  <div>
                    <div className="text-2xl font-bold text-white mb-1">5 min</div>
                    <div className="text-sm text-blue-200">Setup Time</div>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-white mb-1">30 days</div>
                    <div className="text-sm text-blue-200">First Revenue</div>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-white mb-1">24/7</div>
                    <div className="text-sm text-blue-200">Partner Support</div>
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>
        </motion.div>
      </div>
    </section>
  )
}