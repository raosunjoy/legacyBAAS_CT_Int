'use client'

import { Card, CardContent } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Avatar, AvatarFallback, AvatarImage } from '@/components/ui/avatar'
import { Star, Quote, Building2, Users, TrendingUp } from 'lucide-react'
import { motion } from 'framer-motion'

const testimonials = [
  {
    category: 'Reseller Network',
    icon: Building2,
    color: 'blue',
    testimonials: [
      {
        quote: "Legacy B2BaaS transformed our business. We went from $50K to $400K MRR in 8 months through their partner program. The commission structure and support are unmatched.",
        author: "Sarah Chen",
        title: "CEO & Founder",
        company: "FinTech Solutions Inc.",
        avatar: "/avatars/sarah-chen.jpg",
        rating: 5,
        revenue: "$400K MRR",
        growth: "+680%"
      },
      {
        quote: "The partner portal makes everything seamless. Real-time commission tracking, automated payouts, and dedicated support. Our team loves the platform.",
        author: "Marcus Rodriguez",
        title: "Partner Director",
        company: "Global Banking Partners",
        avatar: "/avatars/marcus-rodriguez.jpg",
        rating: 5,
        revenue: "$280K MRR",
        growth: "+450%"
      }
    ]
  },
  {
    category: 'System Integrators',
    icon: Users,
    color: 'green',
    testimonials: [
      {
        quote: "As a certified SI, we've completed 40+ enterprise implementations. The technical documentation and sandbox environments are exceptional. Revenue share model is industry-leading.",
        author: "Dr. James Patel",
        title: "CTO",
        company: "Enterprise Blockchain Solutions",
        avatar: "/avatars/james-patel.jpg",
        rating: 5,
        revenue: "$1.2M Projects",
        growth: "+320%"
      },
      {
        quote: "The reference architecture and professional services support helped us win our largest banking client. $2M implementation contract in our first quarter as an SI partner.",
        author: "Lisa Wang",
        title: "Managing Director",
        company: "Digital Banking Architects",
        avatar: "/avatars/lisa-wang.jpg",
        rating: 5,
        revenue: "$2M Contract",
        growth: "+240%"
      }
    ]
  },
  {
    category: 'Enterprise Direct',
    icon: TrendingUp,
    color: 'purple',
    testimonials: [
      {
        quote: "Self-service provisioning cut our procurement time from 6 months to 5 minutes. The platform handles 50M+ daily transactions flawlessly. Best investment we've made.",
        author: "Michael Thompson",
        title: "Chief Innovation Officer",
        company: "National Bank of Americas",
        avatar: "/avatars/michael-thompson.jpg",
        rating: 5,
        revenue: "50M+ Daily Txns",
        growth: "100% Uptime"
      },
      {
        quote: "Legacy B2BaaS enabled our digital transformation. Seamless SWIFT integration, real-time blockchain settlement, and banking-grade security. Game changer for our industry.",
        author: "Rachel Kim",
        title: "Head of Digital Banking",
        company: "Fortune Global Bank",
        avatar: "/avatars/rachel-kim.jpg",
        rating: 5,
        revenue: "$500M+ Settled",
        growth: "99.99% SLA"
      }
    ]
  }
]

const overallStats = {
  averageRating: 4.9,
  totalReviews: 247,
  partnerSatisfaction: 94,
  nps: 78
}

export function Testimonials() {
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
              <Quote className="w-4 h-4 mr-2" />
              Partner Success Stories
            </Badge>
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Trusted by Industry Leaders
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300 max-w-3xl mx-auto">
              Hear from our partners who are building successful businesses and driving innovation 
              with our banking-to-blockchain integration platform.
            </p>
          </motion.div>
        </div>

        {/* Overall Stats */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5, delay: 0.1 }}
          className="mb-16"
        >
          <Card className="bg-white dark:bg-gray-900 border-2">
            <CardContent className="p-8">
              <div className="grid grid-cols-1 md:grid-cols-4 gap-8 text-center">
                <div>
                  <div className="flex justify-center mb-2">
                    {[...Array(5)].map((_, i) => (
                      <Star 
                        key={i} 
                        className="w-5 h-5 text-yellow-400 fill-current" 
                      />
                    ))}
                  </div>
                  <div className="text-2xl font-bold text-gray-900 dark:text-white">
                    {overallStats.averageRating}
                  </div>
                  <div className="text-sm text-gray-500 dark:text-gray-400">
                    Average Rating
                  </div>
                </div>
                <div>
                  <div className="text-2xl font-bold text-gray-900 dark:text-white">
                    {overallStats.totalReviews}+
                  </div>
                  <div className="text-sm text-gray-500 dark:text-gray-400">
                    Partner Reviews
                  </div>
                </div>
                <div>
                  <div className="text-2xl font-bold text-green-600 dark:text-green-400">
                    {overallStats.partnerSatisfaction}%
                  </div>
                  <div className="text-sm text-gray-500 dark:text-gray-400">
                    Partner Satisfaction
                  </div>
                </div>
                <div>
                  <div className="text-2xl font-bold text-blue-600 dark:text-blue-400">
                    {overallStats.nps}
                  </div>
                  <div className="text-sm text-gray-500 dark:text-gray-400">
                    Net Promoter Score
                  </div>
                </div>
              </div>
            </CardContent>
          </Card>
        </motion.div>

        {/* Testimonials by Category */}
        <div className="space-y-16">
          {testimonials.map((category, categoryIndex) => {
            const Icon = category.icon
            return (
              <motion.div
                key={category.category}
                initial={{ opacity: 0, y: 20 }}
                whileInView={{ opacity: 1, y: 0 }}
                viewport={{ once: true }}
                transition={{ duration: 0.5, delay: categoryIndex * 0.1 }}
              >
                <div className="text-center mb-8">
                  <div className="flex justify-center items-center mb-4">
                    <div className={`flex h-12 w-12 items-center justify-center rounded-lg bg-${category.color}-100 dark:bg-${category.color}-900 mr-3`}>
                      <Icon className={`h-6 w-6 text-${category.color}-600 dark:text-${category.color}-400`} />
                    </div>
                    <h3 className="text-2xl font-semibold text-gray-900 dark:text-white">
                      {category.category}
                    </h3>
                  </div>
                </div>

                <div className="grid grid-cols-1 gap-8 lg:grid-cols-2">
                  {category.testimonials.map((testimonial, index) => (
                    <motion.div
                      key={testimonial.author}
                      initial={{ opacity: 0, scale: 0.95 }}
                      whileInView={{ opacity: 1, scale: 1 }}
                      viewport={{ once: true }}
                      transition={{ duration: 0.5, delay: (categoryIndex * 0.1) + (index * 0.05) }}
                    >
                      <Card className="h-full hover:shadow-lg transition-shadow duration-300">
                        <CardContent className="p-6">
                          <div className="flex items-center mb-4">
                            <div className="flex">
                              {[...Array(testimonial.rating)].map((_, i) => (
                                <Star 
                                  key={i} 
                                  className="w-4 h-4 text-yellow-400 fill-current" 
                                />
                              ))}
                            </div>
                            <div className="ml-auto flex gap-2">
                              <Badge variant="secondary" className="text-xs">
                                {testimonial.revenue}
                              </Badge>
                              <Badge variant="outline" className="text-xs text-green-600 dark:text-green-400">
                                {testimonial.growth}
                              </Badge>
                            </div>
                          </div>

                          <blockquote className="text-gray-700 dark:text-gray-300 mb-6 italic">
                            "{testimonial.quote}"
                          </blockquote>

                          <div className="flex items-center">
                            <Avatar className="h-12 w-12 mr-4">
                              <AvatarImage src={testimonial.avatar} alt={testimonial.author} />
                              <AvatarFallback>
                                {testimonial.author.split(' ').map(n => n[0]).join('')}
                              </AvatarFallback>
                            </Avatar>
                            <div>
                              <div className="font-semibold text-gray-900 dark:text-white">
                                {testimonial.author}
                              </div>
                              <div className="text-sm text-gray-500 dark:text-gray-400">
                                {testimonial.title}
                              </div>
                              <div className="text-sm font-medium text-blue-600 dark:text-blue-400">
                                {testimonial.company}
                              </div>
                            </div>
                          </div>
                        </CardContent>
                      </Card>
                    </motion.div>
                  ))}
                </div>
              </motion.div>
            )
          })}
        </div>

        {/* Success Metrics Summary */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          whileInView={{ opacity: 1, y: 0 }}
          viewport={{ once: true }}
          transition={{ duration: 0.5, delay: 0.4 }}
          className="mt-16"
        >
          <Card className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white border-0">
            <CardContent className="p-8 text-center">
              <h3 className="text-2xl font-bold mb-6">
                Join 315+ Successful Partners
              </h3>
              <p className="text-blue-100 mb-8 max-w-2xl mx-auto">
                Our partners consistently achieve exceptional growth and success. 
                Be part of the ecosystem that's transforming banking and blockchain integration.
              </p>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                <div>
                  <div className="text-3xl font-bold mb-2">240%</div>
                  <div className="text-blue-100">Average YoY Growth</div>
                </div>
                <div>
                  <div className="text-3xl font-bold mb-2">$75K</div>
                  <div className="text-blue-100">Average Partner MRR</div>
                </div>
                <div>
                  <div className="text-3xl font-bold mb-2">8 Months</div>
                  <div className="text-blue-100">Average ROI Timeline</div>
                </div>
              </div>
            </CardContent>
          </Card>
        </motion.div>
      </div>
    </section>
  )
}