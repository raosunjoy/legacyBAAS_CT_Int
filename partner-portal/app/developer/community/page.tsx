'use client'

import { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { 
  Github,
  Star,
  GitFork,
  MessageCircle,
  Users,
  Book,
  Download,
  Code,
  Zap,
  Trophy,
  Calendar,
  ArrowRight,
  ExternalLink,
  Heart,
  FileText,
  Terminal,
  Cpu
} from 'lucide-react'
import Link from 'next/link'

const swiftParserStats = {
  stars: 2847,
  forks: 421,
  contributors: 89,
  downloads: '45K/month',
  version: 'v2.4.1',
  license: 'MIT'
}

const openSourceProjects = [
  {
    name: 'SwiftParser',
    description: 'High-performance SWIFT message parser and validator for modern banking applications',
    stars: 2847,
    language: 'TypeScript',
    category: 'Core Library',
    features: ['MT103/202/700 parsing', 'Real-time validation', 'TypeScript support', 'Zero dependencies'],
    github: 'https://github.com/legacybaas/swift-parser',
    npm: 'npm install @legacybaas/swift-parser'
  },
  {
    name: 'SWIFT Message Generator',
    description: 'Generate valid SWIFT messages for testing and development environments',
    stars: 1205,
    language: 'Python',
    category: 'Developer Tools',
    features: ['Message templates', 'Random data generation', 'CLI interface', 'Batch processing'],
    github: 'https://github.com/legacybaas/swift-generator',
    npm: 'pip install swift-message-generator'
  },
  {
    name: 'ISO 20022 Converter',
    description: 'Convert between SWIFT MT and ISO 20022 MX message formats seamlessly',
    stars: 892,
    language: 'Java',
    category: 'Utilities',
    features: ['MT to MX conversion', 'Validation engine', 'Spring Boot starter', 'Maven plugin'],
    github: 'https://github.com/legacybaas/iso20022-converter',
    npm: 'mvn dependency:add -Dartifact=com.legacybaas:iso20022-converter'
  },
  {
    name: 'Banking SDK Examples',
    description: 'Complete integration examples and tutorials for all supported programming languages',
    stars: 1634,
    language: 'Multi',
    category: 'Examples',
    features: ['5 language examples', 'Best practices', 'Error handling', 'Production patterns'],
    github: 'https://github.com/legacybaas/sdk-examples',
    npm: 'git clone https://github.com/legacybaas/sdk-examples'
  }
]

const communityEvents = [
  {
    type: 'Hackathon',
    title: 'FinTech Innovation Challenge 2025',
    date: 'March 15-17, 2025',
    location: 'Virtual + Singapore',
    prizes: '$50K in prizes',
    description: 'Build the future of banking with blockchain technology',
    status: 'Registration Open'
  },
  {
    type: 'Webinar',
    title: 'SWIFT MT to ISO 20022 Migration Best Practices',
    date: 'February 28, 2025',
    location: 'Online',
    speaker: 'Sarah Chen, Principal Architect',
    description: 'Learn proven strategies for modernizing your SWIFT infrastructure',
    status: 'Upcoming'
  },
  {
    type: 'Workshop',
    title: 'Building High-Performance SWIFT Parsers',
    date: 'February 14, 2025',
    location: 'London + Virtual',
    duration: '4 hours',
    description: 'Hands-on workshop with SwiftParser maintainers',
    status: 'Few Spots Left'
  }
]

const forumTopics = [
  {
    category: 'SwiftParser',
    title: 'Performance optimization for high-volume MT103 processing',
    author: 'dev_alex_2024',
    replies: 23,
    views: 1847,
    lastActivity: '2 hours ago',
    badges: ['Hot', 'Solved']
  },
  {
    category: 'Integration',
    title: 'Best practices for error handling in production environments',
    author: 'banking_engineer',
    replies: 15,
    views: 923,
    lastActivity: '5 hours ago',
    badges: ['Pinned']
  },
  {
    category: 'ISO 20022',
    title: 'Converting MT700 to MX messages - field mapping issues',
    author: 'trade_finance_dev',
    replies: 8,
    views: 445,
    lastActivity: '1 day ago',
    badges: []
  },
  {
    category: 'General',
    title: 'Blockchain settlement timing optimization techniques',
    author: 'crypto_banker',
    replies: 31,
    views: 2156,
    lastActivity: '3 days ago',
    badges: ['Popular']
  }
]

const contributors = [
  {
    name: 'Sarah Chen',
    role: 'Principal Architect',
    avatar: '/avatars/sarah-chen.jpg',
    contributions: 847,
    expertise: ['SWIFT Protocols', 'Performance Optimization'],
    github: 'sarahchen-dev'
  },
  {
    name: 'Marcus Rodriguez',
    role: 'Senior Developer',
    avatar: '/avatars/marcus-rodriguez.jpg',
    contributions: 623,
    expertise: ['ISO 20022', 'Message Validation'],
    github: 'marcusrod'
  },
  {
    name: 'Dr. James Patel',
    role: 'Banking Systems Expert',
    avatar: '/avatars/james-patel.jpg',
    contributions: 445,
    expertise: ['Core Banking', 'Compliance'],
    github: 'drjamespatel'
  },
  {
    name: 'Lisa Wang',
    role: 'DevOps Engineer',
    avatar: '/avatars/lisa-wang.jpg',
    contributions: 356,
    expertise: ['Infrastructure', 'Monitoring'],
    github: 'lisawang-ops'
  }
]

export default function CommunityPage() {
  const [activeTab, setActiveTab] = useState('overview')

  return (
    <div className="min-h-screen bg-background">
      {/* Hero Section */}
      <section className="relative overflow-hidden bg-gradient-to-br from-purple-50 via-blue-50 to-green-100 dark:from-purple-950 dark:via-blue-950 dark:to-green-950">
        <div className="absolute inset-0 bg-grid-black/[0.02] bg-[size:60px_60px]" />
        <div className="relative mx-auto max-w-7xl px-6 py-24 sm:py-32 lg:px-8">
          <div className="text-center">
            <Badge variant="secondary" className="mb-6 text-sm font-medium">
              <Users className="w-4 h-4 mr-2" />
              Open Source Developer Community
            </Badge>
            
            <h1 className="text-4xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-6xl lg:text-7xl">
              Banking Developer
              <span className="block bg-gradient-to-r from-purple-600 to-blue-600 bg-clip-text text-transparent">
                Community
              </span>
            </h1>
            
            <p className="mx-auto mt-6 max-w-3xl text-lg leading-8 text-gray-600 dark:text-gray-300">
              Join thousands of banking developers building the future of financial infrastructure. 
              Contribute to open-source tools, share knowledge, and collaborate on cutting-edge 
              banking-to-blockchain solutions.
            </p>
            
            <div className="mt-10 flex flex-col items-center justify-center gap-4 sm:flex-row">
              <Button asChild size="lg" className="h-12 px-8 bg-purple-600 hover:bg-purple-700">
                <Link href="https://github.com/legacybaas" target="_blank">
                  <Github className="mr-2 h-4 w-4" />
                  View on GitHub
                  <ExternalLink className="ml-2 h-4 w-4" />
                </Link>
              </Button>
              <Button asChild variant="outline" size="lg" className="h-12 px-8">
                <Link href="/developer/community/forum">
                  <MessageCircle className="mr-2 h-4 w-4" />
                  Join Forum
                </Link>
              </Button>
            </div>

            {/* Community Stats */}
            <div className="mt-16 grid grid-cols-2 md:grid-cols-4 gap-8 text-center">
              <div>
                <div className="text-3xl font-bold text-purple-600">{swiftParserStats.contributors}</div>
                <div className="text-sm text-gray-500">Active Contributors</div>
              </div>
              <div>
                <div className="text-3xl font-bold text-blue-600">{swiftParserStats.stars.toLocaleString()}</div>
                <div className="text-sm text-gray-500">GitHub Stars</div>
              </div>
              <div>
                <div className="text-3xl font-bold text-green-600">{swiftParserStats.downloads}</div>
                <div className="text-sm text-gray-500">Monthly Downloads</div>
              </div>
              <div>
                <div className="text-3xl font-bold text-orange-600">12</div>
                <div className="text-sm text-gray-500">Open Source Projects</div>
              </div>
            </div>
          </div>
        </div>
      </section>

      {/* SwiftParser Spotlight */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              SwiftParser - Our Flagship Open Source Project
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              High-performance SWIFT message parsing library trusted by banks worldwide
            </p>
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-12 items-center">
            <div>
              <div className="flex items-center mb-6">
                <div className="flex h-12 w-12 items-center justify-center rounded-lg bg-purple-100 dark:bg-purple-900 mr-4">
                  <Code className="h-6 w-6 text-purple-600 dark:text-purple-400" />
                </div>
                <div>
                  <h3 className="text-2xl font-bold text-gray-900 dark:text-white">SwiftParser</h3>
                  <div className="flex items-center gap-4 mt-1">
                    <Badge variant="outline">{swiftParserStats.version}</Badge>
                    <Badge variant="outline">{swiftParserStats.license}</Badge>
                    <div className="flex items-center text-sm text-gray-500">
                      <Star className="h-4 w-4 mr-1 fill-current text-yellow-500" />
                      {swiftParserStats.stars.toLocaleString()}
                    </div>
                  </div>
                </div>
              </div>

              <p className="text-gray-600 dark:text-gray-300 mb-6">
                The most advanced open-source SWIFT message parser, designed for enterprise banking applications. 
                Zero dependencies, TypeScript support, and optimized for high-throughput processing.
              </p>

              <div className="space-y-4 mb-8">
                <div className="flex items-center text-sm">
                  <Zap className="h-4 w-4 text-green-500 mr-2" />
                  <span>Parses 100K+ messages per second</span>
                </div>
                <div className="flex items-center text-sm">
                  <Shield className="h-4 w-4 text-blue-500 mr-2" />
                  <span>Banking-grade validation and security</span>
                </div>
                <div className="flex items-center text-sm">
                  <Code className="h-4 w-4 text-purple-500 mr-2" />
                  <span>Full TypeScript support with type safety</span>
                </div>
                <div className="flex items-center text-sm">
                  <Heart className="h-4 w-4 text-red-500 mr-2" />
                  <span>Used by 120+ banks and financial institutions</span>
                </div>
              </div>

              <div className="flex gap-3">
                <Button asChild>
                  <Link href="https://github.com/legacybaas/swift-parser" target="_blank">
                    <Github className="mr-2 h-4 w-4" />
                    View Source
                  </Link>
                </Button>
                <Button variant="outline" asChild>
                  <Link href="/developer/docs/swift-parser">
                    <Book className="mr-2 h-4 w-4" />
                    Documentation
                  </Link>
                </Button>
              </div>
            </div>

            <div className="bg-gray-900 text-gray-100 rounded-lg p-6 overflow-x-auto">
              <div className="flex items-center justify-between mb-4">
                <span className="text-sm text-gray-400">quick-start.ts</span>
                <Button variant="ghost" size="sm" className="text-gray-400 hover:text-white">
                  <Download className="h-4 w-4" />
                </Button>
              </div>
              <pre className="text-sm">
                <code>{`import { SwiftParser } from '@legacybaas/swift-parser';

// Initialize parser with validation
const parser = new SwiftParser({
  validate: true,
  strict: true
});

// Parse MT103 message
const mt103Message = \`{1:F01BANKUS33AXXX1234567890}
{2:I103BANKGB2LXXXXN}
{4:
:20:TXN123456789
:32A:240315USD10000,00
:50K:ACME CORPORATION
NEW YORK NY US
:59:GLOBAL TRADE LIMITED
LONDON GB
-}\`;

try {
  const parsed = await parser.parse(mt103Message);
  
  console.log('Message Type:', parsed.messageType); // MT103
  console.log('Amount:', parsed.fields.amount);     // 10000.00
  console.log('Currency:', parsed.fields.currency); // USD
  console.log('Valid:', parsed.isValid);            // true
} catch (error) {
  console.error('Parsing failed:', error.message);
}`}</code>
              </pre>
            </div>
          </div>
        </div>
      </section>

      {/* Open Source Projects */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Open Source Banking Tools
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              A comprehensive suite of tools for modern banking development
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 md:grid-cols-2">
            {openSourceProjects.map((project) => (
              <Card key={project.name} className="bg-white dark:bg-gray-900 hover:shadow-lg transition-shadow duration-300">
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <CardTitle className="text-lg">{project.name}</CardTitle>
                    <div className="flex items-center gap-2">
                      <Badge variant="outline" className="text-xs">{project.category}</Badge>
                      <div className="flex items-center text-sm text-gray-500">
                        <Star className="h-3 w-3 mr-1 fill-current text-yellow-500" />
                        {project.stars.toLocaleString()}
                      </div>
                    </div>
                  </div>
                  <CardDescription>{project.description}</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    <div className="flex flex-wrap gap-2">
                      {project.features.map((feature) => (
                        <Badge key={feature} variant="secondary" className="text-xs">
                          {feature}
                        </Badge>
                      ))}
                    </div>
                    
                    <div className="bg-gray-100 dark:bg-gray-800 rounded p-3 text-sm font-mono">
                      {project.npm}
                    </div>
                    
                    <div className="flex gap-2">
                      <Button asChild variant="outline" size="sm">
                        <Link href={project.github} target="_blank">
                          <Github className="mr-2 h-3 w-3" />
                          GitHub
                        </Link>
                      </Button>
                      <Badge variant="outline" className="flex items-center">
                        <span className="w-2 h-2 rounded-full bg-blue-500 mr-2"></span>
                        {project.language}
                      </Badge>
                    </div>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* Community Events */}
      <section className="py-24 bg-white dark:bg-gray-900">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Community Events & Learning
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Connect, learn, and innovate with fellow banking developers
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
            {communityEvents.map((event) => (
              <Card key={event.title} className="hover:shadow-lg transition-shadow duration-300">
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <Badge variant="outline">{event.type}</Badge>
                    <Badge 
                      variant={event.status === 'Registration Open' ? 'default' : 'secondary'}
                      className={event.status === 'Few Spots Left' ? 'bg-orange-100 text-orange-800' : ''}
                    >
                      {event.status}
                    </Badge>
                  </div>
                  <CardTitle className="text-lg">{event.title}</CardTitle>
                  <CardDescription>{event.description}</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    <div className="flex items-center text-sm text-gray-600 dark:text-gray-300">
                      <Calendar className="h-4 w-4 mr-2" />
                      {event.date}
                    </div>
                    <div className="flex items-center text-sm text-gray-600 dark:text-gray-300">
                      <Users className="h-4 w-4 mr-2" />
                      {event.location}
                    </div>
                    {event.prizes && (
                      <div className="flex items-center text-sm text-green-600 dark:text-green-400">
                        <Trophy className="h-4 w-4 mr-2" />
                        {event.prizes}
                      </div>
                    )}
                    {event.speaker && (
                      <div className="flex items-center text-sm text-blue-600 dark:text-blue-400">
                        <Users className="h-4 w-4 mr-2" />
                        {event.speaker}
                      </div>
                    )}
                  </div>
                  <Button className="w-full mt-6">
                    {event.type === 'Hackathon' ? 'Register Now' : 'Learn More'}
                    <ArrowRight className="ml-2 h-4 w-4" />
                  </Button>
                </CardContent>
              </Card>
            ))}
          </div>
        </div>
      </section>

      {/* Forum & Discussions */}
      <section className="py-24 bg-gray-50 dark:bg-gray-800">
        <div className="mx-auto max-w-7xl px-6 lg:px-8">
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-4xl">
              Developer Forum
            </h2>
            <p className="mt-4 text-lg text-gray-600 dark:text-gray-300">
              Get help, share knowledge, and discuss banking technology
            </p>
          </div>

          <div className="grid grid-cols-1 gap-8 lg:grid-cols-3">
            <div className="lg:col-span-2">
              <Card className="bg-white dark:bg-gray-900">
                <CardHeader>
                  <CardTitle>Recent Discussions</CardTitle>
                  <CardDescription>Latest topics from the developer community</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    {forumTopics.map((topic, index) => (
                      <div key={index} className="border-b border-gray-200 dark:border-gray-700 pb-4 last:border-0">
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <div className="flex items-center gap-2 mb-2">
                              <Badge variant="outline" className="text-xs">{topic.category}</Badge>
                              {topic.badges.map((badge) => (
                                <Badge 
                                  key={badge} 
                                  variant={badge === 'Hot' ? 'destructive' : badge === 'Solved' ? 'default' : 'secondary'}
                                  className="text-xs"
                                >
                                  {badge}
                                </Badge>
                              ))}
                            </div>
                            <h4 className="font-medium text-gray-900 dark:text-white mb-1">
                              {topic.title}
                            </h4>
                            <div className="flex items-center gap-4 text-xs text-gray-500">
                              <span>by {topic.author}</span>
                              <span>{topic.replies} replies</span>
                              <span>{topic.views} views</span>
                              <span>{topic.lastActivity}</span>
                            </div>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                  <Button asChild className="w-full mt-6">
                    <Link href="/developer/community/forum">
                      <MessageCircle className="mr-2 h-4 w-4" />
                      View All Discussions
                    </Link>
                  </Button>
                </CardContent>
              </Card>
            </div>

            <div>
              <Card className="bg-white dark:bg-gray-900">
                <CardHeader>
                  <CardTitle>Top Contributors</CardTitle>
                  <CardDescription>Community champions and maintainers</CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    {contributors.map((contributor) => (
                      <div key={contributor.name} className="flex items-center space-x-3">
                        <div className="w-10 h-10 rounded-full bg-blue-100 dark:bg-blue-900 flex items-center justify-center">
                          <span className="text-sm font-medium text-blue-600 dark:text-blue-400">
                            {contributor.name.split(' ').map(n => n[0]).join('')}
                          </span>
                        </div>
                        <div className="flex-1 min-w-0">
                          <div className="text-sm font-medium text-gray-900 dark:text-white">
                            {contributor.name}
                          </div>
                          <div className="text-xs text-gray-500">
                            {contributor.contributions} contributions
                          </div>
                          <div className="flex flex-wrap gap-1 mt-1">
                            {contributor.expertise.map((skill) => (
                              <Badge key={skill} variant="secondary" className="text-xs">
                                {skill}
                              </Badge>
                            ))}
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>
            </div>
          </div>
        </div>
      </section>

      {/* CTA Section */}
      <section className="py-24 bg-gradient-to-r from-purple-600 to-blue-600 text-white">
        <div className="mx-auto max-w-7xl px-6 lg:px-8 text-center">
          <h2 className="text-3xl font-bold mb-6">
            Ready to Contribute?
          </h2>
          <p className="text-xl text-purple-100 mb-8 max-w-2xl mx-auto">
            Join our community of banking developers building the future of financial infrastructure. 
            Contribute code, share knowledge, and help shape the industry.
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <Button asChild size="lg" className="bg-white text-gray-900 hover:bg-gray-100">
              <Link href="https://github.com/legacybaas" target="_blank">
                <Github className="mr-2 h-4 w-4" />
                Start Contributing
                <ExternalLink className="ml-2 h-4 w-4" />
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg" className="border-white text-white hover:bg-white/10">
              <Link href="/developer/community/forum">
                <MessageCircle className="mr-2 h-4 w-4" />
                Join Forum
              </Link>
            </Button>
          </div>
          
          <div className="mt-12 grid grid-cols-1 md:grid-cols-3 gap-8 text-center">
            <div>
              <div className="text-2xl font-bold mb-2">Open Source</div>
              <div className="text-purple-200">MIT Licensed</div>
            </div>
            <div>
              <div className="text-2xl font-bold mb-2">Global Community</div>
              <div className="text-purple-200">89 Contributors</div>
            </div>
            <div>
              <div className="text-2xl font-bold mb-2">Active Development</div>
              <div className="text-purple-200">Daily Commits</div>
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}