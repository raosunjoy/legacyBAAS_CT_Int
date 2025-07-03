'use client'

import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { ArrowRight, Shield, Award, Building2, Users, Globe } from 'lucide-react'
import Link from 'next/link'
import { motion } from 'framer-motion'

export function Hero() {
  return (
    <section className="relative overflow-hidden bg-gradient-to-br from-slate-50 via-blue-50 to-indigo-100 dark:from-slate-950 dark:via-blue-950 dark:to-indigo-950">
      <div className="absolute inset-0 bg-grid-black/[0.02] bg-[size:60px_60px]" />
      <div className="relative mx-auto max-w-7xl px-6 py-24 sm:py-32 lg:px-8">
        <div className="text-center">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5 }}
          >
            <Badge variant="secondary" className="mb-6 text-sm font-medium">
              <Shield className="w-4 h-4 mr-2" />
              SOC 2 Type II Certified • Banking Grade Security
            </Badge>
          </motion.div>
          
          <motion.h1
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.1 }}
            className="text-4xl font-bold tracking-tight text-gray-900 dark:text-white sm:text-6xl lg:text-7xl"
          >
            Banking-to-Blockchain
            <span className="block bg-gradient-to-r from-blue-600 to-indigo-600 bg-clip-text text-transparent">
              Infrastructure
            </span>
          </motion.h1>
          
          <motion.p
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.2 }}
            className="mx-auto mt-6 max-w-3xl text-lg leading-8 text-gray-600 dark:text-gray-300"
          >
            Trusted by Fortune 500 banks for seamless SWIFT-to-blockchain integration. 
            Enterprise-grade security, 99.99% uptime, and native TCS BaNCS connectivity 
            with real-time settlement across XRP, Ethereum L2, Algorand, and Corda networks.
          </motion.p>
          
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.3 }}
            className="mt-10 flex flex-col items-center justify-center gap-4 sm:flex-row"
          >
            <Button asChild size="lg" className="h-12 px-8 bg-blue-600 hover:bg-blue-700">
              <Link href="/enterprise/demo">
                Schedule Executive Demo
                <ArrowRight className="ml-2 h-4 w-4" />
              </Link>
            </Button>
            <Button asChild variant="outline" size="lg" className="h-12 px-8">
              <Link href="/developer/sandbox">
                Try Developer Sandbox
              </Link>
            </Button>
          </motion.div>
          
          {/* Trust Indicators */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.4 }}
            className="mt-16"
          >
            <p className="text-sm text-gray-500 dark:text-gray-400 mb-8">
              Trusted by leading financial institutions worldwide
            </p>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-8 items-center opacity-60">
              <div className="text-center">
                <div className="text-xs font-medium text-gray-400">National Bank of Americas</div>
              </div>
              <div className="text-center">
                <div className="text-xs font-medium text-gray-400">Global Financial Group</div>
              </div>
              <div className="text-center">
                <div className="text-xs font-medium text-gray-400">Enterprise Banking Corp</div>
              </div>
              <div className="text-center">
                <div className="text-xs font-medium text-gray-400">International Trade Bank</div>
              </div>
            </div>
          </motion.div>

          {/* Key Metrics */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.5 }}
            className="mt-16 grid grid-cols-1 gap-8 sm:grid-cols-3"
          >
            <div className="flex flex-col items-center text-center">
              <div className="flex h-16 w-16 items-center justify-center rounded-full bg-green-100 dark:bg-green-900">
                <Award className="h-8 w-8 text-green-600 dark:text-green-400" />
              </div>
              <h3 className="mt-4 text-lg font-semibold text-gray-900 dark:text-white">
                99.99% Uptime
              </h3>
              <p className="mt-2 text-sm text-gray-600 dark:text-gray-400">
                Enterprise SLA with multi-region failover
              </p>
            </div>
            
            <div className="flex flex-col items-center text-center">
              <div className="flex h-16 w-16 items-center justify-center rounded-full bg-blue-100 dark:bg-blue-900">
                <Building2 className="h-8 w-8 text-blue-600 dark:text-blue-400" />
              </div>
              <h3 className="mt-4 text-lg font-semibold text-gray-900 dark:text-white">
                $2.8B+ Processed
              </h3>
              <p className="mt-2 text-sm text-gray-600 dark:text-gray-400">
                Annual transaction volume through our platform
              </p>
            </div>
            
            <div className="flex flex-col items-center text-center">
              <div className="flex h-16 w-16 items-center justify-center rounded-full bg-purple-100 dark:bg-purple-900">
                <Globe className="h-8 w-8 text-purple-600 dark:text-purple-400" />
              </div>
              <h3 className="mt-4 text-lg font-semibold text-gray-900 dark:text-white">
                120+ Banks
              </h3>
              <p className="mt-2 text-sm text-gray-600 dark:text-gray-400">
                Fortune 500 financial institutions served
              </p>
            </div>
          </motion.div>
        </div>
      </div>
    </section>
  )
}