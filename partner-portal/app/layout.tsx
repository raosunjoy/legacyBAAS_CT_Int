import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import { ClerkProvider } from '@clerk/nextjs'
import { ThemeProvider } from '@/components/theme-provider'
import { Toaster } from '@/components/ui/toaster'
import { QueryProvider } from '@/components/query-provider'
import { Navigation } from '@/components/navigation'
import './globals.css'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'Legacy B2BaaS Partner Portal',
  description: 'Enterprise Partner Portal for Resellers, System Integrators, and Enterprise Clients',
  keywords: ['banking', 'blockchain', 'b2baas', 'partner', 'reseller', 'enterprise'],
  authors: [{ name: 'Legacy B2BaaS Platform' }],
  openGraph: {
    title: 'Legacy B2BaaS Partner Portal',
    description: 'Enterprise Partner Portal for Resellers, System Integrators, and Enterprise Clients',
    url: 'https://partners.legacybaas.com',
    siteName: 'Legacy B2BaaS Partner Portal',
    images: [
      {
        url: '/og-image.png',
        width: 1200,
        height: 630,
        alt: 'Legacy B2BaaS Partner Portal'
      }
    ],
    locale: 'en_US',
    type: 'website'
  },
  twitter: {
    card: 'summary_large_image',
    title: 'Legacy B2BaaS Partner Portal',
    description: 'Enterprise Partner Portal for Resellers, System Integrators, and Enterprise Clients',
    images: ['/og-image.png']
  },
  robots: {
    index: true,
    follow: true,
    googleBot: {
      index: true,
      follow: true,
      'max-video-preview': -1,
      'max-image-preview': 'large',
      'max-snippet': -1
    }
  },
  verification: {
    google: 'your-google-verification-code'
  }
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <ClerkProvider
      appearance={{
        elements: {
          footer: 'hidden'
        }
      }}
    >
      <html lang="en" suppressHydrationWarning>
        <body className={inter.className}>
          <ThemeProvider
            attribute="class"
            defaultTheme="system"
            enableSystem
            disableTransitionOnChange
          >
            <QueryProvider>
              <div className="min-h-screen bg-background">
                <Navigation />
                <main className="flex-1">
                  {children}
                </main>
                <Toaster />
              </div>
            </QueryProvider>
          </ThemeProvider>
        </body>
      </html>
    </ClerkProvider>
  )
}