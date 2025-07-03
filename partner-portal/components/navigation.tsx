'use client'

import { useState } from 'react'
import Link from 'next/link'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import {
  NavigationMenu,
  NavigationMenuContent,
  NavigationMenuItem,
  NavigationMenuLink,
  NavigationMenuList,
  NavigationMenuTrigger,
} from '@/components/ui/navigation-menu'
import { Sheet, SheetContent, SheetTrigger } from '@/components/ui/sheet'
import { 
  Menu,
  Building2,
  Users,
  TrendingUp,
  FileText,
  HelpCircle,
  LogIn,
  UserPlus
} from 'lucide-react'
import { ThemeToggle } from '@/components/theme-toggle'

const partnerPrograms = [
  {
    title: 'Reseller Network',
    description: 'Earn 25% recurring commission on all sales',
    href: '/partner/reseller',
    icon: Building2
  },
  {
    title: 'System Integrator',
    description: '20% revenue share on implementations',
    href: '/partner/integrator',
    icon: Users
  },
  {
    title: 'Enterprise Direct',
    description: 'Self-service portal for Fortune 500',
    href: '/enterprise',
    icon: TrendingUp
  }
]

const resources = [
  {
    title: 'Documentation',
    description: 'API docs, SDKs, and integration guides',
    href: '/docs'
  },
  {
    title: 'Partner Portal',
    description: 'Access your dashboard and analytics',
    href: '/portal'
  },
  {
    title: 'Success Stories',
    description: 'Case studies and testimonials',
    href: '/success-stories'
  },
  {
    title: 'Support Center',
    description: 'Get help and technical support',
    href: '/support'
  }
]

export function Navigation() {
  const [isOpen, setIsOpen] = useState(false)

  return (
    <header className="sticky top-0 z-50 w-full border-b bg-background/95 backdrop-blur supports-[backdrop-filter]:bg-background/60">
      <nav className="mx-auto flex h-16 max-w-7xl items-center justify-between px-6 lg:px-8">
        {/* Logo */}
        <div className="flex items-center">
          <Link href="/" className="flex items-center space-x-2">
            <div className="flex h-8 w-8 items-center justify-center rounded-lg bg-blue-600">
              <Building2 className="h-5 w-5 text-white" />
            </div>
            <span className="hidden font-bold sm:inline-block">
              Legacy B2BaaS
            </span>
            <Badge variant="secondary" className="hidden sm:inline-flex">
              Partners
            </Badge>
          </Link>
        </div>

        {/* Desktop Navigation */}
        <div className="hidden md:flex md:items-center md:space-x-6">
          <NavigationMenu>
            <NavigationMenuList>
              <NavigationMenuItem>
                <NavigationMenuTrigger>Partner Programs</NavigationMenuTrigger>
                <NavigationMenuContent>
                  <ul className="grid w-[400px] gap-3 p-4 md:w-[500px] md:grid-cols-1 lg:w-[600px]">
                    {partnerPrograms.map((program) => {
                      const Icon = program.icon
                      return (
                        <li key={program.title}>
                          <NavigationMenuLink asChild>
                            <Link
                              href={program.href}
                              className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-accent hover:text-accent-foreground focus:bg-accent focus:text-accent-foreground"
                            >
                              <div className="flex items-center space-x-2">
                                <Icon className="h-4 w-4" />
                                <div className="text-sm font-medium leading-none">
                                  {program.title}
                                </div>
                              </div>
                              <p className="line-clamp-2 text-sm leading-snug text-muted-foreground">
                                {program.description}
                              </p>
                            </Link>
                          </NavigationMenuLink>
                        </li>
                      )
                    })}
                  </ul>
                </NavigationMenuContent>
              </NavigationMenuItem>

              <NavigationMenuItem>
                <NavigationMenuTrigger>Resources</NavigationMenuTrigger>
                <NavigationMenuContent>
                  <ul className="grid w-[400px] gap-3 p-4 md:w-[500px] md:grid-cols-2 lg:w-[600px]">
                    {resources.map((resource) => (
                      <li key={resource.title}>
                        <NavigationMenuLink asChild>
                          <Link
                            href={resource.href}
                            className="block select-none space-y-1 rounded-md p-3 leading-none no-underline outline-none transition-colors hover:bg-accent hover:text-accent-foreground focus:bg-accent focus:text-accent-foreground"
                          >
                            <div className="text-sm font-medium leading-none">
                              {resource.title}
                            </div>
                            <p className="line-clamp-2 text-sm leading-snug text-muted-foreground">
                              {resource.description}
                            </p>
                          </Link>
                        </NavigationMenuLink>
                      </li>
                    ))}
                  </ul>
                </NavigationMenuContent>
              </NavigationMenuItem>

              <NavigationMenuItem>
                <Link href="/pricing" legacyBehavior passHref>
                  <NavigationMenuLink className="group inline-flex h-9 w-max items-center justify-center rounded-md bg-background px-4 py-2 text-sm font-medium transition-colors hover:bg-accent hover:text-accent-foreground focus:bg-accent focus:text-accent-foreground focus:outline-none disabled:pointer-events-none disabled:opacity-50 data-[active]:bg-accent/50 data-[state=open]:bg-accent/50">
                    Pricing
                  </NavigationMenuLink>
                </Link>
              </NavigationMenuItem>

              <NavigationMenuItem>
                <Link href="/contact" legacyBehavior passHref>
                  <NavigationMenuLink className="group inline-flex h-9 w-max items-center justify-center rounded-md bg-background px-4 py-2 text-sm font-medium transition-colors hover:bg-accent hover:text-accent-foreground focus:bg-accent focus:text-accent-foreground focus:outline-none disabled:pointer-events-none disabled:opacity-50 data-[active]:bg-accent/50 data-[state=open]:bg-accent/50">
                    Contact
                  </NavigationMenuLink>
                </Link>
              </NavigationMenuItem>
            </NavigationMenuList>
          </NavigationMenu>
        </div>

        {/* Right side actions */}
        <div className="flex items-center space-x-4">
          <ThemeToggle />
          
          {/* Desktop Auth Buttons */}
          <div className="hidden md:flex md:items-center md:space-x-2">
            <Button variant="ghost" size="sm" asChild>
              <Link href="/auth/signin">
                <LogIn className="h-4 w-4 mr-2" />
                Sign In
              </Link>
            </Button>
            <Button size="sm" asChild>
              <Link href="/partner/register">
                <UserPlus className="h-4 w-4 mr-2" />
                Get Started
              </Link>
            </Button>
          </div>

          {/* Mobile Menu */}
          <Sheet open={isOpen} onOpenChange={setIsOpen}>
            <SheetTrigger asChild className="md:hidden">
              <Button variant="ghost" size="sm">
                <Menu className="h-5 w-5" />
                <span className="sr-only">Toggle menu</span>
              </Button>
            </SheetTrigger>
            <SheetContent side="right" className="w-[300px] sm:w-[400px]">
              <div className="flex flex-col space-y-4 mt-6">
                <div>
                  <h4 className="font-medium mb-3">Partner Programs</h4>
                  <div className="space-y-2">
                    {partnerPrograms.map((program) => {
                      const Icon = program.icon
                      return (
                        <Link
                          key={program.title}
                          href={program.href}
                          className="flex items-center space-x-2 rounded-md p-2 hover:bg-accent"
                          onClick={() => setIsOpen(false)}
                        >
                          <Icon className="h-4 w-4" />
                          <span className="text-sm">{program.title}</span>
                        </Link>
                      )
                    })}
                  </div>
                </div>

                <div>
                  <h4 className="font-medium mb-3">Resources</h4>
                  <div className="space-y-2">
                    {resources.map((resource) => (
                      <Link
                        key={resource.title}
                        href={resource.href}
                        className="block rounded-md p-2 text-sm hover:bg-accent"
                        onClick={() => setIsOpen(false)}
                      >
                        {resource.title}
                      </Link>
                    ))}
                  </div>
                </div>

                <div className="border-t pt-4">
                  <div className="space-y-2">
                    <Link
                      href="/pricing"
                      className="block rounded-md p-2 text-sm hover:bg-accent"
                      onClick={() => setIsOpen(false)}
                    >
                      Pricing
                    </Link>
                    <Link
                      href="/contact"
                      className="block rounded-md p-2 text-sm hover:bg-accent"
                      onClick={() => setIsOpen(false)}
                    >
                      Contact
                    </Link>
                  </div>
                </div>

                <div className="border-t pt-4 space-y-2">
                  <Button variant="outline" className="w-full justify-start" asChild>
                    <Link href="/auth/signin" onClick={() => setIsOpen(false)}>
                      <LogIn className="h-4 w-4 mr-2" />
                      Sign In
                    </Link>
                  </Button>
                  <Button className="w-full justify-start" asChild>
                    <Link href="/partner/register" onClick={() => setIsOpen(false)}>
                      <UserPlus className="h-4 w-4 mr-2" />
                      Get Started
                    </Link>
                  </Button>
                </div>
              </div>
            </SheetContent>
          </Sheet>
        </div>
      </nav>
    </header>
  )
}