import { Hero } from '@/components/hero'
import { Features } from '@/components/features'
import { PartnersOverview } from '@/components/partners-overview'
import { Stats } from '@/components/stats'
import { Testimonials } from '@/components/testimonials'
import { CTA } from '@/components/cta'

export default function HomePage() {
  return (
    <div className="flex flex-col">
      <Hero />
      <PartnersOverview />
      <Features />
      <Stats />
      <Testimonials />
      <CTA />
    </div>
  )
}