import { Clock } from '@/logic/components/Clock'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'CLOCK',
  name: '时钟',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new Clock(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/Clock.vue'),
  logicClass: Clock,
  imagePath: '/assets/时钟.svg',
  offset: { x: -120, y: -120 },
}

export default descriptor
