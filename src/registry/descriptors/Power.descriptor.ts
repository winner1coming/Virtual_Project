import { Power } from '@/logic/components/Power'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'POWER',
  name: '电源',
  category: 'io',
  createInstance: (id, type, pos, _ctx) =>
    new Power(id, type, pos),
  viewComponent: () => import('@/components/Wiring/Power.vue'),
  logicClass: Power,
  imagePath: '/assets/电源.svg',
  offset: { x: 0, y: 0 },
}

export default descriptor
