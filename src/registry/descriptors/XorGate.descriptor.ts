import { XorGate } from '@/logic/components/XorGate'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'XOR',
  name: '异或门',
  category: 'gate',
  createInstance: (id, type, pos, ctx) =>
    new XorGate(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Gates/XorGate.vue'),
  logicClass: XorGate,
  imagePath: '/assets/异或门.svg',
  offset: { x: -280, y: -280 },
}

export default descriptor
