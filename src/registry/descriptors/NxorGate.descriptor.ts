import { NxorGate } from '@/logic/components/Nxor'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'NXOR',
  name: '异或非门',
  category: 'gate',
  createInstance: (id, type, pos, ctx) =>
    new NxorGate(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Gates/XnorGate.vue'),
  logicClass: NxorGate,
  imagePath: '/assets/异或非门.svg',
  offset: { x: -280, y: -280 },
}

export default descriptor
