import { NorGate } from '@/logic/components/NorGate'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'NOR',
  name: '或非门',
  category: 'gate',
  createInstance: (id, type, pos, ctx) =>
    new NorGate(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Gates/NorGate.vue'),
  logicClass: NorGate,
  imagePath: '/assets/或非门.svg',
  offset: { x: -280, y: -280 },
}

export default descriptor
