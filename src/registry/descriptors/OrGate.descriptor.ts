import { OrGate } from '@/logic/components/OrGate'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'OR',
  name: '或门',
  category: 'gate',
  createInstance: (id, type, pos, ctx) =>
    new OrGate(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Gates/OrGate.vue'),
  logicClass: OrGate,
  imagePath: '/assets/或门.svg',
  offset: { x: -280, y: -280 },
  size: { width: 150, height: 150 },
}

export default descriptor
