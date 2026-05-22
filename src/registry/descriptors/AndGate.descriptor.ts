import { AndGate } from '@/logic/components/AndGate'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'AND',
  name: '与门',
  category: 'gate',
  createInstance: (id, type, pos, ctx) =>
    new AndGate(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Gates/AndGate.vue'),
  logicClass: AndGate,
  imagePath: '/assets/AND.svg',
  offset: { x: -280, y: -280 },
  size: { width: (AndGate as any).width, height: (AndGate as any).height },
}

export default descriptor
