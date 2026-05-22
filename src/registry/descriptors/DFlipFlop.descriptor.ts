import { DFlipFlop } from '@/logic/components/DFlipFlop'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'D_FLIP_FLOP',
  name: 'D触发器',
  category: 'memory',
  createInstance: (id, type, pos, ctx) =>
    new DFlipFlop(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/memory/DFlipFlop.vue'),
  logicClass: DFlipFlop,
  imagePath: '/assets/D触发器.svg',
  offset: { x: -250, y: -250 },
}

export default descriptor
