import { Combiner } from '@/logic/components/Combiner'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'COMBINER',
  name: '合并器',
  category: 'wiring',
  createInstance: (id, type, pos, ctx) =>
    new Combiner(id, type, pos, undefined, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/Combiner.vue'),
  logicClass: Combiner,
  imagePath: '/assets/combiner.svg',
  offset: { x: -150, y: -280 },
}

export default descriptor
