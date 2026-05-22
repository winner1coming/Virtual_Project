import { Splitter } from '@/logic/components/Splitter'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'SPLITTER',
  name: '分离器',
  category: 'wiring',
  createInstance: (id, type, pos, ctx) =>
    new Splitter(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/Splitter.vue'),
  logicClass: Splitter,
  imagePath: '/assets/splitter.svg',
  offset: { x: -150, y: -280 },
}

export default descriptor
