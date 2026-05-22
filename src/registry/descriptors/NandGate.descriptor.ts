import { NandGate } from '@/logic/components/NandGate'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'NAND',
  name: '与非门',
  category: 'gate',
  createInstance: (id, type, pos, ctx) =>
    new NandGate(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Gates/NandGate.vue'),
  logicClass: NandGate,
  imagePath: '/assets/与非门.svg',
  offset: { x: -280, y: -280 },
}

export default descriptor
