import { NotGate } from '@/logic/components/NotGate'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'NOT',
  name: '非门',
  category: 'gate',
  createInstance: (id, type, pos, _ctx) =>
    new NotGate(id, type, pos),
  viewComponent: () => import('@/components/Gates/NotGate.vue'),
  logicClass: NotGate,
  imagePath: '/assets/非门.svg',
  offset: { x: -280, y: -280 },
  size: { width: 60, height: 60 },
}

export default descriptor
