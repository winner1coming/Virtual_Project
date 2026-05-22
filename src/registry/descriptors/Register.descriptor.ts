import { Register } from '@/logic/components/Register'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'REGISTER',
  name: '寄存器',
  category: 'memory',
  createInstance: (id, type, pos, ctx) =>
    new Register(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/memory/Register.vue'),
  logicClass: Register,
  imagePath: '/assets/寄存器.svg',
  offset: { x: -250, y: -250 },
}

export default descriptor
