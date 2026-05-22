import { ConstantInput } from '@/logic/components/ConstantInput'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'CONSTANT',
  name: '常量',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new ConstantInput(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/Constant.vue'),
  logicClass: ConstantInput,
  imagePath: '/assets/constant.svg',
  offset: { x: -60, y: -50 },
}

export default descriptor
