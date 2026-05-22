import { InputPin } from '@/logic/components/InputPin'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'INPUT',
  name: '输入引脚',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new InputPin(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/InputPin.vue'),
  logicClass: InputPin,
  imagePath: '/assets/inputPin.svg',
  offset: { x: 0, y: 0 },
}

export default descriptor
