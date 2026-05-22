import { OutputPin } from '@/logic/components/OutputPin'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'OUTPUT',
  name: '输出引脚',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new OutputPin(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/OutputPin.vue'),
  logicClass: OutputPin,
  imagePath: '/assets/outputpin.svg',
  offset: { x: 0, y: 0 },
}

export default descriptor
