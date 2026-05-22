import { Button } from '@/logic/components/Button'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'BUTTON',
  name: '按钮',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new Button(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Input_OutPut/Button.vue'),
  logicClass: Button,
  imagePath: '/assets/按钮.svg',
  offset: { x: -110, y: -100 },
}

export default descriptor
