import { Light } from '@/logic/components/Light'
import type { ComponentDescriptor } from '../types'

const _temp = new Light(-1, 'LIGHT')

const descriptor: ComponentDescriptor = {
  type: 'LIGHT',
  name: '发光二极管',
  category: 'io',
  createInstance: (id, type, pos, _ctx) =>
    new Light(id, type, pos),
  viewComponent: () => import('@/components/Input_OutPut/LED.vue'),
  logicClass: Light,
  imagePath: '/assets/LED.svg',
  // Light 没有在构造器中设置 offset，此处使用 CanvasEditor 中原有的 SVG_OFFSET 值
  offset: { x: -145, y: -145 },
}

export default descriptor
