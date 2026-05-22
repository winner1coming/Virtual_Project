import { Ground } from '@/logic/components/Ground'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'GROUND',
  name: '接地',
  category: 'io',
  createInstance: (id, type, pos, _ctx) =>
    new Ground(id, type, pos),
  viewComponent: () => import('@/components/Wiring/Ground.vue'),
  logicClass: Ground,
  imagePath: '/assets/接地.svg',
  offset: { x: 0, y: 0 },
}

export default descriptor
