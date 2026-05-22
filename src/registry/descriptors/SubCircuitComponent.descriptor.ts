import { SubCircuitComponent } from '@/logic/components/SubCircuitComponent'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'SUB_CIRCUIT',
  name: '子电路',
  category: 'memory',
  createInstance: (id, type, pos, ctx) =>
    new SubCircuitComponent(id, type, pos, ctx.name, ctx.projectId),
  viewComponent: () => import('@/components/CustomizeComponent.vue'),
  logicClass: SubCircuitComponent,
  imagePath: '/assets/子组件.svg',
  offset: { x: -280, y: -280 },
}

export default descriptor
