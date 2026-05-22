import { Tunnel } from '@/logic/components/Tunnel'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'TUNNEL',
  name: '隧道',
  category: 'wiring',
  createInstance: (id, type, pos, ctx) =>
    new Tunnel(id, type, pos, ctx.name, ctx.simulator),
  viewComponent: () => import('@/components/Wiring/Tunnel.vue'),
  logicClass: Tunnel,
  imagePath: '/assets/Tunnel.svg',
  offset: { x: -253, y: -310 },
}

export default descriptor
