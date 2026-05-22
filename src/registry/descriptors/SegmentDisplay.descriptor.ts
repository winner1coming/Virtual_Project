import { SegmentDisplay } from '@/logic/components/SegmentDisplay'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'SEGMENT_DISPLAY',
  name: '七段数码器',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new SegmentDisplay(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Input_OutPut/SegmentDisplay.vue'),
  logicClass: SegmentDisplay,
  imagePath: '/assets/7段数码管.svg',
  offset: { x: -250, y: -200 },
}

export default descriptor
