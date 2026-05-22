import { HexDisplay } from '@/logic/components/HexDisplay'
import type { ComponentDescriptor } from '../types'

const descriptor: ComponentDescriptor = {
  type: 'HEX_DISPLAY',
  name: '十六进制数码管',
  category: 'io',
  createInstance: (id, type, pos, ctx) =>
    new HexDisplay(id, type, pos, ctx.simulator),
  viewComponent: () => import('@/components/Input_OutPut/HexDigitalDisplay.vue'),
  logicClass: HexDisplay,
  imagePath: '/assets/Hex数码管.svg',
  offset: { x: -250, y: -200 },
}

export default descriptor
