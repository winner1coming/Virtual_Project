import { BaseComponent } from '@/logic/BaseComponent'
import { EventDrivenSimulator } from '@/logic/Simulator'

export type ComponentCategory = 'gate' | 'io' | 'wiring' | 'memory'

export interface CreationContext {
  name: string
  projectId: number
  simulator: EventDrivenSimulator | null
}

export interface ComponentDescriptor {
  /** 唯一类型标识，如 'AND' */
  type: string
  /** 中文名，用于 UI 面板 */
  name: string
  /** 分类，用于组件面板分组 */
  category: ComponentCategory
  /** 创建逻辑实例的工厂函数 */
  createInstance: (
    id: number,
    type: string,
    position: [number, number],
    context: CreationContext
  ) => BaseComponent
  /** 懒加载视图组件 (Vite 静态分析路径) */
  viewComponent: () => Promise<any>
  /** 逻辑类引用 */
  logicClass: typeof BaseComponent
  /** 拖拽预览图路径 */
  imagePath: string
  /** SVG 偏移量 */
  offset: { x: number; y: number }
  /** 组件尺寸 (可选) */
  size?: { width?: number; height?: number }
}
