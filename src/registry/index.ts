import type { ComponentDescriptor, ComponentCategory } from './types'

// 自动发现所有描述文件 (glob pattern 相对于本文件路径)
const modules = import.meta.glob('./descriptors/*.descriptor.ts', { eager: true })
const allDescriptors = Object.values(modules).map(m => (m as any).default) as ComponentDescriptor[]

// ---- 内部索引 ----
const byType = new Map<string, ComponentDescriptor>()
const byCategory = new Map<ComponentCategory, ComponentDescriptor[]>()
const asyncViewImporters: Record<string, () => Promise<any>> = {}
const logicClassMap: Record<string, any> = {}
const offsetMap: Record<string, { x: number; y: number }> = {}
const imagePathMap: Record<string, string> = {}
const sizeMap: Record<string, { width?: number; height?: number }> = {}

for (const desc of allDescriptors) {
  byType.set(desc.type, desc)

  if (!byCategory.has(desc.category)) {
    byCategory.set(desc.category, [])
  }
  byCategory.get(desc.category)!.push(desc)

  asyncViewImporters[desc.type] = desc.viewComponent
  logicClassMap[desc.type] = desc.logicClass
  offsetMap[desc.type] = desc.offset
  imagePathMap[desc.type] = desc.imagePath
  if (desc.size) {
    sizeMap[desc.type] = desc.size
  }
}

// ---- 对外 API ----

/** 统一创建组件 (替换 useComponentType 的 switch) */
export function createComponent(
  id: number,
  type: string,
  position: [number, number] = [0, 0],
  name: string = '',
  projectId: number = -1,
  simulator: any = null
) {
  const desc = byType.get(type)
  if (!desc) throw new Error(`Unknown component type: ${type}`)
  return desc.createInstance(id, type, position, { name, projectId, simulator })
}

/** 获取指定分类的组件列表 (用于 ComponentList 面板) */
export function getComponentsByCategory(category: ComponentCategory) {
  return (byCategory.get(category) ?? []).map(d => ({
    type: d.type,
    name: d.name
  }))
}

export {
  byType,
  asyncViewImporters,
  logicClassMap,
  offsetMap,
  imagePathMap,
  sizeMap,
  allDescriptors,
}
