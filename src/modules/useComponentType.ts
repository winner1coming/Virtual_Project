import { BaseComponent } from '@/logic/BaseComponent';
import { createComponent } from '@/registry';
import { EventDrivenSimulator } from '@/logic/Simulator';

/**
 * 根据类型统一创建对象。
 * 已委托给 registry 自动发现组件，此处保留为兼容旧调用的薄委托层。
 */
export function createComponentByType(
  id: number,
  type: string,
  position: [number, number] = [0, 0],
  name: string = '',
  projectId: number = -1,
  simulator: null | EventDrivenSimulator = null
): BaseComponent {
  return createComponent(id, type, position, name, projectId, simulator);
}
