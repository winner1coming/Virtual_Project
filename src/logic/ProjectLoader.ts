// // 将项目加载封装为元件
// import { BaseComponent } from './BaseComponent';
// import { SubCircuitComponent } from './components/SubCircuitComponent';
// import { useCircuitStore } from '@/store/CircuitStore';
// import type { ProjectData } from './ProjectData';

// export function instantiateProjectAsComponent(project: ProjectData): SubCircuitComponent {
//   const circuitStore = useCircuitStore();
//   const idMap = new Map<number, number>();

//   // 1. 克隆并重映射 id
//   for (const [oldId, comp] of project.components) {
//     const newId = globalId++;
//     const clone = comp.cloneWithNewId(newId);  // 你需实现此方法
//     circuitStore.components.set(newId, clone);
//     idMap.set(oldId, newId);
//   }

//   // 2. 重建连接
//   for (const conn of project.connections) {
//     const newConn = {
//       fromId: idMap.get(conn.fromId)!,
//       fromPin: conn.fromPin,
//       toId: idMap.get(conn.toId)!,
//       toPin: conn.toPin
//     };
//     // 手动调用 ConnectionManager 来连线
//   }

//   // 3. 构建 SubCircuitComponent
//   const subComp = new SubCircuitComponent(globalId++, project.name, idMap, project.inputPins, project.outputPins);
//   circuitStore.components.set(subComp.id, subComp);
//   return subComp;
// }
