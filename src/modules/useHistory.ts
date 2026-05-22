import { reactive } from 'vue';
import { useCircuitStore } from '@/store/CircuitStore';
export function useHistory(components: any[]) {
  const circuitStore = useCircuitStore();
  const undoStack = circuitStore.undoStack;
  const redoStack = circuitStore.redoStack;

  const MAX_HISTORY = 50;

  // 深拷贝辅助函数
  function deepClone(state: any[]) {
    return JSON.parse(JSON.stringify(state));
  }

  // 保存快照
  function saveSnapshot() {
    // 限制历史记录数量
    if (undoStack.length >= MAX_HISTORY) {
      undoStack.splice(0, undoStack.length-MAX_HISTORY);
    }
    
    undoStack.push(deepClone(components));
    redoStack.length = 0; // 清空重做栈
  }

  // 应用快照
  function applySnapshot(snapshot: any[]) {
    components.splice(0, components.length, ...deepClone(snapshot));
  }

  // 修正后的撤销
  function undo() {
    if (undoStack.length === 0) return; // 修正：检查栈是否为空
    
    // 保存当前状态到重做栈
    redoStack.push(deepClone(components));
    
    // 获取并应用上一个状态
    const prevState = undoStack.pop();
    applySnapshot(prevState);
  }

  // 重做
  function redo() {
    if (redoStack.length === 0) return;
    
    // 保存当前状态到撤销栈
    undoStack.push(deepClone(components));
    
    // 获取并应用下一个状态
    const nextState = redoStack.pop();
    applySnapshot(nextState);
  }

  // 清空画布（支持撤销）
  function clearAll() {
    saveSnapshot(); // 保存当前状态
    components.splice(0, components.length); // 清空组件
  }

  return {
    saveSnapshot,
    undo,
    redo,
    clearAll,
  };
}
// export function useHistory(components: any[]) {
//   const circuitStore = useCircuitStore();
//   const undoStack = circuitStore.undoStack;  // 历史操作栈
//   const redoStack = circuitStore.redoStack;  // 重做栈

//   // 保存当前状态为快照
//   function saveSnapshot() {
//     undoStack.push(JSON.parse(JSON.stringify(components))); // 保存快照到撤销栈
//     redoStack.length = 0; // 清空重做栈
//   }

//   // 应用快照
//   function applySnapshot(snapshot: any[]) {
//     components.splice(0, components.length, ...JSON.parse(JSON.stringify(snapshot)));
//   }

//   // 撤销
//   function undo() {
//     // 保存当前状态到重做栈
//     if (undoStack.length > 0) return
//     // 保存当前状态到重做栈
//     redoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
//     // 获取上一个状态
//     const prevState = undoStack.pop()
//     applySnapshot(prevState)// 应用上一个状态
//   }

//   // 重做
//   function redo() {
//     if (redoStack.length === 0) return
//     // 保存当前状态到重做栈
//     undoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
//     // 获取下一个状态
//     const nextState = redoStack.pop()
//     applySnapshot(nextState)// 应用下一个状态
//   }

//   // 原函数：保存历史（可删除）
//   function saveHistory() {
//     if (undoStack.length >= 50) undoStack.shift()// 限制撤销栈的长度
//     undoStack.push(JSON.parse(JSON.stringify(components)))// 保存快照到撤销栈
//     redoStack.length = 0
//   }

//   // 清空画布：支持撤销
//   function clearAll() {
//     saveSnapshot()// 保存快照
//     //TODO
//     components.splice(0) // 清空组件
//     saveHistory()
//   }

//   // 原函数：清空组件（可删除）
//   function clearComponents() {
//     if (components.length > 0) {
//       saveHistory()
//       components.splice(0)
//     }
//   }

//   return {
//     saveSnapshot,
//     undo,
//     redo,
//     saveHistory,
//     clearAll,
//     clearComponents,
//   };
// }