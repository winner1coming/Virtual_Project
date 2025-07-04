import mitt from 'mitt';

// 定义事件类型（可选）
type Events = {
//   'component-dragstart': { type: string; name: string }; // 组件拖动事件
//   'component-placed': { x: number; y: number }; // 组件放置事件
//   'clear-canvas': void; // 清空画布事件
  'start-place-component': { type: string, projectId: number}; // 开始放置组件事件
  'updateComponentDirection': void;
  'updateComponentInputCount': void;
  'updateComponentBitWidth': void;
};

// 创建事件总线实例
const eventBus = mitt<Events>();

export default eventBus;