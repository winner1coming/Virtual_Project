import mitt from 'mitt';

// 定义事件类型（可选）
type Events = {
  'start-place-component': { type: string, projectId: number}; // 开始放置组件事件
  'updateComponentDirection': void;// 指定元件旋转方向
  'updateComponentBitWidth': void;
  'updatePinPosition':{id: number};
  'updateInputCount': {id: number};
  'freshProject': void; // 刷新项目事件
};

// 创建事件总线实例
const eventBus = mitt<Events>();
export default eventBus;