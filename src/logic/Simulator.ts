// 对外部提供连线的接口，封装好了连线的内部处理逻辑
// 用法：
// const simulator = EventDrivenSimulator.getInstance();
// simulator.connect(id1, pinIdx1, id2, pinIdx2);
// simulator.disconnect(id1, pinIdx1, id2, pinIdx2);
// 注意pinIdx：对于单个组件，输入端是从0开始计数，输出端接着输入端的计数继续
// 如：二输入与门，其输入引脚的pinIdx分别为0, 1，输出引脚为2


import { ConnectionManager, Conn } from './ConnectionManager';
import { useCircuitStore } from '@/store/CircuitStore';
import { BaseComponent } from './BaseComponent';

interface WorkItem {
  id: number;
  idx: number;
  value: number;
}

export class EventDrivenSimulator {
  private static instance: EventDrivenSimulator | null = null;
  private connectionManager: ConnectionManager;
  private circuitStore = useCircuitStore();
  private workQueue: WorkItem[] = [];
  private inQueue: Set<string> = new Set();
  private enableSimulator: Boolean = false; // 是否启用模拟器
  private pause: Boolean = false; // 是否暂停模拟器
  private constructor() {
    this.connectionManager = new ConnectionManager();
  }

  static getInstance(): EventDrivenSimulator {
    if (!EventDrivenSimulator.instance) {
      EventDrivenSimulator.instance = new EventDrivenSimulator();
    }
    return EventDrivenSimulator.instance;
  }
  // 启用模拟器
  enable() {
    this.enableSimulator = true;
  }
  // 禁用模拟器
  disable() {
    this.enableSimulator = false;
    this.workQueue = [];
    this.inQueue.clear();
  }
  // 暂停模拟器
  pauseSimulator() {
    this.pause = true;
  }
  // 恢复模拟器
  resumeSimulator() {
    this.pause = false;
    this.processQueue();
  }

  connect(id1: number, pinIndex1: number, id2: number, pinIndex2: number){
    const comp1 = this.circuitStore.getComponent(id1);
    const comp2 = this.circuitStore.getComponent(id2);
    if (!comp1 || !comp2) return false;

    let outputId: number, outputIdx: number, inputId: number, inputIdx: number;
    let legal = true;

    // 组件的引脚从0开始编号，前n位为输入
    // 当pinIndex1为输出引脚，则其对电线为输入端
    if (pinIndex1 >= comp1.getInputPinCount()) {
      // inputId 对应电线输入端，即其实际上为该元件的输出引脚
      inputId = id1;
      inputIdx = pinIndex1 - comp1.getInputPinCount();
      outputId = id2;
      outputIdx = pinIndex2;
      
      if (pinIndex2 >= comp2.getInputPinCount()) legal = false;
    } else {
      outputId = id1;
      outputIdx = pinIndex1;
      inputId = id2;
      inputIdx = pinIndex2 - comp2.getInputPinCount();
      if (pinIndex2 < comp2.getInputPinCount()) legal = false;
    }

    this.connectionManager.addConnection(inputId, inputIdx, outputId, outputIdx, legal);

    const outputVal = this.circuitStore.getComponent(inputId).getOutputs()[inputIdx];
    // 电线输出端的组件，其索引为idx的输入引脚的输入更改为了outputVal
    this.enqueue(outputId, outputIdx, outputVal);
    // 如果模拟器未启用或暂停，则不处理队列
    if (!this.enableSimulator || this.pause) return;
    this.processQueue();
  }

  disconnect(id1: number, pinIndex1: number, id2: number, pinIndex2: number) {
    const comp1 = this.circuitStore.getComponent(id1);
    const comp2 = this.circuitStore.getComponent(id2);
    if (!comp1 || !comp2) return;

    let outputId: number, outputIdx: number, inputId: number, inputIdx: number;

    // 组件的引脚从0开始编号，前n位为输入
    // 当pinIndex1为输出引脚，则其对电线为输入端
    if (pinIndex1 >= comp1.getInputPinCount()) {
      // inputId 对应电线输入端，即其实际上为该元件的输出引脚
      inputId = id1;
      inputIdx = pinIndex1 - comp1.getInputPinCount();
      outputId = id2;
      outputIdx = pinIndex2;
    } else {
      outputId = id1;
      outputIdx = pinIndex1;
      inputId = id2;
      inputIdx = pinIndex2 - comp2.getInputPinCount();
    }

    this.connectionManager.removeConnection(inputId, inputIdx);

    // 断开连接后，清除该电线输出端的输入引脚的输入
    const component = this.circuitStore.getComponent(outputId);
    if (!component) return false;
    const oldOutputs = [...component.getOutputs()];
    component.changeInput(outputIdx, -1); // 清除输入
    const newOutputs = component.getOutputs();
    // 如果输出没有变化，则不需要通知其他组件
    if(oldOutputs === newOutputs) return;
    // if (this.isEqualOutputs(oldOutputs, newOutputs)) return false;
    const pinMap = this.connectionManager.getOutputPinMap(outputId);
    if (!pinMap) return false;
    for (const [pinIdx, conn] of pinMap.entries()) {
      if (conn.legal) {
        const targetComponent = this.circuitStore.getComponent(conn.id);
        if (!targetComponent) continue;

        // 通知其他组件该组件的输出改变
        this.enqueue(conn.id, conn.idx, newOutputs[pinIdx]);
      }
    }
  }

  // 全局处理输入改变的情况
  // 参数：id :输入改变的组件的id
  //      idx: 该组件的改变输入的引脚
  //     value:  引脚将变为何值
  enqueue(id: number, idx: number, value: number): void {
    const key = `${id}_${idx}`;
    if (this.inQueue.has(key)) return;
    this.workQueue.push({ id, idx, value });
    this.inQueue.add(key);
  }

  processQueue(): void {
    while (this.workQueue.length > 0) {
      // 组件的id为id，它更改其索引为idx的引脚的输入为value
      const { id, idx, value } = this.workQueue.shift()!;
      this.inQueue.delete(`${id}_${idx}`);

      const component = this.circuitStore.getComponent(id);
      if (!component) continue;

      // 获取当前组件的新旧输出
      const oldOutputs = [...component.getOutputs()];
      const newOutputs = component.changeInput(idx, value);      

      if (!this.isEqualOutputs(oldOutputs, newOutputs)) {
        const pinMap = this.connectionManager.getOutputPinMap(id);
        if (!pinMap) continue;

        for (const [pinIdx, conn] of pinMap.entries()) {
          if (conn.legal) {
            const targetComponent = this.circuitStore.getComponent(conn.id);
            if (!targetComponent) continue;

            this.enqueue(conn.id, conn.idx, newOutputs[pinIdx]);
          }
        }
      }
    }
  }

  private isEqualOutputs(a: number[], b: number[]): boolean {
    if (a.length !== b.length) return false;
    return a.every((v, i) => v === b[i]);
  }

  getConnectionManager(): ConnectionManager {
    return this.connectionManager;
  }
}

