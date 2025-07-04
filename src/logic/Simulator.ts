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
import { Tunnel } from './components/Tunnel';

interface WorkItem {
  id: number;
  idx: number;
  value: number;
}

export class EventDrivenSimulator {
  private static instance: EventDrivenSimulator | null = null;
  private connectionManager: ConnectionManager;
  private connManagerMap: Map<number, ConnectionManager> = new Map(); // 用于存储每个项目的连接管理器
  private circuitStore = useCircuitStore();
  private workQueue: WorkItem[] = [];
  private inQueue: Set<string> = new Set();
  private enableSimulator: Boolean = true; // 是否启用模拟器
  private pause: Boolean = false; // 是否暂停模拟器

  // 隧道维护
  public tunnelNameMap: Map<String, number[]> = new Map(); // 存储隧道名字与id
  public InputTunnelMap: Map<String, number[]> = new Map(); // 记录接受输入的隧道
  private projectTunnel: Map<number, [Map<String, number[]>, Map<String, number[]>]> = new Map(); // 存储每个项目的隧道信息
  private constructor() {
    this.connectionManager = new ConnectionManager();
    this.connManagerMap.set(0, this.connectionManager); // 默认项目的连接管理器
    this.projectTunnel.set(0, [this.tunnelNameMap, this.InputTunnelMap]); // 默认项目的隧道信息
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

    // 当不是隧道时
    if(comp1.type !== 'TUNNEL' && comp2.type !== 'TUNNEL') {
      // 组件的引脚从0开始编号，前n位为输入
      // 当pinIndex1为输出引脚，则其对电线为输入端
      if (pinIndex1 >= comp1.getInputPinCount()) {
        // inputId 对应电线输入端，即其实际上为该元件的输出引脚
        inputId = id1;
        inputIdx = pinIndex1 - comp1.getInputPinCount();
        outputId = id2;
        
        if (pinIndex2 >= comp2.getInputPinCount()) {
          //legal = false;
          outputIdx = pinIndex2 - comp2.getInputPinCount();
        }else{
          outputIdx = pinIndex2;
        }
      } else {
        outputId = id1;
        outputIdx = pinIndex1;
        inputId = id2;
        if (pinIndex2 < comp2.getInputPinCount()) {
          //legal = false;
          inputIdx = pinIndex2;
        }else{
          inputIdx = pinIndex2 - comp2.getInputPinCount();
        }
      }
    } else{
      const tunnelComp = comp1.type === 'TUNNEL' ? comp1 : comp2;
      const tunnelPinIndex = comp1.type === 'TUNNEL' ? pinIndex1 : pinIndex2;
      const otherComp = comp1.type === 'TUNNEL' ? comp2 : comp1;
      const otherPinIndex = comp1.type === 'TUNNEL' ? pinIndex2 : pinIndex1;
      // 假如另一组件是输出电流的（则另一组件是输入端）  todo 没有考虑两个都是隧道的情况
      if(otherPinIndex >= otherComp.getInputPinCount()) {
        // inputId 对应电线输入端，即其实际上为该元件的输出引脚
        outputId = tunnelComp.id;
        outputIdx = tunnelPinIndex;
        inputId = otherComp.id;
        inputIdx = otherPinIndex - otherComp.getInputPinCount();
        this.InputTunnelMap.get(tunnelComp.name)?.push(tunnelComp.id); // 记录隧道输入
      }else {
        inputId = tunnelComp.id;
        inputIdx = tunnelPinIndex;
        outputId = otherComp.id;
        outputIdx = otherPinIndex;
      }
    }
    // // 判断位宽是否合法   
    // if(comp1.bitWidth !== comp2.bitWidth) {
    //   legal = false;
    // }
    this.connectionManager.addConnection(inputId, inputIdx, outputId, outputIdx, legal);

    const outputVal = this.circuitStore.getComponent(inputId).getOutputs()[inputIdx];

    // 电线输出端的组件，其索引为idx的输入引脚的输入更改为了outputVal
    this.enqueue(outputId, outputIdx, outputVal);
    // 如果模拟器未启用或暂停，则不处理队列
    if (!this.enableSimulator || this.pause) return;
    this.processQueue();
  }

  // todo 注意考虑多条连线连到同一引脚的情况
  disconnect(id1: number, pinIndex1: number, id2: number, pinIndex2: number) {
    const comp1 = this.circuitStore.getComponent(id1);
    const comp2 = this.circuitStore.getComponent(id2);
    if (!comp1 || !comp2) return;

    let outputId: number, outputIdx: number, inputId: number, inputIdx: number;

    // 当不是隧道时
    if(comp1.type !== 'TUNNEL' && comp2.type !== 'TUNNEL') {
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
    } else{
      const tunnelComp = comp1.type === 'TUNNEL' ? comp1 : comp2;
      const tunnelPinIndex = comp1.type === 'TUNNEL' ? pinIndex1 : pinIndex2;
      const otherComp = comp1.type === 'TUNNEL' ? comp2 : comp1;
      const otherPinIndex = comp1.type === 'TUNNEL' ? pinIndex2 : pinIndex1;
      // 假如另一组件是输出电流的  todo 没有考虑两个都是隧道的情况
      if(otherPinIndex >= otherComp.getInputPinCount()) {
        // inputId 对应电线输入端，即其实际上为该元件的输出引脚
        outputId = tunnelComp.id;
        outputIdx = tunnelPinIndex;
        inputId = otherComp.id;
        inputIdx = otherPinIndex - otherComp.getInputPinCount();
        this.InputTunnelMap.get(tunnelComp.name)!
          .splice(this.InputTunnelMap.get(tunnelComp.name)!.indexOf(tunnelComp.id, 1)); 
        this.enqueue(tunnelComp.id, 0, -1);
      }else {
        inputId = tunnelComp.id;
        inputIdx = tunnelPinIndex;
        outputId = otherComp.id;
        outputIdx = otherPinIndex - otherComp.getInputPinCount();
      }
    }

    this.connectionManager.removeConnection(inputId, inputIdx, outputId, outputIdx);

    // 断开连接后，清除该电线输出端的输入引脚的输入
    const component = this.circuitStore.getComponent(outputId);
    if (!component) return false;
    const oldOutputs = [...component.getOutputs()];
    component.changeInput(outputIdx, -1); 
    const newOutputs = component.getOutputs();
    // 如果输出没有变化，则不需要通知其他组件
    if(oldOutputs === newOutputs) return;
    // if (this.isEqualOutputs(oldOutputs, newOutputs)) return false;
    const pinMap = this.connectionManager.getOutputPinMap(outputId);
    if (!pinMap) return false;
    for (const pinIdx of pinMap.keys()) {
      for(const conn of pinMap.get(pinIdx) || []) {
        if (conn.legal) {
          const targetComponent = this.circuitStore.getComponent(conn.id);
          if (!targetComponent) continue;

          // 通知其他组件该组件的输出改变
          this.enqueue(conn.id, conn.idx, newOutputs[pinIdx]);
        }
      }
    }
    this.processQueue();
  }

  // 取消一个组件与前驱的连接
  disconnectPredecessors(id: number) {
    // 从输入端查找
    const pinMap = this.connectionManager.getInputPinMap(id);
    if (pinMap) {
      for (const pinIdx of pinMap.keys()) {
        for(const conn of pinMap.get(pinIdx) || []) {
          this.disconnect(conn.id, conn.idx, id, pinIdx);
        }
      }
    }
  }
  // 取消一个组件与后继的连接
  disconnectSuccessors(id: number) {
    // 从输出端查找
    const pinMap = this.connectionManager.getOutputPinMap(id);
    if (pinMap) {
      for (const pinIdx of pinMap.keys()) {
        for(const conn of pinMap.get(pinIdx) || []) {
          this.disconnect(id, pinIdx, conn.id, conn.idx);
        }
      }
    }
  }

  // 移除一个组件，删除与其有关的所有连接
  removeComponent(id: number) {
    // 检查是否是隧道
    if( this.circuitStore.getComponent(id)?.type === 'TUNNEL') {
      this.removeTunnel(this.circuitStore.getComponent(id)!.name, id);
    }
    // 删除与该组件有关的所有连接
    // 从输出端查找
    this.disconnectSuccessors(id);
    // 从输入端查找
    this.disconnectPredecessors(id);
  }

  checkComponentConnections(id: number){
    // 检查与该组件相连的connection的合法性
    // 检查与该组件的输出相连的位宽
    const component = this.circuitStore.getComponent(id);
    if (!component) return;
    const pinMap = this.connectionManager.getOutputPinMap(id);
    if (pinMap){
      for (const pinIdx of pinMap.keys()) {
        for(const conn of pinMap.get(pinIdx) || []) {
          const targetComponent = this.circuitStore.getComponent(conn.id);
          if (!targetComponent) continue;
          // 判断位宽是否合法
          if (component.bitWidth !== targetComponent.bitWidth) {
            conn.legal = false; 
            this.enqueue(conn.id, conn.idx, -2); 
          } else {
            if(conn.legal === false){
              conn.legal = true; 
              this.enqueue(conn.id, conn.idx, component.getOutputs()[conn.idx]); // 恢复合法后，重新通知
            }
          }
        }
      }
    }
    
    // 检查与该组件的输入相连的位宽
    const inputPinMap = this.connectionManager.getInputPinMap(id);
    if (inputPinMap){
      for (const pinIdx of inputPinMap.keys()) {
        for(const conn of inputPinMap.get(pinIdx) || []) {
          const targetComponent = this.circuitStore.getComponent(conn.id);
          if (!targetComponent) continue;
          // 判断位宽是否合法
          if (component.bitWidth !== targetComponent.bitWidth) {
            conn.legal = false; 
            this.enqueue(id, pinIdx, -2); 
          } else {
            if(conn.legal === false){
              conn.legal = true; 
              this.enqueue(id, pinIdx, targetComponent.getOutputs()[conn.idx]); // 恢复合法后，重新通知
            }
          }
        }
      }
    }

    this.processQueue();
  }

  // 维护隧道
  // 添加隧道
  addTunnel(name: String, id: number) {
    if (!this.tunnelNameMap.has(name)) {
      this.tunnelNameMap.set(name, []);
      this.InputTunnelMap.set(name, []);
    }
    this.tunnelNameMap.get(name)!.push(id);

    // 更新隧道的输出
    // 获取隧道的输入，传播新的输出
    const inputTunnels = this.InputTunnelMap.get(name);
    if(inputTunnels && inputTunnels.length > 1) {
      this.enqueue(id, 0, -2);
    }else if(inputTunnels?.length === 1) {
      this.enqueue(id, 0, this.circuitStore.getComponent(inputTunnels[0]).outputs[0]);
    }else{
      this.enqueue(id, 0, -1);
    }

    // 假如隧道有输入
    const inputPinMap = this.connectionManager.getInputPinMap(id);
    if (inputPinMap) {
      const inputTunnels = this.InputTunnelMap.get(name)!;
      if (!inputTunnels.includes(id)) {
        inputTunnels.push(id);
      }
      // 传播新的输入
      this.enqueue(id, 0, this.circuitStore.getComponent(id).outputs[0]); 
    }

    

  }

  // 删除隧道
  removeTunnel(name: String, id: number) {
    if (!this.tunnelNameMap.has(name)) return;
    const tunnels = this.tunnelNameMap.get(name)!;
    let index = tunnels.indexOf(id);
    if (index !== -1) {
      tunnels.splice(index, 1);
    }
    index = this.InputTunnelMap.get(name)?.indexOf(id) || -1;
    if (index !== -1) {  // 是输入隧道
      this.InputTunnelMap.get(name)?.splice(index, 1);

      // 传播-1
      this.enqueue(id, 0, -1);
    }

    if (tunnels.length === 0) {
      this.tunnelNameMap.delete(name);
      this.InputTunnelMap.delete(name);
    }
  }

  // 处理输出改变时的情况（给BaseComponent调用）
  // 参数：id :改变输出的组件的id
  //      idx: 该组件的改变输出的引脚
  //     value:  引脚将变为何值
  processOutputChange(id: number, idx: number, value: number): void {
    // 获取组件的后继
    const pinMap = this.connectionManager.getOutputPinMap(id);
    if (!pinMap) return;
    for (const pinIdx of pinMap.keys()) {
      if(pinIdx !== idx) continue;
      for( const conn of pinMap.get(pinIdx) || []) {
        //if (conn.legal) {
          const targetComponent = this.circuitStore.getComponent(conn.id)!;
          if (!targetComponent) continue;

          this.enqueue(conn.id, conn.idx, value);
        //}
      }
    }
    // 处理队列
    this.processQueue();
  }


  // 全局处理输入改变的情况
  // 参数：id :输入改变的组件的id
  //      idx: 该组件的改变输入的引脚
  //     value:  引脚将变为何值
  enqueue(id: number, idx: number, value: number): void {
    const key = `${id}_${idx}`;
    // if (this.inQueue.has(key)) return;
    if (this.inQueue.has(key)) {
      // 更新队列中的任务而不是直接返回 todo 这种方法待测试
      const idx = this.workQueue.findIndex(task => task.id === id && task.idx === idx);
      if (idx !== -1) {
        this.workQueue[idx].value = value; // 更新任务的值
      }
      return;
    }
    this.workQueue.push({ id, idx, value });
    this.inQueue.add(key);
  }

  processQueue(): void {
    // // 移动到web worker中处理，以增加前端的响应性
    // const worker = new Worker(new URL('@/workers/simulatorWorker.ts', import.meta.url));
    // worker.postMessage({
    //   workQueue: this.workQueue,
    //   connectionManager: this.connectionManager,
    //   circuitStore: this.circuitStore,
    // });

    // worker.onmessage = (event) => {
    //   // console.log("Queue processed:", event.data);
    // };

    // worker.onerror = (error) => {
    //   console.error("Worker error:", error);
    // };

    if(!this.enableSimulator || this.pause) return; // 如果模拟器未启用或暂停，则不处理队列
    while (this.workQueue.length > 0) {
      // 组件的id为id，它更改其索引为idx的引脚的输入为value
      const { id, idx, value } = this.workQueue.shift()!;
      this.inQueue.delete(`${id}_${idx}`);

      const component = this.circuitStore.getComponent(id);
      if (!component) continue;

      let oldOutputs: number[];
      let newOutputs: number[];

      if(this.circuitStore.getComponent(id).type !== 'INPUT') {
        // 获取当前组件的新旧输出
        oldOutputs = [...component.getOutputs()];
        newOutputs = component.changeInput(idx, value); 
        if(this.isEqualOutputs(oldOutputs, newOutputs)) continue; // 如果输出没有变化，则不需要通知其他组件
      }else{
        newOutputs = component.getOutputs();
      }

      const pinMap = this.connectionManager.getOutputPinMap(id);
      if (pinMap) {
        for (const pinIdx of pinMap.keys()) {
          for( const conn of pinMap.get(pinIdx) || []) {
            //if (conn.legal) {
              const targetComponent = this.circuitStore.getComponent(conn.id);
              if (!targetComponent) continue;

              this.enqueue(conn.id, conn.idx, newOutputs[pinIdx]);
            }
          //}
        }
      }

      

      // 处理tunnel同步
      if (component?.type === 'TUNNEL') {
        const name = component.name;
        const tunnelIds = this.tunnelNameMap.get(name);
        const inputTunnels = this.InputTunnelMap.get(name);
        if (tunnelIds) {
          for (const tunnelId of tunnelIds) {
            if (tunnelId !== id) {
              if (inputTunnels && inputTunnels.length > 1) {
                this.enqueue(tunnelId, 0, -2); // 强制传播给其他 tunnel
              }else if(inputTunnels && inputTunnels.length === 1){
                this.enqueue(tunnelId, 0, this.circuitStore.getComponent(inputTunnels[0]).outputs[0]);
              }else{
                this.enqueue(tunnelId, 0, -1); 
              }
            }
          }
        }
      }
      
    }
  }

  private isEqualOutputs(a: number[], b: number[]): boolean {
    if (a.length !== b.length) return false;
    return a.every((v, i) => v === b[i]);
  }

  getConnectionManager(id: number): ConnectionManager {
    return this.connManagerMap.get(id)!;
  }
  getProjectTunnel(id: number): [Map<String, number[]>, Map<String, number[]>] {
    return this.projectTunnel.get(id)!;
  }
  changeProject(id: number): void {
    if (!this.connManagerMap.has(id)) {
      this.connManagerMap.set(id, new ConnectionManager());
      this.projectTunnel.set(id, [new Map(), new Map()]); // 初始化该项目的隧道信息
    }
    this.connectionManager = this.connManagerMap.get(id)!;
    const [tunnelNameMap, inputTunnelMap] = this.projectTunnel.get(id)!;
    this.tunnelNameMap = tunnelNameMap;
    this.InputTunnelMap = inputTunnelMap;
  }
}

