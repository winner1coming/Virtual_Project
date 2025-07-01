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
import { EventDrivenSimulator } from './Simulator';

interface WorkItem {
  id: number;
  idx: number;
  value: number;
}

export class SubSimulator {
  private connectionManager: ConnectionManager;
  private componentIdMap: Map<number, BaseComponent> = new Map(); // 用于映射元件的id
  private workQueue: WorkItem[] = [];
  private inQueue: Set<string> = new Set();
  private enableSimulator: Boolean = false; // 是否启用模拟器
  private pause: Boolean = false; // 是否暂停模拟器

  // 隧道维护
  public tunnelNameMap: Map<String, number[]> = new Map(); // 存储隧道名字与id
  public InputTunnelMap: Map<String, number[]> = new Map(); // 记录接受输入的隧道
  
  constructor(projectId: number, componentIdMap: Map<number, BaseComponent> = new Map()) {
    this.connectionManager = EventDrivenSimulator.getInstance().getConnectionManager(projectId);
    [this.tunnelNameMap, this.InputTunnelMap] = EventDrivenSimulator.getInstance().getProjectTunnel(projectId);
    this.componentIdMap = componentIdMap;
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

  // 全局处理输入改变的情况
  // 参数：id :输入改变的组件的id
  //      idx: 该组件的改变输入的引脚
  //     value:  引脚将变为何值
  enqueue(id: number, idx: number, value: number): void {
    const key = `${id}_${idx}`;
    if (this.inQueue.has(key)) return;
    this.workQueue.push({ id, idx, value });
    this.inQueue.add(key);


    // 处理tunnel同步
    const comp = this.componentIdMap.get(id)!;
    if (comp?.type === 'TUNNEL') {
      const name = comp.name;
      const tunnelIds = this.tunnelNameMap.get(name);
      const inputTunnels = this.InputTunnelMap.get(name);
      if (tunnelIds) {
        for (const tunnelId of tunnelIds) {
          if (tunnelId !== id) {
            if (inputTunnels && inputTunnels.length > 1) {
              this.enqueue(tunnelId, 0, -2); // 强制传播给其他 tunnel
            }else if(inputTunnels && inputTunnels.length === 1){
              this.enqueue(tunnelId, 0, this.componentIdMap.get(inputTunnels[0])!.outputs[0]);
            }else{
              this.enqueue(tunnelId, 0, -1); 
            }
          }
        }
      }
    }
  }

  processQueue(): void {
    while (this.workQueue.length > 0) {
      // 组件的id为id，它更改其索引为idx的引脚的输入为value
      const { id, idx, value } = this.workQueue.shift()!;
      this.inQueue.delete(`${id}_${idx}`);

      const component = this.componentIdMap.get(id)!;
      if (!component) continue;

      // 获取当前组件的新旧输出
      const oldOutputs = [...component.getOutputs()];
      const newOutputs = component.changeInput(idx, value);      

      if (!this.isEqualOutputs(oldOutputs, newOutputs)) {
        const pinMap = this.connectionManager.getOutputPinMap(id);
        if (!pinMap) continue;

        for (const pinIdx of pinMap.keys()) {
          for( const conn of pinMap.get(pinIdx) || []) {
            if (conn.legal) {
              const targetComponent = this.componentIdMap.get(conn.id)!;
              if (!targetComponent) continue;

              this.enqueue(conn.id, conn.idx, newOutputs[pinIdx]);
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

  getConnectionManager(): ConnectionManager {
    return this.connectionManager;
  }
}

