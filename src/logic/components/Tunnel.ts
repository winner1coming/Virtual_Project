// Tunnel.ts
import { BaseComponent } from '../BaseComponent';
import { reactive } from 'vue';
import { EventDrivenSimulator } from '../Simulator';

export class Tunnel extends BaseComponent {
  constructor(id: number, type: String, position: [number, number] = [0, 0], name: String, simulator: any = null) {
    super(id, type, position);
    // 只支持输出，隧道输入的逻辑放在模拟器中全局判断  todo 处理模拟器还是子模拟器
    if(!simulator) {
      this.simulator = EventDrivenSimulator.getInstance();
    }
    else {
      this.simulator = simulator;
    }
    this.offset=[-253,-310];
    this.initInputPin(0); 
    this.setName(name); // 设置 tunnel 名称
    EventDrivenSimulator.getInstance().addTunnel(name, id);
  }

  destroy() {
   EventDrivenSimulator.getInstance().removeTunnel(this.name, this.id); 
  }

  setName(name: String): void {
    EventDrivenSimulator.getInstance().removeTunnel(this.name, this.id); // 删除旧的隧道
    EventDrivenSimulator.getInstance().addTunnel(name, this.id); // 添加新的隧
    this.name = name;
  }

  compute(): number[] {
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[] {
    // 直接传播给输出
    this.outputs.splice(0, this.outputs.length, v); // 替换outputs[0]的值
    return this.outputs;
  }

  updatePinPosition(): void {
    this.outputPinPosition.splice(0, this.outputPinPosition.length, [183.98, 310]);
    this.inputPinPosition.splice(0, this.inputPinPosition.length); 
  }
}
