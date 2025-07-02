// Tunnel.ts
import { BaseComponent } from '../BaseComponent';
import { reactive } from 'vue';
import { EventDrivenSimulator } from '../Simulator';

export class Tunnel extends BaseComponent {
  constructor(id: number, type: String, position: [number, number] = [0, 0], name: String) {
    super(id, type, position);
    // 只支持输出，隧道输入的逻辑放在模拟器中全局判断
    this.inputCount = 0;
    this.inputs.splice(0, this.inputs.length);       // 0个输入
    this.inputPinPosition.splice(0, this.inputPinPosition.length, [0,0]);
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
}
