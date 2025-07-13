import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class DFlipFlop extends BaseComponent {
  private lastClockState: number; // 上一次时钟状态
  private q: number; // Q 输出
  private qNot: number; 

  constructor(id: number, type: string, position: [number, number] = [0, 0], simulator: any = null) {
    super(id, type, position);
    this.offset = [-250,-250];
    if (!simulator) {
      this.simulator = EventDrivenSimulator.getInstance();
    } else {
      this.simulator = simulator;
    }
    this.initInputPin(5);    // Data, Clock, Enable, Load, Reset
    this.initOutputPin(2);  // Q Q'

    this.lastClockState = -1; // 初始时钟状态
    this.q = 0; 
    this.qNot = 1; 
    this.updatePinPosition();
  }

  compute(): number[] {
    if(this.inputs[2] === 0) { // Enable 为 0 时，保持当前状态
        return this.outputs;
    }
    if(this.inputs[3] === 1) { // Reset 为 1 时，重置 Q 输出
        this.q = 0;
        this.outputs.splice(0, this.outputs.length, this.q, ~this.q & 1); 
        return this.outputs;
    }
    if(this.inputs[4] === 1) { 
      this.q = this.inputs[0];
      this.qNot = ~this.q & 1; 
      this.outputs.splice(0, this.outputs.length, this.q, this.qNot);
      return this.outputs;
    }

    const d = this.inputs[0]; 
    const clock = this.inputs[1]; 

    // 检查时钟状态变化
    if (clock === 1 && this.lastClockState === 0) {
      // 时钟从低到高变化时，更新 Q 和 Q'
      if (d !== -1 && d !== -2) {
        this.q = d;
        this.qNot = ~d & 1; 
      }
    }

    this.lastClockState = clock; // 更新时钟状态

    // 设置输出
    this.outputs.splice(0, this.outputs.length, this.q, this.qNot);
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[] {
    this.inputs.splice(idx, 1, v); // 替换 idx 位置的值
    if (v === -2) {
      this.outputs.splice(0, this.outputs.length, -2, -2); // 输出引脚错误
    } else {
      return this.compute();
    }
    return this.outputs;
  }
  
  updatePinPosition(): void{
    this.inputPinPosition.splice(0, this.inputPinPosition.length, [92, 235], [92,300],[203,426], [266,426],[329,426]);
    this.outputPinPosition.splice(0, this.outputPinPosition.length, [422, 235],[422,300]);
  }
}