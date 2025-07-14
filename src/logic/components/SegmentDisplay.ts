import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";
export class SegmentDisplay extends BaseComponent{
  constructor(id: number, type: string, position:[number, number] = [0,0], simulator: any = null){
    super(id, type, position);
    this.offset = [-250, -200];
    if(!simulator) {
      this.simulator = EventDrivenSimulator.getInstance(); 
    } else {
      this.simulator = simulator; 
    }
    this.outputs.splice(0, this.outputs.length); 
    this.initInputPin(8); // SegmentDisplay有8个输入引脚
  }

  compute(){   // 返回输出(int)
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[]{
    if (idx < 0 || idx >= this.inputCount) {
      throw new Error(`Input index ${idx} is out of bounds.`);
    }
    this.inputs.splice(idx, 1, v); // 替换idx位置的值
    return this.outputs;
  }
  
  updatePinPosition(): void{
    this.inputPinPosition.splice(0, this.inputPinPosition.length, [182, 61], [234,61],[286,61], [340,61],[182,426],[234,426],[286,426],[340,426]);
  }
}