import { BaseComponent } from "../BaseComponent";
export class SegmentDisplay extends BaseComponent{
  constructor(id: number, type: String, position:[number, number] = [0,0],  pinPosition = []){
    super(id, type, position, pinPosition);
    this.outputs.splice(0, this.outputs.length); 
    this.inputCount = 8;
    this.inputs.splice(0, this.inputs.length, ...Array(this.inputCount).fill(-1)); 
    this.inputInverted.splice(0, this.inputInverted.length, ...Array(this.inputCount).fill(false)); 
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
}