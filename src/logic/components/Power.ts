import { BaseComponent } from "../BaseComponent";
export class Power extends BaseComponent{
  constructor(id: number, type: String, position:[number, number] = [0,0]){
    super(id, type, position);
    this.outputs.splice(0, this.outputs.length, 1); // Power组件的输出始终为1
    this.changeInputPinCount(0); // Power组件没有输入引脚
  }

  compute(){   // 返回输出(int)
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[]{
      throw new Error("Power component does not have inputs to change.");
  }
}