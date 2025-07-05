import { BaseComponent } from "../BaseComponent";
export class Ground extends BaseComponent{
  constructor(id: number, type: String, position:[number, number] = [0,0]){
    super(id, type, position);
    this.outputs.splice(0, this.outputs.length, 0); 
    this.initInputPin(0); // Ground组件没有输入引脚
  }

  compute(){   // 返回输出(int)
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[]{
      throw new Error("Power component does not have inputs to change.");
  }
  updatePinPosition(): void{
    this.outputPinPosition.splice(0, this.outputPinPosition.length, [62, 13]);
  }
}