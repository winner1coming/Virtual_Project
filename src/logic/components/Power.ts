import { BaseComponent } from "../BaseComponent";
export class Power extends BaseComponent{
  constructor(id: number, type: String, position:[number, number] = [0,0],  pinPosition = []){
    super(id, type, position, pinPosition);
    this.outputs.splice(0, this.outputs.length, 1); // Power组件的输出始终为1
    this.inputCount = 0;
    this.inputs.splice(0, this.inputs.length); 
    this.inputInverted.splice(0, this.inputInverted.length); 
  }

  compute(){   // 返回输出(int)
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[]{
      throw new Error("Power component does not have inputs to change.");
  }
}