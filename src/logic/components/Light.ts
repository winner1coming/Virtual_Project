import { BaseComponent } from "../BaseComponent";
export class Light extends BaseComponent{
  constructor(id: number, type: String, position:[number, number] = [0,0]){
    super(id, type, position);
    this.initInputPin(1); 
    this.initOutputPin(0); 
  }

  compute(){   
    return this.inputs;
  }

  changeInput(idx: number, v: number): number[]{
    this.inputs.splice(idx, 1, v); 
    return this.inputs;
  }

  updatePinPosition(): void{
  }
}