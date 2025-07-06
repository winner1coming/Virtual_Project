import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";
export class Button extends BaseComponent{
  constructor(id: number, type: String, position:[number, number] = [0,0], simulator: any = null){
    super(id, type, position);
    if(!simulator) {
      this.simulator = EventDrivenSimulator.getInstance();
    } else {
      this.simulator = simulator;
    }
    this.initInputPin(0); 
    this.initOutputPin(1); 
    this.outputs.splice(0, this.outputs.length, 0); // Button组件的输出初始为0
  }

  compute(){   
    return this.outputs;
  }

  changeInput(idx: number, v: number): number[]{
    this.outputs.splice(0, this.outputs.length, v); 
    this.simulator.processOutputChange(this.id, 0, v);
    return this.outputs;
  }

  updatePinPosition(): void{
  }
}