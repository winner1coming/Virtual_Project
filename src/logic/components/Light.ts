import { BaseComponent } from "../BaseComponent";
export class Light extends BaseComponent{
  constructor(id: number, type: string, position:[number, number] = [0,0]){
    super(id, type, position);
    this.initInputPin(1); 
    this.initOutputPin(0); 
    this.changeInput(0,0);
    this.direction = 'west';
    this.updatePinPosition();
  }

  compute(){   
    return this.inputs;
  }

  changeInput(idx: number, v: number): number[]{
    this.inputs.splice(idx, 1, v); 
    return this.inputs;
  }

  updatePinPosition(): void{
    this.inputPinPosition.splice(0, this.inputPinPosition.length, [-40, 0]);
    if(this.direction === 'east')
    {
        this.inputPinPosition.splice(0, this.inputPinPosition.length, [40, 0]);
    }
    else if(this.direction === 'west')
    {
        this.inputPinPosition.splice(0, this.inputPinPosition.length, [-40, 0]);
    }
    else if(this.direction === 'north')
    {
        this.inputPinPosition.splice(0, this.inputPinPosition.length, [0, -40]);
    }
    else if(this.direction === 'south')
    {
        this.inputPinPosition.splice(0, this.inputPinPosition.length, [0, 40]);
    }
  }
}