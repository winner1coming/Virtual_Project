import { BaseComponent } from "../BaseComponent.js";
import { EventDrivenSimulator } from "../Simulator.js";

export class OrGate extends BaseComponent {
    constructor(id: number, type: String, position:[number, number] = [0,0], simulator: any = null) {
        super(id, type, position);
        this.offset = [-280, -280];
        if(!simulator) {
            this.simulator = EventDrivenSimulator.getInstance(); 
        } else {
            this.simulator = simulator; 
        }
		this.initInputPin(2); // 初始化输入引脚数量为2
        
    }
    // updatePinPosition() {
    //     this.outputPinPosition = this.outputPinPosition.map(pin => {
    //       return [
    //         0 + 497 * this.scale,
    //         0 + 288 * this.scale,
    //       ];
    //     });
    // }
    compute() {
        let hasConnected = false;
        for(const value of this.inputs){
            if(value === -2){
                //this.outputs[0] = -2;
                this.outputs.splice(0, this.outputs.length, -2); 
                return this.outputs;
            }
            if(value !== -1){
                if(!hasConnected){
                    hasConnected = true;
                    // this.outputs[0] = value;
                    this.outputs.splice(0, 1, value); 
                } else{
                    // this.outputs[0] = this.outputs[0] | value;
                    this.outputs.splice(0, 1, this.outputs[0] | value); 
                }
            }
            
        }
        if(!hasConnected) {
            // this.outputs[0] = -1;
            this.outputs.splice(0, this.outputs.length, -1); 
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[]{
        // this.inputs[idx] = v;
        this.inputs.splice(idx, 1, v); 
        if(v===-2){
            // this.outputs[0] = -2;
            this.outputs.splice(0, this.outputs.length, -2);
            return this.outputs;
        }
        return this.compute();
    }
}