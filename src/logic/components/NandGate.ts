import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class NandGate extends BaseComponent {
    constructor(id: number, type: String, position: [number, number] = [0, 0], simulator: any = null) {
        super(id, type, position);
		this.offset = [-280, -280];
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
    }

    compute(): number[] {
        let hasConnected = false;
        let result = 1;
        for (const value of this.inputs) {
            if (value === -2) {
                //this.outputs[0] = -2;
                this.outputs.splice(0, this.outputs.length, -2); // 输出引脚错误
                return this.outputs;
            }
            if (value !== -1) {
                if (!hasConnected) {
                    hasConnected = true;
                    result = value;
                } else {
                    result = result & value;
                }
            }
        }
        if (!hasConnected) {
            //this.outputs[0] = -1;
            this.outputs.splice(0, this.outputs.length, -1); // 如果没有连接任何输入，则输出-1
        } else {
            //this.outputs[0] = result === 1 ? 0 : 1; 
            this.outputs.splice(0, 1, result === 1 ? 0 : 1); // 替换outputs[0]的值
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        //this.inputs[idx] = v;
        this.inputs.splice(idx, 1, v); // 替换idx位置的值
        if (v === -2) {
            // this.outputs[0] = -2;
            this.outputs.splice(0, this.outputs.length, -2); 
        } else {
            return this.compute();
        }
        return this.outputs;
    }
}