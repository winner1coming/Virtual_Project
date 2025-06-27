import { BaseComponent } from "../BaseComponent.js";

export class NotGate extends BaseComponent {
    constructor(id: number, type: String, position:[number, number] = [0,0],  pinPosition = []){
        super(id, type, position, pinPosition);
        //this.inputs = [-1];  // 输入引脚1个
        this.inputs.splice(0, this.inputs.length, -1); // 清空输入引脚并设置默认值
        this.inputCount = 1; // 输入引脚数量
        this.inputInverted.splice(0, this.inputInverted.length, false); 
    }
    compute() {
        // 非门只有一个输入
        if (this.inputs.length !== 1) {
            throw new Error("NotGate requires exactly one input.");
        }
        const input = this.inputs[0];
        if (input < 0) {
            //this.outputs[0] = input;
            this.outputs.splice(0, this.outputs.length, input); // 保持错误状态
        } else if (input === 1) {
            // this.outputs[0] = 0;
            this.outputs.splice(0, 1, 0); 
        } else if (input === 0) {
            // this.outputs[0] = 1;
            this.outputs.splice(0, 1, 1);
        }

        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        if (idx !== 0) {
            throw new Error("NotGate only supports a single input at index 0.");
        }
        //this.inputs[idx] = v;
        this.inputs.splice(idx, 1, v); 
        if (v < 0) {
            // this.outputs[0] = v;
            this.outputs.splice(0, this.outputs.length, v); 
        } else if (v === 1) {
            // this.outputs[0] = 0;
            this.outputs.splice(0, 1, 0);
        } else if (v === 0) {
            // this.outputs[0] = 1;
            this.outputs.splice(0, 1, 1);
        }
        return this.outputs;
    }
}