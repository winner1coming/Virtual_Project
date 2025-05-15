import { BaseComponent } from "../BaseComponent";

export class NandGate extends BaseComponent {
    constructor(id: number, type: String, position: [number, number] = [0, 0], pinPosition = []) {
        super(id, type, position, pinPosition);
        this.inputs = [-1, -1];  // 默认两个输入引脚
        this.outputs = [-1];
    }

    compute(): number[] {
        let hasConnected = false;
        let result = 1;
        for (const value of this.inputs) {
            if (value === -2) {
                this.outputs[0] = -2;
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
            this.outputs[0] = -1;
        } else {
            this.outputs[0] = result === 1 ? 0 : 1; 
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        this.inputs[idx] = v;
        if (v === -2) {
            this.outputs[0] = -2;
        } else {
            return this.compute();
        }
        return this.outputs;
    }
}