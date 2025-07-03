import { BaseComponent } from "../BaseComponent";

export class XorGate extends BaseComponent {
    constructor(id: number, type: String, position: [number, number] = [0, 0]) {
        super(id, type, position);
    }

    compute(): number[] {
        let hasConnected = false;
        let result = 0;
        for (const value of this.inputs) {
            if (value === -2) {
                this.outputs.splice(0, this.outputs.length, -2); // 输出引脚错误
                return this.outputs;
            }
            if (value !== -1) {
                if (!hasConnected) {
                    hasConnected = true;
                    result = value;
                } else {
                    result = result ^ value; 
                }
            }
        }
        if (!hasConnected) {
            this.outputs.splice(0, this.outputs.length, -1); 
        } else {
            this.outputs.splice(0, 1, result); 
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        this.inputs.splice(idx, 1, v); // 替换idx位置的值
        if (v === -2) {
            this.outputs.splice(0, this.outputs.length, -2); // 输出引脚错误
        } else {
            return this.compute();
        }
        return this.outputs;
    }
}