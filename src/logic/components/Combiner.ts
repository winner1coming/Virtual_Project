// Combiner.ts - 合并器
import { BaseComponent } from "../BaseComponent";

export class Combiner extends BaseComponent {
    constructor(id: number, position: [number, number] = [0, 0], bitCount: number = 4) {
        super(id, "combiner", position);
        this.bitCount = bitCount;
        this.inputs = Array(bitCount).fill(-1); // n 个输入
        this.outputs = [ -1 ]; // 只有一个输出
    }

    changeBitCount(bitCount: number) {
        this.bitCount = bitCount;
        this.inputs = Array(bitCount).fill(-1);
    }

    compute(): number[] {
        let valid = true;
        let result = 0;
        for (let i = 0; i < this.bitCount; i++) {
            const bit = this.inputs[i];
            if (bit !== 0 && bit !== 1) {
                valid = false;
                break;
            }
            if (bit === 1) {
                result |= (1 << i); // 低位输入在低编号
            }
        }
        this.outputs[0] = valid ? result : -1;
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        if (idx >= 0 && idx < this.bitCount) {
            this.inputs[idx] = v;
        }
        return this.compute();
    }
}