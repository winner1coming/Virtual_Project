// Splitter.ts - 分离器
import { BaseComponent } from "../BaseComponent";

export class Splitter extends BaseComponent {
    constructor(id: number, position: [number, number] = [0, 0], bitCount: number = 4) {
        super(id, "splitter", position);
        this.bitCount = bitCount;
        //this.inputs = [-1]; // 只有一个输入
        this.inputs.splice(0, this.inputs.length, -1); // 初始未连接状态
        this.inputInverted.splice(0, this.inputInverted.length, false); 
        this.inputCount = 1; // 输入引脚数量
        //this.outputs = Array(bitCount).fill(-1); // n 个输出
        this.outputs.splice(0, this.outputs.length, ...Array(bitCount).fill(-1)); // 初始化输出引脚
    }

    changeBitCount(bitCount: number) {
        this.bitCount = bitCount;
        //this.outputs = Array(bitCount).fill(-1);
        this.outputs.splice(0, this.outputs.length, ...Array(bitCount).fill(-1)); 
    }

    compute(): number[] {
        const inputVal = this.inputs[0];
        if (inputVal === -1 || inputVal < 0) {
            this.outputs.fill(-1);
        } else {
            const binary = inputVal.toString(2).padStart(this.bitCount, '0');
            for (let i = 0; i < this.bitCount; i++) {
                // 高位 -> 高编号
                //this.outputs[i] = binary[this.bitCount - 1 - i] === '1' ? 1 : 0;
                this.outputs.splice(i, 1, binary[this.bitCount - 1 - i] === '1' ? 1 : 0); 
            }
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        if (idx === 0){
            this.inputs.splice(0, 1, v); // 替换idx位置的值
        }
        return this.compute();
    }
}
