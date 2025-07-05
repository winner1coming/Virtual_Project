// Splitter.ts - 分离器
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class Splitter extends BaseComponent {
    constructor(id: number,type: String, position: [number, number] = [0, 0], simulator = null,bitWidth: number = 4) {
        super(id, type, position);
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.bitWidth = bitWidth;
        this.initInputPin(1)
        this.outputs.splice(0, this.outputs.length, ...Array(bitWidth).fill(-1)); // 初始化输出引脚
    }

    setBitWidth(bitWidth: number) {
        this.bitWidth = bitWidth;
        this.outputs.splice(0, this.outputs.length, ...Array(this.bitWidth).fill(-1)); 
    }

    compute(): number[] {
        const inputVal = this.inputs[0];

        if (inputVal === -1) {
            this.outputs.splice(0, this.outputs.length, -1);
        }else if(inputVal === -2){
            this.outputs.splice(0, this.outputs.length,-2);
        } else {
            const binary = inputVal.toString(2).padStart(this.bitWidth, '0');
            for (let i = 0; i < this.bitWidth; i++) {
                this.outputs.splice(i, 1, binary[this.bitWidth - 1 - i] === '1' ? 1 : 0); 
            }
        }

        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        if (idx === 0){
            this.inputs.splice(0, 1, v); // 替换idx位置的值
        }
        if(v === -2){
            this.outputs.splice(0, this.outputs.length, -2);
        }
        return this.compute();
    }
}
