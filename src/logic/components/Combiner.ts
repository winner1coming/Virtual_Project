// Combiner.ts - 合并器
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class Combiner extends BaseComponent {
    constructor(id: number,type: String, position: [number, number] = [0, 0], bitWidth: number = 4, simulator: any = null) {
        super(id, type, position);
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.bitWidth = bitWidth;
        this.inputs.splice(0, this.inputs.length, ...Array(bitWidth).fill(-1));
        this.inputCount = bitWidth;
        this.inputInverted.splice(0, this.inputInverted.length, ...Array(bitWidth).fill(false)); 
    }

    changeBitWidth(bitWidth: number) {
        this.bitWidth = bitWidth;
         this.inputs.splice(0, this.inputs.length, ...Array(bitWidth).fill(-1));
    }

    compute(): number[] {
        let valid = true;
        let result = 0;
        for (let i = 0; i < this.bitWidth; i++) {
            const bit = this.inputs[i];
            if (bit !== 0 && bit !== 1) {
                valid = false;
                break;
            }
            if (bit === 1) {
                result |= (1 << i); // 低位输入在低编号
            }
        }
        this.outputs.splice(0, 1, valid ? result : -1);
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[] {
        if (idx >= 0 && idx < this.bitWidth) {
            this.inputs.splice(idx, 1, v); // 替换idx位置的值
        }
        return this.compute();
    }
}