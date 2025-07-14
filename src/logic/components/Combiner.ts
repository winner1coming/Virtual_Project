// Combiner.ts - 合并器
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";
import { calcInputYs } from "@/logic/utils/useGateLayout";

export class Combiner extends BaseComponent {
    constructor(id: number,type: string, position: [number, number] = [0, 0], bitWidth: number = 4, simulator: any = null) {
        super(id, type, position);
        this.offset = [-150, -280];
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.bitWidth = bitWidth;
        this.initInputPin(this.bitWidth); 
    }

    setBitWidth(bitWidth: number) {
        this.bitWidth = bitWidth;
        this.changeInputPinCount(this.bitWidth); 
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
    
    // 更新引脚位置
    updatePinPosition(): void{
        // 修改输入
        const inputYs = calcInputYs(this.inputs.length);
        let minY = Math.min(...inputYs);
        let maxY = Math.max(...inputYs);
        this.inputPinPosition.splice(0, this.inputPinPosition.length,
        ...inputYs.map((pin, index): [number, number] => {
            return [
            this.direction==='east'? 92:206,
            pin,
            ];
        }));
        // 修改输出
        this.outputPinPosition.splice(0, this.outputPinPosition.length,[
            this.direction==='east'? 198:100,
            minY<246? maxY+36: 439.34,
        ]);
    }
}