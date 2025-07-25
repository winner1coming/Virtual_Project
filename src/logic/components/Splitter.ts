// Splitter.ts - 分离器
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";
import { calcInputYs } from "@/logic/utils/useGateLayout";
import eventBus from "@/modules/useEventBus";

export class Splitter extends BaseComponent {
    constructor(id: number,type: string, position: [number, number] = [0, 0], simulator = null,bitWidth: number = 4) {
        super(id, type, position);
        this.offset = [-150, -280];
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.bitWidth = bitWidth;
        this.initInputPin(1)
        this.initOutputPin(this.bitWidth); 
    }

    setBitWidth(bitWidth: number) {
        this.bitWidth = bitWidth;
        this.changeOutputPinCount(this.bitWidth);
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

    // 更新引脚位置
    updatePinPosition(): void{
        // 修改输出
        const outputYs = calcInputYs(this.outputs.length);
        let minY = Math.min(...outputYs);
        let maxY = Math.max(...outputYs);
        this.outputPinPosition.splice(0, this.outputPinPosition.length,
        ...outputYs.map((pin, index): [number, number] => {
            return [
            this.direction==='east'? 206:92,
            pin,
            ];
        }));
        // 修改输入
        this.inputPinPosition.splice(0, this.inputPinPosition.length,[
            this.direction==='east'? 100:198,
            minY<246? maxY+36: 439.34,
        ]);
    }
}
