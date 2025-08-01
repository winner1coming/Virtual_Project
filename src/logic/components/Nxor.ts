import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";
import { calcInputYs } from "@/logic/utils/useGateLayout";

export class NxorGate extends BaseComponent {
    constructor(id: number, type: string, position: [number, number] = [0, 0], simulator: any = null) {
        super(id, type, position);
        this.offset = [-280, -280];
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.initInputPin(2); // 初始化输入引脚数量为2
    }

   compute(){   // 返回输出(int)
        let hasConnected = false;
        const mask = (1 << this.bitWidth) - 1;
        for (let i = 0; i < this.inputs.length; i++) {
            let value = this.inputs[i];

            // 如果输入引脚被设置为取反，则取反值
            if (this.inputInverted[i]) {
                value = value === -1 || value === -2 ? value : ~value & mask; // 保持 -1 和 -2 不变，其他值取反
            }

            if (value === -2) {
                this.outputs.splice(0, this.outputs.length, -2);
                return this.outputs;
            }

            if (value !== -1) {
                if (!hasConnected) {
                hasConnected = true;
                this.outputs.splice(0, 1, value); 
                } else {
                this.outputs.splice(0, 1, this.outputs[0] ^ value); 
                }
            }
        }
        if(!hasConnected) {
            this.outputs.splice(0, this.outputs.length, -1); // 如果没有连接任何输入，则输出-1
        }else {
            // 如果有连接输入，则输出取反
            this.outputs.splice(0, this.outputs.length, ~this.outputs[0] & mask);
        }
        return this.outputs;
    }

    updatePinPosition(): void{
        // 修改输入
        const inputYs = calcInputYs(this.inputCount);
        this.inputPinPosition.splice(0, this.inputPinPosition.length,
        ...inputYs.map((pin, index): [number, number] => {
            return [
            42,
            pin,
            ];
        }));
        // 修改输出
        this.outputPinPosition = this.outputPinPosition.map(pin => {
        return [
            0 + 497,
            0 + 288,
        ];
        });
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