import eventBus from "@/modules/useEventBus";
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class OutputPin extends BaseComponent {
    // private binaryValue: number[]; // 存储当前二进制值
    constructor(
        id: number, 
        type: string = "OutputPin", 
        position: [number, number] = [0, 0], 
        simulator: any = null
    ) {
        super(id, type, position);
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.direction = 'west';
        this.initInputPin(1); 
        this.outputs.splice(0, this.outputs.length); // 输出引脚不产生输出
        this.outputPinPosition.splice(0, this.outputPinPosition.length); 
    }

    getBits(): number[] {
        if (this.inputs[0] === -1 || this.inputs[0] === -2) {
            return new Array(this.bitWidth).fill(-1);  
        }
        const bits: number[] = [];
        let value = this.inputs[0]; 

        for (let i = 0; i < this.bitWidth; i++) {
            // 提取每一位（从高位到低位）
            bits.unshift(value & 1); // 取最低位
            value = value >> 1; // 右移 1 位
        }
        return bits;
    }

    // 改变输入引脚值
    changeInput(idx: number, v: number): number[] {
        this.inputs.splice(0, 1, v); 
        return this.inputs; 
    }

    // 计算逻辑
    compute(): number[] {
        return this.inputs; 
    }

    getOutputs(): number[] {
        // 直接返回引脚的值
        return this.inputs;
    }

    // 更新引脚位置
    updatePinPosition(): void{
        // 排布相关参数
        const colMax = 8;
        const cellWidth = 40;
        const cellHeight = 60;
        const padding = 40;
        const cols = Math.min(this.bitWidth, colMax);
        const rows = Math.ceil(this.bitWidth / colMax);
        // 宽高
        const svgWidth = cols * cellWidth + padding;
        const svgHeight = rows * cellHeight + padding/2;
        // 修改输出
        if(this.direction === 'east')
        {
            this.inputPinPosition.splice(0, this.inputPinPosition.length, [svgWidth, svgHeight/2]);
        }
        else if(this.direction === 'west')
        {
            this.inputPinPosition.splice(0, this.inputPinPosition.length, [0, svgHeight/2]);
        }
        else if(this.direction === 'north')
        {
            this.inputPinPosition.splice(0, this.inputPinPosition.length, [svgWidth/2, 0]);
        }
        else if(this.direction === 'south')
        {
            this.inputPinPosition.splice(0, this.inputPinPosition.length, [svgWidth/2, svgHeight]);
        }
    }

    // 设置位数
    setBitWidth(bitWidth: number): void {
        if (bitWidth !== this.bitWidth) {
            this.bitWidth = bitWidth;
            const newInputs = new Array(bitWidth).fill(-1);
            const copyLength = Math.min(bitWidth, this.inputs.length);

            for (let i = 0; i < copyLength; i++) {
                if (this.inputs[i] !== undefined) {
                    newInputs[i] = this.inputs[i];
                }
            }

            this.inputs.splice(0, this.inputs.length, ...newInputs); 
            this.updatePinPosition();
            eventBus.emit('updatePinPosition', {id: this.id}); 
        }
    }
}