import eventBus from "@/modules/useEventBus";
import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class OutputPin extends BaseComponent {
    // private binaryValue: number[]; // 存储当前二进制值

    constructor(
        id: number, 
        type: String = "OutputPin", 
        position: [number, number] = [0, 0], 
        simulator: any = null
    ) {
        super(id, type, position);
        if (!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        this.initInputPin(1); 
        this.outputs.splice(0, this.outputs.length); // 输出引脚不产生输出
        // this.binaryValue = new Array(this.bitWidth).fill(0); // 内部二进制表示
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

    // 实现基类抽象方法：改变输入引脚值
    changeInput(idx: number, v: number): number[] {
        // if (idx >= 0 && idx < this.inputs.length) {
        //     //this.inputs[idx] = v;
        //     this.inputs.splice(idx, 1, v); // 替换idx位置的值
        //     this.updateBinaryValue();
        // }
        this.inputs.splice(0, 1, v); 
        return this.inputs; 
    }

    // 实现基类抽象方法：计算逻辑
    compute(): number[] {
        return this.inputs; 
    }

    // // 更新内部二进制值表示
    // private updateBinaryValue(): void {
    //     const inputValue = this.inputs[0]; // 获取第一个输入引脚的值

    //     if (inputValue === -1) { // 未连接状态
    //         this.binaryValue = new Array(this.bitWidth).fill(0);
    //         return;
    //     }

    //     if (inputValue === -2) { // 错误状态
    //         this.binaryValue = new Array(this.bitWidth).fill(0); // 或用其他方式表示错误
    //         return;
    //     }

    //     // 将输入值转换为二进制数组
    //     const binaryStr = Math.max(0, inputValue).toString(2); // 确保非负数
    //     const paddedBinaryStr = binaryStr.padStart(this.bitWidth, '0');
        
    //     // 处理位数超出情况
    //     if (paddedBinaryStr.length > this.bitWidth) {
    //         this.binaryValue = new Array(this.bitWidth).fill(1); // 溢出时全部置1
    //     } else {
    //         this.binaryValue = paddedBinaryStr.slice(-this.bitWidth) // 取最低有效位
    //             .split('')
    //             .map(bit => parseInt(bit, 10));
    //     }
    // }

    // // 获取当前二进制值（副本）
    // getBinaryValue(): number[] {
    //     return [...this.binaryValue];
    // }

    // // 获取当前十进制值
    // getDecimalValue(): number {
    //     return this.binaryValue.reduce((acc, bit, index) => {
    //         return acc + (bit << (this.bitWidth - 1 - index));
    //     }, 0);
    // }

    // 覆盖基类方法：设置位数
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

            this.simulator.checkComponentConnections(this.id); // 检查连线
            this.updatePinPosition();
            eventBus.emit('updatePinPosition', {id: this.id}); 
        }
    }

    // 
    // changeInputPinCount(num: number): void {
    //     if (num !== 1) {
    //         console.warn("OutputPin should have exactly 1 input pin");
    //     }
    //     super.changeInputPinCount(1); // 强制保持1个输入
    // }
}