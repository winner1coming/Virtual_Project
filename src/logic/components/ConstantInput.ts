import { BaseComponent } from "../BaseComponent";
import { EventDrivenSimulator } from "../Simulator";

export class ConstantInput extends BaseComponent {
    private currentValue: number;
    private maxValue: number;

    constructor(
        id: number,
        type: String = "CONSTANT", 
        position: [number, number] = [0, 0],
        simulator: any = null,
        maxValue = 1
    ) {
        super(id, type, position);
        this.offset = [-60, -50];
        if(!simulator) {
            this.simulator = EventDrivenSimulator.getInstance();
        } else {
            this.simulator = simulator;
        }
        //this.inputs = []; // 常量输入没有输入引脚
        this.inputs.splice(0, this.inputs.length); // 清空输入引脚
        this.inputCount = 0; 
        this.inputInverted.splice(0, this.inputInverted.length); // 清空输入反转状态
        //this.outputs = [0]; // 初始输出值为0
        this.outputs.splice(0, this.outputs.length, 0); 
        this.bitWidth = 1; // 默认1位
        this.currentValue = 0;
        this.updateMaxValue();
        this.maxValue = maxValue;
    }

    // 实现基类抽象方法
    compute(): number[] {
        // 对于常量输入，直接返回当前值
        return [this.currentValue];
    }

    // 实现基类抽象方法（常量输入没有输入引脚，所以空实现）
    changeInput(idx: number, v: number): number[] {
        // 输入引脚的改变会导致电路的改变
        this.outputs.splice(0, this.outputs.length, v); // 更新输出
        this.simulator.enqueue(this.id, 0, this.outputs[0]);
        this.simulator.processQueue();
        return this.outputs;
    }

    // 设置常量的值（十进制）
    setValue(value: number): void {
        if (value < 0) {
            // this.outputs = [-2]; // 错误状态
            this.outputs.splice(0, this.outputs.length, -2);
            return;
        }

        if (value > this.maxValue) {
            // this.outputs = [-2]; // 超出范围错误
            this.outputs.splice(0, this.outputs.length, -2);
            return;
        }

        this.currentValue = value;
        //this.outputs = [value]; // 单值输出
        this.outputs.splice(0, this.outputs.length, value); 
    }

    // 获取当前值
    getValue(): number {
        return this.currentValue;
    }

    // 获取二进制表示
    getBinaryValue(): number[] {
        const binaryStr = this.currentValue.toString(2);
        const paddedStr = binaryStr.padStart(this.bitWidth, '0');
        return paddedStr.split('').map(bit => parseInt(bit, 10));
    }

    // 覆盖基类方法：设置位数
    setBitWidth(bitWidth: number): void {
        if (bitWidth > 0 && bitWidth !== this.bitWidth) {
            super.setBitWidth(bitWidth);
            this.updateMaxValue();
            
            // 调整当前值以适应新的位数
            if (this.currentValue > this.maxValue) {
                this.currentValue = this.maxValue;
                //this.outputs = [this.currentValue];
                this.outputs.splice(0, this.outputs.length, this.currentValue);
            }
        }
    }

    // 更新最大可表示值
    private updateMaxValue(): void {
        this.maxValue = Math.pow(2, this.bitWidth) - 1;
    }
}