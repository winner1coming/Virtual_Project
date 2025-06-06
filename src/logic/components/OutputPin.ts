import { BaseComponent } from "../BaseComponent";

export class OutputPin extends BaseComponent {
    private binaryValue: number[]; // 存储当前二进制值

    constructor(
        id: number, 
        type: string = "OutputPin", 
        position: [number, number] = [0, 0], 
        pinPosition = []
    ) {
        super(id, type, position, pinPosition);
        this.inputs = [-1]; // 默认一个输入引脚，初始未连接
        this.outputs = [];  // 输出引脚本身不产生输出
        this.bitCount = 4;  // 默认4位
        this.height = 20;
        this.width = 20;
        this.binaryValue = new Array(this.bitCount).fill(0); // 内部二进制表示
    }

    // 实现基类抽象方法：改变输入引脚值
    changeInput(idx: number, v: number): number[] {
        if (idx >= 0 && idx < this.inputs.length) {
            this.inputs[idx] = v;
            this.updateBinaryValue();
        }
        return this.outputs; // 始终返回空数组
    }

    // 实现基类抽象方法：计算逻辑
    compute(): number[] {
        this.updateBinaryValue();
        return this.outputs; // 输出引脚不产生输出
    }

    // 更新内部二进制值表示
    private updateBinaryValue(): void {
        const inputValue = this.inputs[0]; // 获取第一个输入引脚的值

        if (inputValue === -1) { // 未连接状态
            this.binaryValue = new Array(this.bitCount).fill(0);
            return;
        }

        if (inputValue === -2) { // 错误状态
            this.binaryValue = new Array(this.bitCount).fill(0); // 或用其他方式表示错误
            return;
        }

        // 将输入值转换为二进制数组
        const binaryStr = Math.max(0, inputValue).toString(2); // 确保非负数
        const paddedBinaryStr = binaryStr.padStart(this.bitCount, '0');
        
        // 处理位数超出情况
        if (paddedBinaryStr.length > this.bitCount) {
            this.binaryValue = new Array(this.bitCount).fill(1); // 溢出时全部置1
        } else {
            this.binaryValue = paddedBinaryStr.slice(-this.bitCount) // 取最低有效位
                .split('')
                .map(bit => parseInt(bit, 10));
        }
    }

    // 获取当前二进制值（副本）
    getBinaryValue(): number[] {
        return [...this.binaryValue];
    }

    // 获取当前十进制值
    getDecimalValue(): number {
        return this.binaryValue.reduce((acc, bit, index) => {
            return acc + (bit << (this.bitCount - 1 - index));
        }, 0);
    }

    // 覆盖基类方法：设置位数
    setBitCount(bitCount: number): void {
        if (bitCount > 0 && bitCount !== this.bitCount) {
            super.setBitCount(bitCount); // 调用基类方法
            const oldValue = this.getDecimalValue();
            this.binaryValue = new Array(bitCount).fill(0);
            
            // 转换旧值到新位数
            if (this.inputs[0] >= 0) { // 只处理有效输入
                this.inputs[0] = oldValue;
                this.updateBinaryValue();
            }
        }
    }

    // 覆盖基类方法：改变输入引脚数量（OutputPin应保持1个输入）
    changeInputPinCount(num: number): void {
        if (num !== 1) {
            console.warn("OutputPin should have exactly 1 input pin");
        }
        super.changeInputPinCount(1); // 强制保持1个输入
    }
}