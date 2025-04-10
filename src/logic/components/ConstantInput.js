import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class ConstantInput extends BaseComponent {
    constructor({ id, name, position, value = 0, bitWidth = 1, height = 30, width = 30 } = {}) {
        super({
            id,
            type: "ConstantInput",
            name,
            inputs: [],
            outputs: [value],  // 输出固定值，默认为 0
            height,
            width,
            position,
            pinPosition: []  // 根据需要可添加引脚位置
        });

        // 新增的位宽属性
        this.bitWidth = bitWidth;

        // 根据位宽限制常量值的范围
        this.maxValue = Math.pow(2, this.bitWidth) - 1;
    }

    // 固定输出值，无需重新计算
    compute() {
        return this.outputs;  // 始终返回固定的输出
    }

    // 修改常量值的方法，确保值不超过最大范围
    changeInput(value) {
        // 限制值的范围为 0 到 2^n - 1
        this.outputs[0] = Math.max(0, Math.min(value, this.maxValue));
        return this.outputs;
    }

    // 修改位宽后，重新计算最大值
    setBitWidth(newBitWidth) {
        this.bitWidth = newBitWidth;
        this.maxValue = Math.pow(2, this.bitWidth) - 1;
    }
}
