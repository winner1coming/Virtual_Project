import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";
// Separator 类继承自 BaseComponent
export class Separator extends BaseComponent {
    constructor({ bitLength = 4, multiPinCount = 4, id = null, name = null, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'Separator', inputs: [], outputs: [], height: 20, width: 20, position, pinPosition: [] });

        this.bitLength = bitLength;  // 单引脚输入的位数
        this.multiPinCount = multiPinCount;  // 默认多引脚的数量为4
        this.situation = 1;  // 默认状态：1 表示单引脚输入，多引脚输出；2 表示多引脚输入，单引脚输出

        // 初始化输入引脚（如果是状态1，则只有一个输入引脚）
        this.inputs = Array(bitLength).fill({ name: 'Pin0', value: -1 });  // 单引脚，初始化为-1

        // 初始化输出引脚（如果是状态2，输出引脚个数为多引脚数量）
        this.outputs = [];
        for (let i = 0; i < multiPinCount; i++) {
            this.outputs.push({ name: `Pin${i}`, value: -1 });  // 输出引脚未连接时，值为-1
        }
    }

    // 设置状态
    setSituation(situation) {
        if (situation === 1 || situation === 2) {
            this.situation = situation;
            this.compute();  // 状态改变后需要重新计算
        }
    }

    // 改变多引脚的数量
    changeMultiPinCount(newCount) {
        if (newCount < 0 || newCount > 32) {
            console.error("Pin count must be between 0 and 32.");
            return;
        }

        this.multiPinCount = newCount;
        // 更新 outputs 数组的长度
        this.outputs = [];
        for (let i = 0; i < newCount; i++) {
            this.outputs.push({ name: `Pin${i}`, value: -1 });  // 设置新的输出引脚
        }
    }

    // 计算状态
    compute() {
        if (this.situation === 1) {
            // 状态1：单引脚输入，多引脚输出
            // 将单引脚的n位输入值分解成n个输出引脚的值
            if (this.inputs[0].value !== -1) {
                const inputValue = this.inputs[0].value.toString(2).padStart(this.bitLength, '0');  // 确保有bitLength位
                for (let i = 0; i < this.bitLength; i++) {
                    this.outputs[i].value = inputValue[i] === '1' ? 1 : 0;  // 将每一位的值分配给输出引脚
                }
            }
        } else if (this.situation === 2) {
            // 状态2：多引脚输入，单引脚输出
            // 将多个输入引脚的值合并成一个n位的输出值
            let combinedValue = 0;
            for (let i = 0; i < this.multiPinCount; i++) {
                if (this.inputs[i].value !== -1) {
                    combinedValue |= (this.inputs[i].value << i);  // 合并多位输入值
                }
            }
            this.inputs[0].value = combinedValue;  // 设置单引脚输出值
        }
    }

    // 获取当前的输入引脚值
    getInputPinValue() {
        return this.inputs[0].value;
    }

    // 获取当前的输出引脚值
    getOutputPins() {
        return this.outputs.map(output => `${output.name}:${output.value}`).join(', ');
    }
}

