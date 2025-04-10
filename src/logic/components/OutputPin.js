import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

// OutputPin类，继承自BaseComponent
export class OutputPin extends BaseComponent {
    constructor({ id = null, name = null, bitLength = 4, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'OutputPin', inputs: [("input",SignalState.DISCONNECTED)], outputs: [], height: 20, width: 20, position, pinPosition: [] });
        this.bitLength = bitLength;  // 设置初始位数
        this.value = Array(bitLength).fill(0);  // 默认值为 0000 (位数为4)
    }

    // 设置输入的value并更新输出
    setInputValue(value) {
        if (value.length === this.bitLength) {
            this.value = value.split('').map(bit => (bit === '1' ? 1 : 0));  // 更新value
            this.compute();  // 更新输出
        } else {
            console.error(`Value length must match the bit length of ${this.bitLength}`);
        }
    }

    // 获取当前的二进制值
    getValue() {
        return this.value.join('');
    }

    // 计算当前输入下的outputs，OutputPin的outputs由setInputValue决定
    compute() {
        this.outputs = [...this.value];  // 更新outputs为当前的value
    }
}
