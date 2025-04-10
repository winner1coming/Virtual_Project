// InputPin类，继承自BaseComponent
import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";


export class InputPin extends BaseComponent {
    constructor({ id = null, name = null, bitLength = 4, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'InputPin', inputs: [], outputs: [("output",SignalState.DISCONNECTED)], height: 20, width: 20, position, pinPosition: [] });
        this.bitLength = bitLength; // 二进制输入位数
        this.value = Array(bitLength).fill(0); // 默认为 0000 或根据位数初始化
    }

    // 获取当前的二进制值
    getValue() {
        return this.value.join('');
    }

    // 设置某一位的值
    setBit(index, value) {
        if (index >= 0 && index < this.bitLength) {
            this.value[index] = value ? 1 : 0;
            this.compute(); // 更新output
        }
    }

    // 改变位数，并初始化所有位为0
    changeBitLength(newBitLength) {
        if (newBitLength !== this.bitLength) {
            // 将所有位初始化为0
            this.value = Array(newBitLength).fill(0);
            this.bitLength = newBitLength; // 更新位数
            this.compute(); // 更新output
        }
    }

    // 计算当前输入下的outputs，InputPin将outputs作为整个输入值
    compute() {
        // 更新outputs为value，后续计算时会根据每一位的值来判断电平
        this.outputs = [...this.value];
    }
}
