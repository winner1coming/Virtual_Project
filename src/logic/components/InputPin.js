// InputPin类，继承自BaseComponent
import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";


export class InputPin extends BaseComponent {
    constructor({ id = null, name = null, bitLength = 4, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'InputPin', inputs: [], outputs: [("output",SignalState.DISCONNECTED)], height: 20, width: 20, position, pinPosition: [] });
        this.bitLength = bitLength;
        this.value = Array(bitLength).fill(0);
    }

    getValue() {
        return this.value.join('');
    }

    setBit(index, value) {
        if (index >= 0 && index < this.bitLength) {
            this.value[index] = value ? 1 : 0;
            this.compute();
        }
    }

    changeBitLength(newBitLength) {
        if (newBitLength !== this.bitLength) {
            this.value = Array(newBitLength).fill(0);
            this.bitLength = newBitLength;
            this.compute();
        }
    }

    compute() {
        // 更新outputs为value，后续计算时会根据每一位的值来判断电平
        this.outputs = [...this.value];
    }
}
