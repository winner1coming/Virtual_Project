import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class OutputPin extends BaseComponent {
    constructor({ id = null, name = null, bitCount = 4, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'OutputPin', inputs: [], outputs,bitCount, height: 20, width: 20, position, pinPosition: [] });
        this.bitCount = bitCount;
        this.value = Array(bitCount).fill(0);
        this.state = 'normal';
    }

    setInputValue(value) {
        this.outputs = value;
        //先将值转化为二进制的内容
        const binaryStr = value.toString(2);
        //不足的补0操作
        const paddedBinaryStr = binaryStr.padStart(this.bitCount,'0');
        if(paddedBinaryStr.length !== this.bitCount){
            this.state = 'error';
            return;
        }
        //转化为二进制数组的形式
        this.value = paddedBinaryStr.split('').map(bit => parseInt(bit, 10));
    }

    getValue() {
        return this.value;
    }

    compute() {
        this.outputs;
    }
}
