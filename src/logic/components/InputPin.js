// InputPin类，继承自BaseComponent
import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";


export class InputPin extends BaseComponent {
    constructor({ id = null, name = null, bitCount = 1, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'InputPin', inputs: [], outputs, bitCount,height: 20, width: 20, position, pinPosition: [] });
        this.bitCount = bitCount;
        this.inputs = new Array(bitCount).fill(0);
    }
    //二进制数组变成十进制数，为了输出
    binaryToDecimal() {
        return this.inputs.reduce(
          (acc, bit, index) => acc + (bit << (this.bitCount - 1 - index)), 
          0
        );
    }

    setBit(index) {
        if (index >= 0 && index < this.bitCount) {
            this.inputs[index] = this.inputs[index] == 0 ? 1 : 0;
            this.compute();
        }
    }

    changeBitLength(newBitLength) {
        if (newBitLength !== this.bitCount) {
            this.inputs = new Array(newBitLength).fill(0);
            this.bitCount = newBitLength;
            this.compute();
        }

        //如果想要这个保留原来的逻辑可换成这个
        /*
            if(newBitLength !== this.bitCount){
                const newInputs = new Array(newBitLength).fill(0);
                for(let i = 0; i < Math.min(this..bitCount, newBitLength); i++){
                    newInputs[i] = this.inputs[i];
                }
                this.inputs = newInputs;
                this.bitCount = newBitLength;
                this.compute();
            }
        */
    }

    compute() {
        // 更新outputs为value，后续计算时会根据每一位的值来判断电平
        const decimalValue = this.binaryToDecimal();
        //考虑将output的一些属性封装起来，比如value、还有name（后续对于这个隧道的实现）
        this.outputs = decimalValue;
    }

    getValue(){
        return this.outputs;
    }
}
