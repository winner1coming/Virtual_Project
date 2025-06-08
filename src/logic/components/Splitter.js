import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";
// Separator 类继承自 BaseComponent
export class Separator extends BaseComponent {
    constructor({ bitLength = 4, multiPinCount = 4, id = null, name = null, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'Separator', inputs: [], outputs, height: 20, width: 20, position, pinPosition: [] });

        this.bitLength = bitLength;  
        this.multiPinCount = multiPinCount;
        // 默认状态：1 表示单引脚输入，多引脚输出；2 表示多引脚输入，单引脚输出 
        this.situation = 1;  
        this.inputs = Array(bitLength).fill({ name: 'Pin0', value: SignalState.DISCONNECTED }); 
        this.outputs;
        for (let i = 0; i < multiPinCount; i++) {
            this.outputs.push({ name: `Pin${i}`, value: SignalState.DISCONNECTED });
        }
    }

    setSituation(situation) {
        if (situation === 1 || situation === 2) {
            this.situation = situation;
            this.compute();
        }
    }

    changeMultiPinCount(newCount) {
        if (newCount < 0 || newCount > 32) {
            console.error("Pin count must be between 0 and 32.");
            return;
        }

        this.multiPinCount = newCount;
        this.outputs = [];
        for (let i = 0; i < newCount; i++) {
            this.outputs.push({ name: `Pin${i}`, value: SignalState.DISCONNECTED }); 
        }
    }

    compute() {
        if (this.situation === 1) {
            // 状态1：单引脚输入，多引脚输出
            if (this.inputs[0].value !== -1) {
                const inputValue = this.inputs[0].value.toString(2).padStart(this.bitLength, '0'); 
                for (let i = 0; i < this.bitLength; i++) {
                    this.outputs[i].value = inputValue[i] === '1' ? 1 : 0; 
                }
            }
        } else if (this.situation === 2) {
            // 状态2：多引脚输入，单引脚输出
            let combinedValue = 0;
            for (let i = 0; i < this.multiPinCount; i++) {
                if (this.inputs[i].value !== -1) {
                    combinedValue |= (this.inputs[i].value << i);
                }
            }
            this.inputs[0].value = combinedValue;
        }
    }

    getInputPinValue() {
        return this.inputs[0].value;
    }

    getOutputPins() {
        return this.outputs.map(output => `${output.name}:${output.value}`).join(', ');
    }
}