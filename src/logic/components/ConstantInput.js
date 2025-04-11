import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class ConstantInput extends BaseComponent {
    constructor({ id, name, position, value = 0, bitWidth = 1, height = 30, width = 30 } = {}) {
        super({
            id,
            type: "ConstantInput",
            name,
            inputs: [],
            outputs: [value],
            height,
            width,
            position,
            pinPosition: [] 
        });
        this.bitWidth = bitWidth;
        this.maxValue = Math.pow(2, this.bitWidth) - 1;
    }

    compute() {
        return this.outputs; 
    }

    changeInput(value) {
        this.outputs[0] = Math.max(0, Math.min(value, this.maxValue));
        return this.outputs;
    }

    setBitWidth(newBitWidth) {
        this.bitWidth = newBitWidth;
        this.maxValue = Math.pow(2, this.bitWidth) - 1;
    }
}
