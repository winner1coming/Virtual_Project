import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class OutputPin extends BaseComponent {
    constructor({ id = null, name = null, bitLength = 4, position = { x: 0, y: 0 } } = {}) {
        super({ id, name, type: 'OutputPin', inputs: [("input",SignalState.DISCONNECTED)], outputs: [], height: 20, width: 20, position, pinPosition: [] });
        this.bitLength = bitLength;
        this.value = Array(bitLength).fill(0);
    }

    setInputValue(value) {
        if (value.length === this.bitLength) {
            this.value = value.split('').map(bit => (bit === '1' ? 1 : 0)); 
            this.compute();
        } else {
            console.error(`Value length must match the bit length of ${this.bitLength}`);
        }
    }

    getValue() {
        return this.value.join('');
    }

    compute() {
        this.outputs = [...this.value];
    }
}
