import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class NotGate extends BaseComponent {
    compute() {
        // 非门只有一个输入
        if (this.inputs.length !== 1) {
            throw new Error("NotGate requires exactly one input.");
        }
        const input = this.inputs[0];
        if (input < 0) {
            this.output = input;
        } else if (input === 1) {
            this.output = 0;
        } else if (input === 0) {
            this.output = 1;
        }

        return this.output;
    }

    changeInput(index, v) {
        if (index !== 0) {
            throw new Error("NotGate only supports a single input at index 0.");
        }
        this.inputs[index] = v;
        if (v < 0) {
            this.output = v;
        } else if (v === 1) {
            this.output = 0;
        } else if (v === 0) {
            this.output = 1;
        }
        return this.output;
    }
}