import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class NotGate extends BaseComponent {
    compute() {
        // 非门只有一个输入
        if (this.inputs.length !== 1) {
            throw new Error("NotGate requires exactly one input.");
        }
        const input = this.inputs[0];
        if (input === SignalState.DISCONNECTED) {
            this.outputs = [SignalState.DISCONNECTED];
        } else if (input === SignalState.HIGH) {
            this.outputs = [SignalState.LOW];
        } else if (input === SignalState.LOW) {
            this.outputs = [SignalState.HIGH];
        }

        return this.outputs;
    }

    changeInput(index, v) {
        if (index !== 0) {
            throw new Error("NotGate only supports a single input at index 0.");
        }
        this.inputs[index] = v;
        if (v === SignalState.DISCONNECTED) {
            this.outputs = [SignalState.DISCONNECTED];
        } else if (v === SignalState.HIGH) {
            this.outputs = [SignalState.LOW];
        } else if (v === SignalState.LOW) {
            this.outputs = [SignalState.HIGH];
        }
        return this.outputs;
    }
}