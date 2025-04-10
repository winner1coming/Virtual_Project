import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class OrGate extends BaseComponent {
    compute() {
        let hasConnected = false;
        for (const value of this.inputs) {
            if (value === SignalState.HIGH) {
                this.outputs = [SignalState.HIGH];
                return this.outputs;
            }else if (!hasConnected && value === SignalState.LOW) {
                hasConnected = true;
            }
        }
        if(!hasConnected) {
            this.outputs = [SignalState.DISCONNECTED]
        }
        this.outputs = [SignalState.LOW];
        return this.outputs;
    }

    changeInput(index, v) {
        this.inputs[index] = v;
        if (v === SignalState.HIGH) {
            this.outputs = [SignalState.HIGH];
            return this.outputs;
        }
        return this.compute();
    }
}