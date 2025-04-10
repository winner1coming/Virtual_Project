import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";
export class AndGate extends BaseComponent{
    compute(){
        let hasConnected = false;
        for(const value of this.inputs){
            if(value===SignalState.LOW){
                this.outputs=[SignalState.LOW];
                return this.outputs;
            }else if(!hasConnected && value === SignalState.HIGH){
                hasConnected = true;
            }
        }
        if(!hasConnected) {
            this.outputs = [SignalState.DISCONNECTED]
        }
        else this.outputs = [SignalState.HIGH];
        return this.outputs;
    }
    changeInput(index, v){
        this.inputs[index]=v;
        if(v===SignalState.LOW){
            this.outputs = [SignalState.LOW];
            return this.outputs;
        }
        return this.compute();
    }
}