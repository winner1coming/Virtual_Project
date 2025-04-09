import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";
export class AndGate extends BaseComponent{
    compute(){
        for(const value of this.inputs){
            if(value===SignalState.DISCONNECTED){
                this.outputs=[SignalState.DISCONNECTED];
                return this.outputs;
            }if(value===SignalState.LOW){
                this.outputs=[SignalState.LOW];
                return this.outputs;
            }
        }
       this.outputs = [SignalState.HIGH];
        return this.outputs;
    }
    changeInput(index, v){
        this.inputs[index]=v;
        if(v===SignalState.DISCONNECTED){
            this.outputs=[SignalState.DISCONNECTED];
            return this.outputs;
        }
        if(v===SignalState.LOW){
            this.outputs = [SignalState.LOW];
            return this.outputs;
        }
        this.outputs = [SignalState.HIGH];
        return this.outputs;
    }
}