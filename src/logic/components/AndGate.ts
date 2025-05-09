import { BaseComponent } from "../BaseComponent";
export class AndGate extends BaseComponent{
    constructor(id: number, type: String, position:[number, number] = [0,0],  pinPosition = []){
        super(id, type, position, pinPosition);
        this.inputs = [-1, -1];  // 输入引脚默认两个
        this.outputs = [-1];
    }
    compute(){   // 返回输出(int)
        let hasConnected = false;
        for(const value of this.inputs){
            if(value === -2){
                this.outputs[0] = -2;
                return this.outputs;
            }
            if(value !== -1){
                if(!hasConnected){
                    hasConnected = true;
                    this.outputs[0] = value;
                } else{
                    this.outputs[0] = this.outputs[0] & value;
                }
            }
            
        }
        if(!hasConnected) {
            this.outputs[0] = -1;
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[]{
        this.inputs[idx]=v;
        if(v===-2){
            this.outputs[0] = -2;
        }else{
            return this.compute();
        }
        // console.log(this.inputs);  //
        return this.outputs;
    }
}