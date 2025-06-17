import { BaseComponent } from "../BaseComponent";
export class AndGate extends BaseComponent{
    constructor(id: number, type: String, position:[number, number] = [0,0],  pinPosition = []){
        super(id, type, position, pinPosition);
        ////is.inputs = [-1, -1];  
    }
    compute(){   // 返回输出(int)
        let hasConnected = false;
        for(const value of this.inputs){
            if(value === -2){
                this.outputs.splice(0, this.outputs.length, -2); // 输出引脚错误
                return this.outputs;
            }
            if(value !== -1){
                if(!hasConnected){
                    hasConnected = true;
                    //this.outputs[0] = value;
                    this.outputs.splice(0, 1, value); // 替换outputs[0]的值
                } else{
                    //this.outputs[0] = this.outputs[0] & value;
                    this.outputs.splice(0, 1, this.outputs[0] & value); // 替换outputs[0]的值
                }
            }
            
        }
        if(!hasConnected) {
            this.outputs.splice(0, this.outputs.length, -1); // 如果没有连接任何输入，则输出-1
        }
        return this.outputs;
    }

    changeInput(idx: number, v: number): number[]{
        //this.inputs[idx]=v;
        this.inputs.splice(idx, 1, v); // 替换idx位置的值
        if(v===-2){
            this.outputs.splice(0, this.outputs.length, -2); // 输出引脚错误
        }else{
            return this.compute();
        }
        // console.log(this.inputs);  //
        return this.outputs;
    }
}