import { BaseComponent } from "../BaseComponent.js";
export class AndGate extends BaseComponent{
    // constructor(bitCount=1){this.bitCount=bitCount;}
    compute(){   // 返回输出(int)
        let hasConnected = false;
        for(const value of this.inputs){
            if(value === -2){
                this.output = -2;
                return -2;
            }
            if(value !== -1){
                if(!hasConnected){
                    hasConnected = true;
                    this.output = value;
                } else{
                    this.output = this.output & value;
                }
            }
            
        }
        if(!hasConnected) {
            this.output = -1;
        }
        return this.output;
    }

    changeInput(index, v){
        this.inputs[index]=v;
        if(v===-2){
            this.output = -2;
        }else{
            this.compute();
        }
        // console.log(this.inputs);  //
        return this.output;
    }
}