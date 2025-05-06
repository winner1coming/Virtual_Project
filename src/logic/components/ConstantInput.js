import { BaseComponent } from "../BaseComponent";
import { SignalState } from "../SignalState";

export class ConstantInput extends BaseComponent {
    constructor({ id, name, position, value = 0, bitWidth = 1, height = 30, width = 30 , bitCount = 1} = {}) {
        super({
            id,
            type: "ConstantInput",
            name,
            inputs: [],
            outputs,
            bitCount,
            height,
            width,
            position,
            pinPosition: {}
        });
        this.maxValue = Math.pow(2, this.bitCount) - 1;
        this.state = 'normal';
    }

    compute() {
        return this.outputs; 
    }
    //用户通过直接输入十进制数来改变常量的输入
    getChange(value){
        //如果超过当前位数能表示的，返回错误状态
        if(value > this.maxValue){
            this.state = 'error';
            return;
        }
        this.outputs = value;
    }
    //设置位数
    setBitWidth(newBitWidth) {
        this.bitCount = newBitWidth;
        this.maxValue = Math.pow(2, this.bitCount) - 1;
    }
}
