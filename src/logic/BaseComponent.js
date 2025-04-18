// 电路传输整型，-1表示未连接，-2表示错误
export class BaseComponent{
    // id;
    // type;
    // name;
    // inputs;
    // outputs;
    // height;
    // width;
    // position;
    // pinPosition;
    constructor({ id = null, type = null, name = null, inputs = [], output = null, bitCount = 1, height = 0, width = 0, position = { x: 0, y: 0 }, pinPosition = [] } = {}) {
        this.id = id;
        this.type = type;
        this.name = name;
        this.inputs = inputs;
        this.output = output;
        this.bitCount = bitCount;
        this.height = height;
        this.width = width;
        this.position = position;
        this.pinPosition = pinPosition;
    }
    compute(){}   // 调用后返回output是否改变，改变了则返回true
    changeInput(index, v){}   // 改变某一个引脚的电平，返回outputs
    getInputPinCount(){
        return this.inputs.length;
    }
    getOutput(){
        return this.output;
    }
}