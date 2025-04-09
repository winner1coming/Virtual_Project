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
    constructor({ id = null, type = null, name = null, inputs = [], outputs = [], height = 0, width = 0, position = { x: 0, y: 0 }, pinPosition = [] } = {}) {
        this.id = id;
        this.type = type;
        this.name = name;
        this.inputs = inputs;
        this.outputs = outputs;
        this.height = height;
        this.width = width;
        this.position = position;
        this.pinPosition = pinPosition;
    }
    compute(){}   // 调用后返回outputs
    changeInput(index, v){}   // 改变某一个引脚的电平，返回outputs
}