// 电路传输整型，-1表示未连接，-2表示错误
export abstract class BaseComponent{
    id: number;
    type: String;
    name: String;
    inputs: number[];
    output: number;
    bitCount: number;
    height: number;
    width: number;
    position: [number, number];
    pinPosition: Array<[number, number]>;
    constructor(id = null, type = null, name = null, inputs = [], output = null, bitCount = 1, height = 0, width = 0, position:[number, number] = [0,0], pinPosition = null) {
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
    };
    abstract compute(): number;   // 调用后返回output
    abstract changeInput(idx: number, v: number): number;  // 改变某一个引脚的电平，返回output
    changeInputPinCount(num: number){
        this.inputs = Array(num).fill(-1);
    }
    getInputPinCount(): number{
        return this.inputs.length;
    }
    getOutput(): number{
        return this.output;
    }
}