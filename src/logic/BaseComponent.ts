// 电路传输整型，-1表示未连接，-2表示错误
export abstract class BaseComponent{
    id: number;
    type: String;
    name: String;
    inputs: number[];
    outputs: number[];
    bitCount: number;
    height: number;
    width: number;
    position: [number, number];
    pinPosition: Array<[number, number]>;

    constructor(id: number, type: String, position:[number, number] = [0,0],  pinPosition = []) {
        this.id = id;
        this.type = type;
        this.name = "";    // todo
        this.inputs = [];     // 在子类中需要详细初始化！
        this.outputs = [-1];  // 输出初始值为-1 未连接
        this.bitCount = 1;
        this.height = 1;   // todo
        this.width = 1;
        this.position = position;
        this.pinPosition = pinPosition;
    };

    abstract compute(): number[];   // 调用后返回outputs
    abstract changeInput(idx: number, v: number): number[];  // 改变某一个引脚的电平，返回outputs

    setName(name: String){
        this.name = name;
    }

    setBitCount(bitCount: number){
        this.bitCount = bitCount;
    }

    changeInputPinCount(num: number){
        this.inputs = Array(num).fill(-1);    // 将输入全部置-1
    }

    getInputPinCount(): number{
        return this.inputs.length;
    }
    getOutputs(): number[]{
        return this.outputs;
    }
}