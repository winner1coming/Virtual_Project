import { reactive } from "vue";
// 电路传输整型，-1表示未连接，-2表示错误
export abstract class BaseComponent{
    id: number;
    type: String;
    name: String;
    inputs: number[];
    inputCount: number; // 输入引脚数量
    inputInverted: boolean[]; // 输入引脚是否取反   todo 内部逻辑未实现!
    outputs: number[];
    bitCount: number;
    height: number;
    width: number;
    scale: number; // 缩放比例
    position: [number, number];
    InputPinPosition: Array<[number, number]>;   // todo! 默认为2，部分特殊文件中的这个还没改
    OutputPinPosition: Array<[number, number]>;  // todo! 默认为2，部分特殊文件中的这个还没改
    direction: String; // 组件的方向，'east', 'west', 'north', 'south'

    constructor(id: number, type: String, position:[number, number] = [0,0],  InputPinPosition = []) {
        this.id = id;
        this.type = type;
        this.name = "";    // todo
        
        this.inputs = reactive([-1, -1]);     // 默认2个输入，如果不是，子类需要在构造函数中初始化
        this.inputCount = 2; // 默认2个输入
        this.inputInverted = reactive([false, false]);   // 默认两个引脚

        this.outputs = reactive([-1]);  // 输出初始值为-1 未连接
        this.bitCount = 1;
        this.height = 1;   // todo
        this.width = 1;
        this.scale = 1;    
        this.position = position;
        this.InputPinPosition =  reactive([[0,0], [0,0]]);  // 默认只有两个输入引脚
        this.OutputPinPosition = reactive([[0,0]]); // 默认只有一个输出引脚
        this.direction = 'east';  // 默认方向为东
    };

    abstract compute(): number[];   // 调用后返回outputs
    abstract changeInput(idx: number, v: number): number[];  // 改变某一个引脚的电平，返回outputs
    // // 取反（只给位宽为1的输入引脚用）
    // invertInput(idx: number): void {
    //     this.inputs[idx] = this.inputs[idx] === 1 ? 0 : 1;
    //     this.compute();  // 更新outputs
    // } 

    setName(name: String){
        this.name = name;
    }

    setBitCount(bitCount: number){
        this.bitCount = bitCount;
    }
    setPosition(position: [number, number]) {
        this.position = position;
        const [baseX, baseY] = position;

        // 🟢 更新输入引脚坐标
        for (let i = 0; i < this.inputCount; i++) {
            this.InputPinPosition[i] = [baseX - 20, baseY]; 
        }

        // 🟢 更新输出引脚坐标
        for (let i = 0; i < this.outputs.length; i++) {
            this.OutputPinPosition[i] = [baseX + 80, baseY]; 
        }
    }


    // 会清空输入与引脚的取反状态
    changeInputPinCount(num: number){
        this.inputCount = num;
        this.inputs.splice(0, this.inputs.length, ...Array(num).fill(-1));    // 将输入全部置-1
        this.inputInverted.splice(0, this.inputInverted.length, ...Array(num).fill(false)); // 初始化输入取反状态
        this.InputPinPosition.splice(0, this.InputPinPosition.length, ...Array(num).fill([0,0]));
    }

    changeInputInverted(idx: number){
        if(idx < 0 || idx >= this.inputCount){
            throw new Error(`Input index ${idx} out of bounds for component ${this.type}`);
        }
        this.inputInverted.splice(idx, 1, !this.inputInverted[idx]); // 切换输入取反状态
    }

    getInputPinCount(): number{
        return this.inputs.length;
    }
    getOutputs(): number[]{
        return this.outputs;
    }


    getAllPorts(){
        let result = {
            id: this.id,
            ports:[] as Array<{
                id: number,
                x: number,
                y: number
            }>
        };
        for(let i = 0; i < this.getInputPinCount(); i++){
            result.ports.push({
                id: i,
                x: this.InputPinPosition[i][0],
                y: this.InputPinPosition[i][1],
            });
        }  

        for(let i = 0; i < this.outputs.length; i++){
            result.ports.push({
                id: i + this.getInputPinCount(),
                x: this.OutputPinPosition[i][0],
                y: this.OutputPinPosition[i][1],
            });
        }
        return result;
    }
}