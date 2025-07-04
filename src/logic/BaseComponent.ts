import { reactive } from "vue";
import { calcInputYs } from "@/logic/utils/useGateLayout";
import { EventDrivenSimulator } from "./Simulator";
import { SubSimulator } from "./SubSimulator";

// 电路传输整型，-1表示未连接，-2表示错误
export abstract class BaseComponent{
  id: number;
  type: String;
  name: String;
  inputs: number[];
  inputCount: number; // 输入引脚数量
  inputInverted: boolean[]; // 输入引脚是否取反   todo 内部逻辑未实现!
  outputs: number[];
  bitWidth: number;
  // height: number;
  // width: number;
  scale: number; // 缩放比例
  position: [number, number];
  offset: [number, number];
  inputPinPosition: Array<[number, number]>;   // todo! 默认为2，部分特殊文件中的这个还没改
  //inputYs: number[]; // 输入引脚的y坐标
  outputPinPosition: Array<[number, number]>;  // todo! 默认为2，部分特殊文件中的这个还没改
  direction: string; // 组件的方向，'east', 'west', 'north', 'south'
  simulator!: EventDrivenSimulator | SubSimulator; // 关联的模拟器实例

  constructor(id: number, type: String, position:[number, number] = [0,0]) {
    // 子类初始化构造记得要调用changeInputPinCount()（changeOutputPinCount在outputPin不为1时调用）（会修改一些数组的长度，也会重新计算引脚位置）
    this.id = id;
    this.type = type;
    this.name = "";   
    
    this.inputs = reactive([-1, -1]);     // 默认2个输入，如果不是，子类需要在构造函数中初始化
    this.inputCount = 2; // 默认2个输入
    this.inputInverted = reactive([false, false]);   // 默认两个引脚

    this.outputs = reactive([-1]);  // 输出初始值为-1 未连接
    this.bitWidth = 1;
    this.scale = 1;    
    this.position = reactive(position); // 将 position 包装为 reactive
    this.inputPinPosition =  reactive([[0,0], [0,0]]);  // 默认只有两个输入引脚
    this.outputPinPosition = reactive([[0,0]]); // 默认只有一个输出引脚
    this.direction = 'east';  // 默认方向为东
    this.offset = [0,0];

    // this.simulator = EventDrivenSimulator.getInstance(); 

    // this.changeInputPinCount(2); // 初始化输入引脚数量为2 
    // this.inputYs = calcInputYs(this.inputCount); // 计算输入引脚的y坐标
  };

  // 计算内部逻辑，调用后返回outputs
  abstract compute(): number[];   

  // 改变某一个引脚的电平，返回outputs
  abstract changeInput(idx: number, v: number): number[];  // 改变某一个引脚的电平，返回outputs
  // // 取反（只给位宽为1的输入引脚用）
  // invertInput(idx: number): void {
  //     this.inputs[idx] = this.inputs[idx] === 1 ? 0 : 1;
  //     this.compute();  // 更新outputs
  // } 

  // #region Setters
  // 改变名字
  setName(name: String){
    this.name = name;
  }
  // 改变位宽
  setBitWidth(bitWidth: number){
    this.bitWidth = bitWidth;
    EventDrivenSimulator.getInstance().checkComponentConnections(this.id);
  }
  // 改变position
  setPosition(position: [number, number]) {
    this.position[0] = position[0]; 
    this.position[1] = position[1];
  }
  // 设置缩放比例
  setScale(scale: number) {
    this.scale = scale;
  }
  // #endregion Setters

  // #region 引脚
  // 更新引脚位置
  updatePinPosition(): void{
    // 适用于与门、或门
    // 修改输入
    const inputYs = calcInputYs(this.inputCount);
    this.inputPinPosition.splice(0, this.inputPinPosition.length,
      ...inputYs.map((pin, index): [number, number] => {
        return [
          // 0 + 92 * this.scale,
          // 0 + pin * this.scale,
          92,
          pin,
        ];
    }));
    // 修改输出
    this.outputPinPosition = this.outputPinPosition.map(pin => {
      return [
        0 + 497,
        0 + 288,
      ];
    });
  
  }
  // 会清空输入与引脚的取反状态
  changeInputPinCount(num: number){
    this.inputCount = num;
    this.inputs.splice(0, this.inputs.length, ...Array(num).fill(-1));    // 将输入全部置-1
    this.inputInverted.splice(0, this.inputInverted.length, ...Array(num).fill(false)); // 初始化输入取反状态

    this.updatePinPosition();
    // 取消与前驱的连接
    this.simulator.disconnectPredecessors(this.id);
    for(let i = 0; i < this.outputs.length; i++){
      this.outputs.splice(i, 1, -1); 
      this.simulator.processOutputChange(this.id, i, -1); 
    }
  }
  changeOutputPinCount(num: number){
    this.outputs.splice(0, this.outputs.length, ...Array(num).fill(-1));

    // 取消与后继的连接
    this.simulator.disconnectSuccessors(this.id);
    this.updatePinPosition();
  }
  // #endregion 引脚

  changeInputInverted(idx: number){
    if(idx < 0 || idx >= this.inputCount){
      throw new Error(`Input index ${idx} out of bounds for component ${this.type}`);
    }
    this.inputInverted.splice(idx, 1, !this.inputInverted[idx]); // 切换输入取反状态

    const oldOutputs = [...this.outputs];
    this.compute();
    for(let i = 0; i < this.outputs.length; i++){
      if(this.outputs[i] !== oldOutputs[i]){
        this.simulator.processOutputChange(this.id, i, this.outputs[i]); 
      }
    }
  }

  getInputPinCount(): number{
    return this.inputCount;
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
        x: this.inputPinPosition[i][0]+this.offset[0],
        y: this.inputPinPosition[i][1]+this.offset[1],
      });
    }  

    for(let i = 0; i < this.outputs.length; i++){
      result.ports.push({
        id: i + this.getInputPinCount(),
        x: this.outputPinPosition[i][0]+this.offset[0], 
        y: this.outputPinPosition[i][1]+this.offset[1],
      });
    }
    return result;
  }
}