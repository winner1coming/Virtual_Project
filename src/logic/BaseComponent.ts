import { reactive } from "vue";
import { calcInputYs } from "@/logic/utils/useGateLayout";
import { EventDrivenSimulator } from "./Simulator";
import { useProjectStore } from "@/store/ProjectStore";
import eventBus from "@/modules/useEventBus";

// 电路传输整型，-1表示未连接，-2表示错误
export abstract class BaseComponent{
  id: number;
  type: string;
  name: string;
  inputs: number[];
  inputCount: number; // 输入引脚数量
  inputInverted: boolean[]; // 输入引脚是否取反   
  outputs: number[];
  bitWidth: number;
  scale: number; // 缩放比例
  position: [number, number];
  offset: [number, number];
  inputPinPosition: Array<[number, number]>;   // 默认为2
  outputPinPosition: Array<[number, number]>;  // 默认为2
  direction: string; // 组件的方向，'east', 'west', 'north', 'south'
  simulator!: EventDrivenSimulator; // 关联的模拟器实例

  constructor(id: number, type: string, position:[number, number] = [0,0]) {
    // 子类初始化构造记得要调用initInputPin()（initOutputPin在outputPin不为1时调用）（会修改一些数组的长度，也会重新计算引脚位置）
    this.id = id;
    this.type = type;
    this.name = "";   
    
    this.inputs = reactive([-1, -1]);     // 默认2个输入，如果不是，子类需要在构造函数中初始化
    this.inputCount = 2; // 默认2个输入
    this.inputInverted = reactive([false, false]);   // 默认两个引脚

    this.outputs = reactive([-1]);  // 输出初始值为-1 未连接
    this.bitWidth = 1;
    this.scale = 0.25;    
    this.position = reactive(position); 
    this.inputPinPosition =  reactive([[0,0], [0,0]]);  // 默认只有两个输入引脚
    this.outputPinPosition = reactive([[0,0]]); // 默认只有一个输出引脚
    this.direction = 'east';  // 默认方向为东
    this.offset = [0,0];
  };

  // 计算内部逻辑，调用后返回outputs
  abstract compute(): number[];   
  // 改变某一个引脚的电平，返回outputs
  abstract changeInput(idx: number, v: number): number[]; 

  //#region setters
  // 改变名字
  setName(name: string){
    this.name = name;
  }

  // 改变位宽
  setBitWidth(bitWidth: number){
    this.bitWidth = bitWidth;
    useProjectStore().getCurrentProject().hasChanged = true;
  }

  // 改变position
  setPosition(position: [number, number]) {
    this.position[0] = position[0]; 
    this.position[1] = position[1];
  }

  setDirection(direction: string){
    this.direction = direction;
    this.updatePinPosition();
    eventBus.emit('updatePinPosition', {id: this.id}); 
  }

  // 设置缩放比例
  setScale(scale: number) {
    this.scale = scale;
  }
  //#endregion setters

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
          this.direction==='east'? 92:497,
          pin,
        ];
    }));
    // 修改输出
    this.outputPinPosition = this.outputPinPosition.map(pin => {
      return [
        this.direction==='east'? 497:92,
        0 + 288,
      ];
    });
  }
  
  initOutputPin(num: number){
    this.outputs.splice(0, this.outputs.length, ...Array(num).fill(-1));
    this.outputPinPosition.splice(0, this.outputPinPosition.length, ...Array(num).fill([0, 0])); 
    // 更新输出引脚位置
    this.updatePinPosition(); 
  }

  // 初始化输入引脚，不检查连接
  initInputPin(num: number){
    this.inputCount = num;
    this.inputs.splice(0, this.inputs.length, ...Array(num).fill(-1));    // 将输入全部置-1
    this.inputPinPosition.splice(0, this.inputPinPosition.length, ...Array(num).fill([0, 0]));
    this.inputInverted.splice(0, this.inputInverted.length, ...Array(num).fill(false)); // 初始化输入取反状态

    this.updatePinPosition();
  }

  // 会清空输入与引脚的取反状态
  changeInputPinCount(num: number){
    // 取消与前驱的连接
    this.simulator.disconnectPredecessors(this.id);
    for(let i = 0; i < this.outputs.length; i++){
      this.outputs.splice(i, 1, -1); 
      this.simulator.processOutputChange(this.id, i, -1); 
    }
    this.initInputPin(num); 
    useProjectStore().getCurrentProject().hasChanged = true;
    eventBus.emit('updatePinPosition', {id: this.id}); 
  }
  
  changeOutputPinCount(num: number){
    // 取消与后继的连接
    this.simulator.disconnectSuccessors(this.id);
    this.initOutputPin(num); 
    useProjectStore().getCurrentProject().hasChanged = true;
    eventBus.emit('updatePinPosition', {id: this.id}); 
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
    useProjectStore().getCurrentProject().hasChanged = true;
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
        x: (this.inputPinPosition[i][0]+this.offset[0])*this.scale,
        y: (this.inputPinPosition[i][1]+this.offset[1])*this.scale,
      });
        
    }  

    for(let i = 0; i < this.outputPinPosition.length; i++){
      result.ports.push({
        id: i + this.getInputPinCount(),
        x: (this.outputPinPosition[i][0]+this.offset[0])*this.scale, 
        y: (this.outputPinPosition[i][1]+this.offset[1])*this.scale,
      });
    }
    return result;
  }
}