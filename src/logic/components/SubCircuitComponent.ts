// 模拟子电路的逻辑，承 BaseComponent，内部运行子电路
import { BaseComponent } from '../BaseComponent';
import { useCircuitStore } from '@/store/CircuitStore';
import { reactive } from 'vue';
import { SubSimulator } from '../SubSimulator';
import { useProjectStore } from '@/store/ProjectStore';
import { createComponentByType } from '@/modules/useComponentType';
import { calcInputYs } from "@/logic/utils/useGateLayout";
import { Clock } from './Clock';

export class SubCircuitComponent extends BaseComponent {
  // public inputPins: number[]=[];   // 输入引脚的id
  // public outputPins: number[]=[];
  // public componentIdMap: Map<number, BaseComponent> = new Map(); // 用于映射元件的id
  public inputNames: string[] = []; // 输入引脚的名称
  public outputNames: string[] = []; // 输出引脚的名称
  public copyProjectId: number = 0;
  public projectUUID: string = "";
  public truthTable: number[][] = []; // 真值表

  constructor(
    id: number,
    type: string,
    position:[number, number] = [0, 0],
    name: string,
    projectId: number = -1,
  ) {
    super(id, type, position);
    this.name = name;
    this.offset = [-280, -280];
    if(projectId === -1) return;  // 延后创建
    this.copyProjectId = projectId;

    const circuitStore = useCircuitStore();
    const projectStore = useProjectStore();

    const projectData = projectStore.getProjectById(projectId);
    this.projectUUID = projectData.projectUUID;
    this.initInputPin(projectData.inputPins.length);
    this.initOutputPin(projectData.outputPins.length);

   
    // 计算真值表
    // 切换项目
    const oldProjectId = projectStore.selectedProjectId;
    projectStore.loadProject(projectId);
    circuitStore.changeProject(projectId);
    // 存储旧的输入，顺带存inputName
    const oldInputs = [];
    for(const inputPinId of projectData.inputPins) {
      const comp = circuitStore.getComponent(inputPinId);
      if (comp) {
        oldInputs.push(comp.getOutputs()[0]);
        this.inputNames.push(comp.name);
      } else {
        oldInputs.push(0); 
      }
    }
    // 存outputName
    for(const outputPinId of projectData.outputPins) {
      const comp = circuitStore.getComponent(outputPinId);
      if (comp) {
        this.outputNames.push(comp.name);
      } else {
        this.outputNames.push(""); // 如果没有找到组件，输出名称为空
      }
    }
    // 遍历引脚，计算真值表
    const inputCount = projectData.inputPins.length;
    const outputCount = projectData.outputPins.length;
    const totalCombinations = 1 << inputCount; // 2^inputCount
    for (let i = 0; i < totalCombinations; i++) {
      // 暂停模拟器
      circuitStore.simulator.pauseSimulator();
      // 设置输入
      for (let j = 0; j < inputCount; j++) {
        const value = (i >> j) & 1;
        circuitStore.getComponent(projectData.inputPins[j]).changeInput(0, value);
      }
      // 恢复模拟器
      circuitStore.simulator.resumeSimulator();
      const outputs = [];
      // 获取输出
      for (let j = 0; j < outputCount; j++) {
        const outputPinId = projectData.outputPins[j];
        const comp = circuitStore.getComponent(outputPinId);
        if (comp) {
          outputs.push(comp.getOutputs()[0]);
        } else {
          outputs.push(0); // 如果没有找到组件，输出为0
        }
      }
      this.truthTable.push([...outputs]);
    }

    // 恢复输入
    for (let j = 0; j < inputCount; j++) {
      circuitStore.getComponent(projectData.inputPins[j]).changeInput(0, oldInputs[j]);
    }
    // 换回项目
    projectStore.loadProject(oldProjectId);
    circuitStore.changeProject(oldProjectId);
    

  }

  changeInput(idx: number, v: number): number[] {
    // this.componentIdMap.get(this.inputPins[idx])!.changeInput(0, v);
    let value = v;
    if(v>=0){
      const mask = (1 << this.bitWidth) - 1;
      if(this.inputInverted[idx]){
        value = ~value & mask;
      }
    }
    this.inputs.splice(idx, 1, value); 
    // 计算真值表的索引
    let index = 0;
    for (let i = 0; i < this.inputs.length; i++) {
      if (this.inputs[i] >= 0) {
        index |= (this.inputs[i] & 1) << i; 
      }else if(this.inputs[i] === -1) {
        index |= 0 << i;
      }else if(this.inputs[i] === -2) {
        this.outputs.splice(0, this.outputs.length, ...Array(this.outputs.length).fill(-2));
        return this.outputs; 
      }
    }
    // 更新输出
    this.outputs.splice(0, this.outputs.length, ...this.truthTable[index]);
    return this.outputs;
  }

  compute(): number[] {
    return this.outputs;
  }

  updatePinPosition(): void{
    // 修改输入
    const inputYs = calcInputYs(this.inputCount);
    const outputYs = calcInputYs(this.outputs.length);
    this.inputPinPosition.splice(0, this.inputPinPosition.length,
      ...inputYs.map((pin, index): [number, number] => {
        return [
          149 - 26,
          pin,
        ];
    }));
    // 修改输出
    this.outputPinPosition.splice(0, this.outputPinPosition.length,
      ...outputYs.map((pin, index): [number, number] => {
        return [
          149+223+57,
          pin,
        ];
    }));
  
  }

}
