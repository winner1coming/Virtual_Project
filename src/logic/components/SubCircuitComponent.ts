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
  public inputNames: string[] = []; // 输入引脚的名称
  public outputNames: string[] = []; // 输出引脚的名称
  public copyProjectId: number = 0;
  public projectUUID: string = "";
  projectStore: any;

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

   if(projectData.truthTable.length === 0){
    // 计算真值表
    projectStore.calculateTruthTable(projectId);
   }
   // 存inputName
    for(const inputPinId of projectData.inputPins) {
      const comp = circuitStore.getComponent(inputPinId);
      if (comp) {
        this.inputNames.push(comp.name);
      }else{
        this.inputNames.push(""); 
      }
    }
    // 存outputName
    for(const outputPinId of projectData.outputPins) {
      const comp = circuitStore.getComponent(outputPinId);
      if (comp) {
        this.outputNames.push(comp.name);
      } else {
        this.outputNames.push(""); 
      }
    }
    
  }

  changeInput(idx: number, v: number): number[] {
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
    const projectStore = useProjectStore();
    // 计算真值表
    if(projectStore.getProjectById(this.copyProjectId).truthTable.length === 0 || 
      projectStore.getProjectById(this.copyProjectId).hasChanged) {
      projectStore.calculateTruthTable(this.copyProjectId);
    }
    // 更新输出
    this.outputs.splice(0, this.outputs.length, ...projectStore.getProjectById(this.copyProjectId).truthTable[index]);
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
