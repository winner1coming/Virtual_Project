// 模拟子电路的逻辑，承 BaseComponent，内部运行子电路
import { BaseComponent } from '../BaseComponent';
import { useCircuitStore } from '@/store/CircuitStore';
import { reactive } from 'vue';
import { SubSimulator } from '../SubSimulator';
import { useProjectStore } from '@/store/ProjectStore';
import { createComponentByType } from '@/modules/useComponentType';
import { calcInputYs } from "@/logic/utils/useGateLayout";

export class SubCircuitComponent extends BaseComponent {
  private inputPins: number[];   // 输入引脚的id
  private outputPins: number[];
  private componentIdMap: Map<number, BaseComponent> = new Map(); // 用于映射元件的id
  public inputNames: string[] = []; // 输入引脚的名称
  public outputNames: string[] = []; // 输出引脚的名称

  constructor(
    id: number,
    type: String,
    position:[number, number] = [0, 0],
    name: String,
    projectId: number,
  ) {
    super(id, type, position);
    this.name = name;

    const store = useCircuitStore();
    const projectStore = useProjectStore();

    this.inputPins = projectStore.getProjectById(projectId).inputPins;
    this.outputPins = projectStore.getProjectById(projectId).outputPins;


    this.initInputPin(this.inputPins.length);
    this.initOutputPin(this.outputPins.length);

    // 根据id创建组件
    projectStore.getProjectById(projectId).componentsId.forEach(id => {
      const comp = store.getComponent(id);
      if (comp) {
        this.componentIdMap.set(id, createComponentByType(id, comp.type, comp.position, comp.name));
        if(comp.type === "INPUT") {
          this.componentIdMap.get(id)!.changeInput(0, -1);
          this.inputNames.push(comp.name.toString());
        }else if(comp.type === "OUTPUT") {
          this.componentIdMap.get(id)!.changeInput(0, -1);
          this.outputNames.push(comp.name.toString());
        }
        // 复制关键属性
        this.componentIdMap.get(id)!.setPosition(comp.position);
        this.componentIdMap.get(id)!.setBitWidth(comp.bitWidth);
        this.componentIdMap.get(id)!.initInputPin(comp.inputCount);
        this.componentIdMap.get(id)!.initOutputPin(comp.outputs.length);
        this.componentIdMap.get(id)!.inputInverted.splice(0, this.componentIdMap.get(id)!.inputInverted.length,
          ...comp.inputInverted.map((v) => v))
      }
    });

    this.simulator = new SubSimulator(projectId, this.componentIdMap);
  }

  changeInput(idx: number, v: number): number[] {
    // this.componentIdMap.get(this.inputPins[idx])!.changeInput(0, v);
    this.simulator.enqueue(this.inputPins[idx], 0, v);
    this.simulator.processQueue();
    this.updateInputs();
    this.updateOutputs();
    return this.outputs;
  }

  updateInputs(){
    this.inputPins.forEach((id, idx) => {
      const comp = this.componentIdMap.get(id);
      if (comp) {
        this.inputs[idx] = comp.getOutputs()[0];
      } 
    });
  }
  updateOutputs() {
    this.outputPins.forEach((id, idx) => {
      const comp = this.componentIdMap.get(id);
      if (comp) this.outputs[idx] = comp.getOutputs()[0];
    });
  }

  compute(): number[] {
    this.simulator.processQueue();
    this.updateOutputs();
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
