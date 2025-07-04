// 模拟子电路的逻辑，承 BaseComponent，内部运行子电路
import { BaseComponent } from '../BaseComponent';
import { useCircuitStore } from '@/store/CircuitStore';
import { reactive } from 'vue';
import { SubSimulator } from '../SubSimulator';
import { useProjectStore } from '@/store/ProjectStore';
import { createComponentByType } from '@/modules/useComponentType';

export class SubCircuitComponent extends BaseComponent {
  private inputPins: number[];   // 输入引脚的id
  private outputPins: number[];
  private componentIdMap: Map<number, BaseComponent> = new Map(); // 用于映射元件的id

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

    this.changeInputPinCount(this.inputPins.length);
    this.changeOutputPinCount(this.outputPins.length);

    // 根据id创建组件
    projectStore.getProjectById(projectId).componentsId.forEach(id => {
      const comp = store.getComponent(id);
      if (comp) {
        this.componentIdMap.set(id, createComponentByType(id, comp.type, comp.position, comp.name));
      }
    });

    this.simulator = new SubSimulator(projectId, this.componentIdMap);
  }

  changeInput(idx: number, v: number): number[] {
    this.componentIdMap.get(this.inputPins[idx])!.changeInput(0, v);
    this.simulator.enqueue(this.inputPins[idx], 0, v);
    this.simulator.processQueue();
    this.updateOutputs();
    return this.outputs;
  }

  updateOutputs() {
    const store = useCircuitStore();
    this.outputPins.forEach((id, idx) => {
      const comp = store.getComponent(id);
      if (comp) this.outputs[idx] = comp.getOutputs()[0];
    });
  }

  compute(): number[] {
    this.simulator.processQueue();
    this.updateOutputs();
    return this.outputs;
  }
}
