import { useCircuitStore } from '@/store/CircuitStore';
import { useProjectStore } from '@/store/ProjectStore';

// 只能计算当前项目的真值表
export function calculateTruthTable(projectId: number, inputNames: string[]=[], outputNames: string[]=[]): number[][] {
  const circuitStore = useCircuitStore();
  const projectStore = useProjectStore();

  // 获取项目数据
  const projectData = projectStore.getProjectById(projectId);
  if(!projectData) return [];
  // 根据名字找到对应的引脚 ID
  let inputPins,outputPins;
  if(inputNames.length === 0 && outputNames.length === 0) {
    inputPins = projectData.inputPins;
    outputPins = projectData.outputPins;
  }else{
    inputPins = inputNames.map(name =>
      projectData.inputPins.find(pinId => circuitStore.getComponent(pinId)?.name === name)
    );
    outputPins = outputNames.map(name =>
      projectData.outputPins.find(pinId => circuitStore.getComponent(pinId)?.name === name)
    );

    if (inputPins.includes(undefined) || outputPins.includes(undefined)) {
      return [];
    }
  }
  

  let oldInputs: number[] = [];

  const inputCount = inputPins.length;
  const outputCount = outputPins.length;
  const totalCombinations = 1 << inputCount; // 2^inputCount

  const truthTable: number[][] = [];

  // 保存当前输入状态
  for (const inputPinId of inputPins) {
    const comp = circuitStore.getComponent(inputPinId!);
    if (comp) {
      oldInputs.push(comp.getOutputs()[0]);
    } else {
      oldInputs.push(0); // 如果组件不存在，默认输入为 0
    }
  }

  // 遍历所有输入组合
  for (let i = 0; i < totalCombinations; i++) {
    // 暂停模拟器
    circuitStore.simulator.pauseSimulator();

    // 设置输入
    for (let j = 0; j < inputCount; j++) {
      const value = (i >> j) & 1; // 获取第 j 位的值（0 或 1）
      circuitStore.getComponent(inputPins[j]!).changeInput(0, value);
    }

    // 恢复模拟器
    circuitStore.simulator.resumeSimulator();

    const outputs: number[] = [];
    // 获取输出
    for (const outputPinId of outputPins) {
      const comp = circuitStore.getComponent(outputPinId!);
      if (comp) {
        outputs.push(comp.getOutputs()[0]);
      } else {
        outputs.push(0); // 如果组件不存在，默认输出为 0
      }
    }

    truthTable.push(outputs);
  }

  // 恢复输入状态
  for (let j = 0; j < inputCount; j++) {
    circuitStore.getComponent(inputPins[j]!).changeInput(0, oldInputs[j]);
  }

  return truthTable;
}