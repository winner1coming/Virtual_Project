import { ProjectData } from "@/logic/ProjectData";
import { useProjectStore } from "@/store/ProjectStore";
import { useCircuitStore } from "@/store/CircuitStore";
import { createComponentByType } from "./useComponentType";
const circuitStore = useCircuitStore();
const projectStore = useProjectStore();

export function exportProject(projectDate: ProjectData): void {
  const simulator = circuitStore.simulator;
  const components: any[] = [];
  projectDate.componentsId.forEach(id => {
    const comp = circuitStore.getComponent(id);
    components.push({
      id: comp.id,
      type: comp.type,
      position: comp.position,
      name: comp.name,
      bitWidth: comp.bitWidth,
      inputCount: comp.inputCount,
      outputCount: comp.outputs.length,
      inputInverted: comp.inputInverted ? [...comp.inputInverted] : [],
    });
  });

  // 导出隧道
  const tunnels = {
    tunnelNameMap: Array.from(simulator.tunnelNameMap.entries()),
    InputTunnelMap: Array.from(simulator.InputTunnelMap.entries())
  };

  // 导出连接关系
  const connections = [];
  for (const [fromId, pinMap] of simulator.connectionManager.connections.entries()) {
    for (const pinIdx of pinMap.keys()) {
      for (const conn of pinMap.get(pinIdx) || []) {
        connections.push({
          fromId,
          fromPin: pinIdx,
          toId: conn.id,
          toPin: conn.idx,
        });
      }
    }
  }

  const exportData = {
    name: projectDate.name,
    components,
    tunnels,
    connections,
  };

  const jsonStr = JSON.stringify(exportData, null, 2);
  console.log("导出的完整关卡配置：", jsonStr);

  const blob = new Blob([jsonStr], { type: 'application/json' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'level.json';
  a.click();
  URL.revokeObjectURL(url);
}


export function loadProject(importData: any): void {
  const simulator = circuitStore.simulator;
  if (!importData || !importData.name || !Array.isArray(importData.components)) {
    console.error("导入的关卡数据格式不正确！");
    return;
  }
  projectStore.createProject(importData.name);

  const componentsIdMap = new Map<number, number>();  // 旧到新
  // 加载元件
  for (const comp of importData.components) {
    circuitStore.addComponent(comp.type, comp.position, comp.name, 0);
    const addedComponent = circuitStore.getComponent(comp.id);
    if (addedComponent) {
      addedComponent.setPosition(comp.position);
      addedComponent.setBitWidth(comp.bitWidth);
      addedComponent.initInputPin(comp.inputCount);
      addedComponent.initOutputPin(comp.outputCount);
      addedComponent.inputInverted.splice(0, addedComponent.inputInverted.length,
        ...(comp.inputInverted || []).map((v: boolean) => v));
      componentsIdMap.set(comp.id, addedComponent.id);
    }
  }

  // 加载隧道
  if (importData.tunnels) {
    simulator.tunnelNameMap = new Map(importData.tunnels.tunnelNameMap);
    simulator.InputTunnelMap = new Map(importData.tunnels.InputTunnelMap);
  }

  // 加载连接关系
  if (importData.connections) {
    for (const conn of importData.connections) {
      circuitStore.connect(
        componentsIdMap.get(conn.fromId) || -1,
        conn.fromPin,
        componentsIdMap.get(conn.toId) || -1,
        conn.toPin,
      );
    }
  }

  console.log("成功导入关卡！");
}

export function importProjectFromFile(event: Event): void {
  const input = event.target as HTMLInputElement;
  const file = input.files?.[0];
  if (!file) {
    console.error("未选择文件！");
    return;
  }

  const reader = new FileReader();
  reader.onload = () => {
    try {
      const importData = JSON.parse(reader.result as string);
      loadProject(importData);
    } catch (error) {
      console.error("文件解析失败！", error);
    }
  };
  reader.readAsText(file);
}