// 导入导出电路
// 含子组件的电路，一点要先导入子组件的原本的电路
import { ProjectData } from "@/logic/ProjectData";
import { useProjectStore } from "@/store/ProjectStore";
import { useCircuitStore } from "@/store/CircuitStore";
import { nextTick } from "vue";
import { SubCircuitComponent } from "@/logic/components/SubCircuitComponent";
const circuitStore = useCircuitStore();
const projectStore = useProjectStore();

export function exportProject(projectDate: ProjectData): void {
  const simulator = circuitStore.simulator;
  // 导出组件
  const components: any[] = [];
  projectDate.componentsId.forEach(id => {
    const comp = circuitStore.getComponent(id);
    if(comp.type === "SUB_CIRCUIT") {
      // 子组件的特殊处理
      const sub = comp as SubCircuitComponent;
      components.push({
        id: sub.id,
        type: sub.type,
        position: sub.position,
        name: sub.name,
        bitWidth: sub.bitWidth,
        inputCount: sub.inputCount,
        outputCount: sub.outputs.length,
        inputInverted: sub.inputInverted ? [...sub.inputInverted] : [],
        inputNames: sub.inputNames ? [...sub.inputNames] : [],
        outputNames: sub.outputNames ? [...sub.outputNames] : [],
        copyProjectId: sub.copyProjectId,
        projectUUID: sub.projectUUID,
      });
    }else{
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
    }
  });

  // 导出连接关系
  const connections = [];
  for(const id of projectDate.componentsId) {
    const pinMap = simulator.connectionManager.getOutputPinMap(id);
    if (!pinMap) continue; 
    for (const pinIdx of pinMap.keys()) {
      for (const conn of pinMap.get(pinIdx) || []) {
        connections.push({
          fromId:id,
          fromPin: pinIdx + circuitStore.getComponent(id).inputCount,
          toId: conn.id,
          toPin: conn.idx, 
        });
      }
    }
  }

  const exportData = {
    name: projectDate.name,
    uuid: projectDate.projectUUID,
    mode: projectDate.mode,
    components,
    connections,
  };

  const jsonStr = JSON.stringify(exportData, null, 2);
  const blob = new Blob([jsonStr], { type: 'application/json' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = projectDate.name + '.json';
  a.click();
  URL.revokeObjectURL(url);
}

export async function loadProject(importData: any, canvasRef: any){
  if (!importData || !importData.name || !Array.isArray(importData.components)) {
    console.error("导入的关卡数据格式不正确！");
    return;
  }
  projectStore.getCurrentProject().name = importData.name;
  projectStore.getCurrentProject().projectUUID = importData.uuid;
  projectStore.getCurrentProject().mode = importData.mode;

  const componentsIdMap = new Map<number, number>();  // 旧到新的id的映射
  // 加载元件
  for (const comp of importData.components) {
    await canvasRef.addComponentByScript(comp.type, comp.position);
    // 插入后，元件id即为最新的id-1
    const addedComponent = circuitStore.getComponent(circuitStore.currentId - 1);
    if (addedComponent) {
      addedComponent.setPosition(comp.position);
      addedComponent.setBitWidth(comp.bitWidth);
      addedComponent.setName(comp.name);
      addedComponent.initInputPin(comp.inputCount);
      addedComponent.initOutputPin(comp.outputCount);
      addedComponent.inputInverted.splice(0, addedComponent.inputInverted.length,
        ...(comp.inputInverted || []).map((v: boolean) => v));
      if (comp.type === "INPUT") {
        addedComponent.changeInput(0, 0);
      }
      if(comp.type === "SUB_CIRCUIT") {
        const subComp = addedComponent as SubCircuitComponent;
        subComp.inputNames = comp.inputNames || [];
        subComp.outputNames = comp.outputNames || [];
        subComp.copyProjectId = comp.copyProjectId || 0; 
        subComp.initInputPin(subComp.inputs.length);
        subComp.initOutputPin(subComp.outputs.length);
        subComp.projectUUID = comp.projectUUID || "";
        // 查找是哪个project
        const projectId = projectStore.getProjectIdByUUID(subComp.copyProjectId,subComp.projectUUID);
        if(projectId !== -1) {
          subComp.copyProjectId = projectId;
        }
        
      }
      componentsIdMap.set(comp.id, addedComponent.id);
    
    }
  }

  await nextTick();
  // 加载连接关系
  if (importData.connections) {
    for (const conn of importData.connections) {
      // 画布连线
      canvasRef.connectByScript(componentsIdMap.get(conn.fromId), conn.fromPin, componentsIdMap.get(conn.toId), conn.toPin);
    }
  }
  console.log("成功导入关卡！");
}

export function importProjectFromFile(event: Event, canvasRef: any): void {
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
      loadProject(importData, canvasRef);
    } catch (error) {
      console.error("文件解析失败！", error);
    }
  };
  reader.readAsText(file);
}