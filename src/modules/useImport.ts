import { ProjectData } from "@/logic/ProjectData";
import { useProjectStore } from "@/store/ProjectStore";
import { useCircuitStore } from "@/store/CircuitStore";
import { createComponentByType } from "./useComponentType";
const circuitStore = useCircuitStore();
const projectStore = useProjectStore();

export function exportProject(projectDate: ProjectData): void {
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
    });
  })

  const exportData = {
    name: projectDate.name,
    components: components,
  }

  // 转成 JSON 字符串
  const jsonStr = JSON.stringify(exportData, null, 2);
  console.log("导出的关卡配置：", jsonStr);

  // 自动下载为文件
  const blob = new Blob([jsonStr], { type: 'application/json' });
  const url = URL.createObjectURL(blob);
  const a = document.createElement('a');
  a.href = url;
  a.download = 'level.json';
  a.click();
  URL.revokeObjectURL(url);
}

export function loadProject(importData: any): void {
  if (!importData || !importData.name || !Array.isArray(importData.components)) {
    console.error("导入的关卡数据格式不正确！");
    return;
  }
  projectStore.createProject(importData.name);

  // 加载元件
  if (Array.isArray(importData.components)) {
    for (const comp of importData.components) {
      if (comp && comp.type && Array.isArray(comp.position) && comp.position.length === 2) {
        circuitStore.addComponent(comp.type, comp.position, comp.name, 0);
        const addedComponent = circuitStore.getComponent(comp.id);
        if (addedComponent) {
          addedComponent.setPosition(comp.position);
          addedComponent.setBitWidth(comp.bitWidth);
          addedComponent.initInputPin(comp.inputCount);
          addedComponent.initOutputPin(comp.outputCount);
          addedComponent.inputInverted.splice(0, addedComponent.inputInverted.length,
            ...(comp.inputInverted || []).map((v: boolean) => v));
        }

      } else {
        console.error("关卡数据中的元件格式错误！", comp);
      }
    }
  } else {
    console.error("关卡数据中的组件格式错误！");
  }

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