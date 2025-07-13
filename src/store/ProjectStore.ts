import { defineStore } from 'pinia';
import { reactive, ref } from 'vue';
import type { ProjectData } from '@/logic/ProjectData';
import { useCircuitStore } from './CircuitStore';
import { Clock } from '@/logic/components/Clock';
import { calculateTruthTable as computeTruthTable } from '@/modules/useTruthTable';
export const useProjectStore = defineStore('project', () => {
  const allProjects = reactive(new Map<number, ProjectData>());
  const nextProjectId = ref<number>(0);
  const selectedProjectId = ref<number>(-1);

  // 初始化一个默认项目
  const defaultProject: ProjectData = {
    projectId: 0,
    projectUUID: crypto.randomUUID(),
    mode: 'practice',
    name: '新项目',
    componentsId: [],
    inputPins: [],
    outputPins: [],
    clockIds: [],
    hasChanged: true, // 是否有更改
    truthTable: [], // 真值表
  };
  allProjects.set(defaultProject.projectId, defaultProject);
  selectedProjectId.value = defaultProject.projectId; // 设置默认选中项目
  nextProjectId.value = 1; // 下一个项目ID从1开始

  function createProject(name: string, mode: string=""): ProjectData {
    const project: ProjectData = {
      projectId: nextProjectId.value++,
      projectUUID: crypto.randomUUID(), // 生成一个新的UUID
      mode: mode? mode:useCircuitStore().currentMode,
      name,
      componentsId: [],
      inputPins: [],
      outputPins: [],
      clockIds: [],
      hasChanged: true, // 新项目默认有更改
      truthTable: [], // 新项目默认没有真值表
    };
    allProjects.set(project.projectId, project);
    loadProject(project.projectId);
    console.log(`Created project: ${name} with ID ${project.projectId}`);
    return project;
  }

  // 切换项目
  function loadProject(projectId: number) {
    // // 确认模式
    // const mode = useCircuitStore().currentMode;
    // if(mode === 'tutorial'){
    //   return;
    // }else if(mode === 'practice') {
    //   if(getProjectById(projectId).mode !== 'practice') {

    //   }
    // }
    // 清除当前选中的项目
    if (getCurrentProject().clockIds) {
      // 停止所有时钟
      getCurrentProject().clockIds.forEach(clockId => {
        const clock = useCircuitStore().getComponent(clockId) as Clock;
        if (clock) {
          clock.stop();
        }
      });
    }
    selectedProjectId.value = projectId;
    useCircuitStore().changeProject(projectId);
    // 开始所有时钟
    getCurrentProject().clockIds.forEach(clockId => {
      const clock = useCircuitStore().getComponent(clockId) as Clock;
      if (clock) {
        clock.start();
      }
    });

  }

  function getCurrentProject(): ProjectData {
    return allProjects.get(selectedProjectId.value)!;
  }

  function getProjectById(projectId: number): ProjectData|null {
    if (!allProjects.has(projectId)) {
      return null;
    }
    return allProjects.get(projectId)!;
  }

  function getProjectIdByUUID(projectId:number, uuid: string): number{
    if(allProjects.has(projectId ) && allProjects.get(projectId)!.projectUUID === uuid) {
     return projectId;
    } 
    for(let p of getAllProjects()) {
      if(p.projectUUID === uuid) {
        return p.projectId;
      }
    }
    return -1; 
  }

  function getAllProjects(): ProjectData[] {
    return Array.from(allProjects.values());
  }

  function getProjectIds(): number[] {
    return Array.from(allProjects.keys());
  }

  function deleteProject(projectId: number) {
    if (allProjects.has(projectId)) {
      allProjects.delete(projectId);
      if (selectedProjectId.value === projectId) {
        selectedProjectId.value = -1;
      }
    }
  }

  function calculateTruthTable(projectId: number){
    const oldProjectId = selectedProjectId.value;
    loadProject(projectId);
    allProjects.get(projectId)!.truthTable = computeTruthTable(projectId);
    loadProject(oldProjectId);
    allProjects.get(projectId)!.hasChanged = false; // 计算真值表后，项目不再有更改
  }

  return {
    allProjects,
    nextProjectId,
    selectedProjectId,
    createProject,
    loadProject,
    getCurrentProject,
    getProjectById,
    getProjectIdByUUID,
    getAllProjects,
    deleteProject,
    getProjectIds,
    calculateTruthTable,
  };
});
