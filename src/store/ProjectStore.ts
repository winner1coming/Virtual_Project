// stores/projectStore.ts
import { defineStore } from 'pinia';
import { reactive, ref } from 'vue';
import type { ProjectData } from '@/logic/ProjectData';
import { useCircuitStore } from './CircuitStore';
import { Clock } from '@/logic/components/Clock';
import { c } from 'naive-ui';
export const useProjectStore = defineStore('project', () => {
  const allProjects = reactive(new Map<number, ProjectData>());
  const nextProjectId = ref<number>(0);    // 
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
  };
  allProjects.set(defaultProject.projectId, defaultProject);
  selectedProjectId.value = defaultProject.projectId; // 设置默认选中项目
  nextProjectId.value = 1; // 下一个项目ID从1开始

  function createProject(name: string): ProjectData {
    const project: ProjectData = {
      projectId: nextProjectId.value++,
      projectUUID: crypto.randomUUID(), // 生成一个新的UUID
      mode: useCircuitStore().currentMode,
      name,
      componentsId: [],
      inputPins: [],
      outputPins: [],
      clockIds: [],
    };
    allProjects.set(project.projectId, project);
    loadProject(project.projectId);
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
  function getProjectById(projectId: number): ProjectData {
    if (!allProjects.has(projectId)) {
      throw new Error(`Project with ID ${projectId} not found.`);
    }
    return allProjects.get(projectId)!;
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

  return {
    allProjects,
    nextProjectId,
    selectedProjectId,
    createProject,
    loadProject,
    getCurrentProject,
    getProjectById,
    getAllProjects,
    deleteProject,
    getProjectIds,
  };
});
