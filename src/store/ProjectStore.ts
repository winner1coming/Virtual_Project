// stores/projectStore.ts
import { defineStore } from 'pinia';
import { reactive, ref } from 'vue';
import type { ProjectData } from '@/logic/ProjectData';
import { useCircuitStore } from './CircuitStore';
import { Clock } from '@/logic/components/Clock';

export const useProjectStore = defineStore('project', () => {
  const allProjects = reactive(new Map<number, ProjectData>());
  const currentProjectId = ref<number>(0);
  const selectedProjectId = ref<number>(-1);

  // 初始化一个默认项目
  const defaultProject: ProjectData = {
    projectId: currentProjectId.value++,
    name: "新项目",
    componentsId: [],
    inputPins: [],
    outputPins: [],
    clockIds: [],
  };
  allProjects.set(defaultProject.projectId, defaultProject);
  selectedProjectId.value = defaultProject.projectId;

  function createProject(name: string): ProjectData {
    const project: ProjectData = {
      projectId: currentProjectId.value++,
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
    if (!allProjects.has(projectId)) {
      throw new Error(`Project ${projectId} not found`);
    }
    // 清除当前选中的项目
    if(getCurrentProject().clockIds.length > 0) {
      // 停止所有时钟
      getCurrentProject().clockIds.forEach(clockId => {
        const clock = useCircuitStore().getComponent(clockId) as Clock;
        if (clock) {
          clock.stop();
        }
      });
    }
    selectedProjectId.value = projectId;
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
    currentProjectId,
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
