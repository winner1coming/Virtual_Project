// stores/projectStore.ts
import { defineStore } from 'pinia';
import { reactive, ref } from 'vue';
import type { ProjectData } from '@/logic/ProjectData';

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
    outputPins: []
  };
  allProjects.set(defaultProject.projectId, defaultProject);
  selectedProjectId.value = defaultProject.projectId;

  function createProject(name: string): ProjectData {
    const project: ProjectData = {
      projectId: currentProjectId.value++,
      name,
      componentsId: [],
      inputPins: [],
      outputPins: []
    };
    allProjects.set(project.projectId, project);
    selectedProjectId.value = project.projectId;
    return project;
  }

  function loadProject(projectId: number) {
    if (!allProjects.has(projectId)) {
      throw new Error(`Project ${projectId} not found`);
    }
    selectedProjectId.value = projectId;
    
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
