// 负责项目创建、加载、保存、导出，统一管理 ProjectData
import { reactive, ref, Ref } from 'vue';
import type { ProjectData } from './ProjectData';

export class ProjectManager{
  private static instance: ProjectManager;
  allProjects = reactive<ProjectData[]>([]);
  currentProjectId = 0;
  selectedProject = reactive<ProjectData>(
    {
      projectId: this.currentProjectId++,
      name : "新项目",
      componentsId: [],
      inputPins: [],
      outputPins: []
    }
  );

  constructor() {
    this.allProjects.push(this.selectedProject);
  }

  static getInstance(): ProjectManager {
    if (!ProjectManager.instance) {
      ProjectManager.instance = new ProjectManager();
    }
    return ProjectManager.instance;
  }

  createProject(name: string): ProjectData {
    const newProject: ProjectData = {
      projectId: this.currentProjectId++,
      name,
      componentsId: [],
      inputPins: [],
      outputPins: []
    };
    this.allProjects.push(newProject);
    this.selectedProject = newProject;
    return newProject;
  }

  loadProject(project: ProjectData) {
    this.selectedProject = project;
  }

  getCurrentProject() {
    return this.selectedProject;
  }

  getProjectById(projectId: number): ProjectData{
    return this.allProjects.find(project => project.projectId === projectId)!;
  }

  getAllProjects() {
    return this.allProjects;
  }
}
