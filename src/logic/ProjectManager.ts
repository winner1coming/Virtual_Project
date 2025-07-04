// import { reactive, ref } from 'vue';
// import type { ProjectData } from './ProjectData';

// export class ProjectManager {
//   private static instance: ProjectManager;
//   allProjects = reactive(new Map<number, ProjectData>()); // 使用 Map 存储 id 与 ProjectData 的映射
//   currentProjectId = 0;
//   selectedProjectId = ref<number>(-1); // 当前选中的项目 ID

//   constructor() {
//     const defaultProject: ProjectData = {
//       projectId: this.currentProjectId++,
//       name: "新项目",
//       componentsId: [],
//       inputPins: [],
//       outputPins: []
//     };
//     this.allProjects.set(defaultProject.projectId, defaultProject);
//     this.selectedProjectId.value = defaultProject.projectId;
//   }

//   static getInstance(): ProjectManager {
//     if (!ProjectManager.instance) {
//       ProjectManager.instance = new ProjectManager();
//     }
//     return ProjectManager.instance;
//   }

//   createProject(name: string): ProjectData {
//     const newProject: ProjectData = {
//       projectId: this.currentProjectId++,
//       name,
//       componentsId: [],
//       inputPins: [],
//       outputPins: []
//     };
//     this.allProjects.set(newProject.projectId, newProject); 
//     this.selectedProjectId.value = newProject.projectId; 
//     return newProject;
//   }

//   loadProject(projectId: number) {
//     if (this.allProjects.has(projectId)) {
//       this.selectedProjectId.value = projectId; // 更新选中的项目 ID
//     } else {
//       throw new Error(`Project with ID ${projectId} not found.`);
//     }
//   }

//   getCurrentProject(): ProjectData{
//     return this.allProjects.get(this.selectedProjectId.value)!;
//   }

//   getProjectById(projectId: number): ProjectData{
//     if(!this.allProjects.has(projectId)) {
//       throw new Error(`Project with ID ${projectId} not found.`);
//     }
//     return this.allProjects.get(projectId)!;
//   }

//   getAllProjects(): ProjectData[] {
//     return Array.from(this.allProjects.values()); // 返回所有项目的数组
//   }

//   deleteProject(projectId: number): boolean {
//     if (this.allProjects.has(projectId)) {
//       this.allProjects.delete(projectId);
//       if (this.selectedProjectId.value === projectId) {
//         this.selectedProjectId.value = -1; // 如果删除的是当前选中的项目，清空选中状态
//       }
//       return true;
//     }
//     return false;
//   }
// }