import { reactive } from 'vue';

export function useChangeProject() {
  const projectMap = reactive(new Map<number, { components: any[], ports: any[], connections: any[], Ports: Map<number, any[]> }>());
  let currentProjectId = 0; // 当前项目 ID

  // 初始化第一个项目
  projectMap.set(0, {
    components: reactive([]),
    ports: reactive([]),
    connections: reactive([]),
    Ports: reactive(new Map<number, any[]>()),
  });

  // 切换项目
  function switchProject(projectId: number) {
    if (!projectMap.has(projectId)) {
      console.error(`项目 ${projectId} 不存在`);
      return;
    }

    const projectData = projectMap.get(projectId)!;

    // 更新当前项目 ID
    currentProjectId = projectId;

    // 更新全局信息
    return {
      components: projectData.components,
      ports: projectData.ports,
      connections: projectData.connections,
      Ports: projectData.Ports,
    };
  }

  // 创建新项目
  function createProject(projectId: number) {
    if (projectMap.has(projectId)) {
      console.error(`项目 ${projectId} 已存在`);
      return;
    }

    projectMap.set(projectId, {
      components: reactive([]),
      ports: reactive([]),
      connections: reactive([]),
      Ports: reactive(new Map<number, any[]>()),
    });
  }

  // 删除项目
  function deleteProject(projectId: number) {
    if (!projectMap.has(projectId)) {
      console.error(`项目 ${projectId} 不存在`);
      return;
    }

    projectMap.delete(projectId);

    // 如果删除的是当前项目，重置为默认项目
    if (currentProjectId === projectId) {
      currentProjectId = 0;
    }
  }

  return {
    projectMap,
    currentProjectId,
    switchProject,
    createProject,
    deleteProject,
  };
}