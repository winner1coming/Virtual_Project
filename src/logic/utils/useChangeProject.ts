import { reactive } from 'vue';

export function useChangeProject() {
  const projectMap = reactive(new Map<number, { components: any[], ports: any[], connections: any[], Ports: Map<number, any[]> }>());

  // 初始化第一个项目
  projectMap.set(0, {
    components: reactive([]),
    ports: reactive([]),
    connections: reactive([]),
    Ports: reactive(new Map<number, any[]>()),
  });

  // 切换项目
  function switchProject(projectId: number) {
    if(!projectMap.has(projectId)) {
        projectMap.set(projectId, {
            components: reactive([]), 
            ports: reactive([]),
            connections: reactive([]),
            Ports: reactive(new Map<number, any[]>()),
        });
    }
    const projectData = projectMap.get(projectId)!;

    // 更新全局信息
    return {
      components: projectData.components,
      ports: projectData.ports,
      connections: projectData.connections,
      Ports: projectData.Ports,
    };
  }

  // 删除项目
  function deleteProject(projectId: number) {
    if (!projectMap.has(projectId)) {
      console.error(`项目 ${projectId} 不存在`);
      return;
    }
    projectMap.delete(projectId);
  }

  return {
    projectMap,
    switchProject,
    deleteProject,
  };
}