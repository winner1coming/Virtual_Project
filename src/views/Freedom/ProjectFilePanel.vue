<template>
  <div class="project-panel">
    <h3>项目文件</h3>
    <!-- 项目列表 -->
    <div
      v-for="project in projects"
      :key="project.projectId"
      class="project-item"
      @click="loadProject(project.projectId)"
    >
      {{ project.name }}
    </div>

    <!-- 新建项目按钮 -->
    <n-button
      type="primary"
      class="new-project-btn"
      @click="createNewProject"
    >
      新建项目
    </n-button>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted, reactive } from 'vue';
import { NButton } from 'naive-ui';
import { ProjectManager } from '@/logic/ProjectManager';
import { ProjectData } from '@/logic/ProjectData';

const projectManager = ProjectManager.getInstance();

const projects: ProjectData[] = reactive(projectManager.getAllProjects());

// 加载所有项目
const loadProjects = () => {
  projects.splice(0, projects.length, ...projectManager.getAllProjects()); 
};

// 新建项目
const createNewProject = () => {
  const projectName = prompt('请输入新项目名称：', `新项目 ${projects.length + 1}`);
  if (projectName) {
    projectManager.createProject(projectName);
    loadProjects(); // 更新项目列表
  }
};

// 加载项目
const loadProject = (projectId: number) => {
  projectManager.loadProject(projectId);
  //alert(`已加载项目：${project.name}`);
};

// 在组件挂载时加载项目列表
onMounted(() => {
  //loadProjects();
});
</script>

<style scoped>
.project-panel {
  padding: 10px;
}

.project-item {
  padding: 8px;
  margin: 5px 0;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: pointer;
}

.project-item:hover {
  background-color: #f0f5ff;
}

.new-project-btn {
  margin-top: 15px;
  width: 100%;
}
</style>