<template>
  <div class="project-panel">
    <h3>项目文件</h3>
    <!-- 项目列表 -->
    <div
      v-for="project in projects"
      :key="project.projectId"
      class="project-item"
      :class="{ selected: project.projectId === projectStore.selectedProjectId }"
      @mousedown="handleMouseDown(project.projectId)"
      @contextmenu.prevent="showContextMenu($event, project)"
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

    <!-- 右键菜单 -->
    <n-dropdown
      v-model:show="contextMenuVisible"
      :options="contextMenuOptions"
      :x="contextMenuPosition.x"
      :y="contextMenuPosition.y"
      @select="handleContextMenuSelect"
    />
  </div>
</template>

<script setup lang="ts">
import { ref, reactive, onMounted } from 'vue';
import { c, create, NButton, NDropdown } from 'naive-ui';
import { useProjectStore } from '@/store/ProjectStore';
import { ProjectData } from '@/logic/ProjectData';
import { useCircuitStore } from '@/store/CircuitStore';
import eventBus from '@/modules/useEventBus';

const projectStore = useProjectStore();

// 项目列表
const projects: ProjectData[] = reactive(projectStore.getAllProjects());

// 右键菜单状态
const contextMenuVisible = ref(false);
const contextMenuPosition = reactive({ x: 0, y: 0 });
const selectedProject = ref<ProjectData | null>(null);

// 右键菜单选项
const contextMenuOptions = [
  { label: '重命名', key: 'rename' },
  { label: '删除', key: 'delete' }
];

// 加载所有项目
const loadProjects = () => {
  projects.splice(0, projects.length, ...projectStore.getAllProjects());
};

// 新建项目
const createNewProject = () => {
  const projectName = prompt('请输入新项目名称：', `新项目 ${projects.length + 1}`);
  if (projectName) {
    projectStore.createProject(projectName);
    loadProjects(); // 更新项目列表
  }
};

let clickTimer: number | null = null;
let clickCount = 0;
const handleMouseDown = (projectId: number) => {
  clickCount++;
  if(clickCount === 1){
    clickTimer = setTimeout(() => {
      createSubComponent(projectId); 
      clickCount = 0; 
    }, 200); 
  } else if (clickCount === 2) {
    if (clickTimer) {
      clearTimeout(clickTimer);
      clickTimer = null; // 清除定时器
    }
    loadProject(projectId); // 双击加载项目
    clickCount = 0; // 重置点击计数
  }
};

// 加载项目（双击）
const loadProject = (projectId: number) => {
  if(clickTimer) {
    clearTimeout(clickTimer);
    clickTimer = null; // 清除定时器
  }
  projectStore.loadProject(projectId);
};

// 根据项目ID创建子组件（单击）
const createSubComponent = (projectId: number) => {
  clickTimer = setTimeout(() => {
    console.log("创建子组件", projectId);
    // // 画布那边不用再addComponent todo （预览图的处理）
    // useCircuitStore().addComponent("SUB_CIRCUIT", [0,0], "", projectId);

    eventBus.emit('start-place-component', {type: "SUB_CIRCUIT", projectId: projectId} );

    clickTimer = null; // 清除定时器
  }, 500);
  
};

// 显示右键菜单
const showContextMenu = (event: MouseEvent, project: ProjectData) => {
  contextMenuVisible.value = true;
  contextMenuPosition.x = event.clientX;
  contextMenuPosition.y = event.clientY;
  selectedProject.value = project;
};

// 处理右键菜单选项
const handleContextMenuSelect = (key: string) => {
  if (!selectedProject.value) return;

  if (key === 'rename') {
    const newName = prompt('请输入新的项目名称：', selectedProject.value.name);
    if (newName) {
      selectedProject.value.name = newName;
      loadProjects(); // 更新项目列表
    }
  } else if (key === 'delete') {
    if (confirm(`确定要删除项目 "${selectedProject.value.name}" 吗？`)) {
      projectStore.deleteProject(selectedProject.value.projectId);
      loadProjects(); // 更新项目列表
    }
  }

  contextMenuVisible.value = false; // 隐藏右键菜单
};

// 在组件挂载时加载项目列表
onMounted(() => {
  loadProjects();
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
  border: 1px solid #E0E6ED;
  border-radius: 4px;
  cursor: pointer;
}
.project-item.selected {
  background-color: #E0E6ED;
  border-color: #E0E6ED;
}

.project-item:hover {
  background-color: #f0f5ff;
}

.new-project-btn {
  margin-top: 15px;
  width: 100%;
}
</style>