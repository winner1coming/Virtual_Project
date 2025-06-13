<template>
  <div class="workspace-container">
    <!-- 导航栏-->
    <div class="navbar-container">
      <nav class="navbar">
        <!-- 左侧面包屑 -->
        <div class="nav-left">
          <n-breadcrumb>
            <n-breadcrumb-item @click="goHome">
              <n-icon :component="home"/>首页
            </n-breadcrumb-item>
            <n-breadcrumb-item>
              <n-icon :component="Bo"/>{{ modeLabel }}
            </n-breadcrumb-item>
          </n-breadcrumb>
        </div>

        <!-- 右侧菜单 -->
        <div class="nav-right">
            <n-button quaternary circle @click="handleClick">
              <template #icon>
                <n-icon>
                  <component :is = "showToolbar ? downIcon : forwardIcon"/>
                </n-icon>
              </template>
            </n-button>
        </div>
      </nav>
      
      <!-- 工具栏 - 展开后显示 -->
      <div v-if="showToolbar" class="toolbar">
        <n-button-group class = "buttongroup">
          <n-tooltip trigger="hover">
            <template #trigger>
              <n-button quaternary @click="saveProject" >
                <template #icon>
                  <n-icon><save-icon /></n-icon>
                </template>
              </n-button>
            </template>
            保存
          </n-tooltip>
          
          <n-tooltip trigger="hover">
            <template #trigger>
              <n-button quaternary @click="prevStep">
                <template #icon>
                  <n-icon><arrow-back-icon /></n-icon>
                </template>
              </n-button>
            </template>
            上一步
          </n-tooltip>
          
          <n-tooltip trigger="hover">
            <template #trigger>
              <n-button quaternary @click="nextStep" >
                <template #icon>
                  <n-icon><arrow-forward-icon /></n-icon>
                </template>
              </n-button>
            </template>
            下一步
          </n-tooltip>
          
          <n-tooltip trigger="hover">
            <template #trigger>
              <n-button quaternary @click="clearWorkspace">
                <template #icon>
                  <n-icon><trash-icon /></n-icon>
                </template>
              </n-button>
            </template>
            清空
          </n-tooltip>
        </n-button-group>
      </div>
    </div>

    <div class="main-content">
      <!-- 左侧工具栏按钮区 -->
      <div class="toolbox">
        <div class="drawer-buttons">
          <n-tooltip trigger="hover" placement="right">
            <template #trigger>
              <n-button 
                quaternary 
                class="drawer-button"
                :type="activeDrawer === 'material' ? 'primary' : 'default'"
                @click="toggleDrawer('material')"
              >
                <template #icon>
                  <n-icon><textoutline /></n-icon>
                </template>
              </n-button>
            </template>
            资料
          </n-tooltip>

          <n-tooltip trigger="hover" placement="right">
            <template #trigger>
              <n-button 
                quaternary 
                class="drawer-button"
                :type="activeDrawer === 'component' ? 'primary' : 'default'"
                @click="toggleDrawer('component')"
              >
                <template #icon>
                  <n-icon><cube /></n-icon>
                </template>
              </n-button>
            </template>
            元件
          </n-tooltip>

          <n-tooltip trigger="hover" placement="right">
            <template #trigger>
              <n-button 
                quaternary 
                class="drawer-button"
                :type="activeDrawer === 'project' ? 'primary' : 'default'"
                @click="toggleDrawer('project')"
              >
                <template #icon>
                  <n-icon><folder /></n-icon>
                </template>
              </n-button>
            </template>
            项目文件
          </n-tooltip>
        </div>
      </div>

      <div class="drawer-container" v-if="activeDrawer">
        <!-- 自定义抽屉 -->
        <transition name="slide-fade">
          <n-split
            :default-size="0.08"
            :min="0"
            :max="0.60"
            direction="horizontal"
            class="split-container"
          >
          <template #1>
            <div class="local-drawer">
              <component :is="activeDrawerComponent" />
            </div>
          </template>
          <template #2>
            <!-- 实验区 -->
            <div 
              class="canvas"
              @dragover.prevent
              @drop="onDrop"
            >
            </div>
          </template>
          </n-split>
        </transition>
      </div>
    </div>
  </div>
</template>

<script setup>
import { computed, ref, defineAsyncComponent} from 'vue'
import { useRouter } from 'vue-router'
import { 
  NBreadcrumb, 
  NBreadcrumbItem, 
  NButton, 
  NButtonGroup, 
  NIcon, 
  NTooltip,
  NSplit
} from 'naive-ui'

import { 
  SaveOutline as SaveIcon,
  ArrowBackOutline as ArrowBackIcon,
  ArrowForwardOutline as ArrowForwardIcon,
  TrashOutline as TrashIcon,
  HomeOutline as home,
  BowlingBallOutline as Bo,
  CaretForwardCircleOutline as forwardIcon,
  CaretDownCircleOutline as downIcon,
  DocumentTextOutline as textoutline,
  CubeOutline as cube,
  FolderOpenOutline as folder
} from '@vicons/ionicons5'

const props = defineProps(['mode'])

// 添加新状态
const showToolbar = ref(false)
const router = useRouter()
// 异步加载抽屉内容组件
const MaterialPanel = defineAsyncComponent(() => import('./Freedom/MaterialPanel.vue'))
const ComponentPanel = defineAsyncComponent(() => import('./Freedom/ComponentPanel.vue'))
const ProjectFilePanel = defineAsyncComponent(() => import('./Freedom/ProjectFilePanel.vue'))
const activeDrawer = ref(null)//默认状态是不会打开任何的抽屉

const modeLabels = {
  practice: '自由练习模式',
  challenge: '闯关模式',
  tutorial: '教学模式'
}

const components = [
  { type: 'AND', name: '与门' },
  { type: 'OR', name: '或门' },
  { type: 'NOT', name: '非门' },
  { type: 'XOR', name: '异或门' }
]

const modeLabel = computed(() => modeLabels[props.mode] || '自由练习模式')

const activeDrawerComponent = computed(() => {
  switch(activeDrawer.value){
    case 'material': return MaterialPanel
    case 'component': return ComponentPanel
    case 'project': return ProjectFilePanel
    default: return null
  }
})

// 导航栏相关方法
const goHome = () => {
  router.push({name : 'Home'})
}

const handleClick = () => {
  showToolbar.value = !showToolbar.value
}

const toggleDrawer = (drawerName) => {
  if (activeDrawer.value === drawerName) {
    activeDrawer.value = null // 点击已激活的抽屉则关闭
  } else {
    activeDrawer.value = drawerName // 激活新抽屉
  }
}

const saveProject = () => {
  console.log('保存项目')
}

const prevStep = () => {
  console.log('上一步')
}

const nextStep = () => {
  console.log('下一步')
}

const clearWorkspace = () => {
  console.log('清空工作区')
}

// 以下方法保持不变
const onDragStart = (event, component) => {
  event.dataTransfer.setData('component', JSON.stringify(component))
}

const onDrop = (event) => {
  const component = JSON.parse(event.dataTransfer.getData('component'))
  componentStore.addComponent({
    ...component,
    id: generateUniqueId(),
    x: event.offsetX,
    y: event.offsetY
  })
}

const generateUniqueId = () => Date.now().toString(36) + Math.random().toString(36).substr(2)
</script>

<style scoped>
.workspace-container {
  height: 100vh;
  width: 100vw;
  display: flex;
  flex-direction: column;
}

/* 导航栏容器 */
.navbar-container {
  background: #95bfe8;
  color: white;
}

.navbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0.8rem 1rem;
}

.nav-left {
  display: flex;
  align-items: center;
}

.nav-right {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.mode-label {
  font-size: 0.9rem;
}

/* 工具栏 */
.toolbar {
  display: flex;
  justify-content: flex-start;
  padding: 0.5rem 1rem;
  background: rgba(255, 255, 255, 0.1);
  border-top: 1px solid rgba(255, 255, 255, 0.2);
}

/* 以下样式保持不变 */
.main-content {
  flex: 1;
  display: flex;
}

/* 工具区 - 抽屉式 */
.toolbox {
  display: flex;
  width: 50px; /* 增加宽度以适应内容 */
  background: #b7daf1;
  border-right: 1px solid #ddd;
}

.drawer-buttons {
  display: flex;
  flex-direction: column;
  padding: 1rem 0.5rem;
  gap: 0.5rem;
  background: #98d5e0;
  border-right: 1px solid #ddd;
}

.drawer-button {
  width: 40px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.drawer-content {
  flex: 1;
  padding: 1rem;
  overflow-y: auto;
}

/* 自定义抽屉区域 */
.local-drawer {
  height: 100%;
  background: #dfb8b8;
  padding: 1rem;
  overflow-y: auto;
  box-sizing: border-box;
}

/* 分割容器样式 */
.drawer-container {
  flex: 1;
  position: relative;
  height: 100%;
}

.split-container {
  height: 100%;
  border-right: 1px solid #ddd;
}

/* 调整分割条样式 */
:deep(.n-split-trigger) {
  background-color: #95bfe8;
  width: 8px;
  transition: background-color 0.3s;
}

:deep(.n-split-trigger:hover) {
  background-color: #7aa3cc;
}

/* 动画效果 */
.slide-fade-enter-active,
.slide-fade-leave-active {
  transition: all 0.3s ease;
}
.slide-fade-enter-from,
.slide-fade-leave-to {
  transform: translateX(-20px);
  opacity: 0;
}


.canvas {
  flex: 1;
  position: relative;
  background: #fff;
}

.toolbox-item {
  padding: 1rem;
  margin: 0.5rem 0;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: move;
}
</style>