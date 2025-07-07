<template>
  <div class="workspace-container">
  <!--导航栏-->
  <NavHeader :mode="props.mode" :editorRef="canvasEditorRef"/>
  <template v-if="props.mode === 'tutorial'">
    <TeachingSelector v-if="!experiment" />
    <TeachingGuide v-else/>
  </template>
    <div v-if="props.mode !== 'tutorial' || experiment" class="main-content">
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

      <div class="drawer-container">
        <!-- 自定义抽屉 -->
        <transition name="slide-fade">
          <n-split
            v-model:size="drawerSize"
            :min=0
            :max="0.60"
            @drag-move="handleSplitDrag"
            direction="horizontal"
            class="split-container"
          >
          <template #1>
            <div class="local-drawer">
              <component :is="activeDrawerComponent"/>
            </div>
          </template>

          <template #2>
            <n-split 
            direction="horizontal"
            v-model:size="CanvasSize"
            :min="0.5"
            >
              <template #1>
                <div 
                  :key="idx" 
                  class="canvas">
                  <CanvasEditor ref="canvasEditorRef"/>
                </div>
              </template>
              <template #2 v-if="showRightPDF">
                <div class="pdf-wrapper">
                  <n-button 
                    quaternary
                    circle
                    size="small"
                    class="close-button"
                    @click="closePDF"
                    >
                    <template #icon>
                      <n-icon><close /></n-icon>
                    </template>
                  </n-button>
                  <PDFViewer :pdfFile="currentPdfFile" />
                </div>
              </template>
            </n-split>
          </template>
          </n-split>
        </transition>
      </div>
    </div>
  </div>
</template>

<script setup>
import CanvasEditor from '@/components/CanvasEditor.vue'
import PDFViewer from './Freedom/PDFViewer.vue'
import NavHeader from '@/components/Layout/NavHeader.vue'
import { computed, ref, defineAsyncComponent, provide, nextTick} from 'vue'
import { useRouter, useRoute } from 'vue-router'
import TeachingGuide from './Teaching/TeachingGuide.vue'
import TeachingSelector from './Teaching/TeachingSelector.vue'
import { 
  NButton, 
  NIcon, 
  NTooltip,
  NSplit,
} from 'naive-ui'

import { 
  DocumentTextOutline as textoutline,
  CubeOutline as cube,
  FolderOpenOutline as folder,
  CloseOutline as close,
} from '@vicons/ionicons5'
const props = defineProps(['mode'])
const route = useRoute()

// 异步加载抽屉内容组件
const MaterialPanel = defineAsyncComponent(() => import('./Freedom/MaterialPanel.vue'))
const ComponentPanel = defineAsyncComponent(() => import('./Freedom/ComponentPanel.vue'))
const ProjectFilePanel = defineAsyncComponent(() => import('./Freedom/ProjectFilePanel.vue'))
const activeDrawer = ref('component'); // 默认显示元件抽屉
const drawerSize = ref(0.1);
const CanvasSize = ref(1);
const showRightPDF = ref(false)
const currentPdfFile = ref(null)
const experiment = computed(() => route.query?.experiment)
provide('pdfState', {showRightPDF,currentPdfFile})

const toggleDrawer = (drawerName) => {
  if (activeDrawer.value === drawerName && drawerSize.value > 0) {
    activeDrawer.value = null 
    drawerSize.value = 0
  } else {
    activeDrawer.value = drawerName
    drawerSize.value = 0.1
  }
}

const togglePDF = (file) => {
  if(currentPdfFile.value == file && showRightPDF.value){
    CanvasSize.value = 1
    showRightPDF.value = false
  }else{
    CanvasSize.value = 0.7
    currentPdfFile.value = file
    showRightPDF.value = true
  }
}

provide('togglePDF', togglePDF)

const closePDF = () => {
  CanvasSize.value = 1
  showRightPDF.value = false
}


const activeDrawerComponent = computed(() => {
  switch(activeDrawer.value){
    case 'material': return MaterialPanel
    case 'component': return ComponentPanel
    case 'project': return ProjectFilePanel
    default: return null
  }
})
const handleSplitDrag = () => {
  if(drawerSize.value < 0.08){
    drawerSize.value = 0; 
  }
}

// 维护画布
import { useProjectStore } from '@/store/ProjectStore'
const projectStore = useProjectStore()
const projectIds = computed(() => projectStore.getProjectIds())
const canvasEditorRef = ref(null)

// function setCanvasEditorRef(projectId, el) {
//   if (el) {
//     canvasEditorRefs.set(projectId, el);
//   } else {
//     canvasEditorRefs.delete(projectId);
//   }
// }

</script>

<style scoped>
.workspace-container {
  height: 100vh;
  width: 100vw;
  display: flex;
  flex-direction: column;
  background: #F6F8FA;
  color: #2C3E50;
  font-family: 'Segoe UI','Helvetica Neue', Arial, sans-serif;
}





/* 以下样式保持不变 */
.main-content {
  flex: 1;
  display: flex;
}

/* 工具区 - 抽屉式 */
.toolbox {
  display: flex;
  width: 54px; /* 增加宽度以适应内容 */
  background: #ffffff;
  border-right: 1px solid #E0E6ED;
}

.drawer-buttons {
  display: flex;
  flex-direction: column;
  padding: 0.8rem 0.5rem;
  gap: 0.5rem;
  background: #ffffff;
  border-right: 1px solid #ddd;
}

.drawer-button {
  width: 40px;
  height: 40px;
  display: flex;
  align-items: center;
  justify-content: center;
  border-radius: 6px;
  transition: background-color 0.3s ease;
}

.drawer-button:hover{
  background-color: #E0E6ED;
}

.drawer-content {
  flex: 1;
  padding: 1rem;
  overflow-y: auto;
}

/* 自定义抽屉区域 */
.local-drawer {
  height: 100vh;
  background: #ffffff;
  border-right: 1px solid #E0E6ED;
  padding: 0;
  overflow-y: auto;
  box-sizing: border-box;
}

/* 分割容器样式 */
.drawer-container {
  flex: 1;
  position: relative;
  height: 100vh;
}

.split-container {
  height: 100vh;
  border-right: 1px solid #ddd;
}

/* 调整分割条样式 */
:deep(.n-split-trigger) {
  background-color: #E0E6ED;
  width: 6px;
  transition: background-color 0.3s;
}

:deep(.n-split-trigger:hover) {
  background-color: #CBD5E1;
}

/* 动画效果 */
.pdf-wrapper{
  position: relative;
  height: 100vh;
  width: 100%;
  display: flex;
  flex-direction: column;
  background: #ffffff;
  border-left: 1px solid #E0E6ED;
}

.close-button{
  position: absolute;
  top:10px;
  left: 10px;
  z-index:10;
  background-color: #E0E6ED;
}


.canvas {
  background: #fff;
  height: 100vh;
  width: auto;
}

.toolbox-item {
  padding: 0.8rem;
  margin: 0.5rem 0;
  background: #ffffff;
  border: 1px solid #E0E6ED;
  border-radius: 4px;
  cursor: move;
  transition: box-shadow 0.3s ease;
}

.toolbox-item:hover {
  box-shadow: 0 2px 6px rgba(0,0,0,0.1);
}
</style>