<template>
  <div class="workspace-container">
    <!-- 导航栏 -->
    <nav class="navbar">
      <div class="nav-left">
        <router-link to="/" class="nav-item">首页</router-link>
        <button @click="saveProject" class="nav-item">保存</button>
      </div>
      <div class="nav-right">
        <span>当前模式：{{ modeLabel }}</span>
      </div>
    </nav>

    <!-- 主工作区 -->
    <div class="main-content">
      <!-- 工具区 -->
      <div class="toolbox">
        <div 
          v-for="component in components"
          :key="component.type"
          class="toolbox-item"
          draggable="true"
          @dragstart="onDragStart($event, component)"
        >
          {{ component.name }}
        </div>
      </div>

      <!-- 实验区 -->
      <div 
        class="canvas"
        @dragover.prevent
        @drop="onDrop"
      >
        
      </div>
    </div>
  </div>
</template>

<script setup>
import { computed } from 'vue'

const props = defineProps(['mode'])

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

.navbar {
  display: flex;
  justify-content: space-between;
  padding: 1rem;
  background: #2c3e50;
  color: white;
}

.main-content {
  flex: 1;
  display: flex;
}

.toolbox {
  width: 250px;
  padding: 1rem;
  background: #f5f6fa;
  border-right: 1px solid #ddd;
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