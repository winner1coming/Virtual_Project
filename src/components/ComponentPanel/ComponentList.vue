<template>
  <div class="component-panel">
    <n-collapse accordion="true">
      <n-collapse-item title="逻辑门" name="1">
        <div
          v-for="component in components1"
          :key="component.type"
          class="component-item"
          @click="onDragStart(component.type)"
        >
          {{ component.name }}
        </div>
      </n-collapse-item>
      <n-collapse-item title="输入/输出" name="2">
        <div
          v-for="component in components2"
          :key="component.type"
          class="component-item"
          @click="onDragStart(component.type)"
        >
          {{ component.name }}
        </div>
      </n-collapse-item>
      <n-collapse-item title="线路" name="3">
        <div
          v-for="component in components3"
          :key="component.type"
          class="component-item"
          @click="onDragStart(component.type)"
        >
          {{ component.name }}
        </div>
      </n-collapse-item>
      <n-collapse-item title="存储" name="4">
        <div
          v-for="component in components4"
          :key="component.type"
          class="component-item"
          @click="onDragStart(component.type)"
        >
          {{ component.name }}
        </div>
      </n-collapse-item>
    </n-collapse>
    
  </div>
</template>

<script setup>
import { NCollapse, NCollapseItem } from 'naive-ui';
import eventBus from '@/modules/useEventBus';

const emit = defineEmits(['dragstart']);


import { getComponentsByCategory } from '@/registry'

const components1 = getComponentsByCategory('gate')
const components2 = getComponentsByCategory('io')
const components3 = getComponentsByCategory('wiring')
const components4 = getComponentsByCategory('memory')


const onDragStart = (type) => {
  // event.dataTransfer.setData('component', JSON.stringify(component));
  eventBus.emit('start-place-component', {type: type, projectId:0}); // 发送拖动事件
};
</script>

<style scoped>
.component-panel {
  background-color: #ffffff;
  height: 100%;
  overflow-y: auto;
  padding-top: 20px;
}

.component-item {
  padding: 10px;
  margin: 8px 5px;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: move;
}

.component-item:hover {
  background-color: #f0f5ff;
}
</style>