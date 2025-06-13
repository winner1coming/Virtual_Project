<template>
  <div class="component-panel">
    <h3>电子元件</h3>
    <div
      v-for="component in components"
      :key="component.type"
      class="component-item"
      draggable="true"
      @dragstart="onDragStart($event, component)"
    >
      {{ component.name }}
    </div>
  </div>
</template>

<script setup>
import { NCollapse, NCollapseItem } from 'naive-ui';

const emit = defineEmits(['dragstart']);


const components = [
  { type: 'INPUT', name: '输入端' },
  { type: 'OUTPUT', name: '输出端' },
  { type: 'CLOCK', name: '时钟' },
  { type: 'REGISTER', name: '寄存器' },
  { type: 'MUX', name: '多路选择器' },
  { type: 'DEMUX', name: '解多路选择器' },
  { type: 'AND', name: '与门' },
  { type: 'OR', name: '或门' },
  { type: 'NOT', name: '非门' },
  { type: 'XOR', name: '异或门' },
  { type: 'AND', name: '与门' },
  { type: 'OR', name: '或门' },
  { type: 'NOT', name: '非门' },
  { type: 'XOR', name: '异或门' }
];


const onDragStart = (event, component) => {
  event.dataTransfer.setData('component', JSON.stringify(component));
};
</script>

<style scoped>
.component-panel {
  padding: 10px;
  background-color: transparent;
  height: 100%;
  overflow-y: auto;
  padding-right: 5px;
}

.component-item {
  padding: 10px;
  margin: 8px 0;
  background: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  cursor: move;
}

.component-item:hover {
  background-color: #f0f5ff;
}
</style>