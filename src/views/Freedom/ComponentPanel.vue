<template>
  <n-split
    direction="vertical"
    class="split-container"
    :default-sizes="[50, 50]" 
    style="height: 100%;"
    :max="0.8"
    :min="0.2"
  >
    <!-- 上半部分：电子元件列表 -->
    <template #1>
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

    <!-- 下半部分：元件属性 -->
    <template #2>
      <Properties />
    </template>
  </n-split>
</template>

<script setup>
import { NButton, NSplit } from 'naive-ui';
import Properties from '@/components/ComponentPanel/Properties.vue';

const props = defineProps({
  components: {
    type: Array,
    required: true,
    default: () => [
      { type: 'AND', name: '与门' },
      { type: 'OR', name: '或门' },
      { type: 'NOT', name: '非门' },
      { type: 'XOR', name: '异或门' }
    ]
  }
});

const onDragStart = (event, component) => {
  event.dataTransfer.setData('component', JSON.stringify(component));
};
</script>

<style scoped>
.split-container {
  height: 100%;
  background-color: transparent;
}

.component-panel {
  padding: 10px;
  background-color: transparent;
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