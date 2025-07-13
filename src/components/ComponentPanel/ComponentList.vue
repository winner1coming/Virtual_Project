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


const components1 = [
  { type: 'AND', name: '与门' },
  { type: 'OR', name: '或门' },
  { type: 'NOT', name: '非门' },
  { type: 'XOR', name: '异或门' },
  {type: 'NXOR', name: '异或非门'},
  {type: 'NAND', name: '与非门'},
  {type: 'NOR', name: '或非门'},
];
const components2 = [
  { type: 'INPUT', name: '输入引脚' },
  { type: 'OUTPUT', name: '输出引脚' },
  { type: 'CLOCK', name: '时钟' },
  { type: 'CONSTANT', name: '常量'},
  { type: 'POWER', name: '电源' },
  { type: 'GROUND', name: '接地' },
  {type: 'BUTTON', name: '按钮'},
  {type: 'LIGHT', name: '发光二极管'},
  { type: 'SEGMENT_DISPLAY', name: '七段数码器' },
  {type: 'HEX_DISPLAY', name: '十六进制数码管'},
];
const components3 = [
  {type: 'TUNNEL', name: '隧道'},
  { type: 'SPLITTER', name : '分离器'},
  { type: 'COMBINER', name: '合并器'},
]
const components4 = [
  {type: 'REGISTER', name: '寄存器'},
  {type: 'D_FLIP_FLOP', name: 'D触发器'},
]


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