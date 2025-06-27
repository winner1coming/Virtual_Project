<template>
  <div class="editor-wrapper">
    <!-- 左侧工具栏 -->
    <div class="toolbar">
      <button @click="startPlacingVueComponent('AND')">AND</button>
      <button @click="startPlacingVueComponent('OR')">OR</button>
      <button @click="startPlacingVueComponent('NOT')">NOT</button>
      <button @click="startPlacingVueComponent('NOR')">NOR</button>
      <button @click="startPlacingVueComponent('XOR')">XOR</button>
      <button @click="startPlacingVueComponent('NAND')">NAND</button>
      <button @click="startPlacingVueComponent('XNOR')">XNOR</button>
      <button @click="startPlacingVueComponent('Splitter')">分离器</button>
      <button @click="clearComponents">清空</button>
      <button @click="undo">撤销</button>
      <button @click="redo">重做</button>
    </div>

    <!-- 画布区域 -->
    <div
      class="component-canvas"
      ref="canvasContainer"
      @mousedown="handleMouseDown"
      @mousemove="handleMouseMove"
      @mouseup="handleMouseUp"
      @contextmenu.prevent="handleRightClick"
    >
      <!-- 渲染 Vue 元件组件 -->
      <svg width="800" height="600">
        <g
          v-for="(item, index) in components"
          :key="index"
          :transform="`translate(${item.x}, ${item.y})`"
          @mousedown.prevent.stop="selectComponent(item, $event)"
        >
          <component
            :is="item.type"
            :inputs="item.inputs"
            :output="item.output"
            :onToggleInput="(i) => toggleInput(item, i)"
          />
        </g>
      </svg>

      <!-- 拖动时的预览阴影 -->
      <div
        v-if="currentComponent"
        class="preview-shadow"
        :style="{ left: previewPos.x + 'px', top: previewPos.y + 'px' }"
      >投影</div>
    </div>
  </div>
</template>

<script setup>
import { ref, reactive, onMounted } from 'vue'
import AndGate from './AndGate.vue'
import OrGate from './OrGate.vue'
import NotGate from './NotGate.vue'
import NorGate from './NorGate.vue'
import XorGate from './XorGate.vue'
import NandGate from './NandGate.vue'
import XnorGate from './XnorGate.vue'
import Splitter from './Wiring/Splitter.vue'

const canvasContainer = ref(null)
const components = reactive([])
const undoStack = reactive([])
const redoStack = reactive([])
const currentComponent = ref(null)
const selectedComponent = ref(null)
const isDragging = ref(false)
const dragOffset = reactive({ x: 0, y: 0 })
const previewPos = reactive({ x: 0, y: 0 })

const componentMap = {
  AND: AndGate,
  OR: OrGate,
  NOT: NotGate,
  NOR: NorGate,
  XOR: XorGate,
  NAND: NandGate,
  XNOR: XnorGate,
  Splitter: Splitter,
}

function startPlacingVueComponent(type) {
  const Component = componentMap[type]
  if (Component) {
    currentComponent.value = {
      type: Component,
      x: 0,
      y: 0,
      inputs: type === 'NOT' ? [{ id: 1, value: false }] : [{ id: 1, value: false }, { id: 2, value: false }],
      output: false
    }
  }
}

function handleMouseDown(event) {
  const rect = canvasContainer.value.getBoundingClientRect()
  const x = event.clientX - rect.left
  const y = event.clientY - rect.top

  if (currentComponent.value) {
    components.push({ ...currentComponent.value, x, y })
    saveHistory()
    currentComponent.value = null
  }
}

function handleMouseMove(event) {
  const rect = canvasContainer.value.getBoundingClientRect()
  const x = event.clientX - rect.left
  const y = event.clientY - rect.top

  if (currentComponent.value) {
    previewPos.x = x
    previewPos.y = y
  } else if (isDragging.value && selectedComponent.value) {
    selectedComponent.value.x = x - dragOffset.x
    selectedComponent.value.y = y - dragOffset.y
  }
}

function handleMouseUp() {
  if (isDragging.value) {
    saveHistory()
  }
  isDragging.value = false
}

function selectComponent(item, event) {
  const rect = canvasContainer.value.getBoundingClientRect()
  const x = event.clientX - rect.left
  const y = event.clientY - rect.top
  selectedComponent.value = item
  dragOffset.x = x - item.x
  dragOffset.y = y - item.y
  isDragging.value = true
}


function handleRightClick(event) {
  const rect = canvasContainer.value.getBoundingClientRect()
  const x = event.clientX - rect.left
  const y = event.clientY - rect.top
  const index = components.findIndex(
    (c) => x >= c.x && x <= c.x + 60 && y >= c.y && y <= c.y + 60
  )
  if (index !== -1) {
    components.splice(index, 1)
    saveHistory()
  }
}

function clearComponents() {
  if (components.length > 0) {
    saveHistory()
    components.splice(0)
  }
}

function saveHistory() {
  undoStack.push(JSON.stringify(components))
  redoStack.length = 0
}

function undo() {
  if (undoStack.length === 0) return
  redoStack.push(JSON.stringify(components))
  const prev = undoStack.pop()
  Object.assign(components, JSON.parse(prev))
}

function redo() {
  if (redoStack.length === 0) return
  undoStack.push(JSON.stringify(components))
  const next = redoStack.pop()
  Object.assign(components, JSON.parse(next))
}

function toggleInput(component, index) {
  component.inputs[index].value = !component.inputs[index].value
  if (component.type === AndGate) {
    component.output = component.inputs.every(input => input.value)
  } else if (component.type === OrGate) {
    component.output = component.inputs.some(input => input.value)
  } else if (component.type === NotGate) {
    component.output = !component.inputs[0].value
  }
}
</script>

<style scoped>
.editor-wrapper {
  display: flex;
}
.toolbar {
  display: flex;
  flex-direction: column;
  gap: 10px;
  margin-right: 10px;
}
.component-canvas {
  position: relative;
  width: 800px;
  height: 600px;
  border: 1px solid #ccc;
  background-color: #f0f0f0;
}
.preview-shadow {
  position: absolute;
  width: 60px;
  height: 60px;
  background-color: rgba(0, 0, 0, 0.2);
  border: 1px dashed #333;
  pointer-events: none;
}
.selected {
  outline: 2px solid #007bff;
}
</style>
