<template>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 600 600"
    width="600"
    height="600"
    @mousedown="onMouseDown"
    @mousemove="onMouseMove"
    @mouseup="onMouseUp"
  >
    <g :transform="`translate(${andGate.x}, ${andGate.y})`" cursor="move">
      <!-- AND 门图形 -->
       <!-- 放置引脚的左侧竖线 -->
      <path
        stroke="black"
        stroke-width="12"
        :d="`M149 ${computedLineTop+6}L149 ${computedLineBottom-6}`"
      />
      <!-- 上下两条横线 -->
      <path stroke="black" stroke-width="12" d="M149 395L372 395" />
      <path stroke="black" stroke-width="12" d="M149 181L372 181" />
      <!-- 圆弧 -->
      <path
        fill="none"
        stroke="black"
        stroke-width="12"
        d="M366.5 180C507 259.301 410.965 399.972 366.5 395"
      />
      <!-- 输出横线 -->
      <path stroke="black" stroke-width="12" d="M440 288L497 288" />

      <!-- 输入引脚 -->
      <template v-for="(y, index) in inputYs" :key="index">
        <path :d="`M92 ${y}L149 ${y}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="y" :active="andGate.inputs[index]" @toggle="() => toggleInput(index)" />
      </template>

      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="andGate.output" />
    </g>
  </svg>
</template>

<script setup>
import { ref, computed } from 'vue'
import InputPort from './InputPort.vue'
import OutputPort from './OutputPort.vue'

const inputCount = 2 // 你可以在这里调整输入个数
const baseTop = 175
const baseBottom = 401
const baseHeight = baseBottom - baseTop + 12

// 计算理想高度和输入点位置
const idealSpacing = 40
const idealHeight = (inputCount - 1) * idealSpacing + inputCount*12

const computedLineTop = computed(() => {
  return idealHeight > baseHeight ? 288 - idealHeight / 2: baseTop
})
const computedLineBottom = computed(() => {
  return idealHeight > baseHeight ? 288 + idealHeight / 2: baseBottom
})

const inputYs = computed(() => {
  // const top = computedLineTop.value
  // const bottom = computedLineBottom.value
  return Array.from({ length: inputCount }, (_, i) => {
    // return top + ((bottom - top) / (inputCount + 1)) * (i + 1)
    return 288 + idealSpacing*(i-(inputCount-1)/2)
  })
})

const andGate = ref({
  x: 0,
  y: 0,
  inputs: Array(inputCount).fill(false),
  output: false,
})

function toggleInput(index) {
  andGate.value.inputs[index] = !andGate.value.inputs[index]
  andGate.value.output = andGate.value.inputs.every(val => val)
}

const dragging = ref(false)
const offset = ref({ x: 0, y: 0 })

function onMouseDown(e) {
  dragging.value = true
  offset.value = {
    x: e.offsetX - andGate.value.x,
    y: e.offsetY - andGate.value.y,
  }
}

function onMouseMove(e) {
  if (dragging.value) {
    andGate.value.x = e.offsetX - offset.value.x
    andGate.value.y = e.offsetY - offset.value.y
  }
}

function onMouseUp() {
  dragging.value = false
}
</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
