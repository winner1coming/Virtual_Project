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
      <path
        stroke="black"
        stroke-width="12"
        :d="`M149 ${computedLineTop-6}L149 ${computedLineBottom+6}`"
      />
      <path stroke="black" stroke-width="12" d="M149 395L372 395" />
      <path stroke="black" stroke-width="12" d="M149 181L372 181" />
      <path
        fill="none"
        stroke="black"
        stroke-width="12"
        d="M366.5 180C507 259.301 410.965 399.972 366.5 395"
      />
      <path stroke="black" stroke-width="12" d="M440 288L497 288" />

      <!-- 输入引脚 -->
      <template v-for="(input, index) in andGate.inputs" :key="input.id">
        <path :d="`M92 ${inputYs[index]}L149 ${inputYs[index]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="inputYs[index]" :active="input.value" @toggle="() => toggleInput(index)" />
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

const inputCount = 4 // 输入个数可调整
const baseTop = 181
const baseBottom = 395
const baseHeight = baseBottom - baseTop
const idealSpacing = 50
const idealHeight = (inputCount + 1) * idealSpacing

const computedLineTop = computed(() => {
  return idealHeight > baseHeight ? 288 - idealHeight / 2 : baseTop
})
const computedLineBottom = computed(() => {
  return idealHeight > baseHeight ? 288 + idealHeight / 2 : baseBottom
})

const inputYs = computed(() => {
  const top = computedLineTop.value
  const bottom = computedLineBottom.value
  return Array.from({ length: inputCount }, (_, i) => {
    return top + ((bottom - top) / (inputCount + 1)) * (i + 1)
  })
})

const andGate = ref({
  x: 0,
  y: 0,
  inputs: Array.from({ length: inputCount }, (_, i) => ({ id: `in${i}`, value: false })),
  output: false,
})
// 每个输入引脚都有唯一 id，方便用外部数据更新，比如 andGate.inputs.find(i => i.id === 'in2').value = true,
// 日后可以扩展属性，比如 label, connected, position 等

function setInputValue(index, value) {
  if (index < 0 || index >= andGate.value.inputs.length) return
  andGate.value.inputs[index].value = value
  andGate.value.output = andGate.value.inputs.every(input => input.value)
}
// 用法：setInputValue(0, true)  // 设置第 0 个引脚为高电平
//      setInputValue(1, false) // 设置第 1 个引脚为低电平


function toggleInput(index) {
  andGate.value.inputs[index].value = !andGate.value.inputs[index].value
  andGate.value.output = andGate.value.inputs.every(input => input.value)
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
