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
      <g :transform="`translate(${notGate.x}, ${notGate.y})`" cursor="move">
        <!-- NOT门主体（三角形） -->
        <path
          d="M438.075 288.5L147.769 392.856L147.769 184.144L438.075 288.5Z"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <!-- 输出端圆圈 -->
        <ellipse
          cx="470.245"
          cy="286"
          rx="20.75"
          ry="20"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <!-- 输入导线 -->
        <path stroke="black" stroke-width="12" d="M93 288L150 288" />
  
        <!-- 输入状态端口 -->
        <InputPort :cx="93" :cy="288" :active="notGate.input" @toggle="toggleInput" />
        <!-- 输出状态端口 -->
        <OutputPort :cx="491" :cy="286" :active="notGate.output" />
      </g>
    </svg>
</template>
  
<script setup>
import { ref } from 'vue'
import InputPort from './InputPort.vue'
import OutputPort from './OutputPort.vue'

const notGate = ref({
    x: 0,
    y: 0,
    input: false,
    output: true,
})

function toggleInput() {
    notGate.value.input = !notGate.value.input
    notGate.value.output = !notGate.value.input
}

const dragging = ref(false)
const offset = ref({ x: 0, y: 0 })

function onMouseDown(e) {
    dragging.value = true
    offset.value = {
        x: e.offsetX - notGate.value.x,
        y: e.offsetY - notGate.value.y,
    }
}

function onMouseMove(e) {
    if (dragging.value) {
        notGate.value.x = e.offsetX - offset.value.x
        notGate.value.y = e.offsetY - offset.value.y
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
