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
      <g :transform="`translate(${orGate.x}, ${orGate.y})`" cursor="move">
        <!-- OR 门图形 -->
        <path stroke="black" stroke-width="12" d="M145.999 181L315.999 181" />
        <path fill="none" stroke="black" stroke-width="12" d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739" />
        <path stroke="black" stroke-width="12" d="M440 288L497 288" />
        <path stroke="black" stroke-width="12" d="M368.226 198.885L450.774 292.115" />
        <path stroke="black" stroke-width="12" d="M146 395.115L316 395.115" />
        <path fill="none" stroke="black" stroke-width="12" d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375" />
        <path stroke="black" stroke-width="12" d="M368.227 377.23L450.775 284" />
        <path fill="none" stroke="black" stroke-width="12" d="M149 179.5C188.3 212.65 273.5 295 149 397" />
        <path stroke="black" stroke-width="12" d="M92 326L202 326" />
        <path stroke="black" stroke-width="12" d="M92 246L202 246" />
  
        <!-- 输入状态 -->
        <InputPort :cx="92" :cy="246" :active="orGate.input1" @toggle="() => toggleInput('input1')" />
        <InputPort :cx="92" :cy="326" :active="orGate.input2" @toggle="() => toggleInput('input2')" />
  
        <!-- 输出状态 -->
        <OutputPort :cx="497" :cy="288" :active="orGate.output" />
      </g>
    </svg>
  </template>
  
  <script setup>
  import { ref } from 'vue'
  import InputPort from './InputPort.vue'
  import OutputPort from './OutputPort.vue'
  
  const orGate = ref({
    x: 0,
    y: 0,
    input1: false,
    input2: false,
    output: false,
  })
  
  function toggleInput(port) {
    if (port === 'input1') {
      orGate.value.input1 = !orGate.value.input1
    } else if (port === 'input2') {
      orGate.value.input2 = !orGate.value.input2
    }
    orGate.value.output = orGate.value.input1 || orGate.value.input2
  }
  
  const dragging = ref(false)
  const offset = ref({ x: 0, y: 0 })
  
  function onMouseDown(e) {
    dragging.value = true
    offset.value = {
      x: e.offsetX - orGate.value.x,
      y: e.offsetY - orGate.value.y,
    }
  }
  
  function onMouseMove(e) {
    if (dragging.value) {
      orGate.value.x = e.offsetX - offset.value.x
      orGate.value.y = e.offsetY - offset.value.y
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