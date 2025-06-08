<template>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 600 600"
      width="600"
      height="600"
    >
      <g :transform="`translate(${norGate.x}, ${norGate.y})`" cursor="move">
        <!-- NOR 门图形 -->
        <path stroke="black" stroke-width="12" d="M145.999 181L315.999 181" />
        <path fill="none" stroke="black" stroke-width="12" d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739" />
        <!--00<path stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M534 286L491 286" />-->
        <path stroke="black" stroke-width="12" d="M368.226 198.885L450.774 292.115" />
        <path stroke="black" stroke-width="12" d="M146 395.115L316 395.115" />
        <path fill="none" stroke="black" stroke-width="12" d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375" />
        <path stroke="black" stroke-width="12" d="M368.227 377.23L450.775 284" />
        <path fill="none" stroke="black" stroke-width="12" d="M149 179.5C188.3 212.65 273.5 295 149 397" />
        <path stroke="black" stroke-width="12" d="M92 326L202 326" />
        <path stroke="black" stroke-width="12" d="M92 246L202 246" />
        <ellipse cx="470.2452697753906" cy="286.0000305175781" rx="20.754730224609375" ry="20" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#CCCCCC" fill-opacity="0">
        </ellipse>
  
        <!-- 输入状态 -->
        <InputPort :cx="92" :cy="246" :active="norGate.input1" @toggle="() => toggleInput('input1')" />
        <InputPort :cx="92" :cy="326" :active="norGate.input2" @toggle="() => toggleInput('input2')" />
  
        <!-- 输出状态 -->
        <OutputPort :cx="497" :cy="288" :active="norGate.output" />
      </g>
    </svg>
  </template>
  
  <script setup>
  import { ref } from 'vue'
  import InputPort from './InputPort.vue'
  import OutputPort from './OutputPort.vue'
  
  const norGate = ref({
    x: 0,
    y: 0,
    input1: false,
    input2: false,
    output: false,
  })
  
  function toggleInput(port) {
    if (port === 'input1') {
      norGate.value.input1 = !norGate.value.input1
    } else if (port === 'input2') {
      norGate.value.input2 = !norGate.value.input2
    }
    norGate.value.output = !(norGate.value.input1 || norGate.value.input2)
  }
  
  const dragging = ref(false)
  const offset = ref({ x: 0, y: 0 })
  
  // function onMouseDown(e) {
  //   dragging.value = true
  //   offset.value = {
  //     x: e.offsetX - norGate.value.x,
  //     y: e.offsetY - norGate.value.y,
  //   }
  // }
  
  // function onMouseMove(e) {
  //   if (dragging.value) {
  //     norGate.value.x = e.offsetX - offset.value.x
  //     norGate.value.y = e.offsetY - offset.value.y
  //   }
  // }
  
  // function onMouseUp() {
  //   dragging.value = false
  // }
  </script>
  
  <style scoped>
  svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
  }
  </style>