<template>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
    >
      <g :transform="`translate(${notGate.x}, ${notGate.y}) scale(${notGate.scale})`" cursor="move">
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
        <!-- 输入 -->
        <circle
          v-if="notGate.input.inverted"
          :cx="149 - 26"
          :cy="288"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!notGate.input.inverted" stroke="black" stroke-width="12" d="M93 288L150 288" />
  
        <!-- 输入状态端口 -->
        <InputPort :cx="93" :cy="288" :active="notGate.input.value" @toggle="handleToggleInput" />
        <!-- 输出状态端口 -->
        <OutputPort :cx="497" :cy="286" :active="notGate.output" />
      </g>
    </svg>
</template>
  
<script setup>
import { reactive } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import {
  setInputValue,
  toggleInput,
  setInputInverted,
  setScale
} from '@/logic/usegates/useLogicGates'
// import { useGateLayout } from '@/logic/usegates/useGateLayout'

const notGate = reactive({
    x: 0,
    y: 0,
    input: reactive({
      id: 0,
      value: false,
      inverted: false,
    }),
    output: true,
    scale: 1,
})


function handleToggleInput() {
  notGate.input.value = !notGate.input.value
  updateOutput()
  //handleSetInputInverted(!notGate.input.inverted);
  //handleSetScale(0.5);
}

function handleSetInputValue(value) {
  setInputValue(notGate, 0, value, updateOutput)
}

function handleSetInputInverted(inverted) {
  notGate.input.inverted = inverted
  updateOutput()
}

function handleSetScale(newscale){
  setScale(notGate, newscale)
}

function updateOutput() {
  notGate.output = notGate.input.inverted? notGate.input.value : !notGate.input.value
}

</script>

<style scoped>
    svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
    }
</style>
