<template>
    <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 600" width="300" height="300" style="overflow: visible;">
      <g :transform="`translate(${xorGate.x}, ${xorGate.y}) scale(${xorGate.scale})`" cursor="move">
        <!-- XOR 门图形 -->
        <rect x="0" y="0" width="0" height="0" fill="rgba(249, 249, 249, 1)" />
        <path fill="none" stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M145.999 181L315.999 181">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M440 288L497 288">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M368.226 198.885L450.774 292.115">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M146 395.115L316 395.115">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M368.227 377.23L450.775 284">
        </path>
        <path fill="none" stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149 179.5C188.3 212.65 273.5 295 149 397">
        </path>
        <!-- <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M42 326L152 326">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M42 246L152 246">
        </path> -->
        <path fill="none" stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M97 181C136.3 214.15 221.5 296.5 97 398.5">
        </path>
        <!-- 上方引脚竖线 -->
        <path
          v-if="minY < bezierYMin"
          :d="`M99 ${minY-6}L99 ${bezierYMin+6}`"
          stroke="black"
          stroke-width="12"
        />
        <!-- 下方引脚竖线 -->
        <path
          v-if="maxY > bezierYMax"
          :d="`M99 ${bezierYMax-6}L99 ${maxY+6}`"
          stroke="black"
          stroke-width="12"
        />
        <!-- 输入引脚 -->
        <template v-for="(input, index) in xorGate.inputs" :key="input.id">
          <circle
            v-if="input.inverted"
            :cx="`${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-76}`"
            :cy="inputYs[index]"
            r="16"
            stroke="black"
            stroke-width="12"
            fill="none"
          />
          <path
            v-if="input.inverted"
            :d="`M${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x1-50} ${inputYs[index]}L${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-86} ${inputYs[index]}`"
            stroke="black"
            stroke-width="12"
            />
          <path
            v-if="!input.inverted"
            :d="`M${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x1-50} ${inputYs[index]}L${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-50} ${inputYs[index]}`"
            stroke="black"
            stroke-width="12"
          />
          <InputPort
            :cx="42"
            :cy="inputYs[index]"
            :active="input.value"
            @toggle="() => handleToggleInput(index)"
          />
        </template>
  
  
  
        <!-- 输出状态 -->
        <OutputPort :cx="497" :cy="288" :active="xorGate.output" />
      </g>
    </svg>
  </template>
  
  <script setup>
  import { reactive, computed, onMounted } from 'vue'
  import InputPort from './InputPort.vue'
  import OutputPort from './OutputPort.vue'
  import {
    createInputs,
    setInputValue,
    toggleInput,
    setInputInverted,
    setScale
  } from '@/logic/usegates/useLogicGates'
  import { useGateLayout, getInputLine } from '@/logic/usegates/useGateLayout'
  
  //const inputCount = 3
  //console.log(computedLineBottom, computedLineTop)
  
  const xorGate = reactive({
    x: 0,
    y: 0,
    scale: 1,
    inputCount: 8,
    inputs: [],
    output: false,
  })
  
  let inputYs = useGateLayout(xorGate.inputCount)
  const bezierYMin = 179.5;
  const bezierYMax = 397;
  let minY = Math.min(...inputYs.value);
  let maxY = Math.max(...inputYs.value);
  
  // handleSetInputInverted(1, true);
  // handleSetScale(0.5)
  
  function handleToggleInput(index) {
    toggleInput(xorGate, index, updateOutput)
    // setInputCount(2)
    // handleSetInputInverted(index, true)
    // handleSetScale(0.5)
  }
  
  function handleSetInputInverted(index, inverted) {
    setInputInverted(xorGate, index, inverted, updateOutput)
  }
  
  function handleSetScale(newscale)
  {
    setScale(xorGate, newscale)
  }
  
  function updateOutput() {
    xorGate.output = xorGate.inputs.reduce((acc, input) => {
        const value = input.inverted ? !input.value : input.value
        return acc ^ value
    }, false)
  }

  
  function setInputCount(newCount)
  {
    xorGate.inputCount = newCount;
    inputYs = useGateLayout(xorGate.inputCount)
  
    minY = Math.min(...inputYs.value);
    maxY = Math.max(...inputYs.value);
  }
  
  onMounted(()=>{
   xorGate.inputs = computed(()=>createInputs(xorGate.inputCount));
  })
  
  </script>
  
  <style scoped>
  svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
  }
  </style>
  