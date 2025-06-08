<template>
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 600" width="300" height="300" style="overflow: visible;">
    <g :transform="`translate(${orGate.x}, ${orGate.y}) scale(${orGate.scale})`" cursor="move">
      <!-- OR 门图形 -->
      <path stroke="black" stroke-width="12" d="M145.999 181L315.999 181" />
      <path fill="none" stroke="black" stroke-width="12" d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739" />
      <path stroke="black" stroke-width="12" d="M440 288L497 288" />
      <path stroke="black" stroke-width="12" d="M368.226 198.885L450.774 292.115" />
      <path stroke="black" stroke-width="12" d="M146 395.115L316 395.115" />
      <path fill="none" stroke="black" stroke-width="12" d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375" />
      <path stroke="black" stroke-width="12" d="M368.227 377.23L450.775 284" />
      <path fill="none" stroke="black" stroke-width="12" d="M149 179.5C188.3 212.65 273.5 295 149 397" />
      <!-- <path stroke="black" stroke-width="12" d="M92 326L202 326" />
      <path stroke="black" stroke-width="12" d="M92 246L202 246" /> -->
      <!-- 上方引脚竖线 -->
      <path
        v-if="minY < bezierYMin"
        :d="`M149 ${minY-6}L149 ${bezierYMin+6}`"
        stroke="black"
        stroke-width="12"
      />
      <!-- 下方引脚竖线 -->
      <path
        v-if="maxY > bezierYMax"
        :d="`M149 ${bezierYMax-6}L149 ${maxY+6}`"
        stroke="black"
        stroke-width="12"
      />
      <!-- 输入引脚 -->
      <template v-for="(input, index) in orGate.inputs" :key="input.id">
        <circle
          v-if="input.inverted"
          :cx="`${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-26}`"
          :cy="inputYs[index]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path
          v-if="input.inverted"
          :d="`M${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x1} ${inputYs[index]}L${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2-36} ${inputYs[index]}`"
          stroke="black"
          stroke-width="12"
          />
        <path
          v-if="!input.inverted"
          :d="`M${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x1} ${inputYs[index]}L${getInputLine(index, inputYs[index], bezierYMin, bezierYMax).x2} ${inputYs[index]}`"
          stroke="black"
          stroke-width="12"
        />
        <InputPort
          :cx="92"
          :cy="inputYs[index]"
          :active="input.value"
          @toggle="() => handleToggleInput(index)"
        />
      </template>



      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="orGate.output" />
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

const orGate = reactive({
  x: 0,
  y: 0,
  scale: 1,
  inputCount: 8,
  inputs: [],
  output: false,
})

let inputYs = useGateLayout(orGate.inputCount)
const bezierYMin = 179.5;
const bezierYMax = 397;
let minY = Math.min(...inputYs.value);
let maxY = Math.max(...inputYs.value);

// handleSetInputInverted(1, true);
// handleSetScale(0.5)

function handleToggleInput(index) {
  toggleInput(orGate, index, updateOutput)
  // setInputCount(9)
  // handleSetInputInverted(index, true)
  // handleSetScale(0.5)
}

function handleSetInputInverted(index, inverted) {
  setInputInverted(orGate, index, inverted, updateOutput)
}

function handleSetScale(newscale)
{
  setScale(orGate, newscale)
}

function updateOutput() {
  orGate.output = orGate.inputs.some(input =>
    input.inverted ? !input.value : input.value
  )
}

function setInputCount(newCount)
{
  orGate.inputCount = newCount;
  inputYs = useGateLayout(orGate.inputCount)

  minY = Math.min(...inputYs.value);
  maxY = Math.max(...inputYs.value);
}

onMounted(()=>{
 orGate.inputs = computed(()=>createInputs(orGate.inputCount));
})

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
