<template>
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 600 600" width="300" height="300" style="overflow: visible;">
    <g :transform="`translate(${norGate.x}, ${norGate.y}) scale(${norGate.scale})`" cursor="move">
      <!-- NOR 门图形 -->
      <path stroke="black" stroke-width="12" d="M145.999 181L315.999 181" />
      <path fill="none" stroke="black" stroke-width="12" d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739" />
      <path stroke="black" stroke-width="12" d="M368.226 198.885L450.774 292.115" />
      <path stroke="black" stroke-width="12" d="M146 395.115L316 395.115" />
      <path fill="none" stroke="black" stroke-width="12" d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375" />
      <path stroke="black" stroke-width="12" d="M368.227 377.23L450.775 284" />
      <path fill="none" stroke="black" stroke-width="12" d="M149 179.5C188.3 212.65 273.5 295 149 397" />
      <ellipse cx="470.2452697753906" cy="286.0000305175781" rx="20.754730224609375" ry="20" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#CCCCCC" fill-opacity="0">
      </ellipse>
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
      <template v-for="(input, index) in norGate.inputs" :key="input.id">
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
      <OutputPort :cx="497" :cy="288" :active="norGate.output" />
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

const norGate = reactive({
  x: 0,
  y: 0,
  scale: 1,
  inputCount: 8,
  inputs: [],
  output: false,
})

let inputYs = useGateLayout(norGate.inputCount)
const bezierYMin = 179.5;
const bezierYMax = 397;
let minY = Math.min(...inputYs.value);
let maxY = Math.max(...inputYs.value);

function handleToggleInput(index) {
  toggleInput(norGate, index, updateOutput)
  // setInputCount(9)
  // handleSetInputInverted(index, true)
  // handleSetScale(0.5)
}

function handleSetInputInverted(index, inverted) {
  setInputInverted(norGate, index, inverted, updateOutput)
}

function handleSetScale(newscale)
{
  setScale(norGate, newscale)
}

function updateOutput() {
  norGate.output = !norGate.inputs.some(input =>
    input.inverted ? !input.value : input.value
  )
}

function setInputCount(newCount)
{
  norGate.inputCount = newCount;
  inputYs = useGateLayout(norGate.inputCount)

  minY = Math.min(...inputYs.value);
  maxY = Math.max(...inputYs.value);
}


onMounted(()=>{
 norGate.inputs = computed(()=>createInputs(norGate.inputCount));
 updateOutput();
})

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
