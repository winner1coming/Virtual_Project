<template>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 600 600"
    width="300"
    height="300"
    style="overflow: visible;"
    
  >
    <g :transform="`translate(${andGate.x}, ${andGate.y}) scale(${andGate.scale})`" cursor="move">
      <!-- AND 门图形 -->
       <!--左侧竖线-->
      <path
        stroke="black"
        stroke-width="12"
        :d="minY<181? `M149 ${minY-6}L149 ${maxY+6}`: `M149 175L149 401`"
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
        <circle
          v-if="input.inverted"
          :cx="149 - 26"
          :cy="inputYs[index]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!input.inverted":d="`M92 ${inputYs[index]}L149 ${inputYs[index]}`" stroke="black" stroke-width="12" />
        <!-- <path v-if="input.inverted":d="`M${92 - 30} ${inputYs[index]}L${149 - 44} ${inputYs[index]}`" stroke="black" stroke-width="12" /> -->
        <InputPort :cx="92" :cy="inputYs[index]" :active="input.value" @toggle="() => handleToggleInput(index)"/>
        <!-- <InputPort v-if="input.inverted":cx="92-30" :cy="inputYs[index]" :active="input.value" @toggle="() => toggleInput(index)" /> -->
      </template>

      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="andGate.output" />
    </g>
  </svg>
</template>

<script setup>
import { ref, reactive, computed, onMounted } from 'vue'
import InputPort from './InputPort.vue'
import OutputPort from './OutputPort.vue'
import {
  createInputs,
  setInputValue,
  toggleInput,
  setInputInverted,
  setScale
} from '@/logic/usegates/useLogicGates'
import { useGateLayout } from '@/logic/usegates/useGateLayout'

// const inputCount = 8 // 输入引脚个数可调整



const andGate = reactive({
  x: 0,
  y: 0,
  scale: 1,
  inputCount: 8,
  inputs: [],
  output: false,
})

let inputYs = useGateLayout(andGate.inputCount)

let minY = Math.min(...inputYs.value);
let maxY = Math.max(...inputYs.value);
// 每个输入引脚都有唯一 id，方便用外部数据更新，比如 andGate.inputs.find(i => i.id === 'in2').value = true,
// 日后可以扩展属性，比如 label, connected, position 等

// function setInputValue(index, value) {
//   if (index < 0 || index >= andGate.value.inputs.length) return
//   andGate.value.inputs[index].value = value
//   updateOutput()
// }
// 用法：setInputValue(0, true)  // 设置第 0 个引脚为高电平
//      setInputValue(1, false) // 设置第 1 个引脚为低电平

handleSetInputInverted(0, true);
handleSetScale(0.5);

function handleToggleInput(index) {
  toggleInput(andGate, index, updateOutput)
}

function handleSetInputValue(index, value) {
  setInputValue(andGate, index, value, updateOutput)
}

function handleSetInputInverted(index, inverted) {
  setInputInverted(andGate, index, inverted, updateOutput)
}

function handleSetScale(newscale){
  setScale(andGate, newscale)
}

function setInputCount(newCount)
{
  andGate.inputCount = newCount;
  inputYs = useGateLayout(andGate.inputCount)

  minY = Math.min(...inputYs.value);
  maxY = Math.max(...inputYs.value);
}

function updateOutput() {
  andGate.output = andGate.inputs.every(input =>
    input.inverted ? !input.value : input.value
  )
}

// 提供所有元件端口的位置
function getAllPorts(gate) {
  const ports = [];

  // 输入引脚
  gate.inputs.forEach((input, index) => {
    ports.push({
      id: input.id,
      type: 'input',
      x: gate.x + 92 * gate.scale,
      y: gate.y + inputYs.value[index] * gate.scale,
      component: gate
    });
  });

  // 输出引脚
  ports.push({
    id: 'output',
    type: 'output',
    x: gate.x + 497 * gate.scale,
    y: gate.y + 288 * gate.scale,
    component: gate
  });

  return ports;
}


onMounted(()=>{
 andGate.inputs = computed(()=>createInputs(andGate.inputCount));
})

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
