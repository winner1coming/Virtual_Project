<template>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 600 600"
    width="300"
    height="300"
    style="overflow: visible;"
    
  >
    <g :transform="`translate(${andGate.position[0]}, ${andGate.position[1]}) scale(${andGate.scale})`" cursor="move">
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
        fill="transparent"
        stroke="black"
        stroke-width="12"
        d="M366.5 180C507 259.301 410.965 399.972 366.5 395"
      />
      <path stroke="black" stroke-width="12" d="M440 288L497 288" />

      <!--填充透明区域以便拖拽-->
      <rect
        :x="149"
        :y="181"
        :width="223"
        :height="218"
        fill="transparent"
      />

      <!-- 输入引脚 -->
      <template v-for="(input, index) in andGate.inputs" :key="index">
        <circle
          v-if="andGate.inputInverted[index]"
          :cx="149 - 26"
          :cy="inputYs[index]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!andGate.inputInverted[index]":d="`M92 ${inputYs[index]}L149 ${inputYs[index]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="inputYs[index]" :active="input" :bitWidth="andGate.width" @toggle="() => handleToggleInput(index)"/>
      </template>

      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="andGate.outputs[0]" />
    </g>
  </svg>
</template>

<script setup>
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import InputPort from './InputPort.vue'
import OutputPort from './OutputPort.vue'
import { defineProps } from 'vue'

import { useGateLayout } from '@/logic/usegates/useGateLayout'
import { useCircuitStore } from '@/store/CircuitStore'
import {watchComponentChanges} from '@/modules/useComponentsWatchers'

const circuitStore = useCircuitStore();
// const props = defineProps({
//   id: {
//     type: Number,
//     required: true
//   }
// })
const id = circuitStore.addComponent('And', [0,0]);  // debug

const andGate = computed(() => {
  return circuitStore.getComponent(id);   // debug
  // return circuitStore.getComponent(props.id);  
});

let inputYs = useGateLayout(andGate.value.inputCount)
// let inputYs = computed(()=>{
//   return 
// });

let minY = Math.min(...inputYs.value);
let maxY = Math.max(...inputYs.value);

// 设置引脚位置
andGate.value.InputPinPosition = andGate.value.InputPinPosition.map((pin, index) => {
  return [
    andGate.value.position[0] + 92 * andGate.value.scale,
    andGate.value.position[1] + inputYs.value[index] * andGate.value.scale,
  ];
});
andGate.value.OutputPinPosition = andGate.value.OutputPinPosition.map(pin => {
  return [
    andGate.value.position[0] + 497 * andGate.value.scale,
    andGate.value.position[1] + 288 * andGate.value.scale,
  ];
});

function setInputCount(newCount)
{
  andGate.value.changeInputPinCount(newCount);   // todo 这行应该之后要删
  // 更新输入引脚的布局
  inputYs = useGateLayout(andGate.value.inputCount)

  minY = Math.min(...inputYs.value);
  maxY = Math.max(...inputYs.value);

  // 更新引脚位置
  andGate.value.InputPinPosition = andGate.value.InputPinPosition.map((pin, index) => {
    return [
      andGate.value.position[0] + 92 * andGate.value.scale,
      andGate.value.position[1] + inputYs.value[index] * andGate.value.scale,
    ];
  });
}

const unwatchInputCount  = watchComponentChanges(andGate, setInputCount);

onUnmounted(() => {
  unwatchInputCount(); // 清理监听
});


// 以下为调试用代码，后期可删除---------------------------------------

// andGate.value.changeInput(0, 0);   // todo debug
// andGate.value.changeInput(1, 0);

// 每个输入引脚都有唯一 id，方便用外部数据更新，比如 andGate.inputs.find(i => i.id === 'in2').value = true,
// 日后可以扩展属性，比如 label, connected, position 等

// function setInputValue(index, value) {
//   if (index < 0 || index >= andGate.value.inputs.length) return
//   andGate.value.inputs[index].value = value
//   updateOutput()
// }
// 用法：setInputValue(0, true)  // 设置第 0 个引脚为高电平
//      setInputValue(1, false) // 设置第 1 个引脚为低电平

//handleSetInputInverted(0, true);
// handleSetScale(1);   

// 测试用函数，后期删掉  todo
function handleToggleInput(index) {
  //toggleInput(andGate, index, updateOutput)
  // todotodo
  setInputCount(5);
  if(andGate.value.inputs[index] === 0){
    andGate.value.changeInput(index, 1);
  }else{
    andGate.value.changeInput(index, 0);
  }

  // test
  // this.setInputCount(4); // 更新输入引脚布局
  andGate.value.changeInputInverted(index, !andGate.value.inputInverted[index]); // 切换输入引脚的反相状态
}

// function handleSetInputValue(index, value) {
//   setInputValue(andGate, index, value, updateOutput)
// }

// function handleSetInputInverted(index, inverted) {
//   setInputInverted(andGate, index, inverted, updateOutput)
// }

function handleSetScale(newScale){
  andGate.value.scale = newScale;
}




// function updateOutput() {
//   andGate.output = andGate.inputs.every(input =>
//     input.inverted ? !input.value : input.value
//   )
// }

// 提供所有元件端口的位置
function getAllPorts(gate) {
  const ports = [];

  // 输入引脚
  gate.andGate.value.inputs.forEach((input, index) => {
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

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
