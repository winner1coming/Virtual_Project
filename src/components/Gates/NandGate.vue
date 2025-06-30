<template>
    <svg
      xmlns="http://www.w3.org/2000/svg"
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
      
    >
      <g :transform="`translate(${nandGate.x}, ${nandGate.y}) scale(${nandGate.scale})`" cursor="move">
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
        <!-- <path stroke="black" stroke-width="12" d="M440 288L497 288" /> -->
      <ellipse cx="470.2452697753906" cy="286.0000305175781" rx="20.754730224609375" ry="20" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#CCCCCC" fill-opacity="0">
      </ellipse>
  
        <!-- 输入引脚 -->
        <template v-for="(input, index) in nandGate.inputs" :key="input.id">
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
        <OutputPort :cx="497" :cy="288" :active="nandGate.output" />
      </g>
    </svg>
  </template>
  
  <script setup>
  import { ref, reactive, computed, onMounted } from 'vue'
  import InputPort from '@/components/Ports/InputPort.vue'
  import OutputPort from '@/components/Ports/OutputPort.vue'
  import {
    createInputs,
    setInputValue,
    toggleInput,
    setInputInverted,
    setScale
  } from '@/logic/usegates/useLogicGates'
  import { useGateLayout } from '@/logic/usegates/useGateLayout'
  
  // const inputCount = 8 // 输入引脚个数可调整
  
  
  
  const nandGate = reactive({
    x: 0,
    y: 0,
    scale: 1,
    inputCount: 3,
    inputs: [],
    output: false,
  })
  
  let inputYs = useGateLayout(nandGate.inputCount)
  
  let minY = Math.min(...inputYs.value);
  let maxY = Math.max(...inputYs.value);

  function handleToggleInput(index) {
    toggleInput(nandGate, index, updateOutput)
    // handleSetInputInverted(index, true);
    // handleSetScale(0.5);
    // setInputCount(9);
  }
  
  function handleSetInputValue(index, value) {
    setInputValue(nandGate, index, value, updateOutput)
  }
  
  function handleSetInputInverted(index, inverted) {
    setInputInverted(nandGate, index, inverted, updateOutput)
  }
  
  function handleSetScale(newscale){
    setScale(nandGate, newscale)
  }
  
  function setInputCount(newCount)
  {
    nandGate.inputCount = newCount;
    inputYs = useGateLayout(nandGate.inputCount)
  
    minY = Math.min(...inputYs.value);
    maxY = Math.max(...inputYs.value);
  }
  
  function updateOutput() {
    nandGate.output = !nandGate.inputs.every(input =>
      input.inverted ? !input.value : input.value
    )
  }
  
  onMounted(()=>{
   nandGate.inputs = computed(()=>createInputs(nandGate.inputCount));
   updateOutput();
  })
  
  </script>
  
  <style scoped>
  svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
  }
  </style>
  