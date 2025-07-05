<template>
    <g :transform="`translate(${customizeComponent.offset[0]*customizeComponent.scale}, ${customizeComponent.offset[1]*customizeComponent.scale}) scale(${customizeComponent.scale})`" cursor="move">
      <!-- 自定义图形 -->
      <rect
        x="149"
        :y="minY<181? minY-6 : 175"
        :width="223"
        :height="minY<181? maxY-minY+12: 226"
        fill=transparent
        stroke="black"
        stroke-width="8"
      />

      <!-- 输入引脚 -->
      <template v-for="(input, index) in customizeComponent.inputs" :key="index">
        <circle
          v-if="customizeComponent.inputInverted[index]"
          :cx="149 - 26"
          :cy="customizeComponent.inputPinPosition[index][1]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!customizeComponent.inputInverted[index]":d="`M92 ${customizeComponent.inputPinPosition[index][1]}L149 ${customizeComponent.inputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="customizeComponent.inputPinPosition[index][1]" :active="input" :bitWidth="customizeComponent.bitWidth" @toggle="() => handleToggleInput(index)"/>
      </template>

      <!-- 输出引脚 -->
      <template v-for="(output, index) in customizeComponent.outputs" :key="output.id">
        <path :d="`M372 ${customizeComponent.outputPinPosition[index][1]}L429 ${customizeComponent.outputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <OutputPort :cx="149+223+57" :cy="customizeComponent.outputPinPosition[index][1]" :active="output" />
      </template>
       
      <!--选中方框-->
      <SelectedBox :x="142" :y="minY<181? minY-12 : 175" :width="235" :height="minY<181? (maxY-minY+20): 232" :visible="circuitStore.selectedId===props.id"/>
      
    </g>
</template>

<script setup>
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { defineProps } from 'vue'

import { useCircuitStore } from '@/store/CircuitStore'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  }
})

const customizeComponent = computed(() => {
  // return circuitStore.getComponent(id);   // debug
  return circuitStore.getComponent(props.id);  
});

let minY_in = computed(()=>Math.min(...customizeComponent.value.inputPinPosition.map(pin => pin[1])));
let maxY_in = computed(()=>Math.max(...customizeComponent.value.inputPinPosition.map(pin => pin[1])));

let minY = computed(()=>Math.min(minY_in.value, minY_out.value)); // todo
let maxY = computed(()=>Math.max(maxY_in.value, maxY_out.value)); // todo

// let outputYs = computed(()=>useGateLayout(customizeComponent.value.outputs.length)) // 定义输出的个数 调试用，要删 todo
let minY_out = computed(()=>Math.min(...customizeComponent.value.outputPinPosition.map(pin => pin[1]))); //todo
let maxY_out = computed(()=>Math.max(...customizeComponent.value.outputPinPosition.map(pin => pin[1]))); //todo


</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
