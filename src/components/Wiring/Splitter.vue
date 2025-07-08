<template>
    <g :transform="`translate(${splitter.offset[0]*splitter.scale}, ${splitter.offset[1]*splitter.scale}) scale(${splitter.scale})`" cursor="move">
      <!-- 分离器图形 -->
      <!--选中方框-->
      <SelectedBox :x="94" :y="minY<246? minY-6: 246" :width="206-100+12" :height="minY<246? maxY-minY+48: 401-246+48" :visible="circuitStore.selectedId===props.id"/>
      <!--透明-->
      <rect x="94" :y="minY<246? minY-6: 246" :width="206-100+12" :height="minY<246? maxY-minY+48: 401-246+48" fill="transparent"></rect>
      <!--竖线-->
      <path
        stroke="black"
        stroke-width="12"
        :d="minY<246? `M149 ${minY-6}L149 ${maxY+6}`: `M149 246L149 401`"
      />
      <!-- 左下角斜线 -->
      <!-- <path stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149.5 397.34L100 439.34" /> -->
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" :d="minY<246? `M149 ${maxY-6}L${splitter.direction==='east'? 100:198} ${maxY+36}`: `M149.5 397.34L${splitter.direction==='east'? 100:198} 439.34`" />
      <!-- 输入引脚 -->
      <InputPort :cx="`${splitter.direction==='east'? 100:198}`" :cy="minY<246? maxY+36: 439.34" :active="1" :bitWidth="splitter.bitWidth"/>
      <!-- 输出引脚 -->
      <template v-for="(output, index) in splitter.outputs" :key="output.id">
        <path :d="`M149 ${splitter.outputPinPosition[index][1]}L${splitter.direction==='east'? 206:92} ${splitter.outputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <OutputPort :cx="`${splitter.direction==='east'? 206:92}`" :cy="splitter.outputPinPosition[index][1]" :active="splitter.outputs[index]"/>
      </template>
    </g>
</template>

<script setup>
import { reactive, ref, computed, onMounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { createOutputs, setScale } from '@/logic/usegates/useLogicGates'
import { useGateLayout } from '@/logic/usegates/useGateLayout'

import { useCircuitStore } from '@/store/CircuitStore'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  }
})

const splitter = computed(() => {
  return circuitStore.getComponent(props.id);  
});

// let outputYs = computed(()=>splitter.outputPinPosition.map(item => item[1]));
// let minY = computed(()=>Math.min(...outputYs.value));
// let maxY = computed(()=>Math.max(...outputYs.value));

let outputYs = computed(()=>useGateLayout(splitter.value.outputs.length))

let minY = computed(()=>Math.min(...outputYs.value.value));
let maxY = computed(()=>Math.max(...outputYs.value.value));

// function handleToggleInput(index) {
//     //setOutputCount(9)
// }

// function handleSetScale(newScale) {
//   setScale(splitter, newScale)
// }

// function setOutputCount(newCount)
// {
//   splitter.outputCount = newCount;
//   outputYs = useGateLayout(splitter.outputCount)

//   minY = Math.min(...outputYs.value);
//   maxY = Math.max(...outputYs.value);
// }

// onMounted(() => {
//   splitter.outputs = computed(()=>createOutputs(splitter.outputCount));
// })

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
