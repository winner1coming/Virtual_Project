<template>
    <g :transform="`translate(${combiner.offset[0]*combiner.scale}, ${combiner.offset[1]*combiner.scale}) scale(${combiner.scale})`" cursor="move">
      <!-- 合并器图形 -->
      <!--选中方框-->
      <SelectedBox :x="94" :y="minY<246? minY-6: 246" :width="206-100+12" :height="minY<246? maxY-minY+48: 401-246+48" :visible="circuitStore.selectedId===props.id"/>

      <!--竖线-->
      <path
        stroke="black"
        stroke-width="12"
        :d="minY<246? `M149 ${minY-6}L149 ${maxY+6}`: `M149 246L149 401`"
      />
      <!-- 右下角斜线 -->
      <!-- <path stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149.5 397.34L100 439.34" /> -->
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" :d="minY<246? `M149 ${maxY-6}L198 ${maxY+36}`: `M149.5 397.34L198 439.34`" />
      <!-- 输出引脚 -->
      <OutputPort :cx="198" :cy="minY<246? maxY+36: 439.34" :active="1" :bitWidth="combiner.bitWidth"/>
      <!-- 输入引脚 -->
      <template v-for="(output, index) in combiner.inputs" :key="output.id">
        <path :d="`M149 ${combiner.inputPinPosition[index][1]}L92 ${combiner.inputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="combiner.inputPinPosition[index][1]" :active="combiner.inputs[index]"/>
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

const combiner = computed(() => {
  return circuitStore.getComponent(props.id);  
});

// let outputYs = computed(()=>combiner.outputPinPosition.map(item => item[1]));
// let minY = computed(()=>Math.min(...outputYs.value));
// let maxY = computed(()=>Math.max(...outputYs.value));

let outputYs = computed(()=>useGateLayout(combiner.value.outputs.length))

let minY = computed(()=>Math.min(...outputYs.value.value));
let maxY = computed(()=>Math.max(...outputYs.value.value));

// function handleToggleInput(index) {
//     //setOutputCount(9)
// }

// function handleSetScale(newScale) {
//   setScale(combiner, newScale)
// }

// function setOutputCount(newCount)
// {
//   combiner.outputCount = newCount;
//   outputYs = useGateLayout(combiner.outputCount)

//   minY = Math.min(...outputYs.value);
//   maxY = Math.max(...outputYs.value);
// }

// onMounted(() => {
//   combiner.outputs = computed(()=>createOutputs(combiner.outputCount));
// })

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
