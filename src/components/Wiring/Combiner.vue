<template>
    <g :transform="`translate(${combiner.offset[0]*combiner.scale}, ${combiner.offset[1]*combiner.scale}) scale(${combiner.scale})`" cursor="move">
      <!-- 合并器图形 -->
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
      <!-- 右下角斜线 -->
      <!-- <path stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149.5 397.34L100 439.34" /> -->
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" :d="minY<246? `M149 ${maxY-6}L${combiner.direction==='east'? 198:100} ${maxY+36}`: `M149.5 397.34L${combiner.direction==='east'? 198:100} 439.34`" />
      <!-- 输出引脚 -->
      <OutputPort :cx="combiner.direction==='east'? 198:100" :cy="minY<246? maxY+36: 439.34" :active="1" :bitWidth="combiner.bitWidth"/>
      <!-- 输入引脚 -->
      <template v-for="(input, index) in combiner.inputs" :key="input.id">
        <path :d="`M149 ${combiner.inputPinPosition[index][1]}L${combiner.direction==='east'? 92:206} ${combiner.inputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="`${combiner.direction==='east'? 92:206}`" :cy="combiner.inputPinPosition[index][1]" :active="input"/>
      </template>
    </g>
</template>

<script setup>
import { computed } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
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

let outputYs = computed(()=>useGateLayout(combiner.value.inputCount))

let minY = computed(()=>Math.min(...outputYs.value.value));
let maxY = computed(()=>Math.max(...outputYs.value.value));

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
