<template>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    viewBox="0 0 600 600"
    width="300"
    height="300"
    style="overflow: visible;"
  >
    <g :transform="`translate(${splitter.x}, ${splitter.y}) scale(${splitter.scale})`" cursor="move">
      <!-- 分离器图形 -->
      <!--选中方框-->
      <SelectedBox :x="94" :y="minY<246? minY-6: 246" :width="206-100+12" :height="minY<246? maxY-minY+48: 401-246+48" :visible="true"/>

      <!--竖线-->
      <path
        stroke="black"
        stroke-width="12"
        :d="minY<246? `M149 ${minY-6}L149 ${maxY+6}`: `M149 246L149 401`"
      />
      <!-- 左下角斜线 -->
      <!-- <path stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149.5 397.34L100 439.34" /> -->
      <path stroke="rgba(0, 0, 0, 1)" stroke-width="12" :d="minY<246? `M149 ${maxY-6}L100 ${maxY+36}`: `M149.5 397.34L100 439.34`" />
      <!-- 输入引脚 -->
      <InputPort :cx="100" :cy="minY<246? maxY+36: 439.34" :active="1" :bitWidth="8"/>
      <!-- 输出引脚 -->
      <template v-for="(output, index) in splitter.outputs" :key="output.id">
        <path :d="`M149 ${outputYs[index]}L206 ${outputYs[index]}`" stroke="black" stroke-width="12" />
        <OutputPort :cx="206" :cy="outputYs[index]" :active="1"/>
      </template>
    </g>
  </svg>
</template>

<script setup>
import { reactive, ref, computed, onMounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { createOutputs, setScale } from '@/logic/usegates/useLogicGates'
import { useGateLayout } from '@/logic/usegates/useGateLayout'


const splitter = reactive({
  x: 0,
  y: 0,
  scale: 1,
  outputCount: 4,
  input: 0,
  outputs: [],
})

let outputYs = useGateLayout(splitter.outputCount)

let minY = Math.min(...outputYs.value);
let maxY = Math.max(...outputYs.value);

// function updateOutputs() {
//   splitter.outputs = splitter.outputs.map(() => splitter.input.value)
// }

function handleToggleInput(index) {
    //setOutputCount(9)
}

function handleSetScale(newScale) {
  setScale(splitter, newScale)
}

function setOutputCount(newCount)
{
  splitter.outputCount = newCount;
  outputYs = useGateLayout(splitter.outputCount)

  minY = Math.min(...outputYs.value);
  maxY = Math.max(...outputYs.value);
}

onMounted(() => {
  splitter.outputs = computed(()=>createOutputs(splitter.outputCount));
})
</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
