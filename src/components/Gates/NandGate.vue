<template>
  <g :transform="transform" cursor="move">
    <!-- 门图形 -->
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
    <ellipse cx="470.2452697753906" cy="286.0000305175781" rx="20.754730224609375" ry="20" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#CCCCCC" fill-opacity="0">
    </ellipse>

    <!--填充透明区域以便选中-->
    <rect
      :x="149"
      :y="181"
      :width="223"
      :height="218"
      fill="transparent"
    />
     
    <!--选中方框-->
    <SelectedBox :x="82" :y="minY<181? minY-12: 175" :width="424" :height="minY<181? (maxY-minY+24): 226" :visible="circuitStore.selectedId===props.id"/>
    
    <!-- 输入引脚 -->
    <template v-for="(input, index) in nandGate.inputs" :key="index">
      <circle
        v-if="nandGate.inputInverted[index]"
        :cx="149 - 26"
        :cy="nandGate.inputPinPosition[index][1]"
        r="16"
        stroke="black"
        stroke-width="12"
        fill="none"
      />
      <path v-if="!nandGate.inputInverted[index]":d="`M92 ${nandGate.inputPinPosition[index][1]}L149 ${nandGate.inputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
      <InputPort :cx="92" :cy="nandGate.inputPinPosition[index][1]" :active="input" :bitWidth="nandGate.bitWidth" @toggle="() => handleToggleInput(index)"/>
    </template>

    <!-- 输出状态 -->
    <OutputPort :cx="497" :cy="288" :active="nandGate.outputs[0]" />

  </g>
</template>

<script setup>
import { computed } from 'vue'
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

const nandGate = computed(() => {
return circuitStore.getComponent(props.id);  
});

let minY = computed(()=>Math.min(...nandGate.value.inputPinPosition.map(pin => pin[1])));
let maxY = computed(()=>Math.max(...nandGate.value.inputPinPosition.map(pin => pin[1])));

const directionToAngle = {
  east: 0,
  south: 90,
  west: 180,
  north: 270
}

const transform = computed(() => {
  const [x, y] = nandGate.value.offset;
  const scale = nandGate.value.scale;
  const cx = 295;

  if (nandGate.value.direction === 'east') {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
    `;
  } 
  else if(nandGate.value.direction === 'west')
  {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
      translate(${cx}, 0)
      scale(-1, 1)
      translate(${-cx}, 0)
    `;
  }
  else
  {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
      rotate(${directionToAngle[nandGate.value.direction]},295,${(minY.value+maxY.value)/2})
    `;
  }
});

</script>

<style scoped>
svg {
border: 1px solid #ccc;
background-color: #f8f8f8;
}
</style>
