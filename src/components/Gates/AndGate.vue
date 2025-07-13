<template>
    <g :transform="transform" cursor="move">
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
      <template v-for="(input, index) in andGate.inputs" :key="index">
        <circle
          v-if="andGate.inputInverted[index]"
          :cx="149 - 26"
          :cy="andGate.inputPinPosition[index][1]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!andGate.inputInverted[index]":d="`M92 ${andGate.inputPinPosition[index][1]}L149 ${andGate.inputPinPosition[index][1]}`" stroke="black" stroke-width="12" />
        <InputPort :cx="92" :cy="andGate.inputPinPosition[index][1]" :active="input" :bitWidth="andGate.bitWidth" @toggle="() => handleToggleInput(index)"/>
      </template>

      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="andGate.outputs[0]" />

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

const andGate = computed(() => {
  return circuitStore.getComponent(props.id);  
});

let minY = computed(()=>Math.min(...andGate.value.inputPinPosition.map(pin => pin[1])));
let maxY = computed(()=>Math.max(...andGate.value.inputPinPosition.map(pin => pin[1])));

const directionToAngle = {
  east: 0,
  south: 90,
  west: 180,
  north: 270
}

// 计算偏移量、旋转方位
const transform = computed(() => {
  const [x, y] = andGate.value.offset;
  const scale = andGate.value.scale;
  const cx = 295;

  if (andGate.value.direction === 'east') {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
    `;
  } 
  else if(andGate.value.direction === 'west')
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
      rotate(${directionToAngle[andGate.value.direction]},295,${(minY.value+maxY.value)/2})
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
