<template>
      <g :transform="transform" cursor="move">
      <!-- OR 门图形 -->
      <path stroke="black" stroke-width="12" d="M145.999 181L315.999 181" /> <!--上下两条横线-->
      <path stroke="black" stroke-width="12" d="M146 395.115L316 395.115" />
      <path fill="none" stroke="black" stroke-width="12" d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739" /> <!--右上角的小曲线-->
      <path fill="none" stroke="black" stroke-width="12" d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375" /> <!--右下角的小曲线-->
      <path stroke="black" stroke-width="12" d="M440 288L497 288" /> <!--输出端的横线-->
      <path stroke="black" stroke-width="12" d="M368.226 198.885L450.774 292.115" /> <!--右上角的斜直线-->
      <path stroke="black" stroke-width="12" d="M368.227 377.23L450.775 284" /> <!--右下角的斜直线-->
      <path fill="none" stroke="black" stroke-width="12" d="M149 179.5C188.3 212.65 273.5 295 149 397" /> <!--左侧曲线-->
      <!--填充透明区域-->
      <path
        fill="transparent"
        d="
          M145.999 181
          L315.999 181
          C313.833 179.216 358.755 182.869 373.209 204.739
          L450.774 292.115
          L450.775 284
          L368.227 377.23
          C358.756 393.246 313.834 396.899 303.27 394.502
          L146 395.115
          C273.5 295 188.3 212.65 149 179.5
          Z"
      />

      <!-- 上方引脚竖线 -->
      <path
        v-if="minY < bezierYMin"
        :d="`M149 ${minY-6}L149 ${bezierYMin+6}`"
        stroke="black"
        stroke-width="12"
      />
      <!-- 下方引脚竖线 -->
      <path
        v-if="maxY > bezierYMax"
        :d="`M149 ${bezierYMax-6}L149 ${maxY+6}`"
        stroke="black"
        stroke-width="12"
      />

      <!--选中方框-->
      <SelectedBox :x="82" :y="minY<181? minY-12: 175" :width="424" :height="minY<181? (maxY-minY+24): 226" :visible="circuitStore.selectedId===props.id"/>

      <!-- 输入引脚 -->
      <template v-for="(input, index) in orGate.inputs" :key="input.id">
        <circle
          v-if="orGate.inputInverted[index]"
          :cx="`${getInputLine(index, orGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x2-26}`"
          :cy="orGate.inputPinPosition.map(pin => pin[1])[index]"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path
          v-if="orGate.inputInverted[index]"
          :d="`M${getInputLine(index, orGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x1} ${orGate.inputPinPosition.map(pin => pin[1])[index]}L${getInputLine(index, orGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x2-36} ${orGate.inputPinPosition.map(pin => pin[1])[index]}`"
          stroke="black"
          stroke-width="12"
          />
        <path
          v-if="!orGate.inputInverted[index]"
          :d="`M${getInputLine(index, orGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x1} ${orGate.inputPinPosition.map(pin => pin[1])[index]}L${getInputLine(index, orGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x2} ${orGate.inputPinPosition.map(pin => pin[1])[index]}`"
          stroke="black"
          stroke-width="12"
        />
        <InputPort
          :cx="92"
          :cy="orGate.inputPinPosition.map(pin => pin[1])[index]"
          :active="input"
          :bitWidth="orGate.bitWidth"
          @toggle="() => handleToggleInput(index)"
        />
      </template>



      <!-- 输出状态 -->
      <OutputPort :cx="497" :cy="288" :active="orGate.outputs[0]" />
    </g>
</template>

<script setup>
import { computed } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { defineProps } from 'vue'

import { getInputLine } from '@/logic/usegates/useGateLayout'
import { useCircuitStore } from '@/store/CircuitStore'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  }
})

const orGate = computed(() => {
  return circuitStore.getComponent(props.id);  
});

const bezierYMin = 179.5;
const bezierYMax = 397;
let minY = computed(()=>Math.min(...orGate.value.inputPinPosition.map(pin => pin[1])));
let maxY = computed(()=>Math.max(...orGate.value.inputPinPosition.map(pin => pin[1])));

const directionToAngle = {
  east: 0,
  south: 90,
  west: 180,
  north: 270
}

const transform = computed(() => {
  const [x, y] = orGate.value.offset;
  const scale = orGate.value.scale;
  const cx = 295;

  if (orGate.value.direction === 'east') {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
    `;
  } 
  else if(orGate.value.direction === 'west')
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
      rotate(${directionToAngle[orGate.value.direction]},295,${(minY.value+maxY.value)/2})
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
