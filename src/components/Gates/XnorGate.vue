<template>
  <g :transform="`translate(${xnorGate.offset[0]*xnorGate.scale}, ${xnorGate.offset[1]*xnorGate.scale}) scale(${xnorGate.scale})`" cursor="move">
    <!-- XOR 门图形 -->
    <rect x="0" y="0" width="0" height="0" fill="rgba(249, 249, 249, 1)" />
    <path fill="none" stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M145.999 181L315.999 181">
    </path>
    <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M303.269 181.612C313.833 179.216 358.755 182.869 373.209 204.739">
    </path>
    <!-- <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M440 288L497 288">
    </path> -->
    <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M368.226 198.885L450.774 292.115">
    </path>
    <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M146 395.115L316 395.115">
    </path>
    <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M303.27 394.502C313.834 396.899 358.756 393.246 373.21 371.375">
    </path>
    <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M368.227 377.23L450.775 284">
    </path>
    <path fill="none" stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149 179.5C188.3 212.65 273.5 295 149 397">
    </path>
    <!-- <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M42 326L152 326">
    </path>
    <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M42 246L152 246">
    </path> -->
    <path fill="none" stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M97 181C136.3 214.15 221.5 296.5 97 398.5">
    </path>

    <ellipse cx="470.2452697753906" cy="286.0000305175781" rx="20.754730224609375" ry="20" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#CCCCCC" fill-opacity="0">
    </ellipse>
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
      :d="`M99 ${minY-6}L99 ${bezierYMin+6}`"
      stroke="black"
      stroke-width="12"
    />
    <!-- 下方引脚竖线 -->
    <path
      v-if="maxY > bezierYMax"
      :d="`M99 ${bezierYMax-6}L99 ${maxY+6}`"
      stroke="black"
      stroke-width="12"
    />

    <!--选中方框-->
    <SelectedBox :x="35" :y="minY<181? minY-12: 175" :width="471" :height="minY<181? (maxY-minY+24): 226" :visible="circuitStore.selectedId===props.id"/>

    <!-- 输入引脚 -->
    <template v-for="(input, index) in xnorGate.inputs" :key="input.id">
      <circle
        v-if="input.inverted"
        :cx="`${getInputLine(index, xnorGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x2-76}`"
        :cy="xnorGate.inputPinPosition.map(pin => pin[1])[index]"
        r="16"
        stroke="black"
        stroke-width="12"
        fill="none"
      />
      <path
        v-if="input.inverted"
        :d="`M${getInputLine(index, xnorGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x1-50} ${xnorGate.inputPinPosition.map(pin => pin[1])[index]}L${getInputLine(index, xnorGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x2-86} ${xnorGate.inputPinPosition.map(pin => pin[1])[index]}`"
        stroke="black"
        stroke-width="12"
        />
      <path
        v-if="!input.inverted"
        :d="`M${getInputLine(index, xnorGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x1-50} ${xnorGate.inputPinPosition.map(pin => pin[1])[index]}L${getInputLine(index, xnorGate.inputPinPosition.map(pin => pin[1])[index], bezierYMin, bezierYMax).x2-50} ${xnorGate.inputPinPosition.map(pin => pin[1])[index]}`"
        stroke="black"
        stroke-width="12"
      />
      <InputPort
        :cx="42"
        :cy="xnorGate.inputPinPosition.map(pin => pin[1])[index]"
        :active="xnorGate.inputs[index]"
        @toggle="() => handleToggleInput(index)"
      />
    </template>



    <!-- 输出状态 -->
    <OutputPort :cx="497" :cy="288" :active="xnorGate.outputs[0]" />
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

const xnorGate = computed(() => {
  return circuitStore.getComponent(props.id);  
});

const bezierYMin = 179.5;
const bezierYMax = 397;
let minY = computed(()=>Math.min(...xnorGate.value.inputPinPosition.map(pin => pin[1])));
let maxY = computed(()=>Math.max(...xnorGate.value.inputPinPosition.map(pin => pin[1])));

</script>

  