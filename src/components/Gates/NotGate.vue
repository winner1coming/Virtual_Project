<template>
      <g :transform="`translate(${notGate.offset[0]*notGate.scale}, ${notGate.offset[1]*notGate.scale}) scale(${notGate.scale})`" cursor="move">
        <!-- NOT门主体（三角形） -->
        <path
          d="M438.075 288.5L147.769 392.856L147.769 184.144L438.075 288.5Z"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <!-- 输出端圆圈 -->
        <ellipse
          cx="470.245"
          cy="286"
          rx="20.75"
          ry="20"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <!-- 输入 -->
        <circle
          v-if="notGate.inputInverted[0]"
          :cx="149 - 26"
          :cy="288"
          r="16"
          stroke="black"
          stroke-width="12"
          fill="none"
        />
        <path v-if="!notGate.inputInverted[0]" stroke="black" stroke-width="12" d="M93 288L150 288" />

        <!--选中方框-->
        <SelectedBox :x="83" :y="183" :width="406" :height="209" :visible="circuitStore.selectedId===props.id"/>

        <!-- 输入状态端口 -->
        <InputPort :cx="93" :cy="288" :active="notGate.inputs[0]" @toggle="handleToggleInput" />
        <!-- 输出状态端口 -->
        <OutputPort :cx="497" :cy="286" :active="notGate.outputs[0]" />
      </g>
</template>
  
<script setup>
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import OutputPort from '@/components/Ports/OutputPort.vue'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
import { defineProps } from 'vue'

import { useGateLayout, getInputLine } from '@/logic/usegates/useGateLayout'
import { useCircuitStore } from '@/store/CircuitStore'
import {watchComponentChanges} from '@/modules/useComponentsWatchers'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: Number,
    required: true
  }
})

const notGate = computed(() => {
  return circuitStore.getComponent(props.id);  
});

</script>

<style scoped>
    svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
    }
</style>
