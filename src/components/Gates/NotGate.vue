<template>
        <g :transform="transform" cursor="move">
        <!-- NOT门主体（三角形） -->
        <path
          d="M438.075 288.5L147.769 392.856L147.769 184.144L438.075 288.5Z"
          stroke="black"
          stroke-width="12"
          fill="transparent"
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
        <SelectedBox :x="83" :y="178" :width="420" :height="220" :visible="circuitStore.selectedId===props.id"/>

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

const transform = computed(() => {
  const [x, y] = notGate.value.offset;
  const scale = notGate.value.scale;
  const cx = 295;

  if (notGate.value.direction === 'west') {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
      translate(${cx}, 0)
      scale(-1, 1)
      translate(${-cx}, 0)
    `;
  } else {
    return `
      translate(${x*scale}, ${y*scale})
      scale(${scale})
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
