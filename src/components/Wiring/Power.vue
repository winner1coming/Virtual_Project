<template>
    <g :transform="`translate(${power.offset[0]*power.scale}, ${power.offset[1]*power.scale}) scale(${power.scale})`" cursor="move">
        <path d="M38 20L61.3827 94.25L14.6173 94.25L38 20Z" fill="transparent" stroke="rgba(0, 0, 0, 1)" stroke-width="10"   >
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M38 94L38 124">
        </path>
        
        <!--选中方框-->
        <SelectedBox :x="10" :y="10" :width="60" :height="130" :visible="circuitStore.selectedId===props.id"/>
    
        <!-- 输出状态 -->
        <OutputPort :cx="38" :cy="124" :active="power.outputs[0]" />
    </g>
</template>

<script setup>
import { computed } from 'vue'
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
const power = computed(() => {
  return circuitStore.getComponent(props.id);  
});

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
