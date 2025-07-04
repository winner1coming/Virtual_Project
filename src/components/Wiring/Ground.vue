<template>
    <g :transform="`translate(${ground.offset[0]*ground.scale}, ${ground.offset[1]*ground.scale}) scale(${ground.scale})`" cursor="move">
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="10"    d="M29 75L94 75">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="10"    d="M52 95L72 95">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="10"    d="M10 55L110 55">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M62 15L62 55">
        </path>

        <!--选中方框-->
        <SelectedBox :x="10" :y="10" :width="60" :height="130" :visible="circuitStore.selectedId===props.id"/>
    
        <!-- 输出状态 -->
        <OutputPort :cx="62" :cy="13" :active="ground.outputs[0]" />
    </g>
</template>

<script setup>
import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
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
const ground = computed(() => {
  return circuitStore.getComponent(props.id);  
});

</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
