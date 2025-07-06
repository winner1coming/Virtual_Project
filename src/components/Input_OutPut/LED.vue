<template>
    <g :transform="`translate(${LED.offset[0]*LED.scale}, ${LED.offset[1]*LED.scale}) scale(${LED.scale})`" cursor="move">
      <!-- 圆形 -->
      <circle
        :cx="0"
        :cy="0"
        :r="80"
        :fill="LED.inputs[0]? 'red':'rgba(206.55, 206.55, 206.55, 1)'"
        stroke="black"
        stroke-width="6"
      />
  
      <!--选中方框-->
      <SelectedBox :x="-40" :y="-40" :width="92" :height="92" :visible="circuitStore.selectedId===props.id"/>

      <!-- 输入 -->
      <InputPort :cx="-40" :cy="0" :active="LED.inputs[0]" :bitWidth="LED.bitWidth" />
    </g>
  </template>
  
  <script setup>
  import { ref, computed, watch } from 'vue'
  import InputPort from '@/components/Ports/InputPort.vue'
  import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
  
  import { useCircuitStore } from '@/store/CircuitStore'
  
  const circuitStore = useCircuitStore();
  const props = defineProps({
  id: {
    type: Number,
    required: true
  },
  })
  
  const LED = computed(() => {
  return circuitStore.getComponent(props.id);  
  });

  </script>
  