<template>
    <g :transform="`translate(${myButton.offset[0]*myButton.scale}, ${myButton.offset[1]*myButton.scale}) scale(${myButton.scale})`" cursor="move">
        <rect x="103" y="87" width="70" height="70" stroke="rgba(0, 0, 0, 1)" stroke-width="5" 
            :fill="isPressed ? '#FFFFFF' : '#B3B3B3'" 
        >
        </rect> <!--下面的-->
        <rect x="91" y="76" width="70" height="70" stroke="rgba(0, 0, 0, 1)" stroke-width="5" fill="#FFFFFF" 
            v-show="!isPressed" @mouseup="handleMouseUp" @mouseleave="handleMouseUp" 
            @mousedown.stop.prevent="handleMouseDown"
        >
        </rect> <!--上面的-->
      
      <!--填充透明区域以便选中-->
      <!-- <rect
        x="0"
        y="0"
        :width="svgWidth"
        :height="svgHeight"
        fill=transparent
      /> -->
  
      <!--选中方框-->
      <!-- <SelectedBox :x="-6" :y="-6" :width="svgWidth+12" :height="svgHeight+12" :visible="circuitStore.selectedId===props.id"/> -->

      <!-- 输出 -->
      <OutputPort :cx="173" :cy="157" :active="isPressed" :bitWidth="1" />
    </g>
  </template>
    
  <script setup>
  import { ref, computed, watch } from 'vue'
  import OutputPort from '@/components/Ports/OutputPort.vue'
  import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
    
  import { useCircuitStore } from '@/store/CircuitStore'
  
  const circuitStore = useCircuitStore();
  const props = defineProps({
    id: {
      type: Number,
      required: true
    },
  })
  
  const myButton = computed(() => {
    return circuitStore.getComponent(props.id);  
  });
  const isPressed = ref(false)

  const handleMouseDown = () => {
    isPressed.value = true
  }

  const handleMouseUp = () => {
    isPressed.value = false
  }
  </script>
    