<template>
    <g :transform="`translate(${clock.offset[0]*clock.scale}, ${clock.offset[1]*clock.scale}) scale(${clock.scale})`" cursor="move">
      <!-- 图形 -->
      <rect x="0" y="0" width="235" height="236" stroke="rgba(0, 0, 0, 1)" stroke-width="12" fill="#FFFFFF" />

      <!-- 低电平 -->
      <path :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12" d="M33 159L124 159" />
      <path :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12" d="M38 97.744L38 165" />
      <path :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12" d="M196.992 71L196.992 138.256" />
      <path :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12" d="M118 76L118 164" />
      <path :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12" d="M112 75.76L203 75.76" />

      <!-- 高电平 -->
      <path :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M33 77L123.5 77" />
      <path :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M38 138.256L38 71" />
      <path :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M196.992 165.5L196.992 97.744" />
      <path :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M118 160L118 72" />
      <path :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M112 160.24L202.5 160.24" />

      <!--选中方框-->
      <SelectedBox :x="-6" :y="-6" :width="235+12" :height="236+12" :visible="circuitStore.selectedId===props.id"/>

      <!-- 输出状态 -->
      <OutputPort :cx="clock.outputPinPosition[0][0]" :cy="clock.outputPinPosition[0][1]" :active="clock.outputs[0]" />

      <!--调试用  todo-->
      <!-- <InputPort :cx="140" :cy="288" :active=0 @toggle="() => handleToggleInput()" /> -->
    </g>
</template>

  
  <script setup>
  import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
  import InputPort from '@/components/Ports/InputPort.vue'
  import OutputPort from '@/components/Ports/OutputPort.vue'
  import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
  import { defineProps } from 'vue'
  
  import { useGateLayout } from '@/logic/usegates/useGateLayout'
  import { useCircuitStore } from '@/store/CircuitStore'
  import {watchComponentChanges} from '@/modules/useComponentsWatchers'
  
  const circuitStore = useCircuitStore();
  const props = defineProps({
    id: {
      type: Number,
      required: true
    }
  });
  //const id = circuitStore.addComponent('And', [0,0]);  // debug
  
  const clock = computed(() => {
    // return circuitStore.getComponent(id);   // debug
    return circuitStore.getComponent(props.id);  
  });
  
  // 调试 --todo---------------------------------------------------------------
  function handleToggleInput()
  {
    console.log(1);
    if(clock.value.outputs[0]==1){
      clock.value.outputs[0] = 0;
      console.log(2);
    }
    else{
        clock.value.outputs[0] = 1;
        console.log(3);
    }
  }

</script>