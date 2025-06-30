<template>
    <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
      
    >
      <g :transform="`translate(${clock.position[0]}, ${clock.position[1]}) scale(${clock.scale})`" cursor="move">
        <!-- 图形 -->
        <rect x="140" y="170" width="235" height="236" stroke="rgba(0, 0, 0, 1)" stroke-width="12"      fill="#FFFFFF" />

        <!--低电平-->
        <path    :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12"    d="M173 329L264 329">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'" stroke-width="12" d="M178 267.744L178 335">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'"  stroke-width="12" d="M336.992 241L336.992 308.256">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'"  stroke-width="12" d="M258 246L258 334">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'transparent' : 'black'"  stroke-width="12" d="M252 245.76L343 245.76" />

        <!-- 高电平 -->
        <path    :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M173 247L263.5 247">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M178 308.256L178 241">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M336.992 335.5L336.992 267.744">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M258 330L258 242">
        </path>
        <path    :stroke="clock.outputs[0] === 1 ? 'green' : 'transparent'" stroke-width="12" d="M252 330.24L342.5 330.24">
        </path>

        <!-- 输出状态 -->
        <OutputPort :cx="375" :cy="288" :active="clock.outputs[0]" />

        <!--调试用  todo-->
        <InputPort :cx="140" :cy="288" :active=0 @toggle="() => handleToggleInput()" />
      </g>
    </svg>
  </template>
  
  <script setup>
  import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
  import InputPort from '@/components/Ports/InputPort.vue'
  import OutputPort from '@/components/Ports/OutputPort.vue'
  import { defineProps } from 'vue'
  
  import { useGateLayout } from '@/logic/usegates/useGateLayout'
  import { useCircuitStore } from '@/store/CircuitStore'
  import {watchComponentChanges} from '@/modules/useComponentsWatchers'
  
  const circuitStore = useCircuitStore();
  const id = circuitStore.addComponent('And', [0,0]);  // debug
  
  const clock = computed(() => {
    return circuitStore.getComponent(id);   // debug
    // return circuitStore.getComponent(props.id);  
  });
  
  // 调试 --todo---------------------------------------------------------------
  function handleToggleInput()
  {
    if(clock.value.outputs[0]===1)
        clock.value.outputs[0] = 0;
    else
        clock.value.outputs[0] = 1;
  }

</script>