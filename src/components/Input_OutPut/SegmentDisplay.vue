<template>
    <svg
      viewBox="0 0 600 600"
      width="300"
      height="300"
      style="overflow: visible;"
      
    >
      <g :transform="`translate(${segmentDisplay.position[0]}, ${segmentDisplay.position[1]}) scale(${segmentDisplay.scale})`" cursor="move">
        <!-- 图形 -->
        <!--外边框-->
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149 86L149 401">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M148.999 395L371.999 395">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M149 92L372 92">
        </path>
        <path    stroke="rgba(0, 0, 0, 1)" stroke-width="12"    d="M372 86L372 401">
        </path>

        <!--0显示横线-->
        <path    :stroke="segmentDisplay.inputs[0]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M291.915 243.609L193.777 243.609">
        </path>
        <!--1显示竖线-->
        <path    :stroke="segmentDisplay.inputs[1]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M182 134L182 241.173">
        </path>
        <!--2显示横线-->
        <path    :stroke="segmentDisplay.inputs[2]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M291.915 134L193.777 134">
        </path>
        <!--3显示竖线-->
        <path    :stroke="segmentDisplay.inputs[3]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M304.346 134L304.346 241.173">
        </path>
        <!--4显示竖线-->
        <path    :stroke="segmentDisplay.inputs[4]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M182 244.827L182 352">
        </path>
        <!--5显示横线-->
        <path    :stroke="segmentDisplay.inputs[5]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M291.915 352L193.777 352">
        </path>
        <!--6显示竖线-->
        <path    :stroke="segmentDisplay.inputs[6]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M305 244.827L305 352">
        </path>
        <!--小数点-->
        <path    :stroke="segmentDisplay.inputs[7]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M340 344L340 360">
        </path>

        <!--接口0-->
        <InputPort :cx="182" :cy="91" :active="segmentDisplay.inputs[0]" :bitWidth="segmentDisplay.width" />
        <!--接口1-->
        <InputPort :cx="232" :cy="91" :active="segmentDisplay.inputs[1]" :bitWidth="1" />
        <!--接口2-->
        <InputPort :cx="286" :cy="91" :active="segmentDisplay.inputs[2]" :bitWidth="1" />
        <!--接口3-->
        <InputPort :cx="340" :cy="91" :active="segmentDisplay.inputs[3]" :bitWidth="1" />
        <!--接口4-->
        <InputPort :cx="182" :cy="396" :active="segmentDisplay.inputs[4]" :bitWidth="1" />
        <!--接口5-->
        <InputPort :cx="234" :cy="396" :active="segmentDisplay.inputs[5]" :bitWidth="1" />
        <!--接口6-->
        <InputPort :cx="286" :cy="396" :active="segmentDisplay.inputs[6]" :bitWidth="1" />
        <!--接口7-->
        <InputPort :cx="340" :cy="396" :active="segmentDisplay.inputs[7]" :bitWidth="1" />

        <!-- <InputPort :cx="92" :cy="inputYs[index]" :active="input" :bitWidth="segmentDisplay.width" @toggle="() => handleToggleInput(index)"/> -->
      </g>
    </svg>
  </template>
  
  <script setup>
  import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
  import InputPort from '@/components/Ports/InputPort.vue'
  import { useCircuitStore } from '@/store/CircuitStore'
  
  const circuitStore = useCircuitStore();
  const id = circuitStore.addComponent('And', [0,0]);  // debug todo 要改类型
  
  const segmentDisplay = computed(() => {
    return circuitStore.getComponent(id);   // debug
    // return circuitStore.getComponent(props.id);  
  });

  </script>
  
  <style scoped>
  svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
  }
  </style>
  