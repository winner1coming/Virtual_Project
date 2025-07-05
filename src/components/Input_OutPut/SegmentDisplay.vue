<template>
      <g :transform="`translate(${segmentDisplay.offset[0]*segmentDisplay.scale}, ${segmentDisplay.offset[1]*segmentDisplay.scale}) scale(${segmentDisplay.scale})`" cursor="move">
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
        <rect :x="149" :y="86" :width="372-149" :height="401-92" fill="transparent"></rect>
        <!--选中方框-->
        <SelectedBox :x="140" :y="52" :width="372-149+12" :height="401-92+72" :visible="circuitStore.selectedId===props.id"/>
     

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
        <InputPort :cx="182" :cy="61" :active="segmentDisplay.inputs[0]" :bitWidth="segmentDisplay.width" />
        <path stroke="black" stroke-width="12" d="M182 91L182 61"></path>
        <!--接口1-->
        <InputPort :cx="234" :cy="61" :active="segmentDisplay.inputs[1]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M234 91L234 61"></path>
        <!--接口2-->
        <InputPort :cx="286" :cy="61" :active="segmentDisplay.inputs[2]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M286 91L286 61"></path>
        <!--接口3-->
        <InputPort :cx="340" :cy="61" :active="segmentDisplay.inputs[3]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M340 91L340 61"></path>
        <!--接口4-->
        <InputPort :cx="182" :cy="426" :active="segmentDisplay.inputs[4]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M182 396L182 426"></path>
        <!--接口5-->
        <InputPort :cx="234" :cy="426" :active="segmentDisplay.inputs[5]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M234 396L234 426"></path>
        <!--接口6-->
        <InputPort :cx="286" :cy="426" :active="segmentDisplay.inputs[6]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M286 396L286 426"></path>
        <!--接口7-->
        <InputPort :cx="340" :cy="426" :active="segmentDisplay.inputs[7]" :bitWidth="1" />
        <path stroke="black" stroke-width="12" d="M340 396L340 426"></path>

        <!-- <InputPort :cx="92" :cy="inputYs[index]" :active="input" :bitWidth="segmentDisplay.width" @toggle="() => handleToggleInput(index)"/> -->
      </g>
  </template>
  
  <script setup>
  import { ref, reactive, computed, onMounted, onUnmounted } from 'vue'
  import InputPort from '@/components/Ports/InputPort.vue'
  import { useCircuitStore } from '@/store/CircuitStore'
  import SelectedBox from '@/components/basicComponents/SelectedBox.vue'
  
  const circuitStore = useCircuitStore();
  //const id = circuitStore.addComponent('And', [0,0]);  
  const props = defineProps({
    id: {
      type: String,
      required: true
    }
  }); 
  const segmentDisplay = computed(() => {
    //return circuitStore.getComponent(id);   
    return circuitStore.getComponent(props.id);  
  });

  </script>
  
  <style scoped>
  svg {
    border: 1px solid #ccc;
    background-color: #f8f8f8;
  }
  </style>
  