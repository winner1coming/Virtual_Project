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
      <path    :stroke="activeSegments[0]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M291.915 243.609L193.777 243.609">
      </path>
      <!--1显示竖线-->
      <path    :stroke="activeSegments[1]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M182 134L182 241.173">
      </path>
      <!--2显示横线-->
      <path    :stroke="activeSegments[2]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M291.915 134L193.777 134">
      </path>
      <!--3显示竖线-->
      <path    :stroke="activeSegments[3]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M304.346 134L304.346 241.173">
      </path>
      <!--4显示竖线-->
      <path    :stroke="activeSegments[4]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M182 244.827L182 352">
      </path>
      <!--5显示横线-->
      <path    :stroke="activeSegments[5]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M291.915 352L193.777 352">
      </path>
      <!--6显示竖线-->
      <path    :stroke="activeSegments[6]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M305 244.827L305 352">
      </path>
      <!--小数点-->
      <path    :stroke="segmentDisplay.inputs[1]===1 ? 'red':'rgba(206.55, 206.55, 206.55, 1)'" stroke-width="16"    d="M340 344L340 360">
      </path>

      <!--接口1-->
      <InputPort :cx="208" :cy="426" :active="segmentDisplay.inputs[0]" :bitWidth="segmentDisplay.bitWidth" />
      <path stroke="black" stroke-width="12" d="M208 396L208 426"></path>
      <!--接口7-->
      <InputPort :cx="313" :cy="426" :active="segmentDisplay.inputs[1]" :bitWidth="1" />
      <path stroke="black" stroke-width="12" d="M313 396L313 426"></path>

      <!-- <InputPort :cx="92" :cy="inputYs[index]" :active="input" :bitWidth="segmentDisplay.width" @toggle="() => handleToggleInput(index)"/> -->
    </g>
</template>

<script setup>
import { computed } from 'vue'
import InputPort from '@/components/Ports/InputPort.vue'
import { useCircuitStore } from '@/store/CircuitStore'
import SelectedBox from '@/components/basicComponents/SelectedBox.vue'

const circuitStore = useCircuitStore();
const props = defineProps({
  id: {
    type: String,
    required: true
  }
}); 

const segmentDisplay = computed(() => circuitStore.getComponent(props.id));

const hexToSegments = {
  0: [0, 1, 1, 1, 1, 1, 1],        // 0：只有第0个是0
  1: [0, 0, 0, 1, 0, 0, 1],        // 1：第3、6个是1
  2: [1, 0, 1, 1, 1, 1, 0],        // 2：0,2,3,4,5
  3: [1, 0, 1, 1, 0, 1, 1],        // 3：0,2,3,5,6
  4: [1, 1, 0, 1, 0, 0, 1],        // 4：0,1,3,6
  5: [1, 1, 1, 0, 0, 1, 1],        // 5：0,1,2,5,6
  6: [1, 1, 1, 0, 1, 1, 1],        // 6：0,1,2,4,5,6
  7: [0, 0, 1, 1, 0, 0, 1],        // 7：2,3,6
  8: [1, 1, 1, 1, 1, 1, 1],        // 8：全亮
  9: [1, 1, 1, 1, 0, 1, 1],        // 9：0,1,2,3,5,6
  10: [1, 1, 1, 1, 1, 0, 1],       // A：0,1,2,3,4,5
  11: [1, 1, 0, 0, 1, 1, 1],       // B：0,1,4,5,6
  12: [0, 1, 1, 0, 1, 1, 0],       // C：1,2,4,5
  13: [1, 0, 0, 1, 1, 1, 1],       // D：0,3,4,5,6
  14: [1, 1, 1, 0, 1, 1, 0],       // E：0,1,2,4,5
  15: [1, 1, 1, 0, 1, 0, 0],       // F：0,1,2,3
};

const activeSegments = computed(() => {
  const val = segmentDisplay.value.inputs?.[0] ?? 0;
  const dec = Math.max(0, Math.min(15, val));
  return hexToSegments[dec];
});
</script>

<style scoped>
svg {
  border: 1px solid #ccc;
  background-color: #f8f8f8;
}
</style>
